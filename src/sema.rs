// Semantics analyzer. This pass plays a few important roles as shown
// below:
//
// - Add types to nodes. For example, a tree that represents "1+2" is
//   typed as INT because the result type of an addition of two
//   integers is integer.
//
// - Resolve variable names based on the C scope rules.
//   Local variables are resolved to offsets from the base pointer.
//   Global variables are resolved to their names.
//
// - Insert nodes to make array-to-pointer conversion explicit.
//   Recall that, in C, "array of T" is automatically converted to
//   "pointer to T" in most contexts.
//
// - Scales operands for pointer arithmetic. E.g. ptr+1 becomes ptr+4
//   for integer and becomes ptr+8 for pointer.
//
// - Reject bad assignments, such as `1=2+3`.

use crate::parse::*;
use crate::util::*;
use std::collections::HashMap;
use std::cell::RefCell;
use std::mem;
use std::rc::Rc;

thread_local! {
    static GLOBALS: RefCell<Vec<Var>> = RefCell::new(Vec::new());
    static ENV: RefCell<Env> = RefCell::new(new_env(None));
    static STR_LABEL: RefCell<i32> = RefCell::new(0);
    static STACKSIZE: RefCell<i32> = RefCell::new(0);
}

#[derive(Clone, Debug)]
struct Env {
    vars: HashMap<String, Var>,
    next: Option<Box<Env>>,
}

fn new_env(next: Option<Env>) -> Env {
    Env {
        vars: HashMap::new(),
        next: if let Some(e) = next {
            Some(Box::new(e))
        } else {
            None
        },
    }
}

fn env_var_put(name: String, var: Var) {
    ENV.with(|env| {
        env.borrow_mut().vars.insert(name, var);
    })
}

fn env_push() {
    ENV.with(|env| {
        let e = env.borrow().clone();
        *env.borrow_mut() = new_env(Some(e));
    })
}

fn env_pop() {
    ENV.with(|env| {
        let e = env.borrow().next.clone().unwrap();
        *env.borrow_mut() = *e;
    })
}

fn new_int(val: i32) -> Node {
    let mut node = alloc_node();
    node.op = NodeType::NUM;
    node.ty = Rc::new(RefCell::new(int_ty()));
    node.val = val;
    return node;
}

//impl<'a> Env<'a> {
impl Env {
    fn find(&self, name: & String) -> Option<&Var> {
        if let Some(v) = self.vars.get(name) {
            return Some(v);
        } else if let Some(ref next) = self.next {
            return next.find(name);
        } else {
            return None;
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Var {
    pub ty: Type,
    pub is_local: bool,

    // local
    pub offset: i32,

    // global
    pub name: String,
    pub is_extern: bool,
    pub data: String,
    pub len: usize,
}

fn alloc_var(ty: Type) -> Var {
    Var {
        ty: ty,
        is_local: false,
        offset: 0,
        name: String::new(),
        is_extern: false,
        data: String::new(),
        len: 0,
    }
}

fn new_global(ty: Type, name: String, data: String, len: usize) -> Var {
    let mut var = alloc_var(ty);
    var.is_local = false;
    //var.name = format!(".L.str{}", bump_str_label()).to_string();
    var.name = name;
    var.data = data;
    var.len = len;
    return var;
}

fn init_globals() {
    GLOBALS.with(|globals| {
        *globals.borrow_mut() = Vec::new();
    })
}

fn globals() -> Vec<Var> {
    GLOBALS.with(|globals| {
        globals.borrow().clone()
    })
}

fn globals_push(var: Var) {
    GLOBALS.with(|globals| {
        (*globals.borrow_mut()).push(var);
    })
}

fn stacksize() -> i32 {
    STACKSIZE.with(|stacksize| {
        *stacksize.borrow()
    })
}

fn init_stacksize() {
    set_stacksize(0);
}

fn set_stacksize(i: i32) {
    STACKSIZE.with(|stacksize| {
        *stacksize.borrow_mut() = i;
    })
}

fn add_stacksize(i: i32) {
    STACKSIZE.with(|stacksize| {
        *stacksize.borrow_mut() += i;
    })
}

fn bump_str_label() -> i32 {
    STR_LABEL.with(|str_label| {
        let ret = *str_label.borrow();
        *str_label.borrow_mut() += 1;
        return ret;
    })
}

fn find_var(name: String) -> Option<Var> {
    ENV.with(|env| {
        match (*env.borrow()).find(&name) {
            Some(v) => Some(v.clone()),
            None => None,
        }
    })
}

fn swap(p: &mut Node, q: &mut Node) {
    mem::swap(p, q);
}

fn maybe_decay(base: & mut Node, decay: bool) -> & mut Node {
    if !decay || base.ty.borrow().ty != CType::ARY {
        return base;
    }

    let tmp = base.clone();
    *base = alloc_node();
    base.op = NodeType::ADDR;

    let node_ty = tmp.clone().ty;
    let ty_tmp = *node_ty.borrow().clone().ary_of.unwrap();
    base.ty = Rc::new(RefCell::new(ptr_to(Rc::new(RefCell::new(ty_tmp)))));

    base.expr = Some(Box::new(tmp));
    return base;
}

fn check_lval(node: Box<Node>) {
    match node.op {
        NodeType::LVAR | NodeType::GVAR | NodeType::DEREF | NodeType::DOT => { }
        _ => {
            panic!("not an lvalue: {:?} ({})", node.op, node.name);
        }
    }
}

fn scale_ptr(node: Node, ty: Type) -> Node {
    let mut e = alloc_node();
    e.op = NodeType::MUL;
    e.lhs = Some(Box::new(node.clone()));
    let ptr = ty.ptr_to.unwrap();
    e.rhs = Some(Box::new(new_int(ptr.borrow().size)));
    return e;
}

fn walk<'a>(node: &'a mut Node, decay: bool) -> &'a Node {
    match node.op {
        NodeType::NUM |
            NodeType::NULL |
            NodeType::BREAK => {
            return node;
        }
        NodeType::STR => {
            // A string literal is converted to a reference to an anonymous
            // global variable of type char array.
            let node_ty = node.ty.clone();
            let node_data = node.data.clone();

            let name = format!(".L.str{}", bump_str_label()).to_string();
            let var = new_global(node_ty.borrow().clone(), name, node_data, node.len);
            globals_push(var.clone());

            *node = alloc_node();
            node.op = NodeType::GVAR;
            node.ty = node_ty;
            node.name = var.name;
            return maybe_decay(node, decay);
        }
        NodeType::IDENT => {
            let var = match find_var(node.name.clone()) {
                None => panic!("undefined variable: {}", node.name),
                Some(var) => var,
            };

            if var.is_local {
                *node = alloc_node();
                node.op = NodeType::LVAR;
                node.ty = Rc::new(RefCell::new(var.ty.clone()));
                node.offset = var.offset;
                return maybe_decay(node, decay);
            }

            *node = alloc_node();
            node.op = NodeType::GVAR;
            node.ty = Rc::new(RefCell::new(var.ty.clone()));
            node.name = var.name.clone();
            return maybe_decay(node, decay);
        }
        NodeType::VARDEF => {
            set_stacksize(roundup(stacksize(), node.ty.borrow().align));
            add_stacksize(node.ty.borrow().size);
            node.offset = stacksize();

            let mut var = alloc_var(node.ty.borrow().clone());
            var.is_local = true;
            var.offset = stacksize();
            env_var_put(node.name.clone(), var.clone());

            match node.init {
                Some(_) => {
                    node.init = Some(Box::new(walk(&mut *node.init.clone().unwrap(), true).clone()));
                }
                None => { }
            }
            return node;
        }
        NodeType::IF => {
            node.cond = Some(Box::new(walk(&mut *node.cond.clone().unwrap(), true).clone()));
            node.then = Some(Box::new(walk(&mut *node.then.clone().unwrap(), true).clone()));

            if node.els.is_some() {
                node.els = Some(Box::new(walk(&mut *node.els.clone().unwrap(), true).clone()));
            }
            return node;
        }
        NodeType::FOR => {
            env_push();
            node.init = Some(Box::new(walk(&mut *node.init.clone().unwrap(), true).clone()));
            if let Some(mut cond) = node.cond.clone() {
                node.cond = Some(Box::new(walk(&mut cond, true).clone()));
            }
            if let Some(mut inc) = node.inc.clone() {
                node.inc = Some(Box::new(walk(&mut inc, true).clone()));
            }
            node.body = Some(Box::new(walk(&mut *node.body.clone().unwrap(), true).clone()));
            env_pop();
            return node;
        }
        NodeType::DO_WHILE => {
            node.cond = Some(Box::new(walk(&mut *node.cond.clone().unwrap(), true).clone()));
            node.body = Some(Box::new(walk(&mut *node.body.clone().unwrap(), true).clone()));
            return node;
        }
        NodeType::ADD | NodeType::SUB => {
            node.lhs = Some(Box::new(walk(&mut *node.lhs.clone().unwrap(), true).clone()));
            node.rhs = Some(Box::new(walk(&mut *node.rhs.clone().unwrap(), true).clone()));

            match (&mut node.lhs, &mut node.rhs) {
                (Some(ref mut lhs), Some(ref mut rhs)) => {
                    if rhs.ty.borrow().ty == CType::PTR {
                        swap(lhs, rhs);
                    }
                }
                _ => {}
            }

            if let Some(ref rhs) = node.rhs {
                if rhs.ty.borrow().ty == CType::PTR {
                    panic!("'pointer {:?} pointer' is not defined", node.op);
                }
            }

            if let Some(ref lhs) = node.lhs {
                if lhs.ty.borrow().ty == CType::PTR {
                    node.rhs = Some(Box::new(scale_ptr(*node.rhs.clone().unwrap(), lhs.ty.borrow().clone())));
                }
            }

            if let Some(ref lhs) = node.lhs {
                node.ty = lhs.ty.clone();
            }
            return node;
        }
        NodeType::ADD_EQ |
            NodeType::SUB_EQ => {
                node.lhs = Some(Box::new(walk(&mut *node.lhs.clone().unwrap(), false).clone()));
                check_lval(node.lhs.clone().unwrap());
                node.rhs = Some(Box::new(walk(&mut *node.rhs.clone().unwrap(), true).clone()));
                if let Some(ref lhs) = node.lhs {
                    node.ty = lhs.ty.clone();
                }

                if let Some(ref lhs) = node.lhs {
                    if lhs.ty.borrow().ty == CType::PTR {
                        node.rhs = Some(Box::new(scale_ptr(*node.rhs.clone().unwrap(), lhs.ty.borrow().clone())));
                    }
                }
                return node;
        }

        NodeType::EQL |
            NodeType::MUL_EQ |
            NodeType::DIV_EQ |
            NodeType::MOD_EQ |
            NodeType::SHL_EQ |
            NodeType::SHR_EQ |
            NodeType::BITAND_EQ |
            NodeType::XOR_EQ |
            NodeType::BITOR_EQ => {
            node.lhs = Some(Box::new(walk(&mut *node.lhs.clone().unwrap(), false).clone()));
            check_lval(node.lhs.clone().unwrap());

            node.rhs = Some(Box::new(walk(&mut *node.rhs.clone().unwrap(), true).clone()));
            node.ty = node.lhs.clone().unwrap().ty;
            return node;
        }
        NodeType::DOT => {
            node.expr = Some(Box::new(walk(&mut *node.expr.clone().unwrap(), true).clone()));
            let node_ty = node.expr.clone().unwrap().ty;
            if node_ty.borrow().ty != CType::STRUCT {
                panic!("struct expected before '.'");
            }

            let ty = node.expr.clone().unwrap().ty;
            if ty.borrow().members == None {
                panic!("incomplete type: {:?}", node.expr);
            }

            for m in ty.borrow().clone().members.unwrap().iter() {
                if m.name != node.name {
                    continue;
                }

                node.ty = m.ty.clone();
                node.offset = m.ty.borrow().offset;
                return maybe_decay(node, decay);
            }
            panic!("member missing: {}", node.name);
        }
        NodeType::QUEST => {
            node.cond = Some(Box::new(walk(&mut *node.cond.clone().unwrap(), true).clone()));
            node.then = Some(Box::new(walk(&mut *node.then.clone().unwrap(), true).clone()));
            node.els = Some(Box::new(walk(&mut *node.els.clone().unwrap(), true).clone()));
            node.ty = node.then.clone().unwrap().ty;
            return node;
        }
        NodeType::MUL |
            NodeType::DIV |
            NodeType::MOD |
            NodeType::LT |
            NodeType::OR |
            NodeType::XOR |
            NodeType::AND |
            NodeType::EQ |
            NodeType::NE |
            NodeType::LE |
            NodeType::SHL |
            NodeType::SHR |
            NodeType::LOGAND |
            NodeType::LOGOR => {
                node.lhs = Some(Box::new(walk(&mut *node.lhs.clone().unwrap(), true).clone()));
                node.rhs = Some(Box::new(walk(&mut *node.rhs.clone().unwrap(), true).clone()));

                match node.lhs {
                    Some(ref lhs) => {
                        node.ty = lhs.ty.clone();
                    }
                    None => { }
                }
                return node;
            }
        NodeType::COMMA => {
            node.lhs = Some(Box::new(walk(&mut *node.lhs.clone().unwrap(), true).clone()));
            node.rhs = Some(Box::new(walk(&mut *node.rhs.clone().unwrap(), true).clone()));
            node.ty = node.rhs.clone().unwrap().ty;
            return node;
        }
        NodeType::POST_INC |
            NodeType::POST_DEC |
            NodeType::NEG |
            NodeType::EXCLAM |
            NodeType::NOT => {
            node.expr = Some(Box::new(walk(&mut *node.expr.clone().unwrap(), true).clone()));
            node.ty = node.expr.clone().unwrap().ty;
            return node;
        }
        NodeType::ADDR => {
            node.expr = Some(Box::new(walk(&mut *node.expr.clone().unwrap(), true).clone()));
            check_lval(node.expr.clone().unwrap());
            node.ty = Rc::new(RefCell::new(ptr_to(node.expr.clone().unwrap().ty)));
            return node;
        }
        NodeType::DEREF => {
            node.expr = Some(Box::new(walk(&mut *node.expr.clone().unwrap(), true).clone()));

            match &mut node.expr {
                Some(ref expr) => {
                    if expr.ty.borrow().ty != CType::PTR {
                        panic!("operand must be a pointer: {:?}", expr);
                    }

                    let ptr = expr.ty.borrow().ptr_to.clone().unwrap();
                    if ptr.borrow().ty == CType::VOID {
                        panic!("operand dereference void pointer");
                    }
                }
                None => {}
            }


            let ty_tmp = node.expr.clone().unwrap().ty;
            node.ty = ty_tmp.borrow().clone().ptr_to.unwrap();
            return maybe_decay(node, decay);
        }
        NodeType::RETURN |
            NodeType::EXPR_STMT => {
            node.expr = Some(Box::new(walk(&mut *node.expr.clone().unwrap(), true).clone()));
            return node;
        }
        NodeType::SIZEOF => {
            let mut nexpr = *node.expr.clone().unwrap();
            let expr = walk(&mut nexpr, false);
            let val = expr.ty.borrow().size;

            *node = new_int(val);
            return node;
        }
        NodeType::ALIGNOF => {
            let mut e = node.expr.clone().unwrap();
            let expr = walk(&mut e, false);
            *node = new_int(expr.ty.borrow().align);
            return node;
        }
        NodeType::CALL => {
            for i in 0..node.args.len() {
                node.args[i] = walk(&mut node.args[i], true).clone();
            }
            node.ty = Rc::new(RefCell::new(int_ty()));
            return node;
        }
        NodeType::COMP_STMT => {
            env_push();
            for i in 0..node.stmts.len() {
                node.stmts[i] = walk(&mut node.stmts[i], true).clone();
            }
            env_pop();
            return node;
        }
        NodeType::STMT_EXPR => {
            node.body = Some(Box::new(walk(&mut *node.body.clone().unwrap(), true).clone()));
            node.ty = Rc::new(RefCell::new(int_ty()));
            return node;
        }
        _ => {
            panic!("unknown node type: {:?}", node);
        }
    }
}

pub fn sema(nodes: &mut Vec<Node>) -> Vec<Var> {
    init_globals();

    for node in nodes.iter_mut() {
        if node.op == NodeType::VARDEF {
            let mut var = new_global(node.ty.borrow().clone(), node.name.clone(), node.data.clone(), node.len);
            var.is_extern = node.is_extern;
            globals_push(var.clone());
            env_var_put(node.name.clone(), var);
            continue;
        }

        assert!(node.op == NodeType::FUNC);

        init_stacksize();
        for i in 0..node.args.len() {
            node.args[i] = walk(&mut node.args[i], true).clone();
        }
        node.body = Some(Box::new(walk(&mut *node.body.clone().unwrap(), true).clone()));

        node.stacksize = stacksize();
    }
    return globals();
}
