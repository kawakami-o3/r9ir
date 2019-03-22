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
// - Reject bad assignments, such as `1=2+3`.

use crate::parse::*;
use crate::util::*;
use std::collections::HashMap;
use std::mem;
use std::sync::Mutex;

lazy_static! {
    static ref GLOBALS: Mutex<Vec<Var>> = Mutex::new(Vec::new());
    static ref ENV: Mutex<Env> = Mutex::new(new_env(None));
    static ref STR_LABEL: Mutex<i32> = Mutex::new(0);
    static ref STACKSIZE: Mutex<i32> = Mutex::new(0);
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
    match ENV.lock() {
        Ok(ref mut env) => {
            env.vars.insert(name, var);
        }
        Err(_) => {
            panic!();
        }
    }
}

fn env_push() {
    let mut env = ENV.lock().unwrap();
    *env = new_env(Some(env.clone()));
}

fn env_pop() {
    let mut env = ENV.lock().unwrap();
    *env = *env.next.clone().unwrap();
}

fn new_int(val: i32) -> Node {
    let mut node = alloc_node();
    node.op = NodeType::NUM;
    node.ty = int_ty();
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
    *GLOBALS.lock().unwrap() = Vec::new();
}

fn globals() -> Vec<Var> {
    GLOBALS.lock().unwrap().clone()
}

fn globals_push(var: Var) {
    let mut globals = GLOBALS.lock().unwrap();
    globals.push(var);
}

fn stacksize() -> i32 {
    *STACKSIZE.lock().unwrap()
}

fn init_stacksize() {
    let mut stacksize = STACKSIZE.lock().unwrap();
    *stacksize = 0;
}

fn set_stacksize(i: i32) {
    let mut stacksize = STACKSIZE.lock().unwrap();
    *stacksize = i;
}

fn add_stacksize(i: i32) {
    let mut stacksize = STACKSIZE.lock().unwrap();
    *stacksize += i;
}

fn bump_str_label() -> i32 {
    let mut str_label = STR_LABEL.lock().unwrap();
    let ret = *str_label;
    *str_label += 1;
    return ret;
}

fn find_var(name: String) -> Option<Var> {
    return match ENV.lock() {
        Ok(ref env) => {
            match env.find(&name) {
                Some(v) => Some(v.clone()),
                None => None,
            }
        }
        Err(_) => {
            panic!();
        }
    }
}

fn swap(p: &mut Node, q: &mut Node) {
    mem::swap(p, q);
}

fn maybe_decay(base: & mut Node, decay: bool) -> & mut Node {
    if !decay || base.ty.ty != CType::ARY {
        return base;
    }

    let tmp = base.clone();
    *base = alloc_node();
    base.op = NodeType::ADDR;
    base.ty = ptr_to(*tmp.clone().ty.ary_of.unwrap());

    base.expr = Some(Box::new(tmp));
    return base;
}

fn check_lval(node: & Node) {
    match node.op {
        NodeType::LVAR | NodeType::GVAR | NodeType::DEREF | NodeType::DOT => { }
        _ => {
            panic!("not an lvalue: {:?} ({})", node.op, node.name);
        }
    }
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
            let var = new_global(node_ty.clone(), name, node_data, node.len);
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
                node.ty = var.ty.clone();
                node.offset = var.offset;
                return maybe_decay(node, decay);
            }
                
            *node = alloc_node();
            node.op = NodeType::GVAR;
            node.ty = var.ty.clone();
            node.name = var.name.clone();
            return maybe_decay(node, decay);
        }
        NodeType::VARDEF => {
            set_stacksize(roundup(stacksize(), node.ty.align));
            add_stacksize(node.ty.size);
            node.offset = stacksize();

            let mut var = alloc_var(node.ty.clone());
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
            node.cond = Some(Box::new(walk(&mut *node.cond.clone().unwrap(), true).clone()));
            node.inc = Some(Box::new(walk(&mut *node.inc.clone().unwrap(), true).clone()));
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
                    if rhs.ty.ty == CType::PTR {
                        swap(lhs, rhs);
                    }
                }
                _ => {}
            }
            match node.rhs {
                Some(ref rhs) => {
                    if rhs.ty.ty == CType::PTR {
                        panic!("'pointer {:?} pointer' is not defined", node.op);
                    }
                }
                None => {
                }
            }

            match node.lhs {
                Some(ref lhs) => {
                    node.ty = lhs.ty.clone();
                }
                None => { }
            }

            return node;
        }
        NodeType::EQL => {
            node.lhs = Some(Box::new(walk(&mut *node.lhs.clone().unwrap(), false).clone()));
            check_lval(& *node.lhs.clone().unwrap());

            node.rhs = Some(Box::new(walk(&mut *node.rhs.clone().unwrap(), true).clone()));
            node.ty = node.lhs.clone().unwrap().ty;
            return node;
        }
        NodeType::DOT => {
            node.expr = Some(Box::new(walk(&mut *node.expr.clone().unwrap(), true).clone()));
            if node.expr.clone().unwrap().ty.ty != CType::STRUCT {
                panic!("struct expected before '.'");
            }

            let ty = node.expr.clone().unwrap().ty;
            if ty.members == None {
                panic!("incomplete type: {:?}", node.expr);
            }

            for m in ty.members.unwrap().iter() {
                if m.name != node.name {
                    continue;
                }

                node.ty = m.ty.clone();
                node.offset = m.ty.offset;
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
        NodeType::PRE_INC |
            NodeType::PRE_DEC |
            NodeType::POST_INC |
            NodeType::POST_DEC |
            NodeType::NEG |
            NodeType::EXCLAM => {
            node.expr = Some(Box::new(walk(&mut *node.expr.clone().unwrap(), true).clone()));
            node.ty = node.expr.clone().unwrap().ty;
            return node;
        }
        NodeType::ADDR => {
            node.expr = Some(Box::new(walk(&mut *node.expr.clone().unwrap(), true).clone()));
            check_lval(& *node.expr.clone().unwrap());
            node.ty = ptr_to(node.expr.clone().unwrap().ty);
            return node;
        }
        NodeType::DEREF => {
            node.expr = Some(Box::new(walk(&mut *node.expr.clone().unwrap(), true).clone()));

            match &mut node.expr {
                Some(ref expr) => {
                    if expr.ty.ty != CType::PTR {
                        panic!("operand must be a pointer: {:?}", expr);
                    }

                    if expr.ty.ptr_to.clone().unwrap().ty == CType::VOID {
                        panic!("operand dereference void pointer");
                    }
                }
                None => {}
            }

            node.ty = *node.expr.clone().unwrap().ty.ptr_to.unwrap();
            return node;
        }
        NodeType::RETURN |
            NodeType::EXPR_STMT => {
            node.expr = Some(Box::new(walk(&mut *node.expr.clone().unwrap(), true).clone()));
            return node;
        }
        NodeType::SIZEOF => {
            let mut nexpr = *node.expr.clone().unwrap();
            let expr = walk(&mut nexpr, false);
            let val = expr.ty.size;

            *node = new_int(val);
            return node;
        }
        NodeType::ALIGNOF => {
            let mut e = node.expr.clone().unwrap();
            let expr = walk(&mut e, false);
            *node = new_int(expr.ty.align);
            return node;
        }
        NodeType::CALL => {
            for i in 0..node.args.len() {
                node.args[i] = walk(&mut node.args[i], true).clone();
            }
            node.ty = int_ty();
            return node;
        }
        NodeType::FUNC => {
            for i in 0..node.args.len() {
                node.args[i] = walk(&mut node.args[i], true).clone();
            }
            node.body = Some(Box::new(walk(&mut *node.body.clone().unwrap(), true).clone()));
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
            node.ty = int_ty();
            return node;
        }
        _ => {
            panic!("unknown node type: {:?}", node);
        }
    }
}

pub fn sema(nodes: &mut Vec<Node>) -> Vec<Var> {
    init_globals();
    //let mut topenv = new_env(None);

    for node in nodes.iter_mut() {
        if node.op == NodeType::VARDEF {
            let mut var = new_global(node.ty.clone(), node.name.clone(), node.data.clone(), node.len);
            var.is_extern = node.is_extern;
            globals_push(var.clone());
            env_var_put(node.name.clone(), var);
            continue;
        }

        assert!(node.op == NodeType::FUNC);

        init_stacksize();
        walk(node, true);
        node.stacksize = stacksize();
    }
    return globals();
}
