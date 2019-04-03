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
use crate::token::*;
use crate::util::*;
use std::collections::HashMap;
use std::cell::RefCell;
use std::mem;
use std::rc::Rc;

thread_local! {
    static PROGRAM: RefCell<Option<Rc<RefCell<Program>>>> = RefCell::new(None);
    static LOCALS: RefCell<Vec<Rc<RefCell<Var>>>> = RefCell::new(Vec::new());
    static ENV: RefCell<Env> = RefCell::new(new_env(None));
    static STR_LABEL: RefCell<i32> = RefCell::new(0);
}

#[derive(Clone, Debug)]
struct Env {
    vars: HashMap<String, Rc<RefCell<Var>>>,
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

fn env_var_put(name: String, var: Rc<RefCell<Var>>) {
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

//impl<'a> Env<'a> {
impl Env {
    fn find(&self, name: & String) -> Option<&Rc<RefCell<Var>>> {
        if let Some(v) = self.vars.get(name) {
            return Some(v);
        } else if let Some(ref next) = self.next {
            return next.find(name);
        } else {
            return None;
        }
    }
}

fn init_locals() {
    LOCALS.with(|l| {
        *l.borrow_mut() = Vec::new();
    })
}

fn locals() -> Vec<Rc<RefCell<Var>>> {
    LOCALS.with(|l| {
        l.borrow().clone()
    })
}

fn locals_push(var: Rc<RefCell<Var>>) {
    LOCALS.with(|l| {
        (*l.borrow_mut()).push(var);
    })
}

fn bump_str_label() -> i32 {
    STR_LABEL.with(|str_label| {
        let ret = *str_label.borrow();
        *str_label.borrow_mut() += 1;
        return ret;
    })
}

fn find_var(name: & String) -> Option<Rc<RefCell<Var>>> {
    ENV.with(|env| {
        match (*env.borrow()).find(name) {
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

macro_rules! bad_node {
    ($node:expr, $msg:expr) => {
        bad_token(&*$node.token.clone().unwrap(), $msg.to_string());
        panic!();
    };
}

macro_rules! warn_node {
    ($node:expr, $msg:expr) => {
        warn_token!(*$node.token.clone().unwrap(), $msg);
    };
}

fn check_lval(node: Box<Node>) {
    match node.op {
        NodeType::VAR | NodeType::DEREF | NodeType::DOT => { }
        _ => {
            bad_node!(node, "not an lvalue");
        }
    }
}

fn scale_ptr(op: NodeType, node: Node, ty: Type) -> Node {
    let mut e = alloc_node();
    e.op = op;
    e.lhs = Some(Box::new(node.clone()));
    let ptr = ty.ptr_to.unwrap();
    e.rhs = Some(Box::new(new_int_node(ptr.borrow().size, node.token)));
    return e;
}

fn walk<'a>(node: &'a mut Node, prog: &'a mut Program) -> &'a Node {
    return do_walk(node, true, prog);
}

fn walk_noconv<'a>(node: &'a mut Node, prog: &'a mut Program) -> &'a Node {
    return do_walk(node, false, prog);
}

fn do_walk<'a>(node: &'a mut Node, decay: bool, prog: &'a mut Program) -> &'a Node {
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
            let node_token = node.token.clone();

            let var = Rc::new(RefCell::new(alloc_var()));
            var.borrow_mut().ty = node_ty.borrow().clone();
            var.borrow_mut().is_local = false;
            var.borrow_mut().name = format!(".L.str{}", bump_str_label()).to_string();
            var.borrow_mut().data = node_data;
            var.borrow_mut().len = node.len;
            prog.gvars.push(var.borrow().clone());

            *node = alloc_node(); // new_gvar_node(node_ty.borrow().clone(), var.name);
            node.op = NodeType::VAR;
            node.var = var.clone();
            let ty = var.borrow().ty.clone();
            node.ty = Rc::new(RefCell::new(ty));
            node.token = node_token;
            return maybe_decay(node, decay);
        }
        NodeType::VAR => {
            let var = match find_var(&node.name) {
                None => {
                    bad_node!(node, "undefined variable");
                }
                Some(var) => var,
            };

            node.var = var.clone();
            let v = var.borrow().clone();
            node.ty = Rc::new(RefCell::new(v.ty));
            return maybe_decay(node, decay);
        }
        NodeType::VARDEF => {
            let mut var = Rc::new(RefCell::new(alloc_var()));
            var.borrow_mut().ty = node.ty.borrow().clone();
            var.borrow_mut().is_local = true;
            env_var_put(node.name.clone(), var.clone());
            locals_push(var.clone());
            node.var = var;

            match node.init {
                Some(_) => {
                    node.init = Some(Box::new(walk(&mut *node.init.clone().unwrap(), prog).clone()));
                }
                None => { }
            }
            return node;
        }
        NodeType::IF => {
            node.cond = Some(Box::new(walk(&mut *node.cond.clone().unwrap(), prog).clone()));
            node.then = Some(Box::new(walk(&mut *node.then.clone().unwrap(), prog).clone()));

            if node.els.is_some() {
                node.els = Some(Box::new(walk(&mut *node.els.clone().unwrap(), prog).clone()));
            }
            return node;
        }
        NodeType::FOR => {
            env_push();
            node.init = Some(Box::new(walk(&mut *node.init.clone().unwrap(), prog).clone()));
            if let Some(mut cond) = node.cond.clone() {
                node.cond = Some(Box::new(walk(&mut cond, prog).clone()));
            }
            if let Some(mut inc) = node.inc.clone() {
                node.inc = Some(Box::new(walk(&mut inc, prog).clone()));
            }
            node.body = Some(Box::new(walk(&mut *node.body.clone().unwrap(), prog).clone()));
            env_pop();
            return node;
        }
        NodeType::DO_WHILE => {
            node.cond = Some(Box::new(walk(&mut *node.cond.clone().unwrap(), prog).clone()));
            node.body = Some(Box::new(walk(&mut *node.body.clone().unwrap(), prog).clone()));
            return node;
        }
        NodeType::ADD => {
            node.lhs = Some(Box::new(walk(&mut *node.lhs.clone().unwrap(), prog).clone()));
            node.rhs = Some(Box::new(walk(&mut *node.rhs.clone().unwrap(), prog).clone()));

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
                    bad_node!(node, "pointer + pointer");
                }
            }

            if let Some(ref lhs) = node.lhs {
                if lhs.ty.borrow().ty == CType::PTR {
                    node.rhs = Some(Box::new(scale_ptr(NodeType::MUL, *node.rhs.clone().unwrap(), lhs.ty.borrow().clone())));
                }
            }

            if let Some(ref lhs) = node.lhs {
                node.ty = lhs.ty.clone();
            }
            return node;
        }
        NodeType::SUB => {
            node.lhs = Some(Box::new(walk(&mut *node.lhs.clone().unwrap(), prog).clone()));
            node.rhs = Some(Box::new(walk(&mut *node.rhs.clone().unwrap(), prog).clone()));

            let l = &node.rhs.clone().unwrap().ty;
            let r = &node.lhs.clone().unwrap().ty;
            if l.borrow().ty == CType::PTR && r.borrow().ty == CType::PTR {
                if !same_type(r.clone(), l.clone()) {
                    bad_node!(node, "incompatible pointer");
                }
                *node = scale_ptr(NodeType::DIV, node.clone(), l.borrow().clone());
            }

            node.ty = node.lhs.clone().unwrap().ty;
            return node;
        }
        NodeType::ADD_EQ |
            NodeType::SUB_EQ => {
                node.lhs = Some(Box::new(walk(&mut *node.lhs.clone().unwrap(), prog).clone()));
                check_lval(node.lhs.clone().unwrap());
                node.rhs = Some(Box::new(walk(&mut *node.rhs.clone().unwrap(), prog).clone()));
                if let Some(ref lhs) = node.lhs {
                    node.ty = lhs.ty.clone();
                }

                if let Some(ref lhs) = node.lhs {
                    if lhs.ty.borrow().ty == CType::PTR {
                        node.rhs = Some(Box::new(scale_ptr(NodeType::MUL, *node.rhs.clone().unwrap(), lhs.ty.borrow().clone())));
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
            node.lhs = Some(Box::new(walk(&mut *node.lhs.clone().unwrap(), prog).clone()));
            check_lval(node.lhs.clone().unwrap());

            node.rhs = Some(Box::new(walk(&mut *node.rhs.clone().unwrap(), prog).clone()));
            node.ty = node.lhs.clone().unwrap().ty;
            return node;
        }
        NodeType::DOT => {
            node.expr = Some(Box::new(walk(&mut *node.expr.clone().unwrap(), prog).clone()));
            let node_ty = node.expr.clone().unwrap().ty;
            if node_ty.borrow().ty != CType::STRUCT {
                bad_node!(node, "struct expected before '.'");
            }

            let ty = node.expr.clone().unwrap().ty;
            if ty.borrow().members == None {
                bad_node!(node, format!("incomplete type: {:?}", node.expr));
            }

            for m in ty.borrow().clone().members.unwrap().iter() {
                if m.name != node.name {
                    continue;
                }

                node.ty = m.ty.clone();
                return maybe_decay(node, decay);
            }
            bad_node!(node, format!("member missing: {}", node.name));
        }
        NodeType::QUEST => {
            node.cond = Some(Box::new(walk(&mut *node.cond.clone().unwrap(), prog).clone()));
            node.then = Some(Box::new(walk(&mut *node.then.clone().unwrap(), prog).clone()));
            node.els = Some(Box::new(walk(&mut *node.els.clone().unwrap(), prog).clone()));
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
                node.lhs = Some(Box::new(walk(&mut *node.lhs.clone().unwrap(), prog).clone()));
                node.rhs = Some(Box::new(walk(&mut *node.rhs.clone().unwrap(), prog).clone()));

                match node.lhs {
                    Some(ref lhs) => {
                        node.ty = lhs.ty.clone();
                    }
                    None => { }
                }
                return node;
            }
        NodeType::COMMA => {
            node.lhs = Some(Box::new(walk(&mut *node.lhs.clone().unwrap(), prog).clone()));
            node.rhs = Some(Box::new(walk(&mut *node.rhs.clone().unwrap(), prog).clone()));
            node.ty = node.rhs.clone().unwrap().ty;
            return node;
        }
        NodeType::POST_INC |
            NodeType::POST_DEC |
            NodeType::NEG |
            NodeType::EXCLAM |
            NodeType::NOT => {
            node.expr = Some(Box::new(walk(&mut *node.expr.clone().unwrap(), prog).clone()));
            node.ty = node.expr.clone().unwrap().ty;
            return node;
        }
        NodeType::ADDR => {
            node.expr = Some(Box::new(walk(&mut *node.expr.clone().unwrap(), prog).clone()));
            check_lval(node.expr.clone().unwrap());
            node.ty = Rc::new(RefCell::new(ptr_to(node.expr.clone().unwrap().ty)));
            return node;
        }
        NodeType::DEREF => {
            node.expr = Some(Box::new(walk(&mut *node.expr.clone().unwrap(), prog).clone()));

            match &mut node.expr {
                Some(ref expr) => {
                    if expr.ty.borrow().ty != CType::PTR {
                        bad_node!(node, format!("operand must be a pointer: {:?}", expr));
                    }

                    let ptr = expr.ty.borrow().ptr_to.clone().unwrap();
                    if ptr.borrow().ty == CType::VOID {
                        bad_node!(node, "operand dereference void pointer");
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
            node.expr = Some(Box::new(walk(&mut *node.expr.clone().unwrap(), prog).clone()));
            return node;
        }
        NodeType::SIZEOF => {
            let mut nexpr = *node.expr.clone().unwrap();
            let expr = walk_noconv(&mut nexpr, prog);
            let val = expr.ty.borrow().size;

            *node = new_int_node(val, expr.token.clone());
            return node;
        }
        NodeType::ALIGNOF => {
            let mut e = node.expr.clone().unwrap();
            let expr = walk_noconv(&mut e, prog);
            *node = new_int_node(expr.ty.borrow().align, expr.token.clone());
            return node;
        }
        NodeType::CALL => {
            let var = find_var(&node.name);
            let mut is_int = true;
            if let Some(v) = var {
                let ty = v.borrow().clone().ty;
                if ty.ty == CType::FUNC {
                    node.ty = Rc::new(RefCell::new(*ty.returning.unwrap()));
                    is_int = false;
                }
            }
            if is_int {
                warn_node!(node, "undefined function");
                node.ty = Rc::new(RefCell::new(int_ty()));
            }

            for i in 0..node.args.len() {
                node.args[i] = walk(&mut node.args[i], prog).clone();
            }
            return node;
        }
        NodeType::COMP_STMT => {
            env_push();
            for i in 0..node.stmts.len() {
                node.stmts[i] = walk(&mut node.stmts[i], prog).clone();
            }
            env_pop();
            return node;
        }
        NodeType::STMT_EXPR => {
            node.body = Some(Box::new(walk(&mut *node.body.clone().unwrap(), prog).clone()));
            node.ty = Rc::new(RefCell::new(int_ty()));
            return node;
        }
        _ => {
            panic!("unknown node type: {:?}", node);
        }
    }
}

fn sema_gvar(node: & Node) -> Var {
    let var = Rc::new(RefCell::new(alloc_var()));
    var.borrow_mut().ty = node.ty.borrow().clone();
    var.borrow_mut().is_local = false;
    var.borrow_mut().name = node.name.clone();
    var.borrow_mut().data = node.data.clone();
    var.borrow_mut().len = node.len;
    env_var_put(node.name.clone(), var.clone());
    return var.borrow().clone();
}

fn sema_funcdef(node: &mut Node, prog: &mut Program) {
    init_locals();

    for i in 0..node.args.len() {
        node.args[i] = walk(&mut node.args[i], prog).clone();
    }
    node.body = Some(Box::new(walk(&mut *node.body.clone().unwrap(), prog).clone()));

    let mut off = 0;
    for v in locals().iter_mut() {
        off = roundup(off, v.borrow().ty.align);
        off += v.borrow().ty.size;
        v.borrow_mut().offset = off;
    }
    node.stacksize = off;
}

pub fn sema(prog: &mut Program) {
    let mut nodes = prog.nodes.clone();
    for node in nodes.iter_mut() {
        if node.op == NodeType::VARDEF {
            prog.gvars.push(sema_gvar(node));
            continue;
        }

        let mut var = Rc::new(RefCell::new(alloc_var()));
        var.borrow_mut().ty = node.ty.borrow().clone();
        var.borrow_mut().is_local = false;
        var.borrow_mut().name = node.name.clone();
        env_var_put(node.name.clone(), var);

        if node.op == NodeType::FUNC {
            sema_funcdef(node, prog);
            continue;
        }

        assert!(node.op == NodeType::DECL);
    }

    prog.nodes = nodes;
}
