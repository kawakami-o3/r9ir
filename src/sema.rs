
use crate::parse::*;
use crate::util::*;
use std::collections::HashMap;
use std::mem;
use std::sync::Mutex;

lazy_static! {
    static ref VARS: Mutex<HashMap<String, Var>> = Mutex::new(HashMap::new());
    static ref STACKSIZE: Mutex<i32> = Mutex::new(0);
}

#[derive(Clone, Debug, PartialEq)]
struct Var {
    ty: Type,
    offset: i32,
}

#[allow(dead_code)]
fn print_vars() {
    match VARS.lock() {
        Ok(vars) => {
            println!("[VARS]> {:?}", *vars);
        }
        _ => {}
    }
}

fn vars_put(name: String, var: Var) {
    let mut vars = VARS.lock().unwrap();
    vars.insert(name, var);
}

#[allow(dead_code)]
fn vars_exist(name: & String) -> bool {
    let vars = VARS.lock().unwrap();
    return None != vars.get(name);
}

fn stacksize() -> i32 {
    *STACKSIZE.lock().unwrap()
}

fn init_stacksize() {
    let mut stacksize = STACKSIZE.lock().unwrap();
    *stacksize = 0;
}

fn add_stacksize(i: i32) {
    let mut stacksize = STACKSIZE.lock().unwrap();
    *stacksize += i;
}

fn swap(p: &mut Node, q: &mut Node) {
    mem::swap(p, q);
}

fn addr_of(base: Node, ty: Type) -> Node {
    let mut node = alloc_node();
    node.op = NodeType::ADDR;
    node.ty = ptr_of(ty);

    node.expr = Some(Box::new(base));
    return node;
}

fn walk<'a>(node: &'a mut Node, decay: bool) -> &'a Node {
    match node.op {
        NodeType::NUM => {
            return node;
        }
        NodeType::IDENT => {
            match VARS.lock().unwrap().get(&node.name) {
                None => {
                    panic!("undefined variable: {}", node.name);
                }
                Some(var) => {
                    node.op = NodeType::LVAR;
                    node.offset = var.offset;

                    if decay && var.ty.ty == CType::ARY {
                        *node = addr_of(node.clone(), *var.clone().ty.ary_of.unwrap());
                        return node;
                    }
                    node.ty = var.clone().ty;
                    return node;
                }
            }
        }
        NodeType::VARDEF => {
            add_stacksize(size_of(node.clone().ty));
            node.offset = stacksize();

            let var = Var {
                ty: node.ty.clone(),
                offset: stacksize(),
            };
            vars_put(node.name.clone(), var.clone());

            match node.init {
                Some(_) => {
                    node.init = Some(Box::new(walk(&mut *node.clone().init.unwrap(), true).clone()));
                    //node.init = Some(Box::new(walk(&mut e, true).clone()));
                }
                None => { }
            }
            return node;
        }
        NodeType::IF => {
            node.cond = Some(Box::new(walk(&mut *node.clone().cond.unwrap(), true).clone()));
            node.then = Some(Box::new(walk(&mut *node.clone().then.unwrap(), true).clone()));

            if node.els.is_some() {
                node.els = Some(Box::new(walk(&mut *node.clone().els.unwrap(), true).clone()));
            }
            return node;
        }
        NodeType::FOR => {
            node.init = Some(Box::new(walk(&mut *node.clone().init.unwrap(), true).clone()));
            node.cond = Some(Box::new(walk(&mut *node.clone().cond.unwrap(), true).clone()));
            node.inc = Some(Box::new(walk(&mut *node.clone().inc.unwrap(), true).clone()));
            node.body = Some(Box::new(walk(&mut *node.clone().body.unwrap(), true).clone()));
            return node;
        }
        NodeType::ADD | NodeType::SUB => {
            node.lhs = Some(Box::new(walk(&mut *node.clone().lhs.unwrap(), true).clone()));
            node.rhs = Some(Box::new(walk(&mut *node.clone().rhs.unwrap(), true).clone()));

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
        NodeType::EQ => {
            node.lhs = Some(Box::new(walk(&mut *node.clone().lhs.unwrap(), false).clone()));
            node.rhs = Some(Box::new(walk(&mut *node.clone().rhs.unwrap(), true).clone()));
            node.ty = node.clone().lhs.unwrap().ty;
            return node;
        }
        NodeType::MUL |
            NodeType::DIV |
            NodeType::LT |
            NodeType::LOGAND |
            NodeType::LOGOR => {
                node.lhs = Some(Box::new(walk(&mut *node.clone().lhs.unwrap(), true).clone()));
                node.rhs = Some(Box::new(walk(&mut *node.clone().rhs.unwrap(), true).clone()));

                match node.lhs {
                    Some(ref lhs) => {
                        node.ty = lhs.ty.clone();
                    }
                    None => { }
                }
                return node;
            }
        NodeType::ADDR => {
            node.expr = Some(Box::new(walk(&mut *node.clone().expr.unwrap(), true).clone()));
            node.ty = ptr_of(node.clone().expr.unwrap().ty);
            return node;
        }
        NodeType::DEREF => {
            node.expr = Some(Box::new(walk(&mut *node.clone().expr.unwrap(), true).clone()));
            match &mut node.expr {
                Some(ref expr) => {
                    if expr.ty.ty != CType::PTR {
                        panic!("operand must be a pointer");
                    }
                }
                None => {}
            }

            node.ty = *node.clone().expr.unwrap().ty.ptr_of.unwrap();
            return node;
        }
        NodeType::RETURN => {
            node.expr = Some(Box::new(walk(&mut *node.clone().expr.unwrap(), true).clone()));
            return node;
        }
        NodeType::SIZEOF => {
            let mut nexpr = *node.clone().expr.unwrap();
            let expr = walk(&mut nexpr, false);
            let val = size_of(expr.clone().ty);

            *node = alloc_node();
            node.op = NodeType::NUM;
            node.ty = int_ty();
            node.val = val;
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
            node.body = Some(Box::new(walk(&mut *node.clone().body.unwrap(), true).clone()));
            return node;
        }
        NodeType::COMP_STMT => {
            for i in 0..node.stmts.len() {
                node.stmts[i] = walk(&mut node.stmts[i], true).clone();
            }
            return node;
        }
        NodeType::EXPR_STMT => {
            node.expr = Some(Box::new(walk(&mut *node.clone().expr.unwrap(), true).clone()));
            return node;
        }
        _ => {
            panic!("unknown node type");
        }
    }
}

pub fn sema(nodes: &mut Vec<Node>) {
    for node in nodes.iter_mut() {
        assert!(node.op == NodeType::FUNC);

        init_stacksize();
        walk(node, true);
        node.stacksize = stacksize();
    }
}
