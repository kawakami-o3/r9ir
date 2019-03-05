
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

    let mut copy = alloc_node();
    mem::replace(&mut copy, base);
    node.expr = Some(Box::new(copy));
    return node;
}

macro_rules! walk_some {
    ($opt:expr, $decay:expr) => {
        match &mut $opt {
            Some(n) => {
                walk(n, $decay);
            }
            None => {}
        };
    };
}

fn walk(node: &mut Node, decay: bool) {
    match node.op {
        NodeType::NUM => { }
        NodeType::IDENT => {
            match VARS.lock().unwrap().get(&node.name) {
                None => {
                    panic!("undefined variable: {}", node.name);
                }
                Some(var) => {
                    //node.ty = var.ty.clone();
                    node.op = NodeType::LVAR;
                    node.offset = var.offset;

                    if decay && var.ty.ty == CType::ARY {
                        let node_tmp = node.clone();
                        mem::replace(node, addr_of(node_tmp, *var.clone().ty.ary_of.unwrap()));
                    } else {
                        node.ty = var.clone().ty;
                    }
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

            walk_some!(node.init, true);
        }
        NodeType::IF => {
            walk_some!(node.cond, true);
            walk_some!(node.then, true);
            walk_some!(node.els, true);
        }
        NodeType::FOR => {
            walk_some!(node.init, true);
            walk_some!(node.cond, true);
            walk_some!(node.inc, true);
            walk_some!(node.body, true);
        }
        NodeType::ADD | NodeType::SUB => {
            walk_some!(node.lhs, true);
            walk_some!(node.rhs, true);

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

        }
        NodeType::EQ => {
            walk_some!(node.lhs, false);
            walk_some!(node.rhs, true);
            node.ty = node.clone().lhs.unwrap().ty;
        }
        NodeType::MUL |
            NodeType::DIV |
            NodeType::LT |
            NodeType::LOGAND |
            NodeType::LOGOR => {
                walk_some!(node.lhs, true);
                walk_some!(node.rhs, true);

                match node.lhs {
                    Some(ref lhs) => {
                        node.ty = lhs.ty.clone();
                    }
                    None => { }
                }
            }
        NodeType::DEREF => {
            walk_some!(node.expr, true);
            match &mut node.expr {
                Some(ref expr) => {
                    if expr.ty.ty != CType::PTR {
                        panic!("operand must be a pointer");
                    }
                }
                None => {}
            }

            node.ty = *node.clone().expr.unwrap().ty.ptr_of.unwrap();
        }
        NodeType::RETURN => {
            walk_some!(node.expr, true);
        }
        NodeType::CALL => {
            for n in &mut node.args {
                walk(n, true);
            }
            node.ty = int_ty();
        }
        NodeType::FUNC => {
            for n in &mut node.args {
                walk(n, true);
            }
            walk_some!(node.body, true);
        }
        NodeType::COMP_STMT => {
            for n in &mut node.stmts {
                walk(n, true);
            }
        }
        NodeType::EXPR_STMT => {
            walk_some!(node.expr, true);
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
