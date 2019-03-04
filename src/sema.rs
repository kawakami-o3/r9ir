
use crate::parse::*;
use std::collections::HashMap;
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

macro_rules! walk_some {
    ($opt:expr) => {
        match &mut $opt {
            Some(n) => {
                walk(n);
            }
            None => {}
        };
    };
}

fn walk(node: &mut Node) {
    match node.op {
        NodeType::NUM => { }
        NodeType::IDENT => {
            match VARS.lock().unwrap().get(&node.name) {
                None => {
                    panic!("undefined variable: {}", node.name);
                }
                Some(var) => {
                    node.ty = var.ty.clone();
                    node.op = NodeType::LVAR;
                    node.offset = var.offset;
                }
            }
        }
        NodeType::VARDEF => {
            add_stacksize(8);
            node.offset = stacksize();

            let var = Var {
                ty: node.ty.clone(),
                offset: stacksize(),
            };
            vars_put(node.name.clone(), var.clone());

            walk_some!(node.init);
        }
        NodeType::IF => {
            walk_some!(node.cond);
            walk_some!(node.then);
            walk_some!(node.els);
        }
        NodeType::FOR => {
            walk_some!(node.init);
            walk_some!(node.cond);
            walk_some!(node.inc);
            walk_some!(node.body);
        }
        NodeType::ADD |
            NodeType::SUB |
            NodeType::MUL |
            NodeType::DIV |
            NodeType::EQ |
            NodeType::LT |
            NodeType::LOGAND |
            NodeType::LOGOR => {
                walk_some!(node.lhs);
                walk_some!(node.rhs);

                match node.lhs {
                    Some(ref lhs) => {
                        node.ty = lhs.ty.clone();
                    }
                    None => { }
                }
            }
        NodeType::RETURN | NodeType::DEREF => {
            walk_some!(node.expr);
        }
        NodeType::CALL => {
            for n in &mut node.args {
                walk(n);
            }
            node.ty = int_ty();
        }
        NodeType::FUNC => {
            for n in &mut node.args {
                walk(n);
            }
            walk_some!(node.body);
        }
        NodeType::COMP_STMT => {
            for n in &mut node.stmts {
                walk(n);
            }
        }
        NodeType::EXPR_STMT => {
            walk_some!(node.expr);
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
        walk(node);
        node.stacksize = stacksize();
    }
}
