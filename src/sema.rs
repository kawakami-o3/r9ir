
use crate::parse::*;
use std::collections::HashMap;
use std::sync::Mutex;

lazy_static! {
    static ref VARS: Mutex<HashMap<String, i32>> = Mutex::new(HashMap::new());
    static ref STACKSIZE: Mutex<i32> = Mutex::new(0);
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

fn vars_get(name: & String) -> i32 {
    let vars = VARS.lock().unwrap();
    return *vars.get(name).unwrap();
}

fn vars_put(name: String, stacksize: i32) {
    let mut vars = VARS.lock().unwrap();
    vars.insert(name, stacksize);
}

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
    match node.ty {
        NodeType::NUM => { }
        NodeType::IDENT => {
            if !vars_exist(&node.name) {
                panic!("undefined variable: {}", node.name);
            }

            node.ty = NodeType::LVAR;
            node.offset = vars_get(&node.name);
        }
        NodeType::VARDEF => {
            add_stacksize(8);
            vars_put(node.name.clone(), stacksize());

            node.offset = stacksize();
            match &mut node.init {
                Some(init) => {
                    walk(init);
                }
                None => {}
            }
        }
        NodeType::IF => {
             match &mut node.cond {
                Some(cond) => {
                    walk(cond);
                }
                None => {}
             }
             match &mut node.then {
                Some(then) => {
                    walk(then);
                }
                None => {}
             }
             match &mut node.els {
                Some(els) => {
                    walk(els);
                }
                None => {}
             }
        }
        NodeType::FOR => {
            match &mut node.init {
                Some(init) => {
                    walk(init);
                }
                None => {}
            }
            match &mut node.cond {
                Some(cond) => {
                    walk(cond);
                }
                None => {}
            }

            match &mut node.inc {
                Some(inc) => {
                    walk(inc);
                }
                None => {}
            }
            match &mut node.body {
                Some(body) => {
                    walk(body);
                }
                None => {}
            }
        }
        NodeType::ADD |
            NodeType::SUB |
            NodeType::MUL |
            NodeType::DIV |
            NodeType::EQ |
            NodeType::LT |
            NodeType::LOGAND |
            NodeType::LOGOR => {
                match &mut node.lhs {
                    Some(lhs) => {
                        walk(lhs);
                    }
                    None => {}
                }
                match &mut node.rhs {
                    Some(rhs) => {
                        walk(rhs);
                    }
                    None => {}
                }
            }
        NodeType::RETURN => {
            /*
            match &mut node.expr {
                Some(expr) => {
                    walk(expr);
                }
                None => {}
            }
            */
            walk_some!(node.expr);
        }
        NodeType::CALL => {
            for n in &mut node.args {
                walk(n);
            }
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
        assert!(node.ty == NodeType::FUNC);

        init_stacksize();
        walk(node);
        node.stacksize = stacksize();
    }
}
