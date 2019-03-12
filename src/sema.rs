
use crate::parse::*;
use crate::util::*;
use std::collections::HashMap;
use std::mem;
use std::sync::Mutex;

lazy_static! {
    static ref STR_LABEL: Mutex<i32> = Mutex::new(0);
    static ref GLOBALS: Mutex<Vec<Var>> = Mutex::new(Vec::new());
    static ref STACKSIZE: Mutex<i32> = Mutex::new(0);
}

#[derive(Clone, Debug)]
struct Env<'a> {
    vars: HashMap<String, Var>,
    next: Option<Box<&'a Env<'a>>>,
}

fn new_env<'a>(next: Option<&'a Env>) -> Env<'a> {
    Env {
        vars: HashMap::new(),
        next: if let Some(e) = next {
            Some(Box::new(e))
        } else {
            None
        },
    }
}

impl<'a> Env<'a> {
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
    pub data: String,
    pub len: usize,
}

fn alloc_var(ty: Type) -> Var {
    Var {
        ty: ty,
        is_local: true,
        offset: 0,
        name: String::new(),
        data: String::new(),
        len: 0,
    }
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

fn walk<'a>(env: &'a mut Env, node: &'a mut Node, decay: bool) -> &'a Node {
    match node.op {
        NodeType::NUM => {
            return node;
        }
        NodeType::STR => {
            let name = format!(".L.str{}", bump_str_label());
            let tmp = node.clone();
            let node_ty = tmp.ty;
            let node_str = tmp.str_cnt;

            let mut var = alloc_var(node.clone().ty);
            var.is_local = false;
            var.name = String::from(name.clone());
            var.len = node_str.len() + 1;
            var.data = node_str;
            globals_push(var.clone());

            *node = alloc_node();
            node.op = NodeType::GVAR;
            node.ty = node_ty;
            node.name = name;
            return walk(env, node, decay);
        }
        NodeType::IDENT => {
            match env.find(&node.name) {
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
        NodeType::GVAR => {
            if decay && node.ty.ty == CType::ARY {
                *node = addr_of(node.clone(), *node.clone().ty.ary_of.unwrap());
            }
            return node;
        }
        NodeType::VARDEF => {
            add_stacksize(size_of(node.clone().ty));
            node.offset = stacksize();

            let var = Var {
                ty: node.ty.clone(),
                is_local: true,
                offset: stacksize(),
                name: String::new(),
                data: String::new(),
                len: 0,
            };
            env.vars.insert(node.name.clone(), var.clone());

            match node.init {
                Some(_) => {
                    node.init = Some(Box::new(walk(env, &mut *node.clone().init.unwrap(), true).clone()));
                }
                None => { }
            }
            return node;
        }
        NodeType::IF => {
            node.cond = Some(Box::new(walk(env, &mut *node.clone().cond.unwrap(), true).clone()));
            node.then = Some(Box::new(walk(env, &mut *node.clone().then.unwrap(), true).clone()));

            if node.els.is_some() {
                node.els = Some(Box::new(walk(env, &mut *node.clone().els.unwrap(), true).clone()));
            }
            return node;
        }
        NodeType::FOR => {
            node.init = Some(Box::new(walk(env, &mut *node.clone().init.unwrap(), true).clone()));
            node.cond = Some(Box::new(walk(env, &mut *node.clone().cond.unwrap(), true).clone()));
            node.inc = Some(Box::new(walk(env, &mut *node.clone().inc.unwrap(), true).clone()));
            node.body = Some(Box::new(walk(env, &mut *node.clone().body.unwrap(), true).clone()));
            return node;
        }
        NodeType::ADD | NodeType::SUB => {
            node.lhs = Some(Box::new(walk(env, &mut *node.clone().lhs.unwrap(), true).clone()));
            node.rhs = Some(Box::new(walk(env, &mut *node.clone().rhs.unwrap(), true).clone()));

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
            node.lhs = Some(Box::new(walk(env, &mut *node.clone().lhs.unwrap(), false).clone()));
            if node.clone().lhs.unwrap().op != NodeType::LVAR && node.clone().lhs.unwrap().op != NodeType::DEREF {
                panic!("not an lvalue: {:?} ({})", node.op, node.name);
            }


            node.rhs = Some(Box::new(walk(env, &mut *node.clone().rhs.unwrap(), true).clone()));
            node.ty = node.clone().lhs.unwrap().ty;
            return node;
        }
        NodeType::MUL |
            NodeType::DIV |
            NodeType::LT |
            NodeType::LOGAND |
            NodeType::LOGOR => {
                node.lhs = Some(Box::new(walk(env, &mut *node.clone().lhs.unwrap(), true).clone()));
                node.rhs = Some(Box::new(walk(env, &mut *node.clone().rhs.unwrap(), true).clone()));

                match node.lhs {
                    Some(ref lhs) => {
                        node.ty = lhs.ty.clone();
                    }
                    None => { }
                }
                return node;
            }
        NodeType::ADDR => {
            node.expr = Some(Box::new(walk(env, &mut *node.clone().expr.unwrap(), true).clone()));
            node.ty = ptr_of(node.clone().expr.unwrap().ty);
            return node;
        }
        NodeType::DEREF => {
            node.expr = Some(Box::new(walk(env, &mut *node.clone().expr.unwrap(), true).clone()));
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
            node.expr = Some(Box::new(walk(env, &mut *node.clone().expr.unwrap(), true).clone()));
            return node;
        }
        NodeType::SIZEOF => {
            let mut nexpr = *node.clone().expr.unwrap();
            let expr = walk(env, &mut nexpr, false);
            let val = size_of(expr.clone().ty);

            *node = alloc_node();
            node.op = NodeType::NUM;
            node.ty = int_ty();
            node.val = val;
            return node;
        }
        NodeType::CALL => {
            for i in 0..node.args.len() {
                node.args[i] = walk(env, &mut node.args[i], true).clone();
            }
            node.ty = int_ty();
            return node;
        }
        NodeType::FUNC => {
            for i in 0..node.args.len() {
                node.args[i] = walk(env, &mut node.args[i], true).clone();
            }
            node.body = Some(Box::new(walk(env, &mut *node.clone().body.unwrap(), true).clone()));
            return node;
        }
        NodeType::COMP_STMT => {
            let mut newenv = new_env(Some(env));
            for i in 0..node.stmts.len() {
                node.stmts[i] = walk(&mut newenv, &mut node.stmts[i], true).clone();
            }
            return node;
        }
        NodeType::EXPR_STMT => {
            node.expr = Some(Box::new(walk(env, &mut *node.clone().expr.unwrap(), true).clone()));
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
        init_globals();
        let mut env = new_env(None);
        walk(&mut env, node, true);
        node.stacksize = stacksize();
        node.globals = globals();
    }
}
