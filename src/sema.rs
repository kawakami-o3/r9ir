
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

fn new_global(ty: Type, data: String, len: usize) -> Var {
    let mut var = alloc_var(ty);
    var.is_local = false;
    var.name = format!(".L.str{}", bump_str_label()).to_string();
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

fn maybe_decay(base: & mut Node, decay: bool) -> & mut Node {
    if !decay || base.ty.ty != CType::ARY {
        return base;
    }

    let tmp = base.clone();
    *base = alloc_node();
    base.op = NodeType::ADDR;
    base.ty = ptr_of(*tmp.clone().ty.ary_of.unwrap());

    base.expr = Some(Box::new(tmp));
    return base;
}

fn check_lval(node: & Node) {
    match node.op {
        NodeType::LVAR | NodeType::GVAR | NodeType::DEREF => {
            return;
        }
        _ => {
            panic!("not an lvalue: {:?} ({})", node.op, node.name);
        }
    }
}

fn walk<'a>(env: &'a mut Env, node: &'a mut Node, decay: bool) -> &'a Node {
    match node.op {
        NodeType::NUM => {
            return node;
        }
        NodeType::STR => {
            let node_ty = node.ty.clone();
            let node_data = node.data.clone();

            let var = new_global(node_ty.clone(), node_data, node.len);
            globals_push(var.clone());

            *node = alloc_node();
            node.op = NodeType::GVAR;
            node.ty = node_ty;
            node.name = var.name;
            return maybe_decay(node, decay);
        }
        NodeType::IDENT => {
            let var = match env.find(&node.name) {
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
            add_stacksize(size_of(& node.ty));
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
            check_lval(& *node.clone().lhs.unwrap());

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
            check_lval(& *node.clone().expr.unwrap());
            node.ty = ptr_of(node.clone().expr.unwrap().ty);
            return node;
        }
        NodeType::DEREF => {
            node.expr = Some(Box::new(walk(env, &mut *node.clone().expr.unwrap(), true).clone()));
            match &mut node.expr {
                Some(ref expr) => {
                    if expr.ty.ty != CType::PTR {
                        panic!("operand must be a pointer: {:?}", expr);
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
            let val = size_of(& expr.ty);

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

pub fn sema(nodes: &mut Vec<Node>) -> Vec<Var> {
    init_globals();
    let mut topenv = new_env(None);

    for node in nodes.iter_mut() {
        if node.op == NodeType::VARDEF {
            let var = new_global(node.ty.clone(), node.data.clone(), node.len);
            globals_push(var.clone());
            topenv.vars.insert(node.name.clone(), var);
            continue;
        }

        assert!(node.op == NodeType::FUNC);

        init_stacksize();
        walk(&mut topenv, node, true);
        node.stacksize = stacksize();
    }
    return globals();
}
