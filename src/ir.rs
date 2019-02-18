// Intermediate representation
#![allow(dead_code)]

use crate::parse::*;
use std::collections::HashMap;
use std::sync::Mutex;

fn to_ir_type(node_type: &NodeType) -> IRType {
    match node_type {
        NodeType::ADD => IRType::ADD,
        NodeType::SUB => IRType::SUB,
        NodeType::MUL => IRType::MUL,
        NodeType::DIV => IRType::DIV,
        _ => {
            panic!(format!("unknown NodeType {:?}", node_type));
        }
    }
}

lazy_static! {
    static ref CODE: Mutex<Vec<IR>> = Mutex::new(Vec::new());
    static ref REGNO: Mutex<i32> = Mutex::new(1);
    static ref BASEREG: Mutex<i32> = Mutex::new(0);
    
    static ref VARS: Mutex<HashMap<String,i32>> = Mutex::new(HashMap::new());
    static ref BPOFF: Mutex<i32> = Mutex::new(0);
}

fn basereg() -> i32 {
    *BASEREG.lock().unwrap()
}

#[derive(Clone, Debug, PartialEq)]
pub enum IRType {
    IMM,
    MOV,
    RETURN,
    ALLOCA,
    LOAD,
    STORE,
    KILL,
    ADD,
    SUB,
    MUL,
    DIV,
    NOP,
}

#[derive(Clone, Debug)]
pub struct IR {
    pub op: IRType,
    pub lhs: i32,
    pub rhs: i32,
}

fn add(op: IRType, lhs: i32, rhs: i32) -> usize {
    let ir = IR {
        op: op,
        lhs: lhs,
        rhs: rhs,
    };

    match CODE.lock() {
        Ok(mut code) => {
            (*code).push(ir);
            return code.len() - 1;
        }
        Err(_) => {
            panic!();
        }
    }
}

fn gen_lval(node: Node) -> i32 {
    if node.ty != NodeType::IDENT {
        panic!("not an lvaue");
    }

    let mut vars = VARS.lock().unwrap();
    if None == vars.get(&node.name) {
        let mut bpoff = BPOFF.lock().unwrap();
        (*vars).insert(node.name.clone(), *bpoff);
        *bpoff += 8;
    }

    let mut regno = REGNO.lock().unwrap();
    let r1 = *regno;
    *regno+=1;

    let off = *vars.get(&node.name).unwrap();
    let basereg = BASEREG.lock().unwrap();
    add(IRType::MOV, r1, *basereg);

    let r2 = *regno;
    *regno += 1;
    add(IRType::IMM, r2, off);
    add(IRType::ADD, r1, r2);
    add(IRType::KILL, r2, -1);

    return r1;
}

fn gen_expr(node: Node) -> i32 {
    if node.ty == NodeType::NUM {
        let mut regno = REGNO.lock().unwrap();

        let r = *regno;
        *regno += 1;
        add(IRType::IMM, r, node.val);
        return r;
    }

    if node.ty == NodeType::IDENT {
        let r = gen_lval(node);
        add(IRType::LOAD, r, r);
        return r;
    }

    if node.ty == NodeType::EQ {
        let rhs = gen_expr(*node.rhs.unwrap());
        let lhs = gen_lval(*node.lhs.unwrap());
        add(IRType::STORE, lhs, rhs);
        add(IRType::KILL, rhs, -1);
        return lhs;
    }

    assert!(
        node.ty == NodeType::ADD
            || node.ty == NodeType::SUB
            || node.ty == NodeType::MUL
            || node.ty == NodeType::DIV,
        "not op"
    );

    let lhs = gen_expr(*node.lhs.unwrap());
    let rhs = gen_expr(*node.rhs.unwrap());

    add(to_ir_type(&node.ty), lhs, rhs);
    add(IRType::KILL, rhs, -1);
    return lhs;
}

fn gen_stmt(node: Node) {
    match node.ty {
        NodeType::RETURN => {
            let r = gen_expr(*node.expr.unwrap());
            add(IRType::RETURN, r, -1);
            add(IRType::KILL, r, -1);
        }
        NodeType::EXPR_STMT => {
            let r = gen_expr(*node.expr.unwrap());
            add(IRType::KILL, r, -1);
        }
        NodeType::COMP_STMT => {
            for n in node.stmts {
                gen_stmt(n);
            }
        }
        _ => {
            panic!(format!("unknown node: {:?}", node.ty));
        }
    }
}

pub fn gen_ir(node: Node) -> Vec<IR> {
    assert!(node.ty == NodeType::COMP_STMT, "");

    let alloca_idx = add(IRType::ALLOCA, basereg(), -1);
    gen_stmt(node);

    let mut code = CODE.lock().unwrap();
    code[alloca_idx].rhs = *BPOFF.lock().unwrap();
    return code.clone();
}

