// Intermediate representation
#![allow(dead_code)]

use crate::parse::*;
use std::sync::Mutex;

fn to_ir_type(node_type: &NodeType) -> IRType {
    match node_type {
        NodeType::ADD => IRType::ADD,
        NodeType::SUB => IRType::SUB,
        NodeType::MUL => IRType::MUL,
        NodeType::DIV => IRType::DIV,
        _ => {
            panic!();
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum IRType {
    IMM,
    MOV,
    RETURN,
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
    pub lhs: usize,
    pub rhs: usize,
}

fn add(op: IRType, lhs: usize, rhs: usize) {
    let ir = IR {
        op: op,
        lhs: lhs,
        rhs: rhs,
    };

    CODE.lock().unwrap().push(ir);
}

lazy_static! {
    static ref CODE: Mutex<Vec<IR>> = Mutex::new(Vec::new());
    static ref REGNO: Mutex<usize> = Mutex::new(0);
}

fn gen_expr(node: Node) -> usize {
    if node.ty == NodeType::NUM {
        let mut regno = REGNO.lock().unwrap();

        let r = *regno;
        (*regno) += 1;
        add(IRType::IMM, r, node.val as usize);
        return r;
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
    add(IRType::KILL, rhs, 0);
    return lhs;
}

fn gen_stmt(node: Node) {
    match node.ty {
        NodeType::RETURN => {
            let r = gen_expr(*node.expr.unwrap());
            add(IRType::RETURN, r, 0);
            add(IRType::KILL, r, 0);
        }
        NodeType::EXPR_STMT => {
            let r = gen_expr(*node.expr.unwrap());
            add(IRType::KILL, r, 0);
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
    gen_stmt(node);
    return CODE.lock().unwrap().clone();
}
