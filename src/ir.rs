use crate::parse::*;
use std::sync::Mutex;

// Intermediate representation

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

#[derive(Debug, PartialEq)]
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

#[derive(Debug)]
pub struct IR {
    pub op: IRType,
    pub lhs: usize,
    pub rhs: usize,
}

fn new_ir(op: IRType, lhs: usize, rhs: usize) -> IR {
    IR {
        op: op,
        lhs: lhs,
        rhs: rhs,
    }
}

lazy_static! {
    static ref REGNO: Mutex<usize> = Mutex::new(0);
}

fn gen(v: &mut Vec<IR>, node: Node) -> usize {
    if node.ty == NodeType::NUM {
        let mut regno = REGNO.lock().unwrap();

        let r = *regno;
        (*regno) += 1;
        v.push(new_ir(IRType::IMM, r, node.val as usize));
        return r;
    }

    assert!(
        node.ty == NodeType::ADD
            || node.ty == NodeType::SUB
            || node.ty == NodeType::MUL
            || node.ty == NodeType::DIV,
        "not op"
    );

    let lhs = gen(v, *node.lhs.unwrap());
    let rhs = gen(v, *node.rhs.unwrap());

    v.push(new_ir(to_ir_type(&node.ty), lhs, rhs));
    v.push(new_ir(IRType::KILL, rhs, 0));
    return lhs;
}

pub fn gen_ir(node: Node) -> Vec<IR> {
    let mut v = Vec::new();
    let r = gen(&mut v, node);
    v.push(new_ir(IRType::RETURN, r, 0));
    return v;
}
