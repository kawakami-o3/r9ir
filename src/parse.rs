use crate::token::*;
use std::sync::Mutex;

// Recursive-descendent parser

// ----------------------------------------------
fn to_node_type(ty: &TokenType) -> NodeType {
    match ty {
        TokenType::ADD => NodeType::ADD,
        TokenType::SUB => NodeType::SUB,
        TokenType::MUL => NodeType::MUL,
        TokenType::DIV => NodeType::DIV,
        _ => {
            panic!();
        }
    }
}

fn pos() -> usize {
    *POS.lock().unwrap()
}

fn inc_pos() {
    match POS.lock() {
        Ok(mut pos) => {
            *pos += 1;
        }
        _ => {
            panic!();
        }
    }
}
// ----------------------------------------------

lazy_static! {
    static ref POS: Mutex<usize> = Mutex::new(0);
}

#[derive(Clone, Debug, PartialEq)]
pub enum NodeType {
    NUM,
    ADD,
    SUB,
    MUL,
    DIV,
}

#[derive(Debug)]
pub struct Node {
    pub ty: NodeType,
    pub lhs: Option<Box<Node>>,
    pub rhs: Option<Box<Node>>,
    pub val: usize,
}

fn new_node(ty: NodeType, lhs: Node, rhs: Node) -> Node {
    Node {
        ty: ty,
        lhs: Some(Box::new(lhs)),
        rhs: Some(Box::new(rhs)),
        val: 0,
    }
}

fn new_node_num(val: usize) -> Node {
    Node {
        ty: NodeType::NUM,
        lhs: None,
        rhs: None,
        val: val,
    }
}

fn number(tokens: &Vec<Token>) -> Node {
    let t = &tokens[pos()];
    if t.ty != TokenType::NUM {
        panic!("number expected, but got {}", t.input);
    }
    inc_pos();
    return new_node_num(t.val as usize);
}

fn mul(tokens: &Vec<Token>) -> Node {
    let mut lhs = number(tokens);
    loop {
        let t = &tokens[pos()];
        let op = &t.ty;
        if *op != TokenType::MUL && *op != TokenType::DIV {
            return lhs;
        }
        inc_pos();
        lhs = new_node(to_node_type(op), lhs, number(tokens));
    }
}

fn expr(tokens: &Vec<Token>) -> Node {
    let mut lhs = mul(tokens);
    loop {
        let t = &tokens[pos()];
        let op = &t.ty;
        if *op != TokenType::ADD && *op != TokenType::SUB {
            return lhs;
        }
        inc_pos();
        lhs = new_node(to_node_type(op), lhs, mul(tokens));
    }
}

pub fn parse(tokens: &Vec<Token>) -> Node {
    let node = expr(tokens);

    let t = &tokens[pos()];
    if t.ty != TokenType::EOF {
        panic!("stray token: {}", t.input);
    }
    return node;
}
