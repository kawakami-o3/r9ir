
use crate::token::*;
use std::sync::Mutex;

// Recursive-descendent parser

lazy_static! {
    static ref POS: Mutex<usize> = Mutex::new(0);
}

#[derive(Clone, Debug, PartialEq)]
pub enum NodeType {
    NUM,
    ADD,
    SUB,
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

fn number(tokens: &Vec<Token>, pos: &mut usize) -> Node {
    let t = &tokens[*pos];
    if t.ty == TokenType::NUM {
        let val = t.val;
        *pos += 1;
        return new_node_num(val as usize);
    }
    panic!("number expected, but got {}", t.input);
}

fn expr(tokens: &Vec<Token>, pos: &mut usize) -> Node {
    let mut lhs = number(tokens, pos);
    loop {
        let ty = match &tokens[*pos].ty {
            TokenType::ADD => NodeType::ADD,
            TokenType::SUB => NodeType::SUB,
            _ => {
                break;
            }
        };
        //let ty = to_node_type(&tokens[*pos].ty);
        *pos += 1;
        lhs = new_node(ty, lhs, number(tokens, pos));
    }

    if tokens[*pos].ty != TokenType::EOF {
        panic!("stray token: {}", tokens[*pos].input);
    }
    return lhs;
}

pub fn parse(tokens: &Vec<Token>) -> Node {
    let mut pos = POS.lock().unwrap();
    return expr(tokens, &mut pos);
}
