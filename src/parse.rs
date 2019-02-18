// Recursive-descendent parser

#![allow(non_camel_case_types)]

use crate::token::*;
use std::sync::Mutex;

fn to_node_type(ty: &TokenType) -> NodeType {
    match ty {
        TokenType::ADD => NodeType::ADD,
        TokenType::SUB => NodeType::SUB,
        TokenType::MUL => NodeType::MUL,
        TokenType::DIV => NodeType::DIV,
        _ => {
            panic!(format!("unknown TokenType {:?}", ty));
        }
    }
}

fn pos() -> usize {
    let i = *POS.lock().unwrap();
    return i;
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
    EQ,
    IDENT,
    RETURN,
    COMP_STMT,
    EXPR_STMT,
}

#[derive(Debug)]
pub struct Node {
    pub ty: NodeType,
    pub lhs: Option<Box<Node>>,
    pub rhs: Option<Box<Node>>,
    pub val: i32,
    pub name: String,
    pub expr: Option<Box<Node>>,
    pub stmts: Vec<Node>,
}

fn expect(ty: TokenType, tokens: &Vec<Token>) {
    let t = &tokens[pos()];
    if t.ty != ty {
        panic!(format!("{:?} expected, but got {:?} ", ty, t.ty));
    }
    inc_pos();
}

fn consume(ty: TokenType, tokens: &Vec<Token>) -> bool {
    let t = &tokens[pos()];
    if t.ty != ty {
        return false;
    }
    inc_pos();
    return true;
}

fn new_node(ty: NodeType, lhs: Node, rhs: Node) -> Node {
    Node {
        ty: ty,
        lhs: Some(Box::new(lhs)),
        rhs: Some(Box::new(rhs)),
        val: 0,
        name: String::new(),
        expr: None,
        stmts: Vec::new(),
    }
}

fn term(tokens: &Vec<Token>) -> Node {
    let t = &tokens[pos()];
    inc_pos();

    if t.ty == TokenType::BRA {
        let node = assign(tokens);
        expect(TokenType::KET, tokens);
        return node;
    }

    if t.ty == TokenType::NUM {
        return Node {
            ty: NodeType::NUM,
            lhs: None,
            rhs: None,
            val: t.val,
            name: String::new(),
            expr: None,
            stmts: Vec::new(),
        };
    }

    if t.ty == TokenType::IDENT {
        return Node {
            ty: NodeType::IDENT,
            lhs: None,
            rhs: None,
            val: t.val,
            name: t.name.clone(),
            expr: None,
            stmts: Vec::new(),
        };
    }

    panic!(format!("number expected, but got {}", t.input));
}

fn mul(tokens: &Vec<Token>) -> Node {
    let mut lhs = term(tokens);
    loop {
        let t = &tokens[pos()];
        let op = &t.ty;
        if *op != TokenType::MUL && *op != TokenType::DIV {
            return lhs;
        }
        inc_pos();
        lhs = new_node(to_node_type(op), lhs, term(tokens));
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

fn assign(tokens: &Vec<Token>) -> Node {
    let lhs = expr(tokens);
    if consume(TokenType::EQ, tokens) {
        return new_node(NodeType::EQ, lhs, expr(tokens));
    }
    return lhs;
}

pub fn stmt(tokens: &Vec<Token>) -> Node {
    let mut node = Node {
        ty: NodeType::COMP_STMT,
        lhs: None,
        rhs: None,
        val: 0,
        name: String::new(),
        expr: None,
        stmts: Vec::new(),
    };

    loop {
        let t = &tokens[pos()];
        if t.ty == TokenType::EOF {
            return node;
        }

        let e = match t.ty {
            TokenType::RETURN => {
                inc_pos();
                Node {
                    ty: NodeType::RETURN,

                    lhs: None,
                    rhs: None,
                    val: 0,
                    name: String::new(),
                    expr: Some(Box::new(assign(tokens))),
                    stmts: Vec::new(),
                }
            }
            _ => {
                Node {
                    ty: NodeType::EXPR_STMT,

                    lhs: None,
                    rhs: None,
                    val: 0,
                    name: String::new(),
                    expr: Some(Box::new(assign(tokens))),
                    stmts: Vec::new(),
                }
            },
        };

        node.stmts.push(e);
        expect(TokenType::SEMI_COLON, tokens);
    }
}

pub fn parse(tokens: &Vec<Token>) -> Node {
    return stmt(tokens);
}
