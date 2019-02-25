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
    let mut pos = POS.lock().unwrap();
    *pos += 1;
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
    IF,
    RETURN,
    CALL,
    FUNC,
    COMP_STMT,
    EXPR_STMT,
}

#[derive(Clone, Debug)]
pub struct Node {
    pub ty: NodeType,
    pub lhs: Option<Box<Node>>,
    pub rhs: Option<Box<Node>>,
    pub val: i32,
    pub expr: Option<Box<Node>>,
    pub stmts: Vec<Node>,

    pub name: String,

    // "if"
    pub cond: Option<Box<Node>>,
    pub then: Option<Box<Node>>,
    pub els: Option<Box<Node>>,

    // Function definition
    pub body: Option<Box<Node>>,

    // Function call
    pub args: Vec<Node>,
}

// default node
fn alloc_node() -> Node {
    Node {
        ty: NodeType::NUM,
        lhs: None,
        rhs: None,
        val: 0,
        expr: None,
        stmts: Vec::new(),

        name: String::new(),

        cond: None,
        then: None,
        els: None,

        body: None,

        args: Vec::new(),
    }
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
    let mut node = alloc_node();
    node.ty = ty;
    node.lhs = Some(Box::new(lhs));
    node.rhs = Some(Box::new(rhs));
    return node;
}

fn term(tokens: &Vec<Token>) -> Node {
    let t = &tokens[pos()];
    inc_pos();

    if t.ty == TokenType::BRA {
        let node = assign(tokens);
        expect(TokenType::KET, tokens);
        return node;
    }

    let mut node = alloc_node();
    if t.ty == TokenType::NUM {
        node.ty = NodeType::NUM;
        node.val = t.val;
        return node;
    }

    if t.ty == TokenType::IDENT {
        node.name = t.name.clone();

        if !consume(TokenType::BRA, tokens) {
            node.ty = NodeType::IDENT;
            return node;
        }

        node.ty = NodeType::CALL;
        if consume(TokenType::KET, tokens) {
            return node;
        }

        node.args.push(assign(tokens));
        while consume(TokenType::COMMA, tokens) {
            node.args.push(assign(tokens));
        }
        expect(TokenType::KET, tokens);
        return node;
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
    let mut node = alloc_node();
    let t = &tokens[pos()];

    match t.ty {
        TokenType::IF => {
            inc_pos();
            node.ty = NodeType::IF;
            expect(TokenType::BRA, tokens);
            node.cond = Some(Box::new(assign(tokens)));
            expect(TokenType::KET, tokens);
            node.then = Some(Box::new(stmt(tokens)));
            if consume(TokenType::ELSE, tokens) {
                node.els = Some(Box::new(stmt(tokens)));
            }
            return node;
        }
        TokenType::RETURN => {
            inc_pos();
            node.ty = NodeType::RETURN;
            node.expr = Some(Box::new(assign(tokens)));
            expect(TokenType::SEMI_COLON, tokens);
            return node;
        }
        _ => {
            node.ty = NodeType::EXPR_STMT;
            node.expr = Some(Box::new(assign(tokens)));
            expect(TokenType::SEMI_COLON, tokens);
            return node;
        }
    }
}

pub fn compaund_stmt(tokens: &Vec<Token>) -> Node {
    let mut node = alloc_node();
    node.ty = NodeType::COMP_STMT;

    while !consume(TokenType::C_KET, tokens) {
        node.stmts.push(stmt(tokens));
    }
    return node;
}

fn function(tokens: &Vec<Token>) -> Node {
    let mut node = alloc_node();
    node.ty = NodeType::FUNC;

    let t = &tokens[pos()];
    if t.ty != TokenType::IDENT {
        panic!(format!("function name expected, but got {}", t.input));
    }
    node.name = t.name.clone();
    inc_pos();

    expect(TokenType::BRA, tokens);
    while !consume(TokenType::KET, tokens) {
        node.args.push(term(tokens));
    }
    expect(TokenType::C_BRA, tokens);
    node.body = Some(Box::new(compaund_stmt(tokens)));
    return node;
}

pub fn parse(tokens: &Vec<Token>) -> Vec<Node> {
    let mut v = Vec::new();
    while tokens[pos()].ty != TokenType::EOF {
        v.push(function(tokens));
    }
    return v;
}
