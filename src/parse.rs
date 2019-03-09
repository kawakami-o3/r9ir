// Recursive-descendent parser

#![allow(non_camel_case_types)]

use crate::token::*;
use crate::util::*;
use std::sync::Mutex;

fn to_node_type(ty: &TokenType) -> NodeType {
    match ty {
        TokenType::ADD => NodeType::ADD,
        TokenType::SUB => NodeType::SUB,
        TokenType::MUL => NodeType::MUL,
        TokenType::DIV => NodeType::DIV,
        TokenType::LOGOR => NodeType::LOGOR,
        TokenType::LOGAND => NodeType::LOGAND,
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
    ADD,
    SUB,
    MUL,
    DIV,
    EQ,
    LT,
    NUM,
    STR,
    IDENT,
    VARDEF,
    LVAR,
    GVAR,
    IF,
    FOR,
    ADDR,
    DEREF,
    LOGOR,
    LOGAND,
    RETURN,
    SIZEOF,
    CALL,
    FUNC,
    COMP_STMT,
    EXPR_STMT,
}

#[derive(Clone, Debug, PartialEq)]
pub enum CType {
    INT,
    CHAR,
    PTR,
    ARY,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Type {
    pub ty: CType,
    pub ptr_of: Option<Box<Type>>,
    pub ary_of: Option<Box<Type>>,
    pub len: i32,
}

pub fn alloc_type() -> Type {
    Type {
        ty: CType::INT,
        ptr_of: None,
        ary_of: None,
        len: 0,
    } 
}

pub fn int_ty() -> Type {
    let mut ty = alloc_type();
    ty.ty = CType::INT;
    return ty
}

fn char_ty() -> Type {
    let mut ty = alloc_type();
    ty.ty = CType::CHAR;
    return ty
}

#[derive(Clone, Debug)]
pub struct Node {
    pub op: NodeType,
    pub ty: Type,
    pub lhs: Option<Box<Node>>,
    pub rhs: Option<Box<Node>>,
    pub val: i32,
    pub str_cnt: String,
    pub expr: Option<Box<Node>>,
    pub stmts: Vec<Node>,

    pub name: String,

    // "if" (cond) then "else els
    // "for" (init; cond; inc) body
    pub cond: Option<Box<Node>>,
    pub then: Option<Box<Node>>,
    pub els: Option<Box<Node>>,
    pub init: Option<Box<Node>>,
    pub inc: Option<Box<Node>>,
    pub body: Option<Box<Node>>,

    // Function definition
    pub stacksize: i32,
    pub strings: Vec<Box<Node>>,
    
    // Local variable
    pub offset: i32,

    // Function call
    pub args: Vec<Node>,
}

// default node
pub fn alloc_node() -> Node {
    Node {
        op: NodeType::NUM,
        ty: int_ty(),
        lhs: None,
        rhs: None,
        val: 0,
        str_cnt: String::new(),
        expr: None,
        stmts: Vec::new(),

        name: String::new(),

        cond: None,
        then: None,
        els: None,
        init: None,
        inc: None,
        body: None,

        stacksize: 0,
        strings: Vec::new(),

        offset: 0,

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

fn get_type(tokens: &Vec<Token>) -> Option<Type> {
    let t = &tokens[pos()];
    match t.ty {
        TokenType::INT => {
            return Some(int_ty());
        }
        TokenType::CHAR => {
            return Some(char_ty());
        }
        _ => {
            None
        }
    }
}

fn new_binop(op: NodeType, lhs: Node, rhs: Node) -> Node {
    let mut node = alloc_node();
    node.op = op;
    node.lhs = Some(Box::new(lhs));
    node.rhs = Some(Box::new(rhs));
    return node;
}

fn new_expr(op: NodeType, expr: Node) -> Node {
    let mut node = alloc_node();
    node.op = op;
    node.expr = Some(Box::new(expr));
    return node;
}

fn primary(tokens: &Vec<Token>) -> Node {
    let t = &tokens[pos()];
    inc_pos();

    if t.ty == TokenType::BRA {
        let node = assign(tokens);
        expect(TokenType::KET, tokens);
        return node;
    }

    let mut node = alloc_node();
    if t.ty == TokenType::NUM {
        node.ty = int_ty();
        node.op = NodeType::NUM;
        node.val = t.val;
        return node;
    }

    if t.ty == TokenType::STR {
        node.ty = ary_of(char_ty(), t.str_cnt.len() as i32);
        node.op = NodeType::STR;
        node.str_cnt = t.str_cnt.clone();
    }

    if t.ty == TokenType::IDENT {
        node.name = t.name.clone();

        if !consume(TokenType::BRA, tokens) {
            node.op = NodeType::IDENT;
            return node;
        }

        node.op = NodeType::CALL;
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

fn postfix(tokens: &Vec<Token>) -> Node {
    let mut lhs = primary(tokens);
    while consume(TokenType::S_BRA, tokens) {
        lhs = new_expr(NodeType::DEREF, new_binop(NodeType::ADD, lhs, primary(tokens)));
        expect(TokenType::S_KET, tokens);
    }
    return lhs;
}

fn unary(tokens: &Vec<Token>) -> Node {
    if consume(TokenType::MUL, tokens) {
        return new_expr(NodeType::DEREF, mul(tokens));
    }
    if consume(TokenType::AMP, tokens) {
        return new_expr(NodeType::ADDR, mul(tokens));
    }
    if consume(TokenType::SIZEOF, tokens) {
        return new_expr(NodeType::SIZEOF, unary(tokens));
    }
    return postfix(tokens);
}

fn mul(tokens: &Vec<Token>) -> Node {
    let mut lhs = unary(tokens);
    loop {
        let t = &tokens[pos()];
        if t.ty != TokenType::MUL && t.ty != TokenType::DIV {
            return lhs;
        }
        inc_pos();
        lhs = new_binop(to_node_type(&t.ty), lhs, unary(tokens));
    }
}

fn add(tokens: &Vec<Token>) -> Node {
    let mut lhs = mul(tokens);
    loop {
        let t = &tokens[pos()];
        if t.ty != TokenType::ADD && t.ty != TokenType::SUB {
            return lhs;
        }
        inc_pos();
        lhs = new_binop(to_node_type(&t.ty), lhs, mul(tokens));
    }
}

fn rel(tokens: &Vec<Token>) -> Node {
    let mut lhs = add(tokens);
    loop {
        let t = &tokens[pos()];
        if t.ty == TokenType::LT {
            inc_pos();
            lhs = new_binop(NodeType::LT, lhs, add(tokens));
            continue;
        }
        if t.ty == TokenType::GT {
            inc_pos();
            lhs = new_binop(NodeType::LT, add(tokens), lhs);
            continue;
        }
        return lhs;
    }
}

fn logand(tokens: &Vec<Token>) -> Node {
    let mut lhs = rel(tokens);
    loop {
        let t = &tokens[pos()];
        if t.ty != TokenType::LOGAND {
            return lhs;
        }
        inc_pos();
        lhs = new_binop(to_node_type(&t.ty), lhs, rel(tokens));
    }
}

fn logor(tokens: &Vec<Token>) -> Node {
    let mut lhs = logand(tokens);
    loop {
        let t = &tokens[pos()];
        if t.ty != TokenType::LOGOR {
            return lhs;
        }
        inc_pos();
        lhs = new_binop(to_node_type(&t.ty), lhs, logand(tokens));
    }
}

fn assign(tokens: &Vec<Token>) -> Node {
    let lhs = logor(tokens);
    if consume(TokenType::EQ, tokens) {
        return new_binop(NodeType::EQ, lhs, logor(tokens));
    }
    return lhs;
}

fn do_type(tokens: &Vec<Token>) -> Type {
    let t = &tokens[pos()];
    let ty_opt = get_type(tokens);
    if ty_opt.is_none() {
        panic!("typename expected, but got {}", t.input);
    }
    let mut ty = ty_opt.unwrap();
    inc_pos();

    while consume(TokenType::MUL, tokens) {
        ty = ptr_of(ty);
    }
    return ty;
}

fn read_array<'a>(ty: &'a mut Type, tokens: &Vec<Token>) -> &'a Type {
    let mut v = Vec::new();
    while consume(TokenType::S_BRA, tokens) {
        let len = primary(tokens);
        if len.op != NodeType::NUM {
            panic!("number expected");
        }
        v.push(len);
        expect(TokenType::S_KET, tokens);
    }
    for len in v.iter() {
        *ty = ary_of(ty.clone(), len.val);
    }
    return ty;
}

fn decl(tokens: &Vec<Token>) -> Node {
    let mut node = alloc_node();
    node.op = NodeType::VARDEF;

    // Read the first half of type name (e.g. `int *`).
    node.ty = do_type(tokens);

    // Read an identifier.
    let t = &tokens[pos()];
    if t.ty != TokenType::IDENT {
        panic!("variable name expected, but got {}", t.input);
    }
    node.name = t.name.clone();
    inc_pos();


    // Read the second half of type name (e.g. `[3][5]`).
    node.ty = read_array(&mut node.ty, tokens).clone();

    // Read an initializer.
    if consume(TokenType::EQ, tokens) {
        node.init = Some(Box::new(assign(tokens)));
    }
    expect(TokenType::SEMI_COLON, tokens);
    return node;
}

fn param(tokens: &Vec<Token>) -> Node {
    let mut node = alloc_node();
    node.op = NodeType::VARDEF;
    node.ty = do_type(tokens);

    let t = &tokens[pos()];
    if t.ty != TokenType::IDENT {
        panic!("parameter name expected, but got {}", t.input);
    }
    node.name = t.name.clone();
    inc_pos();
    return node;
}

fn expr_stmt(tokens: &Vec<Token>) -> Node {
    let node = new_expr(NodeType::EXPR_STMT, assign(tokens));
    expect(TokenType::SEMI_COLON, tokens);
    return node;
}

pub fn stmt(tokens: &Vec<Token>) -> Node {
    let mut node = alloc_node();
    let t = &tokens[pos()];

    match t.ty {
        TokenType::INT | TokenType::CHAR => {
            return decl(tokens);
        }
        TokenType::IF => {
            inc_pos();
            node.op = NodeType::IF;
            expect(TokenType::BRA, tokens);
            node.cond = Some(Box::new(assign(tokens)));
            expect(TokenType::KET, tokens);
            node.then = Some(Box::new(stmt(tokens)));
            if consume(TokenType::ELSE, tokens) {
                node.els = Some(Box::new(stmt(tokens)));
            }
            return node;
        }
        TokenType::FOR => {
            inc_pos();
            node.op = NodeType::FOR;
            expect(TokenType::BRA, tokens);
            if get_type(tokens).is_some() {
                node.init = Some(Box::new(decl(tokens)));
            } else {
                node.init = Some(Box::new(expr_stmt(tokens)));
            }
            node.cond = Some(Box::new(assign(tokens)));
            expect(TokenType::SEMI_COLON, tokens);
            node.inc = Some(Box::new(assign(tokens)));
            expect(TokenType::KET, tokens);
            node.body = Some(Box::new(stmt(tokens)));
            return node;
        }
        TokenType::RETURN => {
            inc_pos();
            node.op = NodeType::RETURN;
            node.expr = Some(Box::new(assign(tokens)));
            expect(TokenType::SEMI_COLON, tokens);
            return node;
        }
        TokenType::C_BRA => {
            inc_pos();
            node.op = NodeType::COMP_STMT;
            while !consume(TokenType::C_KET, tokens) {
                node.stmts.push(stmt(tokens));
            }
            return node;
        }
        _ => {
            return expr_stmt(tokens);
        }
    }
}

pub fn compaund_stmt(tokens: &Vec<Token>) -> Node {
    let mut node = alloc_node();
    node.op = NodeType::COMP_STMT;

    while !consume(TokenType::C_KET, tokens) {
        node.stmts.push(stmt(tokens));
    }
    return node;
}

fn function(tokens: &Vec<Token>) -> Node {
    let mut node = alloc_node();
    node.op = NodeType::FUNC;

    let mut t = &tokens[pos()];
    if t.ty != TokenType::INT {
        panic!("function return type expected, but got {}", t.input);
    }
    inc_pos();

    t = &tokens[pos()];
    if t.ty != TokenType::IDENT {
        panic!("function name expected, but got {}", t.input);
    }
    node.name = t.name.clone();
    inc_pos();

    expect(TokenType::BRA, tokens);
    if !consume(TokenType::KET, tokens) {
        node.args.push(param(tokens));
        while consume(TokenType::COMMA, tokens) {
            node.args.push(param(tokens));
        }
        expect(TokenType::KET, tokens);
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
