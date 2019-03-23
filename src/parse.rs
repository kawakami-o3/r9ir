// Recursive-descendent parser

// This is a recursive-descendent parser which constructs abstract
// syntax tree from input tokens.
//
// This parser knows only about BNF of the C grammer and doesn't care
// about its semantics. Therefore, some invalid expressions, such as
// `1+2=3`, are accepted by this parser, but that's intentional.
// Semantic errors are detected in a later pass.

#![allow(non_camel_case_types)]

use crate::sema::*;
use crate::token::*;
use crate::util::*;
use std::collections::HashMap;
use std::sync::Mutex;

fn pos() -> usize {
    let i = *POS.lock().unwrap();
    return i;
}

fn bump_pos() -> usize {
    let mut pos = POS.lock().unwrap();
    let ret = *pos;
    *pos += 1;
    return ret;
}

fn dump_pos() -> usize {
    let mut pos = POS.lock().unwrap();
    let ret = *pos;
    *pos -= 1;
    return ret;
}

lazy_static! {
    static ref POS: Mutex<usize> = Mutex::new(0);
    static ref ENV: Mutex<Env> = Mutex::new(new_env(None));
}

#[derive(Clone)]
struct Env {
    typedefs: HashMap<String, Type>,
    tags: HashMap<String, Type>,
    next: Option<Box<Env>>,
}

impl Env {
    fn find_typedef(& self, name: String) -> Option<Type> {
        match self.typedefs.get(&name) {
            None => {
                match self.next {
                    None => None,
                    Some(ref next) => next.find_typedef(name),
                }
            }
            ty => Some(ty.unwrap().clone()),
        }
    }

    fn find_tag(& self, name: String) -> Option<Type> {
        match self.tags.get(&name) {
            None => {
                match self.next {
                    None => None,
                    Some(ref next) => next.find_tag(name),
                }
            }
            ty => Some(ty.unwrap().clone()),
        }
    }
}

fn new_env(next: Option<Box<Env>>) -> Env {
    Env{
        typedefs: HashMap::new(),
        tags: HashMap::new(),
        next: next,
    }
}

fn env_typedef_put(key: String, val: Type) {
    match ENV.lock() {
        Ok(mut env) => {
            env.typedefs.insert(key, val);
        }
        Err(_) => {
            panic!();
        }
    }
}

fn env_tag_put(key: String, val: Type) {
    match ENV.lock() {
        Ok(mut env) => {
            env.tags.insert(key, val);
        }
        Err(_) => {
            panic!();
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum NodeType {
    ADD,       // +
    SUB,       // -
    MUL,       // *
    DIV,       // /
    EQL,       // =
    LT,        // <
    OR,        // |
    XOR,       // ^
    AND,       // &
    EXCLAM,    // !
    QUEST,     // ?
    COMMA,     // ,
    NUM,       // Number literal
    STR,       // String literal
    IDENT,     // Identifier
    //STRUCT,    // Struct
    VARDEF,    // Variable definition
    LVAR,      // Local variable reference
    GVAR,      // Global variable reference
    IF,        // "if"
    FOR,       // "for"
    DO_WHILE,  // do ... while
    BREAK,     // break
    ADDR,      // address-of operator ("&")
    DEREF,     // pointer dereference ("*")
    DOT,       // Struct member access
    EQ,        // ==
    NE,        // !=
    LE,        // <=
    LOGAND,    // &&
    LOGOR,     // ||
    SHL,       // <<
    SHR,       // >>
    MOD,       // %
    NEG,       // -
    POST_INC,  // post ++
    POST_DEC,  // post --
    MUL_EQ,    // *=
    DIV_EQ,    // /=
    MOD_EQ,    // %=
    ADD_EQ,    // +=
    SUB_EQ,    // -=
    SHL_EQ,    // <<=
    SHR_EQ,    // >>=
    BITAND_EQ, // &=
    XOR_EQ,    // ^=
    BITOR_EQ,  // |=
    RETURN,    // "return"
    SIZEOF,    // "sizeof"
    ALIGNOF,   // "_Alignof"
    CALL,      // Function call
    FUNC,      // Function definition
    COMP_STMT, // Compound statement
    EXPR_STMT, // Expression statement
    STMT_EXPR, // Statement expression (GNU extn.)
    NULL,      // Null statement
}

#[derive(Clone, Debug, PartialEq)]
pub enum CType {
    INT,
    CHAR,
    VOID,
    PTR,
    ARY,
    STRUCT,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Type {
    pub ty: CType,
    pub size: i32,
    pub align: i32,

    // Pointer
    pub ptr_to: Option<Box<Type>>,

    // Array
    pub ary_of: Option<Box<Type>>,
    pub len: i32,

    // Struct
    pub members: Option<Vec<Node>>,
    pub offset: i32,
}

impl Type {
    fn add_members(&mut self, members: Vec<Node>) {
        let mut off = 0;
        let mut ms = members.clone();
        for node in ms.iter_mut() {
            assert!(node.op == NodeType::VARDEF);

            let node_align = node.ty.align;
            let mut t = &mut node.ty;
            off = roundup(off, t.align);
            t.offset = off;
            off += t.size;

            if self.align < node_align {
                self.align = node_align;
            }
        }

        self.members = Some(ms.clone());
        self.size = roundup(off, self.align);
    }
}

pub fn alloc_type() -> Type {
    Type {
        ty: CType::INT,
        size: 0,
        align: 0,
        ptr_to: None,
        ary_of: None,
        len: 0,
        members: None,
        offset: 0,
    } 
}

#[derive(Clone, Debug, PartialEq)]
pub struct Node {
    pub op: NodeType,            // Node type
    pub ty: Type,                // C type
    pub lhs: Option<Box<Node>>,  // left-hand side
    pub rhs: Option<Box<Node>>,  // right-hand side
    pub val: i32,                // Nubmer literal
    pub expr: Option<Box<Node>>, // "return" or expression stmt
    pub stmts: Vec<Node>,        // Compound statemtn

    pub name: String,

    // Global variable
    pub is_extern: bool,
    pub data: String,
    pub len: usize,

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
    pub globals: Vec<Var>,
    
    // Offset from BP or begining of a struct
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
        expr: None,
        stmts: Vec::new(),

        name: String::new(),

        is_extern: false,
        data: String::new(),
        len: 0,

        cond: None,
        then: None,
        els: None,
        init: None,
        inc: None,
        body: None,

        stacksize: 0,
        globals: Vec::new(),

        offset: 0,

        args: Vec::new(),
    }
}

fn null_stmt() -> Node {
    let mut node = alloc_node();
    node.op = NodeType::NULL;
    return node;
}

fn break_stmt() -> Node {
    let mut node = alloc_node();
    node.op = NodeType::BREAK;
    return node;
}

fn find_typedef(name: String) -> Option<Type> {
    let env = ENV.lock().unwrap();
    return env.find_typedef(name)
}

fn find_tag(name: String) -> Option<Type> {
    let env = ENV.lock().unwrap();
    return env.find_tag(name)
}

fn expect(ty: TokenType, tokens: &Vec<Token>) {
    let t = &tokens[pos()];
    if t.ty != ty {
        panic!("{:?} expected, but got {:?}", ty, t.ty);
    }
    bump_pos();
}

fn new_prim_ty(ty: CType, size: i32) -> Type {
    let mut ret = alloc_type();
    ret.ty = ty;
    ret.size = size;
    ret.align = size;
    return ret;
}

fn void_ty() -> Type {
    return new_prim_ty(CType::VOID, 0);
}

fn char_ty() -> Type {
    return new_prim_ty(CType::CHAR, 1);
}

pub fn int_ty() -> Type {
    return new_prim_ty(CType::INT, 4);
}

fn consume(ty: TokenType, tokens: &Vec<Token>) -> bool {
    let t = &tokens[pos()];
    if t.ty != ty {
        return false;
    }
    bump_pos();
    return true;
}

fn is_typename(tokens: &Vec<Token>) -> bool {
    let t = &tokens[pos()];
    if t.ty == TokenType::IDENT {
        return find_typedef(t.name.clone()).is_some();
    }
    return t.ty == TokenType::INT ||
        t.ty == TokenType::CHAR ||
        t.ty == TokenType::VOID ||
        t.ty == TokenType::STRUCT;
}

fn read_type(tokens: &Vec<Token>) -> Option<Type> {
    let t = &tokens[bump_pos()];
    match t.ty {
        TokenType::IDENT => {
            let ty = find_typedef(t.name.clone());
            if ty.is_none() {
                dump_pos();
            }
            return ty;
        }
        TokenType::INT => {
            return Some(int_ty());
        }
        TokenType::CHAR => {
            return Some(char_ty());
        }
        TokenType::VOID => {
            return Some(void_ty());
        }
        TokenType::STRUCT => {

            let mut tag: Option<String> = None;
            let t = &tokens[pos()];
            if t.ty == TokenType::IDENT {
                bump_pos();
                tag = Some(t.name.clone());
            }

            let mut members: Option<Vec<Node>> = None;
            if consume(TokenType::C_BRA, tokens) {
                let mut ms = Vec::new();
                while !consume(TokenType::C_KET, tokens) {
                    ms.push(decl(tokens));
                }
                members = Some(ms);
            }

            if tag.is_none() && members.is_none() {
                panic!("bad struct definition");
            }

            let mut ty: Option<Type> = None;
            if tag.is_some() && members.is_none() {
                ty = find_tag(tag.clone().unwrap());
            }

            if ty.is_none() {
                let mut ty_tmp = alloc_type();
                ty_tmp.ty = CType::STRUCT;
                ty = Some(ty_tmp);
            }

            if members.is_some() {
                let mut ty_tmp = ty.clone().unwrap();
                ty_tmp.add_members(members.unwrap());
                if tag.is_some() {
                    env_tag_put(tag.unwrap(), ty_tmp.clone());
                }

                ty = Some(ty_tmp.clone());
            }
            return ty;
        }
        _ => {
            dump_pos();
            return None;
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

fn new_num(val: i32) -> Node {
    let mut node = alloc_node();
    node.op = NodeType::NUM;
    node.ty = int_ty();
    node.val = val;
    return node;
}

fn ident(tokens: &Vec<Token>) -> String {
    let t = &tokens[bump_pos()];
    if t.ty != TokenType::IDENT {
        panic!("identifier expected, but got {}", t.input);
    }
    return t.name.clone();
}

fn primary(tokens: &Vec<Token>) -> Node {
    let t = &tokens[bump_pos()];

    if t.ty == TokenType::BRA {
        if consume(TokenType::C_BRA, tokens) {
            let mut node = alloc_node();
            node.op = NodeType::STMT_EXPR;
            node.body = Some(Box::new(compound_stmt(tokens)));
            expect(TokenType::KET, tokens);
            return node;
        }
        let node = expr(tokens);
        expect(TokenType::KET, tokens);
        return node;
    }

    let mut node = alloc_node();
    if t.ty == TokenType::NUM {
        return new_num(t.val);
    }

    if t.ty == TokenType::STR {
        node.ty = ary_of(char_ty(), t.str_cnt.len() as i32);
        node.op = NodeType::STR;
        node.data = t.str_cnt.clone();
        node.len = t.len;
        return node;
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

    panic!("number expected, but got {}", t.input);
}

fn postfix(tokens: &Vec<Token>) -> Node {
    let mut lhs = primary(tokens);

    loop {
        if consume(TokenType::INC, tokens) {
            lhs = new_expr(NodeType::POST_INC, lhs);
            continue;
        }

        if consume(TokenType::DEC, tokens) {
            lhs = new_expr(NodeType::POST_DEC, lhs);
            continue;
        }

        if consume(TokenType::DOT, tokens) {
            lhs = new_expr(NodeType::DOT, lhs);
            lhs.name = ident(tokens);
            continue;
        }

        if consume(TokenType::ARROW, tokens) {
            lhs = new_expr(NodeType::DOT, new_expr(NodeType::DEREF, lhs));
            lhs.name = ident(tokens);
            continue;
        }

        if consume(TokenType::S_BRA, tokens) {
            lhs = new_expr(NodeType::DEREF, new_binop(NodeType::ADD, lhs, assign(tokens)));
            expect(TokenType::S_KET, tokens);
            continue;
        }
        return lhs;
    }
}

fn unary(tokens: &Vec<Token>) -> Node {
    if consume(TokenType::SUB, tokens) {
        return new_expr(NodeType::NEG, unary(tokens));
    }
    if consume(TokenType::MUL, tokens) {
        return new_expr(NodeType::DEREF, unary(tokens));
    }
    if consume(TokenType::AMP, tokens) {
        return new_expr(NodeType::ADDR, unary(tokens));
    }
    if consume(TokenType::EXCLAM, tokens) {
        return new_expr(NodeType::EXCLAM, unary(tokens));
    }
    if consume(TokenType::SIZEOF, tokens) {
        return new_expr(NodeType::SIZEOF, unary(tokens));
    }
    if consume(TokenType::ALIGNOF, tokens) {
        return new_expr(NodeType::ALIGNOF, unary(tokens));
    }

    if consume(TokenType::INC, tokens) {
        return new_binop(NodeType::ADD_EQ, unary(tokens), new_num(1));
    }
    if consume(TokenType::DEC, tokens) {
        return new_binop(NodeType::SUB_EQ, unary(tokens), new_num(1));
    }

    return postfix(tokens);
}

fn mul(tokens: &Vec<Token>) -> Node {
    let mut lhs = unary(tokens);
    loop {
        if consume(TokenType::MUL, tokens) {
            lhs = new_binop(NodeType::MUL, lhs, unary(tokens));
        } else if consume(TokenType::DIV, tokens) {
            lhs = new_binop(NodeType::DIV, lhs, unary(tokens));
        } else if consume(TokenType::MOD, tokens) {
            lhs = new_binop(NodeType::MOD, lhs, unary(tokens));
        } else {
            return lhs;
        }
    }
}

fn add(tokens: &Vec<Token>) -> Node {
    let mut lhs = mul(tokens);
    loop {
        if consume(TokenType::ADD, tokens) {
            lhs = new_binop(NodeType::ADD, lhs, mul(tokens));
        } else if consume(TokenType::SUB, tokens) {
            lhs = new_binop(NodeType::SUB, lhs, mul(tokens));
        } else {
            return lhs;
        }
    }
}

fn shift(tokens: &Vec<Token>) -> Node {
    let mut lhs = add(tokens);
    loop {
        if consume(TokenType::SHL, tokens) {
            lhs = new_binop(NodeType::SHL, lhs, add(tokens));
        } else if consume(TokenType::SHR, tokens) {
            lhs = new_binop(NodeType::SHR, lhs, add(tokens));
        } else {
            return lhs;
        }
    }
}

fn relational(tokens: &Vec<Token>) -> Node {
    let mut lhs = shift(tokens);
    loop {
        if consume(TokenType::LT, tokens) {
            lhs = new_binop(NodeType::LT, lhs, shift(tokens));
        } else if consume(TokenType::GT, tokens) {
            lhs = new_binop(NodeType::LT, shift(tokens), lhs);
        } else if consume(TokenType::LE, tokens) {
            lhs = new_binop(NodeType::LE, lhs, shift(tokens));
        } else if consume(TokenType::GE, tokens) {
            lhs = new_binop(NodeType::LE, shift(tokens), lhs);
        } else {
            return lhs;
        }
    }
}

fn equality(tokens: &Vec<Token>) -> Node {
    let mut lhs = relational(tokens);
    loop {
        if consume(TokenType::EQ, tokens) {
            lhs = new_binop(NodeType::EQ, lhs, relational(tokens));
        } else if consume(TokenType::NE, tokens) {
            lhs = new_binop(NodeType::NE, lhs, relational(tokens));
        } else {
            return lhs;
        }
    }
}

fn bit_and(tokens: &Vec<Token>) -> Node {
    let mut lhs = equality(tokens);
    while consume(TokenType::AMP, tokens) {
        lhs = new_binop(NodeType::AND, lhs, equality(tokens));
    }
    return lhs;
}

fn bit_xor(tokens: &Vec<Token>) -> Node {
    let mut lhs = bit_and(tokens);
    while consume(TokenType::HAT, tokens) {
        lhs = new_binop(NodeType::XOR, lhs, bit_and(tokens));
    }
    return lhs;
}

fn bit_or(tokens: &Vec<Token>) -> Node {
    let mut lhs = bit_xor(tokens);
    while consume(TokenType::OR, tokens) {
        lhs = new_binop(NodeType::OR, lhs, bit_xor(tokens));
    }
    return lhs;
}

fn logand(tokens: &Vec<Token>) -> Node {
    let mut lhs = bit_or(tokens);
    while consume(TokenType::LOGAND, tokens) {
        lhs = new_binop(NodeType::LOGAND, lhs, bit_or(tokens));
    }
    return lhs;
}

fn logor(tokens: &Vec<Token>) -> Node {
    let mut lhs = logand(tokens);
    while consume(TokenType::LOGOR, tokens) {
        lhs = new_binop(NodeType::LOGOR, lhs, logand(tokens));
    }
    return lhs;
}

fn conditional(tokens: &Vec<Token>) -> Node {
    let cond = logor(tokens);
    if !consume(TokenType::QUEST, tokens) {
        return cond;
    }

    let mut node = alloc_node();
    node.op = NodeType::QUEST;
    node.cond = Some(Box::new(cond));
    node.then = Some(Box::new(expr(tokens)));
    expect(TokenType::COLON, tokens);
    node.els = Some(Box::new(expr(tokens)));
    return node;
}

fn assignment_op(tokens: &Vec<Token>) -> Option<NodeType> {
    if consume(TokenType::EQL, tokens) {
        Some(NodeType::EQL)
    } else if consume(TokenType::MUL_EQ, tokens) {
        Some(NodeType::MUL_EQ)
    } else if consume(TokenType::DIV_EQ, tokens) {
        Some(NodeType::DIV_EQ)
    } else if consume(TokenType::MOD_EQ, tokens) {
        Some(NodeType::MOD_EQ)
    } else if consume(TokenType::ADD_EQ, tokens) {
        Some(NodeType::ADD_EQ)
    } else if consume(TokenType::SUB_EQ, tokens) {
        Some(NodeType::SUB_EQ)
    } else if consume(TokenType::SHL_EQ, tokens) {
        Some(NodeType::SHL_EQ)
    } else if consume(TokenType::SHR_EQ, tokens) {
        Some(NodeType::SHR_EQ)
    } else if consume(TokenType::BITAND_EQ, tokens) {
        Some(NodeType::BITAND_EQ)
    } else if consume(TokenType::XOR_EQ, tokens) {
        Some(NodeType::XOR_EQ)
    } else if consume(TokenType::BITOR_EQ, tokens) {
        Some(NodeType::BITOR_EQ)
    } else {
        None
    }
}

fn assign(tokens: &Vec<Token>) -> Node {
    let lhs = conditional(tokens);
    let op = assignment_op(tokens);
    if let Some(o) = op {
        return new_binop(o, lhs, assign(tokens));
    }
    return lhs;
}

fn expr(tokens: &Vec<Token>) -> Node {
    let lhs = assign(tokens);
    if !consume(TokenType::COMMA, tokens) {
        return lhs;
    }
    return new_binop(NodeType::COMMA, lhs, expr(tokens));
}

fn do_type(tokens: &Vec<Token>) -> Type {
    let t = &tokens[pos()];
    let ty_opt = read_type(tokens);
    if ty_opt.is_none() {
        panic!("typename expected, but got {}", t.input);
    }
    let mut ty = ty_opt.unwrap().clone();

    while consume(TokenType::MUL, tokens) {
        ty = ptr_to(ty);
    }
    return ty;
}

fn read_array<'a>(ty: &'a mut Type, tokens: &Vec<Token>) -> &'a Type {
    let mut v = Vec::new();
    while consume(TokenType::S_BRA, tokens) {
        let len = expr(tokens);
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
    node.name = ident(tokens);

    // Read the second half of type name (e.g. `[3][5]`).
    node.ty = read_array(&mut node.ty, tokens).clone();
    if node.ty.ty == CType::VOID {
        panic!("void variable: {}", node.name);
    }

    // Read an initializer.
    if consume(TokenType::EQL, tokens) {
        node.init = Some(Box::new(assign(tokens)));
    }
    expect(TokenType::SEMI_COLON, tokens);
    return node;
}

fn param(tokens: &Vec<Token>) -> Node {
    let mut node = alloc_node();
    node.op = NodeType::VARDEF;
    node.ty = do_type(tokens);
    node.name = ident(tokens);
    return node;
}

fn expr_stmt(tokens: &Vec<Token>) -> Node {
    let node = new_expr(NodeType::EXPR_STMT, expr(tokens));
    expect(TokenType::SEMI_COLON, tokens);
    return node;
}

pub fn stmt(tokens: &Vec<Token>) -> Node {
    let mut node = alloc_node();
    let t = &tokens[bump_pos()];

    match t.ty {
        TokenType::TYPEDEF => {
            let node = decl(tokens);
            assert!(node.name.len()>0);
            env_typedef_put(node.name, node.ty);
            return null_stmt();
        }
        TokenType::IF => {
            node.op = NodeType::IF;
            expect(TokenType::BRA, tokens);
            node.cond = Some(Box::new(expr(tokens)));
            expect(TokenType::KET, tokens);
            node.then = Some(Box::new(stmt(tokens)));
            if consume(TokenType::ELSE, tokens) {
                node.els = Some(Box::new(stmt(tokens)));
            }
            return node;
        }
        TokenType::FOR => {
            node.op = NodeType::FOR;
            expect(TokenType::BRA, tokens);

            if is_typename(tokens) {
                node.init = Some(Box::new(decl(tokens)));
            } else if consume(TokenType::SEMI_COLON, tokens) {
                node.init = Some(Box::new(null_stmt()));
            } else {
                node.init = Some(Box::new(expr_stmt(tokens)));
            }

            if !consume(TokenType::SEMI_COLON, tokens) {
                node.cond = Some(Box::new(expr(tokens)));
                expect(TokenType::SEMI_COLON, tokens);
            }

            if !consume(TokenType::KET, tokens) {
                node.inc = Some(Box::new(new_expr(NodeType::EXPR_STMT, expr(tokens))));
                expect(TokenType::KET, tokens);
            }

            node.body = Some(Box::new(stmt(tokens)));
            return node;
        }
        TokenType::WHILE => {
            node.op = NodeType::FOR;
            node.init = Some(Box::new(null_stmt()));
            node.inc = Some(Box::new(null_stmt()));
            expect(TokenType::BRA, tokens);
            node.cond = Some(Box::new(expr(tokens)));
            expect(TokenType::KET, tokens);
            node.body = Some(Box::new(stmt(tokens)));
            return node;
        }
        TokenType::DO => {
            node.op = NodeType::DO_WHILE;
            node.body = Some(Box::new(stmt(tokens)));
            expect(TokenType::WHILE, tokens);
            expect(TokenType::BRA, tokens);
            node.cond = Some(Box::new(expr(tokens)));
            expect(TokenType::KET, tokens);
            expect(TokenType::SEMI_COLON, tokens);
            return node;
        }
        TokenType::BREAK => {
            return break_stmt();
        }
        TokenType::RETURN => {
            node.op = NodeType::RETURN;
            node.expr = Some(Box::new(expr(tokens)));
            expect(TokenType::SEMI_COLON, tokens);
            return node;
        }
        TokenType::C_BRA => {
            node.op = NodeType::COMP_STMT;
            while !consume(TokenType::C_KET, tokens) {
                node.stmts.push(stmt(tokens));
            }
            return node;
        }
        TokenType::SEMI_COLON => {
            return null_stmt();
        }
        _ => {
            dump_pos();
            if is_typename(tokens) {
                return decl(tokens);
            }
            return expr_stmt(tokens);
        }
    }
}

pub fn compound_stmt(tokens: &Vec<Token>) -> Node {
    let mut node = alloc_node();
    node.op = NodeType::COMP_STMT;

    match ENV.lock() {
        Ok(mut env) => {
            *env = new_env(Some(Box::new(env.clone())));
        }
        Err(_) => {
            panic!();
        }
    }
    while !consume(TokenType::C_KET, tokens) {
        node.stmts.push(stmt(tokens));
    }
    match ENV.lock() {
        Ok(mut env) => {
            *env = *env.next.clone().unwrap();
        }
        Err(_) => {
            panic!();
        }
    }
    return node;
}

fn toplevel(tokens: &Vec<Token>) -> Option<Node> {
    let is_typedef = consume(TokenType::TYPEDEF, tokens);
    let is_extern = consume(TokenType::EXTERN, tokens);
    let ty = do_type(tokens);
    // if (!ty) ... // 'do_type' panics when it fails to parse a type name.

    let name = ident(tokens);

    // Function
    if consume(TokenType::BRA, tokens) {
        let mut node = alloc_node();
        node.op = NodeType::FUNC;
        node.ty = ty.clone();
        node.name = name.clone();

        if !consume(TokenType::KET, tokens) {
            node.args.push(param(tokens));
            while consume(TokenType::COMMA, tokens) {
                node.args.push(param(tokens));
            }
            expect(TokenType::KET, tokens);
        }

        expect(TokenType::C_BRA, tokens);
        if is_typedef {
            panic!("typedef {} has function definition", name);
        }
        node.body = Some(Box::new(compound_stmt(tokens)));
        return Some(node);
    }

    let mut ty_tmp = ty.clone();
    let ty = read_array(&mut ty_tmp, tokens);
    expect(TokenType::SEMI_COLON, tokens);

    if is_typedef {
        env_typedef_put(name, ty.clone());
        return None;
    }

    // Global variable
    let mut node = alloc_node();
    node.op = NodeType::VARDEF;
    node.ty = ty.clone();
    node.name = name;
    node.is_extern = is_extern;

    if !is_extern {
        let mut data = String::new();
        for _i in 0..node.ty.size {
            data.push(char::from(0));
        }
        node.data = data;
        node.len = ty.size as usize;
    }
    return Some(node);
}

pub fn parse(tokens: &Vec<Token>) -> Vec<Node> {
    let mut v = Vec::new();

    loop {
        let t = &tokens[pos()];
        if t.ty == TokenType::EOF {
            return v;
        }

        let node = toplevel(tokens);
        if node.is_some() {
            v.push(node.unwrap());
        }
    }
}
