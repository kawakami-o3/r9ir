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
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

fn pos() -> usize {
    POS.with(|pos| {
        *pos.borrow()
    })
}

fn bump_pos() -> usize {
    POS.with(|pos| {
        let ret = *pos.borrow();
        *pos.borrow_mut() += 1;
        return ret;
    })
}

fn dump_pos() -> usize {
    POS.with(|pos| {
        let ret = *pos.borrow();
        *pos.borrow_mut() -= 1;
        return ret;
    })
}

thread_local! {
    static POS: RefCell<usize> = RefCell::new(0);
    static ENV: RefCell<Env> = RefCell::new(new_env(None));
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
    ENV.with(|env| {
        env.borrow_mut().typedefs.insert(key, val);
    })
}

fn env_tag_put(key: String, val: Type) {
    ENV.with(|env| {
        env.borrow_mut().tags.insert(key, val);
    })
}

fn env_push() {
    ENV.with(|env| {
        let e = env.borrow().clone();
        *env.borrow_mut() = new_env(Some(Box::new(e)));
    })
}

fn env_pop() {
    ENV.with(|env| {
        let e = env.borrow().next.clone().unwrap();
        *env.borrow_mut() = *e;
    })
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
    NOT,       // ~
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
    pub ptr_to: Option<Rc<RefCell<Type>>>,

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

            let node_align = node.ty.borrow().align;
            off = roundup(off, node.ty.borrow().align);
            node.ty.borrow_mut().offset = off;
            off += node.ty.borrow().size;

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
    pub ty: Rc<RefCell<Type>>,   // C type
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
        ty: Rc::new(RefCell::new(int_ty())),
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
    ENV.with(|env| {
        return env.borrow().find_typedef(name)
    })
}

fn find_tag(name: String) -> Option<Type> {
    ENV.with(|env| {
        return env.borrow().find_tag(name);
    })
}

fn expect(ty: TokenType, tokens: &Vec<Token>) {
    let t = &tokens[pos()];
    if t.ty == ty {
        bump_pos();
        return;
    }

    if ty != TokenType::EOF {
        bad_token(t, format!("{:?} expected", ty).to_string());
    }
    assert!(ty == TokenType::WHILE);
    bad_token(t, format!("'while' expected").to_string());
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

fn decl_specifiers(tokens: &Vec<Token>) -> Type {
    let t = &tokens[bump_pos()];
    match t.ty {
        TokenType::IDENT => {
            let ty = find_typedef(t.name.clone());
            if ty.is_none() {
                dump_pos();
            }
            return ty.unwrap();
        }
        TokenType::INT => {
            return int_ty();
        }
        TokenType::CHAR => {
            return char_ty();
        }
        TokenType::VOID => {
            return void_ty();
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
                    ms.push(declaration(tokens));
                }
                members = Some(ms);
            }

            if tag.is_none() && members.is_none() {
                bad_token(t, "bad struct definition".to_string());
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
            return ty.unwrap();
        }
        _ => {
            bad_token(&t, "typename expected".to_string());
            panic!();
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
    node.ty = Rc::new(RefCell::new(int_ty()));
    node.val = val;
    return node;
}

fn ident(tokens: &Vec<Token>) -> String {
    let t = &tokens[bump_pos()];
    if t.ty != TokenType::IDENT {
        bad_token(t, "identifier expected".to_string());
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
        node.ty = Rc::new(RefCell::new(ary_of(char_ty(), t.str_cnt.len() as i32)));
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

    bad_token(t, "primary expression expected".to_string());
    panic!(); // To avoid compile error.
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
    if consume(TokenType::TILDA, tokens) {
        return new_expr(NodeType::NOT, unary(tokens));
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

fn read_array<'a>(ty: &'a mut Type, tokens: &Vec<Token>) -> &'a mut Type {
    let mut v = Vec::new();
    while consume(TokenType::S_BRA, tokens) {
        let t = &tokens[pos()];
        let len = expr(tokens);
        if len.op != NodeType::NUM {
            bad_token(t, "number expected".to_string());
        }
        v.push(len);
        expect(TokenType::S_KET, tokens);
    }
    for len in v.iter() {
        *ty = ary_of(ty.clone(), len.val);
    }
    return ty;
}

fn direct_decl(ty: Rc<RefCell<Type>>, tokens: &Vec<Token>) -> Node {
    let t = &tokens[pos()];
    let mut node = alloc_node();
    let node_ty = Rc::new(RefCell::new(alloc_type()));
    let placeholder = node_ty.clone();

    if t.ty == TokenType::IDENT {
        node.op = NodeType::VARDEF;
        node.ty = node_ty;
        node.name = ident(tokens);
    } else if consume(TokenType::BRA, tokens) {
        node = declarator(node_ty, tokens);
        expect(TokenType::KET, tokens);
    } else {
        bad_token(t, "bad direct-declarator".to_string());
    }

    // Read the second half of type name (e.g. `[3][5]`)
    *placeholder.borrow_mut() = read_array(&mut *ty.borrow_mut(), tokens).clone();

    // Read an initializer.
    if consume(TokenType::EQL, tokens) {
        node.init = Some(Box::new(assign(tokens)));
    }
    return node;
}

fn declarator(ty: Rc<RefCell<Type>>, tokens: &Vec<Token>) -> Node {
    let mut t = ty;
    while consume(TokenType::MUL, tokens) {
        t = Rc::new(RefCell::new(ptr_to(t)));
    }
    return direct_decl(t, tokens);
}

fn declaration(tokens: &Vec<Token>) -> Node {
    let ty = decl_specifiers(tokens);
    let node = declarator(Rc::new(RefCell::new(ty)), tokens);
    expect(TokenType::SEMI_COLON, tokens);
    return node;
}

fn param_declaration(tokens: &Vec<Token>) -> Node {
    let ty = decl_specifiers(tokens);
    return declarator(Rc::new(RefCell::new(ty)), tokens);
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
            let node = declaration(tokens);
            assert!(node.name.len()>0);
            env_typedef_put(node.name, node.ty.borrow().clone());
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
                node.init = Some(Box::new(declaration(tokens)));
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
                return declaration(tokens);
            }
            return expr_stmt(tokens);
        }
    }
}

pub fn compound_stmt(tokens: &Vec<Token>) -> Node {
    let mut node = alloc_node();
    node.op = NodeType::COMP_STMT;

    env_push();
    while !consume(TokenType::C_KET, tokens) {
        node.stmts.push(stmt(tokens));
    }
    env_pop();

    return node;
}

fn toplevel(tokens: &Vec<Token>) -> Option<Node> {
    let is_typedef = consume(TokenType::TYPEDEF, tokens);
    let is_extern = consume(TokenType::EXTERN, tokens);

    let mut ty = decl_specifiers(tokens);
    while consume(TokenType::MUL, tokens) {
        ty = ptr_to(Rc::new(RefCell::new(ty)));
    }

    let name = ident(tokens);

    // Function
    if consume(TokenType::BRA, tokens) {
        let mut node = alloc_node();
        node.op = NodeType::FUNC;
        node.ty = Rc::new(RefCell::new(ty));
        node.name = name.clone();

        if !consume(TokenType::KET, tokens) {
            node.args.push(param_declaration(tokens));
            while consume(TokenType::COMMA, tokens) {
                node.args.push(param_declaration(tokens));
            }
            expect(TokenType::KET, tokens);
        }

        let t = &tokens[pos()];
        expect(TokenType::C_BRA, tokens);
        if is_typedef {
            bad_token(t, format!("typedef has function definition").to_string());
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
    node.ty = Rc::new(RefCell::new(ty.clone()));
    node.name = name;
    node.is_extern = is_extern;

    if !is_extern {
        let mut data = String::new();
        for _i in 0..node.ty.borrow().size {
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
