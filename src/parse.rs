// Recursive-descendent parser

// This is a recursive-descendent parser which constructs abstract
// syntax tree from input tokens.
//
// Variable names are resolved at this stage. We create a Var object
// when we see a variable definition and use it when we see a variable
// reference.
//
// Types are added to variables and literals. For other nodes, Sema
// will add type for them.
//
// Semantic checking is omitted from this parser to make the code in
// this file closely resemble the C's BNF. Invalid expressions, such
// as `1+2=3`, are accepted at this stage. Such errors are detected in
// a later pass.

#![allow(non_camel_case_types)]

use crate::gen_ir::*;
use crate::token::*;
use crate::sema::*;
use crate::util::*;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

thread_local! {
    static PROGRAM: RefCell<Program> = RefCell::new(new_program());
    static POS: RefCell<usize> = RefCell::new(0);
    static ENV: RefCell<Env> = RefCell::new(new_env(None));
    static LVARS: RefCell<Vec<Rc<RefCell<Var>>>> = RefCell::new(Vec::new());
    static BREAKS: RefCell<Vec<Node>> = RefCell::new(Vec::new());
    static CONTINUES: RefCell<Vec<Node>> = RefCell::new(Vec::new());
}

fn init_lvars() {
    LVARS.with(|p| {
        *p.borrow_mut() = Vec::new();
    })
}

fn lvars() -> Vec<Rc<RefCell<Var>>> {
    LVARS.with(|p| {
        return p.borrow().clone();
    })
}

fn lvars_push(var: Rc<RefCell<Var>>) {
    LVARS.with(|p| {
        p.borrow_mut().push(var);
    })
}

fn init_breaks() {
    BREAKS.with(|p| {
        *p.borrow_mut() = Vec::new();
    })
}

fn breaks_len() -> usize {
    BREAKS.with(|p| {
        return p.borrow().len();
    })
}

fn breaks_push(node: Node) {
    BREAKS.with(|p| {
        p.borrow_mut().push(node);
    })
}

fn breaks_pop() -> Option<Node> {
    BREAKS.with(|p| {
        return p.borrow_mut().pop();
    })
}

fn breaks_last() -> Node {
    BREAKS.with(|p| {
        let l = p.borrow().len(); 
        return p.borrow()[l-1].clone();
    })
}

fn init_continues() {
    CONTINUES.with(|p| {
        *p.borrow_mut() = Vec::new();
    })
}

fn continues_len() -> usize {
    CONTINUES.with(|p| {
        return p.borrow().len();
    })
}

fn continues_push(node: Node) {
    CONTINUES.with(|p| {
        p.borrow_mut().push(node);
    })
}

fn continues_pop() -> Option<Node> {
    CONTINUES.with(|p| {
        return p.borrow_mut().pop();
    })
}

fn continues_last() -> Node {
    CONTINUES.with(|p| {
        let l = p.borrow().len();
        return p.borrow()[l-1].clone();
    })
}

fn prog_gvars_push(var: Rc<RefCell<Var>>) {
    PROGRAM.with(|p| {
        p.borrow_mut().gvars.push(var);
    })
}

fn prog_nodes_push(node: Rc<RefCell<Node>>) {
    PROGRAM.with(|p| {
        p.borrow_mut().nodes.push(node);
    })
}

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

#[derive(Clone, Debug)]
pub struct Program {
    pub gvars: Vec<Rc<RefCell<Var>>>,
    pub nodes: Vec<Rc<RefCell<Node>>>,
    pub funcs: Vec<IR>,
}

pub fn new_program() -> Program {
    Program {
        gvars: Vec::new(),
        nodes: Vec::new(),
        funcs: Vec::new(),
    }
}

#[derive(Clone, Debug)]
struct Env {
    vars: HashMap<String, Rc<RefCell<Var>>>,
    typedefs: HashMap<String, Type>,
    tags: HashMap<String, Type>,
    next: Option<Box<Env>>,
}

impl Env {
    fn find_var(& self, name: & String) -> Option<Rc<RefCell<Var>>> {
        match self.vars.get(name) {
            None => {
                match self.next {
                    None => None,
                    Some(ref next) => next.find_var(name),
                }
            }
            v => Some(v.unwrap().clone()),
        }
    }

    fn find_typedef(& self, name: & String) -> Option<Type> {
        match self.typedefs.get(name) {
            None => {
                match self.next {
                    None => None,
                    Some(ref next) => next.find_typedef(name),
                }
            }
            ty => Some(ty.unwrap().clone()),
        }
    }

    fn find_tag(& self, name: & String) -> Option<Type> {
        match self.tags.get(name) {
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
        vars: HashMap::new(),
        typedefs: HashMap::new(),
        tags: HashMap::new(),
        next: next,
    }
}

fn env_vars_put(key: String, val: Rc<RefCell<Var>>) {
    ENV.with(|env| {
        env.borrow_mut().vars.insert(key, val);
    })
}

fn env_typedefs_put(key: String, val: Type) {
    ENV.with(|env| {
        env.borrow_mut().typedefs.insert(key, val);
    })
}

fn env_tags_put(key: String, val: Type) {
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
    //STR,       // String literal
    //STRUCT,    // Struct
    DECL,      // declaration
    VARDEF,    // Variable definition
    VAR,       // Variable reference
    IF,        // "if"
    FOR,       // "for"
    DO_WHILE,  // do ... while
    BREAK,     // break
    CONTINUE,  // continue
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
    FUNC,
    //TYPEOF,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Type {
    pub ty: CType,
    pub size: i32,   // sizeof
    pub align: i32,  // alignof
    pub is_extern: bool,

    // Pointer
    pub ptr_to: Option<Rc<RefCell<Type>>>,

    // Array
    pub ary_of: Option<Box<Type>>,
    pub len: i32,

    // Struct
    pub members: Option<Vec<Node>>,
    pub offset: i32,

    // Function
    pub returning: Option<Box<Type>>,

    // Typeof
    pub node: Option<Box<Node>>,
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
        is_extern: false,
        ptr_to: None,
        ary_of: None,
        len: 0,
        members: None,
        offset: 0,
        returning: None,
        node: None,
    } 
}

#[derive(Clone, Debug, PartialEq)]
pub struct Var {
    pub ty: Type,
    pub is_local: bool,

    // local
    pub offset: i32,

    // global
    pub name: String,
    pub data: Option<String>,
    pub len: usize,
}

pub fn alloc_var() -> Var {
    Var {
        ty: alloc_type(),
        is_local: false,

        offset: 0,

        name: String::new(),
        data: None,
        len: 0,
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
    pub var: Option<Rc<RefCell<Var>>>,

    // "if" (cond) then "else els
    // "for" (init; cond; inc) body
    pub cond: Option<Box<Node>>,
    pub then: Option<Box<Node>>,
    pub els: Option<Box<Node>>,
    pub init: Option<Box<Node>>,
    pub inc: Option<Box<Node>>,
    pub body: Option<Box<Node>>,

    pub break_label: usize,
    pub continue_label: usize,
    
    // For break and continue
    pub target: Option<Box<Node>>,

    // Function definition
    pub stacksize: i32,
    pub globals: Vec<Var>,
    pub lvars: Vec<Rc<RefCell<Var>>>,
    
    // Function call
    pub args: Vec<Node>,

    // For error reporting
    pub token: Option<Box<Token>>,
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
        var: None,

        cond: None,
        then: None,
        els: None,
        init: None,
        inc: None,
        body: None,

        break_label: 0,
        continue_label: 0,

        target: None,

        stacksize: 0,
        globals: Vec::new(),
        lvars: Vec::new(),

        args: Vec::new(),

        token: None,
    }
}

fn null_stmt() -> Node {
    let mut node = alloc_node();
    node.op = NodeType::NULL;
    return node;
}

fn find_var(name: & String) -> Option<Rc<RefCell<Var>>> {
    ENV.with(|env| {
        return env.borrow().find_var(name)
    })
}

fn find_typedef(name: & String) -> Option<Type> {
    ENV.with(|env| {
        return env.borrow().find_typedef(name)
    })
}

fn find_tag(name: & String) -> Option<Type> {
    ENV.with(|env| {
        return env.borrow().find_tag(name);
    })
}

fn add_lvar(ty: Type, name: String) -> Rc<RefCell<Var>> {
    let mut var = alloc_var();
    var.ty = ty;
    var.is_local = true;
    let v = Rc::new(RefCell::new(var));
    env_vars_put(name, v.clone());
    lvars_push(v.clone());
    return v;
}

fn add_gvar(ty: Type, name: String, data: Option<String>, len: usize) -> Rc<RefCell<Var>> {
    let mut var = alloc_var();
    var.ty = ty;
    var.name = name.clone();
    var.data = data;
    var.len = len;
    let v = Rc::new(RefCell::new(var));
    env_vars_put(name, v.clone());
    prog_gvars_push(v.clone());
    return v;
}

fn expect(ty: TokenType, tokens: &Vec<Token>) {
    let t = &tokens[pos()];
    if t.ty == ty {
        bump_pos();
        return;
    }

    if ty != TokenType::EOF {
        bad_token(t, format!("{:?} expected", ty));
    }
    assert!(ty == TokenType::WHILE);
    bad_token(t, format!("'while' expected"));
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

fn func_ty(base: Type) -> Type {
    let mut ty = alloc_type();
    ty.returning = Some(Box::new(base));
    return ty;
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
        return find_typedef(&t.name).is_some();
    }
    return t.ty == TokenType::INT ||
        t.ty == TokenType::CHAR ||
        t.ty == TokenType::VOID ||
        t.ty == TokenType::STRUCT ||
        t.ty == TokenType::TYPEOF;
}

fn decl_specifiers(tokens: &Vec<Token>) -> Type {
    let t = &tokens[bump_pos()];
    match t.ty {
        TokenType::IDENT => {
            let ty = find_typedef(&t.name);
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
        TokenType::TYPEOF => {
            expect(TokenType::BRA, tokens);
            let mut node = expr(tokens);
            expect(TokenType::KET, tokens);
            return get_type(&mut node);
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
                    ms.push(declaration(false, tokens));
                }
                members = Some(ms);
            }

            if tag.is_none() && members.is_none() {
                bad_token(t, "bad struct definition".to_string());
            }

            let mut ty: Option<Type> = None;
            if tag.is_some() && members.is_none() {
                ty = find_tag(&tag.clone().unwrap());
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
                    env_tags_put(tag.unwrap(), ty_tmp.clone());
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

pub fn new_node(op: NodeType, t: Option<Box<Token>>) -> Node {
    let mut node = alloc_node();
    node.op = op;
    node.token = t;
    return node;
}

fn new_loop(op: NodeType, t: Option<Box<Token>>) -> Node {
    let mut node = new_node(op, t);
    node.break_label = bump_nlabel();
    node.continue_label = bump_nlabel();
    return node;
}

fn new_binop(op: NodeType, t: Option<Box<Token>>, lhs: Node, rhs: Node) -> Node {
    let mut node = new_node(op, t);
    node.lhs = Some(Box::new(lhs));
    node.rhs = Some(Box::new(rhs));
    return node;
}

fn new_expr(op: NodeType, t: Option<Box<Token>>, expr: Node) -> Node {
    let mut node = new_node(op, t);
    node.expr = Some(Box::new(expr));
    return node;
}

pub fn new_int_node(val: i32, t: Option<Box<Token>>) -> Node {
    let mut node = new_node(NodeType::NUM, t);
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

fn string_literal(t: & Token) -> Node {
    let ty = ary_of(char_ty(), t.str_cnt.len() as i32);
    let name = format!(".L.str{}", bump_nlabel()).to_string();

    let mut node = new_node(NodeType::VAR, Some(Box::new(t.clone())));
    node.ty = Rc::new(RefCell::new(ty.clone()));
    node.var = Some(add_gvar(ty, name, Some(t.str_cnt.clone()), t.len));
    return node;
}

fn local_variable(t: & Token) -> Node {
    let var = find_var(&t.name);
    if var.is_none() {
        bad_token(t, "undefined variable".to_string());
    }
    let mut node = new_node(NodeType::VAR, Some(Box::new(t.clone())));
    let v = var.clone().unwrap();
    node.ty = Rc::new(RefCell::new(v.borrow().ty.clone()));
    node.name = t.name.clone();
    node.var = Some(var.unwrap());
    return node;
}

fn function_call(t: & Token, tokens: &Vec<Token>) -> Node {
    let var = find_var(&t.name);

    let mut node = new_node(NodeType::CALL, Some(Box::new(t.clone())));
    node.name = t.name.clone();

    let mut should_init = true;
    if var.is_some() {
        let v = var.clone().unwrap();
        if v.borrow().ty.ty == CType::FUNC {
            node.ty = Rc::new(RefCell::new(v.borrow().ty.clone()));

            should_init = false;
        }
    }
    if should_init {
        warn_token!(t, "undefined function".to_string());
        node.ty = Rc::new(RefCell::new(func_ty(int_ty())));
    }

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

fn primary(tokens: &Vec<Token>) -> Node {
    let t = &tokens[bump_pos()];

    if t.ty == TokenType::BRA {
        if consume(TokenType::C_BRA, tokens) {
            let mut node = new_node(NodeType::STMT_EXPR, Some(Box::new(t.clone())));
            node.body = Some(Box::new(compound_stmt(tokens)));
            expect(TokenType::KET, tokens);
            return node;
        }
        let node = expr(tokens);
        expect(TokenType::KET, tokens);
        return node;
    }

    if t.ty == TokenType::NUM {
        return new_int_node(t.val, Some(Box::new(t.clone())));
    }

    if t.ty == TokenType::STR {
        return string_literal(t);
    }

    if t.ty == TokenType::IDENT {
        if consume(TokenType::BRA, tokens) {
           return function_call(t, tokens);
        }
        return local_variable(t);
    }

    bad_token(t, "primary expression expected".to_string());
    panic!(); // To avoid compile error.
}

fn postfix(tokens: &Vec<Token>) -> Node {
    let mut lhs = primary(tokens);

    loop {
        let t = &tokens[pos()];

        if consume(TokenType::INC, tokens) {
            lhs = new_expr(NodeType::POST_INC, Some(Box::new(t.clone())), lhs);
            continue;
        }

        if consume(TokenType::DEC, tokens) {
            lhs = new_expr(NodeType::POST_DEC, Some(Box::new(t.clone())), lhs);
            continue;
        }

        if consume(TokenType::DOT, tokens) {
            lhs = new_expr(NodeType::DOT, Some(Box::new(t.clone())), lhs);
            lhs.name = ident(tokens);
            continue;
        }

        if consume(TokenType::ARROW, tokens) {
            lhs = new_expr(NodeType::DOT, Some(Box::new(t.clone())), new_expr(NodeType::DEREF, Some(Box::new(t.clone())), lhs));
            lhs.name = ident(tokens);
            continue;
        }

        if consume(TokenType::S_BRA, tokens) {
            lhs = new_expr(NodeType::DEREF, Some(Box::new(t.clone())), new_binop(NodeType::ADD, Some(Box::new(t.clone())), lhs, assign(tokens)));
            expect(TokenType::S_KET, tokens);
            continue;
        }
        return lhs;
    }
}

fn unary(tokens: &Vec<Token>) -> Node {
    let t = &tokens[pos()];

    if consume(TokenType::SUB, tokens) {
        return new_expr(NodeType::NEG, Some(Box::new(t.clone())), unary(tokens));
    }
    if consume(TokenType::MUL, tokens) {
        return new_expr(NodeType::DEREF, Some(Box::new(t.clone())), unary(tokens));
    }
    if consume(TokenType::AMP, tokens) {
        return new_expr(NodeType::ADDR, Some(Box::new(t.clone())), unary(tokens));
    }
    if consume(TokenType::EXCLAM, tokens) {
        return new_expr(NodeType::EXCLAM, Some(Box::new(t.clone())), unary(tokens));
    }
    if consume(TokenType::TILDA, tokens) {
        return new_expr(NodeType::NOT, Some(Box::new(t.clone())), unary(tokens));
    }
    if consume(TokenType::SIZEOF, tokens) {
        return new_int_node(get_type(&mut unary(tokens)).size, Some(Box::new(t.clone())));
    }
    if consume(TokenType::ALIGNOF, tokens) {
        return new_int_node(get_type(&mut unary(tokens)).align, Some(Box::new(t.clone())));
    }

    if consume(TokenType::INC, tokens) {
        return new_binop(NodeType::ADD_EQ, Some(Box::new(t.clone())), unary(tokens), new_int_node(1, Some(Box::new(t.clone()))));
    }
    if consume(TokenType::DEC, tokens) {
        return new_binop(NodeType::SUB_EQ, Some(Box::new(t.clone())), unary(tokens), new_int_node(1, Some(Box::new(t.clone()))));
    }

    return postfix(tokens);
}

fn mul(tokens: &Vec<Token>) -> Node {
    let mut lhs = unary(tokens);
    loop {
        let t = &tokens[pos()];
        if consume(TokenType::MUL, tokens) {
            lhs = new_binop(NodeType::MUL, Some(Box::new(t.clone())), lhs, unary(tokens));
        } else if consume(TokenType::DIV, tokens) {
            lhs = new_binop(NodeType::DIV, Some(Box::new(t.clone())), lhs, unary(tokens));
        } else if consume(TokenType::MOD, tokens) {
            lhs = new_binop(NodeType::MOD, Some(Box::new(t.clone())), lhs, unary(tokens));
        } else {
            return lhs;
        }
    }
}

fn add(tokens: &Vec<Token>) -> Node {
    let mut lhs = mul(tokens);
    loop {
        let t = &tokens[pos()];
        if consume(TokenType::ADD, tokens) {
            lhs = new_binop(NodeType::ADD, Some(Box::new(t.clone())), lhs, mul(tokens));
        } else if consume(TokenType::SUB, tokens) {
            lhs = new_binop(NodeType::SUB, Some(Box::new(t.clone())), lhs, mul(tokens));
        } else {
            return lhs;
        }
    }
}

fn shift(tokens: &Vec<Token>) -> Node {
    let mut lhs = add(tokens);
    loop {
        let t = &tokens[pos()];
        if consume(TokenType::SHL, tokens) {
            lhs = new_binop(NodeType::SHL, Some(Box::new(t.clone())), lhs, add(tokens));
        } else if consume(TokenType::SHR, tokens) {
            lhs = new_binop(NodeType::SHR, Some(Box::new(t.clone())), lhs, add(tokens));
        } else {
            return lhs;
        }
    }
}

fn relational(tokens: &Vec<Token>) -> Node {
    let mut lhs = shift(tokens);
    loop {
        let t = &tokens[pos()];
        if consume(TokenType::LT, tokens) {
            lhs = new_binop(NodeType::LT, Some(Box::new(t.clone())), lhs, shift(tokens));
        } else if consume(TokenType::GT, tokens) {
            lhs = new_binop(NodeType::LT, Some(Box::new(t.clone())), shift(tokens), lhs);
        } else if consume(TokenType::LE, tokens) {
            lhs = new_binop(NodeType::LE, Some(Box::new(t.clone())), lhs, shift(tokens));
        } else if consume(TokenType::GE, tokens) {
            lhs = new_binop(NodeType::LE, Some(Box::new(t.clone())), shift(tokens), lhs);
        } else {
            return lhs;
        }
    }
}

fn equality(tokens: &Vec<Token>) -> Node {
    let mut lhs = relational(tokens);
    loop {
        let t = &tokens[pos()];
        if consume(TokenType::EQ, tokens) {
            lhs = new_binop(NodeType::EQ, Some(Box::new(t.clone())), lhs, relational(tokens));
        } else if consume(TokenType::NE, tokens) {
            lhs = new_binop(NodeType::NE, Some(Box::new(t.clone())), lhs, relational(tokens));
        } else {
            return lhs;
        }
    }
}

fn bit_and(tokens: &Vec<Token>) -> Node {
    let mut lhs = equality(tokens);
    while consume(TokenType::AMP, tokens) {
        let t = &tokens[pos()];
        lhs = new_binop(NodeType::AND, Some(Box::new(t.clone())), lhs, equality(tokens));
    }
    return lhs;
}

fn bit_xor(tokens: &Vec<Token>) -> Node {
    let mut lhs = bit_and(tokens);
    while consume(TokenType::HAT, tokens) {
        let t = &tokens[pos()];
        lhs = new_binop(NodeType::XOR, Some(Box::new(t.clone())), lhs, bit_and(tokens));
    }
    return lhs;
}

fn bit_or(tokens: &Vec<Token>) -> Node {
    let mut lhs = bit_xor(tokens);
    while consume(TokenType::OR, tokens) {
        let t = &tokens[pos()];
        lhs = new_binop(NodeType::OR, Some(Box::new(t.clone())), lhs, bit_xor(tokens));
    }
    return lhs;
}

fn logand(tokens: &Vec<Token>) -> Node {
    let mut lhs = bit_or(tokens);
    while consume(TokenType::LOGAND, tokens) {
        let t = &tokens[pos()];
        lhs = new_binop(NodeType::LOGAND, Some(Box::new(t.clone())), lhs, bit_or(tokens));
    }
    return lhs;
}

fn logor(tokens: &Vec<Token>) -> Node {
    let mut lhs = logand(tokens);
    while consume(TokenType::LOGOR, tokens) {
        let t = &tokens[pos()];
        lhs = new_binop(NodeType::LOGOR, Some(Box::new(t.clone())), lhs, logand(tokens));
    }
    return lhs;
}

fn conditional(tokens: &Vec<Token>) -> Node {
    let cond = logor(tokens);
    let t = &tokens[pos()];
    if !consume(TokenType::QUEST, tokens) {
        return cond;
    }

    let mut node = new_node(NodeType::QUEST, Some(Box::new(t.clone())));
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
    let t = &tokens[pos()];
    let op = assignment_op(tokens);
    if let Some(o) = op {
        return new_binop(o, Some(Box::new(t.clone())), lhs, assign(tokens));
    }
    return lhs;
}

fn expr(tokens: &Vec<Token>) -> Node {
    let lhs = assign(tokens);
    let t = &tokens[pos()];
    if !consume(TokenType::COMMA, tokens) {
        return lhs;
    }
    return new_binop(NodeType::COMMA, Some(Box::new(t.clone())), lhs, expr(tokens));
}

fn read_array<'a>(ty: &'a mut Type, tokens: &Vec<Token>) -> &'a mut Type {
    let mut v = Vec::new();

    while consume(TokenType::S_BRA, tokens) {
        if consume(TokenType::S_KET, tokens) {
            v.push(-1);
            continue;
        }

        let t = &tokens[pos()];
        let len = expr(tokens);
        if len.op != NodeType::NUM {
            bad_token(t, "number expected".to_string());
        }
        v.push(len.val);
        expect(TokenType::S_KET, tokens);
    }
    for len in v.iter().rev() {
        *ty = ary_of(ty.clone(), *len);
    }
    return ty;
}

fn direct_decl(ty: Rc<RefCell<Type>>, tokens: &Vec<Token>) -> Node {
    let t = &tokens[pos()];
    let mut node = alloc_node();
    let node_ty = Rc::new(RefCell::new(alloc_type()));
    let placeholder = node_ty.clone();

    if t.ty == TokenType::IDENT {
        node = new_node(NodeType::VARDEF, Some(Box::new(t.clone())));
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

fn declaration(define: bool, tokens: &Vec<Token>) -> Node {
    let ty = decl_specifiers(tokens);
    let mut node = declarator(Rc::new(RefCell::new(ty)), tokens);
    expect(TokenType::SEMI_COLON, tokens);
    if define {
        node.var = Some(add_lvar(node.ty.borrow().clone(), node.name.clone()));
    }
    return node;
}

fn param_declaration(tokens: &Vec<Token>) -> Node {
    let ty = decl_specifiers(tokens);
    let mut node = declarator(Rc::new(RefCell::new(ty)), tokens);
    if node.ty.borrow().ty == CType::ARY {
        let tmp = Rc::new(RefCell::new(*node.ty.borrow().ary_of.clone().unwrap()));
        node.ty = Rc::new(RefCell::new(ptr_to(tmp)));
    }
    node.var = Some(add_lvar(node.ty.borrow().clone(), node.name.clone()));
    return node;
}

fn expr_stmt(tokens: &Vec<Token>) -> Node {
    let t = &tokens[pos()];
    let node = new_expr(NodeType::EXPR_STMT, Some(Box::new(t.clone())), expr(tokens));
    expect(TokenType::SEMI_COLON, tokens);
    return node;
}

pub fn stmt(tokens: &Vec<Token>) -> Node {
    let t = &tokens[bump_pos()];

    match t.ty {
        TokenType::TYPEDEF => {
            let node = declaration(false, tokens);
            assert!(node.name.len()>0);
            env_typedefs_put(node.name, node.ty.borrow().clone());
            return null_stmt();
        }
        TokenType::IF => {
            let mut node = new_node(NodeType::IF, Some(Box::new(t.clone())));
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
            let mut node = new_loop(NodeType::FOR, Some(Box::new(t.clone())));
            expect(TokenType::BRA, tokens);
            env_push();
            breaks_push(node.clone());
            continues_push(node.clone());

            if is_typename(tokens) {
                node.init = Some(Box::new(declaration(true, tokens)));
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
                node.inc = Some(Box::new(new_expr(NodeType::EXPR_STMT, Some(Box::new(t.clone())), expr(tokens))));
                expect(TokenType::KET, tokens);
            }

            node.body = Some(Box::new(stmt(tokens)));
            breaks_pop();
            continues_pop();
            env_pop();
            return node;
        }
        TokenType::WHILE => {
            let mut node = new_loop(NodeType::FOR, Some(Box::new(t.clone())));
            breaks_push(node.clone());
            continues_push(node.clone());

            node.init = Some(Box::new(null_stmt()));
            node.inc = Some(Box::new(null_stmt()));
            expect(TokenType::BRA, tokens);
            node.cond = Some(Box::new(expr(tokens)));
            expect(TokenType::KET, tokens);
            node.body = Some(Box::new(stmt(tokens)));
            
            breaks_pop();
            continues_pop();
            return node;
        }
        TokenType::DO => {
            let mut node = new_loop(NodeType::DO_WHILE, Some(Box::new(t.clone())));
            breaks_push(node.clone());
            continues_push(node.clone());

            node.body = Some(Box::new(stmt(tokens)));
            expect(TokenType::WHILE, tokens);
            expect(TokenType::BRA, tokens);
            node.cond = Some(Box::new(expr(tokens)));
            expect(TokenType::KET, tokens);
            expect(TokenType::SEMI_COLON, tokens);
            
            breaks_pop();
            continues_pop();
            return node;
        }
        TokenType::BREAK => {
            if breaks_len() == 0 {
                bad_token(t, "stray break".to_string());
            }
            let mut node =  new_node(NodeType::BREAK, Some(Box::new(t.clone())));
            node.target = Some(Box::new(breaks_last()));
            return node;
        }
        TokenType::CONTINUE => {
            if continues_len() == 0 {
                bad_token(t, "stray continue".to_string());
            }
            let mut node =  new_node(NodeType::CONTINUE, Some(Box::new(t.clone())));

            // 9cc use the last node in 'breaks', not 'continues'.
            node.target = Some(Box::new(continues_last()));

            return node;
        }
        TokenType::RETURN => {
            let mut node = new_node(NodeType::RETURN, Some(Box::new(t.clone())));
            node.expr = Some(Box::new(expr(tokens)));
            expect(TokenType::SEMI_COLON, tokens);
            return node;
        }
        TokenType::C_BRA => {
            env_push();
            let mut node = new_node(NodeType::COMP_STMT, Some(Box::new(t.clone())));
            while !consume(TokenType::C_KET, tokens) {
                node.stmts.push(stmt(tokens));
            }
            env_pop();
            return node;
        }
        TokenType::SEMI_COLON => {
            return null_stmt();
        }
        _ => {
            dump_pos();
            if is_typename(tokens) {
                return declaration(true, tokens);
            }
            return expr_stmt(tokens);
        }
    }
}

pub fn compound_stmt(tokens: &Vec<Token>) -> Node {
    let t = &tokens[pos()];
    let mut node = new_node(NodeType::COMP_STMT, Some(Box::new(t.clone())));

    env_push();
    while !consume(TokenType::C_KET, tokens) {
        node.stmts.push(stmt(tokens));
    }
    env_pop();

    return node;
}

fn toplevel(tokens: &Vec<Token>) {
    let is_typedef = consume(TokenType::TYPEDEF, tokens);
    let is_extern = consume(TokenType::EXTERN, tokens);

    let mut ty = decl_specifiers(tokens);
    while consume(TokenType::MUL, tokens) {
        ty = ptr_to(Rc::new(RefCell::new(ty)));
    }

    let name = ident(tokens);

    // Function
    if consume(TokenType::BRA, tokens) {
        let t = &tokens[pos()];
        let node = Rc::new(RefCell::new(new_node(NodeType::DECL, Some(Box::new(t.clone())))));
        prog_nodes_push(node.clone());

        init_lvars();
        init_breaks();
        init_continues();

        node.borrow_mut().name = name.clone();

        let mut node_ty = alloc_type();
        node_ty.ty = CType::FUNC;
        node_ty.returning = Some(Box::new(ty));
        node.borrow_mut().ty = Rc::new(RefCell::new(node_ty));

        if !consume(TokenType::KET, tokens) {
            node.borrow_mut().args.push(param_declaration(tokens));
            while consume(TokenType::COMMA, tokens) {
                node.borrow_mut().args.push(param_declaration(tokens));
            }
            expect(TokenType::KET, tokens);
        }

        let ty = node.borrow().ty.clone();
        add_lvar(ty.borrow().clone(), name);

        if consume(TokenType::SEMI_COLON, tokens) {
            return;
        }

        node.borrow_mut().op = NodeType::FUNC;
        let t = &tokens[pos()];
        expect(TokenType::C_BRA, tokens);
        if is_typedef {
            bad_token(t, format!("typedef has function definition"));
        }
        node.borrow_mut().body = Some(Box::new(compound_stmt(tokens)));
        node.borrow_mut().lvars = lvars();
        return;
    }

    let mut ty_tmp = ty.clone();
    let ty = read_array(&mut ty_tmp, tokens);
    expect(TokenType::SEMI_COLON, tokens);

    if is_typedef {
        env_typedefs_put(name, ty.clone());
        return;
    }

    // Global variable
    ty.is_extern = is_extern;
    let mut data = String::new();
    for _i in 0..ty.size {
        data.push(char::from(0));
    }
    let len = ty.size;
    add_gvar(ty.clone(), name, None, len as usize);
}

fn is_eof(tokens: &Vec<Token>) -> bool {
    let t = &tokens[pos()];
    return t.ty == TokenType::EOF;
}

pub fn parse(tokens: &Vec<Token>) -> Program {
    while !is_eof(tokens) {
        toplevel(tokens);
    }

    return PROGRAM.with(|p| {
        p.borrow().clone()
    });
}
