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
use crate::sema::*;
use crate::token::*;
use crate::util::*;
use std::cell::RefCell;
use std::collections::BTreeMap;
use std::collections::HashMap;
use std::rc::Rc;

thread_local! {
    static PROGRAM: RefCell<Program> = RefCell::new(new_program());
    static POS: RefCell<usize> = RefCell::new(0);
    static ENV: RefCell<Env> = RefCell::new(new_env(None));
    static LVARS: RefCell<Vec<Rc<RefCell<Var>>>> = RefCell::new(Vec::new());
    static BREAKS: RefCell<Vec<Rc<RefCell<Node>>>> = RefCell::new(Vec::new());
    static CONTINUES: RefCell<Vec<Rc<RefCell<Node>>>> = RefCell::new(Vec::new());
    static SWITCHES: RefCell<Vec<Rc<RefCell<Node>>>> = RefCell::new(Vec::new());
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

fn breaks_push(node: Rc<RefCell<Node>>) {
    BREAKS.with(|p| {
        p.borrow_mut().push(node);
    })
}

fn breaks_pop() -> Option<Rc<RefCell<Node>>> {
    BREAKS.with(|p| {
        return p.borrow_mut().pop();
    })
}

fn breaks_last() -> Rc<RefCell<Node>> {
    BREAKS.with(|p| {
        let l = p.borrow().len();
        return p.borrow()[l - 1].clone();
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

fn continues_push(node: Rc<RefCell<Node>>) {
    CONTINUES.with(|p| {
        p.borrow_mut().push(node);
    })
}

fn continues_pop() -> Option<Rc<RefCell<Node>>> {
    CONTINUES.with(|p| {
        return p.borrow_mut().pop();
    })
}

fn continues_last() -> Rc<RefCell<Node>> {
    CONTINUES.with(|p| {
        let l = p.borrow().len();
        return p.borrow()[l - 1].clone();
    })
}

fn init_switches() {
    SWITCHES.with(|p| {
        *p.borrow_mut() = Vec::new();
    })
}

fn switches_len() -> usize {
    SWITCHES.with(|p| {
        return p.borrow().len();
    })
}

fn switches_push(node: Rc<RefCell<Node>>) {
    SWITCHES.with(|p| {
        p.borrow_mut().push(node);
    })
}

fn switches_pop() -> Option<Rc<RefCell<Node>>> {
    SWITCHES.with(|p| {
        return p.borrow_mut().pop();
    })
}

fn switches_last() -> Rc<RefCell<Node>> {
    SWITCHES.with(|p| {
        let l = p.borrow().len();
        p.borrow_mut()[l - 1].clone()
    })
}

fn prog_gvars_push(var: Rc<RefCell<Var>>) {
    PROGRAM.with(|p| {
        p.borrow_mut().gvars.push(var);
    })
}

fn prog_funcs_push(func: Rc<RefCell<Function>>) {
    PROGRAM.with(|p| {
        p.borrow_mut().funcs.push(func);
    })
}

fn pos() -> usize {
    POS.with(|pos| *pos.borrow())
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
pub struct Function {
    pub name: String,
    pub node: Rc<RefCell<Node>>,
    pub lvars: Vec<Rc<RefCell<Var>>>,
    pub bbs: Vec<Rc<RefCell<BB>>>,
}

#[derive(Clone, Debug)]
pub struct Program {
    pub gvars: Vec<Rc<RefCell<Var>>>,
    pub funcs: Vec<Rc<RefCell<Function>>>,
}

pub fn new_program() -> Program {
    Program {
        gvars: Vec::new(),
        funcs: Vec::new(),
    }
}

#[derive(Clone, Debug)]
struct Env {
    vars: HashMap<String, Rc<RefCell<Var>>>,
    typedefs: HashMap<String, Type>,
    tags: HashMap<String, Type>,
    prev: Option<Box<Env>>,
}

impl Env {
    fn find_var(&self, name: &String) -> Option<Rc<RefCell<Var>>> {
        match self.vars.get(name) {
            None => match self.prev {
                None => None,
                Some(ref prev) => prev.find_var(name),
            },
            v => Some(v.unwrap().clone()),
        }
    }

    fn find_typedef(&self, name: &String) -> Option<Type> {
        match self.typedefs.get(name) {
            None => match self.prev {
                None => None,
                Some(ref prev) => prev.find_typedef(name),
            },
            ty => Some(ty.unwrap().clone()),
        }
    }

    fn find_tag(&self, name: &String) -> Option<Type> {
        match self.tags.get(name) {
            None => match self.prev {
                None => None,
                Some(ref prev) => prev.find_tag(name),
            },
            ty => Some(ty.unwrap().clone()),
        }
    }
}

fn new_env(prev: Option<Box<Env>>) -> Env {
    Env {
        vars: HashMap::new(),
        typedefs: HashMap::new(),
        tags: HashMap::new(),
        prev: prev,
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
        let e = env.borrow().prev.clone().unwrap();
        *env.borrow_mut() = *e;
    })
}

#[derive(Clone, Debug, PartialEq)]
pub enum NodeType {
    ADD,    // +
    SUB,    // -
    MUL,    // *
    DIV,    // /
    EQL,    // =
    LT,     // <
    OR,     // |
    NOT,    // ~
    XOR,    // ^
    AND,    // &
    EXCLAM, // !
    QUEST,  // ?
    COMMA,  // ,
    NUM,    // Number literal
    //STRUCT,    // Struct
    DECL,      // declaration
    VARDEF,    // Variable definition
    VARREF,    // Variable reference
    CAST,      // Cast
    IF,        // "if"
    FOR,       // "for"
    DO_WHILE,  // do ... while
    SWITCH,    // switch
    CASE,      // case
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
    VOID,
    BOOL,
    CHAR,
    INT,
    PTR,
    ARY,
    STRUCT,
    FUNC,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Type {
    pub ty: CType,
    pub size: i32,  // sizeof
    pub align: i32, // alignof

    // Pointer
    pub ptr_to: Option<Rc<RefCell<Type>>>,

    // Array
    pub ary_of: Option<Box<Type>>,
    pub len: i32,

    // Struct
    pub members: Option<BTreeMap<String, Rc<RefCell<Type>>>>,
    pub offset: i32,

    // Function
    pub returning: Option<Box<Type>>,
}

impl Type {
    fn fix_struct_offsets(&mut self) {
        let mut off = 0;
        match self.members {
            Some(ref ms) => {
                for t2 in ms.values() {
                    off = roundup(off, t2.borrow().align);
                    t2.borrow_mut().offset = off;
                    off += t2.borrow().size;

                    let align = t2.borrow().align;
                    if self.align < align {
                        self.align = align;
                    }
                }
            }
            None => {}
        }
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
        returning: None,
    }
}

// Represents a variable.
#[derive(Clone, Debug, PartialEq)]
pub struct Var {
    pub ty: Type,
    pub name: String,
    pub is_local: bool,

    // Local variables are compiled to offsets from RBP.
    pub offset: i32,

    // Global variables are compiled to labels with optional
    // initialized data.
    pub data: Option<String>,

    // For optimizatin passes.
    pub address_taken: bool,
    pub promoted: Option<Rc<RefCell<Reg>>>,
}

pub fn alloc_var() -> Var {
    Var {
        ty: alloc_type(),
        is_local: false,

        offset: 0,

        name: String::new(),
        data: None,

        address_taken: false,
        promoted: None,
    }
}

// AST node
#[derive(Clone, Debug, PartialEq)]
pub struct Node {
    pub op: NodeType,                    // Node type
    pub ty: Rc<RefCell<Type>>,           // C type
    pub lhs: Option<Rc<RefCell<Node>>>,  // left-hand side
    pub rhs: Option<Rc<RefCell<Node>>>,  // right-hand side
    pub val: i32,                        // Nubmer literal
    pub expr: Option<Rc<RefCell<Node>>>, // "return" or expression stmt
    pub stmts: Vec<Rc<RefCell<Node>>>,   // Compound statemtn

    pub name: String,

    // For NodeType::VARREF
    pub var: Option<Rc<RefCell<Var>>>,

    // "if" ( cond ) then "else els
    // "for" ( init; cond; inc ) body
    // "while" ( cond ) body
    // "do" body "while" ( cond )
    // "switch" ( cond ) body
    // "case" val ":" body
    pub cond: Option<Rc<RefCell<Node>>>,
    pub then: Option<Rc<RefCell<Node>>>,
    pub els: Option<Rc<RefCell<Node>>>,
    pub init: Option<Rc<RefCell<Node>>>,
    pub inc: Option<Rc<RefCell<Node>>>,
    pub body: Option<Rc<RefCell<Node>>>,

    // For switch and case
    pub cases: Vec<Rc<RefCell<Node>>>,
    pub bb: Rc<RefCell<BB>>,

    // For case, break and continue
    pub target: Option<Rc<RefCell<Node>>>,
    pub break_: Rc<RefCell<BB>>,
    pub continue_: Rc<RefCell<BB>>,

    // Function definition
    pub params: Vec<Rc<RefCell<Var>>>,

    // Function call
    pub args: Vec<Rc<RefCell<Node>>>,

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

        cases: Vec::new(),
        bb: Rc::new(RefCell::new(alloc_bb())),

        break_: Rc::new(RefCell::new(alloc_bb())),
        continue_: Rc::new(RefCell::new(alloc_bb())),

        target: None,

        params: Vec::new(),
        args: Vec::new(),

        token: None,
    }
}

fn null_stmt() -> Rc<RefCell<Node>> {
    let mut node = alloc_node();
    node.op = NodeType::NULL;
    return Rc::new(RefCell::new(node));
}

fn find_var(name: &String) -> Option<Rc<RefCell<Var>>> {
    ENV.with(|env| return env.borrow().find_var(name))
}

fn find_typedef(name: &String) -> Option<Type> {
    ENV.with(|env| return env.borrow().find_typedef(name))
}

fn find_tag(name: &String) -> Option<Type> {
    ENV.with(|env| {
        return env.borrow().find_tag(name);
    })
}

fn add_lvar(ty: Type, name: String) -> Rc<RefCell<Var>> {
    let mut var = alloc_var();
    var.ty = ty;
    var.is_local = true;
    var.name = name.clone();
    let v = Rc::new(RefCell::new(var));
    env_vars_put(name, v.clone());
    lvars_push(v.clone());
    return v;
}

fn add_gvar(ty: Type, name: String, data: Option<String>, is_extern: bool) -> Rc<RefCell<Var>> {
    let mut var = alloc_var();
    var.ty = ty;
    var.name = name.clone();
    var.data = data;
    let v = Rc::new(RefCell::new(var));
    env_vars_put(name, v.clone());
    if !is_extern {
        prog_gvars_push(v.clone());
    }
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
    return t.ty == TokenType::INT
        || t.ty == TokenType::CHAR
        || t.ty == TokenType::VOID
        || t.ty == TokenType::STRUCT
        || t.ty == TokenType::TYPEOF
        || t.ty == TokenType::BOOL;
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
        TokenType::VOID => {
            return void_ty();
        }
        TokenType::BOOL => {
            return bool_ty();
        }
        TokenType::CHAR => {
            return char_ty();
        }
        TokenType::INT => {
            return int_ty();
        }
        TokenType::TYPEOF => {
            expect(TokenType::BRA, tokens);
            let node = expr(tokens);
            expect(TokenType::KET, tokens);
            return get_type(node);
        }
        TokenType::STRUCT => {
            let t = &tokens[pos()];
            let mut ty: Option<Type> = None;
            let mut tag: Option<String> = None;

            if t.ty == TokenType::IDENT {
                bump_pos();
                tag = Some(t.name.clone());
                ty = find_tag(&t.name);
            }

            if ty.is_none() {
                let mut ty_tmp = alloc_type();
                ty_tmp.ty = CType::STRUCT;
                ty = Some(ty_tmp);
            }

            if consume(TokenType::C_BRA, tokens) {
                let mut ty_tmp = ty.clone().unwrap();
                let mut members = BTreeMap::new();
                while !consume(TokenType::C_KET, tokens) {
                    let node = declaration_type(tokens);
                    members.insert(node.name, node.ty);
                }
                ty_tmp.members = Some(members);
                ty_tmp.fix_struct_offsets();
                ty = Some(ty_tmp);
            }

            if tag.is_none() && ty.clone().unwrap().members.is_none() {
                bad_token(t, "bad struct definition".to_string());
            }

            if tag.is_some() {
                let ty_tmp = ty.clone().unwrap();
                env_tags_put(tag.unwrap(), ty_tmp.clone());
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

fn new_binop(
    op: NodeType,
    t: Option<Box<Token>>,
    lhs: Rc<RefCell<Node>>,
    rhs: Rc<RefCell<Node>>,
) -> Rc<RefCell<Node>> {
    let mut node = new_node(op, t);
    node.lhs = Some(lhs);
    node.rhs = Some(rhs);
    return Rc::new(RefCell::new(node));
}

fn new_expr(op: NodeType, t: Option<Box<Token>>, expr: Rc<RefCell<Node>>) -> Rc<RefCell<Node>> {
    let mut node = new_node(op, t);
    node.expr = Some(expr);
    return Rc::new(RefCell::new(node));
}

fn new_varref(t: Option<Box<Token>>, var: Rc<RefCell<Var>>) -> Rc<RefCell<Node>> {
    let mut node = new_node(NodeType::VARREF, t);
    node.ty = Rc::new(RefCell::new(var.borrow().ty.clone()));
    node.var = Some(var);
    return Rc::new(RefCell::new(node));
}

fn new_deref(t: Option<Box<Token>>, var: Rc<RefCell<Var>>) -> Rc<RefCell<Node>> {
    return new_expr(NodeType::DEREF, t.clone(), new_varref(t, var));
}

pub fn new_int_node(val: i32, t: Option<Box<Token>>) -> Rc<RefCell<Node>> {
    let mut node = new_node(NodeType::NUM, t);
    node.ty = Rc::new(RefCell::new(int_ty()));
    node.val = val;
    return Rc::new(RefCell::new(node));
}

fn ident(tokens: &Vec<Token>) -> String {
    let t = &tokens[bump_pos()];
    if t.ty != TokenType::IDENT {
        bad_token(t, "identifier expected".to_string());
    }
    return t.name.clone();
}

fn string_literal(t: &Token) -> Rc<RefCell<Node>> {
    let ty = ary_of(char_ty(), t.str_cnt.len() as i32);
    let name = format!(".L.str{}", bump_nlabel()).to_string();

    let mut node = new_node(NodeType::VARREF, Some(Box::new(t.clone())));
    node.ty = Rc::new(RefCell::new(ty.clone()));
    node.var = Some(add_gvar(ty, name, Some(t.str_cnt.clone()), false));
    return Rc::new(RefCell::new(node));
}

fn local_variable(t: &Token) -> Rc<RefCell<Node>> {
    let var = find_var(&t.name);
    if var.is_none() {
        bad_token(t, "undefined variable".to_string());
    }
    let mut node = new_node(NodeType::VARREF, Some(Box::new(t.clone())));
    let v = var.clone().unwrap();
    node.ty = Rc::new(RefCell::new(v.borrow().ty.clone()));
    node.name = t.name.clone();
    node.var = Some(var.unwrap());
    return Rc::new(RefCell::new(node));
}

fn function_call(t: &Token, tokens: &Vec<Token>) -> Rc<RefCell<Node>> {
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

    while !consume(TokenType::KET, tokens) {
        if node.args.len() > 0 {
            expect(TokenType::COMMA, tokens);
        }
        node.args.push(assign(tokens));
    }
    return Rc::new(RefCell::new(node));
}

fn stmt_expr(tokens: &Vec<Token>) -> Rc<RefCell<Node>> {
    let t = &tokens[pos()];
    let mut v = Vec::new();

    env_push();
    v.push(stmt(tokens));
    while !consume(TokenType::C_KET, tokens) {
        v.push(stmt(tokens));
    }
    expect(TokenType::KET, tokens);
    env_pop();

    let last = v.pop().unwrap();
    let op = last.borrow().op.clone();
    if op != NodeType::EXPR_STMT {
        let t = last.borrow().token.clone();
        bad_token(
            &*t.unwrap(),
            "statement expression returning void".to_string(),
        );
    }

    let mut node = new_node(NodeType::STMT_EXPR, Some(Box::new(t.clone())));
    node.stmts = v;
    node.expr = last.borrow().expr.clone();
    return Rc::new(RefCell::new(node));
}

fn primary(tokens: &Vec<Token>) -> Rc<RefCell<Node>> {
    let t = &tokens[bump_pos()];

    if t.ty == TokenType::BRA {
        if consume(TokenType::C_BRA, tokens) {
            return stmt_expr(tokens);
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

fn new_stmt_expr(t: Option<Box<Token>>, es: Vec<Rc<RefCell<Node>>>) -> Rc<RefCell<Node>> {
    let mut exprs = es;
    let last = exprs.pop();

    let mut v = Vec::new();
    for e in exprs.iter() {
        v.push(new_expr(NodeType::EXPR_STMT, t.clone(), e.clone()));
    }

    let mut node = new_node(NodeType::STMT_EXPR, t.clone());
    node.stmts = v;
    if let Some(l) = last {
        node.expr = Some(l);
    } else {
        node.expr = None;
    }
    return Rc::new(RefCell::new(node));
}

// `x++` where x is of type T is compiled as
// `({ T *y = &x; T z = *y; *y = *y + 1; *z; })`.
fn new_post_inc(t: Option<Box<Token>>, e: Rc<RefCell<Node>>, imm: i32) -> Rc<RefCell<Node>> {
    let mut v = Vec::new();

    let var1 = add_lvar(ptr_to(e.borrow().ty.clone()), "tmp".to_string());
    let e_ty = e.borrow().ty.clone();
    let var2 = add_lvar(e_ty.borrow().clone(), "tmp".to_string());

    v.push(new_binop(
        NodeType::EQL,
        t.clone(),
        new_varref(t.clone(), var1.clone()),
        new_expr(NodeType::ADDR, t.clone(), e.clone()),
    ));
    v.push(new_binop(
        NodeType::EQL,
        t.clone(),
        new_varref(t.clone(), var2.clone()),
        new_deref(t.clone(), var1.clone()),
    ));
    v.push(new_binop(
        NodeType::EQL,
        t.clone(),
        new_deref(t.clone(), var1.clone()),
        new_binop(
            NodeType::ADD,
            t.clone(),
            new_deref(t.clone(), var1),
            new_int_node(imm, t.clone()),
        ),
    ));
    v.push(new_varref(t.clone(), var2));
    return new_stmt_expr(t, v);
}

fn postfix(tokens: &Vec<Token>) -> Rc<RefCell<Node>> {
    let mut lhs = primary(tokens);

    loop {
        let t = &tokens[pos()];

        if consume(TokenType::INC, tokens) {
            lhs = new_post_inc(Some(Box::new(t.clone())), lhs, 1);
            continue;
        }

        if consume(TokenType::DEC, tokens) {
            lhs = new_post_inc(Some(Box::new(t.clone())), lhs, -1);
            continue;
        }

        if consume(TokenType::DOT, tokens) {
            lhs = new_expr(NodeType::DOT, Some(Box::new(t.clone())), lhs);
            lhs.borrow_mut().name = ident(tokens);
            continue;
        }

        if consume(TokenType::ARROW, tokens) {
            lhs = new_expr(
                NodeType::DOT,
                Some(Box::new(t.clone())),
                new_expr(NodeType::DEREF, Some(Box::new(t.clone())), lhs),
            );
            lhs.borrow_mut().name = ident(tokens);
            continue;
        }

        if consume(TokenType::S_BRA, tokens) {
            lhs = new_expr(
                NodeType::DEREF,
                Some(Box::new(t.clone())),
                new_binop(
                    NodeType::ADD,
                    Some(Box::new(t.clone())),
                    lhs,
                    assign(tokens),
                ),
            );
            expect(TokenType::S_KET, tokens);
            continue;
        }
        return lhs;
    }
}

fn unary(tokens: &Vec<Token>) -> Rc<RefCell<Node>> {
    let t = &tokens[pos()];

    if consume(TokenType::SUB, tokens) {
        return new_binop(
            NodeType::SUB,
            Some(Box::new(t.clone())),
            new_int_node(0, Some(Box::new(t.clone()))),
            unary(tokens),
        );
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
        return new_int_node(get_type(unary(tokens)).size, Some(Box::new(t.clone())));
    }
    if consume(TokenType::ALIGNOF, tokens) {
        return new_int_node(get_type(unary(tokens)).align, Some(Box::new(t.clone())));
    }

    if consume(TokenType::INC, tokens) {
        return new_assign_eq(
            NodeType::ADD,
            unary(tokens),
            new_int_node(1, Some(Box::new(t.clone()))),
        );
    }
    if consume(TokenType::DEC, tokens) {
        return new_assign_eq(
            NodeType::SUB,
            unary(tokens),
            new_int_node(1, Some(Box::new(t.clone()))),
        );
    }

    return postfix(tokens);
}

fn mul(tokens: &Vec<Token>) -> Rc<RefCell<Node>> {
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

fn add(tokens: &Vec<Token>) -> Rc<RefCell<Node>> {
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

fn shift(tokens: &Vec<Token>) -> Rc<RefCell<Node>> {
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

fn relational(tokens: &Vec<Token>) -> Rc<RefCell<Node>> {
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

fn equality(tokens: &Vec<Token>) -> Rc<RefCell<Node>> {
    let mut lhs = relational(tokens);
    loop {
        let t = &tokens[pos()];
        if consume(TokenType::EQ, tokens) {
            lhs = new_binop(
                NodeType::EQ,
                Some(Box::new(t.clone())),
                lhs,
                relational(tokens),
            );
        } else if consume(TokenType::NE, tokens) {
            lhs = new_binop(
                NodeType::NE,
                Some(Box::new(t.clone())),
                lhs,
                relational(tokens),
            );
        } else {
            return lhs;
        }
    }
}

fn bit_and(tokens: &Vec<Token>) -> Rc<RefCell<Node>> {
    let mut lhs = equality(tokens);
    while consume(TokenType::AMP, tokens) {
        let t = &tokens[pos()];
        lhs = new_binop(
            NodeType::AND,
            Some(Box::new(t.clone())),
            lhs,
            equality(tokens),
        );
    }
    return lhs;
}

fn bit_xor(tokens: &Vec<Token>) -> Rc<RefCell<Node>> {
    let mut lhs = bit_and(tokens);
    while consume(TokenType::HAT, tokens) {
        let t = &tokens[pos()];
        lhs = new_binop(
            NodeType::XOR,
            Some(Box::new(t.clone())),
            lhs,
            bit_and(tokens),
        );
    }
    return lhs;
}

fn bit_or(tokens: &Vec<Token>) -> Rc<RefCell<Node>> {
    let mut lhs = bit_xor(tokens);
    while consume(TokenType::OR, tokens) {
        let t = &tokens[pos()];
        lhs = new_binop(
            NodeType::OR,
            Some(Box::new(t.clone())),
            lhs,
            bit_xor(tokens),
        );
    }
    return lhs;
}

fn logand(tokens: &Vec<Token>) -> Rc<RefCell<Node>> {
    let mut lhs = bit_or(tokens);
    while consume(TokenType::LOGAND, tokens) {
        let t = &tokens[pos()];
        lhs = new_binop(
            NodeType::LOGAND,
            Some(Box::new(t.clone())),
            lhs,
            bit_or(tokens),
        );
    }
    return lhs;
}

fn logor(tokens: &Vec<Token>) -> Rc<RefCell<Node>> {
    let mut lhs = logand(tokens);
    while consume(TokenType::LOGOR, tokens) {
        let t = &tokens[pos()];
        lhs = new_binop(
            NodeType::LOGOR,
            Some(Box::new(t.clone())),
            lhs,
            logand(tokens),
        );
    }
    return lhs;
}

fn conditional(tokens: &Vec<Token>) -> Rc<RefCell<Node>> {
    let cond = logor(tokens);
    let t = &tokens[pos()];
    if !consume(TokenType::QUEST, tokens) {
        return cond;
    }

    let mut node = new_node(NodeType::QUEST, Some(Box::new(t.clone())));
    node.cond = Some(cond);
    node.then = Some(expr(tokens));
    expect(TokenType::COLON, tokens);
    node.els = Some(expr(tokens));
    return Rc::new(RefCell::new(node));
}

// `x op= y` where x is of type T is compiled as
// `({T *z = &x; *z = *z op y; })`.
fn new_assign_eq(
    op: NodeType,
    lhs: Rc<RefCell<Node>>,
    rhs: Rc<RefCell<Node>>,
) -> Rc<RefCell<Node>> {
    let mut v = Vec::new();
    let t = lhs.borrow().token.clone();

    // T *z = &x;
    let var = add_lvar(ptr_to(lhs.borrow().ty.clone()), "tmp".to_string());
    v.push(new_binop(
        NodeType::EQL,
        t.clone(),
        new_varref(t.clone(), var.clone()),
        new_expr(NodeType::ADDR, t.clone(), lhs),
    ));

    // *z = *z op y
    v.push(new_binop(
        NodeType::EQL,
        t.clone(),
        new_deref(t.clone(), var.clone()),
        new_binop(op, t.clone(), new_deref(t.clone(), var), rhs),
    ));
    return new_stmt_expr(t, v);
}

fn assign(tokens: &Vec<Token>) -> Rc<RefCell<Node>> {
    let lhs = conditional(tokens);
    let t = &tokens[pos()];

    if consume(TokenType::EQL, tokens) {
        new_binop(
            NodeType::EQL,
            Some(Box::new(t.clone())),
            lhs,
            assign(tokens),
        )
    } else if consume(TokenType::MUL_EQ, tokens) {
        new_assign_eq(NodeType::MUL, lhs, assign(tokens))
    } else if consume(TokenType::DIV_EQ, tokens) {
        new_assign_eq(NodeType::DIV, lhs, assign(tokens))
    } else if consume(TokenType::MOD_EQ, tokens) {
        new_assign_eq(NodeType::MOD, lhs, assign(tokens))
    } else if consume(TokenType::ADD_EQ, tokens) {
        new_assign_eq(NodeType::ADD, lhs, assign(tokens))
    } else if consume(TokenType::SUB_EQ, tokens) {
        new_assign_eq(NodeType::SUB, lhs, assign(tokens))
    } else if consume(TokenType::SHL_EQ, tokens) {
        new_assign_eq(NodeType::SHL, lhs, assign(tokens))
    } else if consume(TokenType::SHR_EQ, tokens) {
        new_assign_eq(NodeType::SHR, lhs, assign(tokens))
    } else if consume(TokenType::AND_EQ, tokens) {
        new_assign_eq(NodeType::LOGAND, lhs, assign(tokens))
    } else if consume(TokenType::XOR_EQ, tokens) {
        new_assign_eq(NodeType::XOR, lhs, assign(tokens))
    } else if consume(TokenType::OR_EQ, tokens) {
        new_assign_eq(NodeType::OR, lhs, assign(tokens))
    } else {
        lhs
    }
}

fn expr(tokens: &Vec<Token>) -> Rc<RefCell<Node>> {
    let lhs = assign(tokens);
    let t = &tokens[pos()];
    if !consume(TokenType::COMMA, tokens) {
        return lhs;
    }
    return new_binop(
        NodeType::COMMA,
        Some(Box::new(t.clone())),
        lhs,
        expr(tokens),
    );
}

fn const_expr(tokens: &Vec<Token>) -> i32 {
    let t = &tokens[pos()];
    let node = expr(tokens);
    if node.borrow().op != NodeType::NUM {
        bad_token(t, "constant expression expected".to_string());
    }
    return node.borrow().val;
}

fn read_array<'a>(ty: &'a mut Type, tokens: &Vec<Token>) -> &'a mut Type {
    let mut v = Vec::new();

    while consume(TokenType::S_BRA, tokens) {
        if consume(TokenType::S_KET, tokens) {
            v.push(-1);
            continue;
        }

        v.push(const_expr(tokens));
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
        node.init = Some(assign(tokens));
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

fn declaration_type(tokens: &Vec<Token>) -> Node {
    let ty = decl_specifiers(tokens);
    let node = declarator(Rc::new(RefCell::new(ty)), tokens);
    expect(TokenType::SEMI_COLON, tokens);
    return node;
}

fn declaration(tokens: &Vec<Token>) -> Rc<RefCell<Node>> {
    let ty = decl_specifiers(tokens);
    let mut node = declarator(Rc::new(RefCell::new(ty)), tokens);
    expect(TokenType::SEMI_COLON, tokens);
    let var = add_lvar(node.ty.borrow().clone(), node.name);

    if node.init.is_none() {
        return null_stmt();
    }

    // Convert `T var = init` to `T var; var = init`.
    let t = node.token;
    let lhs = new_varref(t.clone(), var);
    let rhs = node.init.clone().unwrap();
    node.init = None;

    let expr = new_binop(NodeType::EQL, t.clone(), lhs, rhs);
    return new_expr(NodeType::EXPR_STMT, t, expr);
}

fn param_declaration(tokens: &Vec<Token>) -> Rc<RefCell<Var>> {
    let mut ty = decl_specifiers(tokens);
    let node = declarator(Rc::new(RefCell::new(ty)), tokens);
    ty = node.ty.borrow().clone();
    if ty.ty == CType::ARY {
        let ary_of = ty.clone().ary_of;
        ty = ptr_to(Rc::new(RefCell::new(*ary_of.clone().unwrap())));
    }
    return add_lvar(ty, node.name);
}

fn expr_stmt(tokens: &Vec<Token>) -> Rc<RefCell<Node>> {
    let t = &tokens[pos()];
    let node = new_expr(NodeType::EXPR_STMT, Some(Box::new(t.clone())), expr(tokens));
    expect(TokenType::SEMI_COLON, tokens);
    return node;
}

pub fn stmt(tokens: &Vec<Token>) -> Rc<RefCell<Node>> {
    let t = &tokens[bump_pos()];

    match t.ty {
        TokenType::TYPEDEF => {
            let node = declaration_type(tokens);
            assert!(node.name.len() > 0);
            env_typedefs_put(node.name, node.ty.borrow().clone());
            return null_stmt();
        }
        TokenType::IF => {
            let mut node = new_node(NodeType::IF, Some(Box::new(t.clone())));
            expect(TokenType::BRA, tokens);
            node.cond = Some(expr(tokens));
            expect(TokenType::KET, tokens);
            node.then = Some(stmt(tokens));
            if consume(TokenType::ELSE, tokens) {
                node.els = Some(stmt(tokens));
            }
            return Rc::new(RefCell::new(node));
        }
        TokenType::FOR => {
            let node = Rc::new(RefCell::new(new_node(
                NodeType::FOR,
                Some(Box::new(t.clone())),
            )));
            expect(TokenType::BRA, tokens);
            env_push();
            breaks_push(node.clone());
            continues_push(node.clone());

            if is_typename(tokens) {
                node.borrow_mut().init = Some(declaration(tokens));
            } else if !consume(TokenType::SEMI_COLON, tokens) {
                node.borrow_mut().init = Some(expr_stmt(tokens));
            }

            if !consume(TokenType::SEMI_COLON, tokens) {
                node.borrow_mut().cond = Some(expr(tokens));
                expect(TokenType::SEMI_COLON, tokens);
            }

            if !consume(TokenType::KET, tokens) {
                node.borrow_mut().inc = Some(expr(tokens));
                expect(TokenType::KET, tokens);
            }

            node.borrow_mut().body = Some(stmt(tokens));
            breaks_pop();
            continues_pop();
            env_pop();
            return node;
        }
        TokenType::WHILE => {
            let node = Rc::new(RefCell::new(new_node(
                NodeType::FOR,
                Some(Box::new(t.clone())),
            )));
            breaks_push(node.clone());
            continues_push(node.clone());

            expect(TokenType::BRA, tokens);
            node.borrow_mut().cond = Some(expr(tokens));
            expect(TokenType::KET, tokens);
            node.borrow_mut().body = Some(stmt(tokens));

            breaks_pop();
            continues_pop();
            return node;
        }
        TokenType::DO => {
            let node = Rc::new(RefCell::new(new_node(
                NodeType::DO_WHILE,
                Some(Box::new(t.clone())),
            )));
            breaks_push(node.clone());
            continues_push(node.clone());

            node.borrow_mut().body = Some(stmt(tokens));
            expect(TokenType::WHILE, tokens);
            expect(TokenType::BRA, tokens);
            node.borrow_mut().cond = Some(expr(tokens));
            expect(TokenType::KET, tokens);
            expect(TokenType::SEMI_COLON, tokens);

            breaks_pop();
            continues_pop();
            return node;
        }
        TokenType::SWITCH => {
            let node = Rc::new(RefCell::new(new_node(
                NodeType::SWITCH,
                Some(Box::new(t.clone())),
            )));

            expect(TokenType::BRA, tokens);
            node.borrow_mut().cond = Some(expr(tokens));
            expect(TokenType::KET, tokens);

            breaks_push(node.clone());
            switches_push(node.clone());
            node.borrow_mut().body = Some(stmt(tokens));
            breaks_pop();
            switches_pop();
            return node;
        }
        TokenType::CASE => {
            if switches_len() == 0 {
                bad_token(t, "stray case".to_string());
            }
            let mut node = new_node(NodeType::CASE, Some(Box::new(t.clone())));
            node.val = const_expr(tokens);
            expect(TokenType::COLON, tokens);
            node.body = Some(stmt(tokens));

            let n = Rc::new(RefCell::new(node));
            let s = switches_last();
            s.borrow_mut().cases.push(n.clone());
            return n;
        }
        TokenType::BREAK => {
            if breaks_len() == 0 {
                bad_token(t, "stray break".to_string());
            }
            let mut node = new_node(NodeType::BREAK, Some(Box::new(t.clone())));
            let b = breaks_last();
            node.target = Some(b);
            return Rc::new(RefCell::new(node));
        }
        TokenType::CONTINUE => {
            if continues_len() == 0 {
                bad_token(t, "stray continue".to_string());
            }
            let mut node = new_node(NodeType::CONTINUE, Some(Box::new(t.clone())));

            // 9cc use the last node in 'breaks', not 'continues'.
            let c = continues_last();
            node.target = Some(c);

            return Rc::new(RefCell::new(node));
        }
        TokenType::RETURN => {
            let mut node = new_node(NodeType::RETURN, Some(Box::new(t.clone())));
            node.expr = Some(expr(tokens));
            expect(TokenType::SEMI_COLON, tokens);
            return Rc::new(RefCell::new(node));
        }
        TokenType::C_BRA => {
            return compound_stmt(tokens);
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

pub fn compound_stmt(tokens: &Vec<Token>) -> Rc<RefCell<Node>> {
    let t = &tokens[pos()];
    let mut node = new_node(NodeType::COMP_STMT, Some(Box::new(t.clone())));

    env_push();
    while !consume(TokenType::C_KET, tokens) {
        node.stmts.push(stmt(tokens));
    }
    env_pop();

    return Rc::new(RefCell::new(node));
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
        // initialize here for 9cc compatibility.
        init_lvars();

        let mut params = Vec::new();
        while !consume(TokenType::KET, tokens) {
            if params.len() > 0 {
                expect(TokenType::COMMA, tokens);
            }
            params.push(param_declaration(tokens));
        }

        let t = &tokens[pos()];
        let node = Rc::new(RefCell::new(new_node(
            NodeType::DECL,
            Some(Box::new(t.clone())),
        )));

        init_breaks();
        init_continues();
        init_switches();

        node.borrow_mut().name = name.clone();
        node.borrow_mut().params = params;

        let mut node_ty = alloc_type();
        node_ty.ty = CType::FUNC;
        node_ty.returning = Some(Box::new(ty));
        node.borrow_mut().ty = Rc::new(RefCell::new(node_ty));

        let ty = node.borrow().ty.clone();
        add_lvar(ty.borrow().clone(), name.clone());

        if consume(TokenType::SEMI_COLON, tokens) {
            return;
        }

        node.borrow_mut().op = NodeType::FUNC;
        let t = &tokens[pos()];
        expect(TokenType::C_BRA, tokens);
        if is_typedef {
            bad_token(t, format!("typedef has function definition"));
        }
        node.borrow_mut().body = Some(compound_stmt(tokens));

        prog_funcs_push(Rc::new(RefCell::new(Function {
            name: name,
            node: node,
            lvars: lvars(),
            bbs: Vec::new(),
        })));
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
    add_gvar(ty.clone(), name, None, is_extern);
}

fn is_eof(tokens: &Vec<Token>) -> bool {
    let t = &tokens[pos()];
    return t.ty == TokenType::EOF;
}

pub fn parse(tokens: &Vec<Token>) -> Program {
    while !is_eof(tokens) {
        toplevel(tokens);
    }

    return PROGRAM.with(|p| p.borrow().clone());
}
