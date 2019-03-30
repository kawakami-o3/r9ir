// C preprocessor

use crate::token::*;
use std::cell::RefCell;
use std::collections::HashMap;

thread_local! {
    static MACROS: RefCell<HashMap<String, Macro>> = RefCell::new(HashMap::new());

    static CTX: RefCell<Context> = RefCell::new(Context::new());
}

fn set_ctx(ctx: Context) {
    CTX.with(|c| {
        *c.borrow_mut() = ctx;
    })
}

fn get_ctx() -> Option<Context> {
    CTX.with(|c| {
        Some(c.borrow().clone())
    })
}

fn ctx_pop() {
    CTX.with(|c| {
        let next = c.borrow().next.clone();
        if next.is_some() {
            *c.borrow_mut() = *next.unwrap();
        }
    })
}

fn ctx_output() -> Vec<Token> {
    CTX.with(|c| {
        return c.borrow().output.clone();
    })
}

fn macros_get(key: &String) -> Option<Macro> {
    MACROS.with(|m| {
        match m.borrow().get(key) {
            Some(v) => Some(v.clone()),
            None => None,
        }
    })
}

fn macros_put(key: String, value: Macro) {
    MACROS.with(|m| {
        m.borrow_mut().insert(key, value);
    })
}

#[derive(Clone, Debug)]
struct Context {
    input: Vec<Token>,
    output: Vec<Token>,
    pos: usize,
    next: Option<Box<Context>>,
}

impl Context {
    fn new() -> Context {
        Context {
            input: Vec::new(),
            output: Vec::new(),
            pos: 0,
            next: None,
        }
    }
}

fn new_ctx(next: Option<Context>, input: Vec<Token>) -> Context {
    let mut ctx = Context::new();
    ctx.input = input;
    if next.is_none() {
        ctx.next = None;
    } else {
        ctx.next = Some(Box::new(next.unwrap()));
    }
    return ctx;
}

#[derive(Clone, Debug, PartialEq)]
enum MacroType {
    OBJLIKE,
    FUNCLIKE,
}

#[derive(Clone, Debug)]
struct Macro {
    ty: MacroType,
    tokens: Vec<Token>,
    params: Vec<String>,
}

fn new_macro(ty: MacroType, name: String) {
    let m = Macro {
        ty: ty,
        tokens: Vec::new(),
        params: Vec::new(),
    };
    macros_put(name, m);
}

fn append(v: &mut Vec<Token>) {
    CTX.with(|ctx| {
        ctx.borrow_mut().output.append(v);
    })
}

fn add(t: Token) {
    CTX.with(|ctx| {
        ctx.borrow_mut().output.push(t);
    })
}

fn next() -> Token {
    CTX.with(|ctx| {
        let pos = ctx.borrow().pos;
        assert!(pos < ctx.borrow().input.len());
        ctx.borrow_mut().pos += 1;
        return ctx.borrow().input[pos].clone();
    })
}

fn eof() -> bool {
    CTX.with(|c| {
        let ctx = c.borrow();
        return ctx.pos == ctx.input.len();
    })
}

fn get(ty: TokenType, msg: String) -> Token {
    let t = next();
    if t.ty != ty {
        bad_token(&t, msg);
    }
    return t;
}

fn ident(msg: String) -> String {
    let t = get(TokenType::IDENT, msg);
    return t.name.clone();
}

fn peek() -> Token {
    CTX.with(|c| {
        let ctx = c.borrow();
        return ctx.input[ctx.pos].clone();
    })
}

fn consume(ty: TokenType) -> bool {
    if peek().ty != ty {
        return false;
    }
    CTX.with(|c| {
        c.borrow_mut().pos += 1;
    });
    return true;
}

fn read_until_eol() -> Vec<Token> {
    let mut v = Vec::new();
    while !eof() {
        let t = next();
        if t.ty == TokenType::NEW_LINE {
            break;
        }
        v.push(t.clone());
    }
    return v;
}

fn new_int(val: i32) -> Token {
    let mut t = new_token(TokenType::NUM, 0);
    t.val = val;
    return t;
}

fn new_param(val: i32) -> Token {
    let mut t = new_token(TokenType::PARAM, 0);
    t.val = val;
    return t;
}

fn is_ident(t: &Token, s: &str) -> bool {
    return t.ty == TokenType::IDENT && t.name == s;
}

// Replaces macro parameter tokens with TokenType::PARAM tokens.
fn replace_macro_params(m: &mut Macro) {
    let params = m.params.clone();
    let mut tokens = m.tokens.clone();

    let mut map = HashMap::new();
    for i in 0..params.len() {
        let name = params[i].clone();
        map.insert(name, i as i32);
    }

    for i in 0..tokens.len() {
        let t = tokens[i].clone();
        if t.ty != TokenType::IDENT {
            continue;
        }
        let n = match map.get(&t.name) {
            Some(i) => *i,
            None => {
                continue;
            }
        };
        tokens.remove(i);
        tokens.insert(i, new_param(n));
    }
    m.tokens = tokens;
}

// Process '#' followed by a macro parameter.
fn replace_hash_ident(m: &mut Macro) {
    let tokens = m.tokens.clone();
    let mut v = Vec::new();
    let mut i = 0;
    while i < tokens.len()-1 {
        let t1 = tokens[i].clone();
        let mut t2 = tokens[i+1].clone();

        if t1.ty == TokenType::SHARP && t2.ty == TokenType::PARAM {
            t2.stringize = true;
            v.push(t2);
            i += 1;
        } else {
            v.push(t1);
        }
        i += 1;
    }

    if i == tokens.len() - 1 {
        v.push(tokens[i].clone());
    }
    m.tokens = v;
}

fn read_one_arg() -> Vec<Token> {
    let mut v = Vec::new();
    let start = peek();
    let mut level = 0;

    while !eof() {
        let t = peek();
        if level == 0 {
            if t.ty == TokenType::KET || t.ty == TokenType::COMMA {
                return v;
            }
        }

        next();
        if t.ty == TokenType::BRA {
            level += 1;
        } else if t.ty == TokenType::KET {
            level -= 1;
        }
        v.push(t);
    }
    bad_token(&start, "unclosed macro argument".to_string());
    panic!();
}

fn read_args() -> HashMap<usize, Vec<Token>> {
    let mut v = HashMap::new();
    if consume(TokenType::KET) {
        return v;
    }
    let i = v.len();
    v.insert(i, read_one_arg());
    while !consume(TokenType::KET) {
        get(TokenType::COMMA, "comma expected".to_string());
        let i = v.len();
        v.insert(i, read_one_arg());
    }
    return v;
}

fn stringize(tokens: Vec<Token>) -> Token {
    let mut sb = String::new();

    for i in 0..tokens.len() {
        let t = &tokens[i];
        if i > 0 {
            sb.push(' ');
        }
        sb.push_str(tokstr(&t));
    }
    sb.push('\0');

    let mut t = new_token(TokenType::STR, 0);
    t.len = sb.len();
    t.str_cnt = sb;
    return t;
}

fn add_special_macro(t: & Token) -> bool {
    if is_ident(t, "__LINE__") {
        add(new_int(get_line_number(t)));
        return true;
    }
    return false;
}

fn apply_objlike(m: &mut Macro, _start: & Token) {
    for t in m.tokens.iter() {
        if add_special_macro(t) {
            continue;
        }
        add(t.clone());
    }
}

fn apply_functionlike(m: &mut Macro, start: & Token) {
    get(TokenType::BRA, "comma expected".to_string());
    let args = read_args();
    if m.params.len() != args.len() {
        bad_token(&start, format!("number of parameter does not match ({} != {})", m.params.len(), args.len()).to_string());
        panic!();
    }

    for i in 0..m.tokens.len() {
        let t = &m.tokens[i];
        if add_special_macro(t) {
            continue;
        }

        if t.ty == TokenType::PARAM {
            if t.stringize {
                let j = t.val as usize;
                add(stringize(args.get(&j).unwrap().clone()));
            } else {
                let j = t.val as usize;
                append(&mut args.get(&j).unwrap().clone());
            }
            continue;
        }
        add(t.clone());
    }
}

fn apply(m: &mut Macro, start: & Token) {
    if m.ty == MacroType::OBJLIKE {
        apply_objlike(m, start);
    } else {
        apply_functionlike(m, start);
    }
}


fn define_funclike(name: String) {
    new_macro(MacroType::FUNCLIKE, name.clone());
    let mut m = macros_get(&name).unwrap();
    m.params.push(ident("parameter name expected".to_string()));
    while !consume(TokenType::KET) {
        get(TokenType::COMMA, "comma expected".to_string());
        m.params.push(ident("parameter name expected".to_string()));
    }
    m.tokens = read_until_eol();

    replace_macro_params(&mut m);
    replace_hash_ident(&mut m);

    macros_put(name, m);
}

fn define_objlike(name: String) {
    new_macro(MacroType::OBJLIKE, name.clone());
    let mut m = macros_get(&name).unwrap();
    m.tokens = read_until_eol();
    macros_put(name, m);
}

fn define() {
    let name = ident("macro name expected".to_string());
    if consume(TokenType::BRA) {
        return define_funclike(name);
    }
    return define_objlike(name);
}

fn include() {
    let t = get(TokenType::STR, "string expected".to_string());
    let path = t.str_cnt;
    get(TokenType::NEW_LINE, "newline expected".to_string());
    return append(&mut tokenize(path, false));
}

pub fn preprocess(tokens: Vec<Token>) -> Vec<Token> {
    set_ctx(new_ctx(get_ctx(), tokens.clone()));

    while !eof() {
        let mut t = next();

        if t.ty == TokenType::IDENT {
            match macros_get(&t.name) {
                Some(macro_token) => {
                    apply(&mut macro_token.clone(), &t);
                }
                None => {
                    add(t.clone());
                }
            }
            continue;
        }

        if t.ty != TokenType::SHARP {
            add(t.clone());
            continue;
        }

        t = get(TokenType::IDENT, "identifier expected".to_string());

        if t.name == "define" {
            define();
        } else if t.name == "include" {
            include();
        } else {
            bad_token(&t, "unknown directive".to_string());
        }
    }

    let v = ctx_output();
    ctx_pop();
    return v;
}
