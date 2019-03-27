// C preprocessor

use crate::token::*;
use std::cell::RefCell;
use std::collections::HashMap;

thread_local! {
    static MACROS: RefCell<HashMap<String, Vec<Token>>> = RefCell::new(HashMap::new());

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

fn macros_get(key: &String) -> Option<Vec<Token>> {
    MACROS.with(|m| {
        match m.borrow().get(key) {
            Some(v) => Some(v.clone()),
            None => None,
        }
    })
}

fn macros_put(key: String, value: Vec<Token>) {
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

fn define() {
    let mut t = get(TokenType::IDENT, "macro name expected".to_string());
    let name = t.name;

    let mut v = Vec::new();
    while !eof() {
        t = next();
        if t.ty == TokenType::NEW_LINE {
            break;
        }
        v.push(t.clone());
    }
    macros_put(name, v);
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
                    append(&mut macro_token.clone());
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
