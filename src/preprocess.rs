// C preprocessor

use crate::token::*;
use std::cell::RefCell;
use std::collections::HashMap;

thread_local! {
    static MACROS: RefCell<HashMap<String, Macro>> = RefCell::new(HashMap::new());

    static ENV: RefCell<Env> = RefCell::new(Env::new());
}

fn set_env(env: Env) {
    ENV.with(|c| {
        *c.borrow_mut() = env;
    })
}

fn get_env() -> Option<Env> {
    ENV.with(|c| {
        Some(c.borrow().clone())
    })
}

fn env_pop() {
    ENV.with(|c| {
        let prev = c.borrow().prev.clone();
        if prev.is_some() {
            *c.borrow_mut() = *prev.unwrap();
        }
    })
}

fn env_output() -> Vec<Token> {
    ENV.with(|c| {
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
struct Env {
    input: Vec<Token>,
    output: Vec<Token>,
    pos: usize,
    prev: Option<Box<Env>>,
}

impl Env {
    fn new() -> Env {
        Env {
            input: Vec::new(),
            output: Vec::new(),
            pos: 0,
            prev: None,
        }
    }
}

fn new_env(prev: Option<Env>, input: Vec<Token>) -> Env {
    let mut ctx = Env::new();
    ctx.input = input;
    if prev.is_none() {
        ctx.prev = None;
    } else {
        ctx.prev = Some(Box::new(prev.unwrap()));
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
    ENV.with(|env| {
        env.borrow_mut().output.append(v);
    })
}

fn emit(t: Token) {
    ENV.with(|env| {
        env.borrow_mut().output.push(t);
    })
}

fn next() -> Token {
    ENV.with(|env| {
        let pos = env.borrow().pos;
        assert!(pos < env.borrow().input.len());
        env.borrow_mut().pos += 1;
        return env.borrow().input[pos].clone();
    })
}

fn is_eof() -> bool {
    ENV.with(|c| {
        let env = c.borrow();
        return env.pos == env.input.len();
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
    ENV.with(|c| {
        let env = c.borrow();
        return env.input[env.pos].clone();
    })
}

fn consume(ty: TokenType) -> bool {
    if peek().ty != ty {
        return false;
    }
    ENV.with(|c| {
        c.borrow_mut().pos += 1;
    });
    return true;
}

fn read_until_eol() -> Vec<Token> {
    let mut v = Vec::new();
    while !is_eof() {
        let t = next();
        if t.ty == TokenType::NEW_LINE {
            break;
        }
        v.push(t.clone());
    }
    return v;
}

fn new_int(tmpl: & Token, val: i32) -> Token {
    let mut t = tmpl.clone();
    t.ty = TokenType::NUM;
    t.val = val;
    return t;
}

fn new_string(tmpl: & Token, str_cnt: String, len: usize) -> Token {
    let mut t = tmpl.clone();
    t.ty = TokenType::STR;
    t.str_cnt = str_cnt;
    t.len = len;
    return t;
}

fn new_param(tmpl: & Token, val: i32) -> Token {
    let mut t = tmpl.clone();
    t.ty = TokenType::PARAM;
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
        let t = &tokens[i].clone();
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
        tokens.insert(i, new_param(t, n));
    }
    m.tokens = tokens;
}

// Replaces '#' followed by a macro parameter.
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

    while !is_eof() {
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

fn emit_special_macro(t: & Token) -> bool {
    if is_ident(t, "__LINE__") {
        emit(new_int(t, get_line_number(t)));
        return true;
    }
    return false;
}

fn apply_objlike(m: &mut Macro, _start: & Token) {
    for t in m.tokens.iter() {
        if emit_special_macro(t) {
            continue;
        }
        emit(t.clone());
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
        if emit_special_macro(t) {
            continue;
        }

        if t.ty == TokenType::PARAM {
            if t.stringize {
                let j = t.val as usize;
                let s = stringize(args.get(&j).unwrap().clone());
                emit(new_string(t, s.clone(), s.len()));
            } else {
                let j = t.val as usize;
                append(&mut args.get(&j).unwrap().clone());
            }
            continue;
        }
        emit(t.clone());
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
    while !consume(TokenType::KET) {
        if m.params.len() > 0 {
            get(TokenType::COMMA, ", expected".to_string());
        }
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
    set_env(new_env(get_env(), tokens.clone()));

    while !is_eof() {
        let mut t = next();

        if t.ty == TokenType::IDENT {
            match macros_get(&t.name) {
                Some(macro_token) => {
                    apply(&mut macro_token.clone(), &t);
                }
                None => {
                    emit(t.clone());
                }
            }
            continue;
        }

        if t.ty != TokenType::SHARP {
            emit(t.clone());
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

    let v = env_output();
    env_pop();
    return v;
}
