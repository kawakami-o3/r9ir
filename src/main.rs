// main

#[macro_use]
extern crate lazy_static;

use std::env;
use std::sync::Mutex;

lazy_static! {
    static ref TOKENS: Mutex<Vec<Token>> = Mutex::new(Vec::new());
}

#[derive(PartialEq)]
enum TokenType {
    NUM,
    OP,
    EOF,
}

struct Token {
    ty: TokenType,
    val: i32,
    input: String,
}

fn tokenize(p: &String) {
    let mut tokens = Vec::new();
    let mut t = String::new();
    for c in p.chars() {
        if c.is_whitespace() {
            continue;
        }
        if c == '+' || c == '-' {
            tokens.push(t.clone());
            t = String::new();
            tokens.push(format!("{}", c));
        }
        if c.is_digit(10) {
            t.push(c);
            continue;
        }
    }
    tokens.push(t);

    for s in tokens {
        if s == "+" || s == "-" {
            let tok = Token {
                ty: TokenType::OP,
                val: 0,
                input: s,
            };

            TOKENS.lock().unwrap().push(tok);
        } else {
            let tok = Token {
                ty: TokenType::NUM,
                val: i32::from_str_radix(&s, 10).unwrap(),
                input: s,
            };

            TOKENS.lock().unwrap().push(tok);
        }
    }

    let tok = Token {
        ty: TokenType::EOF,
        val: 0,
        input: String::new(),
    };
    TOKENS.lock().unwrap().push(tok);
}

fn fail(i: usize) {
    match TOKENS.lock().unwrap().get(i) {
        Some(t) => {
            panic!("unextected token: {}", t.input);
        }
        None => {
            panic!("unextected token: none");
        }
    }
}

fn main() {
    let argv: Vec<String> = env::args().collect();
    if argv.len() != 2 {
        println!("Usage: 9cc <code>");
        return;
    }
    tokenize(&argv[1]);

    println!(".intel_syntax noprefix");
    println!(".global main");
    println!("main:");

    let tokens = TOKENS.lock().unwrap();
    if tokens.get(0).unwrap().ty != TokenType::NUM {
        fail(0);
    }

    println!("  mov rax, {}", tokens.get(0).unwrap().val);

    let mut i = 1;
    let mut tok = tokens.get(i).unwrap();
    while tok.ty != TokenType::EOF {
        if tok.ty == TokenType::OP && tok.input == "+" {
            i += 1;
            tok = tokens.get(i).unwrap();
            if tok.ty != TokenType::NUM {
                fail(i);
            }
            println!("  add rax, {}", tok.val);
            i += 1;
            tok = tokens.get(i).unwrap();
            continue;
        }

        if tok.ty == TokenType::OP && tok.input == "-" {
            i += 1;
            tok = tokens.get(i).unwrap();
            if tok.ty != TokenType::NUM {
                fail(i);
            }
            println!("  sub rax, {}", tok.val);
            i += 1;
            tok = tokens.get(i).unwrap();
            continue;
        }

        fail(i);
    }

    println!("  ret");
    return;
}
