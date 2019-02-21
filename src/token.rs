// tokenizer
#![allow(non_camel_case_types)]

use std::collections::HashMap;
use std::sync::Mutex;

lazy_static! {
    static ref KEYWORDS: Mutex<HashMap<String, TokenType>> = Mutex::new(HashMap::new());
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum TokenType {
    NUM,
    ADD,
    SUB,
    MUL,
    DIV,
    EQ,
    BRA,
    KET,
    IDENT,
    IF,
    RETURN,
    SEMI_COLON,
    EOF,
}

#[derive(Debug)]
pub struct Token {
    pub ty: TokenType,
    pub val: i32,
    pub name: String,
    pub input: String,
}

fn scan(p: &String) -> Vec<Token> {
    let mut tokens = Vec::new();
    let char_bytes = p.as_bytes();
    let mut idx = 0;
    while idx < char_bytes.len() {
        let c: char = char::from(char_bytes[idx]);
        if c.is_whitespace() {
            idx += 1;
            continue;
        }
        if "+-*/;=()".contains(c) {
            let ty = match c {
                '+' => TokenType::ADD,
                '-' => TokenType::SUB,
                '*' => TokenType::MUL,
                '/' => TokenType::DIV,
                ';' => TokenType::SEMI_COLON,
                '=' => TokenType::EQ,
                '(' => TokenType::BRA,
                ')' => TokenType::KET,
                _ => panic!(format!("unknown {}", c)),
            };
            let tok = Token {
                ty: ty,
                val: 0,
                name: String::new(),
                input: format!("{}", c),
            };

            tokens.push(tok);

            idx += 1;
            continue;
        }

        // Identifier
        if c.is_alphabetic() || c == '_' {
            let mut s = String::new();
            s.push(c);
            idx += 1;
            while idx < char_bytes.len() {
                let d = char::from(char_bytes[idx]);
                if d.is_alphabetic() || d.is_digit(10) || c == '_' {
                    s.push(d);
                    idx += 1;
                } else {
                    break;
                }
            }

            let keywords = KEYWORDS.lock().unwrap();
            let ty = match keywords.get(&s) {
                Some(ty) => *ty,
                None => TokenType::IDENT,
            };

            let tok = Token {
                ty: ty,
                val: 0,
                name: s.clone(),
                input: s.clone(),
            };

            tokens.push(tok);
            continue;
        }

        // Number
        if c.is_digit(10) {
            let mut s = String::new();
            s.push(c);
            idx += 1;
            while idx < char_bytes.len() {
                let d = char::from(char_bytes[idx]);
                if d.is_digit(10) {
                    s.push(d);
                    idx += 1;
                } else {
                    break;
                }
            }
            let tok = Token {
                ty: TokenType::NUM,
                val: i32::from_str_radix(&s, 10).unwrap(),
                name: String::new(),
                input: s,
            };

            tokens.push(tok);
            continue;
        }

        panic!(format!("cannot tokenize: {}", c));
    }

    let tok = Token {
        ty: TokenType::EOF,
        val: 0,
        name: String::new(),
        input: String::new(),
    };
    tokens.push(tok);

    return tokens;
}

pub fn tokenize(p: &String) -> Vec<Token> {
    match KEYWORDS.lock() {
        Ok(mut keywords) => {
            keywords.insert(String::from("if"), TokenType::IF);
            keywords.insert(String::from("return"), TokenType::RETURN);
        }
        _ => {}
    }

    return scan(p);
}
