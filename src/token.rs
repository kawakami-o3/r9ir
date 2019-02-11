// tokenizer
#![allow(non_camel_case_types)]

use std::collections::HashMap;
use std::sync::Mutex;

lazy_static! {
    pub static ref KEYWORDS: Mutex<HashMap<String, TokenType>> = Mutex::new(HashMap::new());
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum TokenType {
    NUM,
    ADD,
    SUB,
    MUL,
    DIV,
    RETURN,
    SEMI_COLON,
    EOF,
}

#[derive(Debug)]
pub struct Token {
    pub ty: TokenType,
    pub val: usize,
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
        if c == '+' || c == '-' || c == '*' || c == '/' || c == ';' {
            let ty = match c {
                '+' => TokenType::ADD,
                '-' => TokenType::SUB,
                '*' => TokenType::MUL,
                '/' => TokenType::DIV,
                ';' => TokenType::SEMI_COLON,
                _ => panic!(),
            };
            let tok = Token {
                ty: ty,
                val: 0,
                input: format!("{}", c),
            };

            tokens.push(tok);

            idx += 1;
            continue;
        }

        // Keyword
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
                Some(ty) => ty,
                None => {
                    panic!("unknown identifier: {}", s);
                }
            };

            let tok = Token {
                ty: *ty,
                val: 0,
                input: s,
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
                val: usize::from_str_radix(&s, 10).unwrap(),
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
        input: String::new(),
    };
    tokens.push(tok);

    return tokens;
}

pub fn tokenize(p: &String) -> Vec<Token> {
    match KEYWORDS.lock() {
        Ok(mut keywords) => {
            keywords.insert(String::from("return"), TokenType::RETURN);
        }
        _ => {}
    }

    return scan(p);
}
