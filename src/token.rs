// tokenizer
#![allow(non_camel_case_types)]

use std::cmp;
use std::sync::Mutex;

lazy_static! {
    static ref SYMBOLS: Mutex<Vec<Symbol>> = Mutex::new(Vec::new());
}

fn init_symbols() {
    let mut symbols = SYMBOLS.lock().unwrap();

    symbols.push(Symbol { name: "else", ty: TokenType::ELSE });
    symbols.push(Symbol { name: "for", ty: TokenType::FOR });
    symbols.push(Symbol { name: "if", ty: TokenType::IF });
    symbols.push(Symbol { name: "int", ty: TokenType::INT });
    symbols.push(Symbol { name: "return", ty: TokenType::RETURN });
    symbols.push(Symbol { name: "&&", ty: TokenType::LOGAND });
    symbols.push(Symbol { name: "||", ty: TokenType::LOGOR });
}

struct Symbol {
    name: &'static str,
    ty: TokenType,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum TokenType {
    NUM,
    ADD,
    SUB,
    MUL,
    DIV,
    EQ,
    LT,
    GT,
    BRA,
    KET,
    C_BRA,
    C_KET,
    IDENT,
    INT,
    IF,
    FOR,
    ELSE,
    LOGOR,
    LOGAND,
    RETURN,
    COMMA,
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

pub fn tokenize(p: &String) -> Vec<Token> {
    init_symbols();
    let mut tokens = Vec::new();
    let char_bytes = p.as_bytes();
    let mut idx = 0;
    'outer: while idx < char_bytes.len() {
        let c: char = char::from(char_bytes[idx]);
        if c.is_whitespace() {
            idx += 1;
            continue;
        }
        if "+-*/;=(),{}<>".contains(c) {
            let ty = match c {
                '+' => TokenType::ADD,
                '-' => TokenType::SUB,
                '*' => TokenType::MUL,
                '/' => TokenType::DIV,
                ';' => TokenType::SEMI_COLON,
                '=' => TokenType::EQ,
                '<' => TokenType::LT,
                '>' => TokenType::GT,
                '(' => TokenType::BRA,
                ')' => TokenType::KET,
                '{' => TokenType::C_BRA,
                '}' => TokenType::C_KET,
                ',' => TokenType::COMMA,
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

        // Multi-letter token
        match SYMBOLS.lock() {
            Ok(symbols) => {
                for s in symbols.iter() {
                    let bs = &char_bytes[idx..cmp::min(idx+s.name.len(), char_bytes.len())];
                    if s.name.as_bytes() != bs {
                        continue;
                    }

                    tokens.push(Token{
                        ty: s.ty,
                        val: 0,
                        name: String::from(s.name),
                        input: String::from(s.name),
                    });

                    idx += s.name.len();
                    continue 'outer;
                }
            }
            Err(_) => {
                panic!();
            }
        }

        // Identifier
        if c.is_alphabetic() || c == '_' {
            let mut s = String::new();
            s.push(c);
            idx += 1;
            while idx < char_bytes.len() {
                let d = char::from(char_bytes[idx]);
                if d.is_alphabetic() || d.is_digit(10) || d == '_' {
                    s.push(d);
                    idx += 1;
                } else {
                    break;
                }
            }

            let tok = Token {
                ty: TokenType::IDENT,
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

