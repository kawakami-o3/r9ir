// tokenizer
#![allow(non_camel_case_types)]

use std::cmp;
use std::sync::Mutex;

lazy_static! {
    static ref SYMBOLS: Mutex<Vec<Symbol>> = Mutex::new(Vec::new());
}

fn init_symbols() {
    let mut symbols = SYMBOLS.lock().unwrap();

    symbols.push(Symbol { name: "char", ty: TokenType::CHAR });
    symbols.push(Symbol { name: "do", ty: TokenType::DO });
    symbols.push(Symbol { name: "else", ty: TokenType::ELSE });
    symbols.push(Symbol { name: "extern", ty: TokenType::EXTERN });
    symbols.push(Symbol { name: "for", ty: TokenType::FOR });
    symbols.push(Symbol { name: "if", ty: TokenType::IF });
    symbols.push(Symbol { name: "int", ty: TokenType::INT });
    symbols.push(Symbol { name: "return", ty: TokenType::RETURN });
    symbols.push(Symbol { name: "sizeof", ty: TokenType::SIZEOF });
    symbols.push(Symbol { name: "while", ty: TokenType::WHILE });
    symbols.push(Symbol { name: "&&", ty: TokenType::LOGAND });
    symbols.push(Symbol { name: "||", ty: TokenType::LOGOR });
    symbols.push(Symbol { name: "==", ty: TokenType::EQ });
    symbols.push(Symbol { name: "!=", ty: TokenType::NE });
}

struct Symbol {
    name: &'static str,
    ty: TokenType,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum TokenType {
    ADD,
    SUB,
    MUL,
    DIV,
    EQL,
    LT,
    GT,
    BRA,
    KET,
    C_BRA,
    C_KET,
    S_BRA,
    S_KET,
    AMP,
    NUM,
    STR,
    IDENT,
    EXTERN,
    INT,
    CHAR,
    IF,
    FOR,
    DO,    // "do"
    WHILE, // "while"
    EQ,    // ==
    NE,    // !=
    ELSE,
    LOGOR,
    LOGAND,
    RETURN,
    SIZEOF,
    EOF,
    COMMA,
    SEMI_COLON,
}

#[derive(Debug)]
pub struct Token {
    pub ty: TokenType,
    pub val: i32,
    pub name: String,
    pub input: String,

    // String literal
    pub str_cnt: String,
    pub len: usize,
}

fn new_token(ty: TokenType) -> Token {
    Token{
        ty: ty,
        val: 0,
        name: String::new(),
        input: String::new(),
        str_cnt: String::new(),
        len: 0,
    }
}

struct StrInfo {
    cnt: String,
    len: usize,
}

fn read_string(p: &String, idx: usize) -> StrInfo {
    let mut len = 0;
    let mut ret = String::new();

    let char_bytes = p.as_bytes();
    while (idx+len) < p.len() && char::from(char_bytes[idx+len]) != '"' {
        let mut c = char::from(char_bytes[idx+len]);

        if c != '\\' {
            ret.push(c);
            len += 1;
            continue;
        }

        len += 1;
        c = char::from(char_bytes[idx+len]);
        match c {
            'a' => ret.push(char::from(7)), // \a
            'b' => ret.push(char::from(8)), // \b
            'f' => ret.push(char::from(12)), // \f
            'n' => ret.push('\n'), // 10
            'r' => ret.push('\r'), // 13
            't' => ret.push('\t'), // 9
            'v' => ret.push(char::from(11)), // \v
            '\0' => panic!("PREMATURE end of input"),
            _ => ret.push(c),
        }
        len += 1;
    }
    if (idx+len) >= p.len() {
        panic!("premature end of input: {}", ret);
    }
    //ret.push(&format!("\\{:03o}", u32::from(c)));
    ret.push(char::from(0));
    return StrInfo{
        cnt: ret,
        len: len,
    };
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

        // String literal
        if c == '"' {
            let mut t = new_token(TokenType::STR);
            idx += 1;

            let info = read_string(p, idx);
            let s = info.cnt;
            let len = info.len;
            t.str_cnt = s;
            t.len = len;
            tokens.push(t);
            idx += len + 1;
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
                        str_cnt: String::new(),
                        len: 0,
                    });

                    idx += s.name.len();
                    continue 'outer;
                }
            }
            Err(_) => {
                panic!();
            }
        }

        // Single-letter token
        if "+-*/;=(),{}<>[]&".contains(c) {
            let ty = match c {
                '+' => TokenType::ADD,
                '-' => TokenType::SUB,
                '*' => TokenType::MUL,
                '/' => TokenType::DIV,
                ';' => TokenType::SEMI_COLON,
                '=' => TokenType::EQL,
                '<' => TokenType::LT,
                '>' => TokenType::GT,
                ',' => TokenType::COMMA,
                '(' => TokenType::BRA,
                ')' => TokenType::KET,
                '{' => TokenType::C_BRA,
                '}' => TokenType::C_KET,
                '[' => TokenType::S_BRA,
                ']' => TokenType::S_KET,
                '&' => TokenType::AMP,
                _ => panic!(format!("unknown {}", c)),
            };
            let tok = Token {
                ty: ty,
                val: 0,
                name: String::new(),
                input: format!("{}", c),
                str_cnt: String::new(),
                len: 0,
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
                str_cnt: String::new(),
                len: 0,
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
                str_cnt: String::new(),
                len: 0,
            };

            tokens.push(tok);
            continue;
        }

        panic!("cannot tokenize: {}", c);
    }

    let tok = Token {
        ty: TokenType::EOF,
        val: 0,
        name: String::new(),
        input: String::new(),
        str_cnt: String::new(),
        len: 0,
    };
    tokens.push(tok);

    return tokens;
}

