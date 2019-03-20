// Atomic unit in the grammar is called "token".
// For example, `123`, `"abc"` and `while` are tokens.
// The tokenizer splits an input string into tokens.
// Spaces and comments are removed by the tokenizer.

#![allow(non_camel_case_types)]

use std::cmp;
use std::sync::Mutex;
use std::collections::HashMap;

lazy_static! {
    static ref SYMBOLS: Mutex<Vec<Symbol>> = Mutex::new(Vec::new());
    static ref ESCAPED: Mutex<HashMap<char, char>> = Mutex::new(HashMap::new());
}

fn init_symbols() {
    let mut symbols = SYMBOLS.lock().unwrap();

    symbols.push(Symbol { name: "_Alignof", ty: TokenType::ALIGNOF });
    symbols.push(Symbol { name: "char", ty: TokenType::CHAR });
    symbols.push(Symbol { name: "do", ty: TokenType::DO });
    symbols.push(Symbol { name: "else", ty: TokenType::ELSE });
    symbols.push(Symbol { name: "extern", ty: TokenType::EXTERN });
    symbols.push(Symbol { name: "for", ty: TokenType::FOR });
    symbols.push(Symbol { name: "if", ty: TokenType::IF });
    symbols.push(Symbol { name: "int", ty: TokenType::INT });
    symbols.push(Symbol { name: "return", ty: TokenType::RETURN });
    symbols.push(Symbol { name: "sizeof", ty: TokenType::SIZEOF });
    symbols.push(Symbol { name: "struct", ty: TokenType::STRUCT });
    symbols.push(Symbol { name: "typedef", ty: TokenType::TYPEDEF });
    symbols.push(Symbol { name: "void", ty: TokenType::VOID });
    symbols.push(Symbol { name: "while", ty: TokenType::WHILE });
    symbols.push(Symbol { name: "!=", ty: TokenType::NE });
    symbols.push(Symbol { name: "&&", ty: TokenType::LOGAND });
    symbols.push(Symbol { name: "->", ty: TokenType::ARROW });
    symbols.push(Symbol { name: "==", ty: TokenType::EQ });
    symbols.push(Symbol { name: "||", ty: TokenType::LOGOR });
}

fn init_escaped() {
    let mut escaped = ESCAPED.lock().unwrap();

    escaped.insert('a', char::from(7));  // \a
    escaped.insert('b', char::from(8));  // \b
    escaped.insert('f', char::from(12)); // \f
    escaped.insert('n', char::from(10)); // \n
    escaped.insert('r', char::from(13)); // \r
    escaped.insert('t', char::from(9));  // \t
    escaped.insert('v', char::from(11)); // \v
    escaped.insert('e', char::from(27)); // \033
    escaped.insert('E', char::from(27)); // \033
}

fn escaped(c: char) -> Option<char> {
    let mut ret = None;
    match ESCAPED.lock() {
        Ok(escaped) => {
            match escaped.get(&c) {
                Some(c) => {
                    ret = Some(*c);
                }
                None => { }
            }
        }
        Err(_) => { }
    }
    return ret;
}

struct Symbol {
    name: &'static str,
    ty: TokenType,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum TokenType {
    ADD,        // +
    SUB,        // -
    MUL,        // *
    DIV,        // /
    EQL,        // =
    LT,         // <
    GT,         // >
    OR,         // |
    BRA,        // (
    KET,        // )
    C_BRA,      // {
    C_KET,      // }
    S_BRA,      // [
    S_KET,      // ]
    AMP,        // &
    EXCLAM,     // !
    QUEST,      // ?
    DOT,        // .
    COMMA,      // ,
    COLON,      // :
    SEMI_COLON, // ;
    NUM,        // Number literal
    STR,        // String literal
    IDENT,      // Identifier
    ARROW,      // "->"
    EXTERN,     // "extern"
    TYPEDEF,    // "typedef"
    INT,        // "int"
    CHAR,       // "char"
    VOID,       // "void"
    STRUCT,     // "struct"
    IF,         // "if"
    ELSE,       // "else"
    FOR,        // "for"
    DO,         // "do"
    WHILE,      // "while"
    EQ,         // ==
    NE,         // !=
    LOGOR,      // ||
    LOGAND,     // &&
    RETURN,     // "return"
    SIZEOF,     // "sizeof"
    ALIGNOF,    // "_Alignof"
    EOF,        // End marker
}

#[derive(Debug)]
pub struct Token {
    pub ty: TokenType, // Token type
    pub val: i32,      // Nuber literal
    pub name: String,  // Identifier
    pub input: String, // Token string (for error reporting)

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

struct CharInfo {
    chr: char,
    len: usize,
}

fn read_char(p: &String, idx: usize) -> CharInfo {
    if p.len() <= idx {
        panic!("premature end of input");
    }

    let char_bytes = p.as_bytes();
    let mut len = 0;
    let mut c = char::from(char_bytes[idx]);
    if c != '\\' {
        len += 1;
    } else {
        len += 1;
        if p.len() <= idx + len {
            panic!("premature end of input");
        }
        c = char::from(char_bytes[idx+len]);
        match escaped(c) {
            Some(esc) => {
                c = esc;
            }
            None => { }
        }
        len += 1;
    }
    
    if char::from(char_bytes[idx+len]) != '\'' {
        panic!("unclosed character literal");
    }
    len += 1;
    return CharInfo {
        chr: c,
        len: len,
    };
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
        if c == '\0' {
            panic!("PREMATURE end of input");
        }
        match escaped(c) {
            Some(esc) => ret.push(esc),
            None => ret.push(c),
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
    init_escaped();

    let mut tokens = Vec::new();
    let char_bytes = p.as_bytes();
    let mut idx = 0;
    'outer: while idx < char_bytes.len() {
        let c: char = char::from(char_bytes[idx]);
        if c.is_whitespace() {
            idx += 1;
            continue;
        }

        // Line comment
        if c == '/' && char::from(char_bytes[idx+1]) == '/' {
            while char::from(char_bytes[idx]) != '\n' {
                idx += 1;
            }
            continue;
        }

        if c == '/' && char::from(char_bytes[idx+1]) == '*' {
            idx += 2;
            loop {
                if p.len() <= idx {
                    panic!("premature end of input");
                }
                let c1 = char::from(char_bytes[idx]);
                let c2 = char::from(char_bytes[idx+1]);
                if c1 == '*' && c2 == '/' {
                    idx += 2;
                    break;
                } else {
                    idx += 1;
                }
            }
            continue;
        }

        // Character literal
        if c == '\'' {
            let mut t = new_token(TokenType::NUM);
            idx += 1;
            let info = read_char(p, idx);
            t.val = u32::from(info.chr) as i32;

            tokens.push(t);
            idx += info.len;
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
        if "+-*/;=(),{}<>[]&.!?:|".contains(c) {
            let ty = match c {
                '+' => TokenType::ADD,
                '-' => TokenType::SUB,
                '*' => TokenType::MUL,
                '/' => TokenType::DIV,
                ':' => TokenType::COLON,
                ';' => TokenType::SEMI_COLON,
                '=' => TokenType::EQL,
                '<' => TokenType::LT,
                '>' => TokenType::GT,
                '|' => TokenType::OR,
                ',' => TokenType::COMMA,
                '(' => TokenType::BRA,
                ')' => TokenType::KET,
                '{' => TokenType::C_BRA,
                '}' => TokenType::C_KET,
                '[' => TokenType::S_BRA,
                ']' => TokenType::S_KET,
                '&' => TokenType::AMP,
                '!' => TokenType::EXCLAM,
                '?' => TokenType::QUEST,
                '.' => TokenType::DOT,
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

