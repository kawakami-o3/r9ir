// Atomic unit in the grammar is called "token".
// For example, `123`, `"abc"` and `while` are tokens.
// The tokenizer splits an input string into tokens.
// Spaces and comments are removed by the tokenizer.

#![allow(non_camel_case_types)]

use crate::*;
use std::cmp;
use std::sync::Mutex;
use std::collections::HashMap;

lazy_static! {
    static ref SYMBOLS: Mutex<Vec<Symbol>> = Mutex::new(Vec::new());
    static ref ESCAPED: Mutex<HashMap<char, char>> = Mutex::new(HashMap::new());

    static ref INPUT_FILE: Mutex<String> = Mutex::new(String::new());
}

fn input_file() -> String {
    let input = INPUT_FILE.lock().unwrap();
    return input.clone();
}

fn set_input_file(s: String) {
    let mut input = INPUT_FILE.lock().unwrap();
    *input = s;
}

fn init_symbols() {
    let mut symbols = SYMBOLS.lock().unwrap();

    symbols.push(Symbol { name: "<<=", ty: TokenType::SHL_EQ });
    symbols.push(Symbol { name: ">>=", ty: TokenType::SHR_EQ });
    symbols.push(Symbol { name: "!=", ty: TokenType::NE });
    symbols.push(Symbol { name: "&&", ty: TokenType::LOGAND });
    symbols.push(Symbol { name: "++", ty: TokenType::INC });
    symbols.push(Symbol { name: "--", ty: TokenType::DEC });
    symbols.push(Symbol { name: "->", ty: TokenType::ARROW });
    symbols.push(Symbol { name: "<<", ty: TokenType::SHL });
    symbols.push(Symbol { name: "<=", ty: TokenType::LE });
    symbols.push(Symbol { name: "==", ty: TokenType::EQ });
    symbols.push(Symbol { name: ">=", ty: TokenType::GE });
    symbols.push(Symbol { name: ">>", ty: TokenType::SHR });
    symbols.push(Symbol { name: "||", ty: TokenType::LOGOR });
    symbols.push(Symbol { name: "*=", ty: TokenType::MUL_EQ });
    symbols.push(Symbol { name: "/=", ty: TokenType::DIV_EQ });
    symbols.push(Symbol { name: "%=", ty: TokenType::MOD_EQ });
    symbols.push(Symbol { name: "+=", ty: TokenType::ADD_EQ });
    symbols.push(Symbol { name: "-=", ty: TokenType::SUB_EQ });
    symbols.push(Symbol { name: "&=", ty: TokenType::BITAND_EQ });
    symbols.push(Symbol { name: "^=", ty: TokenType::XOR_EQ });
    symbols.push(Symbol { name: "|=", ty: TokenType::BITOR_EQ });
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
    HAT,        // ^
    TILDA,      // ~
    BRA,        // (
    KET,        // )
    C_BRA,      // {
    C_KET,      // }
    S_BRA,      // [
    S_KET,      // ]
    AMP,        // &
    MOD,        // %
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
    BREAK,      // "break"
    EQ,         // ==
    NE,         // !=
    LE,         // <=
    GE,         // >=
    LOGOR,      // ||
    LOGAND,     // &&
    SHL,        // <<
    SHR,        // >>
    INC,        // ++
    DEC,        // --
    MUL_EQ,     // *=
    DIV_EQ,     // /=
    MOD_EQ,     // %=
    ADD_EQ,     // +=
    SUB_EQ,     // -=
    SHL_EQ,     // <<=
    SHR_EQ,     // >>=
    BITAND_EQ,  // &=
    XOR_EQ,     // ^=
    BITOR_EQ,   // |=
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

    // String literal
    pub str_cnt: String,
    pub len: usize,
    
    // For error reporting
    pub start: usize,
}

fn new_token(ty: TokenType, idx: usize) -> Token {
    Token{
        ty: ty,
        val: 0,
        name: String::new(),
        //start: String::new(),
        start: idx,
        str_cnt: String::new(),
        len: 0,
    }
}

struct CharInfo {
    chr: char,
    len: usize,
}

fn print_line(t: & Token) {
    let mut line = 0;
    let mut col = 0;

    let mut target = String::new();
    let mut idx = 0;
    let input = input_file();
    let bytes = input.as_bytes();
    while idx < bytes.len() {
        let p = char::from(bytes[idx]);
        if p == '\n' {
            target = String::new();
            line+=1;
            col = 0;
            idx += 1;
            continue;
        }

        if idx != t.start {
            target.push(p);
            col += 1;
            idx += 1;
            continue;
        }

        eprintln!("error at {}:{}:{}", filename(), line+1, col+1);
        eprintln!();

        idx += 1;
        while char::from(bytes[idx]) != '\n' {
            target.push(char::from(bytes[idx]));
        }
        eprintln!("{}", target);

        for _i in 0..col {
            eprint!(" ");
        }
        eprintln!("^");
        eprintln!();
        return;
    }
}

pub fn bad_token(t: & Token, msg: String) {
    print_line(t);
    panic!(msg);
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
            panic!("premature end of input");
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

fn keyword_map() -> HashMap<String, TokenType> {
    let mut keywords = HashMap::new();
    keywords.insert("_Alignof".to_string(), TokenType::ALIGNOF);
    keywords.insert("break".to_string(), TokenType::BREAK);
    keywords.insert("char".to_string(), TokenType::CHAR);
    keywords.insert("do".to_string(), TokenType::DO);
    keywords.insert("else".to_string(), TokenType::ELSE);
    keywords.insert("extern".to_string(), TokenType::EXTERN);
    keywords.insert("for".to_string(), TokenType::FOR);
    keywords.insert("if".to_string(), TokenType::IF);
    keywords.insert("int".to_string(), TokenType::INT);
    keywords.insert("return".to_string(), TokenType::RETURN);
    keywords.insert("sizeof".to_string(), TokenType::SIZEOF);
    keywords.insert("struct".to_string(), TokenType::STRUCT);
    keywords.insert("typedef".to_string(), TokenType::TYPEDEF);
    keywords.insert("void".to_string(), TokenType::VOID);
    keywords.insert("while".to_string(), TokenType::WHILE);
    return keywords;
} 

pub fn tokenize(p: &String) -> Vec<Token> {
    init_symbols();
    init_escaped();
    set_input_file(p.clone());

    let keywords = keyword_map();
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

        // Block comment
        if &p[idx..idx+2] == "/*" {
            idx += 2;
            while idx < p.len() {
                if &p[idx..idx+2] != "*/" {
                    idx += 1;
                    continue;
                }
                idx += 2;
                continue 'outer;
            }
            panic!("unclosed comment");
        }

        // Character literal
        if c == '\'' {
            let mut t = new_token(TokenType::NUM, idx);
            idx += 1;
            let info = read_char(p, idx);
            t.val = u32::from(info.chr) as i32;

            tokens.push(t);
            idx += info.len;
            continue;
        }

        // String literal
        if c == '"' {
            let mut t = new_token(TokenType::STR, idx);
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

        // Multi-letter symbol
        match SYMBOLS.lock() {
            Ok(symbols) => {
                for s in symbols.iter() {
                    let bs = &char_bytes[idx..cmp::min(idx+s.name.len(), char_bytes.len())];
                    if s.name.as_bytes() != bs {
                        continue;
                    }

                    tokens.push(new_token(s.ty, idx));
                    idx += s.name.len();
                    continue 'outer;
                }
            }
            Err(_) => {
                panic!();
            }
        }

        // Single-letter symbol
        if "+-*/;=(),{}<>[]&.!?:|^%~".contains(c) {
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
                '^' => TokenType::HAT,
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
                '%' => TokenType::MOD,
                '~' => TokenType::TILDA,
                _ => panic!("unknown {}", c),
            };
            tokens.push(new_token(ty, idx));

            idx += 1;
            continue;
        }

        // Keyword or identifier
        if c.is_alphabetic() || c == '_' {
            let mut name = String::new();
            name.push(c);
            idx += 1;
            while idx < char_bytes.len() {
                let d = char::from(char_bytes[idx]);
                if d.is_alphabetic() || d.is_digit(10) || d == '_' {
                    name.push(d);
                    idx += 1;
                } else {
                    break;
                }
            }

            let ty = match keywords.get(&name) {
                Some(k) => *k,
                None => TokenType::IDENT,
            };

            let mut tok = new_token(ty, idx);
            tok.name = name.clone();
            //tok.start = name.clone();
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
            let mut tok = new_token(TokenType::NUM, idx);
            tok.val = i32::from_str_radix(&s, 10).unwrap();
            //tok.start = s;

            tokens.push(tok);
            continue;
        }

        panic!("cannot tokenize: {}", c);
    }

    tokens.push(new_token(TokenType::EOF, 0));

    return tokens;
}

