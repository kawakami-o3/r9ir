// Atomic unit in the grammar is called "token".
// For example, `123`, `"abc"` and `while` are tokens.
// The tokenizer splits an input string into tokens.
// Spaces and comments are removed by the tokenizer.

#![allow(dead_code, non_camel_case_types)]

use crate::*;
use std::cmp;
use std::sync::Mutex;
use std::collections::HashMap;

lazy_static! {
    static ref SYMBOLS: Mutex<Vec<Symbol>> = Mutex::new(Vec::new());
    static ref ESCAPED: Mutex<HashMap<char, char>> = Mutex::new(HashMap::new());

    static ref INPUT_FILE: Mutex<String> = Mutex::new(String::new());

    static ref TOKENS: Mutex<Vec<Token>> = Mutex::new(Vec::new());
    static ref KEYWORDS: Mutex<HashMap<String, TokenType>> = Mutex::new(HashMap::new());
}

fn input_file() -> String {
    let input = INPUT_FILE.lock().unwrap();
    return input.clone();
}

fn set_input_file(s: String) {
    let mut input = INPUT_FILE.lock().unwrap();
    *input = s;
}

fn tokens() -> Vec<Token> {
    let tokens = TOKENS.lock().unwrap();
    return tokens.clone();
}

fn set_tokens(ts: Vec<Token>) {
    let mut tokens = TOKENS.lock().unwrap();
    *tokens = ts;
}

fn add(t: Token) {
    let mut tokens = TOKENS.lock().unwrap();
    tokens.push(t);
}

fn keywords_get(name: &String) -> Option<TokenType> {
    match KEYWORDS.lock() {
        Ok(ref keywords) => {
            match keywords.get(name) {
                Some(ty) => Some(ty.clone()),
                None => None,
            }
        }
        Err(_) => {
            panic!();
        }
    }
}

fn set_keywords(m: HashMap<String, TokenType>) {
    let mut keywords = KEYWORDS.lock().unwrap();
    *keywords = m;
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

#[derive(Clone, Debug)]
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

impl Token {
    fn append(&mut self, t: & Token) {
        self.str_cnt.pop(); // pop '\0'
        self.str_cnt.push_str(&t.str_cnt);
    }
}

fn new_token(ty: TokenType, idx: usize) -> Token {
    Token{
        ty: ty,
        val: 0,
        name: String::new(),
        start: idx,
        str_cnt: String::new(),
        len: 0,
    }
}

// Error reporting

// Finds a line pointed by a given pointer from the input file
// to print it out.
fn print_line(pos: usize) {
    let mut line = 0;
    let mut col = 0;

    let mut target = String::new();
    let mut idx = 0;
    let input = input_file();
    let bytes = input.as_bytes();
    while idx < bytes.len() {
        let p = char::from(bytes[idx]);
        if p == '\n' && idx != pos {
            target = String::new();
            line += 1;
            col = 0;
            idx += 1;
            continue;
        }

        if idx != pos {
            target.push(p);
            col += 1;
            idx += 1;
            continue;
        }


        eprintln!("error at {}:{}:{}", filename(), line+1, col+1);
        eprintln!();

        while char::from(bytes[idx]) != '\n' {
            target.push(char::from(bytes[idx]));
            idx += 1;
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
    print_line(t.start);
    panic!(msg);
}

fn block_comment(p: &String, idx: usize) -> usize {
    let mut ret = idx + 2;
    while ret < p.len() {
        if &p[ret..ret+2] != "*/" {
            ret += 1;
            continue;
        }
        ret += 2;
        return ret - idx;
    }
    panic!("unclosed comment");
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

struct TokenInfo {
    token: Token,
    len: usize,
}

macro_rules! error_char {
    ($t:expr) => {
        bad_token($t, "unclosed character literal".to_string());
    };
}

fn char_literal(p: &String, idx: usize) -> TokenInfo {
    let mut t = new_token(TokenType::NUM, idx);
    let mut len = 1;

    if p.len() <= idx + len {
        error_char!(&t);
    }

    let char_bytes = p.as_bytes();
    let mut c = char::from(char_bytes[idx+len]);
    if c != '\\' {
        len += 1;
        t.val = u32::from(c) as i32;
    } else {
        if p.len() <= idx + len + 1 {
            error_char!(&t);
        }
        c = char::from(char_bytes[idx+len+1]);
        match escaped(c) {
            Some(esc) => {
                c = esc;
            }
            None => { }
        }
        len += 2;
        t.val = u32::from(c) as i32;
    }
    
    if char::from(char_bytes[idx+len]) != '\'' {
        error_char!(&t);
    }
    len += 1;
    return TokenInfo {
        token: t,
        len: len,
    };
}

macro_rules! error_str {
    ($t:expr) => {
        bad_token($t, "unclosed string literal".to_string());
    };
}

fn string_literal(p: &String, idx: usize) -> TokenInfo {
    let mut t = new_token(TokenType::STR, idx);

    let mut len = 1;
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
            error_str!(&t);
        }
        match escaped(c) {
            Some(esc) => ret.push(esc),
            None => ret.push(c),
        }
        len += 1;
    }
    if (idx+len) >= p.len() {
        error_str!(&t);
    }
    //ret.push(&format!("\\{:03o}", u32::from(c)));
    ret.push(char::from(0));
    t.str_cnt = ret;
    t.len = len;
    return TokenInfo{
        token: t,
        len: len,
    };
}

fn ident(p: &String, idx: usize) -> TokenInfo {
    let mut ret = idx;

    let mut name = String::new();
    name.push_str(&p[ret..ret+1]);
    ret += 1;
    while ret < p.len() {
        let b = &p[ret..ret+1].as_bytes();
        let d = char::from(b[0]);
        if d.is_alphabetic() || d.is_digit(10) || d == '_' {
            name.push(d);
            ret += 1;
        } else {
            break;
        }
    }

    let ty = match keywords_get(&name) {
        Some(k) => k,
        None => TokenType::IDENT,
    };

    let mut t = new_token(ty, idx);
    t.name = name.clone();

    return TokenInfo{
        token: t,
        len: ret - idx,
    };
}

fn number(p: &String, idx: usize) -> TokenInfo {
    let mut ret = idx;

    let mut s = String::new();
    s.push_str(&p[ret..ret+1]);
    ret += 1;
    while ret < p.len() {
        let b = &p[ret..ret+1].as_bytes();
        let d = char::from(b[0]);
        if d.is_digit(10) {
            s.push(d);
            ret += 1;
        } else {
            break;
        }
    }
    let mut t = new_token(TokenType::NUM, ret);
    t.val = i32::from_str_radix(&s, 10).unwrap();
    //tok.start = s;

    return TokenInfo {
        token: t,
        len: ret - idx,
    };
}

fn scan() {
    init_symbols();
    init_escaped();

    let p = &input_file();

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
            idx += block_comment(p, idx);
            continue;
        }

        // Character literal
        if c == '\'' {
            let info = char_literal(p, idx);
            add(info.token);
            idx += info.len;
            continue;
        }

        // String literal
        if c == '"' {
            let info = string_literal(p, idx);
            add(info.token);
            idx += info.len + 1;
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

                    add(new_token(s.ty, idx));
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
            add(new_token(ty, idx));

            idx += 1;
            continue;
        }

        // Keyword or identifier
        if c.is_alphabetic() || c == '_' {
            let info = ident(p, idx);
            add(info.token);
            idx += info.len;
            continue;
        }

        // Number
        if c.is_digit(10) {
            let info = number(p, idx);
            add(info.token);
            idx += info.len;
            continue;
        }

        panic!("cannot tokenize: {}", c);
    }

    add(new_token(TokenType::EOF, 0));
}

fn remove_backslash_newline() {
    let p = input_file();
    let mut cnt = String::new();
    let mut i = 0;
    while i < p.len() {
        if i+1 < p.len() && &p[i..i+2] == "\\\n" {
            i += 2;
        }
        cnt.push_str(&p[i..i+1]);
        i += 1;
    }
    set_input_file(cnt);
}

fn join_string_literals() {
    let mut v: Vec<Token> = Vec::new();

    let ts = tokens();
    for t in ts.iter() {
        if v.len() > 0 && v.last().unwrap().ty == TokenType::STR && t.ty == TokenType::STR {
            v.last_mut().unwrap().append(t);
            continue;
        }

        v.push(t.clone());
    }

    set_tokens(v);
}

pub fn tokenize(p: &String) -> Vec<Token> {
    set_keywords(keyword_map());
    set_input_file(p.clone());

    remove_backslash_newline();
    scan();
    join_string_literals();
    return tokens();
}
