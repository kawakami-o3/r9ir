// Atomic unit in the grammar is called "token".
// For example, `123`, `"abc"` and `while` are tokens.
// The tokenizer splits an input string into tokens.
// Spaces and comments are removed by the tokenizer.

#![allow(dead_code, non_camel_case_types)]

use crate::*;
use crate::preprocess::*;
use std::cell::RefCell;
use std::cmp;
use std::collections::HashMap;
use std::fs::File;
use std::io;
use std::io::Read;

thread_local! {
    static SYMBOLS: RefCell<Vec<Symbol>> = RefCell::new(Vec::new());
    static ESCAPED: RefCell<HashMap<char, char>> = RefCell::new(HashMap::new());

    static ENV: RefCell<Env> = RefCell::new(Env::new());
    static KEYWORDS: RefCell<HashMap<String, TokenType>> = RefCell::new(HashMap::new());
}

fn set_env(env: Env) {
    ENV.with(|c| {
        *c.borrow_mut() = env;
    })
}

fn buf() -> String {
    ENV.with(|c| {
        c.borrow().buf.clone()
    })
}

fn set_buf(s: String) {
    ENV.with(|c| {
        c.borrow_mut().buf = s;
    })
}

fn path() -> String {
    ENV.with(|c| {
        c.borrow().path.clone()
    })
}

fn set_path(s: String) {
    ENV.with(|c| {
        c.borrow_mut().path = s;
    })
}

fn tokens() -> Vec<Token> {
    ENV.with(|c| {
        c.borrow().tokens.clone()
    })
}

fn set_tokens(ts: Vec<Token>) {
    ENV.with(|c| {
        c.borrow_mut().tokens = ts;
    })
}

fn env_pop() {
    ENV.with(|c| {
        if let Some(next) = c.borrow().next.clone() {
            *c.borrow_mut() = *next;
        }
    })
}

fn add(t: Token) {
    ENV.with(|c| {
        c.borrow_mut().tokens.push(t);
    })
}

fn keywords_is_none() -> bool {
    KEYWORDS.with(|keywords| {
        keywords.borrow().len() == 0
    })
}

fn keywords_get(name: &String) -> Option<TokenType> {
    KEYWORDS.with(|keywords| {
        match (*keywords.borrow()).get(name) {
            Some(ty) => Some(ty.clone()),
            None => None,
        }
    })
}

fn set_keywords(m: HashMap<String, TokenType>) {
    KEYWORDS.with(|keywords| {
        *keywords.borrow_mut() = m;
    })
}

fn symbols() -> Vec<Symbol> {
    SYMBOLS.with(|s| {
        s.clone().into_inner()
    })
}

fn init_symbols() {
    SYMBOLS.with(|s| {
        let symbols =&mut *s.borrow_mut();

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
        symbols.push(Symbol { name: "&=", ty: TokenType::AND_EQ });
        symbols.push(Symbol { name: "^=", ty: TokenType::XOR_EQ });
        symbols.push(Symbol { name: "|=", ty: TokenType::OR_EQ });
    })
}

fn init_escaped() {
    ESCAPED.with(|e| {
        let escaped = &mut *e.borrow_mut();

        escaped.insert('a', char::from(7));  // \a
        escaped.insert('b', char::from(8));  // \b
        escaped.insert('f', char::from(12)); // \f
        escaped.insert('n', char::from(10)); // \n
        escaped.insert('r', char::from(13)); // \r
        escaped.insert('t', char::from(9));  // \t
        escaped.insert('v', char::from(11)); // \v
        escaped.insert('e', char::from(27)); // \033
        escaped.insert('E', char::from(27)); // \033
    })
}

fn escaped(c: char) -> Option<char> {
    ESCAPED.with(|escaped| {
        match (*escaped.borrow()).get(&c) {
            Some(c) => Some(*c),
            None => None,
        }
    })
}

#[derive(Clone, Debug)]
struct Env {
    path: String,
    buf: String,
    pos: i32,
    tokens: Vec<Token>,
    next: Option<Box<Env>>,
}

impl Env {
    fn new() -> Env {
        Env {
            path: String::new(),
            buf: String::new(),
            pos: 0,
            tokens: Vec::new(),
            next: None,
        }
    }
}

#[derive(Clone)]
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
    SHARP,      // #
    NEW_LINE,   // \n
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
    CONTINUE,   // "continue"
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
    AND_EQ,  // &=
    XOR_EQ,     // ^=
    OR_EQ,   // |=
    RETURN,     // "return"
    SIZEOF,     // "sizeof"
    ALIGNOF,    // "_Alignof"
    TYPEOF,     // "typeof"
    PARAM,      // Function-like macro parameter
    EOF,        // End marker
}

#[derive(Clone, Debug, PartialEq)]
pub struct Token {
    pub ty: TokenType, // Token type
    pub val: i32,      // Nuber literal
    pub name: String,  // Identifier

    // String literal
    pub str_cnt: String,
    pub len: usize,

    // For preprocessor
    pub stringize: bool,

    // For error reporting
    pub buf: String,
    pub path: String,
    pub start: usize,
    pub end: usize,
}

impl Token {
    fn append(&mut self, t: & Token) {
        self.str_cnt.pop(); // pop '\0'
        self.str_cnt.push_str(&t.str_cnt);
    }
}

pub fn new_token(ty: TokenType, start: usize) -> Token {
    Token{
        ty: ty,
        val: 0,
        name: String::new(),

        str_cnt: String::new(),
        len: 0,

        stringize: false,

        buf: buf(),
        path: path(),
        start: start,
        end: 0,
    }
}

fn open_file(f: String) -> Box<Read> {
    let mut path = f;
    if path == "-" {
        return Box::new(io::stdin());
    }

    // remove EOS, '\0'.
    if &path[path.len()-1..] == "\0" {
        path = path[..path.len()-1].to_string();
    }
    match File::open(path) {
        Ok(file) => {
            return Box::new(file);
        }
        Err(e) => {
            panic!(e);
        }
    }
}

fn read_file<T: Read>(file: &mut T) -> String {
    let mut buffer = String::new();
    match file.read_to_string(&mut buffer) {
        Ok(_) => {
            // We want  to make sure that a source file ends with a newline.
            // Add not only one but two to prtect against a backslash at EOF.
            buffer.push_str("\n\n");
            return buffer;
        }
        Err(e) => {
            panic!(e);
        }
    }
}

fn new_env(next: Option<Env>, path: String, buf: String) -> Env {
    let mut env = Env::new();
    env.path = if path == "-" {
        "(stdin)".to_string()
    } else {
        path
    };
    env.buf = buf;
    if next.is_none() {
        env.next = None;
    } else {
        env.next = Some(Box::new(next.unwrap()));
    }
    return env;
}

// Error reporting

// Finds a line pointed by a given pointer from the input file
// to print it out.
pub fn print_line(start: & String, path: & String, pos: usize) {
    let mut line = 0;
    let mut col = 0;

    let mut target = String::new();
    let mut idx = 0;
    let input = start;
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

        eprintln!("error at {}:{}:{}", path, line+1, col+1);
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


#[macro_export]
macro_rules! warn_token {
    ($t:expr, $msg:expr) => {
        if $t.start > 0 {
            print_line(&$t.buf, &$t.path, $t.start);
        }
        eprintln!("{}", $msg);
    };
}


// 'msg' is String instead of &'static str.
// The 'msg' argument is "..." (&str) or format!() (String),
// If msg is &'static str, format! create a temporary variable
// and it can't live long enough.
pub fn bad_token(t: & Token, msg: String) {
    warn_token!(t, msg);
    panic!();
}

pub fn bad_position(idx: usize, msg: String) {
    print_line(&buf(), &path(), idx);
    panic!(msg);
}

pub fn tokstr(t: &Token) -> &str {
    assert!(t.start != 0 || t.end != 0);
    return &t.buf[t.start..t.end];
}

pub fn get_line_number(t: &Token) -> i32 {
    let mut n = 0;
    for i in 0..t.end {
        if &t.buf[i..i+1] == "\n" {
            n += 1;
        }
    }
    return n;
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
    bad_position(idx, "unclosed comment".to_string());
    panic!();
}

fn keyword_map() -> HashMap<String, TokenType> {
    let mut keywords = HashMap::new();
    keywords.insert("_Alignof".to_string(), TokenType::ALIGNOF);
    keywords.insert("break".to_string(), TokenType::BREAK);
    keywords.insert("char".to_string(), TokenType::CHAR);
    keywords.insert("continue".to_string(), TokenType::CONTINUE);
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
    keywords.insert("typeof".to_string(), TokenType::TYPEOF);
    keywords.insert("void".to_string(), TokenType::VOID);
    keywords.insert("while".to_string(), TokenType::WHILE);
    return keywords;
} 

#[derive(Debug)]
struct TokenInfo {
    token: Token,
    len: usize,
}

macro_rules! error_char {
    ($t:expr) => {
        bad_token($t, "unclosed character literal".to_string());
    };
}

fn isoctal(c: char) -> bool {
    '0' <= c && c <= '7'
}

fn hex(c: char) -> i32 {
    match c.to_digit(16) {
        Some(i) => i as i32,
        None => panic!(),
    }
}

// Read a single character in a char or string literal.
fn c_char(t: &mut Token, p: &String, idx: usize) -> TokenInfo {
    let char_bytes = p.as_bytes();
    let mut c = char::from(char_bytes[idx]);
    let mut len = 0;

    // Nonescaped
    if c != '\\' {
        t.val = u32::from(c) as i32;
        return TokenInfo {
            token: t.clone(),
            len: len + 1,
        };
    }

    len += 1;
    c = char::from(char_bytes[idx+len]);

    // Simple (e.g. `\n` or `\a`)
    match escaped(c) {
        Some(esc) => {
            c = esc;

            t.val = u32::from(c) as i32;
            return TokenInfo {
                token: t.clone(),
                len: len + 1,
            };
        }
        None => { }
    }

    // Hexadecimal
    if c == 'x' {
        let mut res = 0;
        len += 1;
        loop {
            c = char::from(char_bytes[idx+len]);
            if let Some(i) = c.to_digit(16) {
                res = res * 16 + i as i32;
                len += 1;
            } else {
                break;
            }
        }

        t.val = res;
        return TokenInfo {
            token: t.clone(),
            len: len,
        };
    }

    // Octal
    if isoctal(c) {
        let mut i = c.to_digit(8).unwrap();
        len += 1;
        c = char::from(char_bytes[idx+len]);
        if isoctal(c) {
            i = i * 8 + c.to_digit(8).unwrap();
            len += 1;
            c = char::from(char_bytes[idx+len]);
        }
        if isoctal(c) {
            i = i * 8 + c.to_digit(8).unwrap();
            len += 1;
        }

        t.val = i as i32;
        return TokenInfo {
            token: t.clone(),
            len: len,
        };
    }

    t.val = u32::from(c) as i32;
    return TokenInfo {
        token: t.clone(),
        len: 2,
    };
}

fn char_literal(p: &String, idx: usize) -> TokenInfo {
    let mut t = new_token(TokenType::NUM, idx);
    let mut len = 1;

    let mut info = c_char(&mut t, p, idx + len);
    len += info.len;

    if &p[idx+len..idx+len+1] != "\'" {
        error_char!(&t);
    }
    len += 1;
    info.len = len;
    info.token.end = idx + info.len;
    return info;
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
        let mut t = new_token(TokenType::NUM, idx);
        let info = c_char(&mut t, p, idx + len);
        let c = char::from(info.token.val as u8);
        ret.push(c);
        len += info.len;
    }
    if (idx+len) >= p.len() {
        error_str!(&t);
    }
    //ret.push(&format!("\\{:03o}", u32::from(c)));
    ret.push(char::from(0));
    t.str_cnt = ret;
    t.len = len;
    t.end = idx + len + 1;
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
    t.end = ret;

    return TokenInfo{
        token: t,
        len: ret - idx,
    };
}

fn hexadecimal(p: &String, idx: usize) -> TokenInfo {
    let mut t = new_token(TokenType::NUM, idx);
    let mut ret = idx + 2;

    if !char::from((&p[ret..ret+1].as_bytes())[0]).is_ascii_hexdigit() {
        bad_token(&t, "bad hexadecimal number".to_string());
    }

    while let Some(i) = char::from((&p[ret..ret+1].as_bytes())[0]).to_digit(16) {
        t.val = t.val * 16 + i as i32;
        ret += 1;
    }

    t.end = ret;
    return TokenInfo {
        token: t,
        len: ret - idx,
    };
}

fn octal(p: &String, idx: usize) -> TokenInfo {
    let mut t = new_token(TokenType::NUM, idx);
    let mut ret = idx + 1;

    while let Some(i) = util::first_char(&p[ret..ret+1]).to_digit(8) {
        t.val = t.val * 8 + i as i32;
        ret += 1;
    }
    t.end = ret;

    return TokenInfo {
        token: t,
        len: ret - idx,
    };
}

fn decimal(p: &String, idx: usize) -> TokenInfo {
    let mut t = new_token(TokenType::NUM, idx);
    let mut ret = idx;

    while let Some(i) = util::first_char(&p[ret..ret+1]).to_digit(10) {
        t.val = t.val * 10 + i as i32;
        ret += 1;
    }

    t.end = ret;
    return TokenInfo {
        token: t,
        len: ret - idx,
    };
}

fn number(p: &String, idx: usize) -> TokenInfo {
    if &p[idx..idx+2].to_lowercase() == "0x" {
        return hexadecimal(p, idx);
    }
    if &p[idx..idx+1] == "0" {
        return octal(p, idx);
    }
    return decimal(p, idx);
}

fn scan() {
    init_symbols();
    init_escaped();
    let symbols = symbols();

    let p = &buf();

    let char_bytes = p.as_bytes();
    let mut idx = 0;
    'outer: while idx < char_bytes.len() {
        let c: char = char::from(char_bytes[idx]);
        // New line (preprocessor-only token)
        if c == '\n' {
            let mut t = new_token(TokenType::NEW_LINE, idx);
            idx += 1;
            t.end = idx;
            add(t);
            continue;
        }

        // Whitespace
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
        for s in symbols.iter() {
            let bs = &char_bytes[idx..cmp::min(idx+s.name.len(), char_bytes.len())];
            if s.name.as_bytes() != bs {
                continue;
            }

            let mut t = new_token(s.ty, idx);
            idx += s.name.len();
            t.end = idx;
            add(t);
            continue 'outer;
        }

        // Single-letter symbol
        if "+-*/;=(),{}<>[]&.!?:|^%~#".contains(c) {
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
                '#' => TokenType::SHARP,
                _ => panic!("unknown {}", c),
            };
            let mut t = new_token(ty, idx);
            idx += 1;
            t.end = idx;
            add(t);
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

        bad_position(idx, "cannot tokenize: {}".to_string());
    }
}

fn replace_crlf(p: String) -> String {
    let mut cnt = String::new();
    let mut i = 0;
    while i < p.len() {
        if i+1 < p.len() && &p[i..i+2] == "\r\n" {
            i += 1;
        }
        cnt.push_str(&p[i..i+1]);
        i += 1;
    }
    return cnt;
}

// Concatenates continuation lines. We keep the total number of
// newline characters the same to keep the line counter sane.
fn remove_backslash_newline(p: String) -> String {
    let mut cnt = 0;
    let mut ret = String::new();
    let mut i = 0;
    while i < p.len() {
        if i+1 < p.len() && &p[i..i+2] == "\\\n" {
            cnt += 1;
            i += 2;
        } else if &p[i..i+1] == "\n" {
            for _i in 0..cnt+1 {
                ret.push('\n');
            }
            i += 1;
            cnt = 0;
        } else {
            ret.push_str(&p[i..i+1]);
            i += 1;
        }
    }
    return ret;
}

fn strip_newlines(tokens: Vec<Token>) -> Vec<Token> {
    let mut v = Vec::new();

//    let ts: Vec<Token> = tokens().iter()
//        .filter(|t| t.ty != TokenType::NEW_LINE)
//        .cloned()
//        .collect();

    for t in tokens.iter() {
        if t.ty != TokenType::NEW_LINE {
            v.push(t.clone());
        }
    }
    return v;
}

fn join_string_literals(tokens: Vec<Token>) -> Vec<Token> {
    let mut v: Vec<Token> = Vec::new();

    let ts = tokens;
    for t in ts.iter() {
        if v.len() > 0 && v.last().unwrap().ty == TokenType::STR && t.ty == TokenType::STR {
            v.last_mut().unwrap().append(t);
            continue;
        }

        v.push(t.clone());
    }

    return v;
}

pub fn tokenize(path: String, add_eof: bool) -> Vec<Token> {
    if keywords_is_none() {
        set_keywords(keyword_map());
    }

    let mut fp = open_file(path.clone());
    let mut buf = read_file(&mut fp);
    buf = replace_crlf(buf);
    buf = remove_backslash_newline(buf);

    set_env(new_env(None, path, buf));
    scan();
    if add_eof {
        add(new_token(TokenType::EOF, 0));
    }

    let mut v = tokens();
    env_pop();

    v = preprocess(v);
    v = strip_newlines(v);
    return join_string_literals(v);
}
