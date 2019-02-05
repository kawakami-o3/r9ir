// main

#[macro_use]
extern crate lazy_static;

use std::boxed::Box;
use std::cell::Cell;
use std::env;
use std::sync::Mutex;

lazy_static! {
    static ref TOKENS: Mutex<Vec<Token>> = Mutex::new(Vec::new());
}

#[derive(Debug, PartialEq)]
enum TokenType {
    NUM,
    PLUS,
    MINUS,
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
            let ty = match s.as_str() {
                "+" => TokenType::PLUS,
                "-" => TokenType::MINUS,
                _ => panic!(),
            };
            let tok = Token {
                ty: ty,
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

// Recursive-descendent parser

lazy_static! {
    static ref POS: Mutex<Cell<usize>> = Mutex::new(Cell::new(0));
}

#[derive(Clone, Debug, PartialEq)]
enum NodeType {
    NUM,
    PLUS,
    MINUS,
}

#[derive(Debug)]
struct Node {
    ty: NodeType,
    lhs: Option<Box<Node>>,
    rhs: Option<Box<Node>>,
    val: i32,
}

fn new_node(ty: NodeType, lhs: Node, rhs: Node) -> Node {
    Node {
        ty: ty,
        lhs: Some(Box::new(lhs)),
        rhs: Some(Box::new(rhs)),
        val: 0,
    }
}

fn new_node_num(val: i32) -> Node {
    Node {
        ty: NodeType::NUM,
        lhs: None,
        rhs: None,
        val: val,
    }
}

fn number(tokens: &Vec<Token>, pos: &mut usize) -> Node {
    //let mut pos = *POS.lock().unwrap().get_mut();
    //if let Some(t) = TOKENS.lock().unwrap().get(*pos) {
    if let Some(t) = tokens.get(*pos) {
        if t.ty == TokenType::NUM {
            let val = t.val;
            *pos += 1;
            return new_node_num(val);
        }
        panic!("number expected, but got {}", t.input);
    }
    panic!("out of boundary");
}

fn expr(tokens: &Vec<Token>, pos: &mut usize) -> Node {
    let mut lhs = number(tokens, pos);
    loop {
        let tok = tokens.get(*pos).unwrap();
        let op = &tok.ty;
        let ty = match op {
            TokenType::PLUS => NodeType::PLUS,
            TokenType::MINUS => NodeType::MINUS,
            _ => {
                break;
            }
        };
        *pos += 1;
        lhs = new_node(ty, lhs, number(tokens, pos));
    }
    return lhs;
}

// Code generator

lazy_static! {
    static ref REGS: Mutex<Vec<String>> = Mutex::new(vec![
        String::from("rdi"),
        String::from("rsi"),
        String::from("r10"),
        String::from("r11"),
        String::from("r12"),
        String::from("r13"),
        String::from("r14"),
        String::from("r15"),
        String::from("NULL")
    ]);
    static ref CUR: Mutex<Cell<usize>> = Mutex::new(Cell::new(0));
}

fn gen(node: Node, cur: &mut usize) -> String {
    if node.ty == NodeType::NUM {
        let regs = REGS.lock().unwrap();
        let reg = regs.get(*cur).unwrap();
        *cur += 1;
        if reg == "NULL" {
            panic!("register exhausted {}", cur);
        }
        println!("  mov {}, {}", reg, node.val);
        return reg.clone();
    }

    let lhs_box = node.lhs.unwrap();
    let rhs_box = node.rhs.unwrap();
    let dst = gen(*lhs_box, cur);
    let src = gen(*rhs_box, cur);

    match node.ty {
        NodeType::PLUS => {
            println!("  add {}, {}", dst, src);
            return dst;
        }
        NodeType::MINUS => {
            println!("  sub {}, {}", dst, src);
            return dst;
        }
        _ => {
            panic!("unknown operator");
        }
    }
}

fn main() {
    let argv: Vec<String> = env::args().collect();
    if argv.len() != 2 {
        println!("Usage: 9cc <code>");
        return;
    }

    // Tokenize and parse.
    tokenize(&argv[1]);
    let mut pos = POS.lock().unwrap();
    let tokens = TOKENS.lock().unwrap();
    let node = expr(&tokens, pos.get_mut());

    // Print the prologue.
    println!(".intel_syntax noprefix");
    println!(".global main");
    println!("main:");

    // Generate code while descending the parse tree.
    let mut cur = CUR.lock().unwrap();
    println!("  mov rax, {}", gen(node, cur.get_mut()));
    println!("  ret");
    return;
}
