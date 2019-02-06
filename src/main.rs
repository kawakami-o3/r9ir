// main

#[macro_use]
extern crate lazy_static;

use std::boxed::Box;
use std::env;
use std::sync::Mutex;

/*
fn to_node_type(token_type: TokenType) -> NodeType {
    match token_type {
        TokenType::ADD => NodeType::ADD,
        TokenType::SUB => NodeType::SUB,
        i => {
            panic!();
        }
    }
}
*/

fn to_ir_type(node_type: NodeType) -> IRType {
    match node_type {
        NodeType::ADD => IRType::ADD,
        NodeType::SUB => IRType::SUB,
        _ => {
            panic!();
        },
    }
}

lazy_static! {
    static ref TOKENS: Mutex<Vec<Token>> = Mutex::new(Vec::new());
}

#[derive(Debug, PartialEq)]
enum TokenType {
    NUM,
    ADD,
    SUB,
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
                "+" => TokenType::ADD,
                "-" => TokenType::SUB,
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
    static ref POS: Mutex<usize> = Mutex::new(0);
}

#[derive(Clone, Debug, PartialEq)]
enum NodeType {
    NUM,
    ADD,
    SUB,
}

#[derive(Debug)]
struct Node {
    ty: NodeType,
    lhs: Option<Box<Node>>,
    rhs: Option<Box<Node>>,
    val: usize,
}

fn new_node(ty: NodeType, lhs: Node, rhs: Node) -> Node {
    Node {
        ty: ty,
        lhs: Some(Box::new(lhs)),
        rhs: Some(Box::new(rhs)),
        val: 0,
    }
}

fn new_node_num(val: usize) -> Node {
    Node {
        ty: NodeType::NUM,
        lhs: None,
        rhs: None,
        val: val,
    }
}

fn number(tokens: &Vec<Token>, pos: &mut usize) -> Node {
    if let Some(t) = tokens.get(*pos) {
        if t.ty == TokenType::NUM {
            let val = t.val;
            *pos += 1;
            return new_node_num(val as usize);
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
            TokenType::ADD => NodeType::ADD,
            TokenType::SUB => NodeType::SUB,
            _ => {
                break;
            }
        };
        *pos += 1;
        lhs = new_node(ty, lhs, number(tokens, pos));
    }

    let tok = tokens.get(*pos).unwrap();
    if tok.ty != TokenType::EOF {
        panic!("stray token: {}", tok.input);
    }
    return lhs;
}

// Intermediate representation

#[derive(Debug, PartialEq)]
enum IRType {
    IMM,
    MOV,
    RETURN,
    KILL,
    ADD,
    SUB,
    NOP,
}

struct IR {
    op: IRType,
    lhs: usize,
    rhs: usize,
}

fn new_ir(op: IRType, lhs: usize, rhs: usize) -> IR {
    IR {
        op: op,
        lhs: lhs,
        rhs: rhs,
    }
}

lazy_static! {
    static ref INS: Mutex<Vec<IR>> = Mutex::new(Vec::new());
    // static ref INP: Mutex<usize> = Mutex::new(0);
    static ref REGNO: Mutex<usize> = Mutex::new(0);
}

fn gen_ir_sub(node: Node) -> usize {
    if node.ty == NodeType::NUM {
        let mut regno = REGNO.lock().unwrap();

        let r = *regno;
        (*regno) += 1;
        match INS.lock() {
            Ok(mut ins) => {
                ins.push(new_ir(IRType::IMM, r, node.val as usize));
            }
            Err(_) => {
                panic!("[gen_ir_sub:1] ins lock");
            }
        }

        return r;
    }

    assert!(node.ty == NodeType::ADD || node.ty == NodeType::SUB, "not op");

    let lhs = gen_ir_sub(*node.lhs.unwrap());
    let rhs = gen_ir_sub(*node.rhs.unwrap());


    match INS.lock() {
        Ok(mut ins) => {
            ins.push(new_ir(to_ir_type(node.ty), lhs, rhs));
            ins.push(new_ir(IRType::KILL, rhs, 0));
        }
        Err(_) => {
            panic!("[gen_ir_sub:2] ins lock");
        }
    }
    return lhs
}

fn gen_ir(node: Node) {
    let r = gen_ir_sub(node);

    match INS.lock() {
        Ok(mut ins) => {
            ins.push(new_ir(IRType::RETURN, r, 0));
        }
        Err(_) => {
            panic!("[gen_ir] ins lock");
        }
    }
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
    ]);
    static ref USED: Mutex<Vec<bool>> = Mutex::new([false; 8].to_vec());
    static ref REG_MAP: Mutex<Vec<i32>> = Mutex::new(Vec::new());
}

fn alloc(ir_reg: usize) -> usize {
    let mut reg_map = REG_MAP.lock().unwrap();
    if let Some(i) = reg_map.get(ir_reg) {
        if *i != -1 {
            let r = *i as usize;
            assert!(USED.lock().unwrap().get(r).expect(&format!("r:{}, i:{}", r, *i)), "");
            return r;
        }
    }

    let regs = REGS.lock().unwrap();
    let mut used = USED.lock().unwrap();
    for i in 0..regs.len() {
        if let Some(u) = used.get(i) {
            if *u {
                continue;
            }
        }
        used[i] = true;
        reg_map[ir_reg] = i as i32;
        return i;
    }
    panic!("register exhausted");
}

fn kill(i: i32) {
    let mut used = USED.lock().unwrap();
    let r = i as usize;
    assert!(used[r]);
    used[r] = false;
}

fn alloc_regs() {
    let mut ins = INS.lock().unwrap();
    for i in 0..ins.len() {
        let ir = &mut ins[i as usize];
        match ir.op {
            IRType::IMM => {
                ir.lhs = alloc(ir.lhs);
            }
            IRType::MOV | IRType::ADD | IRType::SUB => {
                ir.lhs = alloc(ir.lhs);
                ir.rhs = alloc(ir.rhs);
            }
            IRType::RETURN => {
                let mut reg_map = REG_MAP.lock().unwrap();
                kill(reg_map[ir.lhs]);
            }
            IRType::KILL => {
                let mut reg_map = REG_MAP.lock().unwrap();
                kill(reg_map[ir.lhs]);
                ir.op = IRType::NOP;
            }
            _ => { }
        }
    }
}

fn gen_x86() {
    let ins = INS.lock().unwrap();
    let regs = REGS.lock().unwrap();
    for i in 0..ins.len() {
        let ir = &ins[i as usize];
        match ir.op {
            IRType::IMM => {
                println!("  mov {}, {}", regs[ir.lhs as usize], ir.rhs);
            }
            IRType::MOV => {
                println!("  mov {}, {}", regs[ir.lhs as usize], regs[ir.rhs as usize]);
            }
            IRType::RETURN => {
                println!("  mov rax, {}", regs[ir.lhs as usize]);
                println!("  ret");
            }
            IRType::ADD => {
                println!("  add {}, {}", regs[ir.lhs as usize], regs[ir.rhs as usize]);
            }
            IRType::SUB => {
                println!("  sub {}, {}", regs[ir.lhs as usize], regs[ir.rhs as usize]);
            }
            _ => { }
        }
    }
}

fn main() {
    let argv: Vec<String> = env::args().collect();
    if argv.len() != 2 {
        println!("Usage: 9cc <code>");
        return;
    }

    match REG_MAP.lock() {
        Ok(mut reg_map) => {
            for _i in 0..1000 {
                reg_map.push(-1);
            }
        }
        _ => { }
    }

    // Tokenize and parse.
    tokenize(&argv[1]);
    let mut pos = POS.lock().unwrap();
    let tokens = TOKENS.lock().unwrap();
    let node = expr(&tokens, &mut pos);

    gen_ir(node);
    alloc_regs();

    // Print the prologue.
    println!(".intel_syntax noprefix");
    println!(".global main");
    println!("main:");

    gen_x86();
    return;
}
