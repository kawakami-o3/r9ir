// main

#[macro_use]
extern crate lazy_static;

use std::boxed::Box;
use std::env;
use std::sync::Mutex;

/*
fn to_node_type(token_type: &TokenType) -> NodeType {
    match token_type {
        TokenType::ADD => NodeType::ADD,
        TokenType::SUB => NodeType::SUB,
        _ => {
            panic!();
        }
    }
}
*/

fn to_ir_type(node_type: &NodeType) -> IRType {
    match node_type {
        NodeType::ADD => IRType::ADD,
        NodeType::SUB => IRType::SUB,
        _ => {
            panic!();
        },
    }
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

fn tokenize(p: &String) -> Vec<Token> {
    let mut token_strs = Vec::new();
    let mut t = String::new();
    for c in p.chars() {
        if c.is_whitespace() {
            continue;
        }
        if c == '+' || c == '-' {
            token_strs.push(t.clone());
            t = String::new();
            token_strs.push(format!("{}", c));
        }
        if c.is_digit(10) {
            t.push(c);
            continue;
        }
    }
    token_strs.push(t);

    let mut tokens = Vec::new();
    for s in token_strs {
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

            tokens.push(tok);
        } else {
            let tok = Token {
                ty: TokenType::NUM,
                val: i32::from_str_radix(&s, 10).unwrap(),
                input: s,
            };

            tokens.push(tok);
        }
    }

    let tok = Token {
        ty: TokenType::EOF,
        val: 0,
        input: String::new(),
    };
    tokens.push(tok);

    return tokens;
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
    let t = &tokens[*pos];
    if t.ty == TokenType::NUM {
        let val = t.val;
        *pos += 1;
        return new_node_num(val as usize);
    }
    panic!("number expected, but got {}", t.input);
}

fn expr(tokens: &Vec<Token>, pos: &mut usize) -> Node {
    let mut lhs = number(tokens, pos);
    loop {
        let ty = match &tokens[*pos].ty {
            TokenType::ADD => NodeType::ADD,
            TokenType::SUB => NodeType::SUB,
            _ => {
                break;
            }
        };
        //let ty = to_node_type(&tokens[*pos].ty);
        *pos += 1;
        lhs = new_node(ty, lhs, number(tokens, pos));
    }

    if tokens[*pos].ty != TokenType::EOF {
        panic!("stray token: {}", tokens[*pos].input);
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
    static ref REGNO: Mutex<usize> = Mutex::new(0);
}

fn gen_ir_sub(v: &mut Vec<IR>, node: Node) -> usize {
    if node.ty == NodeType::NUM {
        let mut regno = REGNO.lock().unwrap();

        let r = *regno;
        (*regno) += 1;
        v.push(new_ir(IRType::IMM, r, node.val as usize));
        return r;
    }

    assert!(node.ty == NodeType::ADD || node.ty == NodeType::SUB, "not op");

    let lhs = gen_ir_sub(v, *node.lhs.unwrap());
    let rhs = gen_ir_sub(v, *node.rhs.unwrap());

    v.push(new_ir(to_ir_type(&node.ty), lhs, rhs));
    v.push(new_ir(IRType::KILL, rhs, 0));
    return lhs
}

fn gen_ir(node: Node) -> Vec<IR> {
    let mut v = Vec::new();
    let r = gen_ir_sub(&mut v, node);
    v.push(new_ir(IRType::RETURN, r, 0));
    return v;
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
    if reg_map[ir_reg] != -1{
        let r = reg_map[ir_reg] as usize;
        assert!(USED.lock().unwrap()[r], "");
        return r;
    }

    let regs = REGS.lock().unwrap();
    let mut used = USED.lock().unwrap();
    for i in 0..regs.len() {
        if used[i] {
            continue;
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

fn alloc_regs(irv: &mut Vec<IR>) {
    for i in 0..irv.len() {
        let ir = &mut irv[i as usize];
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

fn gen_x86(irv: & Vec<IR>) {
    let regs = REGS.lock().unwrap();
    for i in 0..irv.len() {
        let ir = &irv[i as usize];
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
    let tokens = tokenize(&argv[1]);
    let mut pos = POS.lock().unwrap();
    let node = expr(&tokens, &mut pos);

    let mut irv = gen_ir(node);
    alloc_regs(&mut irv);

    // Print the prologue.
    println!(".intel_syntax noprefix");
    println!(".global main");
    println!("main:");

    gen_x86(&irv);
    return;
}
