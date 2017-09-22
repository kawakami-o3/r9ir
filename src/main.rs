#![allow(unused_imports)]

use std::env;
use std::fmt;
use std::io;
use std::cmp;
use std::io::Write;
use std::io::Read;
use std::collections::LinkedList;

use std::error::Error;
use std::io::prelude::*;
use std::fs::File;
use std::path::Path;


macro_rules! error {
    ($fmt:expr) => (writeln!(&mut io::stderr(), $fmt));
    ($fmt:expr,$($x:tt)*) => (writeln!(&mut io::stderr(), $fmt, $( $x )*));
}

const MAX_ARGS: usize = 6;
fn regs(i: usize) -> String {
    match i {
        0 => String::from("%rcx"),
        1 => String::from("%rdx"),
        2 => String::from("%r8"),
        3 => String::from("%r9"),
        _ => format!("{}(%rsp)", 8 * i)
    }
}

struct Env {
    buffer: Buffer,
    vars: LinkedList<Var>,
}

impl Env {
    fn new() -> Env {
        let mut ret = Env {
            buffer: Buffer::new(),
            vars: LinkedList::new()
        };
        ret.vars.push_back(Var {
            name: String::from("null"),
            pos: 0
        });

        ret
    }

    fn null(&mut self) -> Var {
        Var {
            name: String::from("null"),
            pos: 0
        }
    }

    fn new_var(&mut self, name:String) -> Var {
        let v = Var {
            name: name,
            pos: self.vars.len()
        };

        self.vars.push_back(v.clone());
        v
    }

    fn find_var(&mut self, name: &String) -> Var {
        for x in self.vars.iter() {
            if x.name == *name {
                return x.clone()
            }
        }
        self.null()
    }
}

struct Buffer {
    chars: Vec<char>,
    idx: usize,
}

impl Buffer {
    fn new() -> Buffer {
        let mut vec = Vec::new();

        for i in io::stdin().bytes() {
            vec.push(char::from(i.unwrap()));
        }

        Buffer {
            chars: vec,
            idx: 0,
        }
    }
/*
    fn print(& self) {
        //println!("{:?}", self.chars)

        let mut bytes: Vec<u8> = Vec::new();

        //let a = self.chars[0].as_byte();

        for c in self.chars.clone() {
            let mut bs = [0; 2];
            c.encode_utf8(&mut bs);

            for b in bs.iter() {
                bytes.push(*b);
            }
        }

        //write_debug("hello".as_bytes())
        write_debug(&bytes)
    }
*/
    fn getc(&mut self) -> char {
        self.idx += 1;
        return self.chars[self.idx - 1];
    }

    fn ungetc(&mut self) {
        self.idx -= 1;
    }

    fn can_read(& self) -> bool {
        return self.chars.len() > self.idx;
    }

    fn is_end(& self) -> bool {
        return !self.can_read();
    }

    fn skip_space(&mut self) {
        while self.can_read() {
            let c = self.getc();
            if c.is_whitespace() {
                continue;
            }

            self.ungetc();
            return;
        }
    }
}


impl fmt::Display for Buffer {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut s = String::new();
        for i in &self.chars {
            for c in i.escape_default() {
                s.push(c);
            }
            s.push(',');
        }
        write!(f, "['{}', {}]", s, self.idx)
    }
}

struct Var {
    name: String,
    pos: usize,

}

impl Var {
    /*
    fn new(name:String) -> Var {
        unsafe {
            Var {
                name: name,
                pos: VARS.unwrap().len(), // + 1,
            }
        }
    }
    */

    fn is_null(&self) -> bool {
        self.pos == 0
    }

    fn clone(&self) -> Var {
        Var {
            name: self.name.clone(),
            pos: self.pos,
        }
    }
}

struct Func {
    name: String,
    args: Vec<Ast>
}

enum Ast {
    Op {op:char, left: Box<Ast>, right: Box<Ast>},
    Int(u32),
    //Str(String),
    Sym(Var),
    Func(Func),
    Null
}

impl Ast {
    fn is_null(&self) -> bool {
        match *self {
            Ast::Null => true,
            _ => false
        }
    }
}


// ***** the following content is compatible. *****


fn make_ast_op(op: char, l: Ast, r: Ast) -> Ast {
    Ast::Op {op:op, left: Box::new(l), right: Box::new(r)}
    /*
    match op {
        '+' | '-' | '*' | '/' => Ast::Op {op:op, left: Box::new(l), right: Box::new(r)},
        a => {
            panic!("operator: {}", a);
        }
    }
    */
}

fn make_ast_int(var: u32) -> Ast {
    Ast::Int(var)
}

fn make_ast_sym(var: Var) -> Ast {
    Ast::Sym(var)
}

fn make_ast_funcall(fname: String, args: Vec<Ast>) -> Ast {
    Ast::Func(Func{name: fname, args: args})
}

/*
fn make_var(name: String) -> Var {
    return Var::new(name);
}
*/

fn priority(op: char) -> i32 {
    match op {
        '=' => 1,
        '+' | '-' => 2,
        '*' | '/' => 3,
        //a => { panic!("operator: {}", a) }
        _ => -1
    }
}

fn read_number(environment: &mut Env, mut n: u32) -> Ast {
    loop {
        let c = environment.buffer.getc();
        if !c.is_digit(10) {
            environment.buffer.ungetc();
            return make_ast_int(n);
        }
        n = n * 10 + c.to_digit(10).unwrap();
    }
}

fn read_ident(environment: &mut Env) -> String {
    // let mut is_escape = false;
    let mut sym = String::new(); // no limit symbol length
    // let mut i = 1;

    loop {
        let c = environment.buffer.getc();
        if ! (c.is_digit(10) || c.is_alphabetic()) {
            environment.buffer.ungetc();
            break;
        }
        sym.push(c);
    }
    sym
}

fn read_func_args(environment: &mut Env, fname: String) -> Ast {
    let mut args: Vec<Ast> = vec![];
    loop {
        environment.buffer.skip_space();
        if environment.buffer.getc() == ')' {
            break;
        }
        environment.buffer.ungetc();
        args.push(read_expr2(environment, 0));

        let c = environment.buffer.getc();
        if c == ')' {
            break;
        }
        if c == ',' {
            environment.buffer.skip_space();
        } else {
            panic!("Unexpected character: '{}'", c);
        }
    }

    if args.len() > MAX_ARGS {
        panic!("Too many arguments: {}", fname);
    }

    make_ast_funcall(fname, args)
}

fn read_ident_or_func(environment: &mut Env) -> Ast {
    let name = read_ident(environment);

    environment.buffer.skip_space();
    if environment.buffer.getc() == '(' {
        return read_func_args(environment, name);
    }

    environment.buffer.ungetc();

    let mut v = environment.find_var(&name);
    if v.is_null() {
        v = environment.new_var(name);
    }
    return make_ast_sym(v);
}

fn read_prim(environment: &mut Env) -> Ast {
    if environment.buffer.is_end() {
        return Ast::Null;
    }

    let c = environment.buffer.getc();
    if c.is_digit(10) {
        return read_number(environment, c.to_digit(10).unwrap());
    } else if c.is_alphabetic() {
        environment.buffer.ungetc();
        return read_ident_or_func(environment);
    }
    panic!("Don't know how to handle '{}'",c);
}

fn read_expr2(environment: &mut Env, prec: i32) -> Ast {
    environment.buffer.skip_space();
    let mut ast = read_prim(environment);

    loop {
        environment.buffer.skip_space();
        if environment.buffer.is_end() {
            return ast
        }
        let c = environment.buffer.getc();
        let prec2 = priority(c);
        if prec2 < 0 || prec2 < prec {
            environment.buffer.ungetc();
            return ast
        }

        environment.buffer.skip_space();

        //ast.print();
        ast = make_ast_op(c, ast, read_expr2(environment, prec2 + 1));
    }
}

fn read_expr(environment: &mut Env) -> Ast {
    //let buf: &mut Buffer = &mut Buffer::new();

    let r: Ast = read_expr2(environment, 0);
    if r.is_null() {
        return Ast::Null;
    }

    environment.buffer.skip_space();
    //environment.buffer.print();
    
    let c: char = environment.buffer.getc();
    if c != ';' {
        panic!("Unterminated expression");
    }
    return r;
}

fn emit_binop(ast: Ast) {
    if let Ast::Op {op, left, right} = ast {
        if op == '=' {
            emit_expr(*right);
            if let Ast::Sym(v) = *left {
                print!("mov %eax, -{}(%rbp)\n\t", v.pos * 4);
            } else {
                panic!("Symbol expected");
            }
            return;
        }

        let opasm = match op {
            '+' => "add",
            '-' => "sub",
            '*' => "imul",
            '/' => "---",
            _ => { panic!() }
        };

        emit_expr(*left);
        print!("push %rax\n\t");
        emit_expr(*right);

        if op == '/' {
            print!("mov %eax, %ebx\n\t");
            print!("pop %rax\n\t");
            print!("mov $0, %edx\n\t");
            print!("idiv %ebx\n\t");
        } else {
            print!("pop %rbx\n\t");
            print!("{} %ebx, %eax\n\t", opasm);
        }
    }
}

fn emit_expr(ast: Ast) {
    match ast {
        Ast::Int(i) => {
            print!("mov ${}, %eax\n\t", i);
        }
        Ast::Sym(v) => {
            print!("mov -{}(%rbp), %eax\n\t", v.pos * 4);
        }
        Ast::Func(f) => {
            let n = f.args.len();
            let shift = 8 * 2 * cmp::max(f.args.len() / 2 + 1, 2);
            
            print!("subq ${}, %rsp\n\t", shift);

            for x in f.args {
                emit_expr(x);
                print!("push %rax\n\t");
            }
            for i in 0..n {
                print!("pop {}\n\t", regs(n - i - 1));
            }
            print!("call {}\n\t", f.name);

            print!("addq ${}, %rsp\n\t", shift);
        }
        _ => {
            emit_binop(ast);
        }
    }
}

/*
fn emit_expr(ast: Ast) {
    match ast {
        Ast::Int(i) => {
            print!("mov ${}, %eax\n\t", i);
        }
        Ast::Sym(v) => {
            print!("mov -{}(%rbp), %eax\n\t", v.pos * 4);
        }
        Ast::Func(f) => {
            let n = f.args.len();
            for i in 0..n-1 {
                print!("push %{}\n\t", regs(i));
            }
            for x in f.args {
                emit_expr(x);
                print!("push %rax\n\t");
            }
            for i in 0..n-1 {
                print!("pop %{}\n\t", regs(n - i - 1));
            }
            print!("mov $0, %eax\n\t");
            print!("call {}\n\t", f.name);
            for i in 0..n-1 {
                print!("pop %{}\n\t", regs(n - i - 1));
            }
        }
        _ => {
            emit_binop(ast);
        }
    }
}
*/

fn print_ast(ast: Ast) {
    match ast {
        Ast::Op {op, left, right} => {
            print!("({} ", op);
            print_ast(*left);
            print!(" ");
            print_ast(*right);
            print!(")");
        }
        Ast::Int(i) => {
            print!("{}", i);
        }
        Ast::Sym(v) => {
            print!("{}", v.name);
        }
        Ast::Func(f) => {
            print!("{}(", f.name);
            let mut i: usize = 0;
            let n: usize = f.args.len();
            for x in f.args {
                print_ast(x);
                i+=1;

                if i < n {
                    print!(",");
                }
            }
            print!(")");
        }
        _ => {
        }
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let wantast: bool = args.len() > 1 && args[1] == "-a";
    let environment = &mut Env::new();

    // null
    /*
    unsafe {
        VARS.unwrap().push_back(Var::new(String::from("null")));
    }
    */
/*
    if !wantast {
        print!(".text\n\t\
                .global _mymain\n\
                _mymain:\n\t");
    }
*/

    if !wantast {
        print!(".text\n\t\
                .global mymain\n\
                mymain:\n\t");
    }

    loop {
        let ast: Ast = read_expr(environment);
        if ast.is_null() {
            break;
        }
        if wantast {
            print_ast(ast);
        } else {
            //compile(ast);
            emit_expr(ast);
        }
    }
    if !wantast {
        print!("ret\n");
    }
}
