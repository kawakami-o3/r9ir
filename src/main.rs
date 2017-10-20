#![allow(unused_imports)]

mod string;
mod rcc_env;

use rcc_env::*;
use std::env;
use std::fmt;
use std::io;
use std::cmp;
use std::io::Write;
use std::io::Read;
use std::collections::LinkedList;
use std::process::Command;

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
    if cfg!(target_os = "windows") {
        match i {
            0 => String::from("%rcx"),
            1 => String::from("%rdx"),
            2 => String::from("%r8"),
            3 => String::from("%r9"),
            _ => format!("{}(%rsp)", 16 * i)
        }
    } else {
        match i {
            0 => String::from("%rdi"),
            1 => String::from("%rsi"),
            2 => String::from("%rdx"),
            3 => String::from("%rcx"),
            4 => String::from("%r8"),
            5 => String::from("%r9"),
            _ => format!("{}(%rsp)", 8 * i)
        }
    }
}

fn fname(s: String) -> String {
    if cfg!(target_os = "macos") {
        format!("_{}", s)
    } else {
        s
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

fn make_ast_char(c: char) -> Ast {
    Ast::Char(c)
}

fn make_ast_funcall(fname: String, args: Vec<Ast>) -> Ast {
    Ast::Func(Func{name: fname, args: args})
}

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
    v
}

fn read_string(environment: &mut Env) -> Ast {
    let mut buf = String::new();
    loop {
        if environment.buffer.is_end() {
            panic!("Unterminated string");
        }
        let c = environment.buffer.getc();
        if c == '"' {
            break;
        }

        if c == '\\' {
            if environment.buffer.is_end() {
                panic!("Unterminated \\");
            }
            buf.push(c);
            buf.push(environment.buffer.getc());
        } else {
            buf.push(c);
        }
    }
/*
    let id = environment.strings.len();
    environment.add_str(&buf);
    make_ast_str(id, buf)
    */
    environment.new_str(&buf)
}

fn read_char(environment: &mut Env) -> Ast {
    fn unterminated() {
        panic!("Unterminated char");
    }

    if environment.buffer.is_end() {
        unterminated();
    }

    let chr = match environment.buffer.getc() {
        '\\' => {
            if environment.buffer.is_end() {
                unterminated();
            }
            environment.buffer.getc()
        }
        c => { c }
    };

    if environment.buffer.is_end() {
        unterminated();
    }
    if environment.buffer.getc() != '\'' {
        panic!("Malformed char constant");
    }
    make_ast_char(chr)
}

fn read_prim(environment: &mut Env) -> Ast {
    if environment.buffer.is_end() {
        return Ast::Null;
    }

    let c = environment.buffer.getc();
    if c.is_digit(10) {
        return read_number(environment, c.to_digit(10).unwrap());
    } else if c == '"' {
        return read_string(environment);
    } else if c == '\'' {
        return read_char(environment);
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
            if let Ast::Var(v) = *left {
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
        Ast::Char(c) => {
            print!("mov ${}, %eax\n\t", c as u32);
        }
        Ast::Var(v) => {
            print!("mov -{}(%rbp), %eax\n\t", v.pos * 4);
        }
        Ast::Str(id, _) => {
            print!("lea .s{}(%rip), %rax\n\t", id);
        }
        Ast::Func(f) => {
            // not working on windows with more than 4 arguments.
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
            print!("call {}\n\t", fname(f.name));

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

fn print_quote(s: &String) {
    for c in s.chars() {
        if c == '\"' || c == '\\' {
            print!("\\");
        }
        print!("{}", c);
    }
}

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
        Ast::Char(c) => {
            print!("'{}'", c);
        }
        Ast::Var(v) => {
            print!("{}", v.name);
        }
        Ast::Str(_, s) => {
            print!("\"");
            print_quote(&s);
            print!("\"");
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

fn emit_data_section(environment: & Env) {
    if environment.strings.len() == 0 {
        return;
    }

    print!("\t.data\n");
    for s in &environment.strings {
        match s {
            &Ast::Str(id, ref s) => {
                print!(".s{}:\n\t", id);
                print!(".string \"");
                print_quote(s);
                print!("\"\n");
            }
            _ => {}
        }
    }
    print!("\t");
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let wantast: bool = args.len() > 1 && args[1] == "-a";
    let environment = &mut rcc_env::Env::new();

    let mut asts: Vec<rcc_env::Ast> = vec![];
    loop {
        let ast: rcc_env::Ast = read_expr(environment);
        if ast.is_null() {
            break;
        }
        asts.push(ast);
    }

    if !wantast {
        emit_data_section(environment);

        let main = fname(String::from("mymain"));
        print!(".text\n\t");
        print!(".global {}\n", main);
        print!("{}:\n\t", main);
    }

    for a in asts {
        if wantast {
            print_ast(a);
        } else {
            //compile(ast);
            emit_expr(a);
        }
    }
    if !wantast {
        print!("ret\n");
    }
}
