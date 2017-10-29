#![allow(unused_imports)]
#![allow(unused_macros)]

mod rcc_env;
mod lex;

use rcc_env::*;
use lex::*;

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

fn make_ast_op(op: char, l: Ast, r: Ast) -> Ast {
    Ast::Op {op:op, left: Box::new(l), right: Box::new(r)}
}

fn make_ast_char(c: char) -> Ast {
    Ast::Char(c)
}

fn make_ast_funcall(fname: String, args: Vec<Ast>) -> Ast {
    Ast::Func(Func{name: fname, args: args})
}

fn make_ast_decl(var: Ast, init: Ast) -> Ast {
    Ast::Decl {var: Box::new(var), init: Box::new(init)}
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


fn read_func_args(environment: &mut Env, fname: String) -> Ast {
    let mut args: Vec<Ast> = vec![];
    loop {
        let mut tok = read_token(environment);
        if tok.is_punct(')') {
            break;
        }
        unget_token(environment, tok);

        args.push(read_expr(environment, 0));

        tok = read_token(environment);
        if tok.is_punct(')') {
            break;
        }
        if ! tok.is_punct(',') {
            panic!("Unexpected token: '{}'", tok.to_string());
        }
    }

    if args.len() > MAX_ARGS {
        panic!("Too many arguments: {}", fname);
    }

    make_ast_funcall(fname, args)
}

fn read_ident_or_func(environment: &mut Env, name: String) -> Ast {
    let tok = read_token(environment);
    if tok.is_punct('(') {
        return read_func_args(environment, name);
    }

    unget_token(environment, tok);

    let v = environment.find_var(&name);
    if v.is_null() {
        //v = environment.new_var(name);
        panic!("Undefined variable: {}", name)
    }
    v
}

fn read_prim(environment: &mut Env) -> Ast {
    let tok = read_token(environment);
    if tok.is_null() {
        return Ast::Null;
    }

    match tok {
        Token::Ident(c) => read_ident_or_func(environment, c),
        Token::Int(i) => Ast::Int(i),
        Token::Char(c) => make_ast_char(c),
        Token::Str(ref s) => environment.new_str(s),
        Token::Punct(c) => panic!("unexpected character: '{}'", c),
        Token::Null => panic!("Don't know how to handle 'Null'")
    }
}

fn ensure_lvalue(ast: & Ast) {
    match *ast {
        Ast::Var(_) => {},
        _ => panic!("variable expected")
    }
}

fn read_expr(environment: &mut Env, prec: i32) -> Ast {
    let mut ast = read_prim(environment);

    loop {
        let tok = read_token(environment);

        match tok {
            Token::Punct(c) => {
                let prec2 = priority(c);
                if prec2 < 0 || prec2 < prec {
                    unget_token(environment, tok);
                    return ast;
                }

                if c == '=' {
                    ensure_lvalue(&ast);
                }

                ast = make_ast_op(c, ast, read_expr(environment, prec2 + 1));
            },
            _ => {
                unget_token(environment, tok);
                return ast;
            }
        }
    }
}

fn expect(environment: &mut Env, punct: char) {
    let tok = read_token(environment);
    if ! tok.is_punct(punct) {
        panic!("Unterminated expression: {}", tok.to_string());
    }
}

fn get_ctype(tok: Token) -> rcc_env::CType {
    match tok {
        Token::Ident(ident) => match ident.as_str() {
            "int" => CType::Int,
            "char" => CType::Char,
            "string" => CType::Str,
            _ => CType::Void
        }
        _ => CType::Void
    }
}

fn read_decl(environment: &mut Env) -> Ast {
    let ctype = get_ctype(read_token(environment));
    let name = read_token(environment);
    match name {
        Token::Ident(ident) => {
            let var = environment.new_var(ctype, ident);
            expect(environment, '=');
            let init = read_expr(environment, 0);
            return make_ast_decl(var, init);
        }
        _ => panic!("Identifier expected, but got {}", name.to_string())
    }
}

fn read_decl_or_stmt(environment: &mut Env) -> Ast {
    let mut tok = peek_token(environment);
    if tok.is_null() {
        return Ast::Null;
    }
    
    let r = match tok.is_keyword() {
        true => read_decl(environment),
        false => read_expr(environment, 0)
    };

    tok = read_token(environment);
    if ! tok.is_punct(';') {
        panic!("Unterminated expression: {}", tok.to_string());
    }

    r
}

fn emit_assign(var: Ast, value: Ast) {
    emit_expr(value);
    if let Ast::Var(v) = var {
        print!("mov %eax, -{}(%rbp)\n\t", v.pos * 4);
    } else {
        panic!();
    }
}

fn emit_binop(ast: Ast) {
    if let Ast::Op {op, left, right} = ast {
        if op == '=' {
            emit_assign(*left, *right);
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

            print!("pushq %rbp\n\t");
            print!("movq %rsp, %rbp\n\t");

            print!("subq ${}, %rsp\n\t", shift);

            for x in f.args {
                emit_expr(x);
                print!("push %rax\n\t");
            }
            for i in 0..n {
                print!("pop {}\n\t", regs(n - i - 1));
            }
            print!("mov $0, %eax\n\t");
            print!("call {}\n\t", fname(f.name));

            print!("addq ${}, %rsp\n\t", shift);

            print!("popq %rbp\n\t");
        }
        Ast::Decl {var, init} => {
            emit_assign(*var, *init)
        }
        _ => {
            emit_binop(ast);
        }
    }
}

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
        Ast::Decl {var, init} => {
            if let Ast::Var(Var {ref name, ref ctype, ref pos}) = *var {
                ignore_warn(*pos);
                print!("(decl {} {} ", ctype.to_string(), name);
                print_ast(*init);
                print!(")");
            } else {
                panic!();
            }
        }
        _ => {
        }
    }
}

fn ignore_warn(i: usize) -> usize {
    i + 1
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
        //let ast: rcc_env::Ast = read_expr(environment);
        let ast: rcc_env::Ast = read_decl_or_stmt(environment);
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
