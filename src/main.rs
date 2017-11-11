#![allow(unused_imports)]
#![allow(unused_macros)]
#![allow(unused_variables)]

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

/*
macro_rules! error {
    ($fmt:expr) => (writeln!(&mut io::stderr(), $fmt));
    ($fmt:expr,$($x:tt)*) => (writeln!(&mut io::stderr(), $fmt, $( $x )*));
}
*/

macro_rules! warn {
    ($fmt:expr) => {
        eprintln!($fmt);
    };
    ($fmt:expr,$($x:tt)*) => {
        eprintln!($fmt, $( $x )*);
    }
}

macro_rules! rcc_assert {
    ($x:expr) => {
        if ! $x {
            panic!("Assertion failed: {}", stringify!($x));
        }
    }
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


fn make_ast_binop(op: char, ctype: Ctype, l: Ast, r: Ast) -> Ast {
    Ast::BinOp {op:op, ctype:ctype, left: Box::new(l), right: Box::new(r)}
}


fn make_ast_int(i: u32) -> Ast {
    Ast::Literal { operand: Box::new(Ast::Int(i)) }
}

fn make_ast_char(c: char) -> Ast {
    Ast::Literal { operand: Box::new(Ast::Char(c)) }
}

fn make_ast_string(environment: &mut Env, s: & String) -> Ast {
    Ast::Literal { operand: Box::new(environment.new_str(s)) }
}

fn make_ast_funcall(fname: String, args: Vec<Ast>) -> Ast {
    Ast::Func(Func{name: fname, args: args}, Ctype::Int)
}

//fn make_ast_decl(var: Ast, init: Ast, ctype: Ctype) -> Ast {
//    Ast::Decl {var: Box::new(var), init: Box::new(init), ctype:ctype}
//}
fn make_ast_decl(var: Ast, init: Ast) -> Ast {
    Ast::Decl {var: Box::new(var), init: Box::new(init)}
}

fn make_ast_var(environment: & mut Env, ctype: & Ctype, name: & String) -> Ast {
    return environment.new_var(ctype.clone(), (*name).clone());
}

fn make_ptr_type(ctype: Ctype) -> Ctype {
    Ctype::Ptr(Box::new(ctype))
}

fn is_right_assoc(tok: Token) -> bool {
    match tok {
        Token::Punct(i) => i == '=',
        _ => false
    }
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
        //Token::Int(i) => Ast::Int(i),
        Token::Int(i) => make_ast_int(i),
        Token::Char(c) => make_ast_char(c),
        //Token::Str(ref s) => environment.new_str(s),
        Token::Str(ref s) => make_ast_string(environment, s),
        Token::Punct(c) => panic!("unexpected character: '{}'", c),
        Token::Null => panic!("Don't know how to handle 'Null'")
    }
}

fn result_type_int<'a>(op: char, mut a: &'a Ctype, mut b: &'a Ctype) -> Ctype {
    if a.priority() > b.priority() {
        let tmp = b;
        b = a;
        a = tmp;
    }
    if (*b).is_ptr() {
        if op != '+' && op != '-' {
            return Ctype::Null;
        }
        if ! (*a).is_ptr() {
            warn!("Making a pointer from {}", a.to_string());
            return b.clone();
        }

       return Ctype::Ptr(Box::new(result_type_int(op, &a.ptr(), &b.ptr()))); 
    }


    match *a {
        Ctype::Void => Ctype::Null,
        Ctype::Int | Ctype::Char => Ctype::Int,
        Ctype::Array => {
            result_type_int(op, &make_ptr_type(a.ptr()), b)
        },
        _ => panic!("internal error")
    }
}

fn result_type<'a>(op: char, a: &'a Ast, b: &'a Ast) -> Ctype {
    let ret = result_type_int(op, &a.get_ctype(), &b.get_ctype());
    if ret.is_null() {
        panic!("incompatible operands: {}: {} and {}", op, ast_to_string(a), ast_to_string(b));
    }
    ret
}

fn ensure_lvalue(ast: & Ast) {
    match *ast {
        Ast::Var(_, _) => {},
        _ => panic!("lvalue expected, but got {}", ast_to_string(ast))
    }
}

fn read_unary_expr(environment: &mut Env) -> Ast {
    let tok = read_token(environment);

    if tok.is_punct('&') {
        let operand = read_unary_expr(environment);
        ensure_lvalue(&operand);
        return Ast::Addr {ctype:make_ptr_type(operand.get_ctype()), operand:Box::new(operand)};
    }
    
    if tok.is_punct('*') {
        let operand = read_unary_expr(environment);
        if ! operand.get_ctype().is_ptr() {
            panic!("pointer type expected, but got {}", ast_to_string(&operand))
        }
        return Ast::Deref {ctype:make_ptr_type(operand.get_ctype()), operand:Box::new(operand)};
    }

    unget_token(environment, tok);
    return read_prim(environment);
}

fn read_expr(environment: &mut Env, prec: i32) -> Ast {
    let mut ast = read_unary_expr(environment);

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

                let mut rest = read_expr(environment, prec2 + if is_right_assoc(tok) { 0 } else { 1 });
                let ctype = result_type(c, &mut ast, &mut rest);
                if ctype.is_ptr() && (! ast.get_ctype().is_ptr()) {
                    let tmp = rest;
                    rest = ast;
                    ast = tmp;
                }
                ast = make_ast_binop(c, ctype, ast, rest);
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

fn read_decl(environment: &mut Env) -> Ast {
    let mut ctype = read_token(environment).get_ctype();
    let mut tok: Token;

    loop {
        tok = read_token(environment);
        if ! tok.is_punct('*') {
            break;
        }
        ctype = make_ptr_type(ctype);
    }

    //let name = read_token(environment);
    match tok {
        Token::Ident(ident) => {
            //let var = environment.new_var(ctype.clone(), ident);
            let var = make_ast_var(environment, & ctype.clone(), & ident);
            expect(environment, '=');
            let init = read_expr(environment, 0);
            return make_ast_decl(var, init);
        }
        _ => panic!("Identifier expected, but got {}", tok.to_string())
    }
}

fn read_decl_or_stmt(environment: &mut Env) -> Ast {
    let mut tok = peek_token(environment);
    if tok.is_null() {
        return Ast::Null;
    }

    let r = if tok.is_keyword() {
        read_decl(environment)
    } else {
        read_expr(environment, 0)
    };

    tok = read_token(environment);
    if ! tok.is_punct(';') {
        panic!("Unterminated expression: {}", tok.to_string());
    }

    r
}

fn emit_assign(var: Ast, value: Ast) {
    emit_expr(value);
    if let Ast::Var(v, _) = var {
        print!("mov %rax, -{}(%rbp)\n\t", v.pos * 8);
    } else {
        panic!();
    }
}

fn ctype_shift(ctype: & Ctype) -> i32 {
    match *ctype {
        Ctype::Char => 0,
        Ctype::Int => 2,
        _ => 3
    }
}

fn ctype_size(ctype: & Ctype) -> i32 {
    1 << ctype_shift(ctype)
}

fn emit_pointer_arith<'a>(op: char, left: &'a Ast, right: &'a Ast) {
    rcc_assert!(left.get_ctype().is_ptr());

    emit_expr(left.clone());
    print!("push %rax\n\t");
    emit_expr(right.clone());
    let shift = ctype_shift(& left.get_ctype());
    if shift > 0 {
        print!("sal ${}, %rax\n\t", shift);
    }
    print!("mov %rax, %rbx\n\t");
    print!("pop %rax\n\t");
    print!("add %rbx, %rax\n\t");
}

fn emit_binop(ast: Ast) {
    if let Ast::BinOp {op, ctype, left, right} = ast {
        if op == '=' {
            emit_assign(*left, *right);
            return;
        }

        if ctype.is_ptr() {
            emit_pointer_arith(op, &left, &right);
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
            print!("mov %rax, %rbx\n\t");
            print!("pop %rax\n\t");
            print!("mov $0, %edx\n\t");
            print!("idiv %rbx\n\t");
        } else {
            print!("pop %rbx\n\t");
            print!("{} %rbx, %rax\n\t", opasm);
        }
    }
}

fn emit_expr(ast: Ast) {
    match ast {
        /*
        Ast::Int(i) => {
            print!("mov ${}, %eax\n\t", i);
        }
        Ast::Char(c) => {
            print!("mov ${}, %eax\n\t", c as u32);
        }
        */
        Ast::Literal {operand} => {
            match *operand {
                Ast::Int(i) => {
                    print!("mov ${}, %rax\n\t", i);
                }
                Ast::Char(c) => {
                    print!("mov ${}, %rax\n\t", c as u32);
                }
                Ast::Str(id, _) => {
                    print!("lea .s{}(%rip), %rax\n\t", id);
                }
                _ => {
                    panic!("internal error");
                }
            }
        }
        Ast::Var(v, ctype) => {
            match ctype_size(& ctype) {
                1 => {
                    print!("mov $0, %eax\n\t");
                    print!("mov -{}(%rbp), %al\n\t", v.pos * 8);
                }
                4 => {
                    print!("mov -{}(%rbp), %eax\n\t", v.pos * 8);
                }
                8 => {
                    print!("mov -{}(%rbp), %rax\n\t", v.pos * 8);
                }
                _ => {
                    panic!("internal error");
                }
            }

        }
        Ast::Func(f, ctype) => {
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
        Ast::Addr {ctype, operand} => {
            match *operand {
                Ast::Var(ref v, ref ctype) => {
                    print!("lea -{}(%rbp), %rax\n\t", v.pos * 8);
                }
                _ => {
                    panic!("Failed: Not Addr");
                }
            }
        }
        Ast::Deref {ctype, operand} => {
            match operand.get_ctype() {
                Ctype::Ptr(ref ct) => {
                    emit_expr(*operand);
                    let reg = match ctype_size(ct) {
                        1 => "%bl",
                        4 => "%ebx",
                        8 => "%rbx",
                        _ => panic!("internal error")
                    };
                    print!("mov $0, %ebx\n\t");
                    print!("mov (%rax), {}\n\t", reg);
                    print!("mov %rbx, %rax\n\t");
                }
                _ => {
                    panic!("Failed: Not Deref");
                }
            }
        }
        _ => {
            emit_binop(ast);
        }
    }
}

fn quote(target: &String) -> String {
    let mut s = String::new();
    for c in target.chars() {
        if c == '\"' || c == '\\' {
            s += "\\";
        }
        s += format!("{}", c).as_str();
    }
    return s
}

fn ast_to_string_int(ast: & Ast, mut buf: String) -> String {
    match *ast {
        Ast::BinOp {ref op, ref ctype, ref left, ref right} => {
            buf += format!("({} {} {})", op, ast_to_string(left), ast_to_string(right)).as_str();
        }
        Ast::Literal {ref operand} => {
            match **operand {
                Ast::Int(ref i) => {
                    buf += format!("{}", i).as_str();
                }
                Ast::Char(ref c) => {
                    buf += format!("'{}'", c).as_str();
                }
                Ast::Str(ref i, ref s) => {
                    buf += format!("\"{}\"", quote(&s)).as_str();
                }
                _ => panic!("internal error")
            }
        }
        Ast::Var(ref v, _) => {
            buf += v.name.as_str();
        }
        Ast::Func(ref f, ref ctype) => {
            buf += format!("{}(", f.name).as_str();
            let mut i: usize = 0;
            let n: usize = f.args.len();
            for x in f.get_args() {
                buf += ast_to_string(&x).as_str();
                i+=1;

                if i < n {
                    buf += ",";
                }
            }
            buf += ")";
        }
        Ast::Decl {ref var, ref init} => {
            if let Ast::Var(Var {ref name, ref pos}, ref ctype) = **var {
                buf += format!("(decl {} {} {})", ctype.to_string(), name, ast_to_string(init)).as_str();
            } else {
                panic!("Expected Ast::Var.");
            }
        }
        Ast::Addr {ref ctype, ref operand} => {
            buf += format!("(& {})", ast_to_string(operand)).as_str();
        }
        Ast::Deref {ref ctype, ref operand} => {
            buf += format!("(* {})", ast_to_string(operand)).as_str();
        }
        _ => {
        }
    };

    return buf;
}

fn ast_to_string(ast: & Ast) -> String {
    ast_to_string_int(ast, String::new())
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
                print!(".string \"{}\"\n", quote(s));
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
        print!("push %rbp\n\t");
        print!("mov %rsp, %rbp\n\t");

        if environment.len_vars() > 0 {
            print!("sub ${}, %rsp\n\t", environment.len_vars() * 8);
        }
    }

    for a in asts {
        if wantast {
            print!("{}", ast_to_string(&a));
        } else {
            emit_expr(a);
        }
    }
    if !wantast {
        print!("leave\n\t");
        print!("ret\n");
    }
}
