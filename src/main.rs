use std::env;
use std::fmt;
use std::io;
use std::io::Write;
use std::io::Read;
use std::collections::LinkedList;

macro_rules! error {
    ($fmt:expr) => (writeln!(&mut io::stderr(), $fmt));
    ($fmt:expr,$($x:tt)*) => (writeln!(&mut io::stderr(), $fmt, $( $x )*));
}

struct Env {
    buffer: Buffer,
    vars: LinkedList<Var>,
}

impl Env {
    fn new() -> Env {
        let mut vs = LinkedList::new();
        vs.push_back(Var::new(String::from("null")));
        Env {
            buffer: Buffer::new(),
            //vars:  VARS : LinkedList<Var> = LinkedList::new();
            vars: vs,
        }
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

    fn getc(&mut self) -> char {
        self.idx += 1;
        return self.chars[self.idx - 1];
    }

    fn ungetc(&mut self) {
        self.idx -= 1;
    }

    fn can_read(&mut self) -> bool {
        return self.chars.len() > self.idx;
    }

    fn is_end(&mut self) -> bool {
        return !self.can_read();
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
    fn new(name:String) -> Var {
        unsafe {
            Var {
                name: name,
                pos: VARS.unwrap().len(), // + 1,
            }
        }
    }

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

enum Ast {
    Op {op:char, left: Box<Ast>, right: Box<Ast>},
    Int(u32),
    //Str(String),
    Sym(Var),
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

/*
fn print_quote(s: String) {
    for c in s.chars() {
        if c == '\"' || c == '\\' {
            print!("\\");
        }
        print!("{}", c);
    }
}

fn emit_string(ast: &Ast) {
    print!("\t.data\n\
           .mydata:\n\t\
           .string \"");
    match *ast {
        Ast::Str(ref s) => {
            print_quote(s.clone())
        }
        _ => {
            panic!()
        }
    }
    print!("\"\n\t\
            .text\n\t\
            .global _stringfn\n\
            _stringfn:\n\t\
            lea .mydata(%rip), %rax\n\t\
            ret\n")
}
*/

// ***** the following content is compatible. *****


fn make_ast_op(op: char, l: Ast, r: Ast) -> Ast {
    match op {
        '+' | '-' | '*' | '/' => Ast::Op {op:op, left: Box::new(l), right: Box::new(r)},
        _ => {
            panic!();
        }
    }
}

fn make_ast_int(var: u32) -> Ast {
    Ast::Int(var)
}

fn make_ast_sym(var: Var) -> Ast {
    Ast::Sym(var)
}

//static mut VARS : LinkedList<Var> = LinkedList::new();
static mut VARS : Option<LinkedList<Var>> = None;
//static mut VARS : LinkedList<Var>; // = LinkedList::new();

//fn find_var(name: String) -> Option<&Var> {
#[inline]
//fn find_var<'a>(name: String) -> &'a Var {
fn find_var(name: String) -> Var {
    //let mut iter = VARS.iter();

    unsafe {
        for x in VARS.unwrap() {
            if x.name == name {
                return x.clone();
            }
        }
        return VARS.unwrap().front().unwrap().clone();
    }
}

fn make_var(name: String) -> Var {
    return Var::new(name);
}

fn skip_space(buf: &mut Buffer) {
    while buf.can_read() {
        let c = buf.getc();
        if c.is_whitespace() {
            continue;
        }

        buf.ungetc();
        return;
    }
}

fn priority(op: char) -> i32 {
    match op {
        '=' => 1,
        '+' | '-' => 2,
        '*' | '/' => 3,
        _ => { panic!() }
    }
}

fn read_number(buf: &mut Buffer, mut n: u32) -> Ast {
    loop {
        let c = buf.getc();
        if !c.is_digit(10) {
            buf.ungetc();
            return Ast::Int(n);
        }
        n = n * 10 + c.to_digit(10).unwrap();
    }
}

/* https://github.com/rui314/8cc/commit/6384c521a214484b31cd5c76ccebe1df3269a54e */
fn read_symbol(buf: &mut Buffer) -> Ast {
    let mut is_escape = false;
    let mut sym = String::new(); // no limit symbol length
    let mut i = 1;

    loop {

        let c = buf.getc();
        if !c.is_alphabetic() {
            buf.ungetc();
            break;
        }
        sym.push(c);
    }

    let v = find_var(sym.clone());
    //return make_ast_sym(*v.is_null() ?  make_var(sym) : *v);
    if v.is_null() {
        return make_ast_sym(make_var(sym));
    } else {
        return make_ast_sym(v.clone());
    }
}

fn read_prim(buf: &mut Buffer) -> Ast {
    if buf.is_end() {
        return Ast::Null;
    }

    let c = buf.getc();
    if c.is_digit(10) {
        return read_number(buf, c.to_digit(10).unwrap());
    } else if c.is_alphabetic() {
        return read_symbol(buf);
    }
    panic!("Don't know how to handle '{}'",c);
}

// TODO 若干異なるがうまくいくか?
fn read_expr2(buf: &mut Buffer, prec: i32) -> Ast {
    let mut ast = read_prim(buf);

    loop {
        skip_space(buf);
        if buf.is_end() {
            return ast;
        }
        let c = buf.getc();
        let prec2 = priority(c);
        if prec2 < prec {
            buf.ungetc();
            return ast
        }

        skip_space(buf);
        ast = make_ast_op(c, ast, read_expr2(buf, prec2 + 1));
    }

    return ast;
}

fn read_expr() -> Ast {
    let buf: &mut Buffer = &mut Buffer::new();

    let r: Ast = read_expr2(buf, 0);
    if r.is_null() {
        return Ast::Null;
    }

    skip_space(buf);
    let c: char = buf.getc();
    if c != ';' {
        error!("Unterminated expression");
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
                error!("Symbol expected");
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
        _ => {
            emit_binop(ast);
        }
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
        Ast::Sym(v) => {
            print!("{}", v.name);
        }
        _ => {
        }
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let wantast: bool = args.len() > 1 && args[1] == "-a";

    // null
    /*
    unsafe {
        VARS.unwrap().push_back(Var::new(String::from("null")));
    }
    */

    if !wantast {
        print!(".text\n\t\
                .global _mymain\n\
                _mymain:\n\t");
    }
    loop {
        let ast: Ast = read_expr();
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
