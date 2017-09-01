use std::env;
use std::fmt;
use std::io;
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

fn write_debug(bytes: &[u8]) {
    /*
    let path = Path::new("./debug.log");
    let display = path.display();

    let mut file = match File::create(&path) {
        Err(why) => panic!("couldn't open {}: {}", display, Error::description(&why)),
        Ok(file) => file,
    };

    match file.write_all(bytes) {
        Err(why) => panic!("couldn't read {}: {}", display, Error::description(&why)),
        //Ok(_) => print!("successfully wrote to {}", display),
        Ok(_) => (),
    }
    */
}

struct Env {
    buffer: Buffer,
    vars: LinkedList<Var>,
}

impl Env {
    fn new() -> Env {
        //let mut vs = LinkedList::new();
        let mut ret = Env {
            buffer: Buffer::new(),
            //vars:  VARS : LinkedList<Var> = LinkedList::new();
            vars: LinkedList::new()
        };
        //ret.vars.push_back(ret.new_var(String::from("null")));
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

    //fn has_var(&mut self, name: String) -> bool {
    fn find_var(&mut self, name: String) -> Var {
        for x in self.vars.iter() {
            if x.name == name {
                return x.clone()
            }
        }
        self.null()
    }
}


//static mut V-ARS : LinkedList<Var> = LinkedList::new();
//static mut V-ARS : Option<LinkedList<Var>> = None;
//static mut V-ARS : LinkedList<Var>; // = LinkedList::new();

//fn find_var(name: String) -> Option<&Var> {
/*
#[inline]
//fn find_var<'a>(name: String) -> &'a Var {
fn find_var(name: String) -> Var {
    //let mut iter = V-ARS.iter();

    unsafe {
        for x in VARS.unwrap() {
            if x.name == name {
                return x.clone();
            }
        }
        return VARS.unwrap().front().unwrap().clone();
    }
}
*/

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

    fn print(&self) {
        match *self {
            Ast::Op {op:a, left:ref b, right:ref c} => print!("op"),
            Ast::Int(i) => print!("int"),
            Ast::Sym(ref v) => print!("var {}", v.name),
            Ast::Null => print!("null")
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

/* https://github.com/rui314/8cc/commit/6384c521a214484b31cd5c76ccebe1df3269a54e */
fn read_symbol(environment: &mut Env) -> Ast {
    // let mut is_escape = false;
    let mut sym = String::new(); // no limit symbol length
    // let mut i = 1;

    loop {
        let c = environment.buffer.getc();

        //print!("c={}\n", c);
        if !c.is_alphabetic() {
            environment.buffer.ungetc();
            break;
        }
        sym.push(c);
    }

    //print!("sym={}\n", sym);
    let v = environment.find_var(sym.clone());
    //return make_ast_sym(*v.is_null() ?  make_var(sym) : *v);
    if v.is_null() {
        //return make_ast_sym(make_var(sym));
        return make_ast_sym(environment.new_var(sym));
    } else {
        return make_ast_sym(v.clone());
    }
}

fn read_prim(environment: &mut Env) -> Ast {
    if environment.buffer.is_end() {
        return Ast::Null;
    }

    let c = environment.buffer.getc();
    //print!("[read_prim] c={}\n", c);
    if c.is_digit(10) {
        return read_number(environment, c.to_digit(10).unwrap());
    } else if c.is_alphabetic() {
        environment.buffer.ungetc();
        return read_symbol(environment);
    }
    panic!("Don't know how to handle '{}'",c);
}

// TODO 若干異なるがうまくいくか?
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
    let environment = &mut Env::new();

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
