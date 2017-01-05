use std::env;
use std::fmt;
use std::io;
use std::io::Write;
use std::io::Read;

macro_rules! error {
    ($fmt:expr) => (writeln!(&mut io::stderr(), $fmt));
    ($fmt:expr,$($x:tt)*) => (writeln!(&mut io::stderr(), $fmt, $( $x )*));
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


enum Ast {
    OpPlus {left: Box<Ast>, right: Box<Ast>},
    OpMinus {left: Box<Ast>, right: Box<Ast>},
    Int(u32),
    Str(String)
}

fn new_ast_op(op: char, l: Ast, r: Ast) -> Ast {
    match op {
        '+' => Ast::OpPlus {left: Box::new(l), right: Box::new(r)},
        '-' => Ast::OpMinus {left: Box::new(l), right: Box::new(r)},
        _ => {
            panic!();
        }
    }
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

fn read_number(buf: &mut Buffer, mut n: u32) -> Ast {
    while buf.can_read() {
        let c = buf.getc();

        if false == c.is_digit(10) {
            buf.ungetc();
            break;
        }
        n = n * 10 + c.to_digit(10).unwrap();
    }
    return Ast::Int(n);
}

fn read_string(buf: &mut Buffer) -> Ast {
    let mut is_escape = false;
    let mut s = String::new();

    loop {
        if false == buf.can_read() {
            let _ = error!("Unterminated string");
            panic!();
        }

        let c = buf.getc();
        if is_escape {
            s.push(c);
            is_escape = false;
            continue;
        }
        if c == '"' {
            break;
        }
        if c == '\\' {
            is_escape = true;
            continue;
        }
        s.push(c);
    }

    return Ast::Str(s);
}


fn read_prim(buf: &mut Buffer) -> Ast {
    if false == buf.can_read() {
        panic!("Unexpected EOF");
    }

    let c = buf.getc();
    if c.is_digit(10) {
        return read_number(buf, c.to_digit(10).unwrap());
    } else if c == '"' {
        return read_string(buf);
    }
    panic!("Don't know how to handle '{}'",c);
}

fn read_expr2(buf: &mut Buffer, left: Ast) -> Ast {
    skip_space(buf);
    if false == buf.can_read() {
        return left;
    }

    let op = buf.getc();
    skip_space(buf);
    let right: Ast = read_prim(buf);

    return read_expr2(buf, new_ast_op(op, left, right));
}

fn read_expr() -> Ast {
    let buf: &mut Buffer = &mut Buffer::new();

    let left : Ast = read_prim(buf);
    return read_expr2(buf, left);
}

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

fn emit_binop(ast: Ast) {
    match ast {
        Ast::OpPlus {left, right} => {
            emit_intexpr(*left);
            print!("mov %eax, %ebx\n\t");
            emit_intexpr(*right);
            print!("add %ebx, %eax\n\t");
        }
        Ast::OpMinus {left, right} => {
            emit_intexpr(*left);
            print!("mov %eax, %ebx\n\t");
            emit_intexpr(*right);
            print!("sub %ebx, %eax\n\t");
        }
        _ => {
            panic!("invalid operand");
        }
    }
}

fn ensure_intexpr(ast: &Ast) {
    match *ast {
        Ast::OpPlus {ref left, ref right} => { }
        Ast::OpMinus {ref left, ref right} => { }
        Ast::Int(i) => { }
        _ => {
            panic!("integer or binary operator expected");
        }
    }
}

fn emit_intexpr(ast: Ast) {
    ensure_intexpr(&ast);
    match ast {
        Ast::Int(i) => {
            print!("mov ${}, %eax\n\t", i);
        }
        _ => {
            emit_binop(ast);
        }
    }
}

fn print_ast(ast: Ast) {
    match ast {
        Ast::OpPlus {left, right} => {
            print!("(+ ");
            print_ast(*left);
            print!(" ");
            print_ast(*right);
            print!(")");
        }
        Ast::OpMinus {left, right} => {
            print!("(- ");
            print_ast(*left);
            print!(" ");
            print_ast(*right);
            print!(")");
        }
        Ast::Int(i) => {
            print!("{}", i);
        }
        Ast::Str(s) => {
            print_quote(s);
        }
    }
}

fn compile(ast: Ast) {
    match ast {
        Ast::Str(s) => {
            emit_string(&Ast::Str(s));
        }
        _ => {
            print!(".text\n\t\
                    .global _intfn\n\
                    _intfn:\n\t");
            emit_intexpr(ast);
            print!("ret\n");
        }
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let ast: Ast = read_expr();

    if args.len() > 1 && args[1] == "-a" {
        print_ast(ast);
    } else {
        compile(ast);
    }
}
