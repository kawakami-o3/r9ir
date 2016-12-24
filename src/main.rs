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

fn read_number(buf: &mut Buffer, mut n: u32) -> u32 {
    while buf.can_read() {
        let c = buf.getc();

        if false == c.is_digit(10) {
            buf.ungetc();
            break;
        }
        n = n * 10 + c.to_digit(10).unwrap();
    }
    return n;
}

fn compile_expr2(buf: &mut Buffer) {
    loop {
        skip_space(buf);
        if false == buf.can_read() {
            print!("ret\n");
            return;
        }

        let c = buf.getc();
        let op = match c {
            '+' => "add",
            '-' => "sub",
            _ => { let _ = error!("Operator expected, but got '{}'", c); panic!()}
        };

        skip_space(buf);
        let n = buf.getc();
        if false == n.is_digit(10) {
            let _ = error!("Number expected, but got '{}'", n);
        }
        print!("{} ${}, %rax\n\t", op, read_number(buf, n.to_digit(10).unwrap()));
    }
}

fn compile_expr(buf: &mut Buffer, mut n: u32) {
    n = read_number(buf, n);
    println!("\t.text\n\t\
              .global _intfn\n\
              _intfn:\n\t\
              mov ${}, %rax\n\t", n);
    compile_expr2(buf);
}


fn compile_string(buf: &mut Buffer) {
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

    println!("\t.text\n\t\
             .mydata:\n\t\
            .string \"{}\"\n\t\
            .text\n\t\
            .global _stringfn\n\
            _stringfn:\n\t\
            lea .mydata(%rip), %rax\n\t\
            ret", s);
}

fn compile() {
    let buf: &mut Buffer = &mut Buffer::new();
    let c = buf.getc();

    if c.is_digit(10) {
        return compile_expr(buf, c.to_digit(10).unwrap());
    } else if c == '"' {
        return compile_string(buf);
    }
    let _ = error!("error");
}

fn main() {
    compile();
}
