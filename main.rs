use std::io;
use std::io::Write;
use std::io::Read;

macro_rules! error {
    ($fmt:expr) => (writeln!(&mut std::io::stderr(), $fmt));
    ($fmt:expr,$($x:tt)*) => (writeln!(&mut std::io::stderr(), $fmt, $( $x )*));
}
/*
fn compile_number(mut n: u32) {
    for i in io::stdin().bytes() {
        let b = i.unwrap();
        let c = char::from(b);
        if c.is_whitespace() {
            break;
        }
        if false == c.is_numeric() {
            panic!();
        }
        n = n * 10 + c.to_digit(10).unwrap();
    }

    println!("\t.text\n\t\
            .global _intfn\n\
            _intfn:\n\t\
            mov ${}, %rax\n\t\
            ret", n);
}

fn compile_string() {
    let mut is_escape = false;
    let mut buf = String::new();

    for i in io::stdin().bytes() {
        let b = i.unwrap();
        let c = char::from(b);

        if false == is_escape {
            if c == '"' {
                break;
            }

            if c == '\\' {
                is_escape = true;
                continue;
            }
        }

        buf.push(c);
    }

    buf.push('\0');

    println!("\t.text\n\t\
            .mydata:\n\t\
            .string \"{}\"\n\t\
            .text\n\t\
            .global _stringfn\n\
            _stringfn:\n\t\
            lea .mydata(%rip), %rax\n\t\
            ret", buf);
}
*/


fn compile_number(mut n: u32) {
    for i in io::stdin().bytes() {
        let b = i.unwrap();
        let c = char::from(b);
        if c.is_whitespace() {
            break;
        }
        if false == c.is_numeric() {
            panic!();
        }
        n = n * 10 + c.to_digit(10).unwrap();
    }

    println!("\t.text\n\t\
            .global intfn\n\
            intfn:\n\t\
            mov ${}, %rax\n\t\
            ret", n);
}

fn compile_string() {
    let mut is_escape = false;
    let mut buf = String::new();

    for i in io::stdin().bytes() {
        let b = i.unwrap();
        let c = char::from(b);

        if false == is_escape {
            if c == '"' {
                break;
            }

            if c == '\\' {
                is_escape = true;
                continue;
            }
        }

        buf.push(c);
    }

    buf.push('\0');

    println!("\t.text\n\t\
            .mydata:\n\t\
            .string \"{}\"\n\t\
            .text\n\t\
            .global stringfn\n\
            stringfn:\n\t\
            lea .mydata(%rip), %rax\n\t\
            ret", buf);
}



fn compile() {
    let c =  char::from(io::stdin().bytes().next().and_then(|r| r.ok()).unwrap());

    if c.is_digit(10) {
        return compile_number(c.to_digit(10).unwrap());
    } else if c == '"' {
        return compile_string();
    }
    let _ = error!("error");
}

fn main() {
    compile();
}

/*
fn main() {
    let mut input = String::new();

    io::stdin().read_line(&mut input)
        .expect("Failed to read line");

    let val = input.trim();

    error!("{}{}\n", val, val);

    println!("\t.text\n\t\
            .global _mymain\n\
            _mymain:\n\t\
            mov ${}, %eax\n\t\
            ret\n", val);
}
*/
