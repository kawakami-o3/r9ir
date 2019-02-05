use std::env;

fn main() {
    let argv: Vec<String> = env::args().collect();
    if argv.len() != 2 {
        println!("Usage: 9cc <code>");
        return;
    }

    println!(".intel_syntax noprefix");
    println!(".global main");
    println!("main:");


    let mut tokens = Vec::new();
    let mut t = String::new();
    for c in argv[1].chars() {
        if c.is_digit(10) {
            t.push(c);
            continue;
        }
        if c == '+' || c == '-' {
            tokens.push(t.clone());
            t = String::new();
            tokens.push(format!("{}", c));
        }
    }
    tokens.push(t);

    println!("  mov rax, {}", tokens[0]);
    let mut idx = 1;
    while idx < tokens.len() {
        if tokens[idx] == "+" {
            println!("  add rax, {}", tokens[idx+1]);
            idx += 2;
            continue;
        }
        if tokens[idx] == "-" {
            println!("  sub rax, {}", tokens[idx+1]);
            idx += 2;
            continue;
        }
        panic!("unexpected token {}", tokens[idx]);
    }

    println!("  ret");
    return;
}
