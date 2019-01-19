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
    match i32::from_str_radix(argv[1].as_str(), 10) {
        Ok(i) => {
            println!("  mov rax, {}", i);
        }
        Err(_) => {}
    }
    println!("  ret");
    return;
}
