// main

#[macro_use]
extern crate lazy_static;

mod gen_x86;
mod gen_ir;
mod parse;
mod regalloc;
mod sema;
mod token;
mod util;

use crate::gen_x86::*;
use crate::gen_ir::*;
use crate::parse::*;
use crate::regalloc::*;
use crate::sema::*;
use crate::token::*;
use std::env;
use std::sync::Mutex;

fn main() {
    let argv: Vec<String> = env::args().collect();
    if argv.len() != 2 {
        println!("Usage: 9cc <code>");
        return;
    }

    // Token -> Node -> IR -> asm
    // token -> parse -> sema -> gen_ir -> regalloc -> gen_x86

    // Tokenize and parse.
    //eprintln!("start");
    let tokens = tokenize(&argv[1]);
    //eprintln!("tokenize");
    //let nodes_tmp = parse(&tokens);
    let mut nodes = parse(&tokens);
    //eprintln!("parse");
    sema(&mut nodes);
    let mut fns = gen_ir(nodes);
    //eprintln!("gen_ir");

    alloc_regs(&mut fns);
    //eprintln!("alloc_regs");

    gen_x86(&fns);
    return;
}
