// main

#[macro_use]
extern crate lazy_static;

mod codegen;
mod ir;
mod parse;
mod regalloc;
mod token;

use crate::codegen::*;
use crate::ir::*;
use crate::parse::*;
use crate::regalloc::*;
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
    // token -> parse -> ir -> regalloc -> codegen

    // Tokenize and parse.
    let tokens = tokenize(&argv[1]);
    let mut fns = gen_ir(parse(&tokens));

    alloc_regs(&mut fns);

    gen_x86(&fns);
    return;
}
