// main

#[macro_use]
extern crate lazy_static;

mod gen_x86;
mod gen_ir;
mod irdump;
mod parse;
mod regalloc;
mod sema;
mod token;
mod util;

use crate::gen_x86::*;
use crate::gen_ir::*;
use crate::irdump::*;
use crate::parse::*;
use crate::regalloc::*;
use crate::sema::*;
use crate::token::*;
use std::env;
use std::fs;
use std::sync::Mutex;

fn read_file(filename: String) -> String {
    match fs::read_to_string(filename) {
        Ok(content) => {
            return content;
        }
        Err(_) => {
            panic!();
        }
    }
}

fn main() {
    let argv: Vec<String> = env::args().collect();
    let mut dump_ir1 = false;
    let mut dump_ir2 = false;
    let filename: String;

    if argv.len() == 3 && argv[1] == "-dump-ir1" {
        dump_ir1 = true;
        filename = argv[2].clone();
    } else if argv.len() == 3 && argv[1] == "-dump-ir2" {
        dump_ir2 = true;
        filename = argv[2].clone();
    } else {
        if argv.len() != 2 {
            panic!("Usage: rcc [-test] [-dump-ir1] [-dump-ir2] <file>");
        }
        filename = argv[1].clone();
    }

    // Token -> Node -> IR -> asm
    // token -> parse -> sema -> gen_ir(irdump) -> regalloc -> gen_x86

    // Tokenize and parse.
    let input = read_file(filename);
    let tokens = tokenize(&input);
    let mut nodes = parse(&tokens);
    let globals = sema(&mut nodes);
    let mut fns = gen_ir(nodes);

    if dump_ir1 {
        dump_ir(fns.clone());
    }

    alloc_regs(&mut fns);

    if dump_ir2 {
        dump_ir(fns.clone());
    }

    gen_x86(globals, &fns);
    return;
}
