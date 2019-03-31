// main

mod util;
#[macro_use]
mod token;
mod preprocess;
mod parse;
mod sema;
mod gen_ir;
mod irdump;
mod regalloc;
mod gen_x86;

use crate::gen_x86::*;
use crate::gen_ir::*;
use crate::irdump::*;
use crate::parse::*;
use crate::regalloc::*;
use crate::sema::*;
use crate::token::*;
use std::env;

fn print_node(node: Node, offset: usize) {
    for _i in 0..offset {
        eprint!(" ");
    }
    eprintln!("{:?} {} '{}' {:?}", node.op, node.val, node.name, node.ty);
    //eprintln!("{:?}", node);
    if node.rhs.is_some() {
        print_node(*node.rhs.clone().unwrap(), offset+2);
    }
    if node.lhs.is_some() {
        print_node(*node.lhs.clone().unwrap(), offset+2);
    }
    if node.expr.is_some() {
        print_node(*node.expr.clone().unwrap(), offset+2);
    }
    //eprintln!(">> {}", node.stmts.len());
    for i in node.stmts.iter() {
        print_node(i.clone(), offset+2);
    }


    if node.cond.is_some() { print_node(*node.cond.clone().unwrap(), offset+2); }
    if node.then.is_some() { print_node(*node.then.clone().unwrap(), offset+2); }
    if node.els.is_some() { print_node(*node.els.clone().unwrap(), offset+2); }
    if node.init.is_some() { print_node(*node.init.clone().unwrap(), offset+2); }
    if node.inc.is_some() { print_node(*node.inc.clone().unwrap(), offset+2); }
    if node.body.is_some() { print_node(*node.body.clone().unwrap(), offset+2); }

    for i in node.args.iter() {
        print_node(i.clone(), offset+2);
    }
}

fn usage() {
    panic!("Usage: rcc [-test] [-dump-ir1] [-dump-ir2] <file>");
}


fn main() {
    let argv: Vec<String> = env::args().collect();
    if argv.len() == 1 {
        usage();
    }

    let path: String;
    let mut dump_node = false;
    let mut dump_ir1 = false;
    let mut dump_ir2 = false;

    if argv.len() == 3 && argv[1] == "-dump-node" {
        dump_node = true;
        path = argv[2].clone();
    } else if argv.len() == 3 && argv[1] == "-dump-ir1" {
        dump_ir1 = true;
        path = argv[2].clone();
    } else if argv.len() == 3 && argv[1] == "-dump-ir2" {
        dump_ir2 = true;
        path = argv[2].clone();
    } else {
        if argv.len() != 2 {
            usage();
        }
        path = argv[1].clone();
    }

    // Token -> Node -> IR -> asm
    // token -> parse -> sema -> gen_ir(irdump) -> regalloc -> gen_x86

    // Tokenize and parse.
    let tokens = tokenize(path, true);
    //for i in tokens.iter() { eprintln!(">> {:?}", i); }
    let mut nodes = parse(&tokens);
    //eprintln!("nodes> {:?}", nodes);
    if dump_node {
        for i in nodes.iter() {
            print_node(i.clone(), 0);
        }
        return;
    }
    let globals = sema(&mut nodes);
    let mut fns = gen_ir(nodes);

    if dump_ir1 {
        dump_ir(fns.clone());
        return;
    }

    alloc_regs(&mut fns);

    if dump_ir2 {
        dump_ir(fns.clone());
        return;
    }

    gen_x86(globals, &fns);
    return;
}
