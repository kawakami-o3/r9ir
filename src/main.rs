// main

mod util;
#[macro_use]
mod token;
mod gen_ir;
mod gen_x86;
mod irdump;
mod liveness;
mod opt;
mod parse;
mod preprocess;
mod regalloc;
mod sema;

use crate::gen_ir::*;
use crate::gen_x86::*;
use crate::irdump::*;
use crate::liveness::*;
use crate::opt::*;
use crate::parse::*;
use crate::regalloc::*;
use crate::sema::*;
use crate::token::*;
use std::cell::RefCell;
use std::env;
use std::rc::Rc;

fn print_node(n: Rc<RefCell<Node>>, offset: usize) {
    for _i in 0..offset {
        eprint!(" ");
    }

    let node = n.borrow().clone();
    eprintln!(
        "{:?} {} '{}' {:?}",
        node.op,
        node.val,
        node.name,
        node.ty.borrow().ty
    );
    //eprintln!("{:?}", node);
    if node.rhs.is_some() {
        print_node(node.rhs.clone().unwrap(), offset + 2);
    }
    if node.lhs.is_some() {
        print_node(node.lhs.clone().unwrap(), offset + 2);
    }
    if node.expr.is_some() {
        print_node(node.expr.clone().unwrap(), offset + 2);
    }
    //eprintln!(">> {}", node.stmts.len());
    for i in node.stmts.iter() {
        print_node(i.clone(), offset + 2);
    }

    if node.cond.is_some() {
        print_node(node.cond.clone().unwrap(), offset + 2);
    }
    if node.then.is_some() {
        print_node(node.then.clone().unwrap(), offset + 2);
    }
    if node.els.is_some() {
        print_node(node.els.clone().unwrap(), offset + 2);
    }
    if node.init.is_some() {
        print_node(node.init.clone().unwrap(), offset + 2);
    }
    if node.inc.is_some() {
        print_node(node.inc.clone().unwrap(), offset + 2);
    }
    if node.body.is_some() {
        print_node(node.body.clone().unwrap(), offset + 2);
    }

    for i in node.args.iter() {
        print_node(i.clone(), offset + 2);
    }
}

fn usage() {
    println!("Usage: r9ir [-test] [-dump-ir1] [-dump-ir2] <file>");
}

fn main() {
    let argv: Vec<String> = env::args().collect();
    if argv.len() == 1 {
        usage();
        return;
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
            return;
        }
        path = argv[1].clone();
    }

    // Token -> Node -> IR -> asm
    // token -> parse -> sema -> gen_ir(irdump) -> regalloc -> gen_x86

    // Tokenize and parse.
    let tokens = tokenize(path, true);
    //for i in tokens.iter() { eprintln!(">> {:?} {:?}", i.ty, i.name); }
    let prog = &mut parse(&tokens);
    if dump_node {
        for i in prog.funcs.iter() {
            let n = i.borrow().node.clone();
            print_node(n.clone(), 0);
        }
        return;
    }

    sema(prog);
    gen_ir(prog);

    if dump_ir1 {
        dump_ir(prog.funcs.clone());
        return;
    }

    optimize(prog);
    liveness(prog);
    alloc_regs(prog);

    if dump_ir2 {
        dump_ir(prog.funcs.clone());
        return;
    }

    let asm = gen_x86(prog);

    print!("{}", asm);
    return;
}
