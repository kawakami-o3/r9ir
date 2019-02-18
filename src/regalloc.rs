use crate::ir::*;
use crate::*;

lazy_static! {
    pub static ref REGS: Mutex<Vec<String>> = Mutex::new(vec![
        String::from("rdi"),
        String::from("rsi"),
        String::from("r10"),
        String::from("r11"),
        String::from("r12"),
        String::from("r13"),
        String::from("r14"),
        String::from("r15"),
    ]);
    static ref USED: Mutex<Vec<bool>> = Mutex::new([false; 8].to_vec());
    static ref REG_MAP: Mutex<Vec<i32>> = Mutex::new(Vec::new());
}

// Code generator

fn alloc(ir_reg: i32) -> i32 {
    let mut reg_map = REG_MAP.lock().unwrap();
    if reg_map[ir_reg as usize] != -1 {
        let r = reg_map[ir_reg as usize];
        assert!(USED.lock().unwrap()[r as usize], "");
        return r;
    }

    let regs = REGS.lock().unwrap();
    let mut used = USED.lock().unwrap();
    for i in 0..regs.len() {
        if used[i] {
            continue;
        }
        used[i] = true;
        reg_map[ir_reg as usize] = i as i32;
        return i as i32;
    }
    panic!("register exhausted");
}

fn kill(i: i32) {
    let mut used = USED.lock().unwrap();
    let r = i as usize;
    assert!(used[r]);
    used[r] = false;
}

pub fn alloc_regs(irv: &mut Vec<IR>) {
    match REG_MAP.lock() {
        Ok(mut reg_map) => {
            for _i in 0..irv.len() {
                reg_map.push(-1);
            }
        }
        _ => {}
    }

    for i in 0..irv.len() {
        let mut ir = &mut irv[i as usize];
        match ir.op {
            IRType::IMM | IRType::ALLOCA | IRType::RETURN => {
                ir.lhs = alloc(ir.lhs);
            }
            IRType::MOV | IRType::LOAD | IRType::STORE | IRType::ADD | IRType::SUB | IRType::MUL | IRType::DIV => {
                ir.lhs = alloc(ir.lhs);
                ir.rhs = alloc(ir.rhs);
            }
            IRType::KILL => {
                let mut reg_map = REG_MAP.lock().unwrap();
                kill(reg_map[ir.lhs as usize]);
                ir.op = IRType::NOP;
            }
            _ => {}
        }
    }
}
