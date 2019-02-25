use crate::ir::*;
use crate::*;

lazy_static! {
    pub static ref REGS: Mutex<Vec<String>> = Mutex::new(vec![
        String::from("r10"),
        String::from("r11"),
        String::from("rbx"),
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

fn visit(irv: &mut Vec<IR>) {
    for i in 0..irv.len() {
        let mut ir = &mut irv[i];
        let info = get_irinfo(&ir);

        match info.ty {
            IRInfoType::REG | IRInfoType::REG_IMM | IRInfoType::REG_LABEL => {
                ir.lhs = alloc(ir.lhs);
            }
            IRInfoType::REG_REG => {
                ir.lhs = alloc(ir.lhs);
                ir.rhs = alloc(ir.rhs);
            }
            IRInfoType::CALL => {
                ir.lhs = alloc(ir.lhs);
                for i in 0..ir.nargs {
                    ir.args[i] = alloc(ir.args[i]);
                }
            }
            _ => {}
        }

        if ir.op == IRType::KILL {
            kill(ir.lhs);
            ir.op = IRType::NOP;
        }
    }
}

pub fn alloc_regs(fns: &mut Vec<IR>) {

    for i in 0..fns.len() {
        let mut fun = &mut fns[i];
        match REG_MAP.lock() {
            Ok(mut reg_map) => {
                *reg_map = Vec::new();
                for _i in 0..fun.ir.len() {
                    reg_map.push(-1);
                }
            }
            _ => {
                panic!();
            }
        }

        match USED.lock() {
            Ok(mut used) => {
                *used = [false; 8].to_vec();
            }
            _ => {
                panic!();
            }
        }

        visit(&mut fun.ir);
    }
}
