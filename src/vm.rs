use std::env;
use std::fs::File;
use std::io::Read;

const MEMORY_SIZE: usize = 1024 * 1024;

type Register = (usize, &'static str);

trait RegisterHelpers {
    fn addr(&self) -> usize;
    fn name(&self) -> &'static str;
}

impl RegisterHelpers for Register {
    fn addr(&self) -> usize {
        self.0
    }

    fn name(&self) -> &'static str {
        self.1
    }
}

const EAX: Register = (0, "EAX");
const ECX: Register = (1, "ECX");
const EDX: Register = (2, "EDX");
const EBX: Register = (3, "EBX");
const ESP: Register = (4, "ESP");
const EBP: Register = (5, "EBP");
const ESI: Register = (6, "ESI");
const EDI: Register = (7, "EDI");
const REGISTERS: [Register; 8] = [EAX, ECX, EDX, EBX, ESP, EBP, ESI, EDI];


struct Emulator {
    registers: [u32; REGISTERS.len()],
    eflags: u32,
    memory: Vec<u8>,
    eip: u32,
}

impl Emulator {
    fn create(size: usize, eip: u32, esp: u32) -> Emulator {
        let mut regs = [0; REGISTERS.len()];
        regs[ESP.addr()] = esp;
        Emulator {
            registers: regs,
            memory: vec![0; MEMORY_SIZE],//Vec::with_capacity(size),
            eflags: 0,
            eip: eip,
        }
    }

    fn dump_registers(&self) {
        for i in REGISTERS.iter() {
            println!("{} = {}", i.name(), self.registers[i.addr()]);
        }

        println!("EIP = {}", self.eip);
    }

    fn get_code8(&self, index: usize) -> u32 {
        self.memory[self.eip as usize + index] as u32
    }

    fn get_sign_code8(&self, index: usize) -> i8 {
        self.memory[self.eip as usize + index] as i8
    }

    fn get_code32(&self, index: usize) -> u32 {
        let mut ret: u32 = 0;
        for i in 0..4 {
            ret |= self.get_code8(index + i) << (i * 8)
        }
        return ret;
    }

    fn mov_r32_imm32(&mut self) {
        let reg = self.get_code8(0) - 0xB8;
        let value = self.get_code32(1);
        self.registers[reg as usize] = value;
        self.eip += 5;
    }

    fn short_jump(&mut self) {
        let diff = self.get_sign_code8(1);
        self.eip = (self.eip as i32 + diff as i32 + 2) as u32;
    }
}

fn init_instructions() -> Vec<Option<Box<dyn Fn(&mut Emulator)>>> {
    //let mut v: Vec<Box<dyn Fn(&Emulator)>> = Vec::new();
    let mut v: Vec<Option<Box<dyn Fn(&mut Emulator)>>> = Vec::new();

    for _ in 0..256 {
        //v.push(Box::new(|_| {}));
        v.push(None);
    }

    for i in 0..8 {
        //v[0xB8 + i ] = Box::new(Emulator::mov_r32_imm32);
        v[0xB8 + i ] = Some(Box::new(Emulator::mov_r32_imm32));
    }
    //v[0xEB] = Box::new(Emulator::short_jump);
    v[0xEB] = Some(Box::new(Emulator::short_jump));

    return v;
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("usage: vm filename");
        return;
    }

    let mut emu = Emulator::create(MEMORY_SIZE, 0x0000, 0x7c00);

    let filename: & String = & args[1];

    match File::open(filename) {
        Ok(f) => {
            let mut i = 0;
            for byte in f.bytes() {
                //emu.memory.push(byte.unwrap());
                emu.memory[i] = byte.unwrap();
                i+=1;
            }
        }
        _ => {
            println!("Failed to open {}.", filename);
            return;
        }
    }

    let instructions = init_instructions();

    while emu.eip < MEMORY_SIZE as u32 {
        let code = emu.get_code8(0);

        println!("EIP = {:X}, Code = {:X}", emu.eip, code);

        match &instructions[code as usize] {
            None => {
                println!("Not implemented: {}", code);
                break;
            }
            Some(f) => {
                f(&mut emu);
            }
        }

        if emu.eip == 0x00 {
            println!("\n\nend of program.\n");
            break;
        }
    }

    emu.dump_registers();

}
