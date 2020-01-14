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
            memory: Vec::with_capacity(size),
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

    fn get_sign_code8(&self, index: usize) -> i32 {
        self.memory[self.eip as usize + index] as i32
    }

    fn get_code32(&self, index: usize) -> u32 {
        let mut ret: u32 = 0;
        for i in 0..4 {
            ret |= self.get_code8(index + i) << (i * 8)
        }
        return ret;
    }

    fn mov_r32_imm32(&self) {
    }

    fn short_jump(&self) {
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("usage: vm filename");
        return;
    }

    let mut emu = Emulator::create(MEMORY_SIZE, 0x0000, 0x7c00);

    let filename: & String = & args[1];
    let mut buffer = String::new();

    let mut f = File::open(filename).unwrap();

    for byte in f.bytes() {
        emu.memory.push(byte.unwrap());
    }

    /*
    match File::open(filename) {
        Ok(mut file) => match file.read_to_string(&mut buffer) {
            Ok(_) => {}
            Err(e) => {
                panic!(e);
            }
        },
        Err(e) => {
            panic!(e);
        }
    };

    println!("hello {:?}", buffer);
    */
}
