
const REGISTERS_COUNT: usize = 256;

struct Emulator {
    registers: [u32; REGISTERS_COUNT],
    eflags: u32,
    eip: u32,
}

impl Emulator {
}

fn main() {
    println!("hello");
}
