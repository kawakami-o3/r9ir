use std::io;
use std::io::Write;

const BUFLEN: i32 = 256;

macro_rules! error {
  ($fmt:expr) => (writeln!(&mut std::io::stderr(), $fmt));
  ($fmt:expr,$($x:tt)*) => (writeln!(&mut std::io::stderr(), $fmt, $( $x )*));
}


fn compile_number(n: i32) {
}

fn main() {
  let mut input = String::new();

  io::stdin().read_line(&mut input)
    .expect("Failed to read line");

  let val = input.trim();

  error!("{}{}\n", val, val);

  println!("\t.text\n\t\
            .global _mymain\n\
            _mymain:\n\t\
            mov ${}, %eax\n\t\
            ret\n", val);

}

