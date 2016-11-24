use std::io;

fn main() {
  let mut input = String::new();

  io::stdin().read_line(&mut input)
    .expect("Failed to read line");

  let val = input.trim();


  println!("\t.text\n\t\
            .global _mymain\n\
            _mymain:\n\t\
            mov ${}, %eax\n\t\
            ret\n", val);
}

