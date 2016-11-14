use std::io;

fn main() {
  let mut a = String::new();

  io::stdin().read_line(&mut a)
    .expect("Failed to read line");


  println!("hello {}", a);
}

