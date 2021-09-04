use crate::parser::{parse};
use crate::interpreter::{Interpreter};
use std::io::{self, Write};

mod parser;
mod interpreter;

fn main() -> io::Result<()> {
  let mut interp = Interpreter::new();
  println!("Welcome to RS Math Interpreter. Supported operations: +, -, *, /.");
  println!("Enter your expression. CTRL^C or CMD^C to exit.");

  loop {
    let mut buffer = String::new();
    print!("> ");
    io::stdout().flush()?;
    io::stdin().read_line(&mut buffer)?;
    
    let parsed = parse(&buffer);
    match parsed {
      Ok((rem, expr)) => {
        if rem.len() != 0 {
          println!("Invalid expression");
          continue;
        }

        let eval_res = interp.eval(&expr);
        if interp.print_errors() {
          continue;
        }
        println!("{}", eval_res);
      },
      _ => println!("Error parsing expression"),
    };
  }
}
