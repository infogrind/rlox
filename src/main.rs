use rlox::interpreter;
use std::io::{self, Write};

fn main() {
    let mut buffer = String::new();
    loop {
        buffer.clear();
        print!("> ");
        io::stdout().flush().unwrap();
        match io::stdin().read_line(&mut buffer) {
            Ok(size) => {
                if size == 0 {
                    println!("\nOk, bye!");
                    break;
                } else {
                    println!("{}", interpreter::interpret(buffer.trim()));
                }
            }
            Err(e) => {
                eprintln!("Error: {e:?}");
                break;
            }
        }
    }
}
