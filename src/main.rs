use rlox::interpreter;
use std::io::{self, Write};

fn main() {
    // A buffer to store input read from stdin.
    let mut buffer = String::new();
    loop {
        // We need to clear the buffer each time because read_line() appends to the buffer rather
        // than replacing it.
        buffer.clear();
        print!("> ");
        // flush() is needed to write the prompt to the screen. Here it's considered safe to use
        // unwrap(), as flush() is rarely expected to fail.
        io::stdout().flush().unwrap();

        // Read a line of input.
        match io::stdin().read_line(&mut buffer) {
            Ok(size) => {
                // EOF is indicated by a size of zero. An empty string would still include the
                // trailing newline and thus the size woudl be 1.
                if size == 0 {
                    // The newline is needed to not write it right after the prompt.
                    println!("\nOk, bye!");
                    break;
                } else {
                    // The buffer will still contain the newline character, so we need to trim it
                    // first.
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
