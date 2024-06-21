mod expr;
mod lexer;
mod lox;

use std::env::args;
use std::process::exit;

fn main() {
    let args: Vec<String> = args().collect();

    if args.len() > 2 {
        println!("Usage: rlox [script]");
        exit(64);
    } else if args.len() == 2 {
        if let Err(e) = lox::run_file(&args[1]) {
            if e.kind() == std::io::ErrorKind::NotFound {
                println!("The file doesn't exist!");
                exit(65);
            } else {
                panic!("{}", e);
            }
        };
    } else {
        let _ = lox::run_prompt();
    }
}
