use std::env::args;
use std::process::exit;

use rlox_ast::lox;

fn main() {
    let args: Vec<String> = args().collect();

    match args.len() {
        0 | 1 => {
            let _ = lox::run_prompt();
        }
        2 => {
            if let Err(e) = lox::run_file(&args[1]) {
                eprintln!("{e}");
                exit(65);
            }
        }
        _ => {
            println!("Usage: rlox [script]");
            exit(64);
        }
    }
}
