use rlox::lox;
use std::env::args;
use std::process::exit;

fn main() {
    let args: Vec<String> = args().collect();

    if args.len() > 2 {
        println!("Usage: rlox [script]");
        exit(64);
    } else if args.len() == 2 {
        if let Err(e) = lox::run_file(&args[1]) {
            eprint!("{e}");
            exit(65)
        };
    } else {
        let _ = lox::run_prompt();
    }
}
