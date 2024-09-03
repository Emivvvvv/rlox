use std::env;
use std::process::exit;
use std::fs::File;
use std::io::{self, BufRead, Read};
use rlox_bytecode::vm::{interpret, InterpretError};

fn main() {
    let args: Vec<String> = env::args().collect();

    let result= match args.len() {
        1 => repl(),
        2 => run_file(&args[1]),
        _ => {
            eprintln!("Usage: clox [path]");
            exit(64);
        }
    };

    match result {
        Ok(_) => {}
        Err(interpret_err) => match interpret_err {
            InterpretError::CompileError => exit(65),
            InterpretError::RuntimeError => exit(70),
        }
    }
}


fn repl() -> Result<(), InterpretError> {
    let stdin = io::stdin();
    let mut line = String::new();

    loop {
        print!("> ");
        io::Write::flush(&mut io::stdout()).expect("Failed to flush stdout");

        line.clear();
        if stdin.lock().read_line(&mut line).is_err() {
            panic!("Error while reading input.");
        }

        // clox implementation don't have .trim() here.
        match interpret(&line.trim()) {
            Ok(()) => {},
            Err(err) => return Err(err),
        }
    }
}

fn read_file(path: &str) -> String {
    let mut file = File::open(path).unwrap_or_else(|_| {
        eprintln!("Could not open file \"{}\".", path);
        exit(74);
    });

    let mut buffer = String::new();
    file.read_to_string(&mut buffer).unwrap_or_else(|_| {
        eprintln!("Could not read file \"{}\".", path);
        exit(74);
    });

    buffer
}

fn run_file(path: &str) -> Result<(), InterpretError> {
    let source = read_file(path);
    interpret(&source)
}