use std::fs;
use std::io::{stdin, stdout, BufRead, BufReader, Result, Write};
use std::path::Path;

use crate::lexer::lexer;

static mut HAD_ERROR: bool = false;

pub(crate) fn run_file(file_path: &String) -> Result<()> {
    let path = Path::new(file_path);
    let file_string = fs::read_to_string(path)?;

    run(file_string)?;
    Ok(())
}

pub(crate) fn run_prompt() -> Result<()> {
    let stdin = stdin();
    let input = stdin.lock();
    let mut reader = BufReader::new(input);

    loop {
        print!("> ");
        stdout().flush()?; // Ensure the prompt is displayed immediately

        let mut line = String::new();
        let bytes_read = reader.read_line(&mut line)?;

        if bytes_read == 0 {
            break; // EOF reached
        }

        run(line.trim().to_string())?;
    }

    Ok(())
}

fn run(source: String) -> Result<()> {
    let mut lexer = lexer::Lexer::new(source);
    let tokens = lexer.scan_tokens();

    tokens.iter().for_each(|token| println!("{token}"));
    Ok(())
}

pub(crate) fn report(line_num: usize, line: &str, message: &str) {
    eprintln!("[line {line_num}] {line}: {message}");

    unsafe {
        HAD_ERROR = true;
    }
}
