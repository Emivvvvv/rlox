use std::error::Error;
use std::fmt;
use std::fs;
use std::io;
use std::io::{stdin, stdout, BufRead, BufReader, Write};
use std::path::Path;
use crate::interner::Interner;
use crate::interpreter::{Interpreter, RuntimeError};
use crate::lexer::scanner;
use crate::lexer::token::{Token, TokenType};
use crate::parser::Parser;
use crate::resolver::Resolver;

#[derive(Debug)]
pub enum LoxError {
    IOError(io::Error),
    Error(String),
    RuntimeError(String),
}

impl fmt::Display for LoxError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            LoxError::IOError(err) => write!(f, "IO Error: {}", err),
            LoxError::Error(msg) => write!(f, "Error: {}", msg),
            LoxError::RuntimeError(msg) => write!(f, "Runtime Error: {}", msg),
        }
    }
}

impl Error for LoxError {}

impl From<io::Error> for LoxError {
    fn from(err: io::Error) -> LoxError {
        LoxError::IOError(err)
    }
}

static mut HAD_ERROR: bool = false;
static mut HAD_RUNTIME_ERROR: bool = false;

fn check_errors() -> Result<(), LoxError> {
    unsafe {
        if HAD_ERROR {
            return Err(LoxError::Error("Compilation error".to_string()));
        }
        if HAD_RUNTIME_ERROR {
            return Err(LoxError::RuntimeError("Runtime error".to_string()));
        }
    }
    Ok(())
}

pub fn run_file(file_path: &String) -> Result<(), LoxError> {
    let path = Path::new(file_path);
    let file_string = fs::read_to_string(path)?;

    run(file_string)?;

    check_errors()?;

    Ok(())
}

pub fn run_prompt() -> Result<(), LoxError> {
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

        unsafe {
            HAD_ERROR = false;
        }
    }

    Ok(())
}

pub fn run(source: String) -> Result<(), LoxError> {
    let interner = Interner::new();
    let mut scanner = scanner::Scanner::new(source, &interner);
    scanner.scan_tokens();
    let mut parser = Parser::new(scanner.tokens);
    let statements = parser
        .parse()
        .map_err(|_| LoxError::Error("Error during parsing".to_string()))?;

    check_errors()?;

    let locals = Resolver::new(&interner).resolve_lox(&statements);

    check_errors()?;

    let mut interpreter = Interpreter::new_with_locals(locals);
    interpreter.interpret(&statements);

    Ok(())
}

pub fn error(token: &Token, message: &str) {
    if token.token_type == TokenType::Eof {
        report(token.line, " at end", message);
    } else {
        report(token.line, &format!("at '{}'", token.lexeme), message);
    }
}

pub fn report(line_num: usize, line: &str, message: &str) {
    eprintln!("[line {line_num}] {:?}: {message}", line);

    unsafe {
        HAD_ERROR = true;
    }
}

pub fn runtime_error(error: RuntimeError) {
    let (token, err_msg) = error.get_info();
    eprintln!("{}\n[line {}]", err_msg, token.line);

    unsafe {
        HAD_RUNTIME_ERROR = true;
    }
}