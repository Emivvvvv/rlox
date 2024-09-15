use std::error::Error;
use std::fmt;
use std::fs;
use std::io;
use std::io::{stdin, stdout, BufRead, BufReader, Write};
use std::path::Path;

use crate::interpreter::{Interpreter, RuntimeError};
use crate::lexer::scanner;
use crate::lexer::token::{ErrorToken, TokenType};
use crate::parser::Parser;
use crate::resolver::Resolver;
use crate::symbol::SymbolTable;

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

    run(&file_string)?;

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

        run(line.trim())?;

        unsafe {
            HAD_ERROR = false;
        }
    }

    Ok(())
}

pub fn run(source: &str) -> Result<(), LoxError> {
    let mut symbol_table = SymbolTable::new();
    let lexer_tokens = {
        let mut lexer = scanner::Scanner::new(source, &mut symbol_table);
        lexer.scan_tokens();

        lexer.tokens
    };
    let parser = Parser::new(&symbol_table, lexer_tokens);
    let (statements, expr_pool) = parser
        .parse()
        .map_err(|_| LoxError::Error("Error during parsing".to_string()))?;
    check_errors()?;

    let locals = Resolver::new(&expr_pool, &mut symbol_table).resolve_lox(&statements);

    check_errors()?;

    let mut interpreter = Interpreter::new(&expr_pool, &mut symbol_table, locals);
    interpreter.interpret(&statements);

    Ok(())
}

pub fn error(token: &ErrorToken, message: &str) {
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