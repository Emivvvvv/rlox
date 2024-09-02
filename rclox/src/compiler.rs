use crate::scanner::{Scanner, TokenType};

pub struct Compiler<'a> {
    scanner: Scanner<'a>,
}

impl<'a> Compiler<'a> {
    pub(crate) fn new(source: &'a str) -> Self {
        Compiler {
            scanner: Scanner::new(source),
        }
    }

    pub(crate) fn compile(&mut self) {
        let mut line = -1;
        loop {
            let token = self.scanner.scan_token();
            if token.line != line {
                print!("{:4} ", token.line);
                line = token.line;
            } else {
                print!("   | ");
            }
            println!("{:2} '{}'", token.typ, token.src);

            if token.typ == TokenType::EOF {
                break;
            }
        }

    }
}