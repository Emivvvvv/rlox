use crate::chunk::{Chunk, OpCode};
use crate::debug::disassemble_chunk;
use crate::scanner::{Scanner, Token, TokenType};
use crate::value::Value;
use std::mem;

#[derive(PartialEq, PartialOrd, Clone, Copy)]
enum Precedence {
    None,
    Assignment, // =
    Or,         // or
    And,        // and
    Equality,   // == !=
    Comparison, // < > <= >=
    Term,       // + -
    Factor,     // * /
    Unary,      // ! -
    Call,       // . ()
    Primary,
}

impl Precedence {
    fn to_usize(self) -> usize {
        self as usize
    }

    fn from_usize(value: usize) -> Self {
        match value {
            0 => Precedence::None,
            1 => Precedence::Assignment,
            2 => Precedence::Or,
            3 => Precedence::And,
            4 => Precedence::Equality,
            5 => Precedence::Comparison,
            6 => Precedence::Term,
            7 => Precedence::Factor,
            8 => Precedence::Unary,
            9 => Precedence::Call,
            10 => Precedence::Primary,
            _ => Precedence::Primary, // Default or fallback case
        }
    }

    fn next(&self) -> Self {
        Precedence::from_usize(self.to_usize() + 1)
    }
}

enum PrefixOperation {
    Grouping,
    Unary,
    Number,
    None,
}

enum InfixOperation {
    Binary,
    None,
}

struct ParseRule {
    prefix: PrefixOperation,
    infix: InfixOperation,
    precedence: Precedence,
}

struct Parser<'a> {
    current: Token<'a>,
    previous: Token<'a>,
    had_error: bool,
    panic_mode: bool,
}

impl<'a> Parser<'a> {
    fn new() -> Self {
        Parser {
            current: Token::default(),
            previous: Token::default(),
            had_error: false,
            panic_mode: false,
        }
    }
}

pub struct Compiler<'a> {
    scanner: Scanner<'a>,
    parser: Parser<'a>,
    chunk: Chunk,
    parse_rules: [ParseRule; 6],
}

impl<'a> Compiler<'a> {
    pub(crate) fn new(source: &'a str) -> Self {
        let parse_rules: [ParseRule; 6] = [
            ParseRule {
                prefix: PrefixOperation::Grouping,
                infix: InfixOperation::None,
                precedence: Precedence::None,
            }, // LeftParen
            ParseRule {
                prefix: PrefixOperation::Unary,
                infix: InfixOperation::Binary,
                precedence: Precedence::Term,
            }, // Minus
            ParseRule {
                prefix: PrefixOperation::None,
                infix: InfixOperation::Binary,
                precedence: Precedence::Term,
            }, // Plus
            ParseRule {
                prefix: PrefixOperation::None,
                infix: InfixOperation::Binary,
                precedence: Precedence::Factor,
            }, // Slash & Star
            ParseRule {
                prefix: PrefixOperation::Number,
                infix: InfixOperation::None,
                precedence: Precedence::None,
            }, // Number
            ParseRule {
                prefix: PrefixOperation::None,
                infix: InfixOperation::None,
                precedence: Precedence::None,
            }, // None
        ];

        Compiler {
            scanner: Scanner::new(source),
            parser: Parser::new(),
            chunk: Chunk::new(),
            parse_rules,
        }
    }

    pub fn compile(&mut self) -> Option<Chunk> {
        self.parser.had_error = false;
        self.parser.panic_mode = false;

        self.advance();
        self.expression();
        self.consume(TokenType::Eof, "Expect end of expression.");
        self.end_compiler();
        if self.parser.had_error {
            None
        } else {
            let chunk = mem::replace(&mut self.chunk, Chunk::new());
            Some(chunk)
        }
    }

    fn current_chunk(&mut self) -> &mut Chunk {
        &mut self.chunk
    }

    fn error_at(&mut self, token: &Token, message: &str) {
        if self.parser.panic_mode {
            return;
        }
        self.parser.panic_mode = true;

        eprint!("[line {}] Error", token.line);

        match token.typ {
            TokenType::Eof => eprint!(" at end"),
            TokenType::Error => (),
            _ => eprint!(" at '{}'", token.src),
        }

        eprintln!(": {}", message);
        self.parser.had_error = true;
    }

    fn error(&mut self, message: &str) {
        self.error_at(&self.parser.previous.clone(), message);
    }

    fn error_at_current(&mut self, message: &str) {
        self.error_at(&self.parser.current.clone(), message);
    }

    fn advance(&mut self) {
        self.parser.previous = self.parser.current;

        loop {
            self.parser.current = self.scanner.scan_token();
            if self.parser.current.typ != TokenType::Error {
                break;
            }

            self.error_at_current(self.parser.current.src);
        }
    }

    fn consume(&mut self, typ: TokenType, message: &str) {
        if self.parser.current.typ == typ {
            self.advance();
            return;
        }

        self.error_at_current(message);
    }

    fn emit_byte(&mut self, byte: OpCode) {
        let line = self.parser.previous.line;
        let chunk = self.current_chunk();
        chunk.write_chunk(byte, line);
    }

    #[allow(dead_code)]
    fn emit_bytes(&mut self, byte1: OpCode, byte2: OpCode) {
        self.emit_byte(byte1);
        self.emit_byte(byte2);
    }

    fn emit_return(&mut self) {
        self.emit_byte(OpCode::OpReturn);
    }

    fn make_constant(&mut self, value: Value) -> usize {
        self.current_chunk().add_constant(value)
    }

    fn emit_constant(&mut self, value: Value) {
        let constant = self.make_constant(value);
        self.emit_byte(OpCode::OpConstant(constant))
    }

    fn end_compiler(&mut self) {
        self.emit_return();

        if cfg!(debug_assertions) && self.parser.had_error {
            disassemble_chunk(self.current_chunk(), "code");
        }
    }

    fn binary(&mut self) {
        let operator_type = self.parser.previous.typ;
        let precedence = self.get_rule(&operator_type).precedence;

        // Compile the right operand
        self.parse_precedence(precedence.next());

        // Emit the operator instruction
        match operator_type {
            TokenType::Plus => self.emit_byte(OpCode::OpAdd),
            TokenType::Minus => self.emit_byte(OpCode::OpSubtract),
            TokenType::Star => self.emit_byte(OpCode::OpMultiply),
            TokenType::Slash => self.emit_byte(OpCode::OpDivide),
            _ => unimplemented!(),
        }
    }

    fn grouping(&mut self) {
        self.expression();
        self.consume(TokenType::RightParen, "Expect ')' after expression.");
    }

    fn number(&mut self) {
        let value: f64 = self.parser.previous.src.parse().unwrap();
        self.emit_constant(Value::new_number(value))
    }

    fn unary(&mut self) {
        let operator_type = self.parser.previous.typ;

        // Compile the operand.
        self.parse_precedence(Precedence::Unary);

        // Emit the operator instruction.
        match operator_type {
            TokenType::Minus => self.emit_byte(OpCode::OpNegate),
            _ => unimplemented!(),
        }
    }

    fn parse_precedence(&mut self, precedence: Precedence) {
        self.advance();

        // Get the prefix operation
        let prefix_op = &self.get_rule(&self.parser.previous.typ).prefix;

        match prefix_op {
            PrefixOperation::Grouping => self.grouping(),
            PrefixOperation::Unary => self.unary(),
            PrefixOperation::Number => self.number(),
            PrefixOperation::None => {
                self.error("Expect expression.");
                return;
            }
        }

        // Loop through and process infix rules
        while precedence <= self.get_rule(&self.parser.current.typ).precedence {
            self.advance();

            let infix_op = &self.get_rule(&self.parser.previous.typ).infix;

            match infix_op {
                InfixOperation::Binary => self.binary(),
                InfixOperation::None => break, // No infix rule, exit loop
            }
        }
    }

    fn get_rule(&self, kind: &TokenType) -> &ParseRule {
        match kind {
            TokenType::LeftParen => &self.parse_rules[0],
            TokenType::Minus => &self.parse_rules[1],
            TokenType::Plus => &self.parse_rules[2],
            TokenType::Slash | TokenType::Star => &self.parse_rules[3],
            TokenType::Number => &self.parse_rules[4],
            _ => &self.parse_rules[5],
        }
    }

    fn expression(&mut self) {
        self.parse_precedence(Precedence::Assignment);
    }
}
