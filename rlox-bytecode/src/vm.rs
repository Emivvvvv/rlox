use std::ptr;

use crate::chunk::{Chunk, OpCode};
use crate::compiler::Compiler;
use crate::value::Value;

#[cfg(debug_assertions)]
use crate::debug::disassemble_instruction;

const STACK_MAX: usize = 256;

pub struct VM {
    chunk: Chunk,
    ip: *const OpCode,
    stack: [Value; STACK_MAX],
    stack_top: usize,
}

#[derive(Debug)]
pub enum InterpretError {
    CompileError,
    RuntimeError,
}

impl Default for VM {
    fn default() -> Self {
        Self::new()
    }
}

impl VM {
    pub fn new() -> Self {
        VM {
            chunk: Chunk::new(),
            ip: ptr::null(),
            stack: [Value::new_nil(); STACK_MAX],
            stack_top: 0,
        }
    }

    pub fn interpret(&mut self, source: &str) -> Result<(), InterpretError> {
        let mut compiler = Compiler::new(source);

        // Compile the source and take ownership of the resulting chunk.
        if let Some(chunk) = compiler.compile() {
            // Replace the VM's chunk with the compiled chunk.
            self.chunk = chunk;

            // Set the instruction pointer to the beginning of the chunk's code.
            self.ip = self.chunk.code;

            // Run the VM with the new chunk.
            unsafe { self.run() }
        } else {
            // If compilation failed, return a compile error.
            Err(InterpretError::CompileError)
        }
    }

    #[allow(dead_code)]
    fn reset_stack(&mut self) {
        self.stack_top = 0;
    }

    //

    fn push(&mut self, value: Value) {
        if self.stack_top >= STACK_MAX {
            panic!("Stack overflow!");
        }
        self.stack[self.stack_top] = value;
        self.stack_top += 1;
    }

    fn pop(&mut self) -> Value {
        if self.stack_top == 0 {
            panic!("Stack underflow!");
        }
        self.stack_top -= 1;
        self.stack[self.stack_top]
    }

    fn peek(&self, distance: usize) -> &Value {
        if self.stack_top == 0 || self.stack_top <= distance {
            panic!("Stack underflow during peek!");
        }
        &self.stack[self.stack_top - distance - 1]
    }

    fn is_falsey(&self, value: Value) -> bool {
        value.is_nil() || (value.is_bool() && !value.as_bool())
    }

    unsafe fn run(&mut self) -> Result<(), InterpretError> {
        macro_rules! read_byte {
            () => {{
                let byte = &*self.ip;
                self.ip = self.ip.add(1);
                byte
            }};
        }

        macro_rules! binary_op {
            ($op:tt) => {{
                if !self.peek(0).is_number() || !self.peek(1).is_number() {
                    runtime_error!("Operands must be numbers.");
                }

                let b = self.pop();
                let a = self.pop();
                let _ = a $op b;
                self.push(a);

            }};
        }

        macro_rules! cmp_op {
            ($op:tt) => {{
                if !self.peek(0).is_number() || !self.peek(1).is_number() {
                    runtime_error!("Operands must be numbers.");
                }

                let b = self.pop();
                let a = self.pop();
                let res = a $op b;
                self.push(Value::new_bool(res));
            }};
        }

        macro_rules! runtime_error {
            ( $format:expr) => {{
                eprintln!($format);

                let instruction = self.ip as usize - self.chunk.code as usize;
                let line = self.chunk.lines[instruction];
                eprintln!("[line {:>4}] in script", line);

                self.reset_stack();
            }};
            ( $format:expr, $( $arg:expr),* ) => {{
                eprintln!($format, $( $arg ),*);

                let instruction = self.ip as usize - self.chunk.code as usize;
                let line = self.chunk.lines[instruction];
                eprintln!("[line {:>4}] in script", line);

                self.reset_stack();

                return Err(InterpretError::RuntimeError)
            }}
        }

        #[cfg(debug_assertions)]
        let mut offset = 0;
        #[cfg(debug_assertions)]
        let mut i = 0;

        loop {
            let instruction = read_byte!();

            #[cfg(debug_assertions)]
            {
                self.trace_execution(instruction, &mut i, &mut offset);
            }

            match *instruction {
                OpCode::OpConstant(constant) => {
                    let value = self.chunk.constants[constant];
                    self.push(value);
                }
                OpCode::OpTrue => self.push(Value::new_bool(true)),
                OpCode::OpFalse => self.push(Value::new_bool(false)),
                OpCode::OpEqual => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push(Value::new_bool(a == b))
                }
                OpCode::OpNil => self.push(Value::new_nil()),
                OpCode::OpGreater => cmp_op!(>),
                OpCode::OpLess => cmp_op!(<),
                OpCode::OpAdd => binary_op!(+),
                OpCode::OpSubtract => binary_op!(-),
                OpCode::OpMultiply => binary_op!(*),
                OpCode::OpDivide => binary_op!(/),
                OpCode::OpNot => {
                    let pop_value = self.pop();
                    let falsey = self.is_falsey(pop_value);
                    self.push(Value::new_bool(falsey))
                }
                OpCode::OpNegate => {
                    if !self.peek(0).is_number() {
                        runtime_error!("Operand must be a number.");
                        return Err(InterpretError::RuntimeError);
                    }
                    let popped_value = self.pop();
                    self.push(-popped_value);
                }
                OpCode::OpReturn => {
                    print!("{}", self.pop());
                    println!();
                    return Ok(());
                }
            }
        }
    }

    #[cfg(debug_assertions)]
    fn trace_execution(&self, instruction: &OpCode, i: &mut usize, offset: &mut usize) {
        print!("          ");
        for slot in self.stack.iter().take(self.stack_top) {
            print!("[ {} ]", slot);
        }
        println!();
        disassemble_instruction(&self.chunk, instruction, *i, offset);
        *i += 1;
    }
}
