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
    stack: [*mut Value; STACK_MAX],
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
            stack: [ptr::null_mut(); STACK_MAX], // Initialize with null pointers
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

    fn push(&mut self, value: *mut Value) {
        if self.stack_top >= STACK_MAX {
            panic!("Stack overflow!");
        }
        self.stack[self.stack_top] = value;
        self.stack_top += 1;
    }

    fn pop(&mut self) -> *mut Value {
        if self.stack_top == 0 {
            panic!("Stack underflow!");
        }
        self.stack_top -= 1;
        self.stack[self.stack_top]
    }

    fn peek(&self, distance: usize) -> &*mut Value {
        if self.stack_top == 0 || self.stack_top <= distance {
            panic!("Stack underflow during peek!");
        }
        &self.stack[self.stack_top - distance - 1]
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
                let b = self.pop();
                let a = self.pop();
                let _ = &mut *a $op &mut *b;
                self.push(a);
            }};
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
                    let value_ptr = &mut self.chunk.constants[constant] as *mut Value;
                    self.push(value_ptr);
                }
                OpCode::OpAdd => binary_op!(+),
                OpCode::OpSubtract => binary_op!(-),
                OpCode::OpMultiply => binary_op!(*),
                OpCode::OpDivide => binary_op!(/),
                OpCode::OpNegate => {
                    let _ = -&mut **self.peek(0);
                }
                OpCode::OpReturn => {
                    print!("{}", *self.pop());
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
            if !slot.is_null() {
                unsafe {
                    print!("[ {} ]", **slot);
                }
            } else {
                print!("Undefined behaviour: reached [ null ] stack slot!");
            }
        }
        println!();
        disassemble_instruction(&self.chunk, instruction, *i, offset);
        *i += 1;
    }
}
