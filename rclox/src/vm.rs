use std::ptr;

use crate::chunk::{Chunk, OpCode};
use crate::value::{Value};
use crate::debug::disassemble_instruction;

const STACK_MAX: usize = 256;

pub struct VM<'a> {
    chunk: &'a mut Chunk,
    ip: *const OpCode,
    stack: Vec<*mut Value>,
}

#[derive(Debug)]
pub enum InterpretResult {
    InterpreterOk,
    InterpreterCompileError,
    InterpreterRuntimeError,
}

impl<'a> VM<'a> {
    pub fn new(chunk: &'a mut Chunk) -> Self {
        VM {
            chunk,
            ip: ptr::null(),
            stack: Vec::with_capacity(STACK_MAX), // allocating 256 slots for preventing
        }                                         // multiple mem allocations
    }

    fn reset_stack(&mut self) {
        self.stack.clear();
    }

    fn push(&mut self, value: *mut Value) {
        self.stack.push(value);
    }

    fn pop(&mut self) -> *mut Value {
        self.stack.pop().expect("The stack is empty!")
    }

    fn peek(&self, distance: usize) -> &*mut Value {
        &self.stack[self.stack.len() - distance - 1]
    }

    unsafe fn run(&mut self) -> InterpretResult {
        macro_rules! read_byte {
            () => ({
                let byte = &*self.ip;
                self.ip = self.ip.add(1);
                byte
            });
        }

        macro_rules! binary_op {
            ($op:tt) => {{
                let b = self.pop();
                let a = self.pop();
                let _ = &mut *a $op &mut *b;
                self.push(a);
            }};
        }

        let mut offset = 0;
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
                    unsafe {
                        let _ = -&mut **self.peek(0);
                    }
                },
                OpCode::OpReturn => {
                    unsafe {
                        print!("{}", **&self.pop());
                    }
                    println!();
                    return InterpretResult::InterpreterOk;
                }
            }
        }
    }

    #[cfg(debug_assertions)]
    fn trace_execution(&self, instruction: &OpCode, i: &mut usize, offset: &mut usize) {
        print!("          ");
        for slot in &self.stack {
            unsafe {
                print!("[ {} ]", **slot);
            }
        }
        println!();
        disassemble_instruction(&self.chunk, &instruction, *i, offset);
        *i += 1;
    }
}

pub fn interpret(chunk: &mut Chunk) -> InterpretResult {
    let ip = chunk.code;
    let mut vm = VM::new(chunk);
    vm.ip = ip;
    unsafe { vm.run() }
}

