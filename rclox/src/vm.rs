use std::ptr;

use crate::chunk::{Chunk, OpCode};
use crate::value::{Value, print_value};
use crate::debug::disassemble_instruction;

const STACK_MAX: usize = 256;
static STACK: [Value; STACK_MAX] = [Value::Number(0.0); STACK_MAX];

pub struct VM<'a> {
    chunk: &'a Chunk,
    ip: *const OpCode,
    stack: Vec<Value>,
    // I don't like using Vec here, I'll try to implement an array based stack later.
}

#[derive(Debug)]
pub enum InterpretResult {
    InterpreterOk,
    InterpreterCompileError,
    InterpreterRuntimeError,
}

impl<'a> VM<'a> {
    pub fn new(chunk: &'a Chunk) -> Self {
        VM {
            chunk,
            ip: ptr::null(),
            stack: Vec::with_capacity(STACK_MAX),
        }
    }

    fn reset_stack(&mut self) {
        self.stack.clear();
    }

    fn push(&mut self, value: Value) {
        self.stack.push(value);
    }

    fn pop(&mut self) -> Value {
        self.stack.pop().expect("The stack is empty!")
    }

    fn peek(&self, distance: usize) -> &Value {
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
                self.push(a $op b);
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
                    self.push(self.chunk.constants[constant]);
                }
                OpCode::OpAdd => binary_op!(+),
                OpCode::OpSubtract => binary_op!(-),
                OpCode::OpMultiply => binary_op!(*),
                OpCode::OpDivide => binary_op!(/),
                OpCode::OpNegate => {
                    let negate = -self.pop();
                    self.push(negate);
                },
                OpCode::OpReturn => {
                    print_value(&self.pop());
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
            print!("[ {} ]", slot);
        }
        println!();
        disassemble_instruction(&self.chunk, &instruction, *i, offset);
        *i += 1;
    }
}

pub fn interpret(chunk: &Chunk) -> InterpretResult {
    let mut vm = VM::new(chunk);
    vm.ip = chunk.code;
    unsafe { vm.run() }
}

