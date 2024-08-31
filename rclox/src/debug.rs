
use crate::chunk::{Chunk, OpCode};
use crate::value::print_value;

pub fn disassemble_chunk(chunk: &Chunk, name: &str) {
    println!("== {name} ==");

    for (i, op_code) in chunk.into_iter().enumerate() {
        disassemble_instruction(chunk, op_code, i)
    }
}

fn simple_instruction(name: &str) {
    println!("{}", name)
}

fn constant_instruction(name: &str, chunk: &Chunk, constant_index: usize) {
    print!("{:<16} {:>4} '", name, constant_index);
    print_value(&chunk.constants[constant_index]);
    println!("'");
}

pub fn disassemble_instruction(chunk: &Chunk, instruction: &OpCode, offset: usize) {
    // offset here is not correct for now because we're traversing with iter in Chunk.
    // It's more a counter of `OpCode`s for more than an offset.
    print!("{:04} ", offset);
    if offset > 0 && chunk.lines[offset] == chunk.lines[offset - 1] {
        print!("   | ");
    } else {
        print!("{:>4} ", chunk.lines[offset])
    }
    match instruction {
        OpCode::OpConstant(constant_index) => constant_instruction("OP_CONSTANT", chunk, *constant_index),
        OpCode::OpReturn => simple_instruction("OpReturn"),
        _ => println!("Unknown opcode {:?}", instruction),
    }
}
