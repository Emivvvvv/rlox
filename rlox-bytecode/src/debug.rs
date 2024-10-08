use crate::chunk::{Chunk, OpCode};

pub fn disassemble_chunk(chunk: &Chunk, name: &str) {
    println!("== {name} ==");

    let mut offset = 0_usize;
    for (i, op_code) in chunk.into_iter().enumerate() {
        disassemble_instruction(chunk, op_code, i, &mut offset)
    }
}

fn simple_instruction(name: &str, offset: &mut usize) {
    println!("{}", name);
    *offset += 1;
}

fn constant_instruction(name: &str, chunk: &Chunk, constant_index: usize, offset: &mut usize) {
    print!("{:<16} {:>4} '", name, constant_index);
    print!("{}", &chunk.constants[constant_index]);
    println!("'");
    *offset += 2;
}

pub fn disassemble_instruction(chunk: &Chunk, instruction: &OpCode, i: usize, offset: &mut usize) {
    // because we use a different approach than clox here with using OpCode enums as our
    // instructions, we need to traverse one by one to get each one of them. And the funny thing is
    // rust allocates the same memory even if the instruction is a `OpCode::OpConstant` or
    // `OpCode::OpReturn` even if `OpCode::OpConstant` has a separate usize value inside. Because of
    // that C implementation is more memory efficient. Just for debugging purposes and make it look
    // like c version, we handle offset and the index separately. clox only uses offset to handle both.
    print!("{:04} ", offset);
    if i > 0 && chunk.lines[i] == chunk.lines[i - 1] {
        print!("   | ");
    } else {
        print!("{:>4} ", chunk.lines[i])
    }
    match instruction {
        OpCode::OpConstant(constant_index) => {
            constant_instruction("OP_CONSTANT", chunk, *constant_index, offset)
        }
        OpCode::OpNil => simple_instruction("OP_NIL", offset),
        OpCode::OpTrue => simple_instruction("OP_TRUE", offset),
        OpCode::OpFalse => simple_instruction("OP_FALSE", offset),
        OpCode::OpEqual => simple_instruction("OP_EQUAL", offset),
        OpCode::OpGreater => simple_instruction("OP_GREATER", offset),
        OpCode::OpLess => simple_instruction("OP_LESS", offset),
        OpCode::OpAdd => simple_instruction("OP_ADD", offset),
        OpCode::OpSubtract => simple_instruction("OP_SUBTRACT", offset),
        OpCode::OpMultiply => simple_instruction("OP_MULTIPLY", offset),
        OpCode::OpDivide => simple_instruction("OP_DIVIDE", offset),
        OpCode::OpNot => simple_instruction("OP_NOT", offset),
        OpCode::OpNegate => simple_instruction("OP_NEGATE", offset),
        OpCode::OpReturn => simple_instruction("OP_RETURN", offset),
    }
}
