use rclox::chunk::{Chunk, OpCode};
use rclox::debug::disassemble_chunk;
use rclox::vm::interpret;

fn main() {
    let mut chunk = Chunk::new();

    let constant = chunk.add_constant(1.2.into());
    chunk.write_chunk(OpCode::OpConstant(constant), 123);
    let constant = chunk.add_constant(3.4.into());
    chunk.write_chunk(OpCode::OpConstant(constant), 123);

    chunk.write_chunk(OpCode::OpAdd, 123);

    let constant = chunk.add_constant(5.6.into());
    chunk.write_chunk(OpCode::OpConstant(constant), 123);

    chunk.write_chunk(OpCode::OpDivide, 123);
    chunk.write_chunk(OpCode::OpNegate, 123);

    chunk.write_chunk(OpCode::OpReturn, 123);

    // Interpret the chunk
    let _ = interpret(&chunk);
}
