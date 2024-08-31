use rclox::chunk::{Chunk, OpCode};
use rclox::debug::disassemble_chunk;

fn main() {
    let mut chunk = Chunk::new();

    let constant = chunk.add_constant(1.2);
    chunk.write_chunk(OpCode::OpConstant(constant), 123);
    chunk.write_chunk(OpCode::OpReturn, 123);
    disassemble_chunk(&chunk, "test chunk");
}
