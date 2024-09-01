use rclox::chunk::{Chunk, OpCode};
use rclox::value::Value;
use rclox::vm::interpret;

fn main() {
    let mut chunk = Chunk::new();

    let constant = chunk.add_constant(Value::new_number(1.2));
    chunk.write_chunk(OpCode::OpConstant(constant), 123);
    let constant = chunk.add_constant(Value::new_number(3.4));
    chunk.write_chunk(OpCode::OpConstant(constant), 123);

    chunk.write_chunk(OpCode::OpAdd, 123);

    let constant = chunk.add_constant(Value::new_number(5.6));
    chunk.write_chunk(OpCode::OpConstant(constant), 123);

    chunk.write_chunk(OpCode::OpDivide, 123);
    chunk.write_chunk(OpCode::OpNegate, 123);

    chunk.write_chunk(OpCode::OpReturn, 123);

    // Interpret the chunk
    let _ = interpret(&mut chunk);
}
