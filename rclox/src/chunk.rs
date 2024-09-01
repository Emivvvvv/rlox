use std::ptr;
use std::mem;
use crate::memory::reallocate;
use crate::value::{Value, ValueArray};

#[derive(Debug)]
pub enum OpCode {
    OpConstant(usize),
    OpAdd,
    OpSubtract,
    OpMultiply,
    OpDivide,
    OpNegate,
    OpReturn,
}

pub struct Chunk {
    count: usize,
    capacity: usize,

    pub lines: Vec<i32>,
    pub code: *mut OpCode, // raw pointer to OpCode
    pub constants: ValueArray,
}

impl Chunk {
    pub fn new() -> Self {
        Self {
            count: 0,
            capacity: 0,

            lines: Vec::new(),
            code: ptr::null_mut(),
            constants: Vec::new(),
        }
    }

    pub fn write_chunk(&mut self, byte: OpCode, line: i32) {
        if self.capacity < self.count + 1 {
            let old_capacity = self.capacity;
            self.capacity = self.grow_capacity(old_capacity);
            self.code = self.grow_array(self.code, old_capacity, self.capacity);
        }

        unsafe {
            let value: *const OpCode = &byte;
            ptr::copy_nonoverlapping(
                value,
                self.code.offset(self.count as isize),
                1)
        }
        self.count += 1;

        self.lines.push(line);
    }

    pub fn add_constant(&mut self, value: Value) -> usize {
        self.constants.push(value);
        self.constants.len() - 1
    }

    fn grow_capacity(&self, capacity: usize) -> usize {
        if capacity < 8 {
            8
        } else {
            capacity * 2
        }
    }

    fn grow_array(&self, previous: *mut OpCode, old_count: usize, count: usize) -> *mut OpCode {
        reallocate(
            previous,
            mem::size_of::<OpCode>() * old_count,
            mem::size_of::<OpCode>() * count,
        )
    }

    fn free_array(&mut self) {
        reallocate(self.code, mem::size_of::<u8>() * self.capacity, 0);
    }
}

// a.k.a freeChunk
impl Drop for Chunk {
    // lines and constants are dropped automatically.
    fn drop(&mut self) {
        self.free_array();

        self.count = 0;
        self.capacity = 0;
        self.code = ptr::null_mut();
    }
}

// might need to be modified to achieve clox like offset.
// change is not necessary for now.
impl<'a> IntoIterator for &'a Chunk {
    type Item = &'a OpCode;
    type IntoIter = ChunkIter<'a>;

    fn into_iter(self) -> Self::IntoIter {
        ChunkIter {
            chunk: self,
            offset: 0,
        }
    }
}

pub struct ChunkIter<'a> {
    chunk: &'a Chunk,
    offset: usize,
}

impl<'a> Iterator for ChunkIter<'a> {
    type Item = &'a OpCode;

    fn next(&mut self) -> Option<Self::Item> {
        if self.offset < self.chunk.count {
            let result: &OpCode;
            unsafe {
                result = self
                    .chunk
                    .code
                    .offset(self.offset as isize)
                    .as_ref()
                    .expect("Could not read OpCode.");
            }
            self.offset += 1;
            Some(result)
        } else {
            None
        }
    }
}