use crate::data::{Chunk, Instruction, Value};
use crate::error::InstructionError;

// TODO: Debug + Display trait impls for Instruction, possibly.
impl Instruction {
    pub fn name(&self) -> String {
        match self {
            Instruction::Return => String::from("OP_RETURN"),
            Instruction::Constant => String::from("OP_CONSTANT"),
            Instruction::Negate => String::from("OP_NEGATE"),
            Instruction::Add => String::from("OP_ADD"),
            Instruction::Subtract => String::from("OP_SUBTRACT"),
            Instruction::Multiply => String::from("OP_MULTIPLY"),
            Instruction::Divide => String::from("OP_DIVIDE"),
        }
    }

    // NOTE: I forget to add these. there should be an easier way to have
    // the compiler guide me to do it.
    pub fn from_byte(byte: u8) -> Result<Instruction, InstructionError> {
        match byte {
            0 => Ok(Instruction::Return),
            1 => Ok(Instruction::Constant),
            2 => Ok(Instruction::Negate),
            3 => Ok(Instruction::Add),
            4 => Ok(Instruction::Subtract),
            5 => Ok(Instruction::Multiply),
            6 => Ok(Instruction::Divide),
            _ => Err(InstructionError::UnknownOpcode(byte)),
        }
    }
}

fn unknown_instruction(opcode: u8, offset: usize) -> usize {
    println!("Unknown opcode {}", opcode);
    offset + 1
}

fn simple_instruction(instruction: &Instruction, offset: usize) -> usize {
    println!("{}", instruction.name());
    offset + 1
}

fn constant_instruction(instruction: &Instruction, chunk: &Chunk, offset: usize) -> usize {
    let constant = chunk.code[offset + 1];
    print!("{:-16} {:4} '", instruction.name(), constant);
    println!("{}'", chunk.constants[constant as usize]);
    offset + 2
}

pub fn dissassemble_instruction(chunk: &Chunk, offset: usize) -> usize {
    print!("{:04} ", offset);
    if offset > 0 && chunk.lines[offset] == chunk.lines[offset - 1] {
        print!("   | ");
    } else {
        print!("{:4} ", chunk.lines[offset]);
    }
    // FIXME: an offset may exceed the bounds of the stored code.
    // hence it would be an error that should be known when
    // disassembling. This error should not be in the form of a panic.
    let opcode = chunk.code[offset];
    let instruction = Instruction::from_byte(opcode);
    match instruction {
        Ok(instruction @ Instruction::Return) => simple_instruction(&instruction, offset),
        Ok(instruction @ Instruction::Constant) => {
            constant_instruction(&instruction, chunk, offset)
        }
        Ok(instruction @ Instruction::Negate) => simple_instruction(&instruction, offset),
        Ok(instruction @ Instruction::Add) => simple_instruction(&instruction, offset),
        Ok(instruction @ Instruction::Subtract) => simple_instruction(&instruction, offset),
        Ok(instruction @ Instruction::Multiply) => simple_instruction(&instruction, offset),
        Ok(instruction @ Instruction::Divide) => simple_instruction(&instruction, offset),
        Err(InstructionError::UnknownOpcode(opcode)) => unknown_instruction(opcode, offset),
    }
}

impl Chunk {
    /// Create a new bytecode chunk.
    pub fn new() -> Chunk {
        Chunk {
            code: vec![],
            constants: vec![],
            lines: vec![],
        }
    }

    pub fn write(&mut self, byte: u8, line: i32) {
        self.code.push(byte);
        self.lines.push(line);
    }

    pub fn add_constant(&mut self, value: Value) -> usize {
        self.constants.push(value);
        self.constants.len() - 1
    }

    pub fn dissassemble(&self, name: &str) {
        println!("== {} ==", name);
        let mut offset = 0;
        while offset < self.code.len() {
            offset = dissassemble_instruction(self, offset);
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn chunks_store_code() {
        let mut chunk = Chunk::new();
        chunk.write(Instruction::Return as u8, 123);
        assert_eq!(chunk.code, vec![0],);
        assert_eq!(chunk.lines, vec![123],);
    }
}
