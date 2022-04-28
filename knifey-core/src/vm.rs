use crate::api::chunk::dissassemble_instruction;
use crate::data::{Chunk, Instruction, Value};
use crate::error::InterpretError;

const STACK_MAX: usize = 256;

// TODO: it might be worth taking a ref of Chunk.
pub struct Vm {
    chunk: Chunk,
    ip: usize,
    stack_top: usize,
    stack: [Value; STACK_MAX],
}

// NOTE: This is kind of frilly to save some lines of code.
macro_rules! binary_op {
    ($self:ident, $t:tt) => {
        let b = $self.pop();
        let a = $self.pop();
        $self.push(a $t b);
    }
}

impl Vm {
    // NOTE: we don't keep a static singleton.
    pub fn new() -> Vm {
        Vm {
            chunk: Chunk::new(),
            ip: 0,
            stack_top: 0,
            stack: [0.; STACK_MAX],
        }
    }

    pub fn run(&mut self) -> Result<(), InterpretError> {
        loop {
            // TODO: hide me as a configuration.
            print!("          ");
            for slot in self.stack.iter().take(self.stack_top) {
                print!("[ {} ]", slot);
            }
            println!();
            dissassemble_instruction(&self.chunk, self.ip);

            let byte = self.chunk.code[self.ip];
            self.ip += 1;
            let instruction =
                Instruction::from_byte(byte).map_err(InterpretError::InstructionError)?;
            match instruction {
                Instruction::Return => {
                    println!("{}", self.pop());
                    return Ok(());
                }
                Instruction::Constant => {
                    let constant_address = self.chunk.code[self.ip];
                    self.ip += 1;
                    let constant = self.chunk.constants[constant_address as usize];
                    self.push(constant);
                }
                Instruction::Negate => {
                    let value = self.pop();
                    self.push(-value);
                }
                Instruction::Add => {
                    binary_op!(self, +);
                }
                Instruction::Subtract => {
                    binary_op!(self, -);
                }
                Instruction::Multiply => {
                    binary_op!(self, *);
                }
                Instruction::Divide => {
                    binary_op!(self, /);
                }
            }
        }
    }

    // TODO: take ref of Chunk.
    pub fn interpret(&mut self, chunk: Chunk) -> Result<(), InterpretError> {
        self.chunk = chunk.clone();
        self.run()
    }

    pub fn reset_stack(&mut self) {
        self.stack_top = 0;
    }

    pub fn push(&mut self, value: Value) {
        self.stack[self.stack_top] = value;
        self.stack_top += 1;
    }

    pub fn pop(&mut self) -> Value {
        self.stack_top -= 1;
        self.stack[self.stack_top]
    }
}
