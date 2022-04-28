/// Errors pertaining to instructions and opcodes.
#[derive(Debug, PartialEq)]
pub enum InstructionError {
    UnknownOpcode(u8),
}

#[derive(Debug, PartialEq)]
pub enum InterpretError {
    InstructionError(InstructionError),
    CompileError,
    RuntimeError,
}
