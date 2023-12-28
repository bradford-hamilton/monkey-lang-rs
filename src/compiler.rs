use crate::builtins::BUILTINS;
use crate::bytecode::make;
use crate::bytecode::{self, Instructions, Opcode};
use crate::object::Object;
use crate::symbol_table::SymbolTable;

/// Bytecode contains the Instructions our compiler
/// generated and the constants the compiler evaluated.
struct Bytecode {
    instructions: bytecode::Instructions,
    constants: Vec<Box<dyn Object>>,
}

#[derive(Copy, Clone)]
/// Represents an instruction through an opcode and it's position.
struct EmittedInstruction {
    opcode: Opcode,
    position: usize,
}

impl EmittedInstruction {
    fn new(opcode: Opcode, position: usize) -> Self {
        EmittedInstruction { opcode, position }
    }
}

/// Before we start compiling a function's body (enter a new scope) we push
/// a new CompilationScope on to the scopes stack. While compiling inside
/// this scope, the emit method of the compiler will modify only the fields
/// of the current CompilationScope. Once we're done compiling the function,
/// we leave the scope by popping it off the scopes stack and putting the
/// instructions in a new object::CompiledFunction
struct CompilationScope {
    instructions: Instructions,
    last_instruction: EmittedInstruction,
    prev_instruction: EmittedInstruction,
}

/// defines our compiler with instructions which hold the generated bytecode,
/// constants which serve as our constant pool, and the last and previous
/// instructions
struct Compiler {
    constants: Vec<Box<dyn Object>>,
    symbol_table: SymbolTable,
    scopes: Vec<CompilationScope>,
    scope_index: i64,
}

impl Compiler {
    pub fn new() -> Self {
        let main_scope = CompilationScope {
            instructions: Instructions::new(vec![]),
            last_instruction: EmittedInstruction::new(Opcode::OpNull, 0),
            prev_instruction: EmittedInstruction::new(Opcode::OpNull, 0),
        };
        let mut symbol_table = SymbolTable::new();

        for (i, v) in BUILTINS.iter().enumerate() {
            symbol_table.define_builtin(i as i64, v.0.clone());
        }

        Compiler {
            constants: vec![],
            symbol_table,
            scopes: vec![main_scope],
            scope_index: 0,
        }
    }

    fn bytecode(self) -> Bytecode {
        Bytecode {
            instructions: self.current_instructions(),
            constants: self.constants,
        }
    }

    fn current_instructions(&self) -> Instructions {
        self.scopes[self.scope_index as usize].instructions.clone()
    }

    pub fn emit(&mut self, op: Opcode, operands: Vec<i32>) -> i64 {
        let ins = make(op, operands);
        let pos = self.add_instruction(ins);

        self.set_last_instruction(op, pos as usize);

        pos
    }

    fn add_instruction(&mut self, instruction: Vec<u8>) -> i64 {
        let new_instruction_pos = self.current_instructions().len();
        self.scopes[self.scope_index as usize]
            .instructions
            .extend(instruction);

        new_instruction_pos
    }

    fn set_last_instruction(&mut self, op: Opcode, pos: usize) {
        let previous = self.scopes[self.scope_index as usize].last_instruction;
        let last = EmittedInstruction::new(op, pos);

        self.scopes[self.scope_index as usize].prev_instruction = previous;
        self.scopes[self.scope_index as usize].last_instruction = last;
    }

    pub fn add_constant(&mut self, obj: Box<dyn Object>) -> usize {
        self.constants.push(obj);
        self.constants.len() - 1
    }
}
