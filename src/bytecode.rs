use std::ops::{Index, IndexMut};

pub struct Instructions(Vec<u8>);

impl Instructions {
    pub fn new(data: Vec<u8>) -> Self {
        Instructions(data)
    }

    pub fn to_string(&self) -> String {
        let mut output = String::new();
        let mut i = 0;

        while i < self.0.len() {
            match lookup(self.0[i]) {
                Ok(def) => {
                    let (operands, read) = read_operands(&def, &self.0[i + 1..]);
                    output.push_str(&format!("{:04} {}\n", i, fmt_instruction(def, operands)));
                    i += 1 + read;
                }
                Err(err) => {
                    output.push_str(&format!("ERROR: {}\n", err));
                    i += 1; // Move to next instruction
                }
            }
        }

        output
    }
}

impl Index<usize> for Instructions {
    type Output = u8;

    fn index(&self, index: usize) -> &Self::Output {
        &self.0[index]
    }
}

impl IndexMut<usize> for Instructions {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.0[index]
    }
}

fn fmt_instruction(definition: Definition, operands: Vec<u16>) -> String {
    let operand_count = definition.operand_widths.len();

    if operands.len() != operand_count {
        return format!(
            "ERROR: operand len {} does not match defined {}\n",
            operands.len(),
            operand_count
        );
    }

    match operand_count {
        0 => definition.name.clone(),
        1 => format!("{} {}", definition.name, operands[0]),
        2 => format!("{} {} {}", definition.name, operands[0], operands[1]),
        _ => format!("ERROR: unhandled operandCount for {}\n", definition.name),
    }
}

fn read_operands(def: &Definition, ins: &[u8]) -> (Vec<u16>, usize) {
    let mut operands = Vec::with_capacity(def.operand_widths.len());
    let mut offset = 0;

    for &width in &def.operand_widths {
        match width {
            2 => {
                operands.push(read_uint16(&ins[offset..offset + 2]));
                offset += 2;
            }
            1 => {
                operands.push(read_uint8(&ins[offset..offset + 1]) as u16);
                offset += 1;
            }
            _ => unimplemented!("Operand width not supported"),
        }
    }

    (operands, offset)
}

fn read_uint16(ins: &[u8]) -> u16 {
    ((ins[0] as u16) << 8) | (ins[1] as u16)
}

fn read_uint8(ins: &[u8]) -> u8 {
    ins[0]
}

struct Definition {
    name: String,
    operand_widths: Vec<i64>,
}

type Opcode = u8;

#[derive(Copy, Clone)]
pub enum Opcodes {
    /// Constants
    OpConstant,

    /// Aritmatic & stack pop
    OpAdd,
    OpPop,
    OpSub,
    OpMul,
    OpDiv,
    OpMod,

    /// Boolean
    OpTrue,
    OpFalse,

    /// Comparison/Logical
    OpEqualEqual,
    OpNotEqual,
    OpGreater,
    OpGreaterEqual,
    OpAnd,
    OpOr,

    /// Prefix/unary
    OpMinus,
    OpBang,

    /// Postfix/unary
    OpPlusPlus,
    OpMinusMinus,

    // Jump for conditionals
    /// Jump to alternative if consequence is not truthy
    OpJumpNotTruthy,
    /// Jump no matter what (if we evaluate consequence and dont want the alternative)
    OpJump,

    /// Null
    OpNull,

    /// Get and Set global variables
    OpGetGlobal,
    OpSetGlobal,

    /// Data structures and index access
    OpArray,
    OpHash,
    OpIndex,

    /// Call expression, return with value, return without value
    OpCall,
    /// Tells VM to leave value on top of stack
    OpReturnValue,
    /// Tells VM implicit return of Null
    OpReturn,

    /// Get and Set local bindings
    OpGetLocal,
    OpSetLocal,

    /// Builtin functions
    OpGetBuiltin,

    /// Closures and it's variables
    OpClosure,
    OpGetFree,
    OpCurrentClosure,
}

impl Opcodes {
    pub fn to_opcode(&self) -> Opcode {
        *self as Opcode
    }
}

fn lookup(opcode: u8) -> Result<Definition, String> {
    todo!()
}
