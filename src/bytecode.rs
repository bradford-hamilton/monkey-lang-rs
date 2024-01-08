use lazy_static::lazy_static;
use std::collections::HashMap;
use std::ops::{Index, IndexMut};

#[derive(Clone, Debug, PartialEq)]
pub struct Instructions(pub Vec<u8>);

impl Instructions {
    pub fn new(data: Vec<u8>) -> Self {
        Instructions(data)
    }

    pub fn to_string(&self) -> String {
        let mut output = String::new();
        let mut i = 0;

        while i < self.0.len() {
            match lookup(Opcode::from(self.0[i])) {
                Ok(def) => {
                    let (operands, read) = read_operands(def, &self.0[i + 1..]);
                    output.push_str(&format!("{:04} {}\n", i, fmt_instruction(def, operands)));
                    i += 1 + read;
                }
                Err(err) => {
                    output.push_str(&format!("ERROR: {}\n", err));
                    i += 1;
                }
            }
        }

        output
    }

    pub fn len(&self) -> i64 {
        self.0.len() as i64
    }

    pub fn as_vec_u8(&self) -> Vec<u8> {
        self.0.to_vec()
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

impl Extend<u8> for Instructions {
    fn extend<T: IntoIterator<Item = u8>>(&mut self, iter: T) {
        self.0.extend(iter);
    }
}

fn fmt_instruction(definition: &Definition, operands: Vec<u16>) -> String {
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

pub fn read_uint16(ins: &[u8]) -> u16 {
    ((ins[0] as u16) << 8) | (ins[1] as u16)
}

pub fn read_uint8(ins: &[u8]) -> u8 {
    ins[0]
}

struct Definition {
    name: String,
    operand_widths: Vec<usize>,
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
#[repr(u8)]
pub enum Opcode {
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

impl Opcode {
    pub fn to_u8(&self) -> u8 {
        *self as u8
    }
}

fn lookup(op: Opcode) -> Result<&'static Definition, String> {
    match DEFINITIONS.get(&op) {
        Some(def) => Ok(def),
        None => Err(format!("Opcode {:?} undefined", op)),
    }
}

pub fn make_instruction(op: Opcode, operands: Vec<i32>) -> Vec<u8> {
    let def = match DEFINITIONS.get(&op) {
        Some(def) => def,
        None => return Vec::new(),
    };

    let mut instruction_len = 1;
    for &w in &def.operand_widths {
        instruction_len += w;
    }

    let mut instruction = vec![0; instruction_len];
    instruction[0] = op as u8;

    let mut offset = 1;

    for (&o, &width) in operands.iter().zip(def.operand_widths.iter()) {
        match width {
            2 => {
                let bytes = (o as u16).to_be_bytes();
                instruction[offset..offset + 2].copy_from_slice(&bytes);
            }
            1 => instruction[offset] = o.try_into().expect("operand too large for u8"),
            _ => unimplemented!("unsupported operand width"),
        }
        offset += width;
    }

    instruction
}

lazy_static! {
    static ref DEFINITIONS: HashMap<Opcode, Definition> = {
        let mut m = HashMap::new();

        m.insert(
            Opcode::OpConstant,
            Definition {
                name: String::from("OpConstant"),
                operand_widths: vec![2],
            },
        );
        m.insert(
            Opcode::OpAdd,
            Definition {
                name: String::from("OpAdd"),
                operand_widths: vec![],
            },
        );
        m.insert(
            Opcode::OpPop,
            Definition {
                name: String::from("OpPop"),
                operand_widths: vec![],
            },
        );
        m.insert(
            Opcode::OpSub,
            Definition {
                name: String::from("OpSub"),
                operand_widths: vec![],
            },
        );
        m.insert(
            Opcode::OpMul,
            Definition {
                name: String::from("OpMul"),
                operand_widths: vec![],
            },
        );
        m.insert(
            Opcode::OpDiv,
            Definition {
                name: String::from("OpDiv"),
                operand_widths: vec![],
            },
        );
        m.insert(
            Opcode::OpMod,
            Definition {
                name: String::from("OpMod"),
                operand_widths: vec![],
            },
        );
        m.insert(
            Opcode::OpTrue,
            Definition {
                name: String::from("OpTrue"),
                operand_widths: vec![],
            },
        );
        m.insert(
            Opcode::OpFalse,
            Definition {
                name: String::from("OpFalse"),
                operand_widths: vec![],
            },
        );
        m.insert(
            Opcode::OpEqualEqual,
            Definition {
                name: String::from("OpEqualEqual"),
                operand_widths: vec![],
            },
        );
        m.insert(
            Opcode::OpNotEqual,
            Definition {
                name: String::from("OpNotEqual"),
                operand_widths: vec![],
            },
        );
        m.insert(
            Opcode::OpGreater,
            Definition {
                name: String::from("OpGreater"),
                operand_widths: vec![],
            },
        );
        m.insert(
            Opcode::OpGreaterEqual,
            Definition {
                name: String::from("OpGreaterEqual"),
                operand_widths: vec![],
            },
        );
        m.insert(
            Opcode::OpAnd,
            Definition {
                name: String::from("OpAnd"),
                operand_widths: vec![],
            },
        );
        m.insert(
            Opcode::OpOr,
            Definition {
                name: String::from("OpOr"),
                operand_widths: vec![],
            },
        );
        m.insert(
            Opcode::OpMinus,
            Definition {
                name: String::from("OpMinus"),
                operand_widths: vec![],
            },
        );
        m.insert(
            Opcode::OpBang,
            Definition {
                name: String::from("OpBang"),
                operand_widths: vec![],
            },
        );
        m.insert(
            Opcode::OpPlusPlus,
            Definition {
                name: String::from("OpPlusPlus"),
                operand_widths: vec![],
            },
        );
        m.insert(
            Opcode::OpMinusMinus,
            Definition {
                name: String::from("OpMinusMinus"),
                operand_widths: vec![],
            },
        );
        m.insert(
            Opcode::OpJumpNotTruthy,
            Definition {
                name: String::from("OpJumpNotTruthy"),
                operand_widths: vec![2],
            },
        );
        m.insert(
            Opcode::OpJump,
            Definition {
                name: String::from("OpJump"),
                operand_widths: vec![2],
            },
        );
        m.insert(
            Opcode::OpNull,
            Definition {
                name: String::from("OpNull"),
                operand_widths: vec![],
            },
        );
        m.insert(
            Opcode::OpGetGlobal,
            Definition {
                name: String::from("OpGetGlobal"),
                operand_widths: vec![2],
            },
        );
        m.insert(
            Opcode::OpSetGlobal,
            Definition {
                name: String::from("OpSetGlobal"),
                operand_widths: vec![2],
            },
        );
        m.insert(
            Opcode::OpArray,
            Definition {
                name: String::from("OpArray"),
                operand_widths: vec![2],
            },
        );
        m.insert(
            Opcode::OpHash,
            Definition {
                name: String::from("OpHash"),
                operand_widths: vec![2],
            },
        );
        m.insert(
            Opcode::OpIndex,
            Definition {
                name: String::from("OpIndex"),
                operand_widths: vec![],
            },
        );
        m.insert(
            Opcode::OpCall,
            Definition {
                name: String::from("OpCall"),
                operand_widths: vec![1],
            },
        );
        m.insert(
            Opcode::OpReturnValue,
            Definition {
                name: String::from("OpReturnValue"),
                operand_widths: vec![],
            },
        );
        m.insert(
            Opcode::OpReturn,
            Definition {
                name: String::from("OpReturn"),
                operand_widths: vec![],
            },
        );
        m.insert(
            Opcode::OpGetLocal,
            Definition {
                name: String::from("OpGetLocal"),
                operand_widths: vec![1],
            },
        );
        m.insert(
            Opcode::OpSetLocal,
            Definition {
                name: String::from("OpSetLocal"),
                operand_widths: vec![1],
            },
        );
        m.insert(
            Opcode::OpGetBuiltin,
            Definition {
                name: String::from("OpGetBuiltin"),
                operand_widths: vec![1],
            },
        );
        m.insert(
            Opcode::OpClosure,
            Definition {
                name: String::from("OpClosure"),
                operand_widths: vec![2, 1],
            },
        );
        m.insert(
            Opcode::OpGetFree,
            Definition {
                name: String::from("OpGetFree"),
                operand_widths: vec![1],
            },
        );
        m.insert(
            Opcode::OpCurrentClosure,
            Definition {
                name: String::from("OpCurrentClosure"),
                operand_widths: vec![],
            },
        );

        m
    };
}

impl From<u8> for Opcode {
    fn from(byte: u8) -> Self {
        match byte {
            0 => Opcode::OpConstant,
            1 => Opcode::OpAdd,
            2 => Opcode::OpPop,
            3 => Opcode::OpSub,
            4 => Opcode::OpMul,
            5 => Opcode::OpDiv,
            6 => Opcode::OpMod,
            7 => Opcode::OpTrue,
            8 => Opcode::OpFalse,
            9 => Opcode::OpEqualEqual,
            10 => Opcode::OpNotEqual,
            11 => Opcode::OpGreater,
            12 => Opcode::OpGreaterEqual,
            13 => Opcode::OpAnd,
            14 => Opcode::OpOr,
            15 => Opcode::OpMinus,
            16 => Opcode::OpBang,
            17 => Opcode::OpPlusPlus,
            18 => Opcode::OpMinusMinus,
            19 => Opcode::OpJumpNotTruthy,
            20 => Opcode::OpJump,
            21 => Opcode::OpNull,
            22 => Opcode::OpGetGlobal,
            23 => Opcode::OpSetGlobal,
            24 => Opcode::OpArray,
            25 => Opcode::OpHash,
            26 => Opcode::OpIndex,
            27 => Opcode::OpCall,
            28 => Opcode::OpReturnValue,
            29 => Opcode::OpReturn,
            30 => Opcode::OpGetLocal,
            31 => Opcode::OpSetLocal,
            32 => Opcode::OpGetBuiltin,
            33 => Opcode::OpClosure,
            34 => Opcode::OpGetFree,
            35 => Opcode::OpCurrentClosure,
            _ => panic!("undefined opcode: {}", byte),
        }
    }
}
