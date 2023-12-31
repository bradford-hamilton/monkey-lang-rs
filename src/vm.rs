use crate::ast::ExpressionKey;
use crate::ast::{ArrayLiteral, HashLiteral};
use crate::bytecode::Opcode;
use crate::compiler::Bytecode;
use crate::frame::Frame;
use crate::object::{
    Array, Boolean, Closure, CompiledFunc, HashKey, HashMp, Integer, Null, Object, ObjectType, Str,
};
use crate::object::{HashPair, Hashable};
use std::collections::HashMap;
use std::rc::Rc;

// Integer defining the size of our stack
const STACK_SIZE: i64 = 2048;
// Defines the maximum frames allowed in the VM.
const MAX_FRAMES: i64 = 1024;
// The upper limit on the number of global bindings our VM supports.
const GLOBALS_SIZE: i64 = 65536;

// Defines the virtual machine. It holds the constant pool, instructions, a stack,
// and an integer (index) that points to the next free slot in the stack.
pub struct VirtualMachine<'a> {
    constants: Vec<&'a Box<dyn Object>>,
    stack: Vec<Rc<dyn Object>>,
    sp: u64,
    globals: Vec<Box<dyn Object>>,
    frames: Vec<Frame>,
    frames_index: usize,
}

impl<'a> VirtualMachine<'a> {
    pub fn new(bytecode: Bytecode<'a>) -> Self {
        let main_func = CompiledFunc {
            instructions: bytecode.instructions,
            num_locals: 0,
            num_params: 0,
        };
        let main_closure = Closure {
            func: main_func,
            free: vec![],
        };
        let main_frame = Frame::new(main_closure, 0);
        let mut frames = Vec::with_capacity(MAX_FRAMES as usize);

        frames.push(main_frame);

        VirtualMachine {
            constants: bytecode.constants,
            stack: Vec::with_capacity(STACK_SIZE as usize),
            sp: 0,
            globals: Vec::with_capacity(GLOBALS_SIZE as usize),
            frames,
            frames_index: 1,
        }
    }

    fn current_frame(&self) -> &Frame {
        &self.frames[self.frames_index - 1]
    }

    fn push_frame(&mut self, frame: Frame) {
        if self.frames_index >= self.frames.capacity() {
            panic!("exceeded maximum frames capacity");
        }
        self.frames[self.frames_index] = frame;
        self.frames_index += 1;
    }

    fn pop_frame(&mut self) -> &Frame {
        if self.frames_index < 1 {
            panic!("cannot pop frame, frame index is 0");
        }
        self.frames_index -= 1;
        &self.frames[self.frames_index]
    }

    fn last_popped_stack_element(&self) -> Rc<dyn Object> {
        self.stack[self.sp as usize].clone()
    }
    // pub fn last_popped_stack_element(&self) -> Option<&Box<dyn Object>> {
    //     if self.sp > 0 {
    //         self.stack.get(self.sp - 1)
    //     } else {
    //         None
    //     }
    // }
    // This also applies to some other vm methods here that panic instead
    // of using errors.

    fn push(&mut self, obj: Rc<dyn Object>) {
        if self.sp >= STACK_SIZE as u64 {
            panic!("stack overflow");
        }

        self.stack[self.sp as usize] = obj;
        self.sp += 1;
    }

    fn pop(&mut self) -> Rc<dyn Object> {
        let obj = self.stack[self.sp as usize - 1].clone();
        self.sp -= 1;
        obj
    }

    fn execute_binary_integer_operation(
        &mut self,
        op: Opcode,
        left: Box<dyn Object>,
        right: Box<dyn Object>,
    ) {
        let left_value = match left.as_any().downcast_ref::<Integer>() {
            Some(integer) => integer.value,
            None => panic!("left operand is not an integer"),
        };

        let right_value = match right.as_any().downcast_ref::<Integer>() {
            Some(integer) => integer.value,
            None => panic!("right operand is not an integer"),
        };

        let result = match op {
            Opcode::OpAdd => left_value + right_value,
            Opcode::OpSub => left_value - right_value,
            Opcode::OpMul => left_value * right_value,
            Opcode::OpDiv => left_value / right_value,
            Opcode::OpMod => left_value % right_value,
            _ => panic!("unknown integer operator: {:?}", op),
        };

        self.push(Rc::new(Integer { value: result }));
    }

    fn execute_binary_string_operation(
        &mut self,
        op: Opcode,
        left: Box<dyn Object>,
        right: Box<dyn Object>,
    ) {
        if op != Opcode::OpAdd {
            panic!("unknown String operator: {:?}", op);
        }

        let left_value = match left.as_any().downcast_ref::<Str>() {
            Some(string) => &string.value,
            None => panic!("left operand is not a string"),
        };

        let right_value = match right.as_any().downcast_ref::<Str>() {
            Some(string) => &string.value,
            None => panic!("right operand is not a string"),
        };

        self.push(Rc::new(Str {
            value: left_value.to_owned() + right_value,
        }));
    }

    fn execute_bang_operator(&mut self) {
        let operand = self.pop();
        let result = match operand.as_ref().object_type() {
            ObjectType::Boolean => {
                let value = operand
                    .as_ref()
                    .as_any()
                    .downcast_ref::<Boolean>()
                    .unwrap()
                    .value;
                Rc::new(Boolean { value: !value })
            }
            ObjectType::Null => Rc::new(Boolean { value: true }),
            _ => Rc::new(Boolean { value: false }),
        };

        self.push(result);
    }

    // TODO:
    // func (vm *VM) executePostfixOperator(op code.Opcode, ins code.Instructions, ip int)

    fn build_array(&self, start_index: usize, end_index: usize) -> Rc<dyn Object> {
        let elements = self.stack[start_index..end_index]
            .iter()
            .cloned() // efficient clone of Rc pointers
            .collect::<Vec<Rc<dyn Object>>>();

        Rc::new(Array { elements })
    }

    fn build_hash(&self, start_index: usize, end_index: usize) -> Rc<dyn Object> {
        let mut pairs: HashMap<HashKey, _> = HashMap::new();

        for i in (start_index..end_index).step_by(2) {
            let key = Rc::clone(&self.stack[i]);
            let value = Rc::clone(&self.stack[i + 1]);

            let hash_key: HashKey = if let Some(key) = key.as_any().downcast_ref::<Str>() {
                key.hash_key()
            } else if let Some(key) = key.as_any().downcast_ref::<Boolean>() {
                key.hash_key()
            } else if let Some(key) = key.as_any().downcast_ref::<Integer>() {
                key.hash_key()
            } else {
                panic!("unusable as a hash key: {}", key.inspect());
            };

            pairs.insert(hash_key, HashPair { key, value });
        }

        Rc::new(HashMp { pairs })
    }

    fn execute_array_index(&mut self, array: Rc<dyn Object>, index: Rc<dyn Object>) {
        let array_obj = array.as_any().downcast_ref::<Array>().unwrap();
        let index_obj = index.as_any().downcast_ref::<Integer>().unwrap();

        let i = index_obj.value;
        let max = array_obj.elements.len() as i64 - 1;

        if i < 0 || i > max {
            self.push(Rc::new(Null {}));
        } else {
            let element = Rc::clone(&array_obj.elements[i as usize]);
            self.push(element);
        }
    }

    fn execute_hash_index(&mut self, hash: Rc<dyn Object>, index: Rc<dyn Object>) {
        let hash_object = hash.as_any().downcast_ref::<HashMp>().unwrap();
        let hash_key = if let Some(key) = index.as_any().downcast_ref::<Str>() {
            key.hash_key()
        } else if let Some(key) = index.as_any().downcast_ref::<Boolean>() {
            key.hash_key()
        } else if let Some(key) = index.as_any().downcast_ref::<Integer>() {
            key.hash_key()
        } else {
            panic!("unusable as a hash key: {}", index.inspect());
        };

        match hash_object.pairs.get(&hash_key) {
            Some(pair) => {
                let value = Rc::clone(&pair.value);
                self.push(value)
            }
            None => self.push(Rc::new(Null {})),
        }
    }
}

fn native_bool_to_boolean_obj(input: bool) -> Box<dyn Object> {
    if input {
        Box::new(Boolean { value: true })
    } else {
        Box::new(Boolean { value: false })
    }
}

// Coerce the different object types to booleans for truthy/falsey values.
fn coerce_obj_to_native_bool(object: Box<dyn Object>) -> bool {
    // Check for Boolean type
    if object.object_type() == ObjectType::Boolean {
        if let Some(boolean_obj) = object.as_any().downcast_ref::<Boolean>() {
            return boolean_obj.value;
        }
    }
    // Check for String type
    else if object.object_type() == ObjectType::String {
        if let Some(string_obj) = object.as_any().downcast_ref::<Str>() {
            return !string_obj.value.is_empty();
        }
    }
    // Check for Integer type
    else if object.object_type() == ObjectType::Integer {
        if let Some(integer_obj) = object.as_any().downcast_ref::<Integer>() {
            return integer_obj.value != 0;
        }
    }
    // Check for Array type
    else if object.object_type() == ObjectType::Array {
        if let Some(array_obj) = object.as_any().downcast_ref::<ArrayLiteral>() {
            return !array_obj.elements.is_empty();
        }
    }
    // Check for Hash type
    else if object.object_type() == ObjectType::Hash {
        if let Some(hash_obj) = object.as_any().downcast_ref::<HashLiteral>() {
            return !hash_obj.pairs.is_empty();
        }
    }
    // Check for Null type
    else if object.object_type() == ObjectType::Null {
        return false;
    }

    // Default case for other object types
    true
}
