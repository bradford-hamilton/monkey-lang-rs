use crate::ast::{ArrayLiteral, HashLiteral};
use crate::builtins::Builtin;
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
        let main_closure = Rc::new(Closure {
            func: main_func,
            free: vec![],
        });
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

    pub fn new_with_global_state(bytecode: Bytecode<'a>, state: Vec<Box<dyn Object>>) -> Self {
        let mut vm = VirtualMachine::new(bytecode);
        vm.globals = state;
        vm
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

    fn execute_call(&mut self, num_args: usize) {
        let callee = Rc::clone(&self.stack[self.sp as usize - 1 - num_args]);

        if let Some(closure) = callee.as_any().downcast_ref::<Closure>() {
            self.call_closure(Rc::new(closure.clone()), num_args);
        } else if let Some(builtin) = callee.as_any().downcast_ref::<Builtin>() {
            self.call_builtin(Rc::new(builtin.clone()), num_args);
        } else {
            panic!("calling non-function and non-builtin");
        }
    }

    fn execute_binary_integer_operation(
        &mut self,
        op: Opcode,
        left: Rc<dyn Object>,
        right: Rc<dyn Object>,
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
        left: Rc<dyn Object>,
        right: Rc<dyn Object>,
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

    fn execute_binary_operation(&mut self, op: Opcode) {
        let right = self.pop();
        let left = self.pop();

        let left_type = left.object_type();
        let right_type = right.object_type();

        match (&left_type, &right_type) {
            (ObjectType::Integer, ObjectType::Integer) => {
                self.execute_binary_integer_operation(op, left, right)
            }
            (ObjectType::String, ObjectType::String) => {
                self.execute_binary_string_operation(op, left, right)
            }
            _ => panic!(
                "unsupported types for binary operation: {:?} {:?}",
                left_type, right_type
            ),
        }
    }

    fn execute_minus_operator(&mut self) {
        let operand = self.pop();

        match operand.as_any().downcast_ref::<Integer>() {
            Some(integer) => {
                self.push(Rc::new(Integer {
                    value: -integer.value,
                }));
            }
            None => panic!("unsupported type for negation: {}", operand.object_type()),
        }
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

    fn execute_integer_comparison(
        &mut self,
        op: Opcode,
        left: Rc<dyn Object>,
        right: Rc<dyn Object>,
    ) {
        let left_value = left.as_any().downcast_ref::<Integer>().unwrap().value;
        let right_value = right.as_any().downcast_ref::<Integer>().unwrap().value;

        match op {
            Opcode::OpEqualEqual => {
                self.push(native_bool_to_boolean_obj(right_value == left_value));
            }
            Opcode::OpNotEqual => self.push(native_bool_to_boolean_obj(right_value != left_value)),
            Opcode::OpGreater => self.push(native_bool_to_boolean_obj(left_value > right_value)),
            Opcode::OpGreaterEqual => {
                self.push(native_bool_to_boolean_obj(left_value >= right_value));
            }
            _ => panic!("unknown operator: {:?}", op),
        }
    }

    fn execute_comparison(&mut self, op: Opcode) {
        let right = self.pop();
        let left = self.pop();

        if left.object_type() == ObjectType::Integer || right.object_type() == ObjectType::Integer {
            self.execute_integer_comparison(op, left, right);
        } else {
            match op {
                Opcode::OpEqualEqual => {
                    if left.object_type() == ObjectType::String
                        && right.object_type() == ObjectType::String
                    {
                        self.push(native_bool_to_boolean_obj(
                            left.inspect() == right.inspect(),
                        ));
                    } else {
                        self.push(native_bool_to_boolean_obj(Rc::ptr_eq(&left, &right)));
                    }
                }
                Opcode::OpNotEqual => {
                    if left.object_type() == ObjectType::String
                        && right.object_type() == ObjectType::String
                    {
                        self.push(native_bool_to_boolean_obj(
                            left.inspect() != right.inspect(),
                        ));
                    } else {
                        self.push(native_bool_to_boolean_obj(!Rc::ptr_eq(&left, &right)));
                    }
                }
                _ => panic!("Unknown operator: {:?}", op),
            }
        }
    }

    fn execute_logical_operator(&mut self, op: Opcode) {
        let right = self.pop();
        let left = self.pop();

        let result = match op {
            Opcode::OpAnd => coerce_obj_to_native_bool(left) && coerce_obj_to_native_bool(right),
            Opcode::OpOr => coerce_obj_to_native_bool(left) || coerce_obj_to_native_bool(right),
            _ => panic!("unknown logical operator: {:?}", op),
        };

        self.push(native_bool_to_boolean_obj(result));
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

    fn execute_index_expr(&mut self, left: Rc<dyn Object>, index: Rc<dyn Object>) {
        match left.object_type() {
            ObjectType::Array => {
                if let Some(index_obj) = index.as_any().downcast_ref::<Integer>() {
                    self.execute_array_index(left, Rc::new(index_obj.clone()));
                } else {
                    panic!(
                        "index operator not supported for type: {}",
                        index.object_type()
                    );
                }
            }
            ObjectType::Hash => self.execute_hash_index(left, index),
            _ => panic!(
                "index operator not supported for type: {}",
                left.object_type()
            ),
        }
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

    fn call_closure(&mut self, closure: Rc<dyn Object>, num_args: usize) {
        let closure = closure.as_any().downcast_ref::<Closure>().unwrap();
        if num_args != closure.func.num_params {
            panic!(
                "wrong number of arguments. Expected: {}. Got: {}",
                closure.func.num_params, num_args
            );
        }
        let closure_num_locals = closure.func.num_locals;
        let c = Rc::new(Closure {
            func: closure.func.clone(),
            free: closure.free.clone(),
        });
        let frame = Frame::new(c, self.sp as i64 - num_args as i64);

        self.push_frame(frame);
        self.sp = self.current_frame().base_pointer as u64 + closure_num_locals as u64;
    }

    fn push_closure(&mut self, const_index: usize, num_free: usize) {
        let constant = &self.constants[const_index];
        let function = constant.as_any().downcast_ref::<CompiledFunc>().unwrap();
        let mut free = Vec::with_capacity(num_free);

        for i in 0..num_free {
            let free_obj = Rc::clone(&self.stack[self.sp as usize - num_free + i]);
            free.push(free_obj);
        }

        self.sp -= num_free as u64;

        let closure = Rc::new(Closure {
            func: function.clone(),
            free,
        });

        self.push(closure);
    }

    fn call_builtin(&mut self, builtin: Rc<dyn Object>, num_args: usize) {
        let builtin_func = builtin.as_any().downcast_ref::<Builtin>().unwrap();
        let args: Vec<Rc<dyn Object>> = self.stack[self.sp as usize - num_args..self.sp as usize]
            .iter()
            .cloned()
            .collect();
        let result = builtin_func.call(args);

        self.sp -= num_args as u64 + 1;

        if result.as_any().is::<Null>() {
            self.push(Rc::new(Null {}));
        } else {
            self.push(result);
        }
    }

    fn is_truthy(obj: Rc<dyn Object>) -> bool {
        match obj.as_ref() {
            obj_ref if obj_ref.as_any().is::<Boolean>() => {
                obj_ref.as_any().downcast_ref::<Boolean>().unwrap().value
            }
            obj_ref if obj_ref.as_any().is::<Null>() => false,
            _ => true,
        }
    }
}

fn native_bool_to_boolean_obj(input: bool) -> Rc<dyn Object> {
    if input {
        Rc::new(Boolean { value: true })
    } else {
        Rc::new(Boolean { value: false })
    }
}

// Coerce the different object types to booleans for truthy/falsey values.
fn coerce_obj_to_native_bool(object: Rc<dyn Object>) -> bool {
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
