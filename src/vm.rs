use crate::ast::{ArrayLiteral, HashLiteral};
use crate::builtins::{Builtin, BUILTINS};
use crate::bytecode::{read_uint16, read_uint8, Instructions, Opcode};
use crate::compiler::Bytecode;
use crate::frame::Frame;
use crate::object::{
    Array, Boolean, Closure, CompiledFunc, Error, HashKey, HashMp, Integer, Null, Object,
    ObjectType, Str,
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
    constants: Vec<&'a Rc<dyn Object>>,
    stack: Vec<Rc<dyn Object>>,
    sp: u64,
    globals: Vec<Rc<dyn Object>>,
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

    pub fn new_with_global_state(bytecode: Bytecode<'a>, state: Vec<Rc<dyn Object>>) -> Self {
        let mut vm = VirtualMachine::new(bytecode);
        vm.globals = state;
        vm
    }

    pub fn run(&mut self) {
        let mut ip: usize;
        let mut ins: &Instructions;
        let mut op: Opcode;

        while self.current_frame().ip < self.current_frame().instructions().len() - 1 {
            self.current_frame_mut().ip += 1;

            ip = self.current_frame().ip as usize;
            ins = self.current_frame().instructions();
            op = Opcode::from(ins[ip]);

            match op {
                Opcode::OpConstant => {
                    let const_index = read_uint16(&ins.0[ip + 1..]) as usize;
                    self.current_frame_mut().ip += 2;

                    let constant = self.constants[const_index];
                    self.push(constant.clone());
                }

                Opcode::OpPop => {
                    self.pop();
                }

                Opcode::OpAdd | Opcode::OpSub | Opcode::OpMul | Opcode::OpDiv | Opcode::OpMod => {
                    self.execute_binary_operation(op);
                }

                Opcode::OpTrue => {
                    self.push(Rc::new(Boolean { value: true }));
                }

                Opcode::OpFalse => {
                    self.push(Rc::new(Boolean { value: false }));
                }

                Opcode::OpEqualEqual
                | Opcode::OpNotEqual
                | Opcode::OpGreater
                | Opcode::OpGreaterEqual => {
                    self.execute_comparison(op);
                }

                Opcode::OpAnd | Opcode::OpOr => {
                    self.execute_logical_operator(op);
                }

                Opcode::OpBang => {
                    self.execute_bang_operator();
                }

                Opcode::OpMinus => {
                    self.execute_minus_operator();
                }

                // TODO: Opcode::OpPlusPlus | Opcode::OpMinusMinus => { self.execute_postfix_operator(op, ins, ip); }
                Opcode::OpJump => {
                    let pos = read_uint16(&ins.0[ip + 1..]) as i64;
                    self.current_frame_mut().ip = pos - 1;
                }

                Opcode::OpJumpNotTruthy => {
                    let pos = read_uint16(&ins.0[ip + 1..]) as i64;
                    self.current_frame_mut().ip += 2;

                    let condition = self.pop();
                    if !is_truthy(condition) {
                        self.current_frame_mut().ip = pos - 1;
                    }
                }

                Opcode::OpNull => {
                    self.push(Rc::new(Null {}));
                }

                Opcode::OpSetGlobal => {
                    let global_index = read_uint16(&ins.0[ip + 1..]) as usize;
                    self.current_frame_mut().ip += 2;
                    let element = self.pop();
                    self.globals.insert(global_index, element);
                }

                Opcode::OpGetGlobal => {
                    let global_index = read_uint16(&ins.0[ip + 1..]) as usize;
                    self.current_frame_mut().ip += 2;
                    if global_index >= self.globals.len() {
                        panic!("global index {} is out of bounds", global_index);
                    }
                    self.push(Rc::clone(&self.globals[global_index]));
                }

                Opcode::OpArray => {
                    let num_elements = read_uint16(&ins.0[ip + 1..]) as usize;
                    self.current_frame_mut().ip += 2;

                    let array = self.build_array(self.sp as usize - num_elements, self.sp as usize);
                    self.sp -= num_elements as u64;

                    self.push(array);
                }

                Opcode::OpHash => {
                    let num_elements = read_uint16(&ins.0[ip + 1..]) as usize;
                    self.current_frame_mut().ip += 2;

                    let hash = self.build_hash(self.sp as usize - num_elements, self.sp as usize);
                    self.sp -= num_elements as u64;

                    self.push(hash);
                }

                Opcode::OpIndex => {
                    let index = self.pop();
                    let left = self.pop();

                    self.execute_index_expr(left, index);
                }

                Opcode::OpCall => {
                    let num_args = read_uint8(&ins.0[ip + 1..]) as usize;

                    self.current_frame_mut().ip += 1;
                    self.execute_call(num_args);
                }

                Opcode::OpReturnValue => {
                    let return_value = self.pop();
                    let frame = self.pop_frame();

                    self.sp = frame.base_pointer as u64 - 1;
                    self.push(return_value);
                }

                Opcode::OpReturn => {
                    let frame = self.pop_frame();

                    self.sp = frame.base_pointer as u64 - 1;
                    self.push(Rc::new(Null {}));
                }

                Opcode::OpSetLocal => {
                    let local_index = read_uint8(&ins.0[ip + 1..]) as usize;

                    self.current_frame_mut().ip += 1;

                    let frame = self.current_frame();
                    let base_pointer = frame.base_pointer as usize;

                    self.stack[base_pointer + local_index] = Rc::clone(&self.pop());
                }

                Opcode::OpGetLocal => {
                    let local_index = read_uint8(&ins.0[ip + 1..]) as usize;
                    self.current_frame_mut().ip += 1;

                    let frame = self.current_frame();
                    let base_pointer = frame.base_pointer as usize;

                    let local_var = Rc::clone(&self.stack[base_pointer + local_index]);
                    self.push(local_var);
                }

                Opcode::OpGetBuiltin => {
                    let builtin_index = read_uint8(&ins.0[ip + 1..]) as usize;
                    self.current_frame_mut().ip += 1;

                    let definition = BUILTINS[builtin_index].clone();
                    let builtin = definition.1;

                    self.push(Rc::new(builtin));
                }

                Opcode::OpClosure => {
                    let const_index = read_uint16(&ins.0[ip + 1..]) as usize;
                    let num_free = read_uint8(&ins.0[ip + 3..]) as usize;

                    self.current_frame_mut().ip += 3;
                    self.push_closure(const_index, num_free);
                }

                Opcode::OpGetFree => {
                    let free_index = read_uint8(&ins.0[ip + 1..]) as usize;
                    self.current_frame_mut().ip += 1;

                    let current_closure = self.current_frame().closure.clone();
                    self.push(current_closure.free[free_index].clone());
                }

                Opcode::OpCurrentClosure => {
                    self.push(self.current_frame().closure.clone());
                }

                _ => unimplemented!("invalid opcode: {:?}", op),
            }
        }
    }

    fn current_frame(&self) -> &Frame {
        &self.frames[self.frames_index - 1]
    }

    fn current_frame_mut(&mut self) -> &mut Frame {
        &mut self.frames[self.frames_index - 1]
    }

    fn push_frame(&mut self, frame: Frame) {
        if self.frames_index >= self.frames.capacity() {
            panic!("exceeded maximum frames capacity");
        }
        self.frames.insert(self.frames_index, frame);
        self.frames_index += 1;
    }

    fn pop_frame(&mut self) -> &Frame {
        if self.frames_index < 1 {
            panic!("cannot pop frame, frame index is 0");
        }
        self.frames_index -= 1;
        &self.frames[self.frames_index]
    }

    pub fn last_popped_stack_element(&self) -> Rc<dyn Object> {
        self.stack[self.sp as usize].clone()
    }

    fn push(&mut self, obj: Rc<dyn Object>) {
        if self.sp >= STACK_SIZE as u64 {
            panic!("stack overflow");
        }

        if self.sp as usize >= self.stack.len() {
            self.stack.resize(self.sp as usize + 1, Rc::new(Null {}));
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
                    self.push(native_bool_to_boolean_obj(
                        left.inspect() == right.inspect(),
                    ));
                }
                Opcode::OpNotEqual => {
                    self.push(native_bool_to_boolean_obj(
                        left.inspect() != right.inspect(),
                    ));
                }
                _ => panic!("unknown operator: {:?}", op),
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
        let cl = Rc::new(Closure {
            func: closure.func.clone(),
            free: closure.free.clone(),
        });
        let frame = Frame::new(cl, self.sp as i64 - num_args as i64);
        let frame_bp = frame.base_pointer;

        self.push_frame(frame);
        self.sp = frame_bp as u64 + closure_num_locals as u64;
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

        self.sp -= num_args as u64 - 1;

        if result.as_any().is::<Null>() {
            self.push(Rc::new(Null {}));
        } else {
            self.push(result);
        }
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{ast::RootNode, compiler::Compiler, lexer::Lexer, parser::Parser};

    struct VmTestCase {
        input: String,
        expected: Expected,
    }

    fn run_vm_tests(tests: Vec<VmTestCase>) {
        for test in tests {
            let program = parse(test.input);
            let mut comp = Compiler::new();

            match comp.compile(&program) {
                Ok(_) => {
                    let mut vm = VirtualMachine::new(comp.bytecode());
                    vm.run();
                    let stack_elem = vm.last_popped_stack_element();
                    test_expected_object(test.expected, stack_elem);
                }
                Err(err) => panic!("err compiling: {:?}", err),
            }
        }
    }

    fn parse(input: String) -> RootNode {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        parser.parse_program()
    }

    enum Expected {
        Integer(i64),
        Boolean(bool),
        Null,
        Str(String),
        IntegerArray(Vec<i64>),
        StringArray(Vec<String>),
        HashMap(HashMap<HashKey, i64>),
        Error(String),
    }

    fn test_expected_object(expected: Expected, actual: Rc<dyn Object>) {
        match expected {
            Expected::Integer(expected) => {
                test_integer_object(expected, &actual);
            }
            Expected::Boolean(expected) => {
                test_boolean_object(expected, &actual);
            }
            Expected::Null => {
                assert!(actual.as_any().is::<Null>());
            }
            Expected::Str(expected) => {
                test_string_object(expected, &actual);
            }
            Expected::IntegerArray(expected) => {
                let array = actual.as_any().downcast_ref::<Array>().unwrap();
                assert_eq!(array.elements.len(), expected.len());
                for (expected_elem, actual_elem) in expected.iter().zip(array.elements.iter()) {
                    test_integer_object(*expected_elem, actual_elem);
                }
            }
            Expected::StringArray(expected) => {
                match actual.as_any().downcast_ref::<Array>() {
                    Some(array) => {
                        assert_eq!(array.elements.len(), expected.len());
                        for (expected_elem, actual_elem) in
                            expected.iter().zip(array.elements.iter())
                        {
                            let string_obj = actual_elem.as_any().downcast_ref::<Str>().unwrap();
                            assert_eq!(&string_obj.value, expected_elem);
                        }
                    }
                    None => {
                        // Handle case where actual is not an Array, but a Str
                        if let Some(string_obj) = actual.as_any().downcast_ref::<Str>() {
                            // Expected string array should contain only one element in this case
                            assert_eq!(&string_obj.value, &expected[0]);
                        } else {
                            panic!(
                                "Expected a String or String Array, found: {:?}",
                                actual.inspect()
                            );
                        }
                    }
                }
            }
            Expected::HashMap(expected) => {
                let hash = actual.as_any().downcast_ref::<HashMp>().unwrap();
                assert_eq!(hash.pairs.len(), expected.len());
                for (expected_key, expected_value) in expected {
                    let pair = hash.pairs.get(&expected_key).unwrap();
                    test_integer_object(expected_value, &pair.value);
                }
            }
            Expected::Error(expected_message) => {
                let err_obj = actual.as_any().downcast_ref::<Error>().unwrap();
                assert_eq!(err_obj.message, expected_message);
            }
        }
    }

    fn test_integer_object(expected: i64, actual: &Rc<dyn Object>) {
        if let Some(result) = actual.as_any().downcast_ref::<Integer>() {
            assert_eq!(
                result.value, expected,
                "object has wrong value. Expected: {}. Got: {}",
                expected, result.value
            );
        } else {
            panic!("object is not an Integer. Got: {:?}", actual.inspect());
        }
    }

    fn test_boolean_object(expected: bool, actual: &Rc<dyn Object>) {
        if let Some(result) = actual.as_any().downcast_ref::<Boolean>() {
            assert_eq!(
                result.value, expected,
                "object has wrong value. Expected: {}, Got: {}",
                expected, result.value
            );
        } else {
            panic!("object is not Boolean. Got: {:?}", actual.inspect());
        }
    }

    fn test_string_object(expected: String, actual: &Rc<dyn Object>) {
        if let Some(result) = actual.as_any().downcast_ref::<Str>() {
            assert_eq!(
                result.value, expected,
                "Object has wrong value. Expected: {:?}, Got: {:?}",
                expected, result.value
            );
        } else {
            panic!("Object is not a String. Got: {:?}", actual.inspect());
        }
    }

    #[test]
    fn test_integer_arithmetic() {
        let tests = vec![
            VmTestCase {
                input: "1".to_string(),
                expected: Expected::Integer(1),
            },
            VmTestCase {
                input: "2".to_string(),
                expected: Expected::Integer(2),
            },
            VmTestCase {
                input: "1 + 2".to_string(),
                expected: Expected::Integer(3),
            },
            VmTestCase {
                input: "1 - 2".to_string(),
                expected: Expected::Integer(-1),
            },
            VmTestCase {
                input: "1 * 2".to_string(),
                expected: Expected::Integer(2),
            },
            VmTestCase {
                input: "4 / 2".to_string(),
                expected: Expected::Integer(2),
            },
            VmTestCase {
                input: "50 / 2 * 2 + 10 - 5".to_string(),
                expected: Expected::Integer(55),
            },
            VmTestCase {
                input: "5 + 5 + 5 + 5 - 10".to_string(),
                expected: Expected::Integer(10),
            },
            VmTestCase {
                input: "2 * 2 * 2 * 2 * 2".to_string(),
                expected: Expected::Integer(32),
            },
            VmTestCase {
                input: "5 * 2 + 10".to_string(),
                expected: Expected::Integer(20),
            },
            VmTestCase {
                input: "5 + 2 * 10".to_string(),
                expected: Expected::Integer(25),
            },
            VmTestCase {
                input: "5 * (2 + 10)".to_string(),
                expected: Expected::Integer(60),
            },
            VmTestCase {
                input: "-5".to_string(),
                expected: Expected::Integer(-5),
            },
            VmTestCase {
                input: "-10".to_string(),
                expected: Expected::Integer(-10),
            },
            VmTestCase {
                input: "-50 + 100 + -50".to_string(),
                expected: Expected::Integer(0),
            },
            VmTestCase {
                input: "(5 + 10 * 2 + 15 / 3) * 2 + -10".to_string(),
                expected: Expected::Integer(50),
            },
            VmTestCase {
                input: "10 % 3".to_string(),
                expected: Expected::Integer(1),
            },
            VmTestCase {
                input: "(10 % 3) + 8 % 3".to_string(),
                expected: Expected::Integer(3),
            },
        ];

        run_vm_tests(tests);
    }

    #[test]
    fn test_boolean_expressions() {
        let tests = vec![
            VmTestCase {
                input: "true".to_string(),
                expected: Expected::Boolean(true),
            },
            VmTestCase {
                input: "false".to_string(),
                expected: Expected::Boolean(false),
            },
            VmTestCase {
                input: "1 < 2".to_string(),
                expected: Expected::Boolean(true),
            },
            VmTestCase {
                input: "1 > 2".to_string(),
                expected: Expected::Boolean(false),
            },
            VmTestCase {
                input: "1 < 1".to_string(),
                expected: Expected::Boolean(false),
            },
            VmTestCase {
                input: "1 > 1".to_string(),
                expected: Expected::Boolean(false),
            },
            VmTestCase {
                input: "2 <= 2".to_string(),
                expected: Expected::Boolean(true),
            },
            VmTestCase {
                input: "2 >= 2".to_string(),
                expected: Expected::Boolean(true),
            },
            VmTestCase {
                input: "1 == 1".to_string(),
                expected: Expected::Boolean(true),
            },
            VmTestCase {
                input: "1 != 1".to_string(),
                expected: Expected::Boolean(false),
            },
            VmTestCase {
                input: "1 == 2".to_string(),
                expected: Expected::Boolean(false),
            },
            VmTestCase {
                input: "1 != 2".to_string(),
                expected: Expected::Boolean(true),
            },
            VmTestCase {
                input: "true == true".to_string(),
                expected: Expected::Boolean(true),
            },
            VmTestCase {
                input: "false == false".to_string(),
                expected: Expected::Boolean(true),
            },
            VmTestCase {
                input: "true == false".to_string(),
                expected: Expected::Boolean(false),
            },
            VmTestCase {
                input: "true != false".to_string(),
                expected: Expected::Boolean(true),
            },
            VmTestCase {
                input: "false != true".to_string(),
                expected: Expected::Boolean(true),
            },
            VmTestCase {
                input: "\"monkey\" != \"monkey\"".to_string(),
                expected: Expected::Boolean(false),
            },
            VmTestCase {
                input: "\"monkey\" == \"monkey\"".to_string(),
                expected: Expected::Boolean(true),
            },
            VmTestCase {
                input: "(1 < 2) == true".to_string(),
                expected: Expected::Boolean(true),
            },
            VmTestCase {
                input: "(1 < 2) == false".to_string(),
                expected: Expected::Boolean(false),
            },
            VmTestCase {
                input: "(1 > 2) == true".to_string(),
                expected: Expected::Boolean(false),
            },
            VmTestCase {
                input: "(1 > 2) == false".to_string(),
                expected: Expected::Boolean(true),
            },
            VmTestCase {
                input: "(2 <= 2) == true".to_string(),
                expected: Expected::Boolean(true),
            },
            VmTestCase {
                input: "(2 <= 2) == false".to_string(),
                expected: Expected::Boolean(false),
            },
            VmTestCase {
                input: "(2 >= 2) == true".to_string(),
                expected: Expected::Boolean(true),
            },
            VmTestCase {
                input: "(2 >= 2) == false".to_string(),
                expected: Expected::Boolean(false),
            },
            VmTestCase {
                input: "!true".to_string(),
                expected: Expected::Boolean(false),
            },
            VmTestCase {
                input: "!false".to_string(),
                expected: Expected::Boolean(true),
            },
            VmTestCase {
                input: "!5".to_string(),
                expected: Expected::Boolean(false),
            },
            VmTestCase {
                input: "!!true".to_string(),
                expected: Expected::Boolean(true),
            },
            VmTestCase {
                input: "!!false".to_string(),
                expected: Expected::Boolean(false),
            },
            VmTestCase {
                input: "!!5".to_string(),
                expected: Expected::Boolean(true),
            },
            VmTestCase {
                input: "!(if (false) { 5; })".to_string(),
                expected: Expected::Boolean(true),
            },
            VmTestCase {
                input: "true && true".to_string(),
                expected: Expected::Boolean(true),
            },
            VmTestCase {
                input: "true && false".to_string(),
                expected: Expected::Boolean(false),
            },
            VmTestCase {
                input: "true || false".to_string(),
                expected: Expected::Boolean(true),
            },
            VmTestCase {
                input: "!(true || false)".to_string(),
                expected: Expected::Boolean(false),
            },
            VmTestCase {
                input: "!(if (true && false) { 5; })".to_string(),
                expected: Expected::Boolean(true),
            },
        ];

        run_vm_tests(tests);
    }

    #[test]
    fn test_conditionals() {
        let tests = vec![
            VmTestCase {
                input: "if (true) { 10 }".to_string(),
                expected: Expected::Integer(10),
            },
            VmTestCase {
                input: "if (true) { 10 } else { 20 }".to_string(),
                expected: Expected::Integer(10),
            },
            VmTestCase {
                input: "if (false) { 10 } else { 20 }".to_string(),
                expected: Expected::Integer(20),
            },
            VmTestCase {
                input: "if (1) { 10 }".to_string(),
                expected: Expected::Integer(10),
            },
            VmTestCase {
                input: "if (1 < 2) { 10 }".to_string(),
                expected: Expected::Integer(10),
            },
            VmTestCase {
                input: "if (1 < 2) { 10 } else { 20 }".to_string(),
                expected: Expected::Integer(10),
            },
            VmTestCase {
                input: "if (1 > 2) { 10 } else { 20 }".to_string(),
                expected: Expected::Integer(20),
            },
            VmTestCase {
                input: "if (1 > 2) { 10 }".to_string(),
                expected: Expected::Null,
            },
            VmTestCase {
                input: "if (false) { 10 }".to_string(),
                expected: Expected::Null,
            },
            VmTestCase {
                input: "if ((if (false) { 10 })) { 10 } else { 20 }".to_string(),
                expected: Expected::Integer(20),
            },
        ];

        run_vm_tests(tests);
    }

    #[test]
    fn test_global_let_statements() {
        let tests = vec![
            VmTestCase {
                input: "let one = 1; one".to_string(),
                expected: Expected::Integer(1),
            },
            VmTestCase {
                input: "let one = 1; let two = 2; one + two".to_string(),
                expected: Expected::Integer(3),
            },
            VmTestCase {
                input: "let one = 1; let two = one + one; one + two".to_string(),
                expected: Expected::Integer(3),
            },
            // TODO: after postfix
            // VmTestCase {
            //     input: "let one = 1; one++; one;".to_string(),
            //     expected: Expected::Integer(2),
            // },
            // VmTestCase {
            //     input: "let one = 1; one--; one;".to_string(),
            //     expected: Expected::Integer(0),
            // },
        ];

        run_vm_tests(tests);
    }

    #[test]
    fn test_global_const_statements() {
        let tests = vec![
            VmTestCase {
                input: "const one = 1; one".to_string(),
                expected: Expected::Integer(1),
            },
            VmTestCase {
                input: "const one = 1; const two = 2; one + two".to_string(),
                expected: Expected::Integer(3),
            },
            VmTestCase {
                input: "const one = 1; const two = one + one; one + two".to_string(),
                expected: Expected::Integer(3),
            },
        ];

        run_vm_tests(tests);
    }

    #[test]
    fn test_string_expressions() {
        let tests = vec![
            VmTestCase {
                input: "\"monkey\"".to_string(),
                expected: Expected::Str("monkey".to_string()),
            },
            VmTestCase {
                input: "\"mon\" + \"key\"".to_string(),
                expected: Expected::Str("monkey".to_string()),
            },
            VmTestCase {
                input: "\"mon\" + \"key\" + \"banana\"".to_string(),
                expected: Expected::Str("monkeybanana".to_string()),
            },
        ];

        run_vm_tests(tests);
    }

    #[test]
    fn test_array_literals() {
        let tests = vec![
            VmTestCase {
                input: "[]".to_string(),
                expected: Expected::IntegerArray(vec![]),
            },
            VmTestCase {
                input: "[1, 2, 3]".to_string(),
                expected: Expected::IntegerArray(vec![1, 2, 3]),
            },
            VmTestCase {
                input: "[1 + 2, 3 * 4, 5 + 6]".to_string(),
                expected: Expected::IntegerArray(vec![3, 12, 11]),
            },
        ];

        run_vm_tests(tests);
    }

    #[test]
    fn test_hash_literals() {
        let empty_hash = HashMap::new();

        let mut hash_one = HashMap::new();
        hash_one.insert(Integer { value: 1 }.hash_key(), 2);
        hash_one.insert(Integer { value: 2 }.hash_key(), 3);

        let mut hash_two = HashMap::new();
        hash_two.insert(Integer { value: 2 }.hash_key(), 4);
        hash_two.insert(Integer { value: 6 }.hash_key(), 16);

        let tests = vec![
            VmTestCase {
                input: "{}".to_string(),
                expected: Expected::HashMap(empty_hash),
            },
            VmTestCase {
                input: "{1: 2, 2: 3}".to_string(),
                expected: Expected::HashMap(hash_one),
            },
            VmTestCase {
                input: "{1 + 1: 2 * 2, 3 + 3: 4 * 4}".to_string(),
                expected: Expected::HashMap(hash_two),
            },
        ];

        run_vm_tests(tests);
    }

    #[test]
    fn test_index_expressions() {
        use std::collections::HashMap;

        let mut hash_one = HashMap::new();
        hash_one.insert(Integer { value: 1 }.hash_key(), 1);
        hash_one.insert(Integer { value: 2 }.hash_key(), 2);

        let tests = vec![
            VmTestCase {
                input: "[1, 2, 3][1]".to_string(),
                expected: Expected::Integer(2),
            },
            VmTestCase {
                input: "[1, 2, 3][0 + 2]".to_string(),
                expected: Expected::Integer(3),
            },
            VmTestCase {
                input: "[[1, 1, 1]][0][0]".to_string(),
                expected: Expected::Integer(1),
            },
            VmTestCase {
                input: "[][0]".to_string(),
                expected: Expected::Null,
            },
            VmTestCase {
                input: "[1, 2, 3][99]".to_string(),
                expected: Expected::Null,
            },
            VmTestCase {
                input: "[1][-1]".to_string(),
                expected: Expected::Null,
            },
            VmTestCase {
                input: "{1: 1, 2: 2}[1]".to_string(),
                expected: Expected::Integer(1),
            },
            VmTestCase {
                input: "{1: 1, 2: 2}[2]".to_string(),
                expected: Expected::Integer(2),
            },
            VmTestCase {
                input: "{1: 1}[0]".to_string(),
                expected: Expected::Null,
            },
            VmTestCase {
                input: "{}[0]".to_string(),
                expected: Expected::Null,
            },
        ];

        run_vm_tests(tests);
    }

    #[test]
    fn test_calling_functions_without_arguments() {
        let tests = vec![
            VmTestCase {
                input: "
                let fivePlusTen = func() { 5 + 10; };
                fivePlusTen();
            "
                .to_string(),
                expected: Expected::Integer(15),
            },
            VmTestCase {
                input: "
                let one = func() { 1; };
                let two = func() { 2; };
                one() + two()
            "
                .to_string(),
                expected: Expected::Integer(3),
            },
            VmTestCase {
                input: "
                let a = func() { 1 };
                let b = func() { a() + 1 };
                let c = func() { b() + 1 };
                c();
            "
                .to_string(),
                expected: Expected::Integer(3),
            },
        ];

        run_vm_tests(tests);
    }

    #[test]
    fn test_functions_with_return_statement() {
        let tests = vec![
            VmTestCase {
                input: "
                let earlyExit = func() { return 99; 100; };
                earlyExit();
            "
                .to_string(),
                expected: Expected::Integer(99),
            },
            VmTestCase {
                input: "
                let earlyExit = func() { return 99; return 100; };
                earlyExit();
            "
                .to_string(),
                expected: Expected::Integer(99),
            },
            // TODO: after postfix:
            // VmTestCase {
            //     input: "
            //     let postfixReturn = func() { let one = 1; one++; return one; };
            //     postfixReturn();
            // "
            //     .to_string(),
            //     expected: Expected::Integer(2),
            // },
        ];

        run_vm_tests(tests);
    }

    #[test]
    fn test_functions_without_return_value() {
        let tests = vec![
            VmTestCase {
                input: "
                let noReturn = func() { };
                noReturn();
            "
                .to_string(),
                expected: Expected::Null,
            },
            VmTestCase {
                input: "
                let noReturn = func() { };
                let noReturnTwo = func() { noReturn(); };
                noReturn();
                noReturnTwo();
            "
                .to_string(),
                expected: Expected::Null,
            },
        ];

        run_vm_tests(tests);
    }

    #[test]
    fn test_first_class_functions() {
        let tests = vec![
            VmTestCase {
                input: "
                let returnsOne = func() { 1; };
                let returnsOneReturner = func() { returnsOne; };
                returnsOneReturner()();
            "
                .to_string(),
                expected: Expected::Integer(1),
            },
            VmTestCase {
                input: "
                let returnsOneReturner = func() {
                    let returnsOne = func() { 1; };
                    returnsOne;
                };
                returnsOneReturner()();
            "
                .to_string(),
                expected: Expected::Integer(1),
            },
        ];

        run_vm_tests(tests);
    }

    #[test]
    fn test_calling_functions_with_bindings() {
        let tests = vec![
            VmTestCase {
                input: "
                let one = func() { let one = 1; one };
                one();
            "
                .to_string(),
                expected: Expected::Integer(1),
            },
            VmTestCase {
                input: "
                let oneAndTwo = func() { let one = 1; let two = 2; one + two; };
                oneAndTwo();
            "
                .to_string(),
                expected: Expected::Integer(3),
            },
            VmTestCase {
                input: "
                let oneAndTwo = func() { let one = 1; let two = 2; one + two; };
                let threeAndFour = func() { let three = 3; let four = 4; three + four; };
                oneAndTwo() + threeAndFour();
            "
                .to_string(),
                expected: Expected::Integer(10),
            },
            VmTestCase {
                input: "
                let firstFoobar = func() { let foobar = 50; foobar; };
                let secondFoobar = func() { let foobar = 100; foobar; };
                firstFoobar() + secondFoobar();
            "
                .to_string(),
                expected: Expected::Integer(150),
            },
            VmTestCase {
                input: "
                let globalSeed = 50;
                let minusOne = func() {
                    let num = 1;
                    globalSeed - num;
                };
                let minusTwo = func() {
                    let num = 2;
                    globalSeed - num;
                };
                minusOne() + minusTwo();
            "
                .to_string(),
                expected: Expected::Integer(97),
            },
        ];

        run_vm_tests(tests);
    }

    #[test]
    fn test_calling_functions_with_arguments_and_bindings() {
        let tests = vec![
            VmTestCase {
                input: "
                let identity = func(a) { a; };
                identity(4);
            "
                .to_string(),
                expected: Expected::Integer(4),
            },
            VmTestCase {
                input: "
                let sum = func(a, b) { a + b; };
                sum(1, 2);
            "
                .to_string(),
                expected: Expected::Integer(3),
            },
            VmTestCase {
                input: "
                let sum = func(a, b) {
                    let c = a + b;
                    c;
                };
                sum(1, 2);
            "
                .to_string(),
                expected: Expected::Integer(3),
            },
            VmTestCase {
                input: "
                let sum = func(a, b) {
                    let c = a + b;
                    c;
                };
                sum(1, 2) + sum(3, 4);
            "
                .to_string(),
                expected: Expected::Integer(10),
            },
            VmTestCase {
                input: "
                let sum = func(a, b) {
                    let c = a + b;
                    c;
                };
                let outer = func() {
                    sum(1, 2) + sum(3, 4);
                };
                outer();
            "
                .to_string(),
                expected: Expected::Integer(10),
            },
            VmTestCase {
                input: "
                let globalNum = 10;

                let sum = func(a, b) {
                    let c = a + b;
                    c + globalNum;
                };

                let outer = func() {
                    sum(1, 2) + sum(3, 4) + globalNum;
                };

                outer() + globalNum;
            "
                .to_string(),
                expected: Expected::Integer(50),
            },
        ];

        run_vm_tests(tests);
    }

    #[test]
    fn test_calling_functions_with_wrong_arguments() {
        let tests = vec![
            (
                "func() { 1; }(1);".to_string(),
                "wrong number of arguments. Expected: 0. Got: 1".to_string(),
            ),
            (
                "func(a) { a; }();".to_string(),
                "wrong number of arguments. Expected: 1. Got: 0".to_string(),
            ),
            (
                "func(a, b) { a + b; }(1);".to_string(),
                "wrong number of arguments. Expected: 2. Got: 1".to_string(),
            ),
        ];

        for (input, expected_error) in tests {
            let program = parse(input);
            let mut comp = Compiler::new();
            match comp.compile(&program) {
                Ok(_) => {
                    let mut vm = VirtualMachine::new(comp.bytecode());
                    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                        vm.run();
                    }));
                    assert!(result.is_err(), "expected VM to panic but it did not.");

                    let panic_info = result.unwrap_err();
                    if let Some(panic_message) = panic_info.downcast_ref::<String>() {
                        assert_eq!(
                            panic_message, &expected_error,
                            "wrong VM panic message. Want: {}. Got: {}",
                            expected_error, panic_message
                        );
                    } else {
                        panic!("VM panic did not contain a string message.");
                    }
                }
                Err(err) => {
                    panic!("{:?}", err);
                }
            }
        }
    }

    #[test]
    fn test_builtin_functions() {
        let tests = vec![
            VmTestCase {
                input: "len(\"\")".to_string(),
                expected: Expected::Integer(0),
            },
            VmTestCase {
                input: "len(\"four\")".to_string(),
                expected: Expected::Integer(4),
            },
            VmTestCase {
                input: "len(\"hello world\")".to_string(),
                expected: Expected::Integer(11),
            },
            VmTestCase {
                input: "len(1)".to_string(),
                expected: Expected::Error(
                    "Argument to `len` not supported. Got: INTEGER".to_string(),
                ),
            },
            VmTestCase {
                input: "len(\"one\", \"two\")".to_string(),
                expected: Expected::Error(
                    "Wrong number of arguments. Got: 2, Expected: 1".to_string(),
                ),
            },
            VmTestCase {
                input: "len([1, 2, 3])".to_string(),
                expected: Expected::Integer(3),
            },
            VmTestCase {
                input: "len([])".to_string(),
                expected: Expected::Integer(0),
            },
            VmTestCase {
                input: "print(\"hello\", \"world!\")".to_string(),
                expected: Expected::Null,
            },
            VmTestCase {
                input: "first([1, 2, 3])".to_string(),
                expected: Expected::Integer(1),
            },
            VmTestCase {
                input: "first([])".to_string(),
                expected: Expected::Null,
            },
            VmTestCase {
                input: "first(1)".to_string(),
                expected: Expected::Error(
                    "Argument to `first` must be an Array. Got: INTEGER".to_string(),
                ),
            },
            VmTestCase {
                input: "last([1, 2, 3])".to_string(),
                expected: Expected::Integer(3),
            },
            VmTestCase {
                input: "last([])".to_string(),
                expected: Expected::Null,
            },
            VmTestCase {
                input: "last(1)".to_string(),
                expected: Expected::Error(
                    "Argument to `last` must be an Array. Got: INTEGER".to_string(),
                ),
            },
            VmTestCase {
                input: "rest([1, 2, 3])".to_string(),
                expected: Expected::IntegerArray(vec![2, 3]),
            },
            VmTestCase {
                input: "rest([])".to_string(),
                expected: Expected::Null,
            },
            VmTestCase {
                input: "push([], 1)".to_string(),
                expected: Expected::IntegerArray(vec![1]),
            },
            VmTestCase {
                input: "push(1, 1)".to_string(),
                expected: Expected::Error(
                    "Argument to `push` must be an Array. Got: INTEGER".to_string(),
                ),
            },
            VmTestCase {
                input: "pop([1, 2, 3])".to_string(),
                expected: Expected::IntegerArray(vec![1, 2]),
            },
            VmTestCase {
                input: "pop([\"one\", \"two\", \"three\"])".to_string(),
                expected: Expected::StringArray(vec!["one".to_string(), "two".to_string()]),
            },
            VmTestCase {
                input: "pop([])".to_string(),
                expected: Expected::Null,
            },
            VmTestCase {
                input: "pop([1, 2, 3], \"anything else\")".to_string(),
                expected: Expected::Error(
                    "Wrong number of arguments. Got: 2, Expected: 1".to_string(),
                ),
            },
            VmTestCase {
                input: "split(\"My name is brad\")".to_string(),
                expected: Expected::Error(
                    "Wrong number of arguments. Got: 1, Expected: 2".to_string(),
                ),
            },
            VmTestCase {
                input: "split(\"My name is brad\", \" \")".to_string(),
                expected: Expected::StringArray(vec![
                    "My".to_string(),
                    "name".to_string(),
                    "is".to_string(),
                    "brad".to_string(),
                ]),
            },
            VmTestCase {
                input: "split(\"\", \" \")".to_string(),
                expected: Expected::StringArray(vec![]),
            },
            VmTestCase {
                input: "join([\"My\", \"name\", \"is\", \"brad\"])".to_string(),
                expected: Expected::Error(
                    "Wrong number of arguments. Got: 1, Expected: 2".to_string(),
                ),
            },
            VmTestCase {
                input: "join([\"My\", \"name\", \"is\", \"brad\"], \", \")".to_string(),
                expected: Expected::Str("My, name, is, brad".to_string()),
            },
            VmTestCase {
                input: "join(\"My name is brad\", \" \")".to_string(),
                expected: Expected::Error("First argument to `join` must be an Array".to_string()),
            },
            VmTestCase {
                input: "join(\"\", \" \")".to_string(),
                expected: Expected::Error("First argument to `join` must be an Array".to_string()),
            },
        ];

        run_vm_tests(tests);
    }

    #[test]
    fn test_closures() {
        let tests = vec![
            VmTestCase {
                input: "
                let newClosure = func(a) {
                    func() { a; };
                };
                let closure = newClosure(99);
                closure();
            "
                .to_string(),
                expected: Expected::Integer(99),
            },
            VmTestCase {
                input: "
                let newAdder = func(a, b) {
                    func(c) { a + b + c };
                };
                let adder = newAdder(1, 2);
                adder(8);
            "
                .to_string(),
                expected: Expected::Integer(11),
            },
            VmTestCase {
                input: "
                let newAdder = func(a, b) {
                    let c = a + b;
                    func(d) { c + d };
                };
                let adder = newAdder(1, 2);
                adder(8);
            "
                .to_string(),
                expected: Expected::Integer(11),
            },
            VmTestCase {
                input: "
                let newAdderOuter = func(a, b) {
                    let c = a + b;
                    func(d) {
                        let e = d + c;
                        func(f) { e + f; };
                    };
                };
                let newAdderInner = newAdderOuter(1, 2);
                let adder = newAdderInner(3);
                adder(8);
            "
                .to_string(),
                expected: Expected::Integer(14),
            },
            VmTestCase {
                input: "
                let a = 1;
                let newAdderOuter = func(b) {
                    func(c) {
                        func(d) { a + b + c + d };
                    };
                };
                let newAdderInner = newAdderOuter(2);
                let adder = newAdderInner(3);
                adder(8);
            "
                .to_string(),
                expected: Expected::Integer(14),
            },
            VmTestCase {
                input: "
                let newClosure = func(a, b) {
                    let one = func() { a; };
                    let two = func() { b; };
                    func() { one() + two(); };
                };
                let closure = newClosure(9, 90);
                closure();
            "
                .to_string(),
                expected: Expected::Integer(99),
            },
        ];

        run_vm_tests(tests);
    }

    #[test]
    fn test_recursive_functions() {
        let tests = vec![
            VmTestCase {
                input: "
                let countDown = func(x) {
                    if (x == 0) {
                        return 0;
                    } else {
                        countDown(x - 1);
                    }
                };
                countDown(1);
            "
                .to_string(),
                expected: Expected::Integer(0),
            },
            VmTestCase {
                input: "
                let countDown = func(x) {
                    if (x == 0) {
                        return 0;
                    } else {
                        countDown(x - 1);
                    }
                };
                let wrapper = func() {
                    countDown(1);
                };
                wrapper();
            "
                .to_string(),
                expected: Expected::Integer(0),
            },
            VmTestCase {
                input: "
                let wrapper = func() {
                    let countDown = func(x) {
                        if (x == 0) {
                            return 0;
                        } else {
                            countDown(x - 1);
                        }
                    };
                    countDown(1);
                };
                wrapper();
            "
                .to_string(),
                expected: Expected::Integer(0),
            },
        ];

        run_vm_tests(tests);
    }
}
