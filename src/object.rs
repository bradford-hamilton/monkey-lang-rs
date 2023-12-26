use crate::environment::Environment;
use crate::{
    ast::{BlockStatement, Expression, Identifier},
    bytecode,
};
use lazy_static::lazy_static;
use std::collections::HashMap;
use std::rc::Rc;
use std::sync::Arc;

/// Object represents monkey's object system. Every value in monkey-lang
/// must implement this interface.
pub trait Object {
    fn object_type(&self) -> ObjectType;
    fn inspect(&self) -> String;
}

/// Represents the different available object types in monkey-lang.
pub enum ObjectType {
    Integer,
    Boolean,
    Null,
    ReturnValue,
    Error,
    Function,
    String,
    Builtin,
    Array,
    Hash,
    CompiledFunction,
    Closure,
}

struct Integer {
    pub value: i64,
}

impl Object for Integer {
    fn object_type(&self) -> ObjectType {
        ObjectType::Integer
    }
    fn inspect(&self) -> String {
        self.value.to_string()
    }
}

struct Null {}

impl Object for Null {
    fn object_type(&self) -> ObjectType {
        ObjectType::Null
    }
    fn inspect(&self) -> String {
        String::from("null")
    }
}

struct Array {
    pub elements: Vec<Box<dyn Object>>,
}

impl Object for Array {
    fn object_type(&self) -> ObjectType {
        ObjectType::Array
    }
    fn inspect(&self) -> String {
        let elements_str: Vec<String> = self.elements.iter().map(|e| e.inspect()).collect();
        format!("[{}]", elements_str.join(", "))
    }
}

struct Boolean {
    pub value: bool,
}

impl Object for Boolean {
    fn object_type(&self) -> ObjectType {
        ObjectType::Boolean
    }
    fn inspect(&self) -> String {
        self.value.to_string()
    }
}

/// CompiledFunction holds the instructions we get from the compilation of a function
/// literal and is an object::Object, which means we can add it as a constant to our
/// compiler::bytecode and load it in the VM. It also holds the NumLocals which we pass
/// to the VM to allocate the correct amount of stack space ("hole") to save the local
/// bindings
// TODO: check back in on this comment after implementing.
struct CompiledFunction {
    pub instructions: bytecode::Instructions,
    pub num_locals: usize,
    pub num_params: usize,
}

impl Object for CompiledFunction {
    fn object_type(&self) -> ObjectType {
        ObjectType::CompiledFunction
    }
    fn inspect(&self) -> String {
        format!("CompiledFunction[{:p}]", self)
    }
}

struct Closure {
    pub func: CompiledFunction,
    pub free: Vec<Box<dyn Object>>,
}

impl Object for Closure {
    fn object_type(&self) -> ObjectType {
        ObjectType::Closure
    }
    fn inspect(&self) -> String {
        format!("Closure[{:p}]", self)
    }
}

struct Error {
    pub message: String,
}

impl Object for Error {
    fn object_type(&self) -> ObjectType {
        ObjectType::Error
    }
    fn inspect(&self) -> String {
        self.message.clone()
    }
}

struct Function {
    pub params: Vec<Identifier>,
    pub body: BlockStatement,
    pub env: Environment,
}

impl Object for Function {
    fn object_type(&self) -> ObjectType {
        ObjectType::Function
    }
    fn inspect(&self) -> String {
        let params: Vec<String> = self.params.iter().map(|p| p.string()).collect();
        format!("func({}) {{\n{}\n}}", params.join(", "), self.body.string())
    }
}

struct Str {
    pub value: String,
}

impl Object for Str {
    fn object_type(&self) -> ObjectType {
        ObjectType::String
    }
    fn inspect(&self) -> String {
        self.value.clone()
    }
}

struct Hash {
    pairs: HashMap<HashKey, HashPair>,
}

impl Object for Hash {
    fn object_type(&self) -> ObjectType {
        ObjectType::Hash
    }
    fn inspect(&self) -> String {
        let pairs: Vec<String> = self.pairs.values().map(|pair| pair.inspect()).collect();
        format!("{{ {} }}", pairs.join(", "))
    }
}

struct HashPair {
    key: Box<dyn Object>,
    value: Box<dyn Object>,
}

impl HashPair {
    fn inspect(&self) -> String {
        format!("{}: {}", self.key.inspect(), self.value.inspect())
    }
}

struct HashKey {
    object_type: ObjectType,
    value: u64,
}

/// Hashable is one method called hash_key. Any object that that can be used as a HashKey must
/// implement this interface (ObjectType::String, ObjectType::Boolean, ObjectType::Integer).
pub trait Hashable {
    fn hash_key(&self) -> HashKey;
}

impl Hashable for Boolean {
    fn hash_key(&self) -> HashKey {
        HashKey {
            object_type: ObjectType::Boolean,
            value: if self.value { 1 } else { 0 },
        }
    }
}

impl Hashable for Integer {
    fn hash_key(&self) -> HashKey {
        HashKey {
            object_type: ObjectType::Integer,
            value: self.value as u64,
        }
    }
}

impl Hashable for Str {
    fn hash_key(&self) -> HashKey {
        use std::collections::hash_map::DefaultHasher;
        use std::hash::{Hash, Hasher};

        let mut hasher = DefaultHasher::new();
        self.value.hash(&mut hasher);
        let hash = hasher.finish();

        HashKey {
            object_type: ObjectType::String,
            value: hash,
        }
    }
}

struct ReturnValue {
    pub value: dyn Object,
}

impl Object for ReturnValue {
    fn object_type(&self) -> ObjectType {
        ObjectType::ReturnValue
    }
    fn inspect(&self) -> String {
        self.value.inspect()
    }
}

struct Builtin {
    func: Arc<dyn Fn(Vec<Rc<dyn Object>>) -> Rc<dyn Object> + Send + Sync>,
}

impl Builtin {
    fn new<F>(func: F) -> Self
    where
        F: Fn(Vec<Rc<dyn Object>>) -> Rc<dyn Object> + 'static + Send + Sync,
    {
        Builtin {
            func: Arc::new(func),
        }
    }
    fn call(&self, args: Vec<Rc<dyn Object>>) -> Rc<dyn Object> {
        (self.func)(args)
    }
}

impl Clone for Builtin {
    fn clone(&self) -> Self {
        Builtin {
            func: Arc::clone(&self.func),
        }
    }
}

impl Object for Builtin {
    fn object_type(&self) -> ObjectType {
        ObjectType::Builtin
    }
    fn inspect(&self) -> String {
        String::from("builtin function")
    }
}

lazy_static! {
    static ref BUILTINS: HashMap<String, Builtin> = {
        let mut m = HashMap::new();
        m.insert("len".to_string(), Builtin::new(b_len));
        m.insert("print".to_string(), Builtin::new(b_print));
        m.insert("first".to_string(), Builtin::new(b_first));
        m.insert("last".to_string(), Builtin::new(b_last));
        m.insert("rest".to_string(), Builtin::new(b_rest));
        m.insert("push".to_string(), Builtin::new(b_push));
        m.insert("pop".to_string(), Builtin::new(b_pop));
        m.insert("split".to_string(), Builtin::new(b_split));
        m.insert("join".to_string(), Builtin::new(b_join));
        m
    };
}

fn get_builtin_by_name(name: &str) -> Option<Builtin> {
    BUILTINS.get(name).cloned()
}

fn b_len(args: Vec<Rc<dyn Object>>) -> Rc<dyn Object> {
    todo!();
}

fn b_print(args: Vec<Rc<dyn Object>>) -> Rc<dyn Object> {
    todo!();
}

fn b_first(args: Vec<Rc<dyn Object>>) -> Rc<dyn Object> {
    todo!();
}

fn b_last(args: Vec<Rc<dyn Object>>) -> Rc<dyn Object> {
    todo!();
}

fn b_rest(args: Vec<Rc<dyn Object>>) -> Rc<dyn Object> {
    todo!();
}

fn b_push(args: Vec<Rc<dyn Object>>) -> Rc<dyn Object> {
    todo!();
}

fn b_pop(args: Vec<Rc<dyn Object>>) -> Rc<dyn Object> {
    todo!();
}

fn b_split(args: Vec<Rc<dyn Object>>) -> Rc<dyn Object> {
    todo!();
}

fn b_join(args: Vec<Rc<dyn Object>>) -> Rc<dyn Object> {
    todo!();
}

fn new_error(message: String) -> Error {
    Error { message }
}
