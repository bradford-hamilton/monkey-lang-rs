use crate::builtins::Builtin;
use crate::environment::Environment;
use crate::{
    ast::{BlockStatement, Expression, Identifier},
    bytecode,
};
use std::any::Any;
use std::collections::HashMap;
use std::fmt::Display;
use std::rc::Rc;

/// Object represents monkey's object system. Every value in monkey-lang
/// must implement this interface.
pub trait Object {
    fn object_type(&self) -> ObjectType;
    fn inspect(&self) -> String;
    fn as_any(&self) -> &dyn Any;
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

impl Display for ObjectType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let type_str = match self {
            ObjectType::Integer => "INTEGER",
            ObjectType::Boolean => "BOOLEAN",
            ObjectType::Null => "NULL",
            ObjectType::ReturnValue => "RETURN_VALUE",
            ObjectType::Error => "ERROR",
            ObjectType::Function => "FUNCTION",
            ObjectType::String => "STRING",
            ObjectType::Builtin => "BUILTIN",
            ObjectType::Array => "ARRAY",
            ObjectType::Hash => "HASH",
            ObjectType::CompiledFunction => "COMPILED_FUNCTION",
            ObjectType::Closure => "CLOSURE",
        };
        write!(f, "{}", type_str)
    }
}

pub struct Integer {
    pub value: i64,
}

impl Object for Integer {
    fn object_type(&self) -> ObjectType {
        ObjectType::Integer
    }
    fn inspect(&self) -> String {
        self.value.to_string()
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}

pub struct Null {}

impl Object for Null {
    fn object_type(&self) -> ObjectType {
        ObjectType::Null
    }
    fn inspect(&self) -> String {
        String::from("null")
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}

pub struct Array {
    pub elements: Vec<Rc<dyn Object>>,
}

impl Object for Array {
    fn object_type(&self) -> ObjectType {
        ObjectType::Array
    }
    fn inspect(&self) -> String {
        let elements_str: Vec<String> = self.elements.iter().map(|e| e.inspect()).collect();
        format!("[{}]", elements_str.join(", "))
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}

pub struct Boolean {
    pub value: bool,
}

impl Object for Boolean {
    fn object_type(&self) -> ObjectType {
        ObjectType::Boolean
    }
    fn inspect(&self) -> String {
        self.value.to_string()
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}

/// CompiledFunction holds the instructions we get from the compilation of a function
/// literal and is an object::Object, which means we can add it as a constant to our
/// compiler::bytecode and load it in the VM. It also holds the NumLocals which we pass
/// to the VM to allocate the correct amount of stack space ("hole") to save the local
/// bindings
// TODO: check back in on this comment after implementing.
pub struct CompiledFunction {
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
    fn as_any(&self) -> &dyn Any {
        self
    }
}

pub struct Closure {
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
    fn as_any(&self) -> &dyn Any {
        self
    }
}

pub struct Error {
    pub message: String,
}

impl Object for Error {
    fn object_type(&self) -> ObjectType {
        ObjectType::Error
    }
    fn inspect(&self) -> String {
        self.message.clone()
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}

pub struct Function {
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
    fn as_any(&self) -> &dyn Any {
        self
    }
}

pub struct Str {
    pub value: String,
}

impl Object for Str {
    fn object_type(&self) -> ObjectType {
        ObjectType::String
    }
    fn inspect(&self) -> String {
        self.value.clone()
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}

pub struct Hash {
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
    fn as_any(&self) -> &dyn Any {
        self
    }
}

pub struct HashPair {
    key: Box<dyn Object>,
    value: Box<dyn Object>,
}

impl HashPair {
    fn inspect(&self) -> String {
        format!("{}: {}", self.key.inspect(), self.value.inspect())
    }
}

pub struct HashKey {
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

pub struct ReturnValue {
    pub value: Box<dyn Object>,
}

impl Object for ReturnValue {
    fn object_type(&self) -> ObjectType {
        ObjectType::ReturnValue
    }
    fn inspect(&self) -> String {
        self.value.inspect()
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Object for Builtin {
    fn object_type(&self) -> ObjectType {
        ObjectType::Builtin
    }
    fn inspect(&self) -> String {
        String::from("builtin function")
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}
