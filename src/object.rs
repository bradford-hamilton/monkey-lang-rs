use crate::builtins::Builtin;
use crate::environment::Environment;
use crate::{
    ast::{BlockStatement, Identifier, Node},
    bytecode,
};
use std::any::Any;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::Display;
use std::hash::{Hash, Hasher};
use std::rc::Rc;

/// Object represents monkey's object system. Every value in monkey-lang
/// must implement this interface.
pub trait Object {
    fn object_type(&self) -> ObjectType;
    fn inspect(&self) -> String;
    fn as_any(&self) -> &dyn Any;
}

/// Represents the different available object types in monkey-lang.
#[derive(Hash, PartialEq, Eq, Debug)]
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

#[derive(Clone)]
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
        "null".to_string()
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[derive(Clone)]
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
#[derive(Clone)]
pub struct CompiledFunc {
    pub instructions: bytecode::Instructions,
    pub num_locals: usize,
    pub num_params: usize,
}

impl Object for CompiledFunc {
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

#[derive(Clone)]
pub struct Closure {
    pub func: CompiledFunc,
    pub free: Vec<Rc<dyn Object>>,
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
        format!("Error: {}", self.message)
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}

pub struct Function {
    pub params: Vec<Identifier>,
    pub body: BlockStatement,
    pub env: Rc<RefCell<Environment>>,
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

#[derive(Clone)]
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

/// Hashable is one method called hash_key. Any object that that can be used as a HashKey must
/// implement this interface (ObjectType::String, ObjectType::Boolean, ObjectType::Integer).
pub trait Hashable {
    fn hash_key(&self) -> HashKey;
}

#[derive(PartialEq, Debug, Eq)]
pub struct HashKey {
    object_type: ObjectType,
    value: u64,
}

impl Hash for HashKey {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.object_type.hash(state);
        self.value.hash(state);
    }
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

        let mut hasher = DefaultHasher::new();
        self.value.hash(&mut hasher);
        let hash = hasher.finish();

        HashKey {
            object_type: ObjectType::String,
            value: hash,
        }
    }
}

pub struct HashPair {
    pub key: Rc<dyn Object>,
    pub value: Rc<dyn Object>,
}

pub struct HashMp {
    pub pairs: HashMap<HashKey, HashPair>,
}

impl Object for HashMp {
    fn object_type(&self) -> ObjectType {
        ObjectType::Hash
    }
    fn inspect(&self) -> String {
        let mut pairs_strings = Vec::new();
        for pair in self.pairs.values() {
            let key_inspect = pair.key.inspect();
            let value_inspect = pair.value.inspect();
            pairs_strings.push(format!("{}: {}", key_inspect, value_inspect));
        }
        let pairs_joined = pairs_strings.join(", ");
        format!("{{{}}}", pairs_joined)
    }
    fn as_any(&self) -> &dyn Any {
        self
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

#[cfg(test)]
mod tests {
    use crate::{
        ast::{LetStatement, StringLiteral},
        bytecode::Instructions,
    };

    use super::*;

    #[test]
    fn test_string_hash_key() {
        let hello1 = Str {
            value: String::from("Hello World"),
        };
        let hello2 = Str {
            value: String::from("Hello World"),
        };
        let diff1 = Str {
            value: String::from("My name is johnny"),
        };
        let diff2 = Str {
            value: String::from("My name is johnny"),
        };
        assert_eq!(
            hello1.hash_key(),
            hello2.hash_key(),
            "strings with same content have different hash keys"
        );

        assert_eq!(
            diff1.hash_key(),
            diff2.hash_key(),
            "strings with same content have different hash keys"
        );

        assert_ne!(
            hello1.hash_key(),
            diff1.hash_key(),
            "strings with different content have same hash keys"
        );
    }

    #[test]
    fn test_boolean_hash_key() {
        let true1 = Boolean { value: true };
        let true2 = Boolean { value: true };
        let false1 = Boolean { value: false };
        let false2 = Boolean { value: false };
        assert_eq!(
            true1.hash_key(),
            true2.hash_key(),
            "booleans with same content have different hash keys"
        );
        assert_eq!(
            false1.hash_key(),
            false2.hash_key(),
            "booleans with same content have different hash keys"
        );
        assert_ne!(
            true1.hash_key(),
            false1.hash_key(),
            "booleans with different content have same hash keys"
        );
    }

    #[test]
    fn test_integer_hash_key() {
        let int1 = Integer { value: 10 };
        let int2 = Integer { value: 10 };
        let diff1 = Integer { value: 30 };
        let diff2 = Integer { value: 30 };
        assert_eq!(
            int1.hash_key(),
            int2.hash_key(),
            "integers with same content have different hash keys"
        );
        assert_eq!(
            diff1.hash_key(),
            diff2.hash_key(),
            "integers with same content have different hash keys"
        );
        assert_ne!(
            int1.hash_key(),
            diff1.hash_key(),
            "integers with different content have same hash keys"
        );
    }

    #[test]
    fn test_hash() {
        let mut pairs = HashMap::new();
        pairs.insert(
            HashKey {
                object_type: ObjectType::String,
                value: 1,
            },
            HashPair {
                key: Rc::new(Str {
                    value: String::from("monkey"),
                }),
                value: Rc::new(Str {
                    value: String::from("lang"),
                }),
            },
        );
        let h = HashMp { pairs };
        assert_eq!(
            h.object_type(),
            ObjectType::Hash,
            "Hash object_type() returned wrong type. Expected: HashObj. Got: {:?}",
            h.object_type()
        );
        assert_eq!(
            h.inspect(),
            "{monkey: lang}",
            "h.inspect() returned wrong string representation. Expected: {{monkey: lang}}. Got: {}",
            h.inspect()
        );
    }

    #[test]
    fn test_array() {
        let elements: Vec<Rc<dyn Object>> = vec![
            Rc::new(Integer { value: 1 }),
            Rc::new(Integer { value: 2 }),
            Rc::new(Integer { value: 3 }),
        ];
        let arr = Array { elements };
        assert_eq!(
            arr.object_type(),
            ObjectType::Array,
            "Array object_type() returned wrong type. Expected: ArrayObj. Got: {:?}",
            arr.object_type()
        );
        assert_eq!(
            arr.inspect(),
            "[1, 2, 3]",
            "Array inspect() returned wrong string representation. Expected: [1, 2, 3]. Got: {}",
            arr.inspect()
        );
    }

    #[test]
    fn test_boolean() {
        let b = Boolean { value: false };
        assert_eq!(
            b.object_type(),
            ObjectType::Boolean,
            "Boolean object_type() returned wrong type. Expected: BooleanObj. Got: {:?}",
            b.object_type()
        );
        assert_eq!(
            b.inspect(),
            "false",
            "Boolean inspect() returned wrong string representation. Expected: false. Got: {}",
            b.inspect()
        );
    }

    #[test]
    fn test_closure() {
        let cl = Closure {
            func: CompiledFunc {
                instructions: Instructions::new(vec![]),
                num_locals: 0,
                num_params: 0,
            },
            free: Vec::new(),
        };
        assert_eq!(
            cl.object_type(),
            ObjectType::Closure,
            "Closure object_type() returned wrong type. Expected: ClosureObj. Got: {:?}",
            cl.object_type()
        );
        let expected_inspect = format!("Closure[{:p}]", &cl);
        assert_eq!(
            cl.inspect(),
            expected_inspect,
            "Closure inspect() returned wrong string representation. Expected: {}. Got: {}",
            expected_inspect,
            cl.inspect()
        );
    }

    #[test]
    fn test_compiled_function() {
        let cf = CompiledFunc {
            instructions: Instructions::new(Vec::from("OpDoesntMatter".as_bytes())),
            num_locals: 1,
            num_params: 1,
        };
        assert_eq!(
            cf.object_type(),
            ObjectType::CompiledFunction,
            "CompiledFunction object_type() returned wrong type. Expected: CompiledFunctionObj. Got: {:?}",
            cf.object_type()
        );
        let expected_inspect = format!("CompiledFunction[{:p}]", &cf);
        assert_eq!(
            cf.inspect(),
            expected_inspect,
            "CompiledFunction inspect() returned wrong string representation. Expected: {}. Got: {}",
            expected_inspect,
            cf.inspect()
        );
    }

    #[test]
    fn test_errors() {
        let e = Error {
            message: String::from("Uh oh spaghettio"),
        };
        assert_eq!(
            e.object_type(),
            ObjectType::Error,
            "Error object_type() returned wrong type. Expected: ErrorObj. Got: {:?}",
            e.object_type()
        );
        assert_eq!(
            e.inspect(),
            "Error: Uh oh spaghettio",
            "Error inspect() returned wrong string representation. Expected: Error: Uh oh spaghettio. Got: {}",
            e.inspect()
        );
    }

    #[test]
    fn test_functions() {
        use crate::ast::Identifier;
        use crate::environment::Environment;
        use crate::token::{Token, TokenType};

        let f = Function {
            params: vec![Identifier {
                token: Token {
                    token_type: TokenType::String,
                    literal: String::from(""),
                    line: 0,
                },
                value: String::from("arg1"),
            }],
            body: BlockStatement {
                token: Token {
                    token_type: TokenType::String,
                    literal: String::from("let"),
                    line: 0,
                },
                statements: vec![Box::new(LetStatement {
                    token: Token {
                        token_type: TokenType::String,
                        literal: String::from("let"),
                        line: 0,
                    },
                    name: Identifier {
                        value: String::from("waaat"),
                        token: Token {
                            token_type: TokenType::String,
                            literal: String::from(""),
                            line: 0,
                        },
                    },
                    value: Box::new(StringLiteral {
                        token: Token {
                            token_type: TokenType::String,
                            literal: String::from("thing"),
                            line: 0,
                        },
                        value: String::from(""),
                    }),
                })],
            },
            env: Environment::new(),
        };
        assert_eq!(
            f.object_type(),
            ObjectType::Function,
            "Function object_type() returned wrong type. Expected: FunctionObj. Got: {:?}",
            f.object_type()
        );
        assert_eq!(
            f.inspect(),
            "func(arg1) {\nlet waaat = thing;\n}",
            "Function inspect() returned wrong string representation. Expected:\n func(arg1) {{\nlet waaat = thing;\n}}. Got:\n {}",
            f.inspect()
        );
    }

    #[test]
    fn test_integers() {
        let integer = Integer { value: 666 };
        assert_eq!(
            integer.object_type(),
            ObjectType::Integer,
            "Integer object_type() returned wrong type. Expected: IntegerObj. Got: {:?}",
            integer.object_type()
        );
        assert_eq!(
            integer.inspect(),
            "666",
            "Integer inspect() returned wrong string representation. Expected: 666. Got: {}",
            integer.inspect()
        );
    }

    #[test]
    fn test_null() {
        let n = Null {};
        assert_eq!(
            n.object_type(),
            ObjectType::Null,
            "Null object_type() returned wrong type. Expected: NullObj. Got: {:?}",
            n.object_type()
        );
        assert_eq!(
            n.inspect(),
            "null",
            "Null inspect() returned wrong string representation. Expected: null. Got: {}",
            n.inspect()
        );
    }

    #[test]
    fn test_return_values() {
        let rv = ReturnValue {
            value: Box::new(Str {
                value: "im a returned string".to_string(),
            }),
        };
        assert_eq!(
            rv.object_type(),
            ObjectType::ReturnValue,
            "ReturnValue object_type() returned wrong type. Expected: ReturnValueObj. Got: {:?}",
            rv.object_type()
        );
        assert_eq!(
            rv.inspect(),
            "im a returned string",
            "ReturnValue inspect() returned wrong string representation. Expected: im a returned string. Got: {}",
            rv.inspect()
        );
    }

    #[test]
    fn test_strings() {
        let s = Str {
            value: "thurman merman".to_string(),
        };
        assert_eq!(
            s.object_type(),
            ObjectType::String,
            "String object_type() returned wrong type. Expected: StringObj. Got: {:?}",
            s.object_type()
        );
        assert_eq!(
            s.inspect(),
            "thurman merman",
            "String inspect() returned wrong string representation. Expected: thurman merman. Got: {}",
            s.inspect()
        );
    }

    #[test]
    fn test_builtins() {
        use crate::builtins;
        use std::sync::Arc;

        fn null_builtin_func(_args: Vec<Rc<dyn Object>>) -> Rc<dyn Object> {
            Rc::new(Null {})
        }

        let b = Builtin {
            func: Arc::new(null_builtin_func),
        };

        assert_eq!(
            b.object_type(),
            ObjectType::Builtin,
            "Builtin object_type() returned wrong type. Expected: BuiltinObj. Got: {:?}",
            b.object_type()
        );
        assert_eq!(
            b.inspect(),
            "builtin function",
            "Builtin inspect() returned wrong string representation. Expected: builtin function. Got: {}",
            b.inspect()
        );
        let not_a_builtin = builtins::get_builtin_by_name("notABuiltin");
        assert!(
            not_a_builtin.is_none(),
            "GetBuiltinByName(\"notABuiltin\") should have returned None"
        );
        let err = builtins::new_error(String::from("Message with format verbs"));
        assert_eq!(
            err.message, "Message with format verbs",
            "new_error returned wrong error string. Expected: 'Message with format verbs'. Got: {}",
            err.message
        );
    }
}
