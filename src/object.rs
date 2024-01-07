use crate::builtins::BuiltinObject;
use crate::environment::Environment;
use crate::{
    ast::{BlockStatement, Identifier},
    bytecode,
};
use std::collections::hash_map::DefaultHasher;
use std::collections::HashMap;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::marker::PhantomData;

#[derive(Clone)]
pub enum Object<'a> {
    Integer(i64),
    Boolean(bool),
    Str(String),
    Array(Vec<&'a Object<'a>>),
    Hash(HashObject<'a>),
    Function(&'a FunctionObject<'a>),
    Builtin(&'a BuiltinObject<'a>),
    Null,
    ReturnValue(&'a Object<'a>),
    Error(String),
    CompiledFunc(&'a CompiledFuncObject<'a>),
    Closure(&'a ClosureObject<'a>),
}

impl<'a> PartialEq for Object<'a> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Object::Integer(a), Object::Integer(b)) => a == b,
            (Object::Boolean(a), Object::Boolean(b)) => a == b,
            (Object::Str(a), Object::Str(b)) => a == b,
            (Object::Array(a), Object::Array(b)) => a == b,
            (Object::Hash(a), Object::Hash(b)) => a == b,
            (Object::Function(a), Object::Function(b)) => std::ptr::eq(*a, *b),
            (Object::Builtin(a), Object::Builtin(b)) => a == b,
            (Object::Null, Object::Null) => true,
            (Object::ReturnValue(a), Object::ReturnValue(b)) => a == b,
            (Object::Error(a), Object::Error(b)) => a == b,
            (Object::CompiledFunc(a), Object::CompiledFunc(b)) => std::ptr::eq(*a, *b),
            (Object::Closure(a), Object::Closure(b)) => std::ptr::eq(*a, *b),
            _ => false,
        }
    }
}

impl<'a> Eq for Object<'a> {}

impl<'a> Hash for Object<'a> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Object::Integer(value) => value.hash(state),
            Object::Boolean(value) => value.hash(state),
            Object::Str(value) => value.hash(state),
            Object::Array(elements) => {
                for elem in elements {
                    std::ptr::hash(*elem, state);
                }
            }
            Object::Hash(value) => std::ptr::hash(value, state),
            Object::Function(value) => std::ptr::hash(*value, state),
            Object::Builtin(value) => {
                let mut hasher = DefaultHasher::new();
                value.hash(&mut hasher);
                hasher.finish().hash(state);
            }
            Object::Null => 0.hash(state),
            Object::ReturnValue(value) => std::ptr::hash(*value, state),
            Object::Error(value) => value.hash(state),
            Object::CompiledFunc(value) => std::ptr::hash(*value, state),
            Object::Closure(value) => std::ptr::hash(*value, state),
        }
    }
}

impl<'a> Object<'a> {
    pub fn object_type(&self) -> &'static str {
        match self {
            Object::Integer(_) => "INTEGER",
            Object::Boolean(_) => "BOOLEAN",
            Object::Str(_) => "STRING",
            Object::Array(_) => "ARRAY",
            Object::Hash(_) => "HASH",
            Object::Function(_) => "FUNCTION",
            Object::Builtin(_) => "BUILTIN",
            Object::Null => "NULL",
            Object::ReturnValue(_) => "RETURN_VALUE",
            Object::Error(_) => "ERROR",
            Object::CompiledFunc(_) => "COMPILED_FUNCTION",
            Object::Closure(_) => "CLOSURE",
        }
    }

    // TODO: finish
    pub fn inspect(&self) -> String {
        format!("{}", self)
    }
}

impl<'a> fmt::Display for Object<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Object::Integer(value) => write!(f, "{}", value),
            Object::Boolean(value) => write!(f, "{}", value),
            Object::Str(value) => write!(f, "{}", value),
            Object::Array(elements) => {
                let elems: Vec<String> = elements.iter().map(|e| e.to_string()).collect();
                write!(f, "[{}]", elems.join(", "))
            }
            Object::Hash(hash_object) => {
                write!(f, "{}", hash_object)
            }
            Object::Function(_) => write!(f, "FunctionObject"),
            Object::Builtin(_) => write!(f, "BuiltinFunction"),
            Object::Null => write!(f, "null"),
            Object::ReturnValue(value) => write!(f, "{}", value),
            Object::Error(message) => write!(f, "Error: {}", message),
            Object::CompiledFunc(ptr) => write!(f, "Closure[{:p}]", ptr),
            Object::Closure(ptr) => write!(f, "Closure[{:p}]", ptr),
        }
    }
}

impl<'a> fmt::Debug for Object<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Object::Integer(value) => write!(f, "Integer({})", value),
            Object::Boolean(value) => write!(f, "Boolean({})", value),
            Object::Str(value) => write!(f, "Str({:?})", value),
            Object::Array(elements) => {
                write!(f, "Array(")?;
                f.debug_list().entries(elements).finish()?;
                write!(f, ")")
            }
            Object::Hash(hash_object) => write!(f, "Hash({:?})", hash_object),
            Object::Function(func_object) => write!(f, "Function({:?})", func_object),
            Object::Builtin(builtin_object) => write!(f, "Builtin({:?})", builtin_object),
            Object::Null => write!(f, "Null"),
            Object::ReturnValue(value) => write!(f, "ReturnValue({:?})", value),
            Object::Error(message) => write!(f, "Error({:?})", message),
            Object::CompiledFunc(compiled_func) => write!(f, "CompiledFunc({:?})", compiled_func),
            Object::Closure(closure) => write!(f, "Closure({:?})", closure),
        }
    }
}

pub struct Null {}

/// CompiledFunction holds the instructions we get from the compilation of a function
/// literal and is an object::Object, which means we can add it as a constant to our
/// compiler::bytecode and load it in the VM. It also holds the NumLocals which we pass
/// to the VM to allocate the correct amount of stack space ("hole") to save the local
/// bindings
// TODO: check back in on this comment after implementing.
#[derive(Debug)]
pub struct CompiledFuncObject<'a> {
    pub instructions: bytecode::Instructions,
    pub num_locals: usize,
    pub num_params: usize,
    _marker: PhantomData<&'a ()>,
}

impl<'a> CompiledFuncObject<'a> {
    pub fn new(instructions: bytecode::Instructions, num_locals: usize, num_params: usize) -> Self {
        CompiledFuncObject {
            instructions,
            num_locals,
            num_params,
            _marker: PhantomData,
        }
    }
}

#[derive(Debug)]
pub struct ClosureObject<'a> {
    pub func: &'a CompiledFuncObject<'a>,
    pub free: Vec<Object<'a>>,
}

pub struct Error {
    pub message: String,
}

#[derive(Debug)]
pub struct FunctionObject<'a> {
    pub params: Vec<Identifier>,
    pub body: BlockStatement,
    pub env: Environment<'a>,
}

#[derive(Eq, PartialEq, Hash, Clone, Debug)]
pub struct HashKey<'a> {
    pub object_type: &'static str,
    pub value: Object<'a>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct HashPair<'a> {
    pub key: &'a Object<'a>,
    pub value: &'a Object<'a>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HashObject<'a> {
    pub pairs: HashMap<HashKey<'a>, HashPair<'a>>,
}

impl<'a> HashObject<'a> {
    pub fn new() -> Self {
        HashObject {
            pairs: HashMap::new(),
        }
    }

    pub fn insert(&mut self, hash_key: HashKey<'a>, key: &'a Object<'a>, value: &'a Object<'a>) {
        self.pairs.insert(hash_key, HashPair { key, value });
    }

    fn make_hash_key(&self, object: &Object<'a>) -> HashKey {
        match object {
            Object::Integer(value) => HashKey {
                object_type: "INTEGER",
                value: Object::Integer(*value),
            },
            Object::Boolean(b) => HashKey {
                object_type: "BOOLEAN",
                value: Object::Boolean(*b),
            },
            Object::Str(value) => {
                let mut hasher = std::collections::hash_map::DefaultHasher::new();
                value.hash(&mut hasher);
                let hash = hasher.finish();
                HashKey {
                    object_type: "STRING",
                    value: Object::Integer(hash as i64),
                }
            }
            _ => panic!("unsupported type for hashing"),
        }
    }
}

impl<'a> Object<'a> {
    pub fn hash_key(&self) -> u64 {
        let mut hasher = DefaultHasher::new();

        match self {
            Object::Str(s) => {
                s.hash(&mut hasher);
            }
            Object::Integer(i) => {
                i.hash(&mut hasher);
            }
            Object::Boolean(b) => {
                b.hash(&mut hasher);
            }
            _ => unimplemented!("hash_key method not implemented for this Object variant"),
        }

        hasher.finish()
    }
}

impl<'a> std::fmt::Display for HashObject<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let pairs: Vec<String> = self
            .pairs
            .iter()
            .map(|(key, pair)| format!("{}: {}", key.value, pair.value))
            .collect();
        write!(f, "{{{}}}", pairs.join(", "))
    }
}

pub struct ReturnValueObj<'a> {
    pub value: Object<'a>,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        ast::{Expression, HashLiteral, LetStatement, Statement, StringLiteral},
        bytecode::Instructions,
        token::{Token, TokenType},
    };

    #[test]
    fn test_string_hash_key() {
        let hello1 = Object::Str("Hello World".to_string());
        let hello2 = Object::Str("Hello World".to_string());
        let diff1 = Object::Str("My name is johnny".to_string());
        let diff2 = Object::Str("My name is johnny".to_string());
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
        let true1 = Object::Boolean(true);
        let true2 = Object::Boolean(true);
        let false1 = Object::Boolean(false);
        let false2 = Object::Boolean(false);
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
        let int1 = Object::Integer(10);
        let int2 = Object::Integer(10);
        let diff1 = Object::Integer(30);
        let diff2 = Object::Integer(30);
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
                object_type: "STRING",
                value: (Object::Integer(1)),
            },
            HashPair {
                key: &Object::Str("monkey".to_string()),
                value: &Object::Str("lang".to_string()),
            },
        );
        let hash_obj = HashObject { pairs };
        let h = Object::Hash(hash_obj);

        assert_eq!(
            h.object_type(),
            "Hash",
            "Hash object_type() returned wrong type. Expected: Hash. Got: {:?}",
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
        let elements = vec![
            &Object::Integer(1),
            &Object::Integer(2),
            &Object::Integer(3),
        ];
        let arr = Object::Array(elements);

        assert_eq!(
            arr.object_type(),
            "Array",
            "Array object_type() returned wrong type. Expected: Array. Got: {:?}",
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
        let b = Object::Boolean(false);
        assert_eq!(
            b.object_type(),
            "Boolean",
            "Boolean object_type() returned wrong type. Expected: Boolean. Got: {:?}",
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
        let compiled_func = CompiledFuncObject::new(Instructions::new(vec![]), 0, 0);
        let closure = ClosureObject {
            func: &compiled_func,
            free: vec![],
        };
        let cl = Object::Closure(&closure);

        assert_eq!(
            cl.object_type(),
            "Closure",
            "Closure object_type() returned wrong type. Expected: Closure. Got: {:?}",
            cl.object_type()
        );

        let expected_inspect = format!("Closure[{:p}]", &compiled_func);
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
        let cf = CompiledFuncObject::new(
            Instructions::new(Vec::from("OpDoesntMatter".as_bytes())),
            1,
            1,
        );
        let compiled_func = Object::CompiledFunc(&cf); // Assuming Object::CompiledFunc takes a reference to CompiledFuncObject

        assert_eq!(
            compiled_func.object_type(),
            "CompiledFunction",
            "CompiledFunction object_type() returned wrong type. Expected: CompiledFunction. Got: {:?}",
            compiled_func.object_type()
        );

        let expected_inspect = format!("CompiledFunction[{:p}]", &cf);
        assert_eq!(
            compiled_func.inspect(),
            expected_inspect,
            "CompiledFunction inspect() returned wrong string representation. Expected: {}. Got: {}",
            expected_inspect,
            compiled_func.inspect()
        );
    }

    #[test]
    fn test_errors() {
        let error_message = "Uh oh spaghettio";
        let e = Object::Error(error_message.to_string()); // Assuming Object::Error takes a String

        assert_eq!(
            e.object_type(),
            "Error",
            "Error object_type() returned wrong type. Expected: Error. Got: {:?}",
            e.object_type()
        );
        assert_eq!(
            e.inspect(),
            format!("Error: {}", error_message),
            "Error inspect() returned wrong string representation. Expected: Error: Uh oh spaghettio. Got: {}",
            e.inspect()
        );
    }

    #[test]
    fn test_functions() {
        use crate::ast::Identifier;
        use crate::environment::Environment;
        use crate::token::{Token, TokenType};

        let params = vec![Identifier {
            token: Token {
                token_type: TokenType::String,
                literal: String::from(""),
                line: 0,
            },
            value: String::from("arg1"),
        }];

        let body = BlockStatement {
            token: Token {
                token_type: TokenType::String,
                literal: String::from("let"),
                line: 0,
            },
            statements: vec![Statement::Let(LetStatement {
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
                value: Expression::String(StringLiteral {
                    token: Token {
                        token_type: TokenType::String,
                        literal: String::from("thing"),
                        line: 0,
                    },
                    value: String::from(""),
                }),
            })],
        };

        let env = Environment::new();
        let func_obj = FunctionObject { params, body, env };
        let f = Object::Function(&func_obj);

        assert_eq!(
            f.object_type(),
            "Function",
            "Function object_type() returned wrong type. Expected: Function. Got: {:?}",
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
        let integer = Object::Integer(666);

        assert_eq!(
            integer.object_type(),
            "Integer",
            "Integer object_type() returned wrong type. Expected: Integer. Got: {:?}",
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
        let n = Object::Null;
        assert_eq!(
            n.object_type(),
            "Null",
            "Null object_type() returned wrong type. Expected: Null. Got: {:?}",
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
        let string_literal = StringLiteral {
            token: Token {
                token_type: TokenType::String,
                literal: "im a returned string".to_string(),
                line: 0,
            },
            value: "im a returned string".to_string(),
        };
        let rv = Object::ReturnValue(&Object::Str(string_literal.value));

        assert_eq!(
            rv.object_type(),
            "ReturnValue",
            "ReturnValue object_type() returned wrong type. Expected: ReturnValue. Got: {:?}",
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
        let s = Object::Str("thurman merman".to_string());

        assert_eq!(
            s.object_type(),
            "String",
            "String object_type() returned wrong type. Expected: String. Got: {:?}",
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

        fn null_builtin_func<'a>(args: &[Object]) -> Object<'a> {
            Object::Null
        }

        let builtin_func = BuiltinObject {
            func: null_builtin_func,
        };

        let b = Object::Builtin(&builtin_func);

        assert_eq!(
            b.object_type(),
            "Builtin",
            "Builtin object_type() returned wrong type. Expected: Builtin. Got: {:?}",
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
            err, "Message with format verbs",
            "new_error returned wrong error string. Expected: 'Message with format verbs'. Got: {}",
            err
        );
    }
}
