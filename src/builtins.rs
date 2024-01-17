use crate::object::Object;
use lazy_static::lazy_static;

#[derive(PartialEq, Debug, Hash)]
pub struct BuiltinObject<'a> {
    pub func: fn(&'a [Object<'a>]) -> Object<'a>,
}

impl<'a> BuiltinObject<'a> {
    pub fn new(func: fn(&[Object<'a>]) -> Object<'a>) -> Self {
        BuiltinObject { func }
    }

    pub fn call<'b: 'a>(&self, args: &'b [Object<'a>]) -> Object<'a> {
        (self.func)(args)
    }

    pub fn get_builtin_by_name(name: &str) -> Option<BuiltinObject<'a>> {
        let builtins = vec![
            ("len", BuiltinObject::new(b_len)),
            ("print", BuiltinObject::new(b_print)),
            ("first", BuiltinObject::new(b_first)),
            ("last", BuiltinObject::new(b_last)),
            ("rest", BuiltinObject::new(b_rest)),
            ("push", BuiltinObject::new(b_push)),
            ("pop", BuiltinObject::new(b_pop)),
            ("split", BuiltinObject::new(b_split)),
            ("join", BuiltinObject::new(b_join)),
        ];

        builtins
            .into_iter()
            .find(|(builtin_name, _)| *builtin_name == name)
            .map(|(_, builtin)| builtin)
    }
}

impl<'a> Clone for BuiltinObject<'a> {
    fn clone(&self) -> Self {
        BuiltinObject { func: self.func }
    }
}

lazy_static! {
    pub static ref BUILTINS: Vec<(String, BuiltinObject<'static>)> = {
        vec![
            ("len".to_string(), BuiltinObject::new(b_len)),
            ("print".to_string(), BuiltinObject::new(b_print)),
            ("first".to_string(), BuiltinObject::new(b_first)),
            ("last".to_string(), BuiltinObject::new(b_last)),
            ("rest".to_string(), BuiltinObject::new(b_rest)),
            ("push".to_string(), BuiltinObject::new(b_push)),
            ("pop".to_string(), BuiltinObject::new(b_pop)),
            ("split".to_string(), BuiltinObject::new(b_split)),
            ("join".to_string(), BuiltinObject::new(b_join)),
        ]
    };
}

fn b_len<'a>(args: &[Object<'a>]) -> Object<'a> {
    if args.len() != 1 {
        return Object::Error(new_error(format!(
            "Wrong number of arguments. Got: {}, Expected: 1",
            args.len()
        )));
    }

    match &args[0] {
        Object::Array(array) => Object::Integer(array.len() as i64),
        Object::Str(string) => Object::Integer(string.len() as i64),
        _ => Object::Error(new_error(format!(
            "Argument to `len` not supported. Got: {}",
            args[0].object_type()
        ))),
    }
}

fn b_print<'a>(args: &[Object<'a>]) -> Object<'a> {
    for arg in args {
        match arg {
            Object::Integer(value) => println!("{}", value),
            Object::Boolean(value) => println!("{}", value),
            Object::Str(s) => println!("{}", s),
            Object::Function(func) => todo!("TODO"),
            Object::Builtin(bltn) => todo!("TODO"),
            Object::Array(elements) => {
                let elements_str = elements
                    .iter()
                    .map(|e| e.inspect())
                    .collect::<Vec<_>>()
                    .join(", ");
                println!("[{}]", elements_str);
            }
            Object::Hash(hash) => {
                let pairs_str = hash
                    .pairs
                    .iter()
                    .map(|(k, v)| format!("{}: {}", k.value, v.value))
                    .collect::<Vec<_>>()
                    .join(", ");
                println!("{{{}}}", pairs_str);
            }
            Object::CompiledFunc(func) => println!("CompiledFunction[{:p}]", func),
            Object::Closure(closure) => println!("Closure[{:p}]", closure),
            Object::ReturnValue(value) => println!("{}", value.inspect()),
            Object::Null => println!("null"),
            Object::Error(message) => println!("Error: {}", message),
        }
    }
    Object::Null
}

fn b_first<'a>(args: &[Object<'a>]) -> Object<'a> {
    if args.len() != 1 {
        return Object::Error("Wrong number of arguments. Got: {}, Expected: 1".to_string());
    }

    match &args[0] {
        Object::Array(elements) => {
            if let Some(first_element) = elements.first() {
                first_element.clone()
            } else {
                Object::Null
            }
        }
        _ => Object::Error("Argument to `first` must be an Array.".to_string()),
    }
}

fn b_last<'a>(args: &[Object<'a>]) -> Object<'a> {
    if args.len() != 1 {
        return Object::Error("Wrong number of arguments. Got: {}, Expected: 1".to_string());
    }

    match &args[0] {
        Object::Array(elements) => {
            if let Some(last_element) = elements.last() {
                last_element.clone()
            } else {
                Object::Null
            }
        }
        _ => Object::Error("Argument to `last` must be an Array.".to_string()),
    }
}

fn b_rest<'a>(args: &[Object<'a>]) -> Object<'a> {
    if args.len() != 1 {
        return Object::Error("Wrong number of arguments. Got: {}, Expected: 1".to_string());
    }

    match &args[0] {
        Object::Array(elements) => {
            if elements.len() > 1 {
                let rest_elements = elements[1..].to_vec();
                Object::Array(rest_elements)
            } else {
                Object::Null
            }
        }
        _ => Object::Error("Argument to `rest` must be an Array.".to_string()),
    }
}

fn b_push<'a>(args: &[Object<'a>]) -> Object<'a> {
    if args.len() != 2 {
        return Object::Error("Wrong number of arguments. Got: {}, Expected: 2".to_string());
    }

    match &args[0] {
        Object::Array(elements) => {
            let mut new_elements = elements.clone();
            new_elements.push(args[1].clone());
            Object::Array(new_elements)
        }
        _ => Object::Error("Argument to `push` must be an Array.".to_string()),
    }
}

fn b_pop<'a>(args: &[Object<'a>]) -> Object<'a> {
    if args.len() != 1 {
        return Object::Error("Wrong number of arguments. Got: {}, Expected: 1".to_string());
    }

    match &args[0] {
        Object::Array(elements) => {
            if elements.is_empty() {
                Object::Null
            } else {
                let new_elements = elements[..elements.len() - 1].to_vec();
                Object::Array(new_elements)
            }
        }
        _ => Object::Error("Argument to `pop` must be an Array.".to_string()),
    }
}

fn b_split<'a>(args: &[Object<'a>]) -> Object<'a> {
    if args.len() != 2 {
        return Object::Error("Wrong number of arguments. Got: {}, Expected: 2".to_string());
    }

    if let Object::Str(string) = &args[0] {
        if let Object::Str(split_on) = &args[1] {
            let split_results: Vec<Object<'a>> = string
                .split(split_on)
                .map(|s| Object::Str(s.to_string()))
                .collect();

            Object::Array(split_results)
        } else {
            Object::Error("Second argument to `split` must be a String".to_string())
        }
    } else {
        Object::Error("First argument to `split` must be a String".to_string())
    }
}

fn b_join<'a>(args: &[Object<'a>]) -> Object<'a> {
    if args.len() != 2 {
        return Object::Error("Wrong number of arguments. Got: {}, Expected: 2".to_string());
    }

    if let Object::Array(array) = &args[0] {
        if array.is_empty() {
            return Object::Str("".to_string());
        }

        if let Object::Str(join_on) = &args[1] {
            let elements: Result<Vec<_>, _> = array
                .iter()
                .map(|elem| match elem {
                    Object::Str(s) => Ok(s.clone()),
                    _ => Err("You can only join an array of all strings"),
                })
                .collect();

            match elements {
                Ok(elems) => Object::Str(elems.join(join_on)),
                Err(msg) => Object::Error(msg.to_string()),
            }
        } else {
            Object::Error("Second argument to `join` must be a String".to_string())
        }
    } else {
        Object::Error("First argument to `join` must be an Array".to_string())
    }
}

pub fn new_error(message: String) -> String {
    Object::Error(message).to_string()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_len_builtin() {
        let arr = Object::Array(vec![
            Object::Integer(1),
            Object::Integer(2),
            Object::Integer(3),
        ]);
        let str = Object::Str("neat string".to_string());
        let len_builtin = BuiltinObject::get_builtin_by_name("len").unwrap();

        let args1 = [arr.clone(), str.clone()];
        let args2 = [arr.clone()];
        let args3 = [str.clone()];

        assert_eq!(
            len_builtin.call(&args1).inspect(),
            "Error: Wrong number of arguments. Got: 2, Expected: 1"
        );
        assert_eq!(len_builtin.call(&args2).inspect(), "3");
        assert_eq!(len_builtin.call(&args3).inspect(), "11");
        assert_eq!(
            len_builtin.call(&[Object::Null]).inspect(),
            "Error: Argument to `len` not supported. Got: NULL"
        );
    }

    // TODO: possible integration test to actually test the std output
    #[test]
    fn test_print_builtin() {
        let str = Object::Str("neat string".to_string());
        let print_builtin = BuiltinObject::get_builtin_by_name("print").unwrap();
        let args1 = [str];
        let result = print_builtin.call(&args1);
        if result.object_type() != "NULL" {
            panic!("Print builtin did not return a Null object");
        }
    }

    #[test]
    fn test_first_builtin() {
        let arr = Object::Array(vec![
            Object::Integer(99),
            Object::Integer(7),
            Object::Integer(356),
        ]);
        let empty_arr = Object::Array(Vec::new());
        let first_builtin = BuiltinObject::get_builtin_by_name("first").unwrap();

        let args1 = [arr.clone(), arr.clone()];
        let args2 = [Object::Str("test".to_string())];
        let args3 = [arr];

        assert_eq!(
            first_builtin.call(&args1).inspect(),
            "Error: Wrong number of arguments. Got: 2, Expected: 1"
        );
        assert_eq!(
            first_builtin.call(&args2).inspect(),
            "Error: Argument to `first` must be an Array. Got: STRING"
        );
        assert_eq!(first_builtin.call(&args3).inspect(), "99");
        assert!(matches!(first_builtin.call(&[empty_arr]), Object::Null));
    }

    #[test]
    fn test_last_builtin() {
        let arr = Object::Array(vec![
            Object::Integer(99),
            Object::Integer(7),
            Object::Integer(356),
        ]);
        let empty_arr = Object::Array(Vec::new());
        let str = Object::Str("neat string".to_string());
        let last_builtin = BuiltinObject::get_builtin_by_name("last").unwrap();

        let args1 = [arr.clone(), arr.clone()];
        let args2 = [str];
        let args3 = [arr];
        let args4 = [empty_arr];

        assert_eq!(
            last_builtin.call(&args1).inspect(),
            "Error: Wrong number of arguments. Got: 2, Expected: 1",
            "last builtin returned wrong result for wrong number of arguments"
        );
        assert_eq!(
            last_builtin.call(&args2).inspect(),
            "Error: Argument to `last` must be an Array. Got: STRING",
            "last builtin returned wrong result for non-array argument"
        );
        assert_eq!(
            last_builtin.call(&args3).inspect(),
            "356",
            "last builtin returned wrong result for array"
        );
        assert_eq!(
            last_builtin.call(&args4).inspect(),
            "Null",
            "last builtin returned wrong result for empty array"
        );
    }

    #[test]
    fn test_rest_builtin() {
        let arr = Object::Array(vec![
            Object::Integer(99),
            Object::Integer(7),
            Object::Integer(356),
        ]);
        let empty_arr = Object::Array(Vec::new());
        let str = Object::Str("neat string".to_string());
        let rest_builtin = BuiltinObject::get_builtin_by_name("rest").unwrap();

        let args1 = [arr.clone(), arr.clone()];
        let args2 = [str];
        let args3 = [arr];
        let args4 = [empty_arr];

        assert_eq!(
            rest_builtin.call(&args1).inspect(),
            "Error: Wrong number of arguments. Got: 2, Expected: 1",
            "rest builtin returned wrong result for wrong number of arguments"
        );
        assert_eq!(
            rest_builtin.call(&args2).inspect(),
            "Error: Argument to `rest` must be an Array. Got: STRING",
            "rest builtin returned wrong result for non-array argument"
        );
        assert_eq!(
            rest_builtin.call(&args3).inspect(),
            "[7, 356]",
            "rest builtin returned wrong result for array"
        );
        assert_eq!(
            rest_builtin.call(&args4).inspect(),
            "Null",
            "rest builtin returned wrong result for empty array"
        );
    }

    #[test]
    fn test_push_builtin() {
        let arr = Object::Array(vec![
            Object::Integer(99),
            Object::Integer(7),
            Object::Integer(356),
        ]);
        let new_el = Object::Integer(666);
        let str = Object::Str("neat string".to_string());
        let push_builtin = BuiltinObject::get_builtin_by_name("push").unwrap();

        let args1 = [arr.clone()];
        let args2 = [str, new_el.clone()];
        let args3 = [arr.clone(), new_el.clone()];

        assert_eq!(
            push_builtin.call(&args1).inspect(),
            "Error: Wrong number of arguments. Got: 1, Expected: 2",
            "push builtin returned wrong result for wrong number of arguments"
        );
        assert_eq!(
            push_builtin.call(&args2).inspect(),
            "Error: Argument to `push` must be an Array. Got: STRING",
            "push builtin returned wrong result for non-array argument"
        );
        assert_eq!(
            push_builtin.call(&args3).inspect(),
            "[99, 7, 356, 666]",
            "push builtin returned wrong result for array with a new element"
        );
    }

    #[test]
    fn test_pop_builtin() {
        let arr = Object::Array(vec![
            Object::Integer(99),
            Object::Integer(7),
            Object::Integer(356),
        ]);
        let empty_arr = Object::Array(Vec::new());
        let str = Object::Str("neat string".to_string());
        let pop_builtin = BuiltinObject::get_builtin_by_name("pop").unwrap();

        let args1 = [arr.clone(), str.clone()];
        let args2 = [str.clone()];
        let args3 = [arr.clone()];
        let args4 = [empty_arr];

        assert_eq!(
            pop_builtin.call(&args1).inspect(),
            "Error: Wrong number of arguments. Got: 2, Expected: 1",
            "pop builtin returned wrong result for wrong number of arguments"
        );
        assert_eq!(
            pop_builtin.call(&args2).inspect(),
            "Error: Argument to `pop` must be an Array. Got: STRING",
            "pop builtin returned wrong result for non-array argument"
        );
        assert_eq!(
            pop_builtin.call(&args3).inspect(),
            "[99, 7]",
            "pop builtin returned wrong result for array"
        );
        assert_eq!(
            pop_builtin.call(&args4).inspect(),
            "Null",
            "pop builtin returned wrong result for empty array"
        );
    }

    #[test]
    fn test_split_builtin() {
        let str = Object::Str("My name is brad".to_string());
        let split_on = Object::Str(" ".to_string());
        let empty_str = Object::Str("".to_string());
        let split_builtin = BuiltinObject::get_builtin_by_name("split").unwrap();

        let args1 = [str.clone()];
        let args2 = [str.clone(), split_on.clone()];
        let args3 = [empty_str, split_on.clone()];

        assert_eq!(
            split_builtin.call(&args1).inspect(),
            "Error: Wrong number of arguments. Got: 1, Expected: 2",
            "split builtin returned wrong result for wrong number of arguments"
        );
        assert_eq!(
            split_builtin.call(&args2).inspect(),
            "[\"My\", \"name\", \"is\", \"brad\"]",
            "split builtin returned wrong result for valid arguments"
        );
        assert_eq!(
            split_builtin.call(&args3).inspect(),
            "[]",
            "split builtin returned wrong result for empty string"
        );
    }

    #[test]
    fn test_join_builtin() {
        let array = Object::Array(vec![
            Object::Str("My".to_string()),
            Object::Str("name".to_string()),
            Object::Str("is".to_string()),
            Object::Str("brad".to_string()),
        ]);
        let join_on = Object::Str(" ".to_string());
        let not_an_array = Object::Str("not an array".to_string());
        let join_builtin = BuiltinObject::get_builtin_by_name("join").unwrap();

        let args1 = [array.clone()];
        let args2 = [not_an_array, join_on.clone()];
        let args3 = [array.clone(), join_on.clone()];

        assert_eq!(
            join_builtin.call(&args1).inspect(),
            "Error: Wrong number of arguments. Got: 1, Expected: 2",
            "join builtin returned wrong result for wrong number of arguments"
        );
        assert_eq!(
            join_builtin.call(&args2).inspect(),
            "Error: First argument to `join` must be an Array. Got: STRING",
            "join builtin returned wrong result for non-array first argument"
        );
        assert_eq!(
            join_builtin.call(&args3).inspect(),
            "\"My name is brad\"",
            "join builtin returned wrong result for valid arguments"
        );
    }
}
