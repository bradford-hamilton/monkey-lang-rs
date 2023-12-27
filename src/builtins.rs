use crate::object::{Array, Error, Integer, Null, Object, Str};
use lazy_static::lazy_static;
use std::collections::HashMap;
use std::rc::Rc;
use std::sync::Arc;

pub struct Builtin {
    pub func: Arc<dyn Fn(Vec<Rc<dyn Object>>) -> Rc<dyn Object> + Send + Sync>,
}

impl Builtin {
    pub fn new<F>(func: F) -> Self
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

lazy_static! {
    static ref BUILTINS: HashMap<String, Builtin> = {
        let mut m = HashMap::new();
        m.insert(String::from("len"), Builtin::new(b_len));
        m.insert(String::from("print"), Builtin::new(b_print));
        m.insert(String::from("first"), Builtin::new(b_first));
        m.insert(String::from("last"), Builtin::new(b_last));
        m.insert(String::from("rest"), Builtin::new(b_rest));
        m.insert(String::from("push"), Builtin::new(b_push));
        m.insert(String::from("pop"), Builtin::new(b_pop));
        m.insert(String::from("split"), Builtin::new(b_split));
        m.insert(String::from("join"), Builtin::new(b_join));
        m
    };
}

pub fn get_builtin_by_name(name: &str) -> Option<Builtin> {
    BUILTINS.get(name).cloned()
}

fn b_len(args: Vec<Rc<dyn Object>>) -> Rc<dyn Object> {
    if args.len() != 1 {
        return Rc::new(new_error(format!(
            "Wrong number of arguments. Got: {}, Expected: 1",
            args.len()
        )));
    }

    match args[0].as_ref().as_any().downcast_ref::<Array>() {
        Some(array) => Rc::new(Integer {
            value: array.elements.len() as i64,
        }),
        None => match args[0].as_ref().as_any().downcast_ref::<Str>() {
            Some(string) => Rc::new(Integer {
                value: string.value.len() as i64,
            }),
            None => Rc::new(new_error(format!(
                "Argument to `len` not supported. Got: {}",
                args[0].as_ref().object_type()
            ))),
        },
    }
}

fn b_print(args: Vec<Rc<dyn Object>>) -> Rc<dyn Object> {
    for arg in args.iter() {
        println!("{}", arg.as_ref().inspect());
    }
    Rc::new(Null {})
}

fn b_first(args: Vec<Rc<dyn Object>>) -> Rc<dyn Object> {
    if args.len() != 1 {
        return Rc::new(new_error(format!(
            "Wrong number of arguments. Got: {}, Expected: 1",
            args.len()
        )));
    }

    match args[0].as_ref().as_any().downcast_ref::<Array>() {
        Some(array) => {
            if !array.elements.is_empty() {
                Rc::clone(&array.elements[0])
            } else {
                Rc::new(Null {})
            }
        }
        None => Rc::new(new_error(format!(
            "Argument to `first` must be an Array. Got: {}",
            args[0].as_ref().object_type()
        ))),
    }
}

fn b_last(args: Vec<Rc<dyn Object>>) -> Rc<dyn Object> {
    if args.len() != 1 {
        return Rc::new(new_error(format!(
            "Wrong number of arguments. Got: {}, Expected: 1",
            args.len()
        )));
    }

    match args[0].as_ref().as_any().downcast_ref::<Array>() {
        Some(array) => {
            if let Some(last_element) = array.elements.last() {
                Rc::clone(last_element)
            } else {
                Rc::new(Null {})
            }
        }
        None => Rc::new(new_error(format!(
            "Argument to `last` must be an Array. Got: {}",
            args[0].as_ref().object_type()
        ))),
    }
}

fn b_rest(args: Vec<Rc<dyn Object>>) -> Rc<dyn Object> {
    if args.len() != 1 {
        return Rc::new(new_error(format!(
            "Wrong number of arguments. Got: {}, Expected: 1",
            args.len()
        )));
    }

    match args[0].as_ref().as_any().downcast_ref::<Array>() {
        Some(array) => {
            if array.elements.len() > 1 {
                let rest_elements = array.elements[1..].to_vec();
                Rc::new(Array {
                    elements: rest_elements,
                })
            } else {
                Rc::new(Null {})
            }
        }
        None => Rc::new(new_error(format!(
            "Argument to `rest` must be an Array. Got: {}",
            args[0].as_ref().object_type()
        ))),
    }
}

fn b_push(args: Vec<Rc<dyn Object>>) -> Rc<dyn Object> {
    if args.len() != 2 {
        return Rc::new(new_error(format!(
            "Wrong number of arguments. Got: {}, Expected: 2",
            args.len()
        )));
    }

    match args[0].as_ref().as_any().downcast_ref::<Array>() {
        Some(array) => {
            let mut new_elements = array.elements.clone();
            new_elements.push(Rc::clone(&args[1]));
            Rc::new(Array {
                elements: new_elements,
            })
        }
        None => Rc::new(new_error(format!(
            "Argument to `push` must be an Array. Got: {}",
            args[0].as_ref().object_type()
        ))),
    }
}

fn b_pop(args: Vec<Rc<dyn Object>>) -> Rc<dyn Object> {
    if args.len() != 1 {
        return Rc::new(new_error(format!(
            "Wrong number of arguments. Got: {}, Expected: 1",
            args.len()
        )));
    }

    match args[0].as_ref().as_any().downcast_ref::<Array>() {
        Some(array) => {
            if array.elements.is_empty() {
                Rc::new(Null {})
            } else {
                let new_elements = array.elements[..array.elements.len() - 1].to_vec();
                Rc::new(Array {
                    elements: new_elements,
                })
            }
        }
        None => Rc::new(new_error(format!(
            "Argument to `pop` must be an Array. Got: {}",
            args[0].as_ref().object_type()
        ))),
    }
}

fn b_split(args: Vec<Rc<dyn Object>>) -> Rc<dyn Object> {
    if args.len() != 2 {
        return Rc::new(new_error(format!(
            "Wrong number of arguments. Got: {}, Expected: 2",
            args.len()
        )));
    }

    let str_arg = match args[0].as_ref().as_any().downcast_ref::<Str>() {
        Some(string) => string,
        None => {
            return Rc::new(new_error(
                "First argument to `split` must be a String".to_string(),
            ))
        }
    };
    let split_on_arg = match args[1].as_ref().as_any().downcast_ref::<Str>() {
        Some(string) => string,
        None => {
            return Rc::new(new_error(
                "Second argument to `split` must be a String".to_string(),
            ))
        }
    };
    let split_result = str_arg
        .value
        .split(&split_on_arg.value)
        .map(|s| {
            Rc::new(Str {
                value: s.to_string(),
            }) as Rc<dyn Object>
        })
        .collect();

    Rc::new(Array {
        elements: split_result,
    })
}

fn b_join(args: Vec<Rc<dyn Object>>) -> Rc<dyn Object> {
    if args.len() != 2 {
        return Rc::new(new_error(format!(
            "Wrong number of arguments. Got: {}, Expected: 2",
            args.len()
        )));
    }
    let array_arg = match args[0].as_ref().as_any().downcast_ref::<Array>() {
        Some(array) => array,
        None => {
            return Rc::new(new_error(
                "First argument to `join` must be an Array".to_string(),
            ))
        }
    };
    let join_on_arg = match args[1].as_ref().as_any().downcast_ref::<Str>() {
        Some(string) => string,
        None => {
            return Rc::new(new_error(
                "Second argument to `join` must be a String".to_string(),
            ))
        }
    };
    let mut elements = Vec::new();
    for element in array_arg.elements.iter() {
        match element.as_ref().as_any().downcast_ref::<Str>() {
            Some(string) => elements.push(string.value.clone()),
            None => {
                return Rc::new(new_error(
                    "You can only join an array of all strings".to_string(),
                ))
            }
        }
    }

    Rc::new(Str {
        value: elements.join(&join_on_arg.value),
    })
}

pub fn new_error(message: String) -> Error {
    Error { message }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_len_builtin() {
        let arr = Array {
            elements: vec![
                Rc::new(Integer { value: 1 }),
                Rc::new(Integer { value: 2 }),
                Rc::new(Integer { value: 3 }),
            ],
        };
        let str = Str {
            value: "neat string".to_string(),
        };
        let null = Null {};
        let len_builtin = Rc::new(b_len);

        // Testing with wrong number of arguments
        assert_eq!(
            len_builtin(vec![Rc::new(str), Rc::new(null)]).inspect(),
            "Error: Wrong number of arguments. Got: 2, Expected: 1",
            "len builtin returned wrong result for wrong number of arguments"
        );
        // Testing with an array
        assert_eq!(
            len_builtin(vec![Rc::new(arr)]).inspect(),
            "3",
            "len builtin returned wrong result for array"
        );
        // Testing with a string
        assert_eq!(
            len_builtin(vec![Rc::new(Str {
                value: "neat string".to_string()
            })])
            .inspect(),
            "11",
            "len builtin returned wrong result for string"
        );
        // Testing with unsupported type
        assert_eq!(
            len_builtin(vec![Rc::new(Null {})]).inspect(),
            "Error: Argument to `len` not supported. Got: NULL",
            "len builtin returned wrong result for unsupported type"
        );
    }

    // TODO: possible integration test to actually test the std output
    #[test]
    fn test_print_builtin() {
        let str = Str {
            value: "neat string".to_string(),
        };
        let print_builtin = get_builtin_by_name("print").unwrap();
        let result = print_builtin.call(vec![Rc::new(str)]);
        match result.as_ref().as_any().downcast_ref::<Null>() {
            Some(_) => (),
            None => panic!("Print builtin did not return a Null object"),
        }
    }

    #[test]
    fn test_first_builtin() {
        let arr = Array {
            elements: vec![
                Rc::new(Integer { value: 99 }),
                Rc::new(Integer { value: 7 }),
                Rc::new(Integer { value: 356 }),
            ],
        };
        let empty_arr = Array {
            elements: Vec::new(),
        };
        let str = Str {
            value: "neat string".to_string(),
        };
        let first_builtin = get_builtin_by_name("first").unwrap();

        // Testing with wrong number of arguments
        assert_eq!(
            first_builtin
                .call(vec![Rc::new(arr.clone()), Rc::new(arr.clone())])
                .inspect(),
            "Error: Wrong number of arguments. Got: 2, Expected: 1",
            "first builtin returned wrong result for wrong number of arguments"
        );
        // Testing with non-array argument
        assert_eq!(
            first_builtin.call(vec![Rc::new(str)]).inspect(),
            "Error: Argument to `first` must be an Array. Got: STRING",
            "first builtin returned wrong result for non-array argument"
        );
        // Testing with an array
        assert_eq!(
            first_builtin.call(vec![Rc::new(arr)]).inspect(),
            "99",
            "first builtin returned wrong result for array"
        );
        // Testing with an empty array
        let result = first_builtin.call(vec![Rc::new(empty_arr)]);
        if result.as_ref().as_any().downcast_ref::<Null>().is_none() {
            panic!("first builtin returned wrong result for empty array. Expected: Null object");
        }
    }

    #[test]
    fn test_last_builtin() {
        let arr = Array {
            elements: vec![
                Rc::new(Integer { value: 99 }),
                Rc::new(Integer { value: 7 }),
                Rc::new(Integer { value: 356 }),
            ],
        };
        let empty_arr = Array {
            elements: Vec::new(),
        };
        let str = Str {
            value: String::from("neat string"),
        };
        let last_builtin = get_builtin_by_name("last").unwrap();

        // Testing with wrong number of arguments
        assert_eq!(
            last_builtin
                .call(vec![Rc::new(arr.clone()), Rc::new(arr.clone())])
                .inspect(),
            "Error: Wrong number of arguments. Got: 2, Expected: 1",
            "last builtin returned wrong result for wrong number of arguments"
        );
        // Testing with non-array argument
        assert_eq!(
            last_builtin.call(vec![Rc::new(str)]).inspect(),
            "Error: Argument to `last` must be an Array. Got: STRING",
            "last builtin returned wrong result for non-array argument"
        );
        // Testing with an array
        assert_eq!(
            last_builtin.call(vec![Rc::new(arr)]).inspect(),
            "356",
            "last builtin returned wrong result for array"
        );
        // Testing with an empty array
        let result = last_builtin.call(vec![Rc::new(empty_arr)]);
        if result.as_ref().as_any().downcast_ref::<Null>().is_none() {
            panic!("last builtin returned wrong result for empty array. Expected: Null object");
        }
    }

    #[test]
    fn test_rest_builtin() {
        let arr = Array {
            elements: vec![
                Rc::new(Integer { value: 99 }),
                Rc::new(Integer { value: 7 }),
                Rc::new(Integer { value: 356 }),
            ],
        };
        let empty_arr = Array {
            elements: Vec::new(),
        };
        let str = Str {
            value: String::from("neat string"),
        };
        let rest_builtin = get_builtin_by_name("rest").unwrap();

        // Testing with wrong number of arguments
        assert_eq!(
            rest_builtin
                .call(vec![Rc::new(arr.clone()), Rc::new(arr.clone())])
                .inspect(),
            "Error: Wrong number of arguments. Got: 2, Expected: 1",
            "rest builtin returned wrong result for wrong number of arguments"
        );
        // Testing with non-array argument
        assert_eq!(
            rest_builtin.call(vec![Rc::new(str)]).inspect(),
            "Error: Argument to `rest` must be an Array. Got: STRING",
            "rest builtin returned wrong result for non-array argument"
        );
        // Testing with an array
        assert_eq!(
            rest_builtin.call(vec![Rc::new(arr)]).inspect(),
            "[7, 356]",
            "rest builtin returned wrong result for array"
        );
        // Testing with an empty array
        let result = rest_builtin.call(vec![Rc::new(empty_arr)]);
        if result.as_ref().as_any().downcast_ref::<Null>().is_none() {
            panic!("rest builtin returned wrong result for empty array. Expected: Null object");
        }
    }

    #[test]
    fn test_push_builtin() {
        let arr = Array {
            elements: vec![
                Rc::new(Integer { value: 99 }),
                Rc::new(Integer { value: 7 }),
                Rc::new(Integer { value: 356 }),
            ],
        };
        let new_el = Integer { value: 666 };
        let str = Str {
            value: String::from("neat string"),
        };
        let push_builtin = get_builtin_by_name("push").unwrap();

        // Testing with wrong number of arguments
        assert_eq!(
            push_builtin.call(vec![Rc::new(arr.clone())]).inspect(),
            "Error: Wrong number of arguments. Got: 1, Expected: 2",
            "push builtin returned wrong result for wrong number of arguments"
        );
        // Testing with non-array argument
        assert_eq!(
            push_builtin
                .call(vec![Rc::new(str.clone()), Rc::new(str)])
                .inspect(),
            "Error: Argument to `push` must be an Array. Got: STRING",
            "push builtin returned wrong result for non-array argument"
        );
        // Testing with an array and a new element
        assert_eq!(
            push_builtin
                .call(vec![Rc::new(arr), Rc::new(new_el)])
                .inspect(),
            "[99, 7, 356, 666]",
            "push builtin returned wrong result for array with a new element"
        );
    }

    #[test]
    fn test_pop_builtin() {
        let arr = Array {
            elements: vec![
                Rc::new(Integer { value: 99 }),
                Rc::new(Integer { value: 7 }),
                Rc::new(Integer { value: 356 }),
            ],
        };
        let empty_arr = Array {
            elements: Vec::new(),
        };
        let str = Str {
            value: String::from("neat string"),
        };
        let pop_builtin = get_builtin_by_name("pop").unwrap();

        // Testing with wrong number of arguments
        assert_eq!(
            pop_builtin
                .call(vec![Rc::new(arr.clone()), Rc::new(str.clone())])
                .inspect(),
            "Error: Wrong number of arguments. Got: 2, Expected: 1",
            "pop builtin returned wrong result for wrong number of arguments"
        );
        // Testing with non-array argument
        assert_eq!(
            pop_builtin.call(vec![Rc::new(str)]).inspect(),
            "Error: Argument to `pop` must be an Array. Got: STRING",
            "pop builtin returned wrong result for non-array argument"
        );
        // Testing with an array
        assert_eq!(
            pop_builtin.call(vec![Rc::new(arr)]).inspect(),
            "[99, 7]",
            "pop builtin returned wrong result for array"
        );
        // Testing with an empty array
        let result = pop_builtin.call(vec![Rc::new(empty_arr)]);
        if result.as_ref().as_any().downcast_ref::<Null>().is_none() {
            panic!("pop builtin returned wrong result for empty array. Expected: Null object");
        }
    }

    #[test]
    fn test_split_builtin() {
        let str = Str {
            value: String::from("My name is brad"),
        };
        let split_on = Str {
            value: String::from(" "),
        };
        let array = Array {
            elements: Vec::new(),
        };
        let split_builtin = get_builtin_by_name("split").unwrap();

        // Testing with wrong number of arguments
        assert_eq!(
            split_builtin.call(vec![Rc::new(str.clone())]).inspect(),
            "Error: Wrong number of arguments. Got: 1, Expected: 2",
            "split builtin returned wrong result for wrong number of arguments"
        );
        // Testing with non-string first argument
        assert_eq!(
            split_builtin
                .call(vec![Rc::new(array), Rc::new(str.clone())])
                .inspect(),
            "Error: First argument to `split` must be a String",
            "split builtin returned wrong result for non-string first argument"
        );
        // Testing with valid string and split-on arguments
        assert_eq!(
            split_builtin
                .call(vec![Rc::new(str), Rc::new(split_on)])
                .inspect(),
            "[My, name, is, brad]",
            "split builtin returned wrong result for valid arguments"
        );
    }

    #[test]
    fn test_join_builtin() {
        use crate::object::Boolean;

        let array = Array {
            elements: vec![
                Rc::new(Str {
                    value: "My".to_string(),
                }),
                Rc::new(Str {
                    value: "name".to_string(),
                }),
                Rc::new(Str {
                    value: "is".to_string(),
                }),
                Rc::new(Str {
                    value: "brad".to_string(),
                }),
            ],
        };
        let mixed_array = Array {
            elements: vec![
                Rc::new(Str {
                    value: "My".to_string(),
                }),
                Rc::new(Str {
                    value: "name".to_string(),
                }),
                Rc::new(Boolean { value: true }),
            ],
        };
        let join_on = Str {
            value: " ".to_string(),
        };
        let not_an_array = Str {
            value: "not an array".to_string(),
        };
        let join_builtin = get_builtin_by_name("join").unwrap();

        // Testing with wrong number of arguments
        assert_eq!(
            join_builtin.call(vec![Rc::new(array.clone())]).inspect(),
            "Error: Wrong number of arguments. Got: 1, Expected: 2",
            "join builtin returned wrong result for wrong number of arguments"
        );
        // Testing with non-array first argument
        assert_eq!(
            join_builtin
                .call(vec![Rc::new(not_an_array), Rc::new(join_on.clone())])
                .inspect(),
            "Error: First argument to `join` must be an Array",
            "join builtin returned wrong result for non-array first argument"
        );
        // Testing with mixed array
        assert_eq!(
            join_builtin
                .call(vec![Rc::new(mixed_array), Rc::new(join_on.clone())])
                .inspect(),
            "Error: You can only join an array of all strings",
            "join builtin returned wrong result for mixed array"
        );
        // Testing with valid array and join character
        assert_eq!(
            join_builtin
                .call(vec![Rc::new(array), Rc::new(join_on)])
                .inspect(),
            "My name is brad",
            "join builtin returned wrong result for valid arguments"
        );
    }
}
