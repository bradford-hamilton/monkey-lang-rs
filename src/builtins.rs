use crate::object::{Array, Error, Integer, Null, Object, Str};
use lazy_static::lazy_static;
use std::collections::HashMap;
use std::rc::Rc;
use std::sync::Arc;

pub struct Builtin {
    func: Arc<dyn Fn(Vec<Rc<dyn Object>>) -> Rc<dyn Object> + Send + Sync>,
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

fn new_error(message: String) -> Error {
    Error { message }
}
