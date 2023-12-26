use crate::object::Object;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

/// Environment holds a store of key value pairs and a an "outer", enclosing environment.
pub struct Environment {
    store: HashMap<String, Rc<dyn Object>>,
    outer: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new() -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Environment {
            store: HashMap::new(),
            outer: None,
        }))
    }

    pub fn new_enclosed(outer: Rc<RefCell<Environment>>) -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Environment {
            store: HashMap::new(),
            outer: Some(outer),
        }))
    }

    pub fn get(&self, name: &str) -> Option<Rc<dyn Object>> {
        self.store
            .get(name)
            .cloned()
            .or_else(|| self.outer.as_ref()?.borrow().get(name))
    }

    pub fn set(&mut self, name: String, val: Rc<dyn Object>) {
        self.store.insert(name, val);
    }
}
