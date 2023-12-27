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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::object::Str;

    #[test]
    fn test_environments() {
        let outer_env = Environment::new();
        let env = Environment::new_enclosed(Rc::clone(&outer_env));
        env.borrow_mut().set(
            "innerKey".to_string(),
            Rc::new(Str {
                value: "innerValue".to_string(),
            }),
        );
        outer_env.borrow_mut().set(
            "outerKey".to_string(),
            Rc::new(Str {
                value: "outerValue".to_string(),
            }),
        );
        let obj = env
            .borrow()
            .get("innerKey")
            .expect("Failed to retrieve 'innerKey'");
        assert_eq!(
            obj.inspect(),
            "innerValue",
            "Expected 'innerValue'. Got: {}",
            obj.inspect()
        );
        let obj = env
            .borrow()
            .get("outerKey")
            .expect("Failed to retrieve 'outerKey'");
        assert_eq!(
            obj.inspect(),
            "outerValue",
            "Expected 'outerValue'. Got: {}",
            obj.inspect()
        );
    }
}
