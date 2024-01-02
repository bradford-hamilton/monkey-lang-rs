use crate::object::Object;
use std::collections::HashMap;
use std::rc::Rc;

/// Environment holds a store of key value pairs and a an "outer", enclosing environment.
pub struct Environment<'a> {
    store: HashMap<String, Object<'a>>,
    outer: Option<Rc<Environment<'a>>>,
}

impl<'a> Environment<'a> {
    pub fn new() -> Self {
        Environment {
            store: HashMap::new(),
            outer: None,
        }
    }

    pub fn new_enclosed(outer: Environment) -> Self {
        Environment {
            store: HashMap::new(),
            outer: Some(Rc::new(outer)),
        }
    }

    pub fn get(&self, name: &str) -> Option<Object> {
        self.store.get(name).cloned().or_else(|| {
            self.outer
                .as_ref()
                .and_then(|outer_env| outer_env.get(name))
        })
    }

    pub fn set(&mut self, name: String, val: Object) {
        self.store.insert(name, val);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_environments() {
        let mut outer_env = Environment::new();
        let mut env = Environment::new_enclosed(outer_env);
        env.set(
            "innerKey".to_string(),
            Object::Str("innerValue".to_string()),
        );
        outer_env.set(
            "outerKey".to_string(),
            Object::Str("outerValue".to_string()),
        );
        let obj = env.get("innerKey").expect("Failed to retrieve 'innerKey'");
        assert_eq!(
            obj.inspect(),
            "innerValue",
            "Expected 'innerValue'. Got: {}",
            obj.inspect()
        );
        let obj = env.get("outerKey").expect("Failed to retrieve 'outerKey'");
        assert_eq!(
            obj.inspect(),
            "outerValue",
            "Expected 'outerValue'. Got: {}",
            obj.inspect()
        );
    }
}
