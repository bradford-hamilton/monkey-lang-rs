// A symbol table is a data structure used in interpreters & compilers to associate identifiers
// with information. It can be used in every phase, from lexing to code generation, to store and
// retrieve information about a given identifier (which can be called a symbol). Information such
// as its location, its scope, whether it was previously declared or not, of which type the
// associated value is, and anything else that useful while interpreting or compiling.

use std::collections::HashMap;

/// All available scopes
#[derive(Copy, Clone)]
pub enum SymbolScope {
    Global,
    Local,
    Builtin,
    Free,
    Function,
}

impl SymbolScope {
    fn as_str(&self) -> &'static str {
        match self {
            SymbolScope::Global => "GLOBAL",
            SymbolScope::Local => "LOCAL",
            SymbolScope::Builtin => "BUILTIN",
            SymbolScope::Free => "FREE",
            SymbolScope::Function => "FUNCTION",
        }
    }
}

#[derive(Clone)]
pub struct Symbol {
    name: String,
    scope: SymbolScope,
    index: i64,
}

pub struct SymbolTable {
    store: HashMap<String, Symbol>,
    num_definitions: usize,
    outer: Option<Box<SymbolTable>>,
    free_symbols: Vec<Symbol>,
}

impl SymbolTable {
    pub fn new() -> Self {
        SymbolTable {
            store: HashMap::new(),
            num_definitions: 0,
            outer: None,
            free_symbols: vec![],
        }
    }

    pub fn new_enclosed(outer: Option<Box<SymbolTable>>) -> Self {
        SymbolTable {
            store: HashMap::new(),
            num_definitions: 0,
            outer,
            free_symbols: vec![],
        }
    }

    /// define takes a name and creates a symbol, an index, and assigns scope. It then assigns
    /// the symbol to the SymbolTable's store, increases num_definitions and returns the symbol.
    fn define(&mut self, name: String) -> Symbol {
        let scope = if self.outer.is_none() {
            SymbolScope::Global
        } else {
            SymbolScope::Local
        };
        let sym = Symbol {
            name: name.clone(),
            scope,
            index: self.num_definitions as i64,
        };

        self.store.insert(name.clone(), sym.clone());
        self.num_definitions += 1;

        sym
    }

    /// define_builtin creates and returns a symbol within builtin scope.
    pub fn define_builtin(&mut self, index: i64, name: String) -> Symbol {
        let sym = Symbol {
            name: name.clone(),
            index,
            scope: SymbolScope::Builtin,
        };
        self.store.insert(name.clone(), sym.clone());

        sym
    }

    fn define_free(&mut self, original: Symbol) -> Symbol {
        self.free_symbols.push(original.clone());

        let sym = Symbol {
            name: original.clone().name,
            scope: SymbolScope::Free,
            index: (self.free_symbols.len() - 1) as i64,
        };

        self.store.insert(original.clone().name, sym.clone());

        sym
    }

    /// defines a symbol with function scope.
    fn define_function(&mut self, name: String) -> Symbol {
        let sym = Symbol {
            name: name.clone(),
            scope: SymbolScope::Function,
            index: 0,
        };
        self.store.insert(name, sym.clone());

        sym
    }

    pub fn resolve(&mut self, name: &str) -> Option<Symbol> {
        match self.store.get(name) {
            Some(symbol) => Some(symbol.clone()),
            None => match &mut self.outer {
                Some(outer) => {
                    let obj = outer.resolve(name);
                    if let Some(symbol) = obj {
                        match symbol.scope {
                            SymbolScope::Global | SymbolScope::Builtin => Some(symbol),
                            _ => Some(self.define_free(symbol)),
                        }
                    } else {
                        None
                    }
                }
                None => None,
            },
        }
    }
}
