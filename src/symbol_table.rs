// A symbol table is a data structure used in interpreters & compilers to associate identifiers
// with information. It can be used in every phase, from lexing to code generation, to store and
// retrieve information about a given identifier (which can be called a symbol). Information such
// as its location, its scope, whether it was previously declared or not, of which type the
// associated value is, and anything else that useful while interpreting or compiling.

use std::collections::HashMap;

/// All available scopes
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

struct Symbol {
    name: String,
    scope: SymbolScope,
    index: i64,
}

struct SymbolTable {
    store: HashMap<String, Symbol>,
    num_definitions: i64,
    outer: Box<SymbolTable>,
    free_symbols: Vec<Symbol>,
}
