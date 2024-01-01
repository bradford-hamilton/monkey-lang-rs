use ast::RootNode;
use compiler::Compiler;
use lexer::Lexer;
use parser::Parser;
use std::fs;
use vm::VirtualMachine;

mod ast;
mod builtins;
mod bytecode;
mod compiler;
mod environment;
mod frame;
mod lexer;
mod object;
mod parser;
mod symbol_table;
mod token;
mod vm;

fn main() {
    let filename = "test_input.mo";
    let input = match fs::read_to_string(filename) {
        Ok(string) => string,
        Err(error) => panic!("error reading file {}: {}", filename, error),
    };
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program();

    compile_bytecode_and_run(program);
}

fn compile_bytecode_and_run(root_node: RootNode) {
    let mut compiler = Compiler::new();
    let result = compiler.compile(&root_node);

    match result {
        Ok(_) => {
            println!("compilation successful");
        }
        Err(e) => {
            println!("compilation failed with error: {}", e);
        }
    }

    let code = compiler.bytecode();
    let mut vm = VirtualMachine::new(code);

    vm.run();
}
