use std::fs;

mod ast;
mod compiler;
mod lexer;
mod object;
mod parser;
mod token;

fn main() {
    let filename = "test_input.mo";
    let input = match fs::read_to_string(filename) {
        Ok(string) => string,
        Err(error) => panic!("error reading file {}: {}", filename, error),
    };
    let lexer = lexer::Lexer::new(input);
    let _parser = parser::Parser::new(lexer);

    println!("heyo from the end");
}
