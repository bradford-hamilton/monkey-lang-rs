use std::collections::HashMap;
use std::fmt;

/// Monkey's token types
#[derive(Debug, PartialEq, Eq, Hash, Copy, Clone)]
pub enum TokenType {
    /// Token/character we don't know about
    ILLEGAL,
    /// End of file
    EOF,

    /// Identifiers & literals
    IDENTIFIER, // add, foobar, x, y, ...
    INTEGER,
    STRING,

    /// Operators
    EQUAL,
    PLUS,
    PLUS_PLUS,
    MINUS,
    MINUS_MINUS,
    STAR,
    SLASH,
    MOD,
    BANG,
    EQUAL_EQUAL,
    LESS,
    LESS_EQUAL,
    GREATER,
    GREATER_EQUAL,
    BANG_EQUAL,
    AND,
    OR,

    /// Delimiters
    COMMA,
    COLON,
    SEMICOLON,
    LEFT_PAREN,
    RIGHT_PAREN,
    LEFT_BRACE,
    RIGHT_BRACE,
    LEFT_BRACKET,
    RIGHT_BRACKET,

    /// Keywords
    FUNCTION,
    LET,
    CONST,
    TRUE,
    FALSE,
    IF,
    ELSE,
    RETURN,

    /// None
    NONE,
}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let printable = match *self {
            TokenType::ILLEGAL => "ILLEGAL",
            TokenType::EOF => "EOF",
            TokenType::IDENTIFIER => "IDENTIFIER",
            TokenType::INTEGER => "INTEGER",
            TokenType::STRING => "STRING",
            TokenType::EQUAL => "EQUAL",
            TokenType::PLUS => "PLUS",
            TokenType::PLUS_PLUS => "PLUS_PLUS",
            TokenType::MINUS => "MINUS",
            TokenType::MINUS_MINUS => "MINUS_MINUS",
            TokenType::STAR => "STAR",
            TokenType::SLASH => "SLASH",
            TokenType::MOD => "MOD",
            TokenType::BANG => "BANG",
            TokenType::EQUAL_EQUAL => "EQUAL_EQUAL",
            TokenType::LESS => "LESS",
            TokenType::LESS_EQUAL => "LESS_EQUAL",
            TokenType::GREATER => "GREATER",
            TokenType::GREATER_EQUAL => "GREATER_EQUAL",
            TokenType::BANG_EQUAL => "BANG_EQUAL",
            TokenType::AND => "AND",
            TokenType::OR => "OR",
            TokenType::COMMA => "COMMA",
            TokenType::COLON => "COLON",
            TokenType::SEMICOLON => "SEMICOLON",
            TokenType::LEFT_PAREN => "LEFT_PAREN",
            TokenType::RIGHT_PAREN => "RIGHT_PAREN",
            TokenType::LEFT_BRACE => "LEFT_BRACE",
            TokenType::RIGHT_BRACE => "RIGHT_BRACE",
            TokenType::LEFT_BRACKET => "LEFT_BRACKET",
            TokenType::RIGHT_BRACKET => "RIGHT_BRACKET",
            TokenType::FUNCTION => "FUNCTION",
            TokenType::LET => "LET",
            TokenType::CONST => "CONST",
            TokenType::TRUE => "TRUE",
            TokenType::FALSE => "FALSE",
            TokenType::IF => "IF",
            TokenType::ELSE => "ELSE",
            TokenType::RETURN => "RETURN",
            TokenType::NONE => "NONE",
        };
        write!(f, "{}", printable)
    }
}

/// Token is a struct representing a Monkey token - holds a type and a literal
#[derive(Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub literal: String,
    pub line: usize,
}

// TODO: can this be a static mapping of some sort?
struct Keywords;

impl Keywords {
    pub fn all() -> HashMap<String, TokenType> {
        let mut keywords = HashMap::new();

        keywords.insert("func".to_owned(), TokenType::FUNCTION);
        keywords.insert("let".to_owned(), TokenType::LET);
        keywords.insert("const".to_owned(), TokenType::CONST);
        keywords.insert("true".to_owned(), TokenType::TRUE);
        keywords.insert("false".to_owned(), TokenType::FALSE);
        keywords.insert("if".to_owned(), TokenType::IF);
        keywords.insert("else".to_owned(), TokenType::ELSE);
        keywords.insert("return".to_owned(), TokenType::RETURN);

        keywords
    }
}

/// look_up_identifier checks our keywords map for the scanned keyword. If it finds one, then
/// the keyword's type is returned. If not, the user defined IDENTIFIER is returned
pub fn look_up_identifier(identifier: &String) -> TokenType {
    if Keywords::all().contains_key(identifier) {
        return Keywords::all()[identifier];
    }
    TokenType::IDENTIFIER
}
