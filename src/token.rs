use lazy_static::lazy_static;
use std::collections::HashMap;
use std::fmt;

/// Monkey's token types
#[derive(Debug, PartialEq, Eq, Hash, Copy, Clone)]
pub enum TokenType {
    /// Token/character we don't know about
    Illegal,
    /// End of file
    Eof,

    /// Identifiers & literals
    Identifier, // add, foobar, x, y, ...
    Integer,
    String,

    /// Operators
    Equal,
    Plus,
    PlusPlus,
    Minus,
    MinusMinus,
    Star,
    Slash,
    Mod,
    Bang,
    EqualEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    BangEqual,
    And,
    Or,

    /// Delimiters
    Comma,
    Colon,
    Semicolon,
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,

    /// Keywords
    Function,
    Let,
    Const,
    True,
    False,
    If,
    Else,
    Return,

    /// None
    None,
}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let printable = match *self {
            TokenType::Illegal => "ILLEGAL",
            TokenType::Eof => "EOF",
            TokenType::Identifier => "IDENTIFIER",
            TokenType::Integer => "INTEGER",
            TokenType::String => "STRING",
            TokenType::Equal => "EQUAL",
            TokenType::Plus => "PLUS",
            TokenType::PlusPlus => "PLUS_PLUS",
            TokenType::Minus => "MINUS",
            TokenType::MinusMinus => "MINUS_MINUS",
            TokenType::Star => "STAR",
            TokenType::Slash => "SLASH",
            TokenType::Mod => "MOD",
            TokenType::Bang => "BANG",
            TokenType::EqualEqual => "EQUAL_EQUAL",
            TokenType::Less => "LESS",
            TokenType::LessEqual => "LESS_EQUAL",
            TokenType::Greater => "GREATER",
            TokenType::GreaterEqual => "GREATER_EQUAL",
            TokenType::BangEqual => "BANG_EQUAL",
            TokenType::And => "AND",
            TokenType::Or => "OR",
            TokenType::Comma => "COMMA",
            TokenType::Colon => "COLON",
            TokenType::Semicolon => "SEMICOLON",
            TokenType::LeftParen => "LEFT_PAREN",
            TokenType::RightParen => "RIGHT_PAREN",
            TokenType::LeftBrace => "LEFT_BRACE",
            TokenType::RightBrace => "RIGHT_BRACE",
            TokenType::LeftBracket => "LEFT_BRACKET",
            TokenType::RightBracket => "RIGHT_BRACKET",
            TokenType::Function => "FUNCTION",
            TokenType::Let => "LET",
            TokenType::Const => "CONST",
            TokenType::True => "TRUE",
            TokenType::False => "FALSE",
            TokenType::If => "IF",
            TokenType::Else => "ELSE",
            TokenType::Return => "RETURN",
            TokenType::None => "NONE",
        };
        write!(f, "{}", printable)
    }
}

/// Token is a struct representing a Monkey token - holds a type and a literal
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Token {
    pub token_type: TokenType,
    pub literal: String,
    pub line: usize,
}

lazy_static! {
    static ref KEYWORDS: HashMap<&'static str, TokenType> = {
        let mut m = HashMap::new();
        m.insert("func", TokenType::Function);
        m.insert("let", TokenType::Let);
        m.insert("const", TokenType::Const);
        m.insert("true", TokenType::True);
        m.insert("false", TokenType::False);
        m.insert("if", TokenType::If);
        m.insert("else", TokenType::Else);
        m.insert("return", TokenType::Return);
        m
    };
}

pub fn look_up_identifier(identifier: &str) -> TokenType {
    *KEYWORDS.get(identifier).unwrap_or(&TokenType::Identifier)
}
