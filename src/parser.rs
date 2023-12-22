use crate::token::TokenType;
use lazy_static::lazy_static;
use std::collections::HashMap;

/// Operator precedence table.
pub enum OpPrecedences {
    Lowest = 1,
    /// =
    Equal = 2,
    /// && and ||
    Logical = 3,
    /// > or <
    LessGreater = 4,
    /// +
    Sum = 5,
    /// *
    Product = 6,
    /// %
    Modulo = 7,
    /// -x or !x
    Prefix = 8,
    /// myFunction(x)
    Call = 9,
    /// array[index], hash[key]
    Index = 10,
}

// PRECEDENCES is a lazy static hash map holding all the operators and their
// respective precedence.
lazy_static! {
    static ref PRECEDENCES: HashMap<TokenType, OpPrecedences> = {
        let mut m = HashMap::new();
        m.insert(TokenType::EQUAL_EQUAL, OpPrecedences::Equal);
        m.insert(TokenType::BANG_EQUAL, OpPrecedences::Equal);
        m.insert(TokenType::LESS, OpPrecedences::LessGreater);
        m.insert(TokenType::GREATER, OpPrecedences::LessGreater);
        m.insert(TokenType::LESS_EQUAL, OpPrecedences::LessGreater);
        m.insert(TokenType::GREATER_EQUAL, OpPrecedences::LessGreater);
        m.insert(TokenType::PLUS, OpPrecedences::Sum);
        m.insert(TokenType::MINUS, OpPrecedences::Sum);
        m.insert(TokenType::SLASH, OpPrecedences::Product);
        m.insert(TokenType::STAR, OpPrecedences::Product);
        m.insert(TokenType::MOD, OpPrecedences::Modulo);
        m.insert(TokenType::AND, OpPrecedences::Logical);
        m.insert(TokenType::OR, OpPrecedences::Logical);
        m.insert(TokenType::LEFT_PAREN, OpPrecedences::Call);
        m.insert(TokenType::LEFT_BRACKET, OpPrecedences::Index);
        m
    };
}
