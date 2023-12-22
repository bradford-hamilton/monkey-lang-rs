use crate::ast;
use crate::lexer::Lexer;
use crate::token::{Token, TokenType};
use lazy_static::lazy_static;
use std::collections::HashMap;

/// Operator precedence table.
#[derive(Copy, Clone, PartialEq, PartialOrd)]
#[repr(usize)]
pub enum OpPrecedence {
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
    static ref PRECEDENCES: HashMap<TokenType, OpPrecedence> = {
        let mut m = HashMap::new();
        m.insert(TokenType::EQUAL_EQUAL, OpPrecedence::Equal);
        m.insert(TokenType::BANG_EQUAL, OpPrecedence::Equal);
        m.insert(TokenType::LESS, OpPrecedence::LessGreater);
        m.insert(TokenType::GREATER, OpPrecedence::LessGreater);
        m.insert(TokenType::LESS_EQUAL, OpPrecedence::LessGreater);
        m.insert(TokenType::GREATER_EQUAL, OpPrecedence::LessGreater);
        m.insert(TokenType::PLUS, OpPrecedence::Sum);
        m.insert(TokenType::MINUS, OpPrecedence::Sum);
        m.insert(TokenType::SLASH, OpPrecedence::Product);
        m.insert(TokenType::STAR, OpPrecedence::Product);
        m.insert(TokenType::MOD, OpPrecedence::Modulo);
        m.insert(TokenType::AND, OpPrecedence::Logical);
        m.insert(TokenType::OR, OpPrecedence::Logical);
        m.insert(TokenType::LEFT_PAREN, OpPrecedence::Call);
        m.insert(TokenType::LEFT_BRACKET, OpPrecedence::Index);
        m
    };
}

/// Prefix, infix, and postfix function type definitions.
type PrefixParseFunc = fn(parser: &mut Parser) -> Box<dyn ast::Expression>;
type InfixParseFunc =
    fn(parser: &mut Parser, expr: Box<dyn ast::Expression>) -> Box<dyn ast::Expression>;
type PostfixParseFunc = fn(parser: &mut Parser) -> Box<dyn ast::Expression>;

/// Parser holds all the resources needed to parse a monkey program.
pub struct Parser {
    lexer: Lexer,
    errors: Vec<String>,

    current_token: Token,
    peek_token: Token,
    prev_token: Token,

    prefix_parse_funcs: HashMap<TokenType, PrefixParseFunc>,
    infix_parse_funcs: HashMap<TokenType, InfixParseFunc>,
    postfix_parse_funcs: HashMap<TokenType, PostfixParseFunc>,
}

impl Parser {
    /// New takes a Lexer, creates a Parser with that Lexer, sets the
    /// current and peek tokens, and returns the Parser.
    pub fn new(lexer: Lexer) -> Parser {
        let mut parser = Parser {
            lexer,
            errors: vec![],
            current_token: Token {
                line: 0,
                literal: String::from(""),
                token_type: TokenType::NONE,
            },
            peek_token: Token {
                line: 0,
                literal: String::from(""),
                token_type: TokenType::NONE,
            },
            prev_token: Token {
                line: 0,
                literal: String::from(""),
                token_type: TokenType::NONE,
            },
            prefix_parse_funcs: HashMap::new(),
            infix_parse_funcs: HashMap::new(),
            postfix_parse_funcs: HashMap::new(),
        };

        // Register all of our prefix parse funcs
        parser.register_prefix(TokenType::IDENTIFIER, parse_identifier);

        // Register all of our infix parse funcs
        parser.register_infix(TokenType::PLUS, parse_infix_expr);

        parser
    }

    fn register_prefix(&mut self, token_type: TokenType, func: PrefixParseFunc) {
        self.prefix_parse_funcs.insert(token_type, func);
    }

    fn register_infix(&mut self, token_type: TokenType, func: InfixParseFunc) {
        self.infix_parse_funcs.insert(token_type, func);
    }

    fn next_token(&mut self) {
        self.prev_token = self.current_token.clone();
        self.current_token = self.peek_token.clone();
        self.peek_token = self.lexer.clone().next_token();
    }

    fn current_token_precedence(&mut self) -> OpPrecedence {
        match PRECEDENCES.get(&self.current_token.token_type) {
            Some(p) => *p,
            None => OpPrecedence::Lowest,
        }
    }

    fn peek_token_precedence(&self) -> OpPrecedence {
        match PRECEDENCES.get(&self.peek_token.token_type) {
            Some(p) => *p,
            _ => OpPrecedence::Lowest,
        }
    }

    fn parse_expr(&mut self, precedence: OpPrecedence) -> Option<Box<dyn ast::Expression>> {
        let prefix = match self.prefix_parse_funcs.get(&self.current_token.token_type) {
            Some(&func) => func,
            _ => {
                self.no_prefix_parse_func_error(self.current_token.clone());
                return None;
            }
        };

        let mut left_expr = prefix(self);

        while !self.peek_token_type_is(TokenType::SEMICOLON)
            && precedence < self.peek_token_precedence()
        {
            let infix = match self.infix_parse_funcs.get(&self.peek_token.token_type) {
                Some(&func) => func,
                _ => {
                    return Some(left_expr);
                }
            };
            self.next_token();
            left_expr = infix(self, left_expr);
        }

        Some(left_expr)
    }

    fn peek_token_type_is(&self, token_type: TokenType) -> bool {
        self.peek_token.token_type == token_type
    }

    fn no_prefix_parse_func_error(&mut self, token: Token) {
        let msg = format!(
            "Line {}: No prefix parse function for {} found",
            self.current_token.line, token.literal,
        );
        self.errors.push(msg);
    }
}

fn parse_identifier(parser: &mut Parser) -> Box<dyn ast::Expression> {
    let contains_key = parser
        .postfix_parse_funcs
        .contains_key(&parser.peek_token.token_type);

    if contains_key {
        let postfix = parser.postfix_parse_funcs[&parser.peek_token.token_type];
        parser.next_token();
        return postfix(parser);
    }

    Box::new(ast::Identifier {
        token: parser.current_token.clone(),
        value: parser.current_token.literal.clone(),
    })
}

fn parse_infix_expr(
    parser: &mut Parser,
    left: Box<dyn ast::Expression>,
) -> Box<dyn ast::Expression> {
    let mut expr = ast::InfixExpression {
        token: parser.current_token.clone(),
        operator: parser.current_token.literal.clone(),
        left,
        right: Box::new(ast::ZeroValueExpression {}),
    };
    let precedence = parser.current_token_precedence();

    parser.next_token();

    expr.right = match parser.parse_expr(precedence) {
        Some(expr) => expr,
        _ => {
            let msg = format!(
                "Line {}: Failed to parse expression {}.",
                parser.current_token.line, parser.current_token.literal,
            );
            parser.errors.push(msg);
            Box::new(ast::ZeroValueExpression {})
        }
    };

    Box::new(expr)
}
