use crate::ast::{
    BlockStatement, Boolean, Expression, Identifier, IfExpression, InfixExpression, IntegerLiteral,
    LetStatement, PrefixExpression, Statement, ZeroValueExpression, ZeroValueStatement,
};
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
type PrefixParseFunc = fn(parser: &mut Parser) -> Box<dyn Expression>;
type InfixParseFunc = fn(parser: &mut Parser, expr: Box<dyn Expression>) -> Box<dyn Expression>;
type PostfixParseFunc = fn(parser: &mut Parser) -> Box<dyn Expression>;

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
        parser.register_prefix(TokenType::INTEGER, parse_integer_literal);
        parser.register_prefix(TokenType::BANG, parse_prefix_expr);
        parser.register_prefix(TokenType::MINUS, parse_prefix_expr);
        parser.register_prefix(TokenType::TRUE, parse_boolean);
        parser.register_prefix(TokenType::FALSE, parse_boolean);
        parser.register_prefix(TokenType::LEFT_PAREN, parse_grouped_expr);
        parser.register_prefix(TokenType::IF, parse_if_expr);

        // Register all of our infix parse funcs
        parser.register_infix(TokenType::PLUS, parse_infix_expr);

        // Register all of our postfix parse funcs
        // TODO...parser.register_postfix();

        // Read two tokens, so currentToken and peekToken are both set.
        parser.next_token();
        parser.next_token();

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

    fn current_token_type_is(&self, token_type: TokenType) -> bool {
        self.current_token.token_type == token_type
    }

    fn peek_token_type_is(&self, token_type: TokenType) -> bool {
        self.peek_token.token_type == token_type
    }

    fn expect_peek_type(&mut self, token_type: TokenType) -> bool {
        if self.peek_token_type_is(token_type) {
            self.next_token();
            return true;
        }

        self.peek_error(token_type);

        return false;
    }

    fn parse_expr(&mut self, precedence: OpPrecedence) -> Option<Box<dyn Expression>> {
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

    fn parse_block_statement(&mut self) -> BlockStatement {
        let mut block = BlockStatement {
            token: self.current_token.clone(),
            statements: vec![],
        };

        self.next_token();

        while !self.current_token_type_is(TokenType::RIGHT_BRACE)
            && !self.current_token_type_is(TokenType::EOF)
        {
            let statement = match self.parse_statement() {
                Some(stmt) => stmt,
                _ => {
                    return BlockStatement {
                        token: Token {
                            line: 0,
                            literal: String::from(""),
                            token_type: TokenType::NONE,
                        },
                        statements: vec![],
                    }
                }
            };

            self.next_token();
        }

        block
    }

    fn parse_statement(&mut self) -> Option<Box<dyn Statement>> {
        let ret = match self.current_token.token_type {
            TokenType::LET => parse_let_statement(self),
            TokenType::CONST => parse_const_statement(self),
            TokenType::RETURN => parse_return_statement(self),
            _ => parse_expr_statement(self),
        };

        Some(ret)
    }

    fn no_prefix_parse_func_error(&mut self, token: Token) {
        let msg = format!(
            "Line {}: No prefix parse function for {} found",
            self.current_token.line, token.literal,
        );
        self.errors.push(msg);
    }

    fn peek_error(&mut self, token_type: TokenType) {
        let msg = format!(
            "Line {}: Expected token to be {}, but found, {}",
            self.current_token.line, token_type, self.peek_token.literal,
        );
        self.errors.push(msg);
    }
}

fn parse_identifier(parser: &mut Parser) -> Box<dyn Expression> {
    let contains_key = parser
        .postfix_parse_funcs
        .contains_key(&parser.peek_token.token_type);

    if contains_key {
        let postfix = parser.postfix_parse_funcs[&parser.peek_token.token_type];
        parser.next_token();
        return postfix(parser);
    }

    Box::new(Identifier {
        token: parser.current_token.clone(),
        value: parser.current_token.literal.clone(),
    })
}

fn parse_infix_expr(parser: &mut Parser, left: Box<dyn Expression>) -> Box<dyn Expression> {
    let mut expr = InfixExpression {
        token: parser.current_token.clone(),
        operator: parser.current_token.literal.clone(),
        left,
        right: Box::new(ZeroValueExpression {}),
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
            Box::new(ZeroValueExpression {})
        }
    };

    Box::new(expr)
}

fn parse_prefix_expr(parser: &mut Parser) -> Box<dyn Expression> {
    let mut expr = PrefixExpression {
        token: parser.current_token.clone(),
        operator: parser.current_token.literal.clone(),
        right: Box::new(ZeroValueExpression {}),
    };

    parser.next_token();

    expr.right = match parser.parse_expr(OpPrecedence::Prefix) {
        Some(expr) => expr,
        _ => {
            let msg = format!(
                "Line {}: Failed to parse expression {}.",
                parser.current_token.line, parser.current_token.literal
            );
            parser.errors.push(msg);
            Box::new(ZeroValueExpression {})
        }
    };

    Box::new(expr)
}

fn parse_grouped_expr(parser: &mut Parser) -> Box<dyn Expression> {
    parser.next_token();

    let expr = match parser.parse_expr(OpPrecedence::Lowest) {
        Some(expr) => expr,
        _ => {
            let msg = format!(
                "Line {}: Failed to parse expression {}.",
                parser.current_token.line, parser.current_token.literal
            );
            parser.errors.push(msg);
            Box::new(ZeroValueExpression {})
        }
    };

    if !parser.expect_peek_type(TokenType::RIGHT_PAREN) {
        return Box::new(ZeroValueExpression {});
    }

    expr
}

fn parse_if_expr(parser: &mut Parser) -> Box<dyn Expression> {
    let mut expr = IfExpression {
        token: parser.current_token.clone(),
        condition: Box::new(ZeroValueExpression {}),
        consequence: BlockStatement {
            token: parser.current_token.clone(),
            statements: vec![],
        },
        alternative: BlockStatement {
            token: parser.current_token.clone(),
            statements: vec![],
        },
    };

    if !parser.expect_peek_type(TokenType::LEFT_PAREN) {
        return Box::new(ZeroValueExpression {});
    }

    parser.next_token();

    expr.condition = match parser.parse_expr(OpPrecedence::Lowest) {
        Some(cond) => cond,
        _ => {
            let msg = format!(
                "Line {}: Failed to parse expression {}.",
                parser.current_token.line, parser.current_token.literal
            );
            parser.errors.push(msg);
            Box::new(ZeroValueExpression {})
        }
    };

    if !parser.expect_peek_type(TokenType::RIGHT_PAREN) {
        return Box::new(ZeroValueExpression {});
    }
    if !parser.expect_peek_type(TokenType::LEFT_BRACE) {
        return Box::new(ZeroValueExpression {});
    }

    expr.consequence = parser.parse_block_statement();

    if parser.peek_token_type_is(TokenType::ELSE) {
        parser.next_token();

        if !parser.expect_peek_type(TokenType::LEFT_BRACE) {
            return Box::new(ZeroValueExpression {});
        }

        expr.alternative = parser.parse_block_statement();
    }

    Box::new(expr)
}

fn parse_let_statement(parser: &mut Parser) -> Box<dyn Statement> {
    let zero_value_token = Token {
        token_type: TokenType::NONE,
        literal: String::from(""),
        line: 0,
    };
    let zero_value_identifier = Identifier {
        token: zero_value_token,
        value: String::from(""),
    };
    let mut statement = LetStatement {
        token: parser.current_token.clone(),
        name: zero_value_identifier,
        value: Box::new(ZeroValueExpression {}),
    };

    if !parser.expect_peek_type(TokenType::IDENTIFIER) {
        return Box::new(ZeroValueStatement {});
    }

    statement.name = Identifier {
        token: parser.current_token.clone(),
        value: parser.current_token.literal.clone(),
    };

    if !parser.expect_peek_type(TokenType::EQUAL) {
        return Box::new(ZeroValueStatement {});
    }

    parser.next_token();

    statement.value = match parser.parse_expr(OpPrecedence::Lowest) {
        Some(expr) => expr,
        _ => {
            let msg = format!(
                "Line {}: Failed to parse expression {}.",
                parser.current_token.line, parser.current_token.literal
            );
            parser.errors.push(msg);
            Box::new(ZeroValueExpression {})
        }
    };

    // TODO: Handle function literal piece here
    // if fl, ok := stmt.Value.(*ast.FunctionLiteral); ok {
    // 	fl.Name = stmt.Name.Value
    // }

    if parser.peek_token_type_is(TokenType::SEMICOLON) {
        parser.next_token();
    }

    Box::new(statement)
}

fn parse_const_statement(parser: &mut Parser) -> Box<dyn Statement> {
    todo!()
}
fn parse_return_statement(parser: &mut Parser) -> Box<dyn Statement> {
    todo!()
}
fn parse_expr_statement(parser: &mut Parser) -> Box<dyn Statement> {
    todo!()
}

fn parse_integer_literal(parser: &mut Parser) -> Box<dyn Expression> {
    Box::new(IntegerLiteral {
        token: parser.current_token.clone(),

        // TODO: update to handle instead of unwrapping:
        value: parser.current_token.literal.parse::<usize>().unwrap(),
    })
}

fn parse_boolean(parser: &mut Parser) -> Box<dyn Expression> {
    Box::new(Boolean {
        token: parser.current_token.clone(),
        value: parser.current_token_type_is(TokenType::TRUE),
    })
}
