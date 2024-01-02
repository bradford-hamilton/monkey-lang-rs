use crate::ast::{
    ArrayLiteral, BlockStatement, Boolean, CallExpression, ConstStatement, Expression,
    ExpressionKey, ExpressionStatement, FunctionLiteral, HashLiteral, Identifier, IfExpression,
    IndexExpression, InfixExpression, IntegerLiteral, LetStatement, Node, PrefixExpression,
    ReturnStatement, RootNode, Statement, StringLiteral, ZeroValueExpression, ZeroValueStatement,
};
use crate::lexer::Lexer;
use crate::token::{Token, TokenType};
use lazy_static::lazy_static;
use std::cell::RefCell;
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
        m.insert(TokenType::EqualEqual, OpPrecedence::Equal);
        m.insert(TokenType::BangEqual, OpPrecedence::Equal);
        m.insert(TokenType::Less, OpPrecedence::LessGreater);
        m.insert(TokenType::Greater, OpPrecedence::LessGreater);
        m.insert(TokenType::LessEqual, OpPrecedence::LessGreater);
        m.insert(TokenType::GreaterEqual, OpPrecedence::LessGreater);
        m.insert(TokenType::Plus, OpPrecedence::Sum);
        m.insert(TokenType::Minus, OpPrecedence::Sum);
        m.insert(TokenType::Slash, OpPrecedence::Product);
        m.insert(TokenType::Star, OpPrecedence::Product);
        m.insert(TokenType::Mod, OpPrecedence::Modulo);
        m.insert(TokenType::And, OpPrecedence::Logical);
        m.insert(TokenType::Or, OpPrecedence::Logical);
        m.insert(TokenType::LeftParen, OpPrecedence::Call);
        m.insert(TokenType::LeftBracket, OpPrecedence::Index);
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
    /// New creates a Parser with the given Lexer, registers all
    /// parser prefix/infix/postfix functions, and sets the current
    /// and peek tokens.
    pub fn new(lexer: Lexer) -> Parser {
        let mut parser = Parser {
            lexer,
            errors: vec![],
            current_token: Token {
                line: 0,
                literal: String::from(""),
                token_type: TokenType::None,
            },
            peek_token: Token {
                line: 0,
                literal: String::from(""),
                token_type: TokenType::None,
            },
            prev_token: Token {
                line: 0,
                literal: String::from(""),
                token_type: TokenType::None,
            },
            prefix_parse_funcs: HashMap::new(),
            infix_parse_funcs: HashMap::new(),
            postfix_parse_funcs: HashMap::new(),
        };

        // Register all of our prefix parse funcs
        parser.register_prefix(TokenType::Identifier, parse_identifier);
        parser.register_prefix(TokenType::Integer, parse_integer_literal);
        parser.register_prefix(TokenType::Bang, parse_prefix_expr);
        parser.register_prefix(TokenType::Minus, parse_prefix_expr);
        parser.register_prefix(TokenType::True, parse_boolean);
        parser.register_prefix(TokenType::False, parse_boolean);
        parser.register_prefix(TokenType::LeftParen, parse_grouped_expr);
        parser.register_prefix(TokenType::If, parse_if_expr);
        parser.register_prefix(TokenType::Function, parse_function_literal);
        parser.register_prefix(TokenType::String, parse_string_literal);
        parser.register_prefix(TokenType::LeftBracket, parse_array_literal);
        parser.register_prefix(TokenType::LeftBrace, parse_hash_literal);

        // Register all of our infix parse funcs
        parser.register_infix(TokenType::Plus, parse_infix_expr);
        parser.register_infix(TokenType::Minus, parse_infix_expr);
        parser.register_infix(TokenType::Slash, parse_infix_expr);
        parser.register_infix(TokenType::Star, parse_infix_expr);
        parser.register_infix(TokenType::Mod, parse_infix_expr);
        parser.register_infix(TokenType::EqualEqual, parse_infix_expr);
        parser.register_infix(TokenType::BangEqual, parse_infix_expr);
        parser.register_infix(TokenType::Less, parse_infix_expr);
        parser.register_infix(TokenType::Greater, parse_infix_expr);
        parser.register_infix(TokenType::LessEqual, parse_infix_expr);
        parser.register_infix(TokenType::GreaterEqual, parse_infix_expr);
        parser.register_infix(TokenType::LeftParen, parse_call_expr);
        parser.register_infix(TokenType::LeftBracket, parse_index_expr);
        parser.register_infix(TokenType::And, parse_infix_expr);
        parser.register_infix(TokenType::Or, parse_infix_expr);

        // TODO:
        // Register all of our postfix parse funcs
        // p.registerPostfix(token.PlusPlus, p.parsePostfixExpression)
        // p.registerPostfix(token.MinusMinus, p.parsePostfixExpression)

        // Read two tokens, so currentToken and peekToken are both set.
        parser.next_token();
        parser.next_token();

        parser
    }

    /// parse_program parses tokens and creates an AST. It returns the RootNode
    /// which holds a slice of Statements (and in turn, the rest of the tree).
    pub fn parse_program(&mut self) -> RootNode {
        let mut root_node = RootNode { statements: vec![] };

        while !self.current_token_type_is(TokenType::Eof) {
            let stmt = self.parse_statement();
            if !is_zero_value_statement(&stmt) {
                root_node.statements.push(stmt);
            }
            self.next_token();
        }

        root_node
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
        self.peek_token = self.lexer.next_token();
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

        false
    }

    fn parse_expression(&mut self, precedence: OpPrecedence) -> Option<Box<dyn Expression>> {
        let prefix = match self.prefix_parse_funcs.get(&self.current_token.token_type) {
            Some(&func) => func,
            _ => {
                self.no_prefix_parse_func_error(self.current_token.clone());
                return None;
            }
        };

        let mut left_expr = prefix(self);

        while !self.peek_token_type_is(TokenType::Semicolon)
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

    fn parse_expression_list(&mut self, end: TokenType) -> Vec<Box<dyn Expression>> {
        let mut list: Vec<Box<dyn Expression>> = vec![];

        if self.peek_token_type_is(end) {
            self.next_token();
            return list;
        }

        self.next_token();

        let expr = match self.parse_expression(OpPrecedence::Lowest) {
            Some(expr) => expr,
            _ => {
                let msg = format!(
                    "Line {}: Failed to parse expression {}.",
                    self.current_token.line, self.current_token.literal
                );
                self.errors.push(msg);
                Box::new(ZeroValueExpression {})
            }
        };

        list.push(expr);

        while self.peek_token_type_is(TokenType::Comma) {
            self.next_token();
            self.next_token();

            let expr = match self.parse_expression(OpPrecedence::Lowest) {
                Some(expr) => expr,
                _ => {
                    let msg = format!(
                        "Line {}: Failed to parse expression {}.",
                        self.current_token.line, self.current_token.literal
                    );
                    self.errors.push(msg);
                    Box::new(ZeroValueExpression {})
                }
            };

            list.push(expr);
        }

        if !self.expect_peek_type(end) {
            return vec![Box::new(ZeroValueExpression {})];
        }

        list
    }

    fn parse_block_statement(&mut self) -> BlockStatement {
        let mut block = BlockStatement {
            token: self.current_token.clone(),
            statements: vec![],
        };

        self.next_token();

        while !self.current_token_type_is(TokenType::RightBrace)
            && !self.current_token_type_is(TokenType::Eof)
        {
            let stmt = self.parse_statement();
            if !is_zero_value_statement(&stmt) {
                block.statements.push(stmt);
            }
            self.next_token();
        }

        block
    }

    fn parse_statement(&mut self) -> Box<dyn Statement> {
        match self.current_token.token_type {
            TokenType::Let => parse_let_statement(self),
            TokenType::Const => parse_const_statement(self),
            TokenType::Return => parse_return_statement(self),
            _ => parse_expr_statement(self),
        }
    }

    fn parse_function_params(&mut self) -> Vec<Identifier> {
        let mut identifiers = vec![];

        if self.peek_token_type_is(TokenType::RightParen) {
            self.next_token();
            return identifiers;
        }

        self.next_token();

        identifiers.push(Identifier {
            token: self.current_token.clone(),
            value: self.current_token.literal.clone(),
        });

        while self.peek_token_type_is(TokenType::Comma) {
            self.next_token();
            self.next_token();

            identifiers.push(Identifier {
                token: self.current_token.clone(),
                value: self.current_token.literal.clone(),
            });
        }

        if !self.expect_peek_type(TokenType::RightParen) {
            return vec![];
        }

        identifiers
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

    expr.right = match parser.parse_expression(precedence) {
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

fn parse_call_expr(parser: &mut Parser, func: Box<dyn Expression>) -> Box<dyn Expression> {
    Box::new(CallExpression {
        token: parser.current_token.clone(),
        func,
        arguments: parser.parse_expression_list(TokenType::RightParen),
    })
}

fn parse_index_expr(parser: &mut Parser, left: Box<dyn Expression>) -> Box<dyn Expression> {
    let mut expr = IndexExpression {
        token: parser.current_token.clone(),
        left,
        index: Box::new(ZeroValueExpression {}),
    };

    parser.next_token();

    expr.index = match parser.parse_expression(OpPrecedence::Lowest) {
        Some(box_expr) => box_expr,
        _ => {
            let msg = format!(
                "Line {}: Failed to parse expression {}.",
                parser.current_token.line, parser.current_token.literal,
            );
            parser.errors.push(msg);
            Box::new(ZeroValueExpression {})
        }
    };

    if !parser.expect_peek_type(TokenType::RightBracket) {
        return Box::new(ZeroValueExpression {});
    }

    Box::new(expr)
}

fn parse_prefix_expr(parser: &mut Parser) -> Box<dyn Expression> {
    let mut expr = PrefixExpression {
        token: parser.current_token.clone(),
        operator: parser.current_token.literal.clone(),
        right: Box::new(ZeroValueExpression {}),
    };

    parser.next_token();

    expr.right = match parser.parse_expression(OpPrecedence::Prefix) {
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

    let expr = match parser.parse_expression(OpPrecedence::Lowest) {
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

    if !parser.expect_peek_type(TokenType::RightParen) {
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

    if !parser.expect_peek_type(TokenType::LeftParen) {
        return Box::new(ZeroValueExpression {});
    }

    parser.next_token();

    expr.condition = match parser.parse_expression(OpPrecedence::Lowest) {
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

    if !parser.expect_peek_type(TokenType::RightParen) {
        return Box::new(ZeroValueExpression {});
    }
    if !parser.expect_peek_type(TokenType::LeftBrace) {
        return Box::new(ZeroValueExpression {});
    }

    expr.consequence = parser.parse_block_statement();

    if parser.peek_token_type_is(TokenType::Else) {
        parser.next_token();

        if !parser.expect_peek_type(TokenType::LeftBrace) {
            return Box::new(ZeroValueExpression {});
        }

        expr.alternative = parser.parse_block_statement();
    }

    Box::new(expr)
}

fn parse_let_statement(parser: &mut Parser) -> Box<dyn Statement> {
    let zero_value_token = Token {
        token_type: TokenType::None,
        literal: String::from(""),
        line: 0,
    };
    let zero_value_identifier = Identifier {
        token: zero_value_token,
        value: String::from(""),
    };
    let mut stmt = LetStatement {
        token: parser.current_token.clone(),
        name: zero_value_identifier,
        value: Box::new(ZeroValueExpression {}),
    };

    if !parser.expect_peek_type(TokenType::Identifier) {
        return Box::new(ZeroValueStatement {});
    }

    stmt.name = Identifier {
        token: parser.current_token.clone(),
        value: parser.current_token.literal.clone(),
    };

    if !parser.expect_peek_type(TokenType::Equal) {
        return Box::new(ZeroValueStatement {});
    }

    parser.next_token();

    stmt.value = match parser.parse_expression(OpPrecedence::Lowest) {
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

    if let Some(func_literal) = stmt.value.as_any().downcast_ref::<FunctionLiteral>() {
        *func_literal.name.borrow_mut() = stmt.name.value.clone();
    }

    if parser.peek_token_type_is(TokenType::Semicolon) {
        parser.next_token();
    }

    Box::new(stmt)
}

fn parse_const_statement(parser: &mut Parser) -> Box<dyn Statement> {
    let zero_value_token = Token {
        token_type: TokenType::None,
        literal: String::from(""),
        line: 0,
    };
    let zero_value_identifier = Identifier {
        token: zero_value_token,
        value: String::from(""),
    };
    let mut stmt = ConstStatement {
        token: parser.current_token.clone(),
        name: zero_value_identifier,
        value: Box::new(ZeroValueExpression {}),
    };

    if !parser.expect_peek_type(TokenType::Identifier) {
        return Box::new(ZeroValueStatement {});
    }

    stmt.name = Identifier {
        token: parser.current_token.clone(),
        value: parser.current_token.literal.clone(),
    };

    if !parser.expect_peek_type(TokenType::Equal) {
        return Box::new(ZeroValueStatement {});
    }

    parser.next_token();

    stmt.value = match parser.parse_expression(OpPrecedence::Lowest) {
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

    if let Some(func_literal) = stmt.value.as_any().downcast_ref::<FunctionLiteral>() {
        *func_literal.name.borrow_mut() = stmt.name.value.clone();
    }

    if parser.peek_token_type_is(TokenType::Semicolon) {
        parser.next_token();
    }

    Box::new(stmt)
}

fn parse_return_statement(parser: &mut Parser) -> Box<dyn Statement> {
    let mut stmt = ReturnStatement {
        token: parser.current_token.clone(),
        return_value: Box::new(ZeroValueExpression {}),
    };

    parser.next_token();

    stmt.return_value = match parser.parse_expression(OpPrecedence::Lowest) {
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

    if parser.peek_token_type_is(TokenType::Semicolon) {
        parser.next_token();
    }

    Box::new(stmt)
}

fn parse_expr_statement(parser: &mut Parser) -> Box<dyn Statement> {
    let mut stmt = ExpressionStatement {
        token: parser.current_token.clone(),
        expression: Box::new(ZeroValueExpression {}),
    };

    stmt.expression = match parser.parse_expression(OpPrecedence::Lowest) {
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

    if parser.peek_token_type_is(TokenType::Semicolon) {
        parser.next_token();
    }

    Box::new(stmt)
}

fn parse_function_literal(parser: &mut Parser) -> Box<dyn Expression> {
    let zero_value_token = Token {
        token_type: TokenType::None,
        literal: String::from(""),
        line: 0,
    };
    let mut func_literal = FunctionLiteral {
        token: parser.current_token.clone(),
        parameters: vec![],
        body: BlockStatement {
            token: zero_value_token,
            statements: vec![],
        },
        name: RefCell::new(String::from("")),
    };

    if !parser.expect_peek_type(TokenType::LeftParen) {
        return Box::new(ZeroValueExpression {});
    }

    func_literal.parameters = parser.parse_function_params();

    if !parser.expect_peek_type(TokenType::LeftBrace) {
        return Box::new(ZeroValueExpression {});
    }

    func_literal.body = parser.parse_block_statement();

    Box::new(func_literal)
}

fn parse_array_literal(parser: &mut Parser) -> Box<dyn Expression> {
    Box::new(ArrayLiteral {
        token: parser.current_token.clone(),
        elements: parser.parse_expression_list(TokenType::RightBracket),
    })
}

fn parse_hash_literal(parser: &mut Parser) -> Box<dyn Expression> {
    let mut hash = HashLiteral {
        token: parser.current_token.clone(),
        pairs: HashMap::new(),
    };

    while !parser.peek_token_type_is(TokenType::RightBrace) {
        parser.next_token();

        let key = match parser.parse_expression(OpPrecedence::Lowest) {
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

        if !parser.expect_peek_type(TokenType::Colon) {
            return Box::new(ZeroValueExpression {});
        }

        parser.next_token();

        let value = match parser.parse_expression(OpPrecedence::Lowest) {
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

        hash.pairs.insert(ExpressionKey::new(key), value);

        if !parser.peek_token_type_is(TokenType::RightBrace)
            && !parser.expect_peek_type(TokenType::Comma)
        {
            return Box::new(ZeroValueExpression {});
        }
    }

    if !parser.expect_peek_type(TokenType::RightBrace) {
        return Box::new(ZeroValueExpression {});
    }

    Box::new(hash)
}

fn parse_string_literal(parser: &mut Parser) -> Box<dyn Expression> {
    Box::new(StringLiteral {
        token: parser.current_token.clone(),
        value: parser.current_token.literal.clone(),
    })
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
        value: parser.current_token_type_is(TokenType::True),
    })
}

fn is_zero_value_statement(stmt: &Box<dyn Statement>) -> bool {
    stmt.as_any().is::<ZeroValueStatement>()
}

#[cfg(test)]
mod tests {
    use super::*;

    enum ExpectedValue {
        Int(i64),
        Bool(bool),
        Ident(String),
    }

    fn check_parser_errors(parser: &Parser) {
        if parser.errors.is_empty() {
            return;
        }
        panic!("parser errors: {:?}", parser.errors);
    }

    fn test_let_statement(stmt: &Box<dyn Statement>, expected_identifier: &str) {
        if stmt.token_literal() != "let" {
            panic!("statement.token_literal not let: {}", stmt.token_literal());
        }
        if !stmt.as_any().is::<LetStatement>() {
            panic!("statement is not a LetStatement");
        }
        let let_statement = stmt.as_any().downcast_ref::<LetStatement>().unwrap();
        if let_statement.name.string() != expected_identifier {
            panic!("incorrect identifier for let statement");
        }
    }

    fn test_const_statement(stmt: &Box<dyn Statement>, expected_identifier: &str) {
        if stmt.token_literal() != "const" {
            panic!(
                "statement.token_literal not const: {}",
                stmt.token_literal()
            );
        }
        if !stmt.as_any().is::<ConstStatement>() {
            panic!("statement is not a ConstStatement");
        }
        let const_statement = stmt.as_any().downcast_ref::<ConstStatement>().unwrap();
        if const_statement.name.string() != expected_identifier {
            panic!("incorrect identifier for let statement");
        }
    }

    fn test_return_statement(stmt: &Box<dyn Statement>) {
        if stmt.token_literal() != "return" {
            panic!(
                "statement.token_literal is not return: {}",
                stmt.token_literal()
            );
        }
        if !stmt.as_any().is::<ReturnStatement>() {
            panic!("statement is not a ReturnStatement");
        }
    }

    fn test_literal_expression(expr: &Box<dyn Expression>, expected_value: ExpectedValue) {
        match expected_value {
            ExpectedValue::Int(value) => {
                test_integer_literal(expr, value);
            }
            ExpectedValue::Bool(value) => {
                test_boolean_literal(expr, value);
            }
            ExpectedValue::Ident(value) => {
                test_identifier(expr, value);
            }
        }
    }

    fn test_integer_literal(expr: &Box<dyn Expression>, value: i64) {
        if !expr.as_any().is::<IntegerLiteral>() {
            panic!("expression is not an integer literal");
        }
        let int_lit = expr.as_any().downcast_ref::<IntegerLiteral>().unwrap();
        if int_lit.value != value as usize {
            panic!(
                "integer literal value incorrect, expected: {}, got: {}",
                value, int_lit.value,
            )
        }
    }

    fn test_boolean_literal(expr: &Box<dyn Expression>, value: bool) {
        if !expr.as_any().is::<Boolean>() {
            panic!("expression is not a boolean");
        }
        let boool = expr.as_any().downcast_ref::<Boolean>().unwrap();
        if boool.value != value {
            panic!(
                "boolean value incorrect, expected: {}, got: {}",
                value, boool.value,
            )
        }
    }

    fn test_identifier(expr: &Box<dyn Expression>, value: String) {
        if !expr.as_any().is::<Identifier>() {
            panic!("expression is not a identifier");
        }
        let ident = expr.as_any().downcast_ref::<Identifier>().unwrap();
        if ident.value != value {
            panic!(
                "identifier value incorrect, expected: {}, got: {}",
                value, ident.value,
            )
        }
    }

    fn test_infix_expression(
        expr: &Box<dyn Expression>,
        left: ExpectedValue,
        operator: &str,
        right: ExpectedValue,
    ) {
        if !expr.as_any().is::<InfixExpression>() {
            panic!("expression is not an infix expression");
        }
        let infix = expr.as_any().downcast_ref::<InfixExpression>().unwrap();

        match left {
            ExpectedValue::Ident(left_data) => {
                test_literal_expression(&infix.left, ExpectedValue::Ident(left_data))
            }
            ExpectedValue::Int(left_data) => {
                test_literal_expression(&infix.left, ExpectedValue::Int(left_data))
            }
            ExpectedValue::Bool(left_data) => {
                test_literal_expression(&infix.left, ExpectedValue::Bool(left_data))
            }
        }

        if infix.operator != operator {
            panic!(
                "infix.operator incorrect, got: {}, expected: {}",
                infix.operator, operator
            );
        }

        match right {
            ExpectedValue::Ident(right_data) => {
                test_literal_expression(&infix.right, ExpectedValue::Ident(right_data));
            }
            ExpectedValue::Int(right_data) => {
                test_literal_expression(&infix.right, ExpectedValue::Int(right_data));
            }
            ExpectedValue::Bool(right_data) => {
                test_literal_expression(&infix.right, ExpectedValue::Bool(right_data));
            }
        }
    }

    #[test]
    fn test_let_statements() {
        let tests = vec![
            (String::from("let x = 5;"), "x", ExpectedValue::Int(5)),
            (
                String::from("let y = true;"),
                "y",
                ExpectedValue::Bool(true),
            ),
            (
                String::from("let foobar = y;"),
                "foobar",
                ExpectedValue::Ident("y".to_string()),
            ),
        ];

        for (input, expected_identifier, expected_value) in tests {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();

            check_parser_errors(&parser);
            assert_eq!(
                program.statements.len(),
                1,
                "program does not contain 1 statement: contains {} statements",
                program.statements.len(),
            );

            let stmt = &program.statements[0];
            test_let_statement(stmt, expected_identifier);

            let ls = stmt.as_any().downcast_ref::<LetStatement>().unwrap();
            test_literal_expression(&ls.value, expected_value);
        }
    }

    #[test]
    fn test_const_statements() {
        let tests = vec![
            (String::from("const x = 5;"), "x", ExpectedValue::Int(5)),
            (
                String::from("const y = true;"),
                "y",
                ExpectedValue::Bool(true),
            ),
            (
                String::from("const foobar = y;"),
                "foobar",
                ExpectedValue::Ident("y".to_string()),
            ),
        ];

        for (input, expected_identifier, expected_value) in tests {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();

            check_parser_errors(&parser);
            assert_eq!(
                program.statements.len(),
                1,
                "program does not contain 1 statement: contains {} statements",
                program.statements.len(),
            );

            let stmt = &program.statements[0];
            test_const_statement(stmt, expected_identifier);

            let ls = stmt.as_any().downcast_ref::<ConstStatement>().unwrap();
            test_literal_expression(&ls.value, expected_value);
        }
    }

    #[test]
    fn test_return_statements() {
        let tests = vec![
            (String::from("return 5;"), ExpectedValue::Int(5)),
            (String::from("return true;"), ExpectedValue::Bool(true)),
            (
                String::from("return foobar;"),
                ExpectedValue::Ident("foobar".to_string()),
            ),
        ];

        for (input, expected_value) in tests {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();

            check_parser_errors(&parser);
            assert_eq!(
                program.statements.len(),
                1,
                "program does not contain 1 statement: contains {} statements",
                program.statements.len(),
            );

            let stmt = &program.statements[0];
            test_return_statement(stmt);

            let rs = stmt.as_any().downcast_ref::<ReturnStatement>().unwrap();
            test_literal_expression(&rs.return_value, expected_value);
        }
    }

    #[test]
    fn test_identifier_expressions() {
        let input = String::from("foobar;");
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        check_parser_errors(&parser);
        assert_eq!(
            program.statements.len(),
            1,
            "program does not contain 1 statement: contains {} statements",
            program.statements.len(),
        );

        let stmt = &program.statements[0];
        if !stmt.as_any().is::<ExpressionStatement>() {
            panic!("expression is not an expression statement");
        }
        let exp_stmt = stmt.as_any().downcast_ref::<ExpressionStatement>().unwrap();
        if let Some(identifier) = &exp_stmt.expression.as_any().downcast_ref::<Identifier>() {
            if identifier.value != "foobar" {}
        } else {
            panic!("expression statement's expression is not an identifier");
        }
    }

    #[test]
    fn test_integer_literal_expressions() {
        let input = String::from("5;");
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        check_parser_errors(&parser);
        assert_eq!(
            program.statements.len(),
            1,
            "program does not contain 1 statement: contains {} statements",
            program.statements.len(),
        );

        let stmt = &program.statements[0];
        if !stmt.as_any().is::<ExpressionStatement>() {
            panic!(
                "statement is not an expression statement: {:?}, {:?}",
                stmt.as_ref().token_literal(),
                stmt.as_ref().string()
            );
        }
        let expr_stmt = stmt.as_any().downcast_ref::<ExpressionStatement>().unwrap();

        if let Some(int_lit) = expr_stmt
            .expression
            .as_any()
            .downcast_ref::<IntegerLiteral>()
        {
            if int_lit.value != 5 {
                panic!(
                    "integer literal's value incorrect, expected: {}, got: {}",
                    5, int_lit.value
                );
            }
        } else {
            panic!("expression statement is not an integer literal");
        }
    }

    #[test]
    fn test_parsing_prefix_expressions() {
        let prefix_tests = vec![
            ("!5;", "!", ExpectedValue::Int(5)),
            ("-15", "-", ExpectedValue::Int(15)),
            ("!true", "!", ExpectedValue::Bool(true)),
            ("!false", "!", ExpectedValue::Bool(false)),
        ];

        for (input, operator, integer_value) in prefix_tests {
            let lexer = Lexer::new(input.to_string());
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();

            check_parser_errors(&parser);
            assert_eq!(
                program.statements.len(),
                1,
                "program does not contain 1 statement: contains {} statements",
                program.statements.len(),
            );

            let stmt = &program.statements[0];
            if !stmt.as_any().is::<ExpressionStatement>() {
                panic!("statement is not an expression statement");
            }
            let expr_stmt = stmt.as_any().downcast_ref::<ExpressionStatement>().unwrap();

            if let Some(prefix_expr) = expr_stmt
                .expression
                .as_any()
                .downcast_ref::<PrefixExpression>()
            {
                assert_eq!(
                    prefix_expr.operator, operator,
                    "expr.Operator is not '{}'. Got: '{}'",
                    operator, prefix_expr.operator
                );

                test_literal_expression(&prefix_expr.right, integer_value);
            } else {
                panic!("stmt is not a prefix expression");
            }
        }
    }

    #[test]
    fn test_parsing_infix_expressions() {
        let infix_tests = vec![
            ("5 + 5;", ExpectedValue::Int(5), "+", ExpectedValue::Int(5)),
            ("5 - 5;", ExpectedValue::Int(5), "-", ExpectedValue::Int(5)),
            ("5 * 5;", ExpectedValue::Int(5), "*", ExpectedValue::Int(5)),
            ("5 / 5;", ExpectedValue::Int(5), "/", ExpectedValue::Int(5)),
            ("5 > 5;", ExpectedValue::Int(5), ">", ExpectedValue::Int(5)),
            ("5 < 5;", ExpectedValue::Int(5), "<", ExpectedValue::Int(5)),
            (
                "5 >= 5;",
                ExpectedValue::Int(5),
                ">=",
                ExpectedValue::Int(5),
            ),
            (
                "5 <= 5;",
                ExpectedValue::Int(5),
                "<=",
                ExpectedValue::Int(5),
            ),
            (
                "5 == 5;",
                ExpectedValue::Int(5),
                "==",
                ExpectedValue::Int(5),
            ),
            (
                "5 != 5;",
                ExpectedValue::Int(5),
                "!=",
                ExpectedValue::Int(5),
            ),
            (
                "true == true",
                ExpectedValue::Bool(true),
                "==",
                ExpectedValue::Bool(true),
            ),
            (
                "true != false",
                ExpectedValue::Bool(true),
                "!=",
                ExpectedValue::Bool(false),
            ),
            (
                "false == false",
                ExpectedValue::Bool(false),
                "==",
                ExpectedValue::Bool(false),
            ),
            (
                "true && false",
                ExpectedValue::Bool(true),
                "&&",
                ExpectedValue::Bool(false),
            ),
            (
                "true || false",
                ExpectedValue::Bool(true),
                "||",
                ExpectedValue::Bool(false),
            ),
            ("10 % 3", ExpectedValue::Int(10), "%", ExpectedValue::Int(3)),
        ];

        for (input, left_value, operator, right_value) in infix_tests {
            let lexer = Lexer::new(input.to_string());
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();

            check_parser_errors(&parser);
            assert_eq!(
                program.statements.len(),
                1,
                "program does not contain 1 statement: contains {} statements",
                program.statements.len(),
            );

            let stmt = &program.statements[0];
            if !stmt.as_any().is::<ExpressionStatement>() {
                panic!("statement is not an expression statement");
            }
            let expr_stmt = stmt.as_any().downcast_ref::<ExpressionStatement>().unwrap();

            if let Some(infix_expr) = expr_stmt
                .expression
                .as_any()
                .downcast_ref::<InfixExpression>()
            {
                assert_eq!(infix_expr.operator, operator);
                test_literal_expression(&infix_expr.left, left_value);
                test_literal_expression(&infix_expr.right, right_value);
            } else {
                panic!("stmt is not an infix expression");
            }
        }
    }

    // TODO: finish after finishing postfix expressions
    // #[test]
    // fn test_parsing_postfix_expressions() {
    //     let postfix_tests = vec![("five++", "++"), ("five--", "--")];

    //     for (input, operator) in postfix_tests {
    //         let lexer = Lexer::new(input.to_string());
    //         let mut parser = Parser::new(lexer);
    //         let program = parser.parse_program();

    //         check_parser_errors(&parser);
    //         assert_eq!(
    //             program.statements.len(),
    //             1,
    //             "program does not contain 1 statement: contains {} statements",
    //             program.statements.len(),
    //         );

    //         let stmt = &program.statements[0];
    //         if !stmt.as_any().is::<ExpressionStatement>() {
    //             panic!("statement is not an expression statement");
    //         }
    //         let expr_stmt = stmt.as_any().downcast_ref::<ExpressionStatement>().unwrap();

    //         if let Some(postfix_expr) = expr_stmt
    //             .expression
    //             .as_any()
    //             .downcast_ref::<PostfixExpression>()
    //         {
    //             assert_eq!(
    //                 postfix_expr.operator, operator,
    //                 "expr.Operator is not '{}'. Got: '{}'",
    //                 operator, postfix_expr.operator
    //             );
    //         } else {
    //             panic!("stmt is not a postfix expression");
    //         }
    //     }
    // }

    #[test]
    fn test_operator_precedence_parsing() {
        let tests = vec![
            ("-a * b", "((-a) * b)"),
            ("!-a", "(!(-a))"),
            ("a + b + c", "((a + b) + c)"),
            ("a + b - c", "((a + b) - c)"),
            ("a * b * c", "((a * b) * c)"),
            ("a * b / c", "((a * b) / c)"),
            ("a + b / c", "(a + (b / c))"),
            ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
            ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
            ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
            ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
            ("true", "true"),
            ("false", "false"),
            ("3 > 5 == false", "((3 > 5) == false)"),
            ("3 < 5 == true", "((3 < 5) == true)"),
            ("3 >= 5 == false", "((3 >= 5) == false)"),
            ("3 <= 5 == true", "((3 <= 5) == true)"),
            ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
            ("(5 + 5) * 2", "((5 + 5) * 2)"),
            ("2 / (5 + 5)", "(2 / (5 + 5))"),
            ("(5 + 5) * 2 * (5 + 5)", "(((5 + 5) * 2) * (5 + 5))"),
            ("-(5 + 5)", "(-(5 + 5))"),
            ("!(true == true)", "(!(true == true))"),
            ("a + add(b * c) + d", "((a + add((b * c))) + d)"),
            (
                "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
                "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
            ),
            (
                "add(a + b + c * d / f + g)",
                "add((((a + b) + ((c * d) / f)) + g))",
            ),
            (
                "a * [1, 2, 3, 4][b * c] * d",
                "((a * ([1, 2, 3, 4][(b * c)])) * d)",
            ),
            (
                "add(a * b[2], b[1], 2 * [1, 2][1])",
                "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))",
            ),
        ];

        for (input, expected) in tests {
            let lexer = Lexer::new(input.to_string());
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();

            check_parser_errors(&parser);
            let actual = program.string();

            assert_eq!(expected, actual, "Expected: {}, got: {}", expected, actual);
        }
    }

    #[test]
    fn test_boolean_expression() {
        let tests = vec![("true", true), ("false", false)];

        for (input, expected_bool) in tests {
            let lexer = Lexer::new(input.to_string());
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();

            check_parser_errors(&parser);

            assert_eq!(
                program.statements.len(),
                1,
                "program doesn't have enough statements. Got: {}",
                program.statements.len()
            );

            let stmt = &program.statements[0];
            if !stmt.as_any().is::<ExpressionStatement>() {
                panic!("statement is not an expression statement");
            }
            let expr_stmt = stmt.as_any().downcast_ref::<ExpressionStatement>().unwrap();

            if let Some(boolean) = expr_stmt.expression.as_any().downcast_ref::<Boolean>() {
                assert_eq!(
                    boolean.value, expected_bool,
                    "boolean.Value not {}. Got: {}",
                    expected_bool, boolean.value
                );
            } else {
                panic!("statement is not a boolean");
            }
        }
    }

    #[test]
    fn test_if_expression() {
        let input = "if (x < y) { x }";
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        check_parser_errors(&parser);

        assert_eq!(
            program.statements.len(),
            1,
            "program doesn't contain 1 statement. Got: {}",
            program.statements.len()
        );

        let stmt = &program.statements[0];
        if !stmt.as_any().is::<ExpressionStatement>() {
            panic!("statement is not an expression statement");
        }
        let expr_stmt = stmt.as_any().downcast_ref::<ExpressionStatement>().unwrap();

        if !expr_stmt.expression.as_any().is::<IfExpression>() {
            panic!("statement is not an if expression");
        }
        let if_expr = expr_stmt
            .expression
            .as_any()
            .downcast_ref::<IfExpression>()
            .unwrap();

        test_infix_expression(
            &if_expr.condition,
            ExpectedValue::Ident(String::from("x")),
            "<",
            ExpectedValue::Ident(String::from("y")),
        );

        assert_eq!(
            if_expr.consequence.statements.len(),
            1,
            "Consequence is not 1 statement. Got: {}",
            if_expr.consequence.statements.len()
        );

        if !if_expr.consequence.statements[0]
            .as_any()
            .is::<ExpressionStatement>()
        {
            panic!("if expression not an expression statement");
        }

        let consequenc_statement = if_expr.consequence.statements[0]
            .as_any()
            .downcast_ref::<ExpressionStatement>()
            .unwrap();

        test_identifier(&consequenc_statement.expression, String::from("x"));

        if if_expr.alternative.as_any().is::<ZeroValueStatement>() {
            panic!("the if expression's alternative was not nil");
        }
    }

    #[test]
    fn test_if_else_expression() {
        let input = "if (x < y) { x } else { y }";
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        check_parser_errors(&parser);

        assert_eq!(
            program.statements.len(),
            1,
            "program doesn't contain 1 statement. Got: {}",
            program.statements.len()
        );

        let stmt = &program.statements[0];
        if !stmt.as_any().is::<ExpressionStatement>() {
            panic!("statement is not an expression statement");
        }
        let expr_stmt = stmt.as_any().downcast_ref::<ExpressionStatement>().unwrap();

        if !expr_stmt.expression.as_any().is::<IfExpression>() {
            panic!("statement is not an if expression");
        }
        let if_expr = expr_stmt
            .expression
            .as_any()
            .downcast_ref::<IfExpression>()
            .unwrap();

        test_infix_expression(
            &if_expr.condition,
            ExpectedValue::Ident(String::from("x")),
            "<",
            ExpectedValue::Ident(String::from("y")),
        );

        test_infix_expression(
            &if_expr.condition,
            ExpectedValue::Ident(String::from("x")),
            "<",
            ExpectedValue::Ident(String::from("y")),
        );

        assert_eq!(
            if_expr.consequence.statements.len(),
            1,
            "Consequence is not 1 statement. Got: {}",
            if_expr.consequence.statements.len()
        );

        let consequence_statement = if_expr.consequence.statements[0]
            .as_any()
            .downcast_ref::<ExpressionStatement>()
            .unwrap();

        test_identifier(&consequence_statement.expression, String::from("x"));

        assert_eq!(
            if_expr.alternative.statements.len(),
            1,
            "Alternative does not contain 1 statement. Got: {}",
            if_expr.alternative.statements.len()
        );

        let alternative_statement = if_expr.alternative.statements[0]
            .as_any()
            .downcast_ref::<ExpressionStatement>()
            .unwrap();

        test_identifier(&alternative_statement.expression, String::from("y"));
    }

    #[test]
    fn test_function_literal_parsing() {
        let input = "func(x, y) { x + y; }";
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        check_parser_errors(&parser);

        assert_eq!(
            program.statements.len(),
            1,
            "program doesn't contain 1 statement. Got: {}",
            program.statements.len()
        );

        let stmt = &program.statements[0];
        if !stmt.as_any().is::<ExpressionStatement>() {
            panic!("statement is not an expression statement");
        }
        let expr_stmt = stmt.as_any().downcast_ref::<ExpressionStatement>().unwrap();

        if !expr_stmt.expression.as_any().is::<FunctionLiteral>() {
            panic!("statement is not a function literal");
        }
        let function = expr_stmt
            .expression
            .as_any()
            .downcast_ref::<FunctionLiteral>()
            .unwrap();

        assert_eq!(
            function.parameters.len(),
            2,
            "function literal parameters wrong. Expected: 2, Got: {}",
            function.parameters.len()
        );

        let identifier_1 = function.parameters[0].value.clone();
        let identifier_2 = function.parameters[1].value.clone();

        assert_eq!(identifier_1, "x");
        assert_eq!(identifier_2, "y");

        assert_eq!(
            function.body.statements.len(),
            1,
            "function.body.statements wrong number of statements. Expected: 1, Got: {}",
            function.body.statements.len()
        );

        let body_stmt = &function.body.statements[0];
        if !body_stmt.as_any().is::<ExpressionStatement>() {
            panic!("body statement is not an expression statement");
        }
        let body = body_stmt
            .as_any()
            .downcast_ref::<ExpressionStatement>()
            .unwrap();

        test_infix_expression(
            &body.expression,
            ExpectedValue::Ident(String::from("x")),
            "+",
            ExpectedValue::Ident(String::from("y")),
        );
    }

    #[test]
    fn test_function_parameter_parsing() {
        let tests = vec![
            ("func() {};", vec![] as Vec<&str>),
            ("func(x) {};", vec!["x"]),
            ("func(x, y, z) {};", vec!["x", "y", "z"]),
        ];

        for (input, expected_params) in tests {
            let lexer = Lexer::new(input.to_string());
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();

            check_parser_errors(&parser);

            assert_eq!(
                program.statements.len(),
                1,
                "program doesn't contain 1 statement. Got: {}",
                program.statements.len()
            );

            let stmt = &program.statements[0];
            if !stmt.as_any().is::<ExpressionStatement>() {
                panic!("statement is not an expression statement");
            }
            let expr_stmt = stmt.as_any().downcast_ref::<ExpressionStatement>().unwrap();

            if !expr_stmt.expression.as_any().is::<FunctionLiteral>() {
                panic!("statement is not a function literal");
            }
            let function = expr_stmt
                .expression
                .as_any()
                .downcast_ref::<FunctionLiteral>()
                .unwrap();

            assert_eq!(
                function.parameters.len(),
                expected_params.len(),
                "length of parameters wrong. Expected: {}. Got: {}",
                expected_params.len(),
                function.parameters.len()
            );

            for (i, &expected_param) in expected_params.iter().enumerate() {
                let param = &function.parameters[i];
                assert_eq!(param.value, expected_param);
            }
        }
    }

    #[test]
    fn test_call_expression_parsing() {
        let input = "add(1, 2 * 3, 4 + 5);";
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        check_parser_errors(&parser);

        assert_eq!(
            program.statements.len(),
            1,
            "program doesn't contain 1 statement. Got: {}",
            program.statements.len()
        );

        let stmt = &program.statements[0];
        if !stmt.as_any().is::<ExpressionStatement>() {
            panic!("stmt is not an expression statement");
        }
        let expr_stmt = stmt.as_any().downcast_ref::<ExpressionStatement>().unwrap();

        if !expr_stmt.expression.as_any().is::<CallExpression>() {
            panic!("stmt.Expression is not a CallExpression");
        }
        let call_expr = expr_stmt
            .expression
            .as_any()
            .downcast_ref::<CallExpression>()
            .unwrap();

        test_identifier(&call_expr.func, String::from("add"));

        assert_eq!(
            call_expr.arguments.len(),
            3,
            "Wrong length of arguments. Got: {}",
            call_expr.arguments.len()
        );

        test_literal_expression(&call_expr.arguments[0], ExpectedValue::Int(1));
        test_infix_expression(
            &call_expr.arguments[1],
            ExpectedValue::Int(2),
            "*",
            ExpectedValue::Int(3),
        );
        test_infix_expression(
            &call_expr.arguments[2],
            ExpectedValue::Int(4),
            "+",
            ExpectedValue::Int(5),
        );
    }

    #[test]
    fn test_call_expression_parameter_parsing() {
        let tests = vec![
            ("add();", "add", vec![] as Vec<&str>),
            ("add(1);", "add", vec!["1"]),
            (
                "add(1, 2 * 3, 4 + 5);",
                "add",
                vec!["1", "(2 * 3)", "(4 + 5)"],
            ),
        ];

        for (input, expected_ident, expected_args) in tests {
            let lexer = Lexer::new(input.to_string());
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();

            check_parser_errors(&parser);

            assert_eq!(
                program.statements.len(),
                1,
                "program doesn't contain 1 statement. Got: {}",
                program.statements.len()
            );

            let stmt = &program.statements[0];
            if !stmt.as_any().is::<ExpressionStatement>() {
                panic!("stmt is not an expression statement");
            }
            let expr_stmt = stmt.as_any().downcast_ref::<ExpressionStatement>().unwrap();

            if !expr_stmt.expression.as_any().is::<CallExpression>() {
                panic!("expression statement's expression is not a call expression");
            }
            let call_expr = expr_stmt
                .expression
                .as_any()
                .downcast_ref::<CallExpression>()
                .unwrap();

            test_identifier(&call_expr.func, expected_ident.to_string());

            assert_eq!(
                call_expr.arguments.len(),
                expected_args.len(),
                "Wrong number of arguments. Expected: {}, Got: {}",
                expected_args.len(),
                call_expr.arguments.len()
            );

            for (i, expected_arg) in expected_args.iter().enumerate() {
                assert_eq!(
                    call_expr.arguments[i].string(),
                    *expected_arg,
                    "Argument {} wrong. Expected: {}, Got: {}",
                    i,
                    expected_arg,
                    call_expr.arguments[i].string()
                );
            }
        }
    }

    #[test]
    fn test_string_literal_expression() {
        let input = "\"hello world\";";
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        check_parser_errors(&parser);

        assert_eq!(
            program.statements.len(),
            1,
            "program doesn't contain 1 statement. Got: {}",
            program.statements.len()
        );

        let stmt = &program.statements[0];
        if !stmt.as_any().is::<ExpressionStatement>() {
            panic!("stmt is not an expression statement");
        }
        let expr_stmt = stmt.as_any().downcast_ref::<ExpressionStatement>().unwrap();

        if !expr_stmt.expression.as_any().is::<StringLiteral>() {
            panic!("expression statement's expression is not a string literal");
        }
        let string_literal = expr_stmt
            .expression
            .as_any()
            .downcast_ref::<StringLiteral>()
            .unwrap();

        assert_eq!(
            string_literal.value, "hello world",
            "String literal value incorrect. Expected: {}, Got: {}",
            "hello world", string_literal.value
        );
    }

    #[test]
    fn test_parsing_array_literals() {
        let input = "[1, 2 * 2, 3 + 3]";
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        check_parser_errors(&parser);

        assert_eq!(
            program.statements.len(),
            1,
            "program doesn't contain 1 statement. Got: {}",
            program.statements.len()
        );

        let stmt = &program.statements[0];
        if !stmt.as_any().is::<ExpressionStatement>() {
            panic!("stmt is not an expression statement");
        }
        let expr_stmt = stmt.as_any().downcast_ref::<ExpressionStatement>().unwrap();

        if !expr_stmt.expression.as_any().is::<ArrayLiteral>() {
            panic!("expression statement's expression is not an array literal");
        }
        let array_literal = expr_stmt
            .expression
            .as_any()
            .downcast_ref::<ArrayLiteral>()
            .unwrap();

        assert_eq!(
            array_literal.elements.len(),
            3,
            "len(array.elements) not 3. Got: {}",
            array_literal.elements.len()
        );

        test_literal_expression(&array_literal.elements[0], ExpectedValue::Int(1));
        test_infix_expression(
            &array_literal.elements[1],
            ExpectedValue::Int(2),
            "*",
            ExpectedValue::Int(2),
        );
        test_infix_expression(
            &array_literal.elements[2],
            ExpectedValue::Int(3),
            "+",
            ExpectedValue::Int(3),
        );
    }

    #[test]
    fn test_parsing_hash_literals_string_keys() {
        let input = "{\"one\": 1, \"two\": 2, \"three\": 3}".to_string();
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        check_parser_errors(&parser);

        let stmt = &program.statements[0];
        if !stmt.as_any().is::<ExpressionStatement>() {
            panic!("stmt is not an expression statement");
        }
        let expr_stmt = stmt.as_any().downcast_ref::<ExpressionStatement>().unwrap();

        if !expr_stmt.expression.as_any().is::<HashLiteral>() {
            panic!("expression statement's expression is not a hash literal");
        }
        let hash_literal = expr_stmt
            .expression
            .as_any()
            .downcast_ref::<HashLiteral>()
            .unwrap();

        let expected = vec![("one", 1), ("two", 2), ("three", 3)]
            .into_iter()
            .collect::<HashMap<&str, i64>>();

        assert_eq!(
            hash_literal.pairs.len(),
            expected.len(),
            "hash.Pairs has wrong length"
        );

        for (key, value) in &hash_literal.pairs {
            let string_literal = match key.0.as_any().downcast_ref::<StringLiteral>() {
                Some(literal) => literal,
                None => panic!("Key is not a StringLiteral"),
            };

            let expected_value = expected
                .get(string_literal.value.as_str())
                .expect("Expected value not found");

            test_integer_literal(value, *expected_value); // Assuming a function to test integer literals
        }
    }

    #[test]
    fn test_parsing_empty_hash_literal() {
        let input = "{}";
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        check_parser_errors(&parser);

        assert_eq!(
            program.statements.len(),
            1,
            "program doesn't contain 1 statement. Got: {}",
            program.statements.len()
        );

        let stmt = &program.statements[0];
        if !stmt.as_any().is::<ExpressionStatement>() {
            panic!("stmt is not an expression statement");
        }
        let expr_stmt = stmt.as_any().downcast_ref::<ExpressionStatement>().unwrap();

        if !expr_stmt.expression.as_any().is::<HashLiteral>() {
            panic!("expression statements expression is not a hash literal");
        }
        let hash_literal = expr_stmt
            .expression
            .as_any()
            .downcast_ref::<HashLiteral>()
            .unwrap();

        assert_eq!(
            hash_literal.pairs.len(),
            0,
            "hash.pairs has wrong length. Got: {}",
            hash_literal.pairs.len()
        );
    }

    #[test]
    fn test_parsing_hash_literals_with_expressions() {
        let input = "{\"one\": 0 + 1, \"two\": 10 - 8, \"three\": 15 / 5}";
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        check_parser_errors(&parser);

        assert_eq!(
            program.statements.len(),
            1,
            "program doesn't contain 1 statement. Got: {}",
            program.statements.len()
        );

        let stmt = &program.statements[0];
        if !stmt.as_any().is::<ExpressionStatement>() {
            panic!("stmt is not an expression statement");
        }
        let expr_stmt = stmt.as_any().downcast_ref::<ExpressionStatement>().unwrap();

        if !expr_stmt.expression.as_any().is::<HashLiteral>() {
            panic!("expression statements expression is not a hash literal");
        }
        let hash_literal = expr_stmt
            .expression
            .as_any()
            .downcast_ref::<HashLiteral>()
            .unwrap();

        assert_eq!(
            hash_literal.pairs.len(),
            3,
            "hash.pairs has wrong length. Got: {}",
            hash_literal.pairs.len()
        );

        let test_cases = vec![
            ("one", 0, "+", 1),
            ("two", 10, "-", 8),
            ("three", 15, "/", 5),
        ];

        for (key, left_val, operator, right_val) in test_cases {
            let value = hash_literal
                .pairs
                .get(&ExpressionKey::new(Box::new(StringLiteral {
                    token: Token {
                        token_type: TokenType::String,
                        literal: key.to_string(),
                        line: 0,
                    },
                    value: key.to_string(),
                })))
                .unwrap_or_else(|| panic!("No value found for key {}", key));

            test_infix_expression(
                value,
                ExpectedValue::Int(left_val),
                operator,
                ExpectedValue::Int(right_val),
            );
        }
    }

    #[test]
    fn test_parsing_index_expressions() {
        let input = "myArray[1 + 1]";
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        check_parser_errors(&parser);

        assert_eq!(
            program.statements.len(),
            1,
            "program doesn't contain 1 statement. Got: {}",
            program.statements.len()
        );

        let stmt = &program.statements[0];
        if !stmt.as_any().is::<ExpressionStatement>() {
            panic!("stmt is not an expression statement");
        }
        let expr_stmt = stmt.as_any().downcast_ref::<ExpressionStatement>().unwrap();

        if !expr_stmt.expression.as_any().is::<IndexExpression>() {
            panic!("expression statements expression is not an index expression");
        }
        let index_expression = expr_stmt
            .expression
            .as_any()
            .downcast_ref::<IndexExpression>()
            .unwrap();

        test_identifier(&index_expression.left, String::from("myArray"));
        test_infix_expression(
            &index_expression.index,
            ExpectedValue::Int(1),
            "+",
            ExpectedValue::Int(1),
        );
    }

    #[test]
    fn test_function_literal_with_name() {
        let input = "let myFunction = func() { };";
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        check_parser_errors(&parser);

        assert_eq!(
            program.statements.len(),
            1,
            "program doesn't contain 1 statement. Got: {}",
            program.statements.len()
        );

        let stmt = &program.statements[0];
        if !stmt.as_any().is::<LetStatement>() {
            panic!("program.Statements[0] is not a LetStatement");
        }
        let let_stmt = stmt.as_any().downcast_ref::<LetStatement>().unwrap();

        if !let_stmt.value.as_any().is::<FunctionLiteral>() {
            panic!("expression statements expression is not a function literal");
        }
        let function_literal = let_stmt
            .value
            .as_any()
            .downcast_ref::<FunctionLiteral>()
            .unwrap();

        if function_literal.name.take() != *"myFunction" {
            panic!(
                "function literal name wrong. Expected 'myFunction'. Got: '{}'",
                function_literal.name.take(),
            );
        }
    }
}
