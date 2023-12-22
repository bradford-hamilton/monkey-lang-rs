use crate::ast::{
    ArrayLiteral, BlockStatement, Boolean, ConstStatement, Expression, ExpressionStatement,
    FunctionLiteral, HashLiteral, Identifier, IfExpression, InfixExpression, IntegerLiteral,
    LetStatement, PrefixExpression, ReturnStatement, Statement, StringLiteral, ZeroValueExpression,
    ZeroValueStatement,
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
        let mut list: Vec<Box<dyn Expression>> = vec![Box::new(ZeroValueExpression {})];

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

        if self.expect_peek_type(end) {
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

            // if stmt == ZeroValueExpression

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

fn parse_call_expr(_parser: &mut Parser, _left: Box<dyn Expression>) -> Box<dyn Expression> {
    todo!();
}
fn parse_index_expr(_parser: &mut Parser, _left: Box<dyn Expression>) -> Box<dyn Expression> {
    todo!();
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

    // TODO: Handle function literal piece here
    // if fl, ok := stmt.Value.(*ast.FunctionLiteral); ok {
    // 	fl.Name = stmt.Name.Value
    // }

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

    // TODO: Handle function literal piece here
    // if fl, ok := stmt.Value.(*ast.FunctionLiteral); ok {
    // 	fl.Name = stmt.Name.Value
    // }

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
        name: String::from(""),
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
    let mut array = ArrayLiteral {
        token: parser.current_token.clone(),
        elements: vec![],
    };

    array.elements = parser.parse_expression_list(TokenType::RightBracket);

    Box::new(array)
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

        hash.pairs.insert(key.string(), value);

        if !parser.peek_token_type_is(TokenType::RightBrace)
            && !parser.expect_peek_type(TokenType::Comma)
        {
            return Box::new(ZeroValueExpression {});
        }
    }

    if !parser.peek_token_type_is(TokenType::RightBrace)
        && !parser.expect_peek_type(TokenType::Comma)
    {
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
