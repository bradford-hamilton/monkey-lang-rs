use crate::token::Token;
use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::rc::Rc;

pub enum Node {
    Root(RootNode),
    Statement(Statement),
    Expression(Expression),
}

#[derive(Eq, Hash, PartialEq)]
pub enum Statement {
    Let(LetStatement),
    Const(ConstStatement),
    Return(ReturnStatement),
    Expression(ExpressionStatement),
    Block(BlockStatement),
    ZeroValue(ZeroValueStatement),
}

#[derive(Eq, Hash, PartialEq)]
pub enum Expression {
    Identifier(Identifier),
    Integer(IntegerLiteral),
    Boolean(Boolean),
    String(StringLiteral),
    Prefix(PrefixExpression),
    Infix(InfixExpression),
    Index(IndexExpression),
    If(IfExpression),
    Function(FunctionLiteral),
    Hash(HashLiteral),
    Array(ArrayLiteral),
    Call(CallExpression),
    ZeroValue(ZeroValueExpression),
}

impl RootNode {
    pub fn token_literal(&self) -> String {
        if let Some(first_statement) = self.statements.first() {
            first_statement.token_literal()
        } else {
            "".to_string()
        }
    }

    pub fn string(&self) -> String {
        let mut out = String::new();
        for statement in &self.statements {
            out.push_str(&statement.string());
        }
        out
    }
}

impl Node {
    pub fn token_literal(&self) -> String {
        match self {
            Node::Root(node) => node.token_literal(),
            Node::Statement(stmt) => stmt.token_literal(),
            Node::Expression(expr) => expr.token_literal(),
        }
    }

    pub fn string(&self) -> String {
        match self {
            Node::Root(node) => node.string(),
            Node::Statement(stmt) => stmt.string(),
            Node::Expression(expr) => expr.string(),
        }
    }
}

impl Statement {
    pub fn token_literal(&self) -> String {
        match self {
            Statement::Let(stmt) => stmt.token.literal,
            Statement::Const(stmt) => stmt.token.literal,
            Statement::Return(stmt) => stmt.token.literal,
            Statement::Expression(stmt) => stmt.token.literal,
            Statement::Block(stmt) => stmt.token.literal,
            Statement::ZeroValue(_) => "zerovaluestatement".to_string(),
        }
    }

    pub fn string(&self) -> String {
        match self {
            Statement::Let(stmt) => format!(
                "{} {} = {};",
                stmt.token.literal,
                stmt.name.value,
                stmt.value.string()
            ),
            Statement::Const(stmt) => format!(
                "{} {} = {};",
                stmt.token.literal,
                stmt.name.value,
                stmt.value.string()
            ),
            Statement::Return(stmt) => {
                format!("{} {};", stmt.token.literal, stmt.return_value.string())
            }
            Statement::Expression(stmt) => stmt.expression.string(),
            Statement::Block(stmt) => {
                let stmt_strings: Vec<_> = stmt.statements.iter().map(|s| s.string()).collect();
                format!("{{ {} }}", stmt_strings.join(" "))
            }
            Statement::ZeroValue(_) => "zerovaluestatement".to_string(),
        }
    }
}

impl Expression {
    pub fn token_literal(&self) -> String {
        match self {
            Expression::Identifier(expr) => expr.token.literal,
            Expression::Integer(expr) => expr.token.literal,
            Expression::Boolean(expr) => expr.token.literal,
            Expression::String(expr) => expr.token.literal,
            Expression::Prefix(expr) => expr.token.literal,
            Expression::Infix(expr) => expr.token.literal,
            Expression::Index(expr) => expr.token.literal,
            Expression::If(expr) => expr.token.literal,
            Expression::Function(expr) => expr.token.literal,
            Expression::Hash(expr) => expr.token.literal,
            Expression::Array(expr) => expr.token.literal,
            Expression::Call(expr) => expr.token.literal,
            Expression::ZeroValue(_) => "zerovalueexpression".to_string(),
        }
    }

    pub fn string(&self) -> String {
        match self {
            Expression::Identifier(expr) => expr.value,
            Expression::Integer(expr) => expr.value.to_string(),
            Expression::Boolean(expr) => expr.value.to_string(),
            Expression::String(expr) => expr.token.literal,
            Expression::Prefix(expr) => format!("({}{})", expr.operator, expr.right.string()),
            Expression::Infix(expr) => format!(
                "({} {} {})",
                expr.left.string(),
                expr.operator,
                expr.right.string()
            ),
            Expression::Index(expr) => format!("({}[{}])", expr.left.string(), expr.index.string()),
            Expression::If(expr) => {
                let mut result = format!(
                    "if {} {}",
                    expr.condition.string(),
                    expr.consequence.string()
                );
                if !expr.alternative.statements.is_empty() {
                    result.push_str(&format!(" else {}", expr.alternative.string()));
                }
                result
            }
            Expression::Function(expr) => {
                let params: Vec<_> = expr.parameters.iter().map(|p| p.value).collect();
                format!("fn({}) {}", params.join(", "), expr.body.string())
            }
            Expression::Hash(expr) => {
                let pairs: Vec<String> = expr
                    .pairs
                    .iter()
                    .map(|(key, value)| format!("{}:{}", key.string(), value.string()))
                    .collect();

                format!("{{{}}}", pairs.join(", "))
            }
            Expression::Array(expr) => {
                let elements: Vec<String> = expr.elements.iter().map(|el| el.string()).collect();

                format!("[{}]", elements.join(", "))
            }
            Expression::Call(expr) => {
                let args: Vec<_> = expr.arguments.iter().map(|a| a.string()).collect();
                format!("{}({})", expr.function.string(), args.join(", "))
            }
            Expression::ZeroValue(_) => "zerovalueexpression".to_string(),
        }
    }
}

pub struct RootNode {
    pub statements: Vec<Statement>,
}

#[derive(Eq, Hash, PartialEq)]
pub struct LetStatement {
    pub token: Token,
    pub name: Identifier,
    pub value: Expression,
}

#[derive(Eq, Hash, PartialEq)]
pub struct ConstStatement {
    pub token: Token,
    pub name: Identifier,
    pub value: Expression,
}

#[derive(Eq, Hash, PartialEq)]
pub struct ReturnStatement {
    pub token: Token,
    pub return_value: Expression,
}

#[derive(Eq, Hash, PartialEq)]
pub struct ExpressionStatement {
    pub token: Token,
    pub expression: Expression,
}

#[derive(Eq, Hash, PartialEq)]
pub struct BlockStatement {
    pub token: Token,
    pub statements: Vec<Statement>,
}

impl BlockStatement {
    pub fn token_literal(&self) -> String {
        self.token.literal
    }

    pub fn string(&self) -> String {
        let mut out = String::new();
        for statement in &self.statements {
            out.push_str(&statement.string());
        }
        out
    }
}

#[derive(Eq, Hash, PartialEq, Debug)]
pub struct Identifier {
    pub token: Token,
    pub value: String,
}

impl Identifier {
    pub fn string(self) -> String {
        self.value
    }
}

#[derive(Eq, Hash, PartialEq)]
pub struct IntegerLiteral {
    pub token: Token,
    pub value: i64,
}

#[derive(Eq, Hash, PartialEq)]
pub struct Boolean {
    pub token: Token,
    pub value: bool,
}

#[derive(Eq, Hash, PartialEq)]
pub struct PrefixExpression {
    pub token: Token,
    pub operator: String,
    pub right: Rc<Expression>,
}

#[derive(Eq, Hash, PartialEq)]
pub struct InfixExpression {
    pub token: Token,
    pub left: Rc<Expression>,
    pub operator: String,
    pub right: Rc<Expression>,
}

#[derive(Eq, Hash, PartialEq)]
pub struct IfExpression {
    pub token: Token,
    pub condition: Rc<Expression>,
    pub consequence: BlockStatement,
    pub alternative: BlockStatement,
}

#[derive(Eq, Hash, PartialEq)]
pub struct FunctionLiteral {
    pub token: Token,
    pub parameters: Vec<Identifier>,
    pub body: BlockStatement,
    pub name: String,
}

#[derive(Eq, Hash, PartialEq)]
pub struct CallExpression {
    pub token: Token,
    pub function: Rc<Expression>,
    pub arguments: Vec<Expression>,
}

#[derive(Eq, Hash, PartialEq)]
pub struct StringLiteral {
    pub token: Token,
    pub value: String,
}

#[derive(Eq, Hash, PartialEq)]
pub struct ArrayLiteral {
    pub token: Token,
    pub elements: Vec<Expression>,
}

#[derive(Eq, PartialEq)]
pub struct HashLiteral {
    pub token: Token,
    pub pairs: HashMap<Expression, Expression>,
}

impl Hash for HashLiteral {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.token.hash(state);

        // Create a Vec of keys and sort it using the custom sort key
        let mut pairs: Vec<_> = self.pairs.iter().collect();
        pairs.sort_by(|a, b| Self::sort_key(a.0).cmp(&Self::sort_key(b.0)));

        // Hash each sorted pair
        for (key, value) in pairs {
            key.hash(state);
            value.hash(state);
        }
    }
}

impl HashLiteral {
    fn sort_key(expr: &Expression) -> String {
        expr.string()
    }
}

#[derive(Eq, Hash, PartialEq)]
pub struct IndexExpression {
    pub token: Token,
    pub left: Rc<Expression>,
    pub index: Rc<Expression>,
}

#[derive(Eq, Hash, PartialEq)]
pub struct ZeroValueStatement {}

#[derive(Eq, Hash, PartialEq)]
pub struct ZeroValueExpression {}
