use crate::token::Token;
use std::collections::HashMap;

pub enum Node {
    Root(RootNode),
    Statement(Statement),
    Expression(Expression),
}

pub enum Statement {
    Let(LetStatement),
    Const(ConstStatement),
    Return(ReturnStatement),
    Expression(ExpressionStatement),
    Block(BlockStatement),
    ZeroValue(ZeroValueStatement),
}

pub enum Expression {
    Identifier(Identifier),
    IntegerLiteral(IntegerLiteral),
    Boolean(Boolean),
    Prefix(PrefixExpression),
    Infix(InfixExpression),
    If(IfExpression),
    Function(FunctionLiteral),
    Call(CallExpression),
    ZeroValue(ZeroValueExpression),
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
            Statement::Let(stmt) => stmt.token.literal.clone(),
            Statement::Const(stmt) => stmt.token.literal.clone(),
            Statement::Return(stmt) => stmt.token.literal.clone(),
            Statement::Expression(stmt) => stmt.token.literal.clone(),
            Statement::Block(stmt) => stmt.token.literal.clone(),
            Statement::ZeroValue(_) => "zerovaluestatement".to_string(),
        }
    }

    pub fn string(&self) -> String {
        match self {
            Statement::Let(stmt) => format!(
                "{} {} = {};",
                stmt.token.literal,
                stmt.name.string(),
                stmt.value.string()
            ),
            Statement::Const(stmt) => format!(
                "{} {} = {};",
                stmt.token.literal,
                stmt.name.string(),
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
            Expression::Identifier(expr) => expr.token.literal.clone(),
            Expression::IntegerLiteral(expr) => expr.token.literal.clone(),
            Expression::Boolean(expr) => expr.token.literal.clone(),
            Expression::Prefix(expr) => expr.token.literal.clone(),
            Expression::Infix(expr) => expr.token.literal.clone(),
            Expression::If(expr) => expr.token.literal.clone(),
            Expression::Function(expr) => expr.token.literal.clone(),
            Expression::Call(expr) => expr.token.literal.clone(),
            Expression::ZeroValue(_) => "zerovalueexpression".to_string(),
        }
    }

    pub fn string(&self) -> String {
        match self {
            Expression::Identifier(expr) => expr.value.clone(),
            Expression::IntegerLiteral(expr) => expr.value.to_string(),
            Expression::Boolean(expr) => expr.value.to_string(),
            Expression::Prefix(expr) => format!("({}{})", expr.operator, expr.right.string()),
            Expression::Infix(expr) => format!(
                "({} {} {})",
                expr.left.string(),
                expr.operator,
                expr.right.string()
            ),
            Expression::If(expr) => {
                let mut result = format!(
                    "if {} {}",
                    expr.condition.string(),
                    expr.consequence.string()
                );
                if let Some(alternative) = &expr.alternative {
                    result.push_str(&format!(" else {}", alternative.string()));
                }
                result
            }
            Expression::Function(expr) => {
                let params: Vec<_> = expr.parameters.iter().map(|p| p.string()).collect();
                format!("fn({}) {}", params.join(", "), expr.body.string())
            }
            Expression::Call(expr) => {
                let args: Vec<_> = expr.arguments.iter().map(|a| a.string()).collect();
                format!("{}({})", expr.func.string(), args.join(", "))
            }
            Expression::ZeroValue(_) => "zerovalueexpression".to_string(),
        }
    }
}

pub struct RootNode {
    pub statements: Vec<Statement>,
}

// impl RootNode {
//     pub fn token_literal(&self) -> String {
//         if !self.statements.is_empty() {
//             return self.statements[0].token_literal();
//         }
//         String::new()
//     }

//     pub fn string(&self) -> String {
//         self.statements
//             .iter()
//             .map(|s| s.string())
//             .collect::<Vec<String>>()
//             .join("")
//     }
// }

pub struct LetStatement {
    pub token: Token,
    pub name: Identifier,
    pub value: Expression,
}

// impl LetStatement {
//     pub fn token_literal(&self) -> String {
//         self.token.literal.clone()
//     }

//     pub fn string(&self) -> String {
//         format!(
//             "{} {} = {};",
//             self.token.literal,
//             self.name.string(),
//             self.value.string()
//         )
//     }
// }

pub struct ConstStatement {
    pub token: Token,
    pub name: Identifier,
    pub value: Expression,
}

// impl ConstStatement {
//     pub fn token_literal(&self) -> String {
//         self.token.literal.clone()
//     }

//     pub fn string(&self) -> String {
//         format!(
//             "{} {} = {};",
//             self.token_literal(),
//             self.name.string(),
//             self.value.string()
//         )
//     }
// }

pub struct ReturnStatement {
    pub token: Token,
    pub return_value: Expression,
}

// impl ReturnStatement {
//     pub fn token_literal(&self) -> String {
//         self.token.literal.clone()
//     }

//     pub fn string(&self) -> String {
//         format!("{} {};", self.token.literal, self.return_value.string())
//     }
// }

pub struct ExpressionStatement {
    pub token: Token,
    pub expression: Expression,
}

// impl ExpressionStatement {
//     pub fn token_literal(&self) -> String {
//         self.token.literal.clone()
//     }

//     pub fn string(&self) -> String {
//         self.expression.string()
//     }
// }

pub struct BlockStatement {
    pub token: Token,
    pub statements: Vec<Statement>,
}

// impl BlockStatement {
//     pub fn token_literal(&self) -> String {
//         self.token.literal.clone()
//     }

//     pub fn string(&self) -> String {
//         self.statements
//             .iter()
//             .map(|s| s.string())
//             .collect::<Vec<String>>()
//             .join("")
//     }
// }

pub struct Identifier {
    pub token: Token,
    pub value: String,
}

// impl Identifier {
//     pub fn token_literal(&self) -> String {
//         self.token.literal.clone()
//     }

//     pub fn string(&self) -> String {
//         self.value.clone()
//     }
// }

pub struct IntegerLiteral {
    pub token: Token,
    pub value: i64,
}

// impl IntegerLiteral {
//     pub fn token_literal(&self) -> String {
//         self.token.literal.clone()
//     }

//     pub fn string(&self) -> String {
//         self.value.to_string()
//     }
// }

pub struct Boolean {
    pub token: Token,
    pub value: bool,
}

// impl Boolean {
//     pub fn token_literal(&self) -> String {
//         self.token.literal.clone()
//     }

//     pub fn string(&self) -> String {
//         self.value.to_string()
//     }
// }

pub struct PrefixExpression {
    pub token: Token,
    pub operator: String,
    pub right: Expression,
}

// impl PrefixExpression {
//     pub fn token_literal(&self) -> String {
//         self.token.literal.clone()
//     }

//     pub fn string(&self) -> String {
//         format!("({}{})", self.operator, self.right.string())
//     }
// }

pub struct InfixExpression {
    pub token: Token,
    pub left: Expression,
    pub operator: String,
    pub right: Expression,
}

// impl InfixExpression {
//     pub fn token_literal(&self) -> String {
//         self.token.literal.clone()
//     }

//     pub fn string(&self) -> String {
//         format!(
//             "({} {} {})",
//             self.left.string(),
//             self.operator,
//             self.right.string()
//         )
//     }
// }

pub struct IfExpression {
    pub token: Token,
    pub condition: Expression,
    pub consequence: BlockStatement,
    pub alternative: BlockStatement,
}

// impl IfExpression {
//     pub fn token_literal(&self) -> String {
//         self.token.literal.clone()
//     }

//     pub fn string(&self) -> String {
//         let mut result = format!(
//             "if {} {}",
//             self.condition.string(),
//             self.consequence.string()
//         );

//         if let Some(alternative) = &self.alternative {
//             result = format!("{} else {}", result, alternative.string());
//         }

//         result
//     }
// }

pub struct FunctionLiteral {
    pub token: Token,
    pub parameters: Vec<Identifier>,
    pub body: BlockStatement,
    pub name: String,
}

// impl FunctionLiteral {
//     pub fn token_literal(&self) -> String {
//         self.token.literal.clone()
//     }

//     pub fn string(&self) -> String {
//         let params = self
//             .parameters
//             .iter()
//             .map(|p| p.string())
//             .collect::<Vec<String>>()
//             .join(", ");
//         format!("{}({}) {}", self.token.literal, params, self.body.string())
//     }
// }

pub struct CallExpression {
    pub token: Token,
    pub function: Expression,
    pub arguments: Vec<Expression>,
}

// impl CallExpression {
//     pub fn token_literal(&self) -> String {
//         self.token.literal.clone()
//     }

//     pub fn string(&self) -> String {
//         let args = self
//             .arguments
//             .iter()
//             .map(|arg| arg.string())
//             .collect::<Vec<String>>()
//             .join(", ");
//         format!("{}({})", self.function.string(), args)
//     }
// }

pub struct StringLiteral {
    pub token: Token,
    pub value: String,
}

// impl StringLiteral {
//     pub fn token_literal(&self) -> String {
//         self.token.literal.clone()
//     }

//     pub fn string(&self) -> String {
//         self.value.clone()
//     }
// }

pub struct ArrayLiteral {
    pub token: Token,
    pub elements: Vec<Expression>,
}

// impl ArrayLiteral {
//     pub fn token_literal(&self) -> String {
//         self.token.literal.clone()
//     }

//     pub fn string(&self) -> String {
//         let elements = self
//             .elements
//             .iter()
//             .map(|e| e.string())
//             .collect::<Vec<String>>()
//             .join(", ");
//         format!("[{}]", elements)
//     }
// }

pub struct HashLiteral {
    pub token: Token,
    pub pairs: HashMap<Expression, Expression>,
}

// impl HashLiteral {
//     pub fn token_literal(&self) -> String {
//         self.token.literal.clone()
//     }

//     pub fn string(&self) -> String {
//         let pairs = self
//             .pairs
//             .iter()
//             .map(|(key, value)| format!("{}: {}", key.string(), value.string()))
//             .collect::<Vec<String>>()
//             .join(", ");
//         format!("{{{}}}", pairs)
//     }
// }

pub struct IndexExpression {
    pub token: Token,
    pub left: Expression,
    pub index: Expression,
}

// impl IndexExpression {
//     pub fn token_literal(&self) -> String {
//         self.token.literal.clone()
//     }

//     pub fn string(&self) -> String {
//         format!("({}[{}])", self.left.string(), self.index.string())
//     }
// }

pub struct ZeroValueStatement {}

// impl ZeroValueStatement {
//     pub fn token_literal(&self) -> String {
//         "zerovaluestatement".to_string()
//     }

//     pub fn string(&self) -> String {
//         "zerovaluestatement".to_string()
//     }
// }

pub struct ZeroValueExpression {}

// impl ZeroValueExpression {
//     pub fn token_literal(&self) -> String {
//         "zerovalueexpression".to_string()
//     }

//     pub fn string(&self) -> String {
//         "zerovalueexpression".to_string()
//     }
// }
