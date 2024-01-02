use crate::token::Token;
use std::any::Any;
use std::cell::RefCell;
use std::collections::HashMap;
use std::hash::{Hash, Hasher};

/// Node - nodes in our ast will provide a token_literal and string methods for debugging.
pub trait Node {
    fn token_literal(&self) -> String;
    fn string(&self) -> String;
    fn as_any(&self) -> &dyn Any;
}

/// Statement - must provide statement_node, token_literal, and string methods. Statements do not produce values.
pub trait Statement: Node {
    fn as_node(&self) -> &dyn Node;
}

/// Expression - must provide expression_node, token_literal, and string methods. Expressions produce values.
pub trait Expression: Node {
    fn as_node(&self) -> &dyn Node;
}

/// RootNode of every AST our parser produces.
pub struct RootNode {
    pub statements: Vec<Box<dyn Statement>>,
}

impl Node for RootNode {
    /// token_literal returns the RootNode's literal and satisfies the Node trait.
    fn token_literal(&self) -> String {
        if !self.statements.is_empty() {
            return self.statements[0].token_literal();
        }
        String::from("")
    }

    /// string returns a buffer containing the programs Statements as strings.
    fn string(&self) -> String {
        let mut statements_string = String::from("");
        for s in &self.statements {
            statements_string += &s.string();
        }
        statements_string
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

/// ZeroValueExpression - is used for initializations.
pub struct ZeroValueExpression {}
impl Node for ZeroValueExpression {
    fn token_literal(&self) -> String {
        "zero value expression".to_string()
    }
    fn string(&self) -> String {
        "zero value expression".to_string()
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Expression for ZeroValueExpression {
    fn as_node(&self) -> &dyn Node {
        self
    }
}

/// ZeroValueStatement is used for initializations.
pub struct ZeroValueStatement {}
impl Node for ZeroValueStatement {
    fn token_literal(&self) -> String {
        "zero value statement".to_string()
    }
    fn string(&self) -> String {
        "zero value statement".to_string()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Statement for ZeroValueStatement {
    fn as_node(&self) -> &dyn Node {
        self
    }
}

/// Identifier - holds IDENTIFIER token and it's value (add, foobar, x, y, ...).
pub struct Identifier {
    pub token: Token,
    pub value: String,
}

impl Node for Identifier {
    /// token_literal returns the Identifier's literal and satisfies the Node trait.
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    /// string - returns a string representation of the Identifier and satisfies our Node trait.
    fn string(&self) -> String {
        self.value.clone()
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Expression for Identifier {
    fn as_node(&self) -> &dyn Node {
        self
    }
}

/// IntegerLiteral - holds the token and it's value (int64)
pub struct IntegerLiteral {
    pub token: Token,
    pub value: usize,
}

impl Node for IntegerLiteral {
    /// token_literal returns the IntegerLiteral's literal and satisfies the Node trait.
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    /// string - returns a string representation of the IntegerLiteral and satisfies our Node trait
    fn string(&self) -> String {
        self.token.literal.clone()
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Expression for IntegerLiteral {
    fn as_node(&self) -> &dyn Node {
        self
    }
}

/// PrefixExpression - holds the token, a string version of the operator, and the expression to the right of it
pub struct PrefixExpression {
    /// The prefix token (! or -)
    pub token: Token,
    /// String (either "!" or "-")
    pub operator: String,
    /// The expression to the right of the operator
    pub right: Box<dyn Expression>,
}

impl Node for PrefixExpression {
    /// token_literal returns the PrefixExpression's literal and satisfies the Node trait.
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    /// String - returns a string representation of the operator followed by it's expression
    /// to the right (-5) and satisfies our Node trait
    fn string(&self) -> String {
        let mut buf = String::from("(");
        buf += &self.operator[..];
        buf += &self.right.string();
        buf += ")";
        buf
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Expression for PrefixExpression {
    fn as_node(&self) -> &dyn Node {
        self
    }
}

/// Boolean - holds the token and it's value (a boolean).
pub struct Boolean {
    /// The prefix token (! or -)
    pub token: Token,
    /// String (either "!" or "-")
    pub value: bool,
}

impl Node for Boolean {
    /// token_literal returns the Boolean's literal and satisfies the Node trait.
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    /// string - returns a string representation of the Boolean and satisfies our Node trait
    fn string(&self) -> String {
        self.token.literal.clone()
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Expression for Boolean {
    fn as_node(&self) -> &dyn Node {
        self
    }
}

/// IfExpression - holds the token, the condition expression and the consequence & alternative
/// block statements. Structure: if (<condition>) <consequence> else <alternative>
pub struct IfExpression {
    pub token: Token, // The If token
    pub condition: Box<dyn Expression>,
    pub consequence: BlockStatement,
    pub alternative: BlockStatement,
}

impl Node for IfExpression {
    /// token_literal returns the IfExpression's literal and satisfies the Node trait.
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    /// string - returns a string representation of the IfExpression and satisfies our Node trait
    fn string(&self) -> String {
        let mut result = format!(
            "if {} {}",
            self.condition.string(),
            self.consequence.string(),
        );

        if !self.alternative.as_any().is::<ZeroValueStatement>() {
            result = format!("{} else {}", result, self.alternative.string());
        }

        result
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Expression for IfExpression {
    fn as_node(&self) -> &dyn Node {
        self
    }
}

/// BlockStatement - holds the token "{", and a slice of statements
pub struct BlockStatement {
    pub token: Token,
    pub statements: Vec<Box<dyn Statement>>,
}

impl Node for BlockStatement {
    /// token_literal returns the BlockStatement's literal and satisfies the Node trait.
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    /// string - returns a string representation of the BlockStatement and satisfies our Node trait
    fn string(&self) -> String {
        self.statements
            .iter()
            .map(|stmt| stmt.string())
            .collect::<Vec<String>>()
            .join("")
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Expression for BlockStatement {
    fn as_node(&self) -> &dyn Node {
        self
    }
}

/// LetStatement - Name holds the identifier of the binding and Value for the expression that produces the value.
pub struct LetStatement {
    pub token: Token,
    pub name: Identifier,
    pub value: Box<dyn Expression>,
}

impl Node for LetStatement {
    /// token_literal returns the LetStatement's literal and satisfies the Node trait.
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    /// string - returns a string representation of the LetStatement and satisfies our Node trait
    fn string(&self) -> String {
        format!(
            "{} {} = {};",
            self.token.literal,
            self.name.value,
            self.value.string(),
        )
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Statement for LetStatement {
    fn as_node(&self) -> &dyn Node {
        self
    }
}

/// ConstStatement - Name holds the identifier of the binding and value for the expression that produces the value.
pub struct ConstStatement {
    pub token: Token,
    pub name: Identifier,
    pub value: Box<dyn Expression>,
}

impl Node for ConstStatement {
    /// token_literal returns the ConstStatement's literal and satisfies the Node trait.
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    /// string - returns a string representation of the ConstStatement and satisfies our Node trait
    fn string(&self) -> String {
        format!(
            "{} {} = {};",
            self.token.literal,
            self.name.value,
            self.value.string(),
        )
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Statement for ConstStatement {
    fn as_node(&self) -> &dyn Node {
        self
    }
}

/// ReturnStatement - pretty self explanatory, holds RETURN token and return value
pub struct ReturnStatement {
    pub token: Token,
    /// The 'return' token
    pub return_value: Box<dyn Expression>,
}

impl Node for ReturnStatement {
    /// token_literal returns the ReturnStatement's literal and satisfies the Node trait.
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    /// string - returns a string representation of the ReturnStatement and satisfies our Node trait
    fn string(&self) -> String {
        format!("{} {};", self.token.literal, self.return_value.string())
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Statement for ReturnStatement {
    fn as_node(&self) -> &dyn Node {
        self
    }
}

/// ExpressionStatement - holds the first token of the expression and the expression
pub struct ExpressionStatement {
    pub token: Token,
    /// The first token of the expression
    pub expression: Box<dyn Expression>,
}

impl Node for ExpressionStatement {
    /// token_literal returns the ExpressionStatement's literal and satisfies the Node trait.
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    /// string - returns a string representation of the ExpressionStatement and satisfies our Node trait
    fn string(&self) -> String {
        if self.expression.as_any().is::<ZeroValueExpression>() {
            return String::from("");
        }
        self.expression.string()
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Statement for ExpressionStatement {
    fn as_node(&self) -> &dyn Node {
        self
    }
}

/// FunctionLiteral - holds the token, the function params (a vec of Identifier), and
/// the function Body (BlockStatement). Structure: func <parameters> <block statement>
pub struct FunctionLiteral {
    pub token: Token, // The 'func' token
    pub parameters: Vec<Identifier>,
    pub body: BlockStatement,
    pub name: RefCell<String>,
}

impl Node for FunctionLiteral {
    /// token_literal returns the FunctionLiteral's literal and satisfies the Node trait.
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    /// string - returns a string representation of the FunctionLiteral and satisfies our Node trait
    fn string(&self) -> String {
        let params = self
            .parameters
            .iter()
            .map(|p| p.string())
            .collect::<Vec<String>>()
            .join(", ");

        let mut result = self.token.literal.clone();
        if !self.name.borrow().is_empty() {
            result.push_str(&format!("<{}>", self.name.borrow()));
        }
        result.push_str(&format!("({}) {}", params, self.body.string()));

        result
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Expression for FunctionLiteral {
    fn as_node(&self) -> &dyn Node {
        self
    }
}

/// StringLiteral holds the token and it's value (string)
pub struct StringLiteral {
    pub token: Token,
    pub value: String,
}

impl Node for StringLiteral {
    /// token_literal returns the StringLiteral's literal and satisfies the Node trait.
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    /// string - returns a string representation of the StringLiteral and satisfies our Node trait
    fn string(&self) -> String {
        self.token.literal.clone()
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Expression for StringLiteral {
    fn as_node(&self) -> &dyn Node {
        self
    }
}

/// ArrayLiteral holds the token: '[' and an array of expressions (Elements)
pub struct ArrayLiteral {
    pub token: Token,
    /// the '[' token
    pub elements: Vec<Box<dyn Expression>>,
}

impl Node for ArrayLiteral {
    /// token_literal returns the ArrayLiteral's literal and satisfies the Node trait.
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    /// string - returns a string representation of the ArrayLiteral and satisfies our Node trait
    fn string(&self) -> String {
        let elements_str = self
            .elements
            .iter()
            .map(|element| element.string())
            .collect::<Vec<String>>()
            .join(", ");

        format!("[{}]", elements_str)
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Expression for ArrayLiteral {
    fn as_node(&self) -> &dyn Node {
        self
    }
}

/// HashLiteral holds the '{' token and the pairs in the hash
pub struct HashLiteral {
    pub token: Token,
    /// The '{' token
    pub pairs: HashMap<ExpressionKey, Box<dyn Expression>>,
}

pub struct ExpressionKey(pub Box<dyn Expression>);

impl ExpressionKey {
    pub fn new(expr: Box<dyn Expression>) -> Self {
        ExpressionKey(expr)
    }
}

impl PartialEq for ExpressionKey {
    fn eq(&self, other: &Self) -> bool {
        self.0.string() == other.0.string()
    }
}

impl Eq for ExpressionKey {}

impl Hash for ExpressionKey {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.string().hash(state);
    }
}

impl Node for HashLiteral {
    /// token_literal returns the HashLiteral's literal and satisfies the Node trait.
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    /// string - returns a string representation of the HashLiteral and satisfies our Node trait
    fn string(&self) -> String {
        let pairs = self
            .pairs
            .iter()
            .map(|(key, value)| {
                let key_expr = &key.0;
                format!("{}: {}", key_expr.string(), value.string())
            })
            .collect::<Vec<String>>()
            .join(", ");

        format!("{{{}}}", pairs)
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Expression for HashLiteral {
    fn as_node(&self) -> &dyn Node {
        self
    }
}

/// InfixExpression holds the token, the expression to the left of it, a string version of
/// the operator, and the expression to the right of it
pub struct InfixExpression {
    pub token: Token,
    /// The operator token (+, -, *, etc)
    pub left: Box<dyn Expression>,
    pub operator: String,
    pub right: Box<dyn Expression>,
}

impl Node for InfixExpression {
    /// token_literal returns the InfixExpression's literal and satisfies the Node trait.
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    /// string - returns a string representation of the InfixExpression and satisfies our Node trait
    fn string(&self) -> String {
        format!(
            "({} {} {})",
            self.left.string(),
            self.operator,
            self.right.string(),
        )
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Expression for InfixExpression {
    fn as_node(&self) -> &dyn Node {
        self
    }
}

// TODO: add the newer additional types and impls from PR's since the last go at this.

/// CallExpression holds the token, func, and its arguments.
pub struct CallExpression {
    pub token: Token,
    pub func: Box<dyn Expression>,
    pub arguments: Vec<Box<dyn Expression>>,
}

impl Node for CallExpression {
    /// token_literal returns the CallExpression's literal and satisfies the Node trait.
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    /// string - returns a string representation of the CallExpression and satisfies our Node trait
    fn string(&self) -> String {
        let args: Vec<String> = self.arguments.iter().map(|arg| arg.string()).collect();
        format!("{}({})", self.func.string(), args.join(", "))
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Expression for CallExpression {
    fn as_node(&self) -> &dyn Node {
        self
    }
}

pub struct IndexExpression {
    pub token: Token,
    pub left: Box<dyn Expression>,
    pub index: Box<dyn Expression>,
}

impl Node for IndexExpression {
    /// token_literal returns the IndexExpression's literal and satisfies the Node trait.
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    /// string - returns a string representation of the IndexExpression and satisfies our Node trait
    fn string(&self) -> String {
        format!("({}[{}])", self.left.string(), self.index.string())
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Expression for IndexExpression {
    fn as_node(&self) -> &dyn Node {
        self
    }
}
