use crate::token::Token;
use std::any::Any;
use std::cell::RefCell;
use std::collections::HashMap;

/// Node - nodes in our ast will provide a token_literal and string methods for debugging.
trait Node {
    fn token_literal(&self) -> String;
    fn string(&self) -> String;
}

/// Statement - must provide statement_node, token_literal, and string methods. Statements do not produce values.
pub trait Statement {
    fn token_literal(&self) -> String;
    fn string(&self) -> String;
    fn statement_node(&self);
    fn as_any(&self) -> &dyn Any;
}

/// Expression - must provide expression_node, token_literal, and string methods. Expressions produce values.
pub trait Expression {
    fn token_literal(&self) -> String;
    fn string(&self) -> String;
    fn expression_node(&self);
    fn as_any(&self) -> &dyn Any;
}

/// RootNode of every AST our parser produces.
pub struct RootNode {
    pub statements: Vec<Box<dyn Statement>>,
}

impl RootNode {
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
}

/// ZeroValueExpression - is used for initializations.
pub struct ZeroValueExpression {}
impl Expression for ZeroValueExpression {
    fn token_literal(&self) -> String {
        String::from("zero value")
    }
    fn string(&self) -> String {
        String::from("zero value")
    }
    fn expression_node(&self) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}

/// ZeroValueStatement is used for initializations.
pub struct ZeroValueStatement {}
impl Statement for ZeroValueStatement {
    fn token_literal(&self) -> String {
        String::from("zero value")
    }
    fn string(&self) -> String {
        String::from("zero value")
    }
    fn statement_node(&self) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}

/// Identifier - holds IDENTIFIER token and it's value (add, foobar, x, y, ...).
pub struct Identifier {
    pub token: Token,
    pub value: String,
}

impl Expression for Identifier {
    /// token_literal returns the Identifier's literal and satisfies the Node trait.
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    /// string - returns a string representation of the Identifier and satisfies our Node trait.
    fn string(&self) -> String {
        self.value.clone()
    }
    fn expression_node(&self) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}

/// IntegerLiteral - holds the token and it's value (int64)
pub struct IntegerLiteral {
    pub token: Token,
    pub value: usize,
}

impl Expression for IntegerLiteral {
    /// token_literal returns the IntegerLiteral's literal and satisfies the Node trait.
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    /// string - returns a string representation of the IntegerLiteral and satisfies our Node trait
    fn string(&self) -> String {
        self.token.literal.clone()
    }
    fn expression_node(&self) {}
    fn as_any(&self) -> &dyn Any {
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

impl Expression for PrefixExpression {
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
    fn expression_node(&self) {}
    fn as_any(&self) -> &dyn Any {
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

impl Expression for Boolean {
    /// token_literal returns the Boolean's literal and satisfies the Node trait.
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    /// string - returns a string representation of the Boolean and satisfies our Node trait
    fn string(&self) -> String {
        self.token.literal.clone()
    }
    fn expression_node(&self) {}
    fn as_any(&self) -> &dyn Any {
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

impl Expression for IfExpression {
    /// token_literal returns the IfExpression's literal and satisfies the Node trait.
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    /// string - returns a string representation of the IfExpression and satisfies our Node trait
    fn string(&self) -> String {
        let mut buf = "if".to_owned();
        // TODO: match on condition and get it's Expression
        // buf += self.condition.string();
        buf += " ";
        // buf += self.consequence.string();
        // TODO: something besides nil
        // if self.alternative != nil {
        //     buf += " else ";
        //     buf += self.alternative.string();
        // }
        buf
    }
    fn expression_node(&self) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}

/// BlockStatement - holds the token "{", and a slice of statements
pub struct BlockStatement {
    pub token: Token,
    pub statements: Vec<Box<dyn Statement>>,
}

impl Statement for BlockStatement {
    /// token_literal returns the BlockStatement's literal and satisfies the Node trait.
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    /// string - returns a string representation of the BlockStatement and satisfies our Node trait
    fn string(&self) -> String {
        // TODO: loop over self.statements and call .string() on each
        "BlockStatement".to_owned()
    }
    fn statement_node(&self) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Expression for BlockStatement {
    /// token_literal returns the BlockStatement's literal and satisfies the Node trait.
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    /// string - returns a string representation of the BlockStatement and satisfies our Node trait
    fn string(&self) -> String {
        // TODO: loop over self.statements and call .string() on each
        String::from("BlockStatement")
    }
    fn expression_node(&self) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}

/// LetStatement - Name holds the identifier of the binding and Value for the expression that produces the value.
pub struct LetStatement {
    pub token: Token,
    pub name: Identifier,
    pub value: Box<dyn Expression>,
}

impl Statement for LetStatement {
    /// token_literal returns the LetStatement's literal and satisfies the Node trait.
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    /// string - returns a string representation of the LetStatement and satisfies our Node trait
    fn string(&self) -> String {
        // TODO: actually implement this
        String::from("LetStatement")
    }
    fn statement_node(&self) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}

/// ConstStatement - Name holds the identifier of the binding and value for the expression that produces the value.
pub struct ConstStatement {
    pub token: Token,
    pub name: Identifier,
    pub value: Box<dyn Expression>,
}

impl Statement for ConstStatement {
    /// token_literal returns the ConstStatement's literal and satisfies the Node trait.
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    /// string - returns a string representation of the ConstStatement and satisfies our Node trait
    fn string(&self) -> String {
        // TODO: actually implement this
        String::from("ConstStatement")
    }
    fn statement_node(&self) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}

/// ReturnStatement - pretty self explanatory, holds RETURN token and return value
pub struct ReturnStatement {
    pub token: Token,
    /// The 'return' token
    pub return_value: Box<dyn Expression>,
}

impl Statement for ReturnStatement {
    /// token_literal returns the ReturnStatement's literal and satisfies the Node trait.
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    /// string - returns a string representation of the ReturnStatement and satisfies our Node trait
    fn string(&self) -> String {
        // TODO: actually implement this
        String::from("ReturnStatement")
    }
    fn statement_node(&self) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}

/// ExpressionStatement - holds the first token of the expression and the expression
pub struct ExpressionStatement {
    pub token: Token,
    /// The first token of the expression
    pub expression: Box<dyn Expression>,
}

impl Statement for ExpressionStatement {
    /// token_literal returns the ExpressionStatement's literal and satisfies the Node trait.
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    /// string - returns a string representation of the ExpressionStatement and satisfies our Node trait
    fn string(&self) -> String {
        // TODO: actually implement this
        String::from("ExpressionStatement")
    }
    fn statement_node(&self) {}
    fn as_any(&self) -> &dyn Any {
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

impl Expression for FunctionLiteral {
    /// token_literal returns the FunctionLiteral's literal and satisfies the Node trait.
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    /// string - returns a string representation of the FunctionLiteral and satisfies our Node trait
    fn string(&self) -> String {
        // TODO: actually implement them
        String::from("FunctionLiteral")
    }
    fn expression_node(&self) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}

/// StringLiteral holds the token and it's value (string)
pub struct StringLiteral {
    pub token: Token,
    pub value: String,
}

impl Expression for StringLiteral {
    /// token_literal returns the StringLiteral's literal and satisfies the Node trait.
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    /// string - returns a string representation of the StringLiteral and satisfies our Node trait
    fn string(&self) -> String {
        self.token.literal.clone()
    }
    fn expression_node(&self) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}

/// ArrayLiteral holds the token: '[' and an array of expressions (Elements)
pub struct ArrayLiteral {
    pub token: Token,
    /// the '[' token
    pub elements: Vec<Box<dyn Expression>>,
}

impl Expression for ArrayLiteral {
    /// token_literal returns the ArrayLiteral's literal and satisfies the Node trait.
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    /// string - returns a string representation of the ArrayLiteral and satisfies our Node trait
    fn string(&self) -> String {
        // TODO: actually implement them
        String::from("ArrayLiteral")
    }
    fn expression_node(&self) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}

/// HashLiteral holds the '{' token and the pairs in the hash
pub struct HashLiteral {
    pub token: Token,
    /// The '{' token
    pub pairs: HashMap<String, Box<dyn Expression>>,
}

impl Expression for HashLiteral {
    /// token_literal returns the HashLiteral's literal and satisfies the Node trait.
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    /// string - returns a string representation of the HashLiteral and satisfies our Node trait
    fn string(&self) -> String {
        // TODO: actually implement them
        String::from("HashLiteral")
    }
    fn expression_node(&self) {}
    fn as_any(&self) -> &dyn Any {
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

impl Expression for InfixExpression {
    /// token_literal returns the InfixExpression's literal and satisfies the Node trait.
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    /// string - returns a string representation of the InfixExpression and satisfies our Node trait
    fn string(&self) -> String {
        // TODO: actually implement them
        String::from("InfixExpression")
    }
    fn expression_node(&self) {}
    fn as_any(&self) -> &dyn Any {
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

impl Expression for CallExpression {
    /// token_literal returns the CallExpression's literal and satisfies the Node trait.
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    /// string - returns a string representation of the CallExpression and satisfies our Node trait
    fn string(&self) -> String {
        // TODO: actually implement them
        String::from("CallExpression")
    }
    fn expression_node(&self) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}

pub struct IndexExpression {
    pub token: Token,
    pub left: Box<dyn Expression>,
    pub index: Box<dyn Expression>,
}

impl Expression for IndexExpression {
    /// token_literal returns the IndexExpression's literal and satisfies the Node trait.
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    /// string - returns a string representation of the IndexExpression and satisfies our Node trait
    fn string(&self) -> String {
        // TODO: actually implement them
        String::from("IndexExpression")
    }
    fn expression_node(&self) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}
