use crate::token::{look_up_identifier, Token, TokenType};

#[derive(Clone)]
pub struct Lexer {
    input: Vec<char>,
    current_char: char,
    position: usize,
    read_position: usize,
    line: usize,
}

impl Lexer {
    pub fn new(input: String) -> Self {
        let mut lexer = Lexer {
            input: input.chars().collect(),
            current_char: 'a',
            position: 0,
            read_position: 0,
            line: 0,
        };
        lexer.read_char();

        lexer
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.current_char = '\0';
        } else {
            self.current_char = self.input[self.read_position];
        }

        self.position = self.read_position;
        self.read_position += 1;
    }

    fn read_string(&mut self) -> String {
        let position = self.position + 1;

        loop {
            self.read_char();
            if self.current_char == '"' || self.current_char == '\0' {
                break;
            }
        }

        self.input[position..self.position].iter().collect()
    }

    fn read_identifier(&mut self) -> String {
        let position = self.position;

        while is_letter(self.current_char) {
            self.read_char();
        }

        self.input[position..self.position].iter().collect()
    }

    fn read_integer(&mut self) -> String {
        let position = self.position;

        while self.current_char.is_ascii_digit() {
            self.read_char();
        }

        self.input[position..self.position].iter().collect()
    }

    fn skip_whitespace(&mut self) {
        while self.current_char == ' '
            || self.current_char == '\t'
            || self.current_char == '\n'
            || self.current_char == '\r'
        {
            if self.current_char == '\n' {
                self.line += 1;
            }
            self.read_char();
        }
    }

    fn skip_single_line_comment(&mut self) {
        while self.current_char != '\n' && self.current_char != '\0' {
            self.read_char();
        }
        self.skip_whitespace();
    }

    fn skip_multi_line_comment(&mut self) {
        let mut end_found = false;

        while !end_found {
            if self.current_char == '\0' {
                end_found = true;
            }

            if self.current_char == '*' && self.peek() == '/' {
                end_found = true;
                self.read_char();
            }

            self.read_char();
        }

        self.skip_whitespace();
    }

    fn peek(&self) -> char {
        if self.read_position >= self.input.len() {
            return '\0';
        }
        self.input[self.read_position]
    }

    pub fn next_token(&mut self) -> Token {
        let mut token = Token {
            token_type: TokenType::None,
            literal: "".to_owned(),
            line: 0,
        };

        self.skip_whitespace();

        match self.current_char {
            '=' => {
                if self.peek() == '=' {
                    let ch = self.current_char;

                    self.read_char();

                    token = Token {
                        token_type: TokenType::EqualEqual,
                        literal: ch.to_string() + &self.current_char.to_string(),
                        line: self.line,
                    };
                } else {
                    token = new_token(TokenType::Equal, self.line, self.current_char.to_string());
                }
            }
            '+' => {
                if self.peek() == '+' {
                    let ch = self.current_char;

                    self.read_char();

                    token = Token {
                        token_type: TokenType::PlusPlus,
                        literal: ch.to_string() + &self.current_char.to_string(),
                        line: self.line,
                    };
                } else {
                    token = new_token(TokenType::Plus, self.line, self.current_char.to_string());
                }
            }
            '-' => {
                if self.peek() == '-' {
                    let ch = self.current_char;

                    self.read_char();

                    token = Token {
                        token_type: TokenType::MinusMinus,
                        literal: ch.to_string() + &self.current_char.to_string(),
                        line: self.line,
                    };
                } else {
                    token = new_token(TokenType::Minus, self.line, self.current_char.to_string());
                }
            }
            '!' => {
                if self.peek() == '=' {
                    let ch = self.current_char;

                    self.read_char();

                    token = Token {
                        token_type: TokenType::BangEqual,
                        literal: ch.to_string() + &self.current_char.to_string(),
                        line: self.line,
                    };
                } else {
                    token = new_token(TokenType::Bang, self.line, self.current_char.to_string());
                }
            }
            '*' => {
                token = new_token(TokenType::Star, self.line, self.current_char.to_string());
            }
            '/' => {
                if self.peek() == '/' {
                    self.skip_single_line_comment();
                    return self.next_token();
                }
                if self.peek() == '*' {
                    self.skip_multi_line_comment();
                    return self.next_token();
                }
                token = new_token(TokenType::Slash, self.line, self.current_char.to_string());
            }
            '%' => {
                token = new_token(TokenType::Mod, self.line, self.current_char.to_string());
            }
            '<' => {
                if self.peek() == '=' {
                    let ch = self.current_char;

                    self.read_char();

                    let literal = ch.to_string() + &self.current_char.to_string();

                    token = new_token(TokenType::LessEqual, self.line, literal);
                } else {
                    token = new_token(TokenType::Less, self.line, self.current_char.to_string());
                }
            }
            '>' => {
                if self.peek() == '=' {
                    let ch = self.current_char;

                    self.read_char();

                    let literal = ch.to_string() + &self.current_char.to_string();

                    token = new_token(TokenType::GreaterEqual, self.line, literal);
                } else {
                    token = new_token(TokenType::Greater, self.line, self.current_char.to_string());
                }
            }
            '&' => {
                if self.peek() == '&' {
                    let ch = self.current_char;

                    self.read_char();

                    let literal = ch.to_string() + &self.current_char.to_string();

                    token = new_token(TokenType::And, self.line, literal);
                }
            }
            '|' => {
                if self.peek() == '|' {
                    let ch = self.current_char;

                    self.read_char();

                    let literal = ch.to_string() + &self.current_char.to_string();

                    token = new_token(TokenType::Or, self.line, literal);
                }
            }
            ',' => {
                token = new_token(TokenType::Comma, self.line, self.current_char.to_string());
            }
            ':' => {
                token = new_token(TokenType::Colon, self.line, self.current_char.to_string());
            }
            ';' => {
                token = new_token(
                    TokenType::Semicolon,
                    self.line,
                    self.current_char.to_string(),
                );
            }
            '(' => {
                token = new_token(
                    TokenType::LeftParen,
                    self.line,
                    self.current_char.to_string(),
                );
            }
            ')' => {
                token = new_token(
                    TokenType::RightParen,
                    self.line,
                    self.current_char.to_string(),
                );
            }
            '{' => {
                token = new_token(
                    TokenType::LeftBrace,
                    self.line,
                    self.current_char.to_string(),
                );
            }
            '}' => {
                token = new_token(
                    TokenType::RightBrace,
                    self.line,
                    self.current_char.to_string(),
                );
            }
            '[' => {
                token = new_token(
                    TokenType::LeftBracket,
                    self.line,
                    self.current_char.to_string(),
                );
            }
            ']' => {
                token = new_token(
                    TokenType::RightBracket,
                    self.line,
                    self.current_char.to_string(),
                );
            }
            '"' => {
                token.token_type = TokenType::String;
                token.literal = self.read_string();
                token.line = self.line;
            }
            '\0' => {
                token.literal = String::from("");
                token.token_type = TokenType::Eof;
                token.line = self.line;
            }
            _ => {
                if is_letter(self.current_char) {
                    token.literal = self.read_identifier();
                    token.token_type = look_up_identifier(&token.literal);
                    token.line = self.line;
                    return token;
                } else if self.current_char.is_ascii_digit() {
                    token.literal = self.read_integer();
                    token.token_type = TokenType::Integer;
                    token.line = self.line;
                    return token;
                } else {
                    token = new_token(TokenType::Illegal, self.line, self.current_char.to_string());
                }
            }
        }

        self.read_char();

        token
    }
}

fn new_token(token_type: TokenType, line: usize, literal: String) -> Token {
    Token {
        token_type,
        literal,
        line,
    }
}

fn is_letter(character: char) -> bool {
    character.is_ascii_lowercase()
        || character.is_ascii_uppercase()
        || character == '_'
        || character == '?'
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_next_token() {
        let test_input = r#"
let five = 5;
let ten = 10;
let add = func(x, y) {
    x + y;
};
let result = add(five, ten);

!-*/5;
5 < 10 > 5;

if (5 < 10) {
    return true;
} else {
    return false;
}

10 == 10;
10 != 9;

"foobar"
"foo bar"

/* multiline comment */

[1, 2];

{"foo": "bar"}

true && false;
true || false;

// This is a comment above the number 10
10

10 // This is a comment to the right of 10

const cantChangeMe = "neato";

10 % 3;

five++
five--

5 >= 5;
5 <= 5;

/*
    multiline comment
*/

let snake_case_with_question_mark? = true;
    "#;
        let expected_tokens = vec![
            Token {
                token_type: TokenType::Let,
                literal: String::from("let"),
                line: 1,
            },
            Token {
                token_type: TokenType::Identifier,
                literal: String::from("five"),
                line: 1,
            },
            Token {
                token_type: TokenType::Equal,
                literal: String::from("="),
                line: 1,
            },
            Token {
                token_type: TokenType::Integer,
                literal: String::from("5"),
                line: 1,
            },
            Token {
                token_type: TokenType::Semicolon,
                literal: String::from(";"),
                line: 1,
            },
            Token {
                token_type: TokenType::Let,
                literal: String::from("let"),
                line: 2,
            },
            Token {
                token_type: TokenType::Identifier,
                literal: String::from("ten"),
                line: 2,
            },
            Token {
                token_type: TokenType::Equal,
                literal: String::from("="),
                line: 2,
            },
            Token {
                token_type: TokenType::Integer,
                literal: String::from("10"),
                line: 2,
            },
            Token {
                token_type: TokenType::Semicolon,
                literal: String::from(";"),
                line: 2,
            },
            Token {
                token_type: TokenType::Let,
                literal: String::from("let"),
                line: 3,
            },
            Token {
                token_type: TokenType::Identifier,
                literal: String::from("add"),
                line: 3,
            },
            Token {
                token_type: TokenType::Equal,
                literal: String::from("="),
                line: 3,
            },
            Token {
                token_type: TokenType::Function,
                literal: String::from("func"),
                line: 3,
            },
            Token {
                token_type: TokenType::LeftParen,
                literal: String::from("("),
                line: 3,
            },
            Token {
                token_type: TokenType::Identifier,
                literal: String::from("x"),
                line: 3,
            },
            Token {
                token_type: TokenType::Comma,
                literal: String::from(","),
                line: 3,
            },
            Token {
                token_type: TokenType::Identifier,
                literal: String::from("y"),
                line: 3,
            },
            Token {
                token_type: TokenType::RightParen,
                literal: String::from(")"),
                line: 3,
            },
            Token {
                token_type: TokenType::LeftBrace,
                literal: String::from("{"),
                line: 3,
            },
            Token {
                token_type: TokenType::Identifier,
                literal: String::from("x"),
                line: 4,
            },
            Token {
                token_type: TokenType::Plus,
                literal: String::from("+"),
                line: 4,
            },
            Token {
                token_type: TokenType::Identifier,
                literal: String::from("y"),
                line: 4,
            },
            Token {
                token_type: TokenType::Semicolon,
                literal: String::from(";"),
                line: 4,
            },
            Token {
                token_type: TokenType::RightBrace,
                literal: String::from("}"),
                line: 5,
            },
            Token {
                token_type: TokenType::Semicolon,
                literal: String::from(";"),
                line: 5,
            },
            Token {
                token_type: TokenType::Let,
                literal: String::from("let"),
                line: 6,
            },
            Token {
                token_type: TokenType::Identifier,
                literal: String::from("result"),
                line: 6,
            },
            Token {
                token_type: TokenType::Equal,
                literal: String::from("="),
                line: 6,
            },
            Token {
                token_type: TokenType::Identifier,
                literal: String::from("add"),
                line: 6,
            },
            Token {
                token_type: TokenType::LeftParen,
                literal: String::from("("),
                line: 6,
            },
            Token {
                token_type: TokenType::Identifier,
                literal: String::from("five"),
                line: 6,
            },
            Token {
                token_type: TokenType::Comma,
                literal: String::from(","),
                line: 6,
            },
            Token {
                token_type: TokenType::Identifier,
                literal: String::from("ten"),
                line: 6,
            },
            Token {
                token_type: TokenType::RightParen,
                literal: String::from(")"),
                line: 6,
            },
            Token {
                token_type: TokenType::Semicolon,
                literal: String::from(";"),
                line: 6,
            },
            Token {
                token_type: TokenType::Bang,
                literal: String::from("!"),
                line: 8,
            },
            Token {
                token_type: TokenType::Minus,
                literal: String::from("-"),
                line: 8,
            },
            Token {
                token_type: TokenType::Star,
                literal: String::from("*"),
                line: 8,
            },
            Token {
                token_type: TokenType::Slash,
                literal: String::from("/"),
                line: 8,
            },
            Token {
                token_type: TokenType::Integer,
                literal: String::from("5"),
                line: 8,
            },
            Token {
                token_type: TokenType::Semicolon,
                literal: String::from(";"),
                line: 8,
            },
            Token {
                token_type: TokenType::Integer,
                literal: String::from("5"),
                line: 9,
            },
            Token {
                token_type: TokenType::Less,
                literal: String::from("<"),
                line: 9,
            },
            Token {
                token_type: TokenType::Integer,
                literal: String::from("10"),
                line: 9,
            },
            Token {
                token_type: TokenType::Greater,
                literal: String::from(">"),
                line: 9,
            },
            Token {
                token_type: TokenType::Integer,
                literal: String::from("5"),
                line: 9,
            },
            Token {
                token_type: TokenType::Semicolon,
                literal: String::from(";"),
                line: 9,
            },
            Token {
                token_type: TokenType::If,
                literal: String::from("if"),
                line: 11,
            },
            Token {
                token_type: TokenType::LeftParen,
                literal: String::from("("),
                line: 11,
            },
            Token {
                token_type: TokenType::Integer,
                literal: String::from("5"),
                line: 11,
            },
            Token {
                token_type: TokenType::Less,
                literal: String::from("<"),
                line: 11,
            },
            Token {
                token_type: TokenType::Integer,
                literal: String::from("10"),
                line: 11,
            },
            Token {
                token_type: TokenType::RightParen,
                literal: String::from(")"),
                line: 11,
            },
            Token {
                token_type: TokenType::LeftBrace,
                literal: String::from("{"),
                line: 11,
            },
            Token {
                token_type: TokenType::Return,
                literal: String::from("return"),
                line: 12,
            },
            Token {
                token_type: TokenType::True,
                literal: String::from("true"),
                line: 12,
            },
            Token {
                token_type: TokenType::Semicolon,
                literal: String::from(";"),
                line: 12,
            },
            Token {
                token_type: TokenType::RightBrace,
                literal: String::from("}"),
                line: 13,
            },
            Token {
                token_type: TokenType::Else,
                literal: String::from("else"),
                line: 13,
            },
            Token {
                token_type: TokenType::LeftBrace,
                literal: String::from("{"),
                line: 13,
            },
            Token {
                token_type: TokenType::Return,
                literal: String::from("return"),
                line: 14,
            },
            Token {
                token_type: TokenType::False,
                literal: String::from("false"),
                line: 14,
            },
            Token {
                token_type: TokenType::Semicolon,
                literal: String::from(";"),
                line: 14,
            },
            Token {
                token_type: TokenType::RightBrace,
                literal: String::from("}"),
                line: 15,
            },
            Token {
                token_type: TokenType::Integer,
                literal: String::from("10"),
                line: 17,
            },
            Token {
                token_type: TokenType::EqualEqual,
                literal: String::from("=="),
                line: 17,
            },
            Token {
                token_type: TokenType::Integer,
                literal: String::from("10"),
                line: 17,
            },
            Token {
                token_type: TokenType::Semicolon,
                literal: String::from(";"),
                line: 17,
            },
            Token {
                token_type: TokenType::Integer,
                literal: String::from("10"),
                line: 18,
            },
            Token {
                token_type: TokenType::BangEqual,
                literal: String::from("!="),
                line: 18,
            },
            Token {
                token_type: TokenType::Integer,
                literal: String::from("9"),
                line: 18,
            },
            Token {
                token_type: TokenType::Semicolon,
                literal: String::from(";"),
                line: 18,
            },
            Token {
                token_type: TokenType::String,
                literal: String::from("foobar"),
                line: 20,
            },
            Token {
                token_type: TokenType::String,
                literal: String::from("foo bar"),
                line: 21,
            },
            Token {
                token_type: TokenType::LeftBracket,
                literal: String::from("["),
                line: 25,
            },
            Token {
                token_type: TokenType::Integer,
                literal: String::from("1"),
                line: 25,
            },
            Token {
                token_type: TokenType::Comma,
                literal: String::from(","),
                line: 25,
            },
            Token {
                token_type: TokenType::Integer,
                literal: String::from("2"),
                line: 25,
            },
            Token {
                token_type: TokenType::RightBracket,
                literal: String::from("]"),
                line: 25,
            },
            Token {
                token_type: TokenType::Semicolon,
                literal: String::from(";"),
                line: 25,
            },
            Token {
                token_type: TokenType::LeftBrace,
                literal: String::from("{"),
                line: 27,
            },
            Token {
                token_type: TokenType::String,
                literal: String::from("foo"),
                line: 27,
            },
            Token {
                token_type: TokenType::Colon,
                literal: String::from(":"),
                line: 27,
            },
            Token {
                token_type: TokenType::String,
                literal: String::from("bar"),
                line: 27,
            },
            Token {
                token_type: TokenType::RightBrace,
                literal: String::from("}"),
                line: 27,
            },
            Token {
                token_type: TokenType::True,
                literal: String::from("true"),
                line: 29,
            },
            Token {
                token_type: TokenType::And,
                literal: String::from("&&"),
                line: 29,
            },
            Token {
                token_type: TokenType::False,
                literal: String::from("false"),
                line: 29,
            },
            Token {
                token_type: TokenType::Semicolon,
                literal: String::from(";"),
                line: 29,
            },
            Token {
                token_type: TokenType::True,
                literal: String::from("true"),
                line: 30,
            },
            Token {
                token_type: TokenType::Or,
                literal: String::from("||"),
                line: 30,
            },
            Token {
                token_type: TokenType::False,
                literal: String::from("false"),
                line: 30,
            },
            Token {
                token_type: TokenType::Semicolon,
                literal: String::from(";"),
                line: 30,
            },
            Token {
                token_type: TokenType::Integer,
                literal: String::from("10"),
                line: 33,
            },
            Token {
                token_type: TokenType::Integer,
                literal: String::from("10"),
                line: 35,
            },
            Token {
                token_type: TokenType::Const,
                literal: String::from("const"),
                line: 37,
            },
            Token {
                token_type: TokenType::Identifier,
                literal: String::from("cantChangeMe"),
                line: 37,
            },
            Token {
                token_type: TokenType::Equal,
                literal: String::from("="),
                line: 37,
            },
            Token {
                token_type: TokenType::String,
                literal: String::from("neato"),
                line: 37,
            },
            Token {
                token_type: TokenType::Semicolon,
                literal: String::from(";"),
                line: 37,
            },
            Token {
                token_type: TokenType::Integer,
                literal: String::from("10"),
                line: 39,
            },
            Token {
                token_type: TokenType::Mod,
                literal: String::from("%"),
                line: 39,
            },
            Token {
                token_type: TokenType::Integer,
                literal: String::from("3"),
                line: 39,
            },
            Token {
                token_type: TokenType::Semicolon,
                literal: String::from(";"),
                line: 39,
            },
            Token {
                token_type: TokenType::Identifier,
                literal: String::from("five"),
                line: 41,
            },
            Token {
                token_type: TokenType::PlusPlus,
                literal: String::from("++"),
                line: 41,
            },
            Token {
                token_type: TokenType::Identifier,
                literal: String::from("five"),
                line: 42,
            },
            Token {
                token_type: TokenType::MinusMinus,
                literal: String::from("--"),
                line: 42,
            },
            Token {
                token_type: TokenType::Integer,
                literal: String::from("5"),
                line: 44,
            },
            Token {
                token_type: TokenType::GreaterEqual,
                literal: String::from(">="),
                line: 44,
            },
            Token {
                token_type: TokenType::Integer,
                literal: String::from("5"),
                line: 44,
            },
            Token {
                token_type: TokenType::Semicolon,
                literal: String::from(";"),
                line: 44,
            },
            Token {
                token_type: TokenType::Integer,
                literal: String::from("5"),
                line: 45,
            },
            Token {
                token_type: TokenType::LessEqual,
                literal: String::from("<="),
                line: 45,
            },
            Token {
                token_type: TokenType::Integer,
                literal: String::from("5"),
                line: 45,
            },
            Token {
                token_type: TokenType::Semicolon,
                literal: String::from(";"),
                line: 45,
            },
            Token {
                token_type: TokenType::Let,
                literal: String::from("let"),
                line: 49,
            },
            Token {
                token_type: TokenType::Identifier,
                literal: String::from("snake_case_with_question_mark?"),
                line: 49,
            },
            Token {
                token_type: TokenType::Equal,
                literal: String::from("="),
                line: 49,
            },
            Token {
                token_type: TokenType::True,
                literal: String::from("true"),
                line: 49,
            },
            Token {
                token_type: TokenType::Semicolon,
                literal: String::from(";"),
                line: 49,
            },
            Token {
                token_type: TokenType::Eof,
                literal: String::from(""),
                line: 50,
            },
        ];

        let mut lexer = Lexer::new(test_input.to_string());

        for expected in expected_tokens {
            let token = lexer.next_token();
            assert_eq!(expected, token);
        }
    }
}
