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

        while is_integer(self.current_char) {
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
            token_type: TokenType::NONE,
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
                        token_type: TokenType::EQUAL_EQUAL,
                        literal: ch.to_string() + &self.current_char.to_string(),
                        line: self.line,
                    };
                } else {
                    token = new_token(TokenType::EQUAL, self.line, self.current_char.to_string());
                }
            }
            '+' => {
                if self.peek() == '+' {
                    let ch = self.current_char;

                    self.read_char();

                    token = Token {
                        token_type: TokenType::PLUS_PLUS,
                        literal: ch.to_string() + &self.current_char.to_string(),
                        line: self.line,
                    };
                } else {
                    token = new_token(TokenType::PLUS, self.line, self.current_char.to_string());
                }
            }
            '-' => {
                if self.peek() == '-' {
                    let ch = self.current_char;

                    self.read_char();

                    token = Token {
                        token_type: TokenType::MINUS_MINUS,
                        literal: ch.to_string() + &self.current_char.to_string(),
                        line: self.line,
                    };
                } else {
                    token = new_token(TokenType::MINUS, self.line, self.current_char.to_string());
                }
            }
            '!' => {
                if self.peek() == '=' {
                    let ch = self.current_char;

                    self.read_char();

                    token = Token {
                        token_type: TokenType::BANG_EQUAL,
                        literal: ch.to_string() + &self.current_char.to_string(),
                        line: self.line,
                    };
                } else {
                    token = new_token(TokenType::BANG, self.line, self.current_char.to_string());
                }
            }
            '*' => {
                token = new_token(TokenType::STAR, self.line, self.current_char.to_string());
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
                token = new_token(TokenType::SLASH, self.line, self.current_char.to_string());
            }
            '%' => {
                token = new_token(TokenType::MOD, self.line, self.current_char.to_string());
            }
            '<' => {
                if self.peek() == '=' {
                    let ch = self.current_char;

                    self.read_char();

                    let literal = ch.to_string() + &self.current_char.to_string();

                    token = new_token(TokenType::LESS_EQUAL, self.line, literal);
                } else {
                    token = new_token(TokenType::LESS, self.line, self.current_char.to_string());
                }
            }
            '>' => {
                if self.peek() == '=' {
                    let ch = self.current_char;

                    self.read_char();

                    let literal = ch.to_string() + &self.current_char.to_string();

                    token = new_token(TokenType::GREATER_EQUAL, self.line, literal);
                } else {
                    token = new_token(TokenType::GREATER, self.line, self.current_char.to_string());
                }
            }
            '&' => {
                if self.peek() == '&' {
                    let ch = self.current_char;

                    self.read_char();

                    let literal = ch.to_string() + &self.current_char.to_string();

                    token = new_token(TokenType::AND, self.line, literal);
                }
            }
            '|' => {
                if self.peek() == '|' {
                    let ch = self.current_char;

                    self.read_char();

                    let literal = ch.to_string() + &self.current_char.to_string();

                    token = new_token(TokenType::OR, self.line, literal);
                }
            }
            ',' => {
                token = new_token(TokenType::COMMA, self.line, self.current_char.to_string());
            }
            ':' => {
                token = new_token(TokenType::COLON, self.line, self.current_char.to_string());
            }
            ';' => {
                token = new_token(
                    TokenType::SEMICOLON,
                    self.line,
                    self.current_char.to_string(),
                );
            }
            '(' => {
                token = new_token(
                    TokenType::LEFT_PAREN,
                    self.line,
                    self.current_char.to_string(),
                );
            }
            ')' => {
                token = new_token(
                    TokenType::RIGHT_PAREN,
                    self.line,
                    self.current_char.to_string(),
                );
            }
            '{' => {
                token = new_token(
                    TokenType::LEFT_BRACE,
                    self.line,
                    self.current_char.to_string(),
                );
            }
            '}' => {
                token = new_token(
                    TokenType::RIGHT_BRACE,
                    self.line,
                    self.current_char.to_string(),
                );
            }
            '[' => {
                token = new_token(
                    TokenType::LEFT_BRACKET,
                    self.line,
                    self.current_char.to_string(),
                );
            }
            ']' => {
                token = new_token(
                    TokenType::RIGHT_BRACKET,
                    self.line,
                    self.current_char.to_string(),
                );
            }
            '"' => {
                token.token_type = TokenType::STRING;
                token.literal = self.read_string();
                token.line = self.line;
            }
            '\0' => {
                token.literal = String::from("");
                token.token_type = TokenType::EOF;
                token.line = self.line;
            }
            _ => {
                if is_letter(self.current_char) {
                    token.literal = self.read_identifier();
                    token.token_type = look_up_identifier(&token.literal);
                    token.line = self.line;
                    return token;
                } else if is_integer(self.current_char) {
                    token.literal = self.read_integer();
                    token.token_type = TokenType::INTEGER;
                    token.line = self.line;
                    return token;
                } else {
                    token = new_token(TokenType::ILLEGAL, self.line, self.current_char.to_string());
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
    'a' <= character && character <= 'z'
        || 'A' <= character && character <= 'Z'
        || character == '_'
        || character == '?'
}

fn is_integer(character: char) -> bool {
    '0' <= character && character <= '9'
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
                token_type: TokenType::LET,
                literal: String::from("let"),
                line: 1,
            },
            Token {
                token_type: TokenType::IDENTIFIER,
                literal: String::from("five"),
                line: 1,
            },
            Token {
                token_type: TokenType::EQUAL,
                literal: String::from("="),
                line: 1,
            },
            Token {
                token_type: TokenType::INTEGER,
                literal: String::from("5"),
                line: 1,
            },
            Token {
                token_type: TokenType::SEMICOLON,
                literal: String::from(";"),
                line: 1,
            },
            Token {
                token_type: TokenType::LET,
                literal: String::from("let"),
                line: 2,
            },
            Token {
                token_type: TokenType::IDENTIFIER,
                literal: String::from("ten"),
                line: 2,
            },
            Token {
                token_type: TokenType::EQUAL,
                literal: String::from("="),
                line: 2,
            },
            Token {
                token_type: TokenType::INTEGER,
                literal: String::from("10"),
                line: 2,
            },
            Token {
                token_type: TokenType::SEMICOLON,
                literal: String::from(";"),
                line: 2,
            },
            Token {
                token_type: TokenType::LET,
                literal: String::from("let"),
                line: 3,
            },
            Token {
                token_type: TokenType::IDENTIFIER,
                literal: String::from("add"),
                line: 3,
            },
            Token {
                token_type: TokenType::EQUAL,
                literal: String::from("="),
                line: 3,
            },
            Token {
                token_type: TokenType::FUNCTION,
                literal: String::from("func"),
                line: 3,
            },
            Token {
                token_type: TokenType::LEFT_PAREN,
                literal: String::from("("),
                line: 3,
            },
            Token {
                token_type: TokenType::IDENTIFIER,
                literal: String::from("x"),
                line: 3,
            },
            Token {
                token_type: TokenType::COMMA,
                literal: String::from(","),
                line: 3,
            },
            Token {
                token_type: TokenType::IDENTIFIER,
                literal: String::from("y"),
                line: 3,
            },
            Token {
                token_type: TokenType::RIGHT_PAREN,
                literal: String::from(")"),
                line: 3,
            },
            Token {
                token_type: TokenType::LEFT_BRACE,
                literal: String::from("{"),
                line: 3,
            },
            Token {
                token_type: TokenType::IDENTIFIER,
                literal: String::from("x"),
                line: 4,
            },
            Token {
                token_type: TokenType::PLUS,
                literal: String::from("+"),
                line: 4,
            },
            Token {
                token_type: TokenType::IDENTIFIER,
                literal: String::from("y"),
                line: 4,
            },
            Token {
                token_type: TokenType::SEMICOLON,
                literal: String::from(";"),
                line: 4,
            },
            Token {
                token_type: TokenType::RIGHT_BRACE,
                literal: String::from("}"),
                line: 5,
            },
            Token {
                token_type: TokenType::SEMICOLON,
                literal: String::from(";"),
                line: 5,
            },
            Token {
                token_type: TokenType::LET,
                literal: String::from("let"),
                line: 6,
            },
            Token {
                token_type: TokenType::IDENTIFIER,
                literal: String::from("result"),
                line: 6,
            },
            Token {
                token_type: TokenType::EQUAL,
                literal: String::from("="),
                line: 6,
            },
            Token {
                token_type: TokenType::IDENTIFIER,
                literal: String::from("add"),
                line: 6,
            },
            Token {
                token_type: TokenType::LEFT_PAREN,
                literal: String::from("("),
                line: 6,
            },
            Token {
                token_type: TokenType::IDENTIFIER,
                literal: String::from("five"),
                line: 6,
            },
            Token {
                token_type: TokenType::COMMA,
                literal: String::from(","),
                line: 6,
            },
            Token {
                token_type: TokenType::IDENTIFIER,
                literal: String::from("ten"),
                line: 6,
            },
            Token {
                token_type: TokenType::RIGHT_PAREN,
                literal: String::from(")"),
                line: 6,
            },
            Token {
                token_type: TokenType::SEMICOLON,
                literal: String::from(";"),
                line: 6,
            },
            Token {
                token_type: TokenType::BANG,
                literal: String::from("!"),
                line: 8,
            },
            Token {
                token_type: TokenType::MINUS,
                literal: String::from("-"),
                line: 8,
            },
            Token {
                token_type: TokenType::STAR,
                literal: String::from("*"),
                line: 8,
            },
            Token {
                token_type: TokenType::SLASH,
                literal: String::from("/"),
                line: 8,
            },
            Token {
                token_type: TokenType::INTEGER,
                literal: String::from("5"),
                line: 8,
            },
            Token {
                token_type: TokenType::SEMICOLON,
                literal: String::from(";"),
                line: 8,
            },
            Token {
                token_type: TokenType::INTEGER,
                literal: String::from("5"),
                line: 9,
            },
            Token {
                token_type: TokenType::LESS,
                literal: String::from("<"),
                line: 9,
            },
            Token {
                token_type: TokenType::INTEGER,
                literal: String::from("10"),
                line: 9,
            },
            Token {
                token_type: TokenType::GREATER,
                literal: String::from(">"),
                line: 9,
            },
            Token {
                token_type: TokenType::INTEGER,
                literal: String::from("5"),
                line: 9,
            },
            Token {
                token_type: TokenType::SEMICOLON,
                literal: String::from(";"),
                line: 9,
            },
            Token {
                token_type: TokenType::IF,
                literal: String::from("if"),
                line: 11,
            },
            Token {
                token_type: TokenType::LEFT_PAREN,
                literal: String::from("("),
                line: 11,
            },
            Token {
                token_type: TokenType::INTEGER,
                literal: String::from("5"),
                line: 11,
            },
            Token {
                token_type: TokenType::LESS,
                literal: String::from("<"),
                line: 11,
            },
            Token {
                token_type: TokenType::INTEGER,
                literal: String::from("10"),
                line: 11,
            },
            Token {
                token_type: TokenType::RIGHT_PAREN,
                literal: String::from(")"),
                line: 11,
            },
            Token {
                token_type: TokenType::LEFT_BRACE,
                literal: String::from("{"),
                line: 11,
            },
            Token {
                token_type: TokenType::RETURN,
                literal: String::from("return"),
                line: 12,
            },
            Token {
                token_type: TokenType::TRUE,
                literal: String::from("true"),
                line: 12,
            },
            Token {
                token_type: TokenType::SEMICOLON,
                literal: String::from(";"),
                line: 12,
            },
            Token {
                token_type: TokenType::RIGHT_BRACE,
                literal: String::from("}"),
                line: 13,
            },
            Token {
                token_type: TokenType::ELSE,
                literal: String::from("else"),
                line: 13,
            },
            Token {
                token_type: TokenType::LEFT_BRACE,
                literal: String::from("{"),
                line: 13,
            },
            Token {
                token_type: TokenType::RETURN,
                literal: String::from("return"),
                line: 14,
            },
            Token {
                token_type: TokenType::FALSE,
                literal: String::from("false"),
                line: 14,
            },
            Token {
                token_type: TokenType::SEMICOLON,
                literal: String::from(";"),
                line: 14,
            },
            Token {
                token_type: TokenType::RIGHT_BRACE,
                literal: String::from("}"),
                line: 15,
            },
            Token {
                token_type: TokenType::INTEGER,
                literal: String::from("10"),
                line: 17,
            },
            Token {
                token_type: TokenType::EQUAL_EQUAL,
                literal: String::from("=="),
                line: 17,
            },
            Token {
                token_type: TokenType::INTEGER,
                literal: String::from("10"),
                line: 17,
            },
            Token {
                token_type: TokenType::SEMICOLON,
                literal: String::from(";"),
                line: 17,
            },
            Token {
                token_type: TokenType::INTEGER,
                literal: String::from("10"),
                line: 18,
            },
            Token {
                token_type: TokenType::BANG_EQUAL,
                literal: String::from("!="),
                line: 18,
            },
            Token {
                token_type: TokenType::INTEGER,
                literal: String::from("9"),
                line: 18,
            },
            Token {
                token_type: TokenType::SEMICOLON,
                literal: String::from(";"),
                line: 18,
            },
            Token {
                token_type: TokenType::STRING,
                literal: String::from("foobar"),
                line: 20,
            },
            Token {
                token_type: TokenType::STRING,
                literal: String::from("foo bar"),
                line: 21,
            },
            Token {
                token_type: TokenType::LEFT_BRACKET,
                literal: String::from("["),
                line: 25,
            },
            Token {
                token_type: TokenType::INTEGER,
                literal: String::from("1"),
                line: 25,
            },
            Token {
                token_type: TokenType::COMMA,
                literal: String::from(","),
                line: 25,
            },
            Token {
                token_type: TokenType::INTEGER,
                literal: String::from("2"),
                line: 25,
            },
            Token {
                token_type: TokenType::RIGHT_BRACKET,
                literal: String::from("]"),
                line: 25,
            },
            Token {
                token_type: TokenType::SEMICOLON,
                literal: String::from(";"),
                line: 25,
            },
            Token {
                token_type: TokenType::LEFT_BRACE,
                literal: String::from("{"),
                line: 27,
            },
            Token {
                token_type: TokenType::STRING,
                literal: String::from("foo"),
                line: 27,
            },
            Token {
                token_type: TokenType::COLON,
                literal: String::from(":"),
                line: 27,
            },
            Token {
                token_type: TokenType::STRING,
                literal: String::from("bar"),
                line: 27,
            },
            Token {
                token_type: TokenType::RIGHT_BRACE,
                literal: String::from("}"),
                line: 27,
            },
            Token {
                token_type: TokenType::TRUE,
                literal: String::from("true"),
                line: 29,
            },
            Token {
                token_type: TokenType::AND,
                literal: String::from("&&"),
                line: 29,
            },
            Token {
                token_type: TokenType::FALSE,
                literal: String::from("false"),
                line: 29,
            },
            Token {
                token_type: TokenType::SEMICOLON,
                literal: String::from(";"),
                line: 29,
            },
            Token {
                token_type: TokenType::TRUE,
                literal: String::from("true"),
                line: 30,
            },
            Token {
                token_type: TokenType::OR,
                literal: String::from("||"),
                line: 30,
            },
            Token {
                token_type: TokenType::FALSE,
                literal: String::from("false"),
                line: 30,
            },
            Token {
                token_type: TokenType::SEMICOLON,
                literal: String::from(";"),
                line: 30,
            },
            Token {
                token_type: TokenType::INTEGER,
                literal: String::from("10"),
                line: 33,
            },
            Token {
                token_type: TokenType::INTEGER,
                literal: String::from("10"),
                line: 35,
            },
            Token {
                token_type: TokenType::CONST,
                literal: String::from("const"),
                line: 37,
            },
            Token {
                token_type: TokenType::IDENTIFIER,
                literal: String::from("cantChangeMe"),
                line: 37,
            },
            Token {
                token_type: TokenType::EQUAL,
                literal: String::from("="),
                line: 37,
            },
            Token {
                token_type: TokenType::STRING,
                literal: String::from("neato"),
                line: 37,
            },
            Token {
                token_type: TokenType::SEMICOLON,
                literal: String::from(";"),
                line: 37,
            },
            Token {
                token_type: TokenType::INTEGER,
                literal: String::from("10"),
                line: 39,
            },
            Token {
                token_type: TokenType::MOD,
                literal: String::from("%"),
                line: 39,
            },
            Token {
                token_type: TokenType::INTEGER,
                literal: String::from("3"),
                line: 39,
            },
            Token {
                token_type: TokenType::SEMICOLON,
                literal: String::from(";"),
                line: 39,
            },
            Token {
                token_type: TokenType::IDENTIFIER,
                literal: String::from("five"),
                line: 41,
            },
            Token {
                token_type: TokenType::PLUS_PLUS,
                literal: String::from("++"),
                line: 41,
            },
            Token {
                token_type: TokenType::IDENTIFIER,
                literal: String::from("five"),
                line: 42,
            },
            Token {
                token_type: TokenType::MINUS_MINUS,
                literal: String::from("--"),
                line: 42,
            },
            Token {
                token_type: TokenType::INTEGER,
                literal: String::from("5"),
                line: 44,
            },
            Token {
                token_type: TokenType::GREATER_EQUAL,
                literal: String::from(">="),
                line: 44,
            },
            Token {
                token_type: TokenType::INTEGER,
                literal: String::from("5"),
                line: 44,
            },
            Token {
                token_type: TokenType::SEMICOLON,
                literal: String::from(";"),
                line: 44,
            },
            Token {
                token_type: TokenType::INTEGER,
                literal: String::from("5"),
                line: 45,
            },
            Token {
                token_type: TokenType::LESS_EQUAL,
                literal: String::from("<="),
                line: 45,
            },
            Token {
                token_type: TokenType::INTEGER,
                literal: String::from("5"),
                line: 45,
            },
            Token {
                token_type: TokenType::SEMICOLON,
                literal: String::from(";"),
                line: 45,
            },
            Token {
                token_type: TokenType::LET,
                literal: String::from("let"),
                line: 49,
            },
            Token {
                token_type: TokenType::IDENTIFIER,
                literal: String::from("snake_case_with_question_mark?"),
                line: 49,
            },
            Token {
                token_type: TokenType::EQUAL,
                literal: String::from("="),
                line: 49,
            },
            Token {
                token_type: TokenType::TRUE,
                literal: String::from("true"),
                line: 49,
            },
            Token {
                token_type: TokenType::SEMICOLON,
                literal: String::from(";"),
                line: 49,
            },
            Token {
                token_type: TokenType::EOF,
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
