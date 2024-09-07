use std::cmp::Ordering;

/// An experimental TSQL [`Parser`].
///
/// I've had trouble finding a good TSQL formatter for my team. I enjoyed contributing to [Ruff], so I've been interested in
/// building a formatter of my own for a while. **This is meant to be experimental and used for learning purposes.**
///
/// As with Ruff's parser, I'd like to implement [`Parser`] using the [Pratt parsing algorithm] and optimized to parse TSQL code.
///
/// I considered using [sqlparser-rs] as a parser for the formatter, but I don't want to spend time generating code for an
/// [orphan rule] workaround. I'd rather spend that time writing this parser.
///
/// [Ruff]: https://github.com/astral-sh/ruff
/// [Pratt parsing algorithm]: https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html
/// [sqlparser-rs]: https://github.com/sqlparser-rs/sqlparser-rs
/// [orphan rule]: https://doc.rust-lang.org/reference/items/implementations.html#trait-implementation-coherence
struct Parser<'src> {
    source: &'src str,
    lexer: Lexer<'src>,
}

impl Parser<'_> {
    fn new(source: &str) -> Parser {
        Parser {
            source,
            lexer: Lexer::new(source),
        }
    }

    fn parse(&mut self) -> Vec<Token> {
        let mut tokens = Vec::new();

        while let Some(token) = self.lexer.next_token() {
            tokens.push(token)
        }

        tokens
    }
}

struct Lexer<'src> {
    source: &'src str,
    cursor: usize,
    prev_cursor: usize,
}

impl Lexer<'_> {
    fn new(source: &str) -> Lexer {
        Lexer {
            source,
            cursor: 0,
            prev_cursor: 0,
        }
    }

    fn next_token(&mut self) -> Option<Token> {
        self.skip_whitespace();

        match self.current() {
            Some('\'') => Some(self.lex_string()),
            Some('-') => {
                if self.peek().is_some_and(char::is_whitespace) {
                    Some(self.eat(TokenKind::Minus))
                } else {
                    Some(self.lex_number())
                }
            }
            Some('=') => Some(self.eat(TokenKind::Eq)),
            Some('*') => Some(self.eat(TokenKind::Star)),
            Some(';') => Some(self.eat(TokenKind::Semicolon)),
            Some('!') => {
                if self.peek().is_some_and(|it| it == '=') {
                    Some(self.eat(TokenKind::NotEq))
                } else {
                    unimplemented!()
                }
            }
            Some('<') => {
                if self.peek().is_some_and(|it| it == '=') {
                    Some(self.eat(TokenKind::LessThanEq))
                } else if self.peek().is_some_and(char::is_whitespace) {
                    Some(self.eat(TokenKind::LessThan))
                } else {
                    unimplemented!()
                }
            }
            Some('>') => {
                if self.peek().is_some_and(|it| it == '=') {
                    Some(self.eat(TokenKind::GreaterThanEq))
                } else if self.peek().is_some_and(char::is_whitespace) {
                    Some(self.eat(TokenKind::GreaterThan))
                } else {
                    unimplemented!()
                }
            }
            Some('[') => Some(self.eat(TokenKind::LeftSqBracket)),
            Some(']') => Some(self.eat(TokenKind::RightSqBracket)),
            Some(ch) if ch.is_ascii() => {
                if self.prev() == '[' {
                    Some(self.lex_ascii_until(']'))
                } else {
                    Some(self.lex_ascii())
                }
            }
            Some(_) => unimplemented!(),
            None => None,
        }
    }

    fn current(&self) -> Option<char> {
        self.source.chars().nth(self.cursor)
    }

    fn peek(&self) -> Option<char> {
        self.source.chars().nth(self.cursor + 1)
    }

    fn prev(&self) -> char {
        self.source
            .chars()
            .nth(self.prev_cursor)
            .expect("cursor starts at zero")
    }

    fn eat(&mut self, kind: TokenKind) -> Token {
        match kind {
            TokenKind::Star
            | TokenKind::Semicolon
            | TokenKind::Eq
            | TokenKind::Minus
            | TokenKind::LessThan
            | TokenKind::GreaterThan
            | TokenKind::LeftSqBracket
            | TokenKind::RightSqBracket => self.advance(1),
            TokenKind::LessThanEq | TokenKind::GreaterThanEq | TokenKind::NotEq => self.advance(2),
            _ => panic!("cannot eat kind"),
        }

        Token { kind, value: None }
    }

    /// Advance the cursor by some `n` positions.
    fn advance(&mut self, n: usize) {
        self.prev_cursor = self.cursor;
        self.cursor += n
    }

    fn skip_whitespace(&mut self) {
        while let Some(ch) = self.current() {
            if ch.is_whitespace() {
                self.advance(1);
            } else {
                break;
            }
        }
    }

    fn lex_ascii(&mut self) -> Token {
        if self.current().is_some_and(char::is_numeric) {
            return self.lex_number();
        }

        let start = self.cursor;

        while let Some(ch) = self.current() {
            if ch.is_whitespace() || !ch.is_ascii() | is_closing_char(ch) {
                break;
            } else {
                self.advance(1);
            }
        }

        keyword_or_word_token(&self.source[start..self.cursor])
    }

    fn lex_ascii_until(&mut self, c: char) -> Token {
        let start = self.cursor;

        while let Some(ch) = self.current() {
            if ch.is_whitespace() || !ch.is_ascii() || ch == c {
                break;
            } else {
                self.advance(1);
            }
        }

        keyword_or_word_token(&self.source[start..self.cursor])
    }

    fn lex_number(&mut self) -> Token {
        let start = self.cursor;

        if self.current().is_some_and(|it| matches!(it, '-' | '+')) {
            self.advance(1)
        }

        let mut has_decimal_point = false;

        while let Some(ch) = self.current() {
            if ch.is_numeric() {
                self.advance(1);
            } else if ch == '.' {
                if !has_decimal_point {
                    has_decimal_point = true;
                    self.advance(1);
                } else {
                    unimplemented!()
                }
            } else {
                break;
            }
        }

        if has_decimal_point {
            Token {
                kind: TokenKind::Float,
                value: Some(TokenValue::Float(
                    self.source[start..self.cursor]
                        .parse::<f64>()
                        .expect("f64 from str"),
                )),
            }
        } else {
            Token {
                kind: TokenKind::Int,
                value: Some(TokenValue::Int(
                    self.source[start..self.cursor]
                        .parse::<i64>()
                        .expect("i64 from str"),
                )),
            }
        }
    }

    fn lex_string(&mut self) -> Token {
        // Assume current is opening '
        self.advance(1);

        let start = self.cursor;

        while let Some(ch) = self.current() {
            if ch != '\'' {
                self.advance(1);
            } else {
                break;
            }
        }

        let end = self.cursor;

        // Skip closing '
        self.advance(1);

        Token {
            kind: TokenKind::String,
            value: Some(TokenValue::String(self.source[start..end].into())),
        }
    }
}

fn is_closing_char(ch: char) -> bool {
    matches!(ch, ']' | ';')
}

fn keyword_or_word_token(s: &str) -> Token {
    if s.eq_ignore_ascii_case("select") {
        Token {
            kind: TokenKind::Select,
            value: None,
        }
    } else if s.eq_ignore_ascii_case("from") {
        Token {
            kind: TokenKind::From,
            value: None,
        }
    } else if s.eq_ignore_ascii_case("table") {
        Token {
            kind: TokenKind::Table,
            value: None,
        }
    } else if s.eq_ignore_ascii_case("where") {
        Token {
            kind: TokenKind::Where,
            value: None,
        }
    } else if s.eq_ignore_ascii_case("column") {
        Token {
            kind: TokenKind::Column,
            value: None,
        }
    } else if s.eq_ignore_ascii_case("is") {
        Token {
            kind: TokenKind::Is,
            value: None,
        }
    } else if s.eq_ignore_ascii_case("null") {
        Token {
            kind: TokenKind::Null,
            value: None,
        }
    } else if s.eq_ignore_ascii_case("and") {
        Token {
            kind: TokenKind::And,
            value: None,
        }
    } else {
        Token {
            kind: TokenKind::Word,
            value: Some(TokenValue::Word(s.into())),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Token {
    kind: TokenKind,
    value: Option<TokenValue>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum TokenKind {
    Word,
    Select,
    From,
    Table,
    Star,
    Where,
    Column,
    Is,
    Null,
    Semicolon,
    Float,
    Int,
    And,
    Eq,
    Minus,
    String,
    LessThan,
    LessThanEq,
    GreaterThan,
    GreaterThanEq,
    LeftSqBracket,
    RightSqBracket,
    NotEq,
}

#[derive(Debug, Clone, PartialEq)]
enum TokenValue {
    Word(Box<str>),
    Int(i64),
    Float(f64),
    String(Box<str>),
}

impl Eq for TokenValue {}

impl PartialOrd for TokenValue {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for TokenValue {
    fn cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(other).unwrap_or(Ordering::Equal)
    }
}

#[test]
fn test_simple_select_statement() {
    let source = "select * from table;";
    let mut parser = Parser::new(source);
    let tokens = parser.parse();
    assert_eq!(
        tokens,
        vec![
            Token {
                kind: TokenKind::Select,
                value: None
            },
            Token {
                kind: TokenKind::Star,
                value: None
            },
            Token {
                kind: TokenKind::From,
                value: None
            },
            Token {
                kind: TokenKind::Table,
                value: None
            },
            Token {
                kind: TokenKind::Semicolon,
                value: None
            }
        ]
    )
}

#[test]
fn test_simple_select_where_eq_and_less_than_eq_int() {
    let source = "select * from table where col1 = 1 and col2 <= 4";
    let mut parser = Parser::new(source);
    let tokens = parser.parse();
    assert_eq!(
        tokens,
        vec![
            Token {
                kind: TokenKind::Select,
                value: None
            },
            Token {
                kind: TokenKind::Star,
                value: None
            },
            Token {
                kind: TokenKind::From,
                value: None
            },
            Token {
                kind: TokenKind::Table,
                value: None
            },
            Token {
                kind: TokenKind::Where,
                value: None
            },
            Token {
                kind: TokenKind::Word,
                value: Some(TokenValue::Word("col1".into()))
            },
            Token {
                kind: TokenKind::Eq,
                value: None
            },
            Token {
                kind: TokenKind::Int,
                value: Some(TokenValue::Int(1)),
            },
            Token {
                kind: TokenKind::And,
                value: None,
            },
            Token {
                kind: TokenKind::Word,
                value: Some(TokenValue::Word("col2".into()))
            },
            Token {
                kind: TokenKind::LessThanEq,
                value: None
            },
            Token {
                kind: TokenKind::Int,
                value: Some(TokenValue::Int(4))
            }
        ]
    )
}

#[test]
fn test_simple_where_eq_string_and_eq_float() {
    let source = "select * from word where column = 'string' and another_word = -0.1;";
    let mut parser = Parser::new(source);

    let tokens = parser.parse();

    assert_eq!(
        tokens,
        vec![
            Token {
                kind: TokenKind::Select,
                value: None,
            },
            Token {
                kind: TokenKind::Star,
                value: None,
            },
            Token {
                kind: TokenKind::From,
                value: None,
            },
            Token {
                kind: TokenKind::Word,
                value: Some(TokenValue::Word("word".into())),
            },
            Token {
                kind: TokenKind::Where,
                value: None,
            },
            Token {
                kind: TokenKind::Column,
                value: None,
            },
            Token {
                kind: TokenKind::Eq,
                value: None,
            },
            Token {
                kind: TokenKind::String,
                value: Some(TokenValue::String("string".into())),
            },
            Token {
                kind: TokenKind::And,
                value: None,
            },
            Token {
                kind: TokenKind::Word,
                value: Some(TokenValue::Word("another_word".into())),
            },
            Token {
                kind: TokenKind::Eq,
                value: None,
            },
            Token {
                kind: TokenKind::Float,
                value: Some(TokenValue::Float(-0.1)),
            },
            Token {
                kind: TokenKind::Semicolon,
                value: None,
            },
        ]
    );
}

#[test]
fn test_simple_select_where_bracket_not_eq() {
    let source = "select * from [word] where [col] != 5";
    let mut parser = Parser::new(source);

    let tokens = parser.parse();

    assert_eq!(
        tokens,
        vec![
            Token {
                kind: TokenKind::Select,
                value: None,
            },
            Token {
                kind: TokenKind::Star,
                value: None,
            },
            Token {
                kind: TokenKind::From,
                value: None,
            },
            Token {
                kind: TokenKind::LeftSqBracket,
                value: None,
            },
            Token {
                kind: TokenKind::Word,
                value: Some(TokenValue::Word("word".into())),
            },
            Token {
                kind: TokenKind::RightSqBracket,
                value: None,
            },
            Token {
                kind: TokenKind::Where,
                value: None,
            },
            Token {
                kind: TokenKind::LeftSqBracket,
                value: None,
            },
            Token {
                kind: TokenKind::Word,
                value: Some(TokenValue::Word("col".into())),
            },
            Token {
                kind: TokenKind::RightSqBracket,
                value: None,
            },
            Token {
                kind: TokenKind::NotEq,
                value: None,
            },
            Token {
                kind: TokenKind::Int,
                value: Some(TokenValue::Int(5)),
            },
        ]
    );
}

#[test]
fn test_simple_select_where_bracket_greater_than_eq_and_bracket_less_than_float() {
    let source = "SeLeCt * from [word] where [col] >= 5 and [col] < 100.2";
    let mut parser = Parser::new(source);

    let tokens = parser.parse();

    assert_eq!(
        tokens,
        vec![
            Token {
                kind: TokenKind::Select,
                value: None,
            },
            Token {
                kind: TokenKind::Star,
                value: None,
            },
            Token {
                kind: TokenKind::From,
                value: None,
            },
            Token {
                kind: TokenKind::LeftSqBracket,
                value: None,
            },
            Token {
                kind: TokenKind::Word,
                value: Some(TokenValue::Word("word".into())),
            },
            Token {
                kind: TokenKind::RightSqBracket,
                value: None,
            },
            Token {
                kind: TokenKind::Where,
                value: None,
            },
            Token {
                kind: TokenKind::LeftSqBracket,
                value: None,
            },
            Token {
                kind: TokenKind::Word,
                value: Some(TokenValue::Word("col".into())),
            },
            Token {
                kind: TokenKind::RightSqBracket,
                value: None,
            },
            Token {
                kind: TokenKind::GreaterThanEq,
                value: None,
            },
            Token {
                kind: TokenKind::Int,
                value: Some(TokenValue::Int(5)),
            },
            Token {
                kind: TokenKind::And,
                value: None,
            },
            Token {
                kind: TokenKind::LeftSqBracket,
                value: None,
            },
            Token {
                kind: TokenKind::Word,
                value: Some(TokenValue::Word("col".into())),
            },
            Token {
                kind: TokenKind::RightSqBracket,
                value: None,
            },
            Token {
                kind: TokenKind::LessThan,
                value: None
            },
            Token {
                kind: TokenKind::Float,
                value: Some(TokenValue::Float(100.2)),
            },
        ]
    );
}
