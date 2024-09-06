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
}

impl Lexer<'_> {
    fn new(source: &str) -> Lexer {
        Lexer { source, cursor: 0 }
    }

    fn next_token(&mut self) -> Option<Token> {
        self.skip_whitespace();

        match self.current() {
            Some('\'') => Some(self.lex_string()),
            Some('-') => {
                if self.peak().is_some_and(char::is_whitespace) {
                    self.cursor += 1;
                    Some(Token {
                        kind: TokenKind::Minus,
                        value: None,
                    })
                } else {
                    Some(self.lex_number())
                }
            }
            Some('=') => {
                self.cursor += 1;
                Some(Token {
                    kind: TokenKind::Eq,
                    value: None,
                })
            }
            Some('*') => {
                self.cursor += 1;
                Some(Token {
                    kind: TokenKind::Star,
                    value: None,
                })
            }
            Some(';') => {
                self.cursor += 1;
                Some(Token {
                    kind: TokenKind::Semicolon,
                    value: None,
                })
            }
            Some(ch) if ch.is_ascii() => Some(self.lex_ascii()),
            Some(_) => unimplemented!(),
            None => None,
        }
    }

    fn current(&self) -> Option<char> {
        self.source.chars().nth(self.cursor)
    }

    fn peak(&self) -> Option<char> {
        self.source.chars().nth(self.cursor + 1)
    }

    fn skip_whitespace(&mut self) {
        while let Some(ch) = self.current() {
            if ch.is_whitespace() {
                self.cursor += 1;
            } else {
                break;
            }
        }
    }

    fn lex_ascii(&mut self) -> Token {
        if self
            .source
            .chars()
            .nth(self.cursor)
            .is_some_and(char::is_numeric)
        {
            return self.lex_number();
        }

        let start = self.cursor;

        while let Some(ch) = self.current() {
            if ch.is_whitespace() | !ch.is_ascii() {
                break;
            } else {
                self.cursor += 1;
            }
        }

        match &self.source[start..self.cursor] {
            "select" => Token {
                kind: TokenKind::Select,
                value: None,
            },
            "from" => Token {
                kind: TokenKind::From,
                value: None,
            },
            "table" => Token {
                kind: TokenKind::Table,
                value: None,
            },
            "where" => Token {
                kind: TokenKind::Where,
                value: None,
            },
            "column" => Token {
                kind: TokenKind::Column,
                value: None,
            },
            "is" => Token {
                kind: TokenKind::Is,
                value: None,
            },
            "null" => Token {
                kind: TokenKind::Null,
                value: None,
            },
            "and" => Token {
                kind: TokenKind::And,
                value: None,
            },
            word => Token {
                kind: TokenKind::Word,
                value: Some(TokenValue::Word(word.into())),
            },
        }
    }

    fn lex_number(&mut self) -> Token {
        let start = self.cursor;

        if self.current().is_some_and(|it| matches!(it, '-' | '+')) {
            self.cursor += 1
        }

        let mut has_decimal_point = false;

        while let Some(ch) = self.source.chars().nth(self.cursor) {
            if ch.is_numeric() {
                self.cursor += 1;
            } else if ch == '.' {
                if !has_decimal_point {
                    has_decimal_point = true;
                    self.cursor += 1;
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
        self.cursor += 1;

        let start = self.cursor;

        while let Some(ch) = self.current() {
            if ch != '\'' {
                self.cursor += 1;
            } else {
                break;
            }
        }

        let end = self.cursor;

        // Skip closing '
        self.cursor += 1;

        Token {
            kind: TokenKind::String,
            value: Some(TokenValue::String(self.source[start..end].into())),
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
fn test_parser_works() {
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
