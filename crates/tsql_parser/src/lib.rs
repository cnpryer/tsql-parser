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

        match self.source.chars().nth(self.cursor) {
            Some(ch) if ch.is_ascii_alphabetic() => Some(self.parse_keyword()),
            Some('*') => {
                self.cursor += 1;
                Some(Token {
                    kind: TokenKind::Star,
                })
            }
            Some(';') => {
                self.cursor += 1;
                Some(Token {
                    kind: TokenKind::Semicolon,
                })
            }
            Some(_) => unimplemented!(),
            None => None,
        }
    }

    fn skip_whitespace(&mut self) {
        while let Some(ch) = self.source.chars().nth(self.cursor) {
            if ch.is_whitespace() {
                self.cursor += 1;
            } else {
                break;
            }
        }
    }

    /// Parse a keyword as [`Token`].
    fn parse_keyword(&mut self) -> Token {
        let start = self.cursor;

        while let Some(ch) = self.source.chars().nth(self.cursor) {
            if ch.is_ascii_alphanumeric() {
                self.cursor += 1;
            } else {
                break;
            }
        }

        match &self.source[start..self.cursor] {
            "select" => Token {
                kind: TokenKind::Select,
            },
            "from" => Token {
                kind: TokenKind::From,
            },
            "table" => Token {
                kind: TokenKind::Table,
            },
            _ => unimplemented!(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Token {
    kind: TokenKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum TokenKind {
    Select,
    From,
    Table,
    Star,
    Semicolon,
}

#[test]
fn test_parser_works() {
    let source = "select * from table;";
    let mut parser = Parser::new(source);

    let tokens = parser.parse();

    assert_eq!(
        tokens,
        vec![
            Token {
                kind: TokenKind::Select
            },
            Token {
                kind: TokenKind::Star
            },
            Token {
                kind: TokenKind::From
            },
            Token {
                kind: TokenKind::Table
            },
            Token {
                kind: TokenKind::Semicolon
            },
        ]
    );
}
