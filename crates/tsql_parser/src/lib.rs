#![allow(dead_code)]

use std::cmp::Ordering;

/// An experimental TSQL [`Parser`].
///
/// This [`Parser`] is inspired by [ruff], [sqlparser-rs], and the [Pratt parsing algorithm].
///
/// [ruff]: https://github.com/astral-sh/ruff
/// [sqlparser-rs]: https://github.com/sqlparser-rs/sqlparser-rs
/// [Pratt parsing algorithm]: https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html
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
            tokens.push(token);
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
            Some(',') => Some(self.eat(TokenKind::Comma)),
            Some('.') => Some(self.eat(TokenKind::Dot)),
            Some('@') => Some(self.eat(TokenKind::At)),
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
            Some('(') => Some(self.eat(TokenKind::LeftParen)),
            Some(')') => Some(self.eat(TokenKind::RightParen)),
            Some(ch) if ch.is_identifier_start() => Some(self.lex_identifier()),
            Some(ch) if ch.is_ascii_digit() => Some(self.lex_number()),
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
            | TokenKind::RightSqBracket
            | TokenKind::LeftParen
            | TokenKind::RightParen
            | TokenKind::Comma
            | TokenKind::Dot
            | TokenKind::At => self.advance(1),
            TokenKind::LessThanEq | TokenKind::GreaterThanEq | TokenKind::NotEq => self.advance(2),
            _ => panic!("cannot eat kind"),
        }

        Token { kind, value: None }
    }

    /// Advance the cursor by some `n` positions.
    fn advance(&mut self, n: usize) {
        self.prev_cursor = self.cursor;
        self.cursor += n;
    }

    fn skip_whitespace(&mut self) {
        while let Some(ch) = self.current() {
            match ch {
                ' ' | '\t' | '\\' | '\r' | '\n' => self.advance(ch.len_utf8()),
                _ => break,
            }
        }
    }

    fn lex_number(&mut self) -> Token {
        let start = self.cursor;

        if self.current().is_some_and(|it| matches!(it, '-' | '+')) {
            self.advance(1);
        }

        let mut has_decimal_point = false;

        while let Some(ch) = self.current() {
            if ch.is_numeric() {
                self.advance(1);
            } else if ch == '.' {
                if has_decimal_point {
                    unimplemented!()
                } else {
                    has_decimal_point = true;
                    self.advance(1);
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
            if ch == '\'' {
                break;
            }
            self.advance(1);
        }

        let end = self.cursor;

        // Skip closing '
        self.advance(1);

        Token {
            kind: TokenKind::String,
            value: Some(TokenValue::String(self.source[start..end].into())),
        }
    }

    fn lex_identifier(&mut self) -> Token {
        let start = self.cursor;
        while let Some(ch) = self.current() {
            if !ch.is_identifier_continue() {
                break;
            }
            self.advance(1);
        }

        let identifier = &self.source[start..self.cursor];
        if let Some(keyword) = lookup_keyword(identifier) {
            return Token {
                kind: TokenKind::Keyword(keyword),
                value: None,
            };
        }

        Token {
            kind: TokenKind::Word,
            value: Some(TokenValue::Word(identifier.into())),
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
    Keyword(Keyword),
    Word,
    Star,
    Semicolon,
    Float,
    Int,
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
    LeftParen,
    RightParen,
    Comma,
    Dot,
    At,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Keyword {
    Select,
    From,
    Where,
    And,
    Join,
    Right,
    Left,
    As,
    On,
    In,
    Count,
    Distinct,
}

/// Checks if a string is a keyword, ignoring case.
#[must_use]
pub fn lookup_keyword(s: &str) -> Option<Keyword> {
    match s.to_ascii_uppercase().as_str() {
        "SELECT" => Some(Keyword::Select),
        "FROM" => Some(Keyword::From),
        "WHERE" => Some(Keyword::Where),
        "AND" => Some(Keyword::And),
        "JOIN" => Some(Keyword::Join),
        "RIGHT" => Some(Keyword::Right),
        "LEFT" => Some(Keyword::Left),
        "AS" => Some(Keyword::As),
        "ON" => Some(Keyword::On),
        "IN" => Some(Keyword::In),
        "COUNT" => Some(Keyword::Count),
        "DISTINCT" => Some(Keyword::Distinct),
        _ => None,
    }
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

pub trait CharExt {
    /// true if this char is a valid start of an identifier (ASCII letter or `_`)
    fn is_identifier_start(&self) -> bool;
    /// true if this char is a valid continuation of an identifier (ASCII letter, digit, or `_`)
    fn is_identifier_continue(&self) -> bool;
}

impl CharExt for char {
    #[inline]
    fn is_identifier_start(&self) -> bool {
        self.is_ascii_alphabetic() || *self == '_'
    }

    #[inline]
    fn is_identifier_continue(&self) -> bool {
        self.is_ascii_alphanumeric() || *self == '_'
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[allow(clippy::too_many_lines)]
    #[test]
    fn test_parse_tokens() {
        // Import variants to make the assertion more concise.
        use super::Keyword::*;
        use super::TokenKind::*;

        let source = r"
            select
                'literal',
                [column1],
                column2 as alias1,
                count(distinct column3) as alias2
            from table1
            join table2 on (table1.id = table2.id)
            left join table3 on (table2.id = table3.id)
            where
                table1.column1 = 'string'
                and table2.column1 in (@variable1)
        ";

        let tokens = Parser::new(source).parse();

        assert_eq!(
            tokens,
            vec![
                Token {
                    kind: Keyword(Select),
                    value: None
                },
                Token {
                    kind: String,
                    value: Some(TokenValue::String("literal".into()))
                },
                Token {
                    kind: Comma,
                    value: None
                },
                Token {
                    kind: LeftSqBracket,
                    value: None
                },
                Token {
                    kind: Word,
                    value: Some(TokenValue::Word("column1".into()))
                },
                Token {
                    kind: RightSqBracket,
                    value: None
                },
                Token {
                    kind: Comma,
                    value: None
                },
                Token {
                    kind: Word,
                    value: Some(TokenValue::Word("column2".into()))
                },
                Token {
                    kind: Keyword(As),
                    value: None
                },
                Token {
                    kind: Word,
                    value: Some(TokenValue::Word("alias1".into()))
                },
                Token {
                    kind: Comma,
                    value: None
                },
                Token {
                    kind: Keyword(Count),
                    value: None
                },
                Token {
                    kind: LeftParen,
                    value: None
                },
                Token {
                    kind: Keyword(Distinct),
                    value: None
                },
                Token {
                    kind: Word,
                    value: Some(TokenValue::Word("column3".into()))
                },
                Token {
                    kind: RightParen,
                    value: None
                },
                Token {
                    kind: Keyword(As),
                    value: None
                },
                Token {
                    kind: Word,
                    value: Some(TokenValue::Word("alias2".into()))
                },
                Token {
                    kind: Keyword(From),
                    value: None
                },
                Token {
                    kind: Word,
                    value: Some(TokenValue::Word("table1".into()))
                },
                Token {
                    kind: Keyword(Join),
                    value: None
                },
                Token {
                    kind: Word,
                    value: Some(TokenValue::Word("table2".into()))
                },
                Token {
                    kind: Keyword(On),
                    value: None
                },
                Token {
                    kind: LeftParen,
                    value: None
                },
                Token {
                    kind: Word,
                    value: Some(TokenValue::Word("table1".into()))
                },
                Token {
                    kind: Dot,
                    value: None
                },
                Token {
                    kind: Word,
                    value: Some(TokenValue::Word("id".into()))
                },
                Token {
                    kind: Eq,
                    value: None
                },
                Token {
                    kind: Word,
                    value: Some(TokenValue::Word("table2".into()))
                },
                Token {
                    kind: Dot,
                    value: None
                },
                Token {
                    kind: Word,
                    value: Some(TokenValue::Word("id".into()))
                },
                Token {
                    kind: RightParen,
                    value: None
                },
                Token {
                    kind: Keyword(Left),
                    value: None
                },
                Token {
                    kind: Keyword(Join),
                    value: None
                },
                Token {
                    kind: Word,
                    value: Some(TokenValue::Word("table3".into()))
                },
                Token {
                    kind: Keyword(On),
                    value: None
                },
                Token {
                    kind: LeftParen,
                    value: None
                },
                Token {
                    kind: Word,
                    value: Some(TokenValue::Word("table2".into()))
                },
                Token {
                    kind: Dot,
                    value: None
                },
                Token {
                    kind: Word,
                    value: Some(TokenValue::Word("id".into()))
                },
                Token {
                    kind: Eq,
                    value: None
                },
                Token {
                    kind: Word,
                    value: Some(TokenValue::Word("table3".into()))
                },
                Token {
                    kind: Dot,
                    value: None
                },
                Token {
                    kind: Word,
                    value: Some(TokenValue::Word("id".into()))
                },
                Token {
                    kind: RightParen,
                    value: None
                },
                Token {
                    kind: Keyword(Where),
                    value: None
                },
                Token {
                    kind: Word,
                    value: Some(TokenValue::Word("table1".into()))
                },
                Token {
                    kind: Dot,
                    value: None
                },
                Token {
                    kind: Word,
                    value: Some(TokenValue::Word("column1".into()))
                },
                Token {
                    kind: Eq,
                    value: None
                },
                Token {
                    kind: String,
                    value: Some(TokenValue::String("string".into()))
                },
                Token {
                    kind: Keyword(And),
                    value: None
                },
                Token {
                    kind: Word,
                    value: Some(TokenValue::Word("table2".into()))
                },
                Token {
                    kind: Dot,
                    value: None
                },
                Token {
                    kind: Word,
                    value: Some(TokenValue::Word("column1".into()))
                },
                Token {
                    kind: Keyword(In),
                    value: None
                },
                Token {
                    kind: LeftParen,
                    value: None
                },
                Token {
                    kind: At,
                    value: None
                },
                Token {
                    kind: Word,
                    value: Some(TokenValue::Word("variable1".into()))
                },
                Token {
                    kind: RightParen,
                    value: None
                },
            ]
        );
    }
}
