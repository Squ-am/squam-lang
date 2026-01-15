use logos::Logos;
use crate::{Span, Token, TokenKind};

/// The Squam lexer.
///
/// Converts source code into a stream of tokens.
pub struct Lexer<'src> {
    inner: logos::Lexer<'src, TokenKind>,
    source: &'src str,
    file_id: u16,
    peeked: Option<Token>,
    /// Track if we've returned EOF
    at_eof: bool,
}

impl<'src> Lexer<'src> {
    /// Create a new lexer for the given source code.
    pub fn new(source: &'src str, file_id: u16) -> Self {
        Self {
            inner: TokenKind::lexer(source),
            source,
            file_id,
            peeked: None,
            at_eof: false,
        }
    }

    /// Get the source code being lexed.
    pub fn source(&self) -> &'src str {
        self.source
    }

    /// Get the file ID for this lexer.
    pub fn file_id(&self) -> u16 {
        self.file_id
    }

    /// Get the next token from the source.
    pub fn next_token(&mut self) -> Token {
        if let Some(token) = self.peeked.take() {
            return token;
        }

        if self.at_eof {
            return Token::new(
                TokenKind::Eof,
                Span::new(self.source.len(), self.source.len(), self.file_id),
            );
        }

        match self.inner.next() {
            Some(Ok(kind)) => {
                let span = self.inner.span();
                Token::new(TokenKind::from(kind), Span::new(span.start, span.end, self.file_id))
            }
            Some(Err(())) => {
                let span = self.inner.span();
                Token::new(TokenKind::Error, Span::new(span.start, span.end, self.file_id))
            }
            None => {
                self.at_eof = true;
                Token::new(
                    TokenKind::Eof,
                    Span::new(self.source.len(), self.source.len(), self.file_id),
                )
            }
        }
    }

    /// Peek at the next token without consuming it.
    pub fn peek(&mut self) -> &Token {
        if self.peeked.is_none() {
            self.peeked = Some(self.next_token());
        }
        self.peeked.as_ref().unwrap()
    }

    /// Check if the next token is of the given kind.
    pub fn check(&mut self, kind: TokenKind) -> bool {
        self.peek().kind == kind
    }

    /// Get the slice of source code for a span.
    pub fn slice(&self, span: Span) -> &'src str {
        &self.source[span.start as usize..span.end as usize]
    }

    /// Get the slice for the current token in the inner lexer.
    pub fn current_slice(&self) -> &'src str {
        self.inner.slice()
    }

    /// Collect all tokens into a vector.
    pub fn collect_all(&mut self) -> Vec<Token> {
        let mut tokens = Vec::new();
        loop {
            let token = self.next_token();
            let is_eof = token.kind == TokenKind::Eof;
            tokens.push(token);
            if is_eof {
                break;
            }
        }
        tokens
    }
}

impl<'src> Iterator for Lexer<'src> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let token = self.next_token();
        if token.kind == TokenKind::Eof && self.at_eof {
            // Return None after the first EOF
            if self.peeked.is_none() {
                return None;
            }
        }
        Some(token)
    }
}

/// Parse an integer literal, handling different bases.
pub fn parse_int(s: &str) -> Result<i64, std::num::ParseIntError> {
    let s = s.replace('_', "");
    if s.starts_with("0x") || s.starts_with("0X") {
        i64::from_str_radix(&s[2..], 16)
    } else if s.starts_with("0b") || s.starts_with("0B") {
        i64::from_str_radix(&s[2..], 2)
    } else if s.starts_with("0o") || s.starts_with("0O") {
        i64::from_str_radix(&s[2..], 8)
    } else {
        s.parse()
    }
}

/// Parse a float literal.
pub fn parse_float(s: &str) -> Result<f64, std::num::ParseFloatError> {
    s.replace('_', "").parse()
}

/// Parse a string literal, handling escape sequences.
pub fn parse_string(s: &str) -> Result<String, StringParseError> {
    // Remove surrounding quotes
    let s = &s[1..s.len() - 1];
    let mut result = String::with_capacity(s.len());
    let mut chars = s.chars().peekable();

    while let Some(c) = chars.next() {
        if c == '\\' {
            match chars.next() {
                Some('n') => result.push('\n'),
                Some('r') => result.push('\r'),
                Some('t') => result.push('\t'),
                Some('\\') => result.push('\\'),
                Some('0') => result.push('\0'),
                Some('"') => result.push('"'),
                Some('\'') => result.push('\''),
                Some('x') => {
                    // Hex escape: \xNN
                    let mut hex = String::new();
                    for _ in 0..2 {
                        match chars.next() {
                            Some(h) if h.is_ascii_hexdigit() => hex.push(h),
                            _ => return Err(StringParseError::InvalidHexEscape),
                        }
                    }
                    let code = u8::from_str_radix(&hex, 16)
                        .map_err(|_| StringParseError::InvalidHexEscape)?;
                    result.push(code as char);
                }
                Some('u') => {
                    // Unicode escape: \u{NNNN}
                    if chars.next() != Some('{') {
                        return Err(StringParseError::InvalidUnicodeEscape);
                    }
                    let mut hex = String::new();
                    loop {
                        match chars.next() {
                            Some('}') => break,
                            Some(h) if h.is_ascii_hexdigit() => hex.push(h),
                            _ => return Err(StringParseError::InvalidUnicodeEscape),
                        }
                    }
                    let code = u32::from_str_radix(&hex, 16)
                        .map_err(|_| StringParseError::InvalidUnicodeEscape)?;
                    let c = char::from_u32(code)
                        .ok_or(StringParseError::InvalidUnicodeEscape)?;
                    result.push(c);
                }
                Some(c) => return Err(StringParseError::InvalidEscape(c)),
                None => return Err(StringParseError::UnterminatedEscape),
            }
        } else {
            result.push(c);
        }
    }

    Ok(result)
}

/// Parse a character literal.
pub fn parse_char(s: &str) -> Result<char, StringParseError> {
    let inner = &s[1..s.len() - 1];
    let parsed = parse_string(&format!("\"{}\"", inner))?;
    let mut chars = parsed.chars();
    match (chars.next(), chars.next()) {
        (Some(c), None) => Ok(c),
        _ => Err(StringParseError::InvalidCharLiteral),
    }
}

/// Errors that can occur when parsing string/char literals.
#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum StringParseError {
    #[error("invalid escape sequence: \\{0}")]
    InvalidEscape(char),
    #[error("unterminated escape sequence")]
    UnterminatedEscape,
    #[error("invalid hex escape sequence")]
    InvalidHexEscape,
    #[error("invalid unicode escape sequence")]
    InvalidUnicodeEscape,
    #[error("invalid character literal")]
    InvalidCharLiteral,
}

#[cfg(test)]
mod tests {
    use super::*;

    fn lex(source: &str) -> Vec<TokenKind> {
        Lexer::new(source, 0)
            .collect_all()
            .into_iter()
            .map(|t| t.kind)
            .collect()
    }

    #[test]
    fn test_empty() {
        assert_eq!(lex(""), vec![TokenKind::Eof]);
    }

    #[test]
    fn test_whitespace() {
        assert_eq!(lex("   \t\n  "), vec![TokenKind::Eof]);
    }

    #[test]
    fn test_integers() {
        assert_eq!(lex("42"), vec![TokenKind::IntLiteral, TokenKind::Eof]);
        assert_eq!(lex("1_000_000"), vec![TokenKind::IntLiteral, TokenKind::Eof]);
        assert_eq!(lex("0xFF"), vec![TokenKind::IntLiteral, TokenKind::Eof]);
        assert_eq!(lex("0b1010"), vec![TokenKind::IntLiteral, TokenKind::Eof]);
        assert_eq!(lex("0o777"), vec![TokenKind::IntLiteral, TokenKind::Eof]);
    }

    #[test]
    fn test_floats() {
        assert_eq!(lex("3.14"), vec![TokenKind::FloatLiteral, TokenKind::Eof]);
        assert_eq!(lex("1.0e10"), vec![TokenKind::FloatLiteral, TokenKind::Eof]);
        assert_eq!(lex("2e5"), vec![TokenKind::FloatLiteral, TokenKind::Eof]);
    }

    #[test]
    fn test_strings() {
        assert_eq!(lex(r#""hello""#), vec![TokenKind::StringLiteral, TokenKind::Eof]);
        assert_eq!(lex(r#""with \"escapes\"""#), vec![TokenKind::StringLiteral, TokenKind::Eof]);
    }

    #[test]
    fn test_chars() {
        assert_eq!(lex("'a'"), vec![TokenKind::CharLiteral, TokenKind::Eof]);
        assert_eq!(lex("'\\n'"), vec![TokenKind::CharLiteral, TokenKind::Eof]);
    }

    #[test]
    fn test_keywords() {
        assert_eq!(lex("fn"), vec![TokenKind::Fn, TokenKind::Eof]);
        assert_eq!(lex("let"), vec![TokenKind::Let, TokenKind::Eof]);
        assert_eq!(lex("mut"), vec![TokenKind::Mut, TokenKind::Eof]);
        assert_eq!(lex("if else"), vec![TokenKind::If, TokenKind::Else, TokenKind::Eof]);
        assert_eq!(lex("struct enum impl trait"), vec![
            TokenKind::Struct, TokenKind::Enum, TokenKind::Impl, TokenKind::Trait, TokenKind::Eof
        ]);
    }

    #[test]
    fn test_identifiers() {
        assert_eq!(lex("foo"), vec![TokenKind::Identifier, TokenKind::Eof]);
        assert_eq!(lex("_bar"), vec![TokenKind::Identifier, TokenKind::Eof]);
        assert_eq!(lex("FooBar123"), vec![TokenKind::Identifier, TokenKind::Eof]);
    }

    #[test]
    fn test_operators() {
        assert_eq!(lex("+ - * / %"), vec![
            TokenKind::Plus, TokenKind::Minus, TokenKind::Star, TokenKind::Slash, TokenKind::Percent, TokenKind::Eof
        ]);
        assert_eq!(lex("== != < <= > >="), vec![
            TokenKind::EqEq, TokenKind::BangEq, TokenKind::Lt, TokenKind::LtEq, TokenKind::Gt, TokenKind::GtEq, TokenKind::Eof
        ]);
        assert_eq!(lex("&& || !"), vec![
            TokenKind::AndAnd, TokenKind::OrOr, TokenKind::Bang, TokenKind::Eof
        ]);
    }

    #[test]
    fn test_punctuation() {
        assert_eq!(lex("( ) [ ] { }"), vec![
            TokenKind::LParen, TokenKind::RParen, TokenKind::LBracket, TokenKind::RBracket,
            TokenKind::LBrace, TokenKind::RBrace, TokenKind::Eof
        ]);
        assert_eq!(lex(":: -> =>"), vec![
            TokenKind::ColonColon, TokenKind::Arrow, TokenKind::FatArrow, TokenKind::Eof
        ]);
        assert_eq!(lex(".. ..="), vec![
            TokenKind::DotDot, TokenKind::DotDotEq, TokenKind::Eof
        ]);
    }

    #[test]
    fn test_comments() {
        assert_eq!(lex("// comment\n42"), vec![TokenKind::IntLiteral, TokenKind::Eof]);
        assert_eq!(lex("/* block */ 42"), vec![TokenKind::IntLiteral, TokenKind::Eof]);
    }

    #[test]
    fn test_function() {
        let tokens = lex("fn add(a: i64, b: i64) -> i64 { a + b }");
        assert_eq!(tokens, vec![
            TokenKind::Fn,
            TokenKind::Identifier, // add
            TokenKind::LParen,
            TokenKind::Identifier, // a
            TokenKind::Colon,
            TokenKind::Identifier, // i64
            TokenKind::Comma,
            TokenKind::Identifier, // b
            TokenKind::Colon,
            TokenKind::Identifier, // i64
            TokenKind::RParen,
            TokenKind::Arrow,
            TokenKind::Identifier, // i64
            TokenKind::LBrace,
            TokenKind::Identifier, // a
            TokenKind::Plus,
            TokenKind::Identifier, // b
            TokenKind::RBrace,
            TokenKind::Eof,
        ]);
    }

    #[test]
    fn test_parse_int() {
        assert_eq!(parse_int("42"), Ok(42));
        assert_eq!(parse_int("1_000"), Ok(1000));
        assert_eq!(parse_int("0xFF"), Ok(255));
        assert_eq!(parse_int("0b1010"), Ok(10));
        assert_eq!(parse_int("0o77"), Ok(63));
    }

    #[test]
    fn test_parse_string() {
        assert_eq!(parse_string(r#""hello""#), Ok("hello".to_string()));
        assert_eq!(parse_string(r#""with\nnewline""#), Ok("with\nnewline".to_string()));
        assert_eq!(parse_string(r#""tab\there""#), Ok("tab\there".to_string()));
        assert_eq!(parse_string(r#""quote\"here""#), Ok("quote\"here".to_string()));
        assert_eq!(parse_string(r#""\x41""#), Ok("A".to_string()));
    }

    #[test]
    fn test_parse_char() {
        assert_eq!(parse_char("'a'"), Ok('a'));
        assert_eq!(parse_char("'\\n'"), Ok('\n'));
        assert_eq!(parse_char("'\\x41'"), Ok('A'));
    }
}