mod span;
mod token;
mod lexer;

pub use span::{Span, Spanned};
pub use token::{Token, TokenKind};
pub use lexer::{Lexer, parse_int, parse_float, parse_string, parse_char, StringParseError};