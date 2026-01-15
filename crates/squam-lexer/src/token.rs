use logos::Logos;
use crate::Span;

/// All token types in Squam.
#[derive(Logos, Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[logos(skip r"[ \t\r\n\f]+")]
pub enum TokenKind {
    // LITERALS
    /// Integer literal: 123, 1_000, 0xFF, 0b1010, 0o777
    #[regex(r"0[xX][0-9a-fA-F][0-9a-fA-F_]*", priority = 3)]
    #[regex(r"0[bB][01][01_]*", priority = 3)]
    #[regex(r"0[oO][0-7][0-7_]*", priority = 3)]
    #[regex(r"[0-9][0-9_]*", priority = 2)]
    IntLiteral,

    /// Float literal: 1.0, 3.14, 1e10, 2.5e-3
    #[regex(r"[0-9][0-9_]*\.[0-9][0-9_]*([eE][+-]?[0-9]+)?")]
    #[regex(r"[0-9][0-9_]*[eE][+-]?[0-9]+")]
    FloatLiteral,

    /// String literal: "hello", "with \"escapes\""
    #[regex(r#""([^"\\]|\\.)*""#)]
    StringLiteral,

    /// Character literal: 'a', '\n', '\x41'
    #[regex(r"'([^'\\]|\\.)'")]
    CharLiteral,

    /// Boolean true
    #[token("true")]
    True,

    /// Boolean false
    #[token("false")]
    False,

    // KEYWORDS
    #[token("fn")]
    Fn,
    #[token("let")]
    Let,
    #[token("mut")]
    Mut,
    #[token("const")]
    Const,
    #[token("if")]
    If,
    #[token("else")]
    Else,
    #[token("match")]
    Match,
    #[token("loop")]
    Loop,
    #[token("while")]
    While,
    #[token("for")]
    For,
    #[token("in")]
    In,
    #[token("break")]
    Break,
    #[token("continue")]
    Continue,
    #[token("return")]
    Return,
    #[token("struct")]
    Struct,
    #[token("enum")]
    Enum,
    #[token("impl")]
    Impl,
    #[token("trait")]
    Trait,
    #[token("type")]
    Type,
    #[token("pub")]
    Pub,
    #[token("mod")]
    Mod,
    #[token("use")]
    Use,
    #[token("as")]
    As,
    #[token("self")]
    SelfLower,
    #[token("Self")]
    SelfUpper,
    #[token("ref")]
    Ref,
    #[token("move")]
    Move,
    #[token("where")]
    Where,

    // IDENTIFIER
    /// Identifier: foo, _bar, FooBar123
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*")]
    Identifier,

    // OPERATORS
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,
    #[token("%")]
    Percent,
    #[token("==")]
    EqEq,
    #[token("!=")]
    BangEq,
    #[token("<")]
    Lt,
    #[token("<=")]
    LtEq,
    #[token(">")]
    Gt,
    #[token(">=")]
    GtEq,
    #[token("&&")]
    AndAnd,
    #[token("||")]
    OrOr,
    #[token("!")]
    Bang,
    #[token("&")]
    And,
    #[token("|")]
    Or,
    #[token("^")]
    Caret,
    #[token("~")]
    Tilde,
    #[token("<<")]
    Shl,
    #[token(">>")]
    Shr,
    #[token("=")]
    Eq,
    #[token("+=")]
    PlusEq,
    #[token("-=")]
    MinusEq,
    #[token("*=")]
    StarEq,
    #[token("/=")]
    SlashEq,
    #[token("%=")]
    PercentEq,
    #[token("&=")]
    AndEq,
    #[token("|=")]
    OrEq,
    #[token("^=")]
    CaretEq,
    #[token("<<=")]
    ShlEq,
    #[token(">>=")]
    ShrEq,

    // PUNCTUATION
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("[")]
    LBracket,
    #[token("]")]
    RBracket,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[token(",")]
    Comma,
    #[token(";")]
    Semicolon,
    #[token(":")]
    Colon,
    #[token("::")]
    ColonColon,
    #[token(".")]
    Dot,
    #[token("..")]
    DotDot,
    #[token("..=")]
    DotDotEq,
    #[token("...")]
    Ellipsis,
    #[token("->")]
    Arrow,
    #[token("=>")]
    FatArrow,
    #[token("?")]
    Question,
    #[token("@")]
    At,
    #[token("#")]
    Hash,

    // COMMENTS
    /// Line comment: // ...
    #[regex(r"//[^\n]*", logos::skip)]
    LineComment,

    /// Block comment: /* ... */
    #[regex(r"/\*([^*]|\*[^/])*\*/", logos::skip)]
    BlockComment,

    /// End of file
    Eof,

    /// Lexer error
    Error,
}

impl TokenKind {
    /// Returns true if this token is a keyword.
    pub fn is_keyword(&self) -> bool {
        matches!(
            self,
            TokenKind::Fn
                | TokenKind::Let
                | TokenKind::Mut
                | TokenKind::Const
                | TokenKind::If
                | TokenKind::Else
                | TokenKind::Match
                | TokenKind::Loop
                | TokenKind::While
                | TokenKind::For
                | TokenKind::In
                | TokenKind::Break
                | TokenKind::Continue
                | TokenKind::Return
                | TokenKind::Struct
                | TokenKind::Enum
                | TokenKind::Impl
                | TokenKind::Trait
                | TokenKind::Type
                | TokenKind::Pub
                | TokenKind::Mod
                | TokenKind::Use
                | TokenKind::As
                | TokenKind::SelfLower
                | TokenKind::SelfUpper
                | TokenKind::Ref
                | TokenKind::Move
                | TokenKind::Where
                | TokenKind::True
                | TokenKind::False
        )
    }

    /// Returns true if this token is a literal.
    pub fn is_literal(&self) -> bool {
        matches!(
            self,
            TokenKind::IntLiteral
                | TokenKind::FloatLiteral
                | TokenKind::StringLiteral
                | TokenKind::CharLiteral
                | TokenKind::True
                | TokenKind::False
        )
    }

    /// Returns true if this token can start an expression.
    pub fn can_start_expr(&self) -> bool {
        matches!(
            self,
            TokenKind::IntLiteral
                | TokenKind::FloatLiteral
                | TokenKind::StringLiteral
                | TokenKind::CharLiteral
                | TokenKind::True
                | TokenKind::False
                | TokenKind::Identifier
                | TokenKind::LParen
                | TokenKind::LBracket
                | TokenKind::LBrace
                | TokenKind::If
                | TokenKind::Match
                | TokenKind::Loop
                | TokenKind::While
                | TokenKind::For
                | TokenKind::Return
                | TokenKind::Break
                | TokenKind::Continue
                | TokenKind::Minus
                | TokenKind::Bang
                | TokenKind::Star
                | TokenKind::And
                | TokenKind::OrOr
                | TokenKind::SelfLower
                | TokenKind::SelfUpper
                | TokenKind::Move
        )
    }

    /// Get a human-readable name for this token kind.
    pub fn name(&self) -> &'static str {
        match self {
            TokenKind::IntLiteral => "integer",
            TokenKind::FloatLiteral => "float",
            TokenKind::StringLiteral => "string",
            TokenKind::CharLiteral => "character",
            TokenKind::True => "true",
            TokenKind::False => "false",
            TokenKind::Fn => "fn",
            TokenKind::Let => "let",
            TokenKind::Mut => "mut",
            TokenKind::Const => "const",
            TokenKind::If => "if",
            TokenKind::Else => "else",
            TokenKind::Match => "match",
            TokenKind::Loop => "loop",
            TokenKind::While => "while",
            TokenKind::For => "for",
            TokenKind::In => "in",
            TokenKind::Break => "break",
            TokenKind::Continue => "continue",
            TokenKind::Return => "return",
            TokenKind::Struct => "struct",
            TokenKind::Enum => "enum",
            TokenKind::Impl => "impl",
            TokenKind::Trait => "trait",
            TokenKind::Type => "type",
            TokenKind::Pub => "pub",
            TokenKind::Mod => "mod",
            TokenKind::Use => "use",
            TokenKind::As => "as",
            TokenKind::SelfLower => "self",
            TokenKind::SelfUpper => "Self",
            TokenKind::Ref => "ref",
            TokenKind::Move => "move",
            TokenKind::Where => "where",
            TokenKind::Identifier => "identifier",
            TokenKind::Plus => "+",
            TokenKind::Minus => "-",
            TokenKind::Star => "*",
            TokenKind::Slash => "/",
            TokenKind::Percent => "%",
            TokenKind::EqEq => "==",
            TokenKind::BangEq => "!=",
            TokenKind::Lt => "<",
            TokenKind::LtEq => "<=",
            TokenKind::Gt => ">",
            TokenKind::GtEq => ">=",
            TokenKind::AndAnd => "&&",
            TokenKind::OrOr => "||",
            TokenKind::Bang => "!",
            TokenKind::And => "&",
            TokenKind::Or => "|",
            TokenKind::Caret => "^",
            TokenKind::Tilde => "~",
            TokenKind::Shl => "<<",
            TokenKind::Shr => ">>",
            TokenKind::Eq => "=",
            TokenKind::PlusEq => "+=",
            TokenKind::MinusEq => "-=",
            TokenKind::StarEq => "*=",
            TokenKind::SlashEq => "/=",
            TokenKind::PercentEq => "%=",
            TokenKind::AndEq => "&=",
            TokenKind::OrEq => "|=",
            TokenKind::CaretEq => "^=",
            TokenKind::ShlEq => "<<=",
            TokenKind::ShrEq => ">>=",
            TokenKind::LParen => "(",
            TokenKind::RParen => ")",
            TokenKind::LBracket => "[",
            TokenKind::RBracket => "]",
            TokenKind::LBrace => "{",
            TokenKind::RBrace => "}",
            TokenKind::Comma => ",",
            TokenKind::Semicolon => ";",
            TokenKind::Colon => ":",
            TokenKind::ColonColon => "::",
            TokenKind::Dot => ".",
            TokenKind::DotDot => "..",
            TokenKind::DotDotEq => "..=",
            TokenKind::Ellipsis => "...",
            TokenKind::Arrow => "->",
            TokenKind::FatArrow => "=>",
            TokenKind::Question => "?",
            TokenKind::At => "@",
            TokenKind::Hash => "#",
            TokenKind::LineComment => "line comment",
            TokenKind::BlockComment => "block comment",
            TokenKind::Eof => "end of file",
            TokenKind::Error => "error",
        }
    }
}

impl std::fmt::Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name())
    }
}

/// A token with its kind and source location.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Token {
    pub fn new(kind: TokenKind, span: Span) -> Self {
        Self { kind, span }
    }

    pub fn dummy(kind: TokenKind) -> Self {
        Self {
            kind,
            span: Span::dummy(),
        }
    }
}