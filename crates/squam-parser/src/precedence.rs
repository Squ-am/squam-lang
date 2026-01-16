use crate::ast::BinaryOp;
use squam_lexer::TokenKind;

/// Operator precedence levels (higher = binds tighter).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[repr(u8)]
pub enum Precedence {
    /// Lowest precedence (for statements, etc.)
    Lowest = 0,
    /// Assignment: `=`, `+=`, `-=`, etc.
    Assignment = 1,
    /// Range: `..`, `..=`
    Range = 2,
    /// Logical or: `||`
    Or = 3,
    /// Logical and: `&&`
    And = 4,
    /// Comparison: `==`, `!=`, `<`, `<=`, `>`, `>=`
    Comparison = 5,
    /// Bitwise or: `|`
    BitOr = 6,
    /// Bitwise xor: `^`
    BitXor = 7,
    /// Bitwise and: `&`
    BitAnd = 8,
    /// Shift: `<<`, `>>`
    Shift = 9,
    /// Addition/subtraction: `+`, `-`
    Term = 10,
    /// Multiplication/division/remainder: `*`, `/`, `%`
    Factor = 11,
    /// Type cast: `as`
    Cast = 12,
    /// Unary operators: `-`, `!`, `~`, `&`, `*`
    Unary = 13,
    /// Try operator: `?`
    Try = 14,
    /// Call, index, field access: `()`, `[]`, `.`
    Postfix = 15,
}

impl Precedence {
    /// Get the precedence of a binary operator.
    pub fn of_binary(op: BinaryOp) -> Self {
        match op {
            BinaryOp::Or => Precedence::Or,
            BinaryOp::And => Precedence::And,
            BinaryOp::Eq
            | BinaryOp::Ne
            | BinaryOp::Lt
            | BinaryOp::Le
            | BinaryOp::Gt
            | BinaryOp::Ge => Precedence::Comparison,
            BinaryOp::BitOr => Precedence::BitOr,
            BinaryOp::BitXor => Precedence::BitXor,
            BinaryOp::BitAnd => Precedence::BitAnd,
            BinaryOp::Shl | BinaryOp::Shr => Precedence::Shift,
            BinaryOp::Add | BinaryOp::Sub => Precedence::Term,
            BinaryOp::Mul | BinaryOp::Div | BinaryOp::Rem => Precedence::Factor,
        }
    }

    /// Get the precedence for a token that can be an infix operator.
    pub fn of_infix_token(kind: TokenKind) -> Option<Self> {
        Some(match kind {
            // Assignment operators
            TokenKind::Eq
            | TokenKind::PlusEq
            | TokenKind::MinusEq
            | TokenKind::StarEq
            | TokenKind::SlashEq
            | TokenKind::PercentEq
            | TokenKind::AndEq
            | TokenKind::OrEq
            | TokenKind::CaretEq
            | TokenKind::ShlEq
            | TokenKind::ShrEq => Precedence::Assignment,

            // Range
            TokenKind::DotDot | TokenKind::DotDotEq => Precedence::Range,

            // Logical
            TokenKind::OrOr => Precedence::Or,
            TokenKind::AndAnd => Precedence::And,

            // Comparison
            TokenKind::EqEq
            | TokenKind::BangEq
            | TokenKind::Lt
            | TokenKind::LtEq
            | TokenKind::Gt
            | TokenKind::GtEq => Precedence::Comparison,

            // Bitwise
            TokenKind::Or => Precedence::BitOr,
            TokenKind::Caret => Precedence::BitXor,
            TokenKind::And => Precedence::BitAnd,
            TokenKind::Shl | TokenKind::Shr => Precedence::Shift,

            // Arithmetic
            TokenKind::Plus | TokenKind::Minus => Precedence::Term,
            TokenKind::Star | TokenKind::Slash | TokenKind::Percent => Precedence::Factor,

            // Cast
            TokenKind::As => Precedence::Cast,

            // Postfix
            TokenKind::LParen | TokenKind::LBracket | TokenKind::Dot | TokenKind::Question => {
                Precedence::Postfix
            }

            _ => return None,
        })
    }

    /// Get the associativity of this precedence level.
    /// Returns true for right-associative, false for left-associative.
    pub fn is_right_associative(self) -> bool {
        matches!(self, Precedence::Assignment)
    }
}

/// Convert a token kind to a binary operator.
pub fn token_to_binary_op(kind: TokenKind) -> Option<BinaryOp> {
    Some(match kind {
        TokenKind::Plus => BinaryOp::Add,
        TokenKind::Minus => BinaryOp::Sub,
        TokenKind::Star => BinaryOp::Mul,
        TokenKind::Slash => BinaryOp::Div,
        TokenKind::Percent => BinaryOp::Rem,
        TokenKind::EqEq => BinaryOp::Eq,
        TokenKind::BangEq => BinaryOp::Ne,
        TokenKind::Lt => BinaryOp::Lt,
        TokenKind::LtEq => BinaryOp::Le,
        TokenKind::Gt => BinaryOp::Gt,
        TokenKind::GtEq => BinaryOp::Ge,
        TokenKind::AndAnd => BinaryOp::And,
        TokenKind::OrOr => BinaryOp::Or,
        TokenKind::And => BinaryOp::BitAnd,
        TokenKind::Or => BinaryOp::BitOr,
        TokenKind::Caret => BinaryOp::BitXor,
        TokenKind::Shl => BinaryOp::Shl,
        TokenKind::Shr => BinaryOp::Shr,
        _ => return None,
    })
}

/// Convert a compound assignment token to the corresponding binary operator.
pub fn compound_assign_to_op(kind: TokenKind) -> Option<BinaryOp> {
    Some(match kind {
        TokenKind::PlusEq => BinaryOp::Add,
        TokenKind::MinusEq => BinaryOp::Sub,
        TokenKind::StarEq => BinaryOp::Mul,
        TokenKind::SlashEq => BinaryOp::Div,
        TokenKind::PercentEq => BinaryOp::Rem,
        TokenKind::AndEq => BinaryOp::BitAnd,
        TokenKind::OrEq => BinaryOp::BitOr,
        TokenKind::CaretEq => BinaryOp::BitXor,
        TokenKind::ShlEq => BinaryOp::Shl,
        TokenKind::ShrEq => BinaryOp::Shr,
        _ => return None,
    })
}
