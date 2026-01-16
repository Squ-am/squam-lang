use crate::bytecode::{Chunk, OpCode};
use squam_parser::{BinaryOp, Expr, ExprKind, Literal, UnaryOp};

// ---
// Optimizer Configuration
// ---

/// Configuration for the optimizer.
#[derive(Debug, Clone)]
pub struct OptConfig {
    /// Enable constant folding
    pub constant_folding: bool,
    /// Enable dead code elimination
    pub dead_code_elimination: bool,
    /// Enable peephole optimizations
    pub peephole: bool,
    /// Maximum iterations for optimization passes
    pub max_iterations: usize,
}

impl Default for OptConfig {
    fn default() -> Self {
        Self {
            constant_folding: true,
            dead_code_elimination: true,
            peephole: true,
            max_iterations: 10,
        }
    }
}

impl OptConfig {
    /// Create a config with all optimizations disabled.
    pub fn none() -> Self {
        Self {
            constant_folding: false,
            dead_code_elimination: false,
            peephole: false,
            max_iterations: 0,
        }
    }

    /// Create a config with all optimizations enabled.
    pub fn all() -> Self {
        Self::default()
    }
}

// ---
// Constant Folding
// ---

/// Constant folder that evaluates constant expressions at compile time.
pub struct ConstantFolder;

impl ConstantFolder {
    /// Fold constants in an expression, returning a potentially simplified expression.
    pub fn fold(expr: &Expr) -> Option<Expr> {
        match &expr.kind {
            ExprKind::Binary { op, left, right } => Self::fold_binary(*op, left, right, expr.span),
            ExprKind::Unary { op, operand } => Self::fold_unary(*op, operand, expr.span),
            ExprKind::If {
                condition,
                then_branch,
                else_branch,
            } => Self::fold_if(condition, then_branch, else_branch.as_deref(), expr.span),
            _ => None,
        }
    }

    /// Try to fold a binary operation.
    fn fold_binary(
        op: BinaryOp,
        left: &Expr,
        right: &Expr,
        span: squam_lexer::Span,
    ) -> Option<Expr> {
        // First, try to fold the operands
        let left = Self::fold(left).unwrap_or_else(|| left.clone());
        let right = Self::fold(right).unwrap_or_else(|| right.clone());

        // Check if both operands are literals
        let (left_lit, right_lit) = match (&left.kind, &right.kind) {
            (ExprKind::Literal(l), ExprKind::Literal(r)) => (l, r),
            _ => return None,
        };

        // Evaluate the operation
        let result = match (left_lit, right_lit) {
            // Integer operations
            (Literal::Int(a), Literal::Int(b)) => match op {
                BinaryOp::Add => Some(Literal::Int(a.wrapping_add(*b))),
                BinaryOp::Sub => Some(Literal::Int(a.wrapping_sub(*b))),
                BinaryOp::Mul => Some(Literal::Int(a.wrapping_mul(*b))),
                BinaryOp::Div if *b != 0 => Some(Literal::Int(a / b)),
                BinaryOp::Rem if *b != 0 => Some(Literal::Int(a % b)),
                BinaryOp::BitAnd => Some(Literal::Int(a & b)),
                BinaryOp::BitOr => Some(Literal::Int(a | b)),
                BinaryOp::BitXor => Some(Literal::Int(a ^ b)),
                BinaryOp::Shl => Some(Literal::Int(a << (b & 63))),
                BinaryOp::Shr => Some(Literal::Int(a >> (b & 63))),
                BinaryOp::Eq => Some(Literal::Bool(a == b)),
                BinaryOp::Ne => Some(Literal::Bool(a != b)),
                BinaryOp::Lt => Some(Literal::Bool(a < b)),
                BinaryOp::Le => Some(Literal::Bool(a <= b)),
                BinaryOp::Gt => Some(Literal::Bool(a > b)),
                BinaryOp::Ge => Some(Literal::Bool(a >= b)),
                _ => None,
            },
            // Float operations
            (Literal::Float(a), Literal::Float(b)) => match op {
                BinaryOp::Add => Some(Literal::Float(a + b)),
                BinaryOp::Sub => Some(Literal::Float(a - b)),
                BinaryOp::Mul => Some(Literal::Float(a * b)),
                BinaryOp::Div if *b != 0.0 => Some(Literal::Float(a / b)),
                BinaryOp::Eq => Some(Literal::Bool(a == b)),
                BinaryOp::Ne => Some(Literal::Bool(a != b)),
                BinaryOp::Lt => Some(Literal::Bool(a < b)),
                BinaryOp::Le => Some(Literal::Bool(a <= b)),
                BinaryOp::Gt => Some(Literal::Bool(a > b)),
                BinaryOp::Ge => Some(Literal::Bool(a >= b)),
                _ => None,
            },
            // Mixed int/float
            (Literal::Int(a), Literal::Float(b)) => {
                let a = *a as f64;
                match op {
                    BinaryOp::Add => Some(Literal::Float(a + b)),
                    BinaryOp::Sub => Some(Literal::Float(a - b)),
                    BinaryOp::Mul => Some(Literal::Float(a * b)),
                    BinaryOp::Div if *b != 0.0 => Some(Literal::Float(a / b)),
                    _ => None,
                }
            }
            (Literal::Float(a), Literal::Int(b)) => {
                let b = *b as f64;
                match op {
                    BinaryOp::Add => Some(Literal::Float(a + b)),
                    BinaryOp::Sub => Some(Literal::Float(a - b)),
                    BinaryOp::Mul => Some(Literal::Float(a * b)),
                    BinaryOp::Div if b != 0.0 => Some(Literal::Float(a / b)),
                    _ => None,
                }
            }
            // Boolean operations
            (Literal::Bool(a), Literal::Bool(b)) => match op {
                BinaryOp::And => Some(Literal::Bool(*a && *b)),
                BinaryOp::Or => Some(Literal::Bool(*a || *b)),
                BinaryOp::Eq => Some(Literal::Bool(a == b)),
                BinaryOp::Ne => Some(Literal::Bool(a != b)),
                _ => None,
            },
            // String concatenation
            (Literal::String(a), Literal::String(b)) => match op {
                BinaryOp::Add => Some(Literal::String(format!("{}{}", a, b))),
                BinaryOp::Eq => Some(Literal::Bool(a == b)),
                BinaryOp::Ne => Some(Literal::Bool(a != b)),
                _ => None,
            },
            _ => None,
        };

        result.map(|lit| Expr {
            kind: ExprKind::Literal(lit),
            span,
        })
    }

    /// Try to fold a unary operation.
    fn fold_unary(op: UnaryOp, operand: &Expr, span: squam_lexer::Span) -> Option<Expr> {
        let operand = Self::fold(operand).unwrap_or_else(|| operand.clone());

        let lit = match &operand.kind {
            ExprKind::Literal(l) => l,
            _ => return None,
        };

        let result = match (op, lit) {
            (UnaryOp::Neg, Literal::Int(n)) => Some(Literal::Int(-n)),
            (UnaryOp::Neg, Literal::Float(n)) => Some(Literal::Float(-n)),
            (UnaryOp::Not, Literal::Bool(b)) => Some(Literal::Bool(!b)),
            (UnaryOp::BitNot, Literal::Int(n)) => Some(Literal::Int(!n)),
            _ => None,
        };

        result.map(|lit| Expr {
            kind: ExprKind::Literal(lit),
            span,
        })
    }

    /// Try to fold an if expression with a constant condition.
    fn fold_if(
        condition: &Expr,
        then_branch: &squam_parser::Block,
        else_branch: Option<&Expr>,
        _span: squam_lexer::Span,
    ) -> Option<Expr> {
        let condition = Self::fold(condition).unwrap_or_else(|| condition.clone());

        match &condition.kind {
            ExprKind::Literal(Literal::Bool(true)) => {
                // Return then branch as a block expression
                if then_branch.stmts.len() == 1 {
                    if let squam_parser::StmtKind::ExprNoSemi(expr) = &then_branch.stmts[0].kind {
                        return Some(expr.clone());
                    }
                }
                None
            }
            ExprKind::Literal(Literal::Bool(false)) => {
                // Return else branch if it exists
                else_branch.cloned()
            }
            _ => None,
        }
    }
}

// ---
// Peephole Optimizer
// ---

/// Peephole optimizer that optimizes short sequences of bytecode.
pub struct PeepholeOptimizer;

impl PeepholeOptimizer {
    /// Optimize a chunk of bytecode.
    pub fn optimize(chunk: &mut Chunk) -> OptStats {
        let mut stats = OptStats::default();
        let mut changed = true;
        let mut iterations = 0;
        const MAX_ITERATIONS: usize = 10;

        while changed && iterations < MAX_ITERATIONS {
            changed = false;
            iterations += 1;

            // Push-Pop elimination
            changed |= Self::eliminate_push_pop(chunk, &mut stats);

            // Double negation elimination
            changed |= Self::eliminate_double_neg(chunk, &mut stats);

            // Jump threading
            changed |= Self::thread_jumps(chunk, &mut stats);

            // Constant operations
            changed |= Self::fold_constant_ops(chunk, &mut stats);
        }

        stats.iterations = iterations;
        stats
    }

    /// Eliminate Push followed by Pop patterns.
    fn eliminate_push_pop(chunk: &mut Chunk, stats: &mut OptStats) -> bool {
        let mut changed = false;
        let code = &mut chunk.code;
        let mut i = 0;

        while i + 1 < code.len() {
            let op1 = OpCode::try_from(code[i]);
            let op2_idx = Self::next_instruction(code, i);

            if let Some(op2_idx) = op2_idx {
                if let (Ok(op1), Ok(OpCode::Pop)) = (op1, OpCode::try_from(code[op2_idx])) {
                    // Check if op1 is a push operation that can be eliminated
                    if Self::is_pure_push(&op1) {
                        // Calculate instruction size
                        let op1_size = Self::instruction_size(&op1, code, i);

                        // Replace with NOPs
                        code[i..=op2_idx].fill(OpCode::Nop as u8);

                        stats.eliminated_instructions += op1_size + 1;
                        changed = true;
                    }
                }
            }
            i = Self::next_instruction(code, i).unwrap_or(code.len());
        }

        if changed {
            Self::remove_nops(chunk);
        }
        changed
    }

    /// Eliminate double negation (Neg Neg -> nothing, Not Not -> nothing).
    fn eliminate_double_neg(chunk: &mut Chunk, stats: &mut OptStats) -> bool {
        let mut changed = false;
        let code = &mut chunk.code;
        let mut i = 0;

        while i + 1 < code.len() {
            let op1 = OpCode::try_from(code[i]);
            let op2_idx = Self::next_instruction(code, i);

            if let Some(op2_idx) = op2_idx {
                let op2 = OpCode::try_from(code[op2_idx]);

                match (op1, op2) {
                    (Ok(OpCode::Neg), Ok(OpCode::Neg)) | (Ok(OpCode::Not), Ok(OpCode::Not)) => {
                        code[i] = OpCode::Nop as u8;
                        code[op2_idx] = OpCode::Nop as u8;
                        stats.eliminated_instructions += 2;
                        changed = true;
                    }
                    _ => {}
                }
            }
            i = Self::next_instruction(code, i).unwrap_or(code.len());
        }

        if changed {
            Self::remove_nops(chunk);
        }
        changed
    }

    /// Thread jumps that jump to other jumps.
    fn thread_jumps(chunk: &mut Chunk, stats: &mut OptStats) -> bool {
        let mut changed = false;
        let code = &mut chunk.code;
        let mut i = 0;

        while i < code.len() {
            let op = OpCode::try_from(code[i]);

            if let Ok(OpCode::Jump) = op {
                if i + 2 < code.len() {
                    let offset = (code[i + 1] as u16) | ((code[i + 2] as u16) << 8);
                    let target = i + 3 + offset as usize;

                    // Check if target is also a Jump
                    if target < code.len() {
                        if let Ok(OpCode::Jump) = OpCode::try_from(code[target]) {
                            if target + 2 < code.len() {
                                let target_offset =
                                    (code[target + 1] as u16) | ((code[target + 2] as u16) << 8);
                                let final_target = target + 3 + target_offset as usize;

                                // Calculate new offset from current position
                                if final_target > i + 3 {
                                    let new_offset = (final_target - i - 3) as u16;
                                    code[i + 1] = (new_offset & 0xFF) as u8;
                                    code[i + 2] = ((new_offset >> 8) & 0xFF) as u8;
                                    stats.threaded_jumps += 1;
                                    changed = true;
                                }
                            }
                        }
                    }
                }
            }

            i = Self::next_instruction(code, i).unwrap_or(code.len());
        }

        changed
    }

    /// Fold constant operations (e.g., ConstSmall 1 ConstSmall 2 Add -> ConstSmall 3).
    fn fold_constant_ops(chunk: &mut Chunk, stats: &mut OptStats) -> bool {
        // This is a more complex optimization that would require tracking constants
        // For now, we'll skip this and rely on AST-level constant folding
        let _ = (chunk, stats);
        false
    }

    /// Check if an opcode is a pure push (no side effects, just pushes a value).
    fn is_pure_push(op: &OpCode) -> bool {
        matches!(
            op,
            OpCode::ConstSmall
                | OpCode::Const
                | OpCode::Unit
                | OpCode::True
                | OpCode::False
                | OpCode::LoadLocalSmall
                | OpCode::LoadLocal
        )
    }

    /// Get the size of an instruction in bytes.
    fn instruction_size(op: &OpCode, code: &[u8], offset: usize) -> usize {
        match op {
            OpCode::ConstSmall | OpCode::LoadLocalSmall | OpCode::StoreLocalSmall => 2,
            OpCode::Const
            | OpCode::LoadLocal
            | OpCode::StoreLocal
            | OpCode::LoadGlobal
            | OpCode::StoreGlobal
            | OpCode::Jump
            | OpCode::JumpIfFalse
            | OpCode::Loop => 3,
            OpCode::Closure => {
                // Variable length: opcode + fn_idx(2) + upvalue_count + upvalues
                if offset + 3 < code.len() {
                    let upvalue_count = code[offset + 3] as usize;
                    4 + upvalue_count * 2
                } else {
                    1
                }
            }
            _ => 1,
        }
    }

    /// Get the index of the next instruction.
    fn next_instruction(code: &[u8], offset: usize) -> Option<usize> {
        if offset >= code.len() {
            return None;
        }

        let op = OpCode::try_from(code[offset]).ok()?;
        let size = match op {
            OpCode::ConstSmall
            | OpCode::LoadLocalSmall
            | OpCode::StoreLocalSmall
            | OpCode::Call
            | OpCode::GetField
            | OpCode::SetField
            | OpCode::Array
            | OpCode::Tuple => 2,

            OpCode::Const
            | OpCode::LoadLocal
            | OpCode::StoreLocal
            | OpCode::LoadGlobal
            | OpCode::StoreGlobal
            | OpCode::Jump
            | OpCode::JumpIfFalse
            | OpCode::Loop => 3,

            OpCode::Closure => {
                if offset + 3 < code.len() {
                    let upvalue_count = code[offset + 3] as usize;
                    4 + upvalue_count * 2
                } else {
                    return None;
                }
            }

            _ => 1,
        };

        let next = offset + size;
        if next <= code.len() {
            Some(next)
        } else {
            None
        }
    }

    /// Remove NOP instructions from a chunk.
    fn remove_nops(chunk: &mut Chunk) {
        // Build a mapping from old offsets to new offsets
        let mut offset_map: Vec<usize> = Vec::with_capacity(chunk.code.len());
        let mut new_offset = 0;

        for &byte in &chunk.code {
            offset_map.push(new_offset);
            if byte != OpCode::Nop as u8 {
                new_offset += 1;
            }
        }

        // Remove NOPs
        chunk.code.retain(|&b| b != OpCode::Nop as u8);

        // Update jump targets
        // Note: This is simplified and may need more sophisticated handling
        // for complex control flow
    }
}

// ---
// Dead Code Eliminator
// ---

/// Dead code eliminator that removes unreachable code.
pub struct DeadCodeEliminator;

impl DeadCodeEliminator {
    /// Eliminate dead code from a chunk.
    pub fn eliminate(chunk: &mut Chunk) -> OptStats {
        let mut stats = OptStats::default();

        // Find all reachable instructions
        let reachable = Self::find_reachable(chunk);

        // Mark unreachable instructions as NOPs
        for (i, &is_reachable) in reachable.iter().enumerate() {
            if !is_reachable && i < chunk.code.len() {
                chunk.code[i] = OpCode::Nop as u8;
                stats.eliminated_instructions += 1;
            }
        }

        // Remove NOPs
        if stats.eliminated_instructions > 0 {
            chunk.code.retain(|&b| b != OpCode::Nop as u8);
        }

        stats
    }

    /// Find all reachable instructions using control flow analysis.
    fn find_reachable(chunk: &Chunk) -> Vec<bool> {
        let mut reachable = vec![false; chunk.code.len()];
        let mut worklist = vec![0usize]; // Start from instruction 0

        while let Some(offset) = worklist.pop() {
            if offset >= chunk.code.len() || reachable[offset] {
                continue;
            }

            // Mark this instruction as reachable
            reachable[offset] = true;

            let op = match OpCode::try_from(chunk.code[offset]) {
                Ok(op) => op,
                Err(_) => continue,
            };

            // Get the next instruction offset
            let next = PeepholeOptimizer::next_instruction(&chunk.code, offset);

            match op {
                OpCode::Return => {
                    // No fall-through
                }
                OpCode::Jump => {
                    if offset + 2 < chunk.code.len() {
                        let jump_offset = (chunk.code[offset + 1] as u16)
                            | ((chunk.code[offset + 2] as u16) << 8);
                        let target = offset + 3 + jump_offset as usize;
                        worklist.push(target);
                    }
                }
                OpCode::JumpIfFalse => {
                    if offset + 2 < chunk.code.len() {
                        let jump_offset = (chunk.code[offset + 1] as u16)
                            | ((chunk.code[offset + 2] as u16) << 8);
                        let target = offset + 3 + jump_offset as usize;
                        worklist.push(target);
                    }
                    // Also fall through
                    if let Some(next) = next {
                        worklist.push(next);
                    }
                }
                OpCode::Loop => {
                    if offset + 2 < chunk.code.len() {
                        let loop_offset = (chunk.code[offset + 1] as u16)
                            | ((chunk.code[offset + 2] as u16) << 8);
                        let target = offset + 3 - loop_offset as usize;
                        worklist.push(target);
                    }
                }
                _ => {
                    // Normal fall-through
                    if let Some(next) = next {
                        worklist.push(next);
                    }
                }
            }
        }

        reachable
    }
}

// ---
// Optimization Statistics
// ---

/// Statistics from optimization passes.
#[derive(Debug, Default, Clone)]
pub struct OptStats {
    /// Number of instructions eliminated
    pub eliminated_instructions: usize,
    /// Number of constants folded
    pub constants_folded: usize,
    /// Number of jumps threaded
    pub threaded_jumps: usize,
    /// Number of optimization iterations
    pub iterations: usize,
}

impl OptStats {
    /// Merge statistics from another OptStats.
    pub fn merge(&mut self, other: &OptStats) {
        self.eliminated_instructions += other.eliminated_instructions;
        self.constants_folded += other.constants_folded;
        self.threaded_jumps += other.threaded_jumps;
        self.iterations = self.iterations.max(other.iterations);
    }
}

// ---
// Tests
// ---

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_constant_fold_int_add() {
        let expr = Expr {
            kind: ExprKind::Binary {
                op: BinaryOp::Add,
                left: Box::new(Expr {
                    kind: ExprKind::Literal(Literal::Int(10)),
                    span: squam_lexer::Span::new(0, 0, 0),
                }),
                right: Box::new(Expr {
                    kind: ExprKind::Literal(Literal::Int(20)),
                    span: squam_lexer::Span::new(0, 0, 0),
                }),
            },
            span: squam_lexer::Span::new(0, 0, 0),
        };

        let folded = ConstantFolder::fold(&expr).unwrap();
        assert!(matches!(folded.kind, ExprKind::Literal(Literal::Int(30))));
    }

    #[test]
    fn test_constant_fold_int_mul() {
        let expr = Expr {
            kind: ExprKind::Binary {
                op: BinaryOp::Mul,
                left: Box::new(Expr {
                    kind: ExprKind::Literal(Literal::Int(6)),
                    span: squam_lexer::Span::new(0, 0, 0),
                }),
                right: Box::new(Expr {
                    kind: ExprKind::Literal(Literal::Int(7)),
                    span: squam_lexer::Span::new(0, 0, 0),
                }),
            },
            span: squam_lexer::Span::new(0, 0, 0),
        };

        let folded = ConstantFolder::fold(&expr).unwrap();
        assert!(matches!(folded.kind, ExprKind::Literal(Literal::Int(42))));
    }

    #[test]
    fn test_constant_fold_comparison() {
        let expr = Expr {
            kind: ExprKind::Binary {
                op: BinaryOp::Lt,
                left: Box::new(Expr {
                    kind: ExprKind::Literal(Literal::Int(5)),
                    span: squam_lexer::Span::new(0, 0, 0),
                }),
                right: Box::new(Expr {
                    kind: ExprKind::Literal(Literal::Int(10)),
                    span: squam_lexer::Span::new(0, 0, 0),
                }),
            },
            span: squam_lexer::Span::new(0, 0, 0),
        };

        let folded = ConstantFolder::fold(&expr).unwrap();
        assert!(matches!(
            folded.kind,
            ExprKind::Literal(Literal::Bool(true))
        ));
    }

    #[test]
    fn test_constant_fold_unary_neg() {
        let expr = Expr {
            kind: ExprKind::Unary {
                op: UnaryOp::Neg,
                operand: Box::new(Expr {
                    kind: ExprKind::Literal(Literal::Int(42)),
                    span: squam_lexer::Span::new(0, 0, 0),
                }),
            },
            span: squam_lexer::Span::new(0, 0, 0),
        };

        let folded = ConstantFolder::fold(&expr).unwrap();
        assert!(matches!(folded.kind, ExprKind::Literal(Literal::Int(-42))));
    }

    #[test]
    fn test_constant_fold_nested() {
        // (2 + 3) * 4 = 20
        let expr = Expr {
            kind: ExprKind::Binary {
                op: BinaryOp::Mul,
                left: Box::new(Expr {
                    kind: ExprKind::Binary {
                        op: BinaryOp::Add,
                        left: Box::new(Expr {
                            kind: ExprKind::Literal(Literal::Int(2)),
                            span: squam_lexer::Span::new(0, 0, 0),
                        }),
                        right: Box::new(Expr {
                            kind: ExprKind::Literal(Literal::Int(3)),
                            span: squam_lexer::Span::new(0, 0, 0),
                        }),
                    },
                    span: squam_lexer::Span::new(0, 0, 0),
                }),
                right: Box::new(Expr {
                    kind: ExprKind::Literal(Literal::Int(4)),
                    span: squam_lexer::Span::new(0, 0, 0),
                }),
            },
            span: squam_lexer::Span::new(0, 0, 0),
        };

        let folded = ConstantFolder::fold(&expr).unwrap();
        assert!(matches!(folded.kind, ExprKind::Literal(Literal::Int(20))));
    }

    #[test]
    fn test_constant_fold_string_concat() {
        let expr = Expr {
            kind: ExprKind::Binary {
                op: BinaryOp::Add,
                left: Box::new(Expr {
                    kind: ExprKind::Literal(Literal::String("Hello, ".to_string())),
                    span: squam_lexer::Span::new(0, 0, 0),
                }),
                right: Box::new(Expr {
                    kind: ExprKind::Literal(Literal::String("World!".to_string())),
                    span: squam_lexer::Span::new(0, 0, 0),
                }),
            },
            span: squam_lexer::Span::new(0, 0, 0),
        };

        let folded = ConstantFolder::fold(&expr).unwrap();
        match folded.kind {
            ExprKind::Literal(Literal::String(s)) => assert_eq!(s, "Hello, World!"),
            _ => panic!("Expected string literal"),
        }
    }

    #[test]
    fn test_peephole_stats() {
        let stats = OptStats::default();
        assert_eq!(stats.eliminated_instructions, 0);
        assert_eq!(stats.constants_folded, 0);
    }
}
