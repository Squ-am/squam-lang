use crate::ast::*;
use crate::precedence::{compound_assign_to_op, token_to_binary_op, Precedence};
use squam_lexer::{
    parse_char, parse_float, parse_int, parse_string, Lexer, Span, Token, TokenKind,
};
use std::sync::Arc;

/// Parse errors.
#[derive(Debug, Clone, thiserror::Error)]
pub enum ParseError {
    #[error("unexpected token: expected {expected}, found {found}")]
    UnexpectedToken {
        expected: String,
        found: String,
        span: Span,
    },
    #[error("unexpected end of file")]
    UnexpectedEof { span: Span },
    #[error("{message}")]
    Custom { message: String, span: Span },
}

impl ParseError {
    pub fn span(&self) -> Span {
        match self {
            ParseError::UnexpectedToken { span, .. } => *span,
            ParseError::UnexpectedEof { span } => *span,
            ParseError::Custom { span, .. } => *span,
        }
    }
}

/// The Squam parser.
pub struct Parser<'src> {
    lexer: Lexer<'src>,
    source: &'src str,
    current: Token,
    errors: Vec<ParseError>,
}

impl<'src> Parser<'src> {
    /// Create a new parser for the given source code.
    pub fn new(source: &'src str, file_id: u16) -> Self {
        let mut lexer = Lexer::new(source, file_id);
        let current = lexer.next_token();
        Self {
            lexer,
            source,
            current,
            errors: Vec::new(),
        }
    }

    /// Get the accumulated parse errors.
    pub fn errors(&self) -> &[ParseError] {
        &self.errors
    }

    /// Take the accumulated parse errors.
    pub fn take_errors(&mut self) -> Vec<ParseError> {
        std::mem::take(&mut self.errors)
    }

    /// Check if parsing had errors.
    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }

    // ---
    // Token manipulation
    // ---

    /// Advance to the next token, returning the current one.
    fn advance(&mut self) -> Token {
        std::mem::replace(&mut self.current, self.lexer.next_token())
    }

    /// Check if the current token is of the given kind.
    fn check(&self, kind: TokenKind) -> bool {
        self.current.kind == kind
    }

    /// Check if the current token is one of the given kinds.
    fn check_any(&self, kinds: &[TokenKind]) -> bool {
        kinds.contains(&self.current.kind)
    }

    /// Check if the next token (after current) is of the given kind.
    fn peek_is(&mut self, kind: TokenKind) -> bool {
        self.lexer.peek().kind == kind
    }

    /// Consume the current token if it matches, returning true.
    fn consume(&mut self, kind: TokenKind) -> bool {
        if self.check(kind) {
            self.advance();
            true
        } else {
            false
        }
    }

    /// Expect the current token to be of the given kind, advancing if so.
    fn expect(&mut self, kind: TokenKind) -> Result<Token, ParseError> {
        if self.check(kind) {
            Ok(self.advance())
        } else {
            let err = ParseError::UnexpectedToken {
                expected: kind.name().to_string(),
                found: self.current.kind.name().to_string(),
                span: self.current.span,
            };
            self.errors.push(err.clone());
            Err(err)
        }
    }

    /// Get the source slice for a span.
    fn slice(&self, span: Span) -> &'src str {
        &self.source[span.start as usize..span.end as usize]
    }

    /// Report an error.
    fn error(&mut self, message: impl Into<String>, span: Span) {
        self.errors.push(ParseError::Custom {
            message: message.into(),
            span,
        });
    }

    /// Create a symbol from a span.
    fn symbol(&self, span: Span) -> Symbol {
        Arc::from(self.slice(span))
    }

    // ---
    // Module parsing
    // ---

    /// Parse a complete module.
    pub fn parse_module(&mut self) -> Module {
        let start = self.current.span;
        let mut items = Vec::new();

        while !self.check(TokenKind::Eof) {
            match self.parse_item() {
                Ok(item) => items.push(item),
                Err(_) => {
                    // Error recovery: skip to next item
                    self.recover_to_item();
                }
            }
        }

        let span = if items.is_empty() {
            start
        } else {
            start.merge(items.last().unwrap().span())
        };

        Module { items, span }
    }

    /// Skip tokens until we find something that looks like an item start.
    fn recover_to_item(&mut self) {
        while !self.check(TokenKind::Eof) {
            if self.check_any(&[
                TokenKind::Fn,
                TokenKind::Struct,
                TokenKind::Enum,
                TokenKind::Impl,
                TokenKind::Trait,
                TokenKind::Type,
                TokenKind::Const,
                TokenKind::Use,
                TokenKind::Mod,
                TokenKind::Pub,
            ]) {
                return;
            }
            self.advance();
        }
    }

    // ---
    // Item parsing
    // ---

    /// Parse a top-level item.
    fn parse_item(&mut self) -> Result<Item, ParseError> {
        // Parse attributes first
        let attributes = self.parse_attributes();
        let visibility = self.parse_visibility();

        match self.current.kind {
            TokenKind::Async => {
                self.advance();
                self.expect(TokenKind::Fn)?;
                self.parse_function_inner(visibility, true).map(Item::Function)
            }
            TokenKind::Fn => {
                self.advance();
                self.parse_function_inner(visibility, false).map(Item::Function)
            }
            TokenKind::Struct => self.parse_struct(visibility, attributes).map(Item::Struct),
            TokenKind::Enum => self.parse_enum(visibility, attributes).map(Item::Enum),
            TokenKind::Impl => self.parse_impl().map(Item::Impl),
            TokenKind::Trait => self.parse_trait(visibility).map(Item::Trait),
            TokenKind::Type => self.parse_type_alias(visibility).map(Item::TypeAlias),
            TokenKind::Const => self.parse_const(visibility).map(Item::Const),
            TokenKind::Use => self.parse_use(visibility).map(Item::Use),
            TokenKind::Mod => self.parse_mod(visibility).map(Item::Mod),
            _ => {
                let err = ParseError::UnexpectedToken {
                    expected: "item".to_string(),
                    found: self.current.kind.name().to_string(),
                    span: self.current.span,
                };
                self.errors.push(err.clone());
                Err(err)
            }
        }
    }

    /// Parse visibility modifier.
    fn parse_visibility(&mut self) -> Visibility {
        if self.consume(TokenKind::Pub) {
            Visibility::Public
        } else {
            Visibility::Private
        }
    }

    /// Parse attributes: #[attr], #[derive(Foo, Bar)]
    fn parse_attributes(&mut self) -> Vec<Attribute> {
        let mut attrs = Vec::new();
        while self.check(TokenKind::Hash) {
            if let Some(attr) = self.parse_attribute() {
                attrs.push(attr);
            } else {
                break;
            }
        }
        attrs
    }

    /// Parse a single attribute.
    fn parse_attribute(&mut self) -> Option<Attribute> {
        let start = self.current.span;

        if !self.consume(TokenKind::Hash) {
            return None;
        }

        if !self.consume(TokenKind::LBracket) {
            // Not a valid attribute, just a bare #
            return None;
        }

        // Parse the attribute name
        let name = match self.parse_identifier() {
            Ok(name) => name,
            Err(_) => return None,
        };

        let kind = if name.name.as_ref() == "derive" {
            // Parse #[derive(Trait1, Trait2, ...)]
            if !self.consume(TokenKind::LParen) {
                return None;
            }

            let mut traits = Vec::new();
            while !self.check(TokenKind::RParen) && !self.check(TokenKind::Eof) {
                match self.parse_identifier() {
                    Ok(ident) => traits.push(ident),
                    Err(_) => return None,
                }
                if !self.consume(TokenKind::Comma) {
                    break;
                }
            }

            if !self.consume(TokenKind::RParen) {
                return None;
            }

            AttributeKind::Derive(traits)
        } else {
            // Generic attribute with optional args
            let mut args = Vec::new();
            if self.consume(TokenKind::LParen) {
                while !self.check(TokenKind::RParen) && !self.check(TokenKind::Eof) {
                    if let Some(arg) = self.parse_attribute_arg() {
                        args.push(arg);
                    }
                    if !self.consume(TokenKind::Comma) {
                        break;
                    }
                }
                if !self.consume(TokenKind::RParen) {
                    return None;
                }
            }
            AttributeKind::Named { name, args }
        };

        let end_span = self.current.span;
        if !self.consume(TokenKind::RBracket) {
            return None;
        }

        let span = start.merge(end_span);
        Some(Attribute { kind, span })
    }

    /// Parse an attribute argument.
    fn parse_attribute_arg(&mut self) -> Option<AttributeArg> {
        // Try parsing an identifier
        if self.check(TokenKind::Identifier) {
            let ident = self.parse_identifier().ok()?;

            // Check for assignment
            if self.consume(TokenKind::Eq) {
                let value = self.parse_attribute_arg()?;
                Some(AttributeArg::Assign {
                    name: ident,
                    value: Box::new(value),
                })
            } else {
                Some(AttributeArg::Ident(ident))
            }
        } else if self.check(TokenKind::IntLiteral) {
            let token = self.advance();
            let s = self.slice(token.span);
            if let Ok(value) = parse_int(s) {
                Some(AttributeArg::Literal(Literal::Int(value)))
            } else {
                None
            }
        } else if self.check(TokenKind::StringLiteral) {
            let token = self.advance();
            let s = self.slice(token.span);
            if let Ok(value) = parse_string(s) {
                Some(AttributeArg::Literal(Literal::String(value)))
            } else {
                None
            }
        } else {
            None
        }
    }

    /// Parse a function definition (after `fn` keyword has been consumed).
    fn parse_function_inner(
        &mut self,
        visibility: Visibility,
        is_async: bool,
    ) -> Result<FunctionDef, ParseError> {
        let start = self.current.span;

        let name = self.parse_identifier()?;
        let generics = self.parse_optional_generics()?;

        self.expect(TokenKind::LParen)?;
        let params = self.parse_function_params()?;
        self.expect(TokenKind::RParen)?;

        let return_type = if self.consume(TokenKind::Arrow) {
            Some(self.parse_type()?)
        } else {
            None
        };

        let body = self.parse_block()?;
        let span = start.merge(body.span);

        Ok(FunctionDef {
            is_async,
            visibility,
            name,
            generics,
            params,
            return_type,
            body,
            span,
        })
    }

    /// Parse function parameters.
    fn parse_function_params(&mut self) -> Result<Vec<Parameter>, ParseError> {
        let mut params = Vec::new();

        while !self.check(TokenKind::RParen) && !self.check(TokenKind::Eof) {
            // Handle self parameter
            if self.check(TokenKind::SelfLower)
                || (self.check(TokenKind::And) && self.lexer.check(TokenKind::SelfLower))
                || (self.check(TokenKind::And) && self.lexer.check(TokenKind::Mut))
            {
                let param = self.parse_self_param()?;
                params.push(param);
            } else {
                let param = self.parse_param()?;
                params.push(param);
            }

            if !self.consume(TokenKind::Comma) {
                break;
            }
        }

        Ok(params)
    }

    /// Parse a self parameter.
    fn parse_self_param(&mut self) -> Result<Parameter, ParseError> {
        let start = self.current.span;
        let mutable;
        let by_ref;

        if self.consume(TokenKind::And) {
            by_ref = true;
            mutable = self.consume(TokenKind::Mut);
        } else {
            by_ref = false;
            mutable = self.consume(TokenKind::Mut);
        }

        let self_token = self.expect(TokenKind::SelfLower)?;
        let span = start.merge(self_token.span);

        let pattern = Pattern {
            kind: PatternKind::Binding {
                mutable,
                by_ref,
                name: Identifier::new("self", self_token.span),
                subpattern: None,
            },
            span,
        };

        // Self type is implicit
        let ty = Type {
            kind: if by_ref {
                TypeKind::Reference {
                    mutable,
                    inner: Box::new(Type {
                        kind: TypeKind::Path(TypePath {
                            segments: vec![PathSegment {
                                ident: Identifier::new("Self", self_token.span),
                                args: None,
                            }],
                            span: self_token.span,
                        }),
                        span: self_token.span,
                    }),
                }
            } else {
                TypeKind::Path(TypePath {
                    segments: vec![PathSegment {
                        ident: Identifier::new("Self", self_token.span),
                        args: None,
                    }],
                    span: self_token.span,
                })
            },
            span: self_token.span,
        };

        Ok(Parameter {
            pattern,
            ty,
            default: None,
            span,
        })
    }

    /// Parse a regular parameter.
    fn parse_param(&mut self) -> Result<Parameter, ParseError> {
        let start = self.current.span;
        let pattern = self.parse_pattern()?;
        self.expect(TokenKind::Colon)?;
        let ty = self.parse_type()?;

        // Parse optional default value
        let (default, end_span) = if self.consume(TokenKind::Eq) {
            let expr = self.parse_expression()?;
            let end = expr.span;
            (Some(expr), end)
        } else {
            (None, ty.span)
        };

        let span = start.merge(end_span);
        Ok(Parameter {
            pattern,
            ty,
            default,
            span,
        })
    }

    /// Parse an identifier.
    fn parse_identifier(&mut self) -> Result<Identifier, ParseError> {
        let token = self.expect(TokenKind::Identifier)?;
        Ok(Identifier::new(self.symbol(token.span), token.span))
    }

    /// Parse a struct definition.
    fn parse_struct(
        &mut self,
        visibility: Visibility,
        attributes: Vec<Attribute>,
    ) -> Result<StructDef, ParseError> {
        let start = self.current.span;
        self.expect(TokenKind::Struct)?;

        let name = self.parse_identifier()?;
        let generics = self.parse_optional_generics()?;

        let (fields, end_span) = if self.check(TokenKind::LBrace) {
            // Named fields
            self.advance();
            let fields = self.parse_struct_fields()?;
            let end = self.expect(TokenKind::RBrace)?;
            (StructFields::Named(fields), end.span)
        } else if self.check(TokenKind::LParen) {
            // Tuple fields
            self.advance();
            let fields = self.parse_tuple_fields()?;
            let end = self.expect(TokenKind::RParen)?;
            self.expect(TokenKind::Semicolon)?;
            (StructFields::Tuple(fields), end.span)
        } else {
            // Unit struct
            let end = self.expect(TokenKind::Semicolon)?;
            (StructFields::Unit, end.span)
        };

        Ok(StructDef {
            attributes,
            visibility,
            name,
            generics,
            fields,
            span: start.merge(end_span),
        })
    }

    /// Parse named struct fields.
    fn parse_struct_fields(&mut self) -> Result<Vec<StructField>, ParseError> {
        let mut fields = Vec::new();

        while !self.check(TokenKind::RBrace) && !self.check(TokenKind::Eof) {
            let visibility = self.parse_visibility();
            let start = self.current.span;
            let name = self.parse_identifier()?;
            self.expect(TokenKind::Colon)?;
            let ty = self.parse_type()?;

            // Parse optional default value
            let default = if self.consume(TokenKind::Eq) {
                Some(Box::new(self.parse_expression()?))
            } else {
                None
            };

            let span = start.merge(ty.span);

            fields.push(StructField {
                visibility,
                name,
                ty,
                default,
                span,
            });

            if !self.consume(TokenKind::Comma) {
                break;
            }
        }

        Ok(fields)
    }

    /// Parse tuple struct fields.
    fn parse_tuple_fields(&mut self) -> Result<Vec<TupleField>, ParseError> {
        let mut fields = Vec::new();

        while !self.check(TokenKind::RParen) && !self.check(TokenKind::Eof) {
            let visibility = self.parse_visibility();
            let ty = self.parse_type()?;
            let span = ty.span;

            fields.push(TupleField {
                visibility,
                ty,
                span,
            });

            if !self.consume(TokenKind::Comma) {
                break;
            }
        }

        Ok(fields)
    }

    /// Parse an enum definition.
    fn parse_enum(
        &mut self,
        visibility: Visibility,
        attributes: Vec<Attribute>,
    ) -> Result<EnumDef, ParseError> {
        let start = self.current.span;
        self.expect(TokenKind::Enum)?;

        let name = self.parse_identifier()?;
        let generics = self.parse_optional_generics()?;

        self.expect(TokenKind::LBrace)?;
        let variants = self.parse_enum_variants()?;
        let end = self.expect(TokenKind::RBrace)?;

        Ok(EnumDef {
            attributes,
            visibility,
            name,
            generics,
            variants,
            span: start.merge(end.span),
        })
    }

    /// Parse enum variants.
    fn parse_enum_variants(&mut self) -> Result<Vec<EnumVariant>, ParseError> {
        let mut variants = Vec::new();

        while !self.check(TokenKind::RBrace) && !self.check(TokenKind::Eof) {
            let start = self.current.span;
            let name = self.parse_identifier()?;

            let fields = if self.check(TokenKind::LBrace) {
                self.advance();
                let fields = self.parse_struct_fields()?;
                self.expect(TokenKind::RBrace)?;
                StructFields::Named(fields)
            } else if self.check(TokenKind::LParen) {
                self.advance();
                let fields = self.parse_tuple_fields()?;
                self.expect(TokenKind::RParen)?;
                StructFields::Tuple(fields)
            } else {
                StructFields::Unit
            };

            let discriminant = if self.consume(TokenKind::Eq) {
                Some(Box::new(self.parse_expression()?))
            } else {
                None
            };

            let span = start.merge(self.current.span);
            variants.push(EnumVariant {
                name,
                fields,
                discriminant,
                span,
            });

            if !self.consume(TokenKind::Comma) {
                break;
            }
        }

        Ok(variants)
    }

    /// Parse an impl block.
    fn parse_impl(&mut self) -> Result<ImplBlock, ParseError> {
        let start = self.current.span;
        self.expect(TokenKind::Impl)?;

        let generics = self.parse_optional_generics()?;

        // Parse type (and possibly trait)
        let first_type = self.parse_type()?;

        let (trait_, self_ty) = if self.consume(TokenKind::For) {
            // `impl Trait for Type`
            let trait_path = match first_type.kind {
                TypeKind::Path(p) => p,
                _ => {
                    self.error("expected trait path", first_type.span);
                    return Err(ParseError::Custom {
                        message: "expected trait path".to_string(),
                        span: first_type.span,
                    });
                }
            };
            let self_ty = self.parse_type()?;
            (Some(trait_path), self_ty)
        } else {
            (None, first_type)
        };

        self.expect(TokenKind::LBrace)?;
        let items = self.parse_impl_items()?;
        let end = self.expect(TokenKind::RBrace)?;

        Ok(ImplBlock {
            generics,
            trait_,
            self_ty,
            items,
            span: start.merge(end.span),
        })
    }

    /// Parse impl block items.
    fn parse_impl_items(&mut self) -> Result<Vec<ImplItem>, ParseError> {
        let mut items = Vec::new();

        while !self.check(TokenKind::RBrace) && !self.check(TokenKind::Eof) {
            let visibility = self.parse_visibility();

            let item = match self.current.kind {
                TokenKind::Async => {
                    self.advance();
                    self.expect(TokenKind::Fn)?;
                    ImplItem::Function(self.parse_function_inner(visibility, true)?)
                }
                TokenKind::Fn => {
                    self.advance();
                    ImplItem::Function(self.parse_function_inner(visibility, false)?)
                }
                TokenKind::Const => ImplItem::Const(self.parse_const(visibility)?),
                TokenKind::Type => ImplItem::Type(self.parse_type_alias(visibility)?),
                _ => {
                    self.error(
                        "expected fn, const, or type in impl block",
                        self.current.span,
                    );
                    self.advance();
                    continue;
                }
            };
            items.push(item);
        }

        Ok(items)
    }

    /// Parse a trait definition.
    fn parse_trait(&mut self, visibility: Visibility) -> Result<TraitDef, ParseError> {
        let start = self.current.span;
        self.expect(TokenKind::Trait)?;

        let name = self.parse_identifier()?;
        let generics = self.parse_optional_generics()?;

        // Parse supertraits
        let bounds = if self.consume(TokenKind::Colon) {
            self.parse_type_bounds()?
        } else {
            Vec::new()
        };

        self.expect(TokenKind::LBrace)?;
        let items = self.parse_trait_items()?;
        let end = self.expect(TokenKind::RBrace)?;

        Ok(TraitDef {
            visibility,
            name,
            generics,
            bounds,
            items,
            span: start.merge(end.span),
        })
    }

    /// Parse trait items.
    fn parse_trait_items(&mut self) -> Result<Vec<TraitItem>, ParseError> {
        let mut items = Vec::new();

        while !self.check(TokenKind::RBrace) && !self.check(TokenKind::Eof) {
            let item = match self.current.kind {
                TokenKind::Async => {
                    self.advance();
                    self.expect(TokenKind::Fn)?;
                    TraitItem::Function(self.parse_trait_function_inner(true)?)
                }
                TokenKind::Fn => {
                    self.advance();
                    TraitItem::Function(self.parse_trait_function_inner(false)?)
                }
                TokenKind::Const => TraitItem::Const(self.parse_trait_const()?),
                TokenKind::Type => TraitItem::Type(self.parse_trait_type()?),
                _ => {
                    self.error("expected fn, const, or type in trait", self.current.span);
                    self.advance();
                    continue;
                }
            };
            items.push(item);
        }

        Ok(items)
    }

    /// Parse a trait function (after `fn` keyword has been consumed).
    fn parse_trait_function_inner(&mut self, is_async: bool) -> Result<TraitFunction, ParseError> {
        let start = self.current.span;

        let name = self.parse_identifier()?;
        let generics = self.parse_optional_generics()?;

        self.expect(TokenKind::LParen)?;
        let params = self.parse_function_params()?;
        self.expect(TokenKind::RParen)?;

        let return_type = if self.consume(TokenKind::Arrow) {
            Some(self.parse_type()?)
        } else {
            None
        };

        let default = if self.check(TokenKind::LBrace) {
            Some(self.parse_block()?)
        } else {
            self.expect(TokenKind::Semicolon)?;
            None
        };

        let span = start.merge(self.current.span);
        Ok(TraitFunction {
            is_async,
            name,
            generics,
            params,
            return_type,
            default,
            span,
        })
    }

    /// Parse a trait const.
    fn parse_trait_const(&mut self) -> Result<TraitConst, ParseError> {
        let start = self.current.span;
        self.expect(TokenKind::Const)?;

        let name = self.parse_identifier()?;
        self.expect(TokenKind::Colon)?;
        let ty = self.parse_type()?;

        let default = if self.consume(TokenKind::Eq) {
            Some(self.parse_expression()?)
        } else {
            None
        };

        self.expect(TokenKind::Semicolon)?;
        let span = start.merge(self.current.span);

        Ok(TraitConst {
            name,
            ty,
            default,
            span,
        })
    }

    /// Parse a trait associated type.
    fn parse_trait_type(&mut self) -> Result<TraitType, ParseError> {
        let start = self.current.span;
        self.expect(TokenKind::Type)?;

        let name = self.parse_identifier()?;

        let bounds = if self.consume(TokenKind::Colon) {
            self.parse_type_bounds()?
        } else {
            Vec::new()
        };

        let default = if self.consume(TokenKind::Eq) {
            Some(self.parse_type()?)
        } else {
            None
        };

        self.expect(TokenKind::Semicolon)?;
        let span = start.merge(self.current.span);

        Ok(TraitType {
            name,
            bounds,
            default,
            span,
        })
    }

    /// Parse a type alias.
    fn parse_type_alias(&mut self, visibility: Visibility) -> Result<TypeAlias, ParseError> {
        let start = self.current.span;
        self.expect(TokenKind::Type)?;

        let name = self.parse_identifier()?;
        let generics = self.parse_optional_generics()?;

        self.expect(TokenKind::Eq)?;
        let ty = self.parse_type()?;
        self.expect(TokenKind::Semicolon)?;

        Ok(TypeAlias {
            visibility,
            name,
            generics,
            ty,
            span: start.merge(self.current.span),
        })
    }

    /// Parse a const definition.
    fn parse_const(&mut self, visibility: Visibility) -> Result<ConstDef, ParseError> {
        let start = self.current.span;
        self.expect(TokenKind::Const)?;

        let name = self.parse_identifier()?;
        self.expect(TokenKind::Colon)?;
        let ty = self.parse_type()?;
        self.expect(TokenKind::Eq)?;
        let value = self.parse_expression()?;
        self.expect(TokenKind::Semicolon)?;

        Ok(ConstDef {
            visibility,
            name,
            ty,
            value,
            span: start.merge(self.current.span),
        })
    }

    /// Parse a use declaration.
    fn parse_use(&mut self, visibility: Visibility) -> Result<UseDecl, ParseError> {
        let start = self.current.span;
        self.expect(TokenKind::Use)?;

        let tree = self.parse_use_tree()?;
        self.expect(TokenKind::Semicolon)?;

        Ok(UseDecl {
            visibility,
            tree,
            span: start.merge(self.current.span),
        })
    }

    /// Parse a use tree.
    fn parse_use_tree(&mut self) -> Result<UseTree, ParseError> {
        let mut path = Vec::new();

        // Parse path segments
        loop {
            if self.check(TokenKind::Star) {
                self.advance();
                return Ok(UseTree::Glob { path });
            }

            if self.check(TokenKind::LBrace) {
                self.advance();
                let mut items = Vec::new();
                while !self.check(TokenKind::RBrace) && !self.check(TokenKind::Eof) {
                    items.push(self.parse_use_tree()?);
                    if !self.consume(TokenKind::Comma) {
                        break;
                    }
                }
                self.expect(TokenKind::RBrace)?;
                return Ok(UseTree::Nested { path, items });
            }

            let ident = self.parse_identifier()?;
            path.push(ident);

            if self.consume(TokenKind::ColonColon) {
                continue;
            }

            // Check for alias
            let alias = if self.consume(TokenKind::As) {
                Some(self.parse_identifier()?)
            } else {
                None
            };

            return Ok(UseTree::Path { path, alias });
        }
    }

    /// Parse a mod declaration.
    fn parse_mod(&mut self, visibility: Visibility) -> Result<ModDecl, ParseError> {
        let start = self.current.span;
        self.expect(TokenKind::Mod)?;

        let name = self.parse_identifier()?;

        let items = if self.check(TokenKind::LBrace) {
            self.advance();
            let mut items = Vec::new();
            while !self.check(TokenKind::RBrace) && !self.check(TokenKind::Eof) {
                match self.parse_item() {
                    Ok(item) => items.push(item),
                    Err(_) => self.recover_to_item(),
                }
            }
            self.expect(TokenKind::RBrace)?;
            Some(items)
        } else {
            self.expect(TokenKind::Semicolon)?;
            None
        };

        Ok(ModDecl {
            visibility,
            name,
            items,
            span: start.merge(self.current.span),
        })
    }

    // ---
    // Generics
    // ---

    /// Parse optional generics.
    fn parse_optional_generics(&mut self) -> Result<Option<Generics>, ParseError> {
        if !self.check(TokenKind::Lt) {
            return Ok(None);
        }

        let start = self.current.span;
        self.advance();

        let mut params = Vec::new();
        while !self.check(TokenKind::Gt) && !self.check(TokenKind::Eof) {
            if self.check(TokenKind::Const) {
                params.push(GenericParam::Const(self.parse_const_param()?));
            } else {
                params.push(GenericParam::Type(self.parse_type_param()?));
            }

            if !self.consume(TokenKind::Comma) {
                break;
            }
        }

        self.expect(TokenKind::Gt)?;

        let where_clause = if self.check(TokenKind::Where) {
            Some(self.parse_where_clause()?)
        } else {
            None
        };

        let span = start.merge(self.current.span);
        Ok(Some(Generics {
            params,
            where_clause,
            span,
        }))
    }

    /// Parse a type parameter.
    fn parse_type_param(&mut self) -> Result<TypeParam, ParseError> {
        let start = self.current.span;
        let name = self.parse_identifier()?;

        let bounds = if self.consume(TokenKind::Colon) {
            self.parse_type_bounds()?
        } else {
            Vec::new()
        };

        let default = if self.consume(TokenKind::Eq) {
            Some(self.parse_type()?)
        } else {
            None
        };

        let span = start.merge(self.current.span);
        Ok(TypeParam {
            name,
            bounds,
            default,
            span,
        })
    }

    /// Parse a const parameter.
    fn parse_const_param(&mut self) -> Result<ConstParam, ParseError> {
        let start = self.current.span;
        self.expect(TokenKind::Const)?;

        let name = self.parse_identifier()?;
        self.expect(TokenKind::Colon)?;
        let ty = self.parse_type()?;

        let default = if self.consume(TokenKind::Eq) {
            Some(self.parse_expression()?)
        } else {
            None
        };

        let span = start.merge(self.current.span);
        Ok(ConstParam {
            name,
            ty,
            default,
            span,
        })
    }

    /// Parse type bounds.
    fn parse_type_bounds(&mut self) -> Result<Vec<TypePath>, ParseError> {
        let mut bounds = Vec::new();

        loop {
            bounds.push(self.parse_type_path()?);

            if !self.consume(TokenKind::Plus) {
                break;
            }
        }

        Ok(bounds)
    }

    /// Parse a where clause.
    fn parse_where_clause(&mut self) -> Result<WhereClause, ParseError> {
        let start = self.current.span;
        self.expect(TokenKind::Where)?;

        let mut predicates = Vec::new();

        loop {
            let ty = self.parse_type()?;
            self.expect(TokenKind::Colon)?;
            let bounds = self.parse_type_bounds()?;

            let span = ty.span.merge(self.current.span);
            predicates.push(WherePredicate { ty, bounds, span });

            if !self.consume(TokenKind::Comma) {
                break;
            }

            // Stop if we hit a brace (start of body)
            if self.check(TokenKind::LBrace) {
                break;
            }
        }

        let span = start.merge(self.current.span);
        Ok(WhereClause { predicates, span })
    }

    // ---
    // Types
    // ---

    /// Parse a type.
    pub fn parse_type(&mut self) -> Result<Type, ParseError> {
        let start = self.current.span;

        let kind = match self.current.kind {
            TokenKind::And => {
                // Reference type
                self.advance();
                let mutable = self.consume(TokenKind::Mut);
                let inner = self.parse_type()?;
                TypeKind::Reference {
                    mutable,
                    inner: Box::new(inner),
                }
            }
            TokenKind::LParen => {
                // Tuple or unit type
                self.advance();
                if self.consume(TokenKind::RParen) {
                    TypeKind::Unit
                } else {
                    let mut types = vec![self.parse_type()?];
                    while self.consume(TokenKind::Comma) {
                        if self.check(TokenKind::RParen) {
                            break;
                        }
                        types.push(self.parse_type()?);
                    }
                    self.expect(TokenKind::RParen)?;

                    if types.len() == 1 {
                        // Single element tuple needs trailing comma
                        return Ok(types.pop().unwrap());
                    }
                    TypeKind::Tuple(types)
                }
            }
            TokenKind::LBracket => {
                // Array or slice type
                self.advance();
                let element = self.parse_type()?;

                if self.consume(TokenKind::Semicolon) {
                    // Array type
                    let size = self.parse_expression()?;
                    self.expect(TokenKind::RBracket)?;
                    TypeKind::Array {
                        element: Box::new(element),
                        size: Box::new(size),
                    }
                } else {
                    // Slice type
                    self.expect(TokenKind::RBracket)?;
                    TypeKind::Slice {
                        element: Box::new(element),
                    }
                }
            }
            TokenKind::Bang => {
                self.advance();
                TypeKind::Never
            }
            TokenKind::Fn => {
                // Function type
                self.advance();
                self.expect(TokenKind::LParen)?;

                let mut params = Vec::new();
                while !self.check(TokenKind::RParen) && !self.check(TokenKind::Eof) {
                    params.push(self.parse_type()?);
                    if !self.consume(TokenKind::Comma) {
                        break;
                    }
                }
                self.expect(TokenKind::RParen)?;

                let return_type = if self.consume(TokenKind::Arrow) {
                    Some(Box::new(self.parse_type()?))
                } else {
                    None
                };

                TypeKind::Function {
                    params,
                    return_type,
                }
            }
            TokenKind::Identifier | TokenKind::SelfUpper | TokenKind::ColonColon => {
                TypeKind::Path(self.parse_type_path()?)
            }
            _ => {
                let err = ParseError::UnexpectedToken {
                    expected: "type".to_string(),
                    found: self.current.kind.name().to_string(),
                    span: self.current.span,
                };
                self.errors.push(err.clone());
                return Err(err);
            }
        };

        let span = start.merge(self.current.span);
        Ok(Type { kind, span })
    }

    /// Parse a type path.
    fn parse_type_path(&mut self) -> Result<TypePath, ParseError> {
        let start = self.current.span;
        let mut segments = Vec::new();

        // Handle leading ::
        if self.consume(TokenKind::ColonColon) {
            // Global path
        }

        loop {
            let ident = if self.check(TokenKind::SelfUpper) {
                let token = self.advance();
                Identifier::new("Self", token.span)
            } else {
                self.parse_identifier()?
            };

            let args = if self.check(TokenKind::Lt) {
                Some(self.parse_generic_args()?)
            } else {
                None
            };

            segments.push(PathSegment { ident, args });

            if !self.consume(TokenKind::ColonColon) {
                break;
            }
        }

        let span = start.merge(self.current.span);
        Ok(TypePath { segments, span })
    }

    /// Parse generic arguments.
    fn parse_generic_args(&mut self) -> Result<GenericArgs, ParseError> {
        let start = self.current.span;
        self.expect(TokenKind::Lt)?;

        let mut args = Vec::new();
        while !self.check(TokenKind::Gt) && !self.check(TokenKind::Eof) {
            // Try to parse as type, fall back to const expr
            args.push(GenericArg::Type(self.parse_type()?));

            if !self.consume(TokenKind::Comma) {
                break;
            }
        }

        self.expect(TokenKind::Gt)?;
        let span = start.merge(self.current.span);

        Ok(GenericArgs { args, span })
    }

    // ---
    // Statements
    // ---

    /// Parse a block.
    pub fn parse_block(&mut self) -> Result<Block, ParseError> {
        let start = self.current.span;
        self.expect(TokenKind::LBrace)?;

        let mut stmts = Vec::new();
        while !self.check(TokenKind::RBrace) && !self.check(TokenKind::Eof) {
            match self.parse_statement() {
                Ok(stmt) => stmts.push(stmt),
                Err(_) => {
                    // Error recovery: skip to semicolon or brace
                    while !self.check_any(&[
                        TokenKind::Semicolon,
                        TokenKind::RBrace,
                        TokenKind::Eof,
                    ]) {
                        self.advance();
                    }
                    self.consume(TokenKind::Semicolon);
                }
            }
        }

        let end = self.expect(TokenKind::RBrace)?;
        Ok(Block {
            stmts,
            span: start.merge(end.span),
        })
    }

    /// Parse a statement.
    fn parse_statement(&mut self) -> Result<Stmt, ParseError> {
        let start = self.current.span;

        // Let statement
        if self.check(TokenKind::Let) {
            return self.parse_let_statement();
        }

        // Item in block
        if self.check_any(&[
            TokenKind::Fn,
            TokenKind::Struct,
            TokenKind::Enum,
            TokenKind::Impl,
            TokenKind::Trait,
            TokenKind::Type,
            TokenKind::Const,
            TokenKind::Use,
            TokenKind::Mod,
            TokenKind::Pub,
        ]) {
            let item = self.parse_item()?;
            return Ok(Stmt {
                kind: StmtKind::Item(Box::new(item)),
                span: start.merge(self.current.span),
            });
        }

        // Empty statement
        if self.consume(TokenKind::Semicolon) {
            return Ok(Stmt {
                kind: StmtKind::Empty,
                span: start,
            });
        }

        // Expression statement
        let expr = self.parse_expression()?;
        let span = start.merge(expr.span);

        if self.consume(TokenKind::Semicolon) {
            Ok(Stmt {
                kind: StmtKind::Expr(expr),
                span,
            })
        } else {
            // Expression without semicolon (tail expression)
            Ok(Stmt {
                kind: StmtKind::ExprNoSemi(expr),
                span,
            })
        }
    }

    /// Parse a let statement.
    fn parse_let_statement(&mut self) -> Result<Stmt, ParseError> {
        let start = self.current.span;
        self.expect(TokenKind::Let)?;

        let pattern = self.parse_pattern()?;

        let ty = if self.consume(TokenKind::Colon) {
            Some(self.parse_type()?)
        } else {
            None
        };

        let init = if self.consume(TokenKind::Eq) {
            Some(self.parse_expression()?)
        } else {
            None
        };

        self.expect(TokenKind::Semicolon)?;

        Ok(Stmt {
            kind: StmtKind::Let { pattern, ty, init },
            span: start.merge(self.current.span),
        })
    }

    // ---
    // Expressions (Pratt parser)
    // ---

    /// Parse an expression.
    pub fn parse_expression(&mut self) -> Result<Expr, ParseError> {
        self.parse_expr_with_restrictions(Precedence::Lowest, false)
    }

    /// Parse an expression where struct literals are not allowed.
    /// Used in if/while conditions, match scrutinees, etc.
    fn parse_expression_no_struct(&mut self) -> Result<Expr, ParseError> {
        self.parse_expr_with_restrictions(Precedence::Lowest, true)
    }

    /// Parse an expression with minimum precedence and optional struct literal restriction.
    fn parse_expr_with_restrictions(
        &mut self,
        min_prec: Precedence,
        no_struct: bool,
    ) -> Result<Expr, ParseError> {
        let mut left = self.parse_prefix_expr_with_restrictions(no_struct)?;

        while let Some(prec) = Precedence::of_infix_token(self.current.kind) {
            if prec < min_prec {
                break;
            }

            // Handle right associativity
            let next_prec = if prec.is_right_associative() {
                prec
            } else {
                Precedence::from(prec as u8 + 1)
            };

            left = self.parse_infix_expr(left, next_prec)?;
        }

        Ok(left)
    }

    /// Parse an expression with minimum precedence.
    fn parse_expr_precedence(&mut self, min_prec: Precedence) -> Result<Expr, ParseError> {
        let mut left = self.parse_prefix_expr()?;

        while let Some(prec) = Precedence::of_infix_token(self.current.kind) {
            if prec < min_prec {
                break;
            }

            // Handle right associativity
            let next_prec = if prec.is_right_associative() {
                prec
            } else {
                Precedence::from(prec as u8 + 1)
            };

            left = self.parse_infix_expr(left, next_prec)?;
        }

        Ok(left)
    }

    /// Parse a prefix expression.
    fn parse_prefix_expr(&mut self) -> Result<Expr, ParseError> {
        self.parse_prefix_expr_with_restrictions(false)
    }

    /// Parse a prefix expression with optional struct literal restriction.
    fn parse_prefix_expr_with_restrictions(&mut self, no_struct: bool) -> Result<Expr, ParseError> {
        let start = self.current.span;

        match self.current.kind {
            // Literals
            TokenKind::IntLiteral => {
                let token = self.advance();
                let s = self.slice(token.span);
                let value = parse_int(s).map_err(|_| ParseError::Custom {
                    message: "invalid integer literal".to_string(),
                    span: token.span,
                })?;
                Ok(Expr {
                    kind: ExprKind::Literal(Literal::Int(value)),
                    span: token.span,
                })
            }
            TokenKind::FloatLiteral => {
                let token = self.advance();
                let s = self.slice(token.span);
                let value = parse_float(s).map_err(|_| ParseError::Custom {
                    message: "invalid float literal".to_string(),
                    span: token.span,
                })?;
                Ok(Expr {
                    kind: ExprKind::Literal(Literal::Float(value)),
                    span: token.span,
                })
            }
            TokenKind::StringLiteral => {
                let token = self.advance();
                let s = self.slice(token.span);
                let value = parse_string(s).map_err(|e| ParseError::Custom {
                    message: e.to_string(),
                    span: token.span,
                })?;
                Ok(Expr {
                    kind: ExprKind::Literal(Literal::String(value)),
                    span: token.span,
                })
            }
            TokenKind::FStringLiteral => {
                let token = self.advance();
                let s = self.slice(token.span);
                let parts = self.parse_format_string(s, token.span)?;
                Ok(Expr {
                    kind: ExprKind::FormatString { parts },
                    span: token.span,
                })
            }
            TokenKind::CharLiteral => {
                let token = self.advance();
                let s = self.slice(token.span);
                let value = parse_char(s).map_err(|e| ParseError::Custom {
                    message: e.to_string(),
                    span: token.span,
                })?;
                Ok(Expr {
                    kind: ExprKind::Literal(Literal::Char(value)),
                    span: token.span,
                })
            }
            TokenKind::True => {
                let token = self.advance();
                Ok(Expr {
                    kind: ExprKind::Literal(Literal::Bool(true)),
                    span: token.span,
                })
            }
            TokenKind::False => {
                let token = self.advance();
                Ok(Expr {
                    kind: ExprKind::Literal(Literal::Bool(false)),
                    span: token.span,
                })
            }

            // Unary operators
            TokenKind::Minus => {
                self.advance();
                let operand = self.parse_expr_precedence(Precedence::Unary)?;
                let span = start.merge(operand.span);
                Ok(Expr {
                    kind: ExprKind::Unary {
                        op: UnaryOp::Neg,
                        operand: Box::new(operand),
                    },
                    span,
                })
            }
            TokenKind::Bang => {
                self.advance();
                let operand = self.parse_expr_precedence(Precedence::Unary)?;
                let span = start.merge(operand.span);
                Ok(Expr {
                    kind: ExprKind::Unary {
                        op: UnaryOp::Not,
                        operand: Box::new(operand),
                    },
                    span,
                })
            }
            TokenKind::Tilde => {
                self.advance();
                let operand = self.parse_expr_precedence(Precedence::Unary)?;
                let span = start.merge(operand.span);
                Ok(Expr {
                    kind: ExprKind::Unary {
                        op: UnaryOp::BitNot,
                        operand: Box::new(operand),
                    },
                    span,
                })
            }
            TokenKind::Star => {
                // Dereference
                self.advance();
                let operand = self.parse_expr_precedence(Precedence::Unary)?;
                let span = start.merge(operand.span);
                Ok(Expr {
                    kind: ExprKind::Dereference {
                        operand: Box::new(operand),
                    },
                    span,
                })
            }
            TokenKind::And => {
                // Reference
                self.advance();
                let mutable = self.consume(TokenKind::Mut);
                let operand = self.parse_expr_precedence(Precedence::Unary)?;
                let span = start.merge(operand.span);
                Ok(Expr {
                    kind: ExprKind::Reference {
                        mutable,
                        operand: Box::new(operand),
                    },
                    span,
                })
            }

            // Grouped expression or tuple
            TokenKind::LParen => {
                self.advance();
                if self.consume(TokenKind::RParen) {
                    // Unit literal
                    return Ok(Expr {
                        kind: ExprKind::Literal(Literal::Unit),
                        span: start.merge(self.current.span),
                    });
                }

                let first = self.parse_expression()?;

                if self.consume(TokenKind::Comma) {
                    // Tuple
                    let mut elements = vec![first];
                    while !self.check(TokenKind::RParen) && !self.check(TokenKind::Eof) {
                        elements.push(self.parse_expression()?);
                        if !self.consume(TokenKind::Comma) {
                            break;
                        }
                    }
                    let end = self.expect(TokenKind::RParen)?;
                    Ok(Expr {
                        kind: ExprKind::Tuple(elements),
                        span: start.merge(end.span),
                    })
                } else {
                    // Grouped expression
                    let end = self.expect(TokenKind::RParen)?;
                    Ok(Expr {
                        kind: ExprKind::Grouped(Box::new(first)),
                        span: start.merge(end.span),
                    })
                }
            }

            // Array
            TokenKind::LBracket => {
                self.advance();
                if self.consume(TokenKind::RBracket) {
                    return Ok(Expr {
                        kind: ExprKind::Array(Vec::new()),
                        span: start.merge(self.current.span),
                    });
                }

                let first = self.parse_expression()?;

                if self.consume(TokenKind::Semicolon) {
                    // Array repeat
                    let count = self.parse_expression()?;
                    let end = self.expect(TokenKind::RBracket)?;
                    Ok(Expr {
                        kind: ExprKind::ArrayRepeat {
                            value: Box::new(first),
                            count: Box::new(count),
                        },
                        span: start.merge(end.span),
                    })
                } else {
                    // Array literal
                    let mut elements = vec![first];
                    while self.consume(TokenKind::Comma) {
                        if self.check(TokenKind::RBracket) {
                            break;
                        }
                        elements.push(self.parse_expression()?);
                    }
                    let end = self.expect(TokenKind::RBracket)?;
                    Ok(Expr {
                        kind: ExprKind::Array(elements),
                        span: start.merge(end.span),
                    })
                }
            }

            // Block
            TokenKind::LBrace => {
                let block = self.parse_block()?;
                let span = block.span;
                Ok(Expr {
                    kind: ExprKind::Block(block),
                    span,
                })
            }

            // If expression
            TokenKind::If => self.parse_if_expr(),

            // Match expression
            TokenKind::Match => self.parse_match_expr(),

            // Loop
            TokenKind::Loop => self.parse_loop_expr(None),

            // While loop
            TokenKind::While => self.parse_while_expr(None),

            // For loop
            TokenKind::For => self.parse_for_expr(None),

            // Return
            TokenKind::Return => {
                self.advance();
                let value = if self.check_any(&[
                    TokenKind::Semicolon,
                    TokenKind::RBrace,
                    TokenKind::Comma,
                    TokenKind::Eof,
                ]) {
                    None
                } else {
                    Some(Box::new(self.parse_expression()?))
                };
                let span = start.merge(self.current.span);
                Ok(Expr {
                    kind: ExprKind::Return { value },
                    span,
                })
            }

            // Break
            TokenKind::Break => {
                self.advance();
                let label = None;
                let value = if self.check_any(&[
                    TokenKind::Semicolon,
                    TokenKind::RBrace,
                    TokenKind::Comma,
                    TokenKind::Eof,
                ]) {
                    None
                } else {
                    Some(Box::new(self.parse_expression()?))
                };
                let span = start.merge(self.current.span);
                Ok(Expr {
                    kind: ExprKind::Break { label, value },
                    span,
                })
            }

            // Continue
            TokenKind::Continue => {
                self.advance();
                let label = None;
                let span = start.merge(self.current.span);
                Ok(Expr {
                    kind: ExprKind::Continue { label },
                    span,
                })
            }

            // Closure
            TokenKind::OrOr => {
                // Empty closure params: ||
                self.advance();
                self.parse_closure_body(Vec::new(), start)
            }
            TokenKind::Or => {
                // Closure with params: |x, y|
                self.advance();
                let params = self.parse_closure_params()?;
                self.expect(TokenKind::Or)?;
                self.parse_closure_body(params, start)
            }
            TokenKind::Move => {
                self.advance();
                if self.check(TokenKind::OrOr) {
                    self.advance();
                    self.parse_closure_body(Vec::new(), start)
                } else {
                    self.expect(TokenKind::Or)?;
                    let params = self.parse_closure_params()?;
                    self.expect(TokenKind::Or)?;
                    self.parse_closure_body(params, start)
                }
            }

            // Async block: async { ... }
            TokenKind::Async => {
                self.advance();
                let block = self.parse_block()?;
                let span = start.merge(block.span);
                Ok(Expr {
                    kind: ExprKind::AsyncBlock(block),
                    span,
                })
            }

            // Path or struct literal
            TokenKind::Identifier
            | TokenKind::SelfLower
            | TokenKind::SelfUpper
            | TokenKind::ColonColon => self.parse_path_or_struct_expr(no_struct),

            // Range with no start
            TokenKind::DotDot | TokenKind::DotDotEq => {
                let inclusive = self.current.kind == TokenKind::DotDotEq;
                self.advance();
                let end = if self.current.kind.can_start_expr() {
                    Some(Box::new(self.parse_expr_precedence(Precedence::Range)?))
                } else {
                    None
                };
                let span = start.merge(self.current.span);
                Ok(Expr {
                    kind: ExprKind::Range {
                        start: None,
                        end,
                        inclusive,
                    },
                    span,
                })
            }

            _ => {
                let err = ParseError::UnexpectedToken {
                    expected: "expression".to_string(),
                    found: self.current.kind.name().to_string(),
                    span: self.current.span,
                };
                self.errors.push(err.clone());
                Err(err)
            }
        }
    }

    /// Parse an infix expression.
    fn parse_infix_expr(&mut self, left: Expr, min_prec: Precedence) -> Result<Expr, ParseError> {
        let op_token = self.current.kind;
        let start = left.span;

        match op_token {
            // Binary operators
            _ if token_to_binary_op(op_token).is_some() => {
                let op = token_to_binary_op(op_token).unwrap();
                self.advance();
                let right = self.parse_expr_precedence(min_prec)?;
                let span = start.merge(right.span);
                Ok(Expr {
                    kind: ExprKind::Binary {
                        op,
                        left: Box::new(left),
                        right: Box::new(right),
                    },
                    span,
                })
            }

            // Assignment
            TokenKind::Eq => {
                self.advance();
                let value = self.parse_expr_precedence(min_prec)?;
                let span = start.merge(value.span);
                Ok(Expr {
                    kind: ExprKind::Assign {
                        target: Box::new(left),
                        value: Box::new(value),
                    },
                    span,
                })
            }

            // Compound assignment
            _ if compound_assign_to_op(op_token).is_some() => {
                let op = compound_assign_to_op(op_token).unwrap();
                self.advance();
                let value = self.parse_expr_precedence(min_prec)?;
                let span = start.merge(value.span);
                Ok(Expr {
                    kind: ExprKind::AssignOp {
                        op,
                        target: Box::new(left),
                        value: Box::new(value),
                    },
                    span,
                })
            }

            // Range
            TokenKind::DotDot | TokenKind::DotDotEq => {
                let inclusive = op_token == TokenKind::DotDotEq;
                self.advance();
                let end = if self.current.kind.can_start_expr() {
                    Some(Box::new(self.parse_expr_precedence(Precedence::Range)?))
                } else {
                    None
                };
                let span = start.merge(self.current.span);
                Ok(Expr {
                    kind: ExprKind::Range {
                        start: Some(Box::new(left)),
                        end,
                        inclusive,
                    },
                    span,
                })
            }

            // Type cast
            TokenKind::As => {
                self.advance();
                let ty = self.parse_type()?;
                let span = start.merge(ty.span);
                Ok(Expr {
                    kind: ExprKind::Cast {
                        expr: Box::new(left),
                        ty,
                    },
                    span,
                })
            }

            // Function call
            TokenKind::LParen => {
                self.advance();
                let args = self.parse_call_args()?;
                let end = self.expect(TokenKind::RParen)?;
                let span = start.merge(end.span);
                Ok(Expr {
                    kind: ExprKind::Call {
                        callee: Box::new(left),
                        args,
                    },
                    span,
                })
            }

            // Index
            TokenKind::LBracket => {
                self.advance();
                let index = self.parse_expression()?;
                let end = self.expect(TokenKind::RBracket)?;
                let span = start.merge(end.span);
                Ok(Expr {
                    kind: ExprKind::Index {
                        base: Box::new(left),
                        index: Box::new(index),
                    },
                    span,
                })
            }

            // Field access, method call, or await
            TokenKind::Dot => {
                self.advance();

                // Check for tuple field access (e.g., tuple.0)
                if self.check(TokenKind::IntLiteral) {
                    let token = self.advance();
                    let s = self.slice(token.span);
                    let field = Identifier::new(s, token.span);
                    let span = start.merge(token.span);
                    return Ok(Expr {
                        kind: ExprKind::Field {
                            base: Box::new(left),
                            field,
                        },
                        span,
                    });
                }

                // Check for await: expr.await
                if self.check(TokenKind::Await) {
                    let await_token = self.advance();
                    let span = start.merge(await_token.span);
                    return Ok(Expr {
                        kind: ExprKind::Await {
                            operand: Box::new(left),
                        },
                        span,
                    });
                }

                let field = self.parse_identifier()?;

                if self.check(TokenKind::LParen) {
                    // Method call
                    self.advance();
                    let args = self.parse_call_args()?;
                    let end = self.expect(TokenKind::RParen)?;
                    let span = start.merge(end.span);
                    Ok(Expr {
                        kind: ExprKind::MethodCall {
                            receiver: Box::new(left),
                            method: field,
                            args,
                        },
                        span,
                    })
                } else {
                    // Field access
                    let span = start.merge(field.span);
                    Ok(Expr {
                        kind: ExprKind::Field {
                            base: Box::new(left),
                            field,
                        },
                        span,
                    })
                }
            }

            // Try operator
            TokenKind::Question => {
                self.advance();
                let span = start.merge(self.current.span);
                Ok(Expr {
                    kind: ExprKind::Try {
                        operand: Box::new(left),
                    },
                    span,
                })
            }

            _ => Ok(left),
        }
    }

    /// Parse call arguments.
    fn parse_call_args(&mut self) -> Result<Vec<CallArg>, ParseError> {
        let mut args = Vec::new();

        while !self.check(TokenKind::RParen) && !self.check(TokenKind::Eof) {
            let start = self.current.span;

            // Check for named argument: `name: value`
            let (name, value) =
                if self.check(TokenKind::Identifier) && self.peek_is(TokenKind::Colon) {
                    let ident = self.parse_identifier()?;
                    self.expect(TokenKind::Colon)?;
                    let value = self.parse_expression()?;
                    (Some(ident), value)
                } else {
                    (None, self.parse_expression()?)
                };

            let span = start.merge(value.span);
            args.push(CallArg { name, value, span });

            if !self.consume(TokenKind::Comma) {
                break;
            }
        }

        Ok(args)
    }

    /// Parse an if expression.
    fn parse_if_expr(&mut self) -> Result<Expr, ParseError> {
        let start = self.current.span;
        self.expect(TokenKind::If)?;

        // Struct literals not allowed in if condition (ambiguity with block)
        let condition = self.parse_expression_no_struct()?;
        let then_branch = self.parse_block()?;

        let else_branch = if self.consume(TokenKind::Else) {
            if self.check(TokenKind::If) {
                Some(Box::new(self.parse_if_expr()?))
            } else {
                let block = self.parse_block()?;
                let span = block.span;
                Some(Box::new(Expr {
                    kind: ExprKind::Block(block),
                    span,
                }))
            }
        } else {
            None
        };

        let span = start.merge(self.current.span);
        Ok(Expr {
            kind: ExprKind::If {
                condition: Box::new(condition),
                then_branch,
                else_branch,
            },
            span,
        })
    }

    /// Parse a match expression.
    fn parse_match_expr(&mut self) -> Result<Expr, ParseError> {
        let start = self.current.span;
        self.expect(TokenKind::Match)?;

        // Struct literals not allowed in match scrutinee (ambiguity with match body)
        let scrutinee = self.parse_expression_no_struct()?;
        self.expect(TokenKind::LBrace)?;

        let mut arms = Vec::new();
        while !self.check(TokenKind::RBrace) && !self.check(TokenKind::Eof) {
            arms.push(self.parse_match_arm()?);

            // Allow trailing comma
            self.consume(TokenKind::Comma);
        }

        let end = self.expect(TokenKind::RBrace)?;
        Ok(Expr {
            kind: ExprKind::Match {
                scrutinee: Box::new(scrutinee),
                arms,
            },
            span: start.merge(end.span),
        })
    }

    /// Parse a match arm.
    fn parse_match_arm(&mut self) -> Result<MatchArm, ParseError> {
        let start = self.current.span;
        let pattern = self.parse_pattern()?;

        let guard = if self.consume(TokenKind::If) {
            Some(Box::new(self.parse_expression()?))
        } else {
            None
        };

        self.expect(TokenKind::FatArrow)?;
        let body = self.parse_expression()?;

        let span = start.merge(body.span);
        Ok(MatchArm {
            pattern,
            guard,
            body,
            span,
        })
    }

    /// Parse a loop expression.
    fn parse_loop_expr(&mut self, label: Option<Identifier>) -> Result<Expr, ParseError> {
        let start = self.current.span;
        self.expect(TokenKind::Loop)?;

        let body = self.parse_block()?;
        let span = start.merge(body.span);

        Ok(Expr {
            kind: ExprKind::Loop { label, body },
            span,
        })
    }

    /// Parse a while loop expression.
    fn parse_while_expr(&mut self, label: Option<Identifier>) -> Result<Expr, ParseError> {
        let start = self.current.span;
        self.expect(TokenKind::While)?;

        // Struct literals not allowed in while condition (ambiguity with body)
        let condition = self.parse_expression_no_struct()?;
        let body = self.parse_block()?;
        let span = start.merge(body.span);

        Ok(Expr {
            kind: ExprKind::While {
                label,
                condition: Box::new(condition),
                body,
            },
            span,
        })
    }

    /// Parse a for loop expression.
    fn parse_for_expr(&mut self, label: Option<Identifier>) -> Result<Expr, ParseError> {
        let start = self.current.span;
        self.expect(TokenKind::For)?;

        let pattern = self.parse_pattern()?;
        self.expect(TokenKind::In)?;
        // Use no_struct to prevent `arr { ... }` being parsed as struct literal
        let iterable = self.parse_expression_no_struct()?;
        let body = self.parse_block()?;
        let span = start.merge(body.span);

        Ok(Expr {
            kind: ExprKind::For {
                label,
                pattern,
                iterable: Box::new(iterable),
                body,
            },
            span,
        })
    }

    /// Parse closure parameters.
    fn parse_closure_params(&mut self) -> Result<Vec<ClosureParam>, ParseError> {
        let mut params = Vec::new();

        while !self.check(TokenKind::Or) && !self.check(TokenKind::Eof) {
            let start = self.current.span;
            // Use parse_pattern_no_or to avoid consuming | as or-pattern
            let pattern = self.parse_pattern_no_or()?;

            let ty = if self.consume(TokenKind::Colon) {
                Some(self.parse_type()?)
            } else {
                None
            };

            let span = start.merge(self.current.span);
            params.push(ClosureParam { pattern, ty, span });

            if !self.consume(TokenKind::Comma) {
                break;
            }
        }

        Ok(params)
    }

    /// Parse closure body.
    fn parse_closure_body(
        &mut self,
        params: Vec<ClosureParam>,
        start: Span,
    ) -> Result<Expr, ParseError> {
        let return_type = if self.consume(TokenKind::Arrow) {
            Some(Box::new(self.parse_type()?))
        } else {
            None
        };

        let body = self.parse_expression()?;
        let span = start.merge(body.span);

        Ok(Expr {
            kind: ExprKind::Closure {
                params,
                return_type,
                body: Box::new(body),
            },
            span,
        })
    }

    /// Parse a path expression or struct literal.
    fn parse_path_or_struct_expr(&mut self, no_struct: bool) -> Result<Expr, ParseError> {
        let path = self.parse_expr_path()?;

        // Check for struct literal (unless restricted)
        if !no_struct && self.check(TokenKind::LBrace) {
            // Need to be careful here - could be a block after the path
            // Struct literals are only allowed in certain contexts
            let type_path = TypePath {
                segments: path.segments.clone(),
                span: path.span,
            };
            return self.parse_struct_expr(type_path, path.span);
        }

        Ok(Expr {
            kind: ExprKind::Path(path.clone()),
            span: path.span,
        })
    }

    /// Parse an expression path.
    fn parse_expr_path(&mut self) -> Result<ExprPath, ParseError> {
        let start = self.current.span;
        let mut segments = Vec::new();

        // Handle leading ::
        if self.consume(TokenKind::ColonColon) {
            // Global path
        }

        loop {
            let ident = if self.check(TokenKind::SelfLower) {
                let token = self.advance();
                Identifier::new("self", token.span)
            } else if self.check(TokenKind::SelfUpper) {
                let token = self.advance();
                Identifier::new("Self", token.span)
            } else {
                self.parse_identifier()?
            };

            // Check for turbofish: path::<T>
            let args = if self.check(TokenKind::ColonColon) && self.lexer.check(TokenKind::Lt) {
                self.advance(); // ::
                Some(self.parse_generic_args()?)
            } else {
                None
            };

            segments.push(PathSegment { ident, args });

            if !self.consume(TokenKind::ColonColon) {
                break;
            }
        }

        let span = start.merge(self.current.span);
        Ok(ExprPath { segments, span })
    }

    /// Parse a struct literal expression.
    fn parse_struct_expr(&mut self, path: TypePath, start: Span) -> Result<Expr, ParseError> {
        self.expect(TokenKind::LBrace)?;

        let mut fields = Vec::new();
        let mut rest = None;

        while !self.check(TokenKind::RBrace) && !self.check(TokenKind::Eof) {
            if self.consume(TokenKind::DotDot) {
                rest = Some(Box::new(self.parse_expression()?));
                break;
            }

            let field_start = self.current.span;
            let name = self.parse_identifier()?;

            let value = if self.consume(TokenKind::Colon) {
                Some(self.parse_expression()?)
            } else {
                None
            };

            let span = field_start.merge(self.current.span);
            fields.push(StructExprField { name, value, span });

            if !self.consume(TokenKind::Comma) {
                break;
            }
        }

        let end = self.expect(TokenKind::RBrace)?;
        Ok(Expr {
            kind: ExprKind::Struct { path, fields, rest },
            span: start.merge(end.span),
        })
    }

    // ---
    // Patterns
    // ---

    /// Parse a pattern.
    pub fn parse_pattern(&mut self) -> Result<Pattern, ParseError> {
        self.parse_pattern_or()
    }

    /// Parse a pattern without allowing top-level or-patterns.
    /// Used in contexts where `|` has another meaning (e.g., closure params).
    fn parse_pattern_no_or(&mut self) -> Result<Pattern, ParseError> {
        self.parse_pattern_primary()
    }

    /// Parse an or-pattern.
    fn parse_pattern_or(&mut self) -> Result<Pattern, ParseError> {
        let start = self.current.span;
        let mut patterns = vec![self.parse_pattern_primary()?];

        while self.consume(TokenKind::Or) {
            patterns.push(self.parse_pattern_primary()?);
        }

        if patterns.len() == 1 {
            Ok(patterns.pop().unwrap())
        } else {
            let span = start.merge(patterns.last().unwrap().span);
            Ok(Pattern {
                kind: PatternKind::Or(patterns),
                span,
            })
        }
    }

    /// Parse a primary pattern.
    fn parse_pattern_primary(&mut self) -> Result<Pattern, ParseError> {
        let start = self.current.span;

        match self.current.kind {
            // Wildcard
            TokenKind::Identifier if self.slice(self.current.span) == "_" => {
                self.advance();
                Ok(Pattern {
                    kind: PatternKind::Wildcard,
                    span: start,
                })
            }

            // Rest pattern
            TokenKind::DotDot => {
                self.advance();
                Ok(Pattern {
                    kind: PatternKind::Rest,
                    span: start,
                })
            }

            // Reference pattern
            TokenKind::And => {
                self.advance();
                let mutable = self.consume(TokenKind::Mut);
                let inner = self.parse_pattern()?;
                let span = start.merge(inner.span);
                Ok(Pattern {
                    kind: PatternKind::Reference {
                        mutable,
                        pattern: Box::new(inner),
                    },
                    span,
                })
            }

            // Binding with modifiers
            TokenKind::Mut | TokenKind::Ref => {
                let mutable = self.consume(TokenKind::Mut);
                let by_ref = self.consume(TokenKind::Ref);
                let name = self.parse_identifier()?;

                let subpattern = if self.consume(TokenKind::At) {
                    Some(Box::new(self.parse_pattern()?))
                } else {
                    None
                };

                let span = start.merge(self.current.span);
                Ok(Pattern {
                    kind: PatternKind::Binding {
                        mutable,
                        by_ref,
                        name,
                        subpattern,
                    },
                    span,
                })
            }

            // Literal patterns
            TokenKind::IntLiteral => {
                let token = self.advance();
                let s = self.slice(token.span);
                let value = parse_int(s).unwrap_or(0);
                Ok(Pattern {
                    kind: PatternKind::Literal(Literal::Int(value)),
                    span: token.span,
                })
            }
            TokenKind::FloatLiteral => {
                let token = self.advance();
                let s = self.slice(token.span);
                let value = parse_float(s).unwrap_or(0.0);
                Ok(Pattern {
                    kind: PatternKind::Literal(Literal::Float(value)),
                    span: token.span,
                })
            }
            TokenKind::StringLiteral => {
                let token = self.advance();
                let s = self.slice(token.span);
                let value = parse_string(s).unwrap_or_default();
                Ok(Pattern {
                    kind: PatternKind::Literal(Literal::String(value)),
                    span: token.span,
                })
            }
            TokenKind::CharLiteral => {
                let token = self.advance();
                let s = self.slice(token.span);
                let value = parse_char(s).unwrap_or('\0');
                Ok(Pattern {
                    kind: PatternKind::Literal(Literal::Char(value)),
                    span: token.span,
                })
            }
            TokenKind::True => {
                let token = self.advance();
                Ok(Pattern {
                    kind: PatternKind::Literal(Literal::Bool(true)),
                    span: token.span,
                })
            }
            TokenKind::False => {
                let token = self.advance();
                Ok(Pattern {
                    kind: PatternKind::Literal(Literal::Bool(false)),
                    span: token.span,
                })
            }

            // Tuple pattern
            TokenKind::LParen => {
                self.advance();
                if self.consume(TokenKind::RParen) {
                    return Ok(Pattern {
                        kind: PatternKind::Literal(Literal::Unit),
                        span: start.merge(self.current.span),
                    });
                }

                let mut patterns = vec![self.parse_pattern()?];
                while self.consume(TokenKind::Comma) {
                    if self.check(TokenKind::RParen) {
                        break;
                    }
                    patterns.push(self.parse_pattern()?);
                }
                let end = self.expect(TokenKind::RParen)?;

                if patterns.len() == 1 {
                    // Single element - just grouping
                    Ok(patterns.pop().unwrap())
                } else {
                    Ok(Pattern {
                        kind: PatternKind::Tuple(patterns),
                        span: start.merge(end.span),
                    })
                }
            }

            // Slice pattern
            TokenKind::LBracket => {
                self.advance();
                let mut patterns = Vec::new();
                while !self.check(TokenKind::RBracket) && !self.check(TokenKind::Eof) {
                    patterns.push(self.parse_pattern()?);
                    if !self.consume(TokenKind::Comma) {
                        break;
                    }
                }
                let end = self.expect(TokenKind::RBracket)?;
                Ok(Pattern {
                    kind: PatternKind::Slice(patterns),
                    span: start.merge(end.span),
                })
            }

            // Path pattern (including identifiers, struct patterns, tuple struct patterns)
            TokenKind::Identifier | TokenKind::SelfUpper | TokenKind::ColonColon => {
                self.parse_path_pattern()
            }

            _ => {
                let err = ParseError::UnexpectedToken {
                    expected: "pattern".to_string(),
                    found: self.current.kind.name().to_string(),
                    span: self.current.span,
                };
                self.errors.push(err.clone());
                Err(err)
            }
        }
    }

    /// Parse a path-based pattern.
    fn parse_path_pattern(&mut self) -> Result<Pattern, ParseError> {
        let path = self.parse_expr_path()?;
        let start = path.span;

        // Check if this is a simple binding (single identifier, no path separators)
        if path.segments.len() == 1 && path.segments[0].args.is_none() {
            let name = path.segments[0].ident.clone();

            // Check for @ subpattern
            if self.consume(TokenKind::At) {
                let subpattern = self.parse_pattern()?;
                let span = start.merge(subpattern.span);
                return Ok(Pattern {
                    kind: PatternKind::Binding {
                        mutable: false,
                        by_ref: false,
                        name,
                        subpattern: Some(Box::new(subpattern)),
                    },
                    span,
                });
            }

            // Check for struct pattern or tuple struct pattern
            if self.check(TokenKind::LBrace) {
                return self.parse_struct_pattern(
                    TypePath {
                        segments: path.segments,
                        span: path.span,
                    },
                    start,
                );
            }

            if self.check(TokenKind::LParen) {
                return self.parse_tuple_struct_pattern(
                    TypePath {
                        segments: path.segments,
                        span: path.span,
                    },
                    start,
                );
            }

            // Simple binding
            return Ok(Pattern {
                kind: PatternKind::Binding {
                    mutable: false,
                    by_ref: false,
                    name,
                    subpattern: None,
                },
                span: start,
            });
        }

        // Multi-segment path
        let type_path = TypePath {
            segments: path.segments.clone(),
            span: path.span,
        };

        if self.check(TokenKind::LBrace) {
            self.parse_struct_pattern(type_path, start)
        } else if self.check(TokenKind::LParen) {
            self.parse_tuple_struct_pattern(type_path, start)
        } else {
            Ok(Pattern {
                kind: PatternKind::Path(path),
                span: start,
            })
        }
    }

    /// Parse a struct pattern.
    fn parse_struct_pattern(&mut self, path: TypePath, start: Span) -> Result<Pattern, ParseError> {
        self.expect(TokenKind::LBrace)?;

        let mut fields = Vec::new();
        let mut rest = false;

        while !self.check(TokenKind::RBrace) && !self.check(TokenKind::Eof) {
            if self.consume(TokenKind::DotDot) {
                rest = true;
                break;
            }

            let field_start = self.current.span;
            let name = self.parse_identifier()?;

            let pattern = if self.consume(TokenKind::Colon) {
                Some(self.parse_pattern()?)
            } else {
                None
            };

            let span = field_start.merge(self.current.span);
            fields.push(PatternField {
                name,
                pattern,
                span,
            });

            if !self.consume(TokenKind::Comma) {
                break;
            }
        }

        let end = self.expect(TokenKind::RBrace)?;
        Ok(Pattern {
            kind: PatternKind::Struct { path, fields, rest },
            span: start.merge(end.span),
        })
    }

    /// Parse a tuple struct pattern.
    fn parse_tuple_struct_pattern(
        &mut self,
        path: TypePath,
        start: Span,
    ) -> Result<Pattern, ParseError> {
        self.expect(TokenKind::LParen)?;

        let mut fields = Vec::new();
        while !self.check(TokenKind::RParen) && !self.check(TokenKind::Eof) {
            fields.push(self.parse_pattern()?);
            if !self.consume(TokenKind::Comma) {
                break;
            }
        }

        let end = self.expect(TokenKind::RParen)?;
        Ok(Pattern {
            kind: PatternKind::TupleStruct { path, fields },
            span: start.merge(end.span),
        })
    }

    /// Parse a format string like `f"Hello {name}!"` into parts.
    fn parse_format_string(&mut self, s: &str, span: Span) -> Result<Vec<FormatPart>, ParseError> {
        // Strip the f" prefix and " suffix
        let content = &s[2..s.len() - 1];
        let mut parts = Vec::new();
        let mut current_literal = String::new();
        let mut chars = content.chars().peekable();

        while let Some(c) = chars.next() {
            if c == '\\' {
                // Handle escape sequences
                if let Some(&next) = chars.peek() {
                    match next {
                        '{' | '}' => {
                            // Escaped brace - add literal brace
                            current_literal.push(chars.next().unwrap());
                        }
                        'n' => {
                            chars.next();
                            current_literal.push('\n');
                        }
                        'r' => {
                            chars.next();
                            current_literal.push('\r');
                        }
                        't' => {
                            chars.next();
                            current_literal.push('\t');
                        }
                        '\\' => {
                            chars.next();
                            current_literal.push('\\');
                        }
                        '"' => {
                            chars.next();
                            current_literal.push('"');
                        }
                        '0' => {
                            chars.next();
                            current_literal.push('\0');
                        }
                        _ => {
                            // Unknown escape, keep as-is
                            current_literal.push(c);
                        }
                    }
                } else {
                    current_literal.push(c);
                }
            } else if c == '{' {
                // Start of interpolation
                // First, save any accumulated literal
                if !current_literal.is_empty() {
                    parts.push(FormatPart::Literal(std::mem::take(&mut current_literal)));
                }

                // Collect the expression content until matching '}'
                let mut expr_content = String::new();
                let mut brace_depth = 1;

                while let Some(c) = chars.next() {
                    if c == '{' {
                        brace_depth += 1;
                        expr_content.push(c);
                    } else if c == '}' {
                        brace_depth -= 1;
                        if brace_depth == 0 {
                            break;
                        }
                        expr_content.push(c);
                    } else if c == '\\' && chars.peek() == Some(&'}') {
                        // Escaped closing brace inside expression
                        expr_content.push(chars.next().unwrap());
                    } else {
                        expr_content.push(c);
                    }
                }

                if brace_depth != 0 {
                    return Err(ParseError::Custom {
                        message: "unclosed '{' in format string".to_string(),
                        span,
                    });
                }

                // Parse the expression
                let expr = parse_expr_string(&expr_content).map_err(|e| ParseError::Custom {
                    message: format!("error parsing format string expression: {}", e),
                    span,
                })?;

                parts.push(FormatPart::Expr(Box::new(expr)));
            } else if c == '}' {
                // Unmatched closing brace - error
                return Err(ParseError::Custom {
                    message: "unmatched '}' in format string (use '\\}' to escape)".to_string(),
                    span,
                });
            } else {
                current_literal.push(c);
            }
        }

        // Don't forget any trailing literal
        if !current_literal.is_empty() {
            parts.push(FormatPart::Literal(current_literal));
        }

        // If no parts, add empty string
        if parts.is_empty() {
            parts.push(FormatPart::Literal(String::new()));
        }

        Ok(parts)
    }
}

impl From<u8> for Precedence {
    fn from(value: u8) -> Self {
        match value {
            0 => Precedence::Lowest,
            1 => Precedence::Assignment,
            2 => Precedence::Range,
            3 => Precedence::Or,
            4 => Precedence::And,
            5 => Precedence::Comparison,
            6 => Precedence::BitOr,
            7 => Precedence::BitXor,
            8 => Precedence::BitAnd,
            9 => Precedence::Shift,
            10 => Precedence::Term,
            11 => Precedence::Factor,
            12 => Precedence::Cast,
            13 => Precedence::Unary,
            14 => Precedence::Try,
            15 => Precedence::Postfix,
            _ => Precedence::Lowest,
        }
    }
}

/// Parse an expression from a string (helper for format string interpolation).
fn parse_expr_string(s: &str) -> Result<Expr, ParseError> {
    let mut parser = Parser::new(s, 0);
    parser.parse_expression()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse_expr(source: &str) -> Expr {
        let mut parser = Parser::new(source, 0);
        parser.parse_expression().unwrap()
    }

    fn parse_module_source(source: &str) -> Module {
        let mut parser = Parser::new(source, 0);
        parser.parse_module()
    }

    #[test]
    fn test_literals() {
        let expr = parse_expr("42");
        assert!(matches!(expr.kind, ExprKind::Literal(Literal::Int(42))));

        let expr = parse_expr("3.14");
        assert!(matches!(expr.kind, ExprKind::Literal(Literal::Float(_))));

        let expr = parse_expr("true");
        assert!(matches!(expr.kind, ExprKind::Literal(Literal::Bool(true))));

        let expr = parse_expr(r#""hello""#);
        assert!(matches!(expr.kind, ExprKind::Literal(Literal::String(_))));
    }

    #[test]
    fn test_binary_ops() {
        let expr = parse_expr("1 + 2");
        assert!(matches!(
            expr.kind,
            ExprKind::Binary {
                op: BinaryOp::Add,
                ..
            }
        ));

        let expr = parse_expr("1 * 2 + 3");
        // Should parse as (1 * 2) + 3
        if let ExprKind::Binary { op, left, .. } = expr.kind {
            assert_eq!(op, BinaryOp::Add);
            assert!(matches!(
                left.kind,
                ExprKind::Binary {
                    op: BinaryOp::Mul,
                    ..
                }
            ));
        } else {
            panic!("Expected binary expression");
        }
    }

    #[test]
    fn test_unary_ops() {
        let expr = parse_expr("-42");
        assert!(matches!(
            expr.kind,
            ExprKind::Unary {
                op: UnaryOp::Neg,
                ..
            }
        ));

        let expr = parse_expr("!true");
        assert!(matches!(
            expr.kind,
            ExprKind::Unary {
                op: UnaryOp::Not,
                ..
            }
        ));
    }

    #[test]
    fn test_function_call() {
        let expr = parse_expr("foo(1, 2)");
        if let ExprKind::Call { args, .. } = expr.kind {
            assert_eq!(args.len(), 2);
        } else {
            panic!("Expected call expression");
        }
    }

    #[test]
    fn test_if_expr() {
        let expr = parse_expr("if true { 1 } else { 2 }");
        assert!(matches!(expr.kind, ExprKind::If { .. }));
    }

    #[test]
    fn test_function_def() {
        let module = parse_module_source("fn add(a: i64, b: i64) -> i64 { a + b }");
        assert_eq!(module.items.len(), 1);
        if let Item::Function(func) = &module.items[0] {
            assert_eq!(func.name.name.as_ref(), "add");
            assert_eq!(func.params.len(), 2);
        } else {
            panic!("Expected function");
        }
    }

    #[test]
    fn test_struct_def() {
        let module = parse_module_source("struct Point { x: f64, y: f64 }");
        assert_eq!(module.items.len(), 1);
        if let Item::Struct(s) = &module.items[0] {
            assert_eq!(s.name.name.as_ref(), "Point");
        } else {
            panic!("Expected struct");
        }
    }

    #[test]
    fn test_enum_def() {
        let module = parse_module_source("enum Option<T> { Some(T), None }");
        assert_eq!(module.items.len(), 1);
        if let Item::Enum(e) = &module.items[0] {
            assert_eq!(e.name.name.as_ref(), "Option");
            assert_eq!(e.variants.len(), 2);
        } else {
            panic!("Expected enum");
        }
    }

    #[test]
    fn test_match_expr() {
        let expr = parse_expr("match x { Some(v) => v, None => 0 }");
        if let ExprKind::Match { arms, .. } = expr.kind {
            assert_eq!(arms.len(), 2);
        } else {
            panic!("Expected match expression");
        }
    }

    #[test]
    fn test_closure() {
        let expr = parse_expr("|x, y| x + y");
        assert!(matches!(expr.kind, ExprKind::Closure { .. }));

        let expr = parse_expr("|| 42");
        assert!(matches!(expr.kind, ExprKind::Closure { .. }));
    }
}
