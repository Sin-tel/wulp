use crate::ast::*;
use crate::lexer::Lexer;
use crate::span::format_err;
use crate::span::Span;
use crate::token::{Token, TokenKind};

pub fn parse(input: &str) -> Block {
	let mut tokens = Lexer::new(input);

	let ast = parse_file(input, &mut tokens);

	// make sure we are done
	let tk = tokens.next();
	if tk.kind != TokenKind::Eof {
		format_err(&format!("Expected end of file but found: {tk}."), tk.span, input);
	}
	ast
}

// Block and statement rules

pub fn parse_file(input: &str, tokens: &mut Lexer) -> Block {
	parse_block_inner(input, tokens)
}

/// `{` block `}`
pub fn parse_block(input: &str, tokens: &mut Lexer) -> Block {
	assert_next(input, tokens, TokenKind::LCurly);
	let block = parse_block_inner(input, tokens);
	assert_next(input, tokens, TokenKind::RCurly);
	block
}

pub fn parse_block_inner(input: &str, tokens: &mut Lexer) -> Block {
	let mut stats = Vec::new();
	loop {
		match tokens.peek().kind {
			TokenKind::RCurly | TokenKind::Eof => break,
			TokenKind::Return | TokenKind::Break => {
				// These have to be the last statements in a block
				// TODO: peek here to check it is the last statement, and produce error
				stats.push(parse_statement(input, tokens));
				return Block { stats };
			},
			_ => stats.push(parse_statement(input, tokens)),
		}
	}
	Block { stats }
}

pub fn parse_statement(input: &str, tokens: &mut Lexer) -> Stat {
	let stat = parse_statement_inner(input, tokens);
	// take care of optional semicolon
	if tokens.peek().kind == TokenKind::SemiColon {
		tokens.next();
	}
	stat
}

pub fn parse_statement_inner(input: &str, tokens: &mut Lexer) -> Stat {
	let tk = tokens.peek();
	match tk.kind {
		TokenKind::Break => {
			tokens.next();
			Stat::Break
		},
		TokenKind::Return => Stat::Return(parse_return(input, tokens)),
		TokenKind::LCurly => Stat::Block(parse_block(input, tokens)),
		TokenKind::While => Stat::WhileBlock(parse_while_block(input, tokens)),
		TokenKind::If => Stat::IfBlock(parse_if_block(input, tokens)),
		TokenKind::For => Stat::ForBlock(parse_for_block(input, tokens)),
		TokenKind::Fn => Stat::FnDef(parse_fn_def(input, tokens)),
		_ => {
			// Parse a suffix expression, then check if a `=` or `,` follows to parse (multiple) assignment.
			// If not, it should be a function call.
			let suffix_expr = parse_suffix_expr(input, tokens);
			match tokens.peek().kind {
				TokenKind::Assign | TokenKind::Comma => Stat::Assignment(parse_assignment(suffix_expr, input, tokens)),
				_ => {
					if let Expr {
						span: _,
						kind: ExprKind::Call(e),
					} = suffix_expr
					{
						return Stat::Call(e);
					}
					let tk = tokens.next();
					let msg = format!("Expected `=` but found: {tk}.");
					format_err(&msg, tk.span, input);
					panic!("{msg}");
				},
			}
		},
	}
}

pub fn parse_assignment(first: Expr, input: &str, tokens: &mut Lexer) -> Assignment {
	let mut vars = vec![first];

	while tokens.peek().kind == TokenKind::Comma {
		tokens.next();
		vars.push(parse_suffix_expr(input, tokens));
	}

	// lhs vars can not be a Call, since those arent lvalues.
	for v in &vars {
		if let ExprKind::Call(_) = v.kind {
			let msg = "Can not assign to a function call.";
			format_err(msg, v.span, input);
			panic!("{}", msg);
		}
	}
	assert_next(input, tokens, TokenKind::Assign);

	let exprs = parse_exprs(input, tokens);

	Assignment {
		vars,
		exprs,
		local: false,
	}
}

pub fn parse_fn_def(input: &str, tokens: &mut Lexer) -> FnDef {
	assert_next(input, tokens, TokenKind::Fn);

	let (name, path) = parse_fn_name(input, tokens);
	let body = parse_fn_body(input, tokens);

	FnDef {
		name,
		path,
		body,
		local: false,
	}
}

pub fn parse_fn_name(input: &str, tokens: &mut Lexer) -> (Name, Vec<Property>) {
	let name = parse_name(input, tokens);
	let mut path = Vec::new();

	while tokens.peek().kind == (TokenKind::Period) {
		tokens.next();
		path.push(parse_property(input, tokens));
	}
	(name, path)
}

pub fn parse_fn_body(input: &str, tokens: &mut Lexer) -> FnBody {
	assert_next(input, tokens, TokenKind::LParen);
	let params = parse_parlist(input, tokens);
	assert_next(input, tokens, TokenKind::RParen);

	let body = parse_block(input, tokens);

	FnBody { params, body }
}

pub fn parse_for_block(input: &str, tokens: &mut Lexer) -> ForBlock {
	assert_next(input, tokens, TokenKind::For);

	let names = parse_names(input, tokens);

	assert_next(input, tokens, TokenKind::In);

	let exprs = parse_exprs(input, tokens);
	let block = parse_block(input, tokens);

	ForBlock { names, exprs, block }
}

pub fn parse_if_block(input: &str, tokens: &mut Lexer) -> IfBlock {
	assert_next(input, tokens, TokenKind::If);

	let expr = parse_expr(input, tokens);

	let block = parse_block(input, tokens);

	let mut elseif = Vec::new();

	while tokens.peek().kind == TokenKind::ElseIf {
		elseif.push(parse_elseif(input, tokens));
	}

	let else_block = parse_else_block(input, tokens);

	IfBlock {
		expr,
		block,
		elseif,
		else_block,
	}
}

pub fn parse_elseif(input: &str, tokens: &mut Lexer) -> ElseIf {
	assert_next(input, tokens, TokenKind::ElseIf);
	let expr = parse_expr(input, tokens);
	let block = parse_block(input, tokens);

	ElseIf { expr, block }
}

pub fn parse_else_block(input: &str, tokens: &mut Lexer) -> Option<Block> {
	if tokens.peek().kind == (TokenKind::Else) {
		tokens.next();
		Some(parse_block(input, tokens))
	} else {
		None
	}
}

pub fn parse_while_block(input: &str, tokens: &mut Lexer) -> WhileBlock {
	assert_next(input, tokens, TokenKind::While);

	let expr = parse_expr(input, tokens);
	let block = parse_block(input, tokens);

	WhileBlock { expr, block }
}

pub fn parse_return(input: &str, tokens: &mut Lexer) -> Vec<Expr> {
	assert_next(input, tokens, TokenKind::Return);

	match tokens.peek().kind {
		TokenKind::RCurly | TokenKind::Eof | TokenKind::SemiColon => Vec::new(),
		_ => parse_exprs(input, tokens),
	}
}

// Expression rules

pub fn parse_expr(input: &str, tokens: &mut Lexer) -> Expr {
	parse_sub_expr(input, tokens, 0)
}

// TODO: left / right priority
pub fn parse_sub_expr(input: &str, tokens: &mut Lexer, min_priority: i32) -> Expr {
	let mut expression = match parse_unexp(input, tokens) {
		Some(expr) => expr,
		None => parse_simple_expr(input, tokens),
	};

	while let Some(op) = tokens.peek().as_bin_op() {
		let priority = op.priority();
		if priority <= min_priority {
			break;
		}
		tokens.next();

		let lhs = expression;
		let rhs = parse_sub_expr(input, tokens, priority);

		expression = Expr {
			span: Span::join(lhs.span, rhs.span),
			kind: ExprKind::BinExpr(BinExpr {
				op,
				lhs: Box::new(lhs),
				rhs: Box::new(rhs),
			}),
		};
	}

	expression
}

pub fn parse_simple_expr(input: &str, tokens: &mut Lexer) -> Expr {
	match tokens.peek().kind {
		TokenKind::Nil => {
			let span = tokens.next().span;
			Expr {
				span,
				kind: ExprKind::Literal(Literal::Nil),
			}
		},
		TokenKind::True => {
			let span = tokens.next().span;
			Expr {
				span,
				kind: ExprKind::Literal(Literal::Bool(true)),
			}
		},
		TokenKind::False => {
			let span = tokens.next().span;
			Expr {
				span,
				kind: ExprKind::Literal(Literal::Bool(false)),
			}
		},
		TokenKind::Fn => {
			let span = tokens.next().span;
			Expr {
				span,
				kind: ExprKind::Lambda(parse_fn_body(input, tokens)),
			}
		},
		TokenKind::Str => parse_string(input, tokens),
		TokenKind::Number => parse_number(input, tokens),
		TokenKind::LCurly => parse_table_constructor(input, tokens),
		_ => parse_suffix_expr(input, tokens),
	}
}

pub fn parse_unexp(input: &str, tokens: &mut Lexer) -> Option<Expr> {
	let tk = tokens.peek();
	match tk.as_un_op() {
		Some(op) => {
			tokens.next();
			let expr = parse_sub_expr(input, tokens, op.priority());
			Some(Expr {
				span: Span::join(tk.span, expr.span),
				kind: ExprKind::UnExpr(UnExpr {
					op,
					expr: Box::new(expr),
				}),
			})
		},
		None => None,
	}
}

pub fn parse_suffix_expr(input: &str, tokens: &mut Lexer) -> Expr {
	let mut primary = parse_primary_expr(input, tokens);

	let mut suffix = Vec::new();

	loop {
		match tokens.peek().kind {
			TokenKind::Period => {
				tokens.next();
				suffix.push(Suffix::Property(parse_property(input, tokens)));
			},
			TokenKind::LBracket => {
				tokens.next();
				let expr = parse_expr(input, tokens);
				assert_next(input, tokens, TokenKind::RBracket);
				suffix.push(Suffix::Index(expr));
			},
			TokenKind::LParen => {
				// Build ast node and continue
				assert_next(input, tokens, TokenKind::LParen);
				let args = parse_args(input, tokens);
				let end = assert_next(input, tokens, TokenKind::RParen).span;
				let old_suffix = std::mem::take(&mut suffix);

				let expr = new_suffix_expr(primary, old_suffix);

				primary = Expr {
					span: Span::join(expr.span, end),
					kind: ExprKind::Call(Call {
						expr: Box::new(expr),
						args,
					}),
				};
			},
			_ => break,
		}
	}
	new_suffix_expr(primary, suffix)
}

// if suffix is empty just emit a single expr
fn new_suffix_expr(expr: Expr, suffix: Vec<Suffix>) -> Expr {
	match suffix.last() {
		None => expr,
		Some(s) => {
			let end = match s {
				Suffix::Property(p) => p.span.end,
				// add one to compensate for the ending `]`
				Suffix::Index(e) => e.span.end + 1,
			};
			Expr {
				span: Span::new(expr.span.start, end),
				kind: ExprKind::SuffixExpr(SuffixExpr {
					expr: Box::new(expr),
					suffix,
				}),
			}
		},
	}
}

pub fn parse_primary_expr(input: &str, tokens: &mut Lexer) -> Expr {
	match tokens.peek().kind {
		TokenKind::Name => {
			let name = parse_name(input, tokens);
			Expr {
				span: name.span,
				kind: ExprKind::Name(name),
			}
		},
		TokenKind::LParen => {
			let start = assert_next(input, tokens, TokenKind::LParen).span;
			let inner = parse_expr(input, tokens);
			let end = assert_next(input, tokens, TokenKind::RParen).span;
			Expr {
				span: Span::join(start, end),
				kind: ExprKind::Expr(Box::new(inner)),
			}
		},
		_ => {
			let tk = tokens.next();
			let msg = format!("Expected expression but found: {tk}.");
			format_err(&msg, tk.span, input);
			panic!("{msg}");
		},
	}
}

/// Constructors

pub fn parse_table_constructor(input: &str, tokens: &mut Lexer) -> Expr {
	let start = assert_next(input, tokens, TokenKind::LCurly).span;
	let fields = parse_fields(input, tokens);
	let end = assert_next(input, tokens, TokenKind::RCurly).span;
	Expr {
		span: Span::join(start, end),
		kind: ExprKind::Table(fields),
	}
}

pub fn parse_fields(input: &str, tokens: &mut Lexer) -> Vec<Field> {
	if tokens.peek().kind == TokenKind::RCurly {
		return Vec::new();
	};

	let mut fields = Vec::new();
	loop {
		if tokens.peek().kind == TokenKind::RCurly {
			break;
		}
		let f = parse_field(input, tokens);
		fields.push(f);

		match tokens.peek().kind {
			TokenKind::Comma | TokenKind::SemiColon => {
				tokens.next();
				continue;
			},
			_ => (),
			// _ => break,
		}
	}

	fields
}

pub fn parse_field(input: &str, tokens: &mut Lexer) -> Field {
	match tokens.peek().kind {
		TokenKind::Name => {
			// TODO: this is a bit ugly
			let span = tokens.next().span;

			match tokens.peek().kind {
				// Name '=' expr
				TokenKind::Assign => {
					tokens.next();
					let expr = parse_expr(input, tokens);
					Field::Assign(
						Property {
							span,
							name: span.as_string(input),
						},
						expr,
					)
				},
				// Expr(Name)
				_ => {
					let name = new_name(span);
					Field::Expr(Expr {
						span,
						kind: ExprKind::Name(name),
					})
				},
			}
		},
		TokenKind::Fn => {
			tokens.next();

			let name = parse_property(input, tokens);
			let body = parse_fn_body(input, tokens);

			Field::Fn(name, body)
		},
		_ => Field::Expr(parse_expr(input, tokens)),
	}
}

pub fn parse_args(input: &str, tokens: &mut Lexer) -> Vec<Expr> {
	if tokens.peek().kind == TokenKind::RParen {
		return Vec::new();
	}
	parse_exprs(input, tokens)
}

pub fn parse_exprs(input: &str, tokens: &mut Lexer) -> Vec<Expr> {
	let mut exprs = Vec::new();

	exprs.push(parse_expr(input, tokens));

	while tokens.peek().kind == TokenKind::Comma {
		tokens.next();
		exprs.push(parse_expr(input, tokens));
	}

	exprs
}

pub fn parse_names(input: &str, tokens: &mut Lexer) -> Vec<Name> {
	let mut names = vec![parse_name(input, tokens)];

	while tokens.peek().kind == TokenKind::Comma {
		tokens.next();

		// don't crash on trailing comma
		if tokens.peek().kind == TokenKind::Name {
			names.push(parse_name(input, tokens));
		} else {
			break;
		}
	}
	names
}

pub fn parse_parlist(input: &str, tokens: &mut Lexer) -> Vec<Name> {
	match tokens.peek().kind {
		TokenKind::Name => {
			let names = parse_names(input, tokens);
			if tokens.peek().kind == TokenKind::Comma {
				tokens.next();
			};
			names
		},
		_ => Vec::new(),
	}
}

// Simple terminals

// TODO: multi line string currently broken
fn parse_string(input: &str, tokens: &mut Lexer) -> Expr {
	let span = tokens.next().span;
	let mut chars = span.as_str(input).chars();
	let lit = match chars.next() {
		Some('\'' | '\"') => {
			chars.next_back();
			Literal::Str(chars.as_str().to_string())
		},
		Some('[') => {
			chars.next();
			chars.next_back();
			chars.next_back();
			Literal::Str(chars.as_str().to_string())
		},
		// TODO: if lexer is working properly, this should be unreachable
		_ => {
			let msg = format!("Malformed string: `{}`.", chars.as_str());
			format_err(&msg, span, input);
			panic!("{msg}");
		},
	};

	Expr {
		span,
		kind: ExprKind::Literal(lit),
	}
}

fn parse_number(input: &str, tokens: &mut Lexer) -> Expr {
	let span = tokens.next().span;
	let s = span.as_string(input);
	match s.parse() {
		Ok(num) => Expr {
			span,
			kind: ExprKind::Literal(Literal::Number(num)),
		},
		_ => {
			let msg = format!("Malformed number: `{s}`.");
			format_err(&msg, span, input);
			panic!("{msg}");
		},
	}
}

fn parse_name(input: &str, tokens: &mut Lexer) -> Name {
	let name = new_name(tokens.peek().span);
	assert_next(input, tokens, TokenKind::Name);
	name
}

fn new_name(span: Span) -> Name {
	Name { id: 0, span }
}

fn parse_property(input: &str, tokens: &mut Lexer) -> Property {
	let span = tokens.peek().span;
	let name = span.as_string(input);
	assert_next(input, tokens, TokenKind::Name);
	Property { span, name }
}

fn assert_next(input: &str, tokens: &mut Lexer, expect: TokenKind) -> Token {
	let tk = tokens.next();
	if tk.kind != expect {
		let msg = format!("Expected {} but found {}.", expect, tk.kind);
		format_err(&msg, tk.span, input);
		panic!("{msg}");
	}
	tk
}
