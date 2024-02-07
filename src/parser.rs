use crate::ast::*;
use crate::lexer::Lexer;
use crate::span::format_err;
use crate::token::{Token, TokenKind, Tokens};

pub fn parse(input: &str) -> Block {
	let tokens: Vec<_> = Lexer::new(input).filter(|t| t.kind != TokenKind::Comment).collect();

	let mut tokens = Tokens::new(tokens);

	// println!("{}", tokens);

	let ast = parse_block(input, &mut tokens);

	// make sure we are done
	let tk = tokens.next();
	if tk.kind != TokenKind::Eof {
		format_err(&format!("Expected end of file but found: {tk}."), tk.span, input);
	}
	ast
}

// Block and statement rules

// tokens that can follow after a block
fn block_follow(tk: Token) -> bool {
	matches!(
		tk.kind,
		TokenKind::End | TokenKind::Else | TokenKind::ElseIf | TokenKind::Eof
	)
}

/// block -> {stat} [laststat]
pub fn parse_block(input: &str, tokens: &mut Tokens) -> Block {
	let mut stats = vec![];

	loop {
		let tk = tokens.peek();
		if block_follow(tk) {
			break;
		} else if tk.kind == TokenKind::Return || tk.kind == TokenKind::Break {
			// These have to be the last statements in a block
			stats.push(parse_statement(input, tokens));

			// TODO: peek here to check it is the last statement, and produce error
			return Block { stats };
		}
		stats.push(parse_statement(input, tokens));
	}
	Block { stats }
}

pub fn parse_statement(input: &str, tokens: &mut Tokens) -> Stat {
	let stat = parse_statement_inner(input, tokens);
	// take care of optional semicolon
	if tokens.peek().kind == TokenKind::SemiColon {
		tokens.next();
	}
	stat
}

pub fn parse_statement_inner(input: &str, tokens: &mut Tokens) -> Stat {
	let tk = tokens.peek();
	match tk.kind {
		TokenKind::Break => {
			tokens.next();
			Stat::Break
		},
		TokenKind::Return => Stat::Return(parse_return(input, tokens)),
		TokenKind::Do => Stat::Block(parse_do_block(input, tokens)),
		TokenKind::While => Stat::WhileBlock(parse_while_block(input, tokens)),
		TokenKind::If => Stat::IfBlock(parse_if_block(input, tokens)),
		TokenKind::For => {
			if tokens.peek_n(3).kind == TokenKind::Assign {
				Stat::ForRange(parse_for_range(input, tokens))
			} else {
				Stat::ForIn(parse_for_in(input, tokens))
			}
		},
		TokenKind::Function => Stat::FnDef(parse_fn_def(input, tokens)),
		TokenKind::Local => {
			if tokens.peek_n(2).kind == TokenKind::Function {
				Stat::FnDef(parse_local_fn_def(input, tokens))
			} else {
				Stat::Assignment(parse_local_assignment(input, tokens))
			}
		},
		_ => {
			// Parse a suffix expression, then check if a `=` or `,` follows to parse (multiple) assignment.
			// If not, it should be a function call.
			let suffix_expr = parse_suffix_expr(input, tokens);
			match tokens.peek().kind {
				TokenKind::Assign | TokenKind::Comma => Stat::Assignment(parse_assignment(suffix_expr, input, tokens)),
				_ => {
					if let Expr::Call(e) = suffix_expr {
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

/// assignment -> vars `=` expr_list
/// vars -> suffix_expr {`,` suffix_expr}
pub fn parse_assignment(first: Expr, input: &str, tokens: &mut Tokens) -> Assignment {
	let mut vars = vec![first];

	while tokens.peek().kind == TokenKind::Comma {
		tokens.next();
		vars.push(parse_suffix_expr(input, tokens));
	}

	tokens.assert_next(input, TokenKind::Assign);

	let exprs = parse_exprs(input, tokens);

	// TODO: at this point, none of the lhs vars should end on a Call suffix, since you can't assign there.
	// We can check for this here, but maybe it makes more sense in a later stage if you have to check it anyway.

	Assignment {
		vars,
		exprs,
		local: false,
	}
}

/// local names [`=` expr_list]
pub fn parse_local_assignment(input: &str, tokens: &mut Tokens) -> Assignment {
	tokens.assert_next(input, TokenKind::Local);

	let vars = parse_names(input, tokens).into_iter().map(Expr::Name).collect();

	let exprs = if tokens.peek().kind == TokenKind::Assign {
		tokens.next();
		parse_exprs(input, tokens)
	} else {
		vec![]
	};

	Assignment {
		vars,
		exprs,
		local: true,
	}
}

/// local function Name fn_body
pub fn parse_local_fn_def(input: &str, tokens: &mut Tokens) -> FnDef {
	tokens.assert_next(input, TokenKind::Local);
	tokens.assert_next(input, TokenKind::Function);

	let name = vec![parse_name(input, tokens)];
	let body = parse_fn_body(input, tokens);

	FnDef {
		name,
		body,
		local: true,
	}
}

/// function fn_name fn_body
pub fn parse_fn_def(input: &str, tokens: &mut Tokens) -> FnDef {
	tokens.assert_next(input, TokenKind::Function);

	let name = parse_fn_name(input, tokens);
	let body = parse_fn_body(input, tokens);

	FnDef {
		name,
		body,
		local: false,
	}
}

/// fn_name -> Name {`.` Name} [`:` Name]
pub fn parse_fn_name(input: &str, tokens: &mut Tokens) -> Vec<Name> {
	let mut path = vec![parse_name(input, tokens)];
	// if next token is period then loop
	while tokens.peek().kind == (TokenKind::Period) {
		tokens.next();
		path.push(parse_name(input, tokens));
	}

	path
}

/// fn_body -> `(` [parlist] `)` block end
pub fn parse_fn_body(input: &str, tokens: &mut Tokens) -> FnBody {
	tokens.assert_next(input, TokenKind::LParen);
	let params = parse_parlist(input, tokens);
	tokens.assert_next(input, TokenKind::RParen);
	let body = parse_block(input, tokens);
	tokens.assert_next(input, TokenKind::End);

	FnBody { params, body }
}

/// for names in expr_list do block end
pub fn parse_for_in(input: &str, tokens: &mut Tokens) -> ForIn {
	tokens.assert_next(input, TokenKind::For);

	let names = parse_names(input, tokens);

	tokens.assert_next(input, TokenKind::In);

	let exprs = parse_exprs(input, tokens);
	let block = parse_do_block(input, tokens);

	ForIn { names, exprs, block }
}

/// for Name `=` expr `,` expr [`,` expr] do block end
pub fn parse_for_range(input: &str, tokens: &mut Tokens) -> ForRange {
	tokens.assert_next(input, TokenKind::For);

	let name = parse_name(input, tokens);

	tokens.assert_next(input, TokenKind::Assign);
	let expr_start = parse_expr(input, tokens);

	tokens.assert_next(input, TokenKind::Comma);

	let expr_end = parse_expr(input, tokens);

	let expr_step = if tokens.peek().kind == TokenKind::Comma {
		tokens.next();
		Some(parse_expr(input, tokens))
	} else {
		None
	};

	let block = parse_do_block(input, tokens);

	ForRange {
		name,
		exprs: (expr_start, expr_end, expr_step),
		block,
	}
}

/// if expr then block {elseif expr then block} [else block] end
pub fn parse_if_block(input: &str, tokens: &mut Tokens) -> IfBlock {
	tokens.assert_next(input, TokenKind::If);

	let expr = parse_expr(input, tokens);

	tokens.assert_next(input, TokenKind::Then);

	let block = parse_block(input, tokens);

	let mut elseif = vec![];

	while tokens.peek().kind == TokenKind::ElseIf {
		elseif.push(parse_elseif(input, tokens));
	}

	let else_block = parse_else_block(input, tokens);

	tokens.assert_next(input, TokenKind::End);

	IfBlock {
		expr,
		block,
		elseif,
		else_block,
	}
}

/// elseif expr then block
pub fn parse_elseif(input: &str, tokens: &mut Tokens) -> ElseIf {
	tokens.assert_next(input, TokenKind::ElseIf);
	let expr = parse_expr(input, tokens);
	tokens.assert_next(input, TokenKind::Then);
	let block = parse_block(input, tokens);

	ElseIf { expr, block }
}

/// else block
pub fn parse_else_block(input: &str, tokens: &mut Tokens) -> Option<Block> {
	if tokens.peek().kind == (TokenKind::Else) {
		tokens.next();
		Some(parse_block(input, tokens))
	} else {
		None
	}
}

/// while expr do block end
pub fn parse_while_block(input: &str, tokens: &mut Tokens) -> WhileBlock {
	tokens.assert_next(input, TokenKind::While);

	let expr = parse_expr(input, tokens);
	let block = parse_do_block(input, tokens);

	WhileBlock { expr, block }
}

/// do block end
pub fn parse_do_block(input: &str, tokens: &mut Tokens) -> Block {
	tokens.assert_next(input, TokenKind::Do);
	let block = parse_block(input, tokens);
	tokens.assert_next(input, TokenKind::End);
	block
}

pub fn parse_return(input: &str, tokens: &mut Tokens) -> Vec<Expr> {
	tokens.assert_next(input, TokenKind::Return);

	if block_follow(tokens.peek()) || tokens.peek().kind == TokenKind::SemiColon {
		return vec![];
	}

	parse_exprs(input, tokens)
}

// Expression rules

pub fn parse_expr(input: &str, tokens: &mut Tokens) -> Expr {
	parse_sub_expr(input, tokens, 0)
}

// subexpr -> (simpleexpr | unop subexpr ) { binop subexpr }
// see: https://github.com/lua/lua/blob/2c32bff60987d38a60a58d4f0123f3783da60a63/lparser.c#L1120-L1156
// TODO: left / right priority
pub fn parse_sub_expr(input: &str, tokens: &mut Tokens, min_priority: i32) -> Expr {
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

		let rhs = parse_sub_expr(input, tokens, priority);

		expression = Expr::BinExpr(BinExpr {
			op,
			lhs: Box::new(expression),
			rhs: Box::new(rhs),
		});
	}

	expression
}

pub fn parse_simple_expr(input: &str, tokens: &mut Tokens) -> Expr {
	match tokens.peek().kind {
		TokenKind::Nil => {
			tokens.next();
			Expr::Literal(Literal::Nil)
		},
		TokenKind::True => {
			tokens.next();
			Expr::Literal(Literal::Bool(true))
		},
		TokenKind::False => {
			tokens.next();
			Expr::Literal(Literal::Bool(false))
		},
		TokenKind::Function => {
			tokens.next();
			Expr::Lambda(parse_fn_body(input, tokens))
		},
		TokenKind::Str => Expr::Literal(parse_string(input, tokens)),
		TokenKind::Number => Expr::Literal(parse_number(input, tokens)),
		TokenKind::LCurly => Expr::Table(parse_table_constructor(input, tokens)),
		_ => parse_suffix_expr(input, tokens),
	}
}

pub fn parse_unexp(input: &str, tokens: &mut Tokens) -> Option<Expr> {
	match tokens.peek().as_un_op() {
		Some(op) => {
			tokens.next();
			let expr = parse_sub_expr(input, tokens, op.priority());
			Some(Expr::UnExpr(UnExpr {
				op,
				expr: Box::new(expr),
			}))
		},
		None => None,
	}
}

/// suffix_expr -> primary_expr { suffix }
/// primary_expr -> Name | '(' expr ')'
/// suffix -> `.` Name
///         | `[` expr `]`
pub fn parse_suffix_expr(input: &str, tokens: &mut Tokens) -> Expr {
	let mut primary = parse_primary_expr(input, tokens);

	let mut suffix = Vec::new();

	loop {
		match tokens.peek().kind {
			TokenKind::Period => {
				tokens.next();
				let name = parse_name(input, tokens);

				suffix.push(Suffix::Property(name));
			},
			TokenKind::LBracket => {
				tokens.next();
				let expr = parse_expr(input, tokens);
				tokens.assert_next(input, TokenKind::RBracket);

				suffix.push(Suffix::Index(expr));
			},
			TokenKind::LParen => {
				// Build ast node and continue
				let args = parse_args(input, tokens);
				let old_suffix = std::mem::take(&mut suffix);
				primary = Expr::Call(Call {
					expr: Box::new(new_suffix_expr(primary, old_suffix)),
					args,
				})
			},
			_ => break,
		}
	}

	new_suffix_expr(primary, suffix)
}

// if suffix is empty just emit a single expr
fn new_suffix_expr(expr: Expr, suffix: Vec<Suffix>) -> Expr {
	if suffix.is_empty() {
		return expr;
	}
	Expr::SuffixExpr(SuffixExpr {
		expr: Box::new(expr),
		suffix,
	})
}

/// primary_expr -> Name | '(' expr ')'
pub fn parse_primary_expr(input: &str, tokens: &mut Tokens) -> Expr {
	match tokens.peek().kind {
		TokenKind::Name => Expr::Name(parse_name(input, tokens)),
		TokenKind::LParen => {
			tokens.assert_next(input, TokenKind::LParen);
			let expr = Expr::Expr(Box::new(parse_expr(input, tokens)));
			tokens.assert_next(input, TokenKind::RParen);
			expr
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

/// tableconstructor -> `{` [fieldlist] `}`
/// fieldlist -> field {fieldsep field} [fieldsep]
/// fieldsep -> `,` | `;`
pub fn parse_table_constructor(input: &str, tokens: &mut Tokens) -> Vec<Field> {
	tokens.assert_next(input, TokenKind::LCurly);
	if tokens.peek().kind == TokenKind::RCurly {
		tokens.next();
		return vec![];
	};

	let mut fields = vec![];
	loop {
		let f = parse_field(input, tokens);
		fields.push(f);

		match tokens.peek().kind {
			TokenKind::Comma | TokenKind::SemiColon => {
				tokens.next();
				continue;
			},
			_ => break,
		}
	}

	tokens.assert_next(input, TokenKind::RCurly);
	fields
}

/// field -> `[` expr `]` `=` expr | Name `=` expr | exp
pub fn parse_field(input: &str, tokens: &mut Tokens) -> Field {
	match tokens.peek().kind {
		// Name '=' expr
		TokenKind::Name => {
			let name = parse_name(input, tokens);
			tokens.assert_next(input, TokenKind::Assign);

			let expr = parse_expr(input, tokens);
			Field::Assign(name, expr)
		},
		// expr
		_ => Field::Expr(parse_expr(input, tokens)),
	}
}

/// args ->  `(` [expr_list] `)`
pub fn parse_args(input: &str, tokens: &mut Tokens) -> Vec<Expr> {
	tokens.assert_next(input, TokenKind::LParen);
	if tokens.peek().kind == TokenKind::RParen {
		tokens.next();
		return vec![];
	}
	let expr_list = parse_exprs(input, tokens);
	tokens.assert_next(input, TokenKind::RParen);
	expr_list
}

/// expr_list -> expr {`,` expr}
pub fn parse_exprs(input: &str, tokens: &mut Tokens) -> Vec<Expr> {
	let mut exprs = vec![];

	exprs.push(parse_expr(input, tokens));

	while tokens.peek().kind == TokenKind::Comma {
		tokens.next();
		exprs.push(parse_expr(input, tokens));
	}

	exprs
}

/// names -> Name {`,` Name}
pub fn parse_names(input: &str, tokens: &mut Tokens) -> Vec<Name> {
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

/// parlist -> names [`,`]
pub fn parse_parlist(input: &str, tokens: &mut Tokens) -> Vec<Name> {
	match tokens.peek().kind {
		TokenKind::Name => {
			let names = parse_names(input, tokens);
			if tokens.peek().kind == TokenKind::Comma {
				tokens.next();
			};
			names
		},
		_ => vec![],
	}
}

// Simple terminals

// TODO: multi line string currently broken
fn parse_string(input: &str, tokens: &mut Tokens) -> Literal {
	let tk = tokens.next();
	let mut chars = tk.span.as_str(input).chars();
	match chars.next() {
		Some('\'' | '\"') => {
			chars.next_back();
			Literal::Str(chars.as_str().to_string()) // really?
		},
		Some('[') => {
			chars.next();
			chars.next_back();
			chars.next_back();
			Literal::Str(chars.as_str().to_string()) // really?
		},
		// TODO: if lexer is working properly, this should be unreachable
		_ => {
			let msg = format!("Malformed string: `{}`.", chars.as_str());
			format_err(&msg, tk.span, input);
			panic!("{msg}");
		},
	}
}

fn parse_number(input: &str, tokens: &mut Tokens) -> Literal {
	let tk = tokens.next();
	let s = tk.span.as_string(input);
	match s.parse() {
		Ok(num) => Literal::Number(num),
		_ => {
			let msg = format!("Malformed number: `{s}`.");
			format_err(&msg, tk.span, input);
			panic!("{msg}");
		},
	}
}

fn parse_name(input: &str, tokens: &mut Tokens) -> Name {
	tokens.assert_next(input, TokenKind::Name);
	Name(tokens.cur().span.as_string(input))
}
