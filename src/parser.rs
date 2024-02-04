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

/// funcname ::= Name {`.` Name} [`:` Name]
pub fn parse_funcname(input: &str, tokens: &mut Tokens) -> FuncName {
	tokens.assert_next(input, TokenKind::Name);

	let mut path = vec![Name(tokens.cur().span.as_string(input))];
	// if next token is period then loop
	while tokens.peek().kind == (TokenKind::Period) {
		tokens.next();
		tokens.assert_next(input, TokenKind::Name);
		path.push(Name(tokens.cur().span.as_string(input)));
	}
	let mut method: Option<Name> = None;
	if tokens.peek().kind == TokenKind::Colon {
		tokens.next();
		tokens.assert_next(input, TokenKind::Name);
		method = Some(Name(tokens.cur().span.as_string(input)));
	}
	FuncName { path, method }
}

pub fn parse_simple_exp(input: &str, tokens: &mut Tokens) -> Option<Expr> {
	dbg!(tokens.peek());
	match tokens.peek().kind {
		TokenKind::Nil => {
			tokens.next();
			Some(Expr::Nil)
		},
		TokenKind::True => {
			tokens.next();
			Some(Expr::Bool(true))
		},
		TokenKind::False => {
			tokens.next();
			Some(Expr::Bool(false))
		},
		TokenKind::Function => {
			tokens.next();
			Some(Expr::Lambda(parse_funcbody(input, tokens)))
		},
		TokenKind::Str => Some(Expr::Str(parse_string(input, tokens))),
		TokenKind::Number => Some(Expr::Num(parse_number(input, tokens))),
		TokenKind::LCurly => Some(Expr::Table(parse_table_constructor(input, tokens))),
		_ => None,
	}
}

pub fn parse_unexp(input: &str, tokens: &mut Tokens) -> Option<Expr> {
	match tokens.peek().as_un_op() {
		Some(op) => {
			tokens.next();
			let exp = parse_sub_expr(input, tokens, op.priority());
			Some(Expr::UnExp(UnExp { op, exp: Box::new(exp) }))
		},
		None => None,
	}
}

pub fn parse_expr(input: &str, tokens: &mut Tokens) -> Expr {
	parse_sub_expr(input, tokens, 0)
}

// subexpr ::= (simpleexp | unop subexpr ) { binop subexpr }
// see: https://github.com/lua/lua/blob/2c32bff60987d38a60a58d4f0123f3783da60a63/lparser.c#L1120-L1156
// TODO: left / right priority
pub fn parse_sub_expr(input: &str, tokens: &mut Tokens, min_priority: i32) -> Expr {
	let mut expression = match parse_unexp(input, tokens) {
		Some(expr) => expr,
		None => match parse_simple_exp(input, tokens) {
			Some(expr) => expr,
			None => Expr::PrefixExp(Box::new(parse_prefix_exp(input, tokens))),
		},
	};

	while let Some(op) = tokens.peek().as_bin_op() {
		let priority = op.priority();
		if priority <= min_priority {
			break;
		}
		tokens.next();

		let rhs = parse_sub_expr(input, tokens, priority);

		expression = Expr::BinExp(BinExp {
			op,
			lhs: Box::new(expression),
			rhs: Box::new(rhs),
		});
	}

	expression
}

/// field ::= `[` exp `]` `=` exp | Name `=` exp | exp
pub fn parse_field(input: &str, tokens: &mut Tokens) -> Field {
	let name_token = tokens.next();
	match name_token.kind {
		// Name '=' exp
		TokenKind::Name => {
			tokens.assert_next(input, TokenKind::Assign);

			let expr = parse_expr(input, tokens);
			Field::Assign(Name(name_token.span.as_string(input)), expr)
		},
		// '[' exp ']' '=' exp
		TokenKind::LBracket => {
			let lexpr = parse_expr(input, tokens);

			tokens.assert_next(input, TokenKind::RBracket);
			tokens.assert_next(input, TokenKind::Assign);

			let rexpr = parse_expr(input, tokens);
			Field::ExprAssign(lexpr, rexpr)
		},

		_ => Field::Expr(parse_expr(input, tokens)),
	}
}

/// tableconstructor ::= `{` [fieldlist] `}`
pub fn parse_table_constructor(input: &str, tokens: &mut Tokens) -> Vec<Field> {
	tokens.assert_next(input, TokenKind::LCurly);
	if tokens.peek().kind == TokenKind::RCurly {
		tokens.next();
		return vec![];
	};
	let fieldlist = parse_fieldlist(input, tokens);
	tokens.assert_next(input, TokenKind::RCurly);
	fieldlist
}

/// fieldlist ::= field {fieldsep field} [fieldsep]
/// fieldsep ::= `,` | `;`
pub fn parse_fieldlist(input: &str, tokens: &mut Tokens) -> Vec<Field> {
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
	fields
}

/// vars ::= var {`,` var}
pub fn parse_vars(input: &str, tokens: &mut Tokens) -> Vec<Var> {
	let mut vars = vec![];

	vars.push(parse_var(input, tokens));

	while tokens.peek().kind == TokenKind::Comma {
		tokens.next();
		vars.push(parse_var(input, tokens));
	}

	vars
}

/// explist ::= exp {`,` exp}
pub fn parse_exprs(input: &str, tokens: &mut Tokens) -> Vec<Expr> {
	let mut exprs = vec![];

	exprs.push(parse_expr(input, tokens));

	while tokens.peek().kind == TokenKind::Comma {
		tokens.next();
		exprs.push(parse_expr(input, tokens));
	}

	exprs
}

/// args ::=  `(` [explist] `)`
pub fn parse_args(input: &str, tokens: &mut Tokens) -> Vec<Expr> {
	tokens.assert_next(input, TokenKind::LParen);
	if tokens.peek().kind == TokenKind::RParen {
		tokens.next();
		return vec![];
	}
	let explist = parse_exprs(input, tokens);
	tokens.assert_next(input, TokenKind::RParen);
	explist
}

/// var ::=  Name | prefixexp `[` exp `]` | prefixexp `.` Name
/// e.g.:
/// foo
/// bar[0]
/// bar.bizz
pub fn parse_var(input: &str, tokens: &mut Tokens) -> Var {
	if tokens.peek().kind == TokenKind::Name {
		let name_token = tokens.next();

		// TODO: these are completely incomprehensible, fix it
		match tokens.peek().kind {
			TokenKind::LBracket => {
				tokens.next();

				let expr = parse_expr(input, tokens);

				tokens.assert_next(input, TokenKind::RBracket);

				Var::IndexExpr(IndexExpr {
					expr: Box::new(PrefixExpr::Var(Var::Name(Name(name_token.span.as_string(input))))),
					arg: expr,
				})
			},
			TokenKind::Period => {
				tokens.next();
				tokens.assert_next(input, TokenKind::Name);

				Var::Property(Property {
					expr: Box::new(PrefixExpr::Var(Var::Name(Name(name_token.span.as_string(input))))),
					name: Name(tokens.cur().span.as_string(input)),
				})
			},
			_ => Var::Name(Name(tokens.cur().span.as_string(input))),
		}
	} else {
		let prefixexp = parse_prefix_exp(input, tokens);
		let tk = tokens.peek();

		match tk.kind {
			TokenKind::LBracket => {
				tokens.next();

				let expr = parse_expr(input, tokens);

				tokens.assert_next(input, TokenKind::RBracket);

				Var::IndexExpr(IndexExpr {
					expr: Box::new(prefixexp),
					arg: expr,
				})
			},
			TokenKind::Period => {
				tokens.next();
				tokens.assert_next(input, TokenKind::Name);

				Var::Property(Property {
					expr: Box::new(prefixexp),
					name: Name(tokens.cur().span.as_string(input)),
				})
			},
			// TODO: I'm not sure if this is even reachable
			_ => format_err(&format!("Expected `[` or `.` but found: {tk}."), tk.span, input),
		}
	}
}

/// prefixexp ::= var | functioncall | `(` exp `)`
// functioncall ::=  prefixexp args | prefixexp `:` Name args
// var ::=  Name | prefixexp `[` exp `]` | prefixexp `.` Name
pub fn parse_prefix_exp(input: &str, tokens: &mut Tokens) -> PrefixExpr {
	// TODO: currently broken -- prefixexp `:` Name args
	let tk = tokens.peek();
	match tk.kind {
		TokenKind::LParen => {
			tokens.next();
			let expr = PrefixExpr::Expr(parse_expr(input, tokens));
			tokens.assert_next(input, TokenKind::RParen);
			expr
		},

		// TODO: these are completely incomprehensible, fix it
		TokenKind::Name => {
			tokens.next();
			match tokens.peek().kind {
				TokenKind::LBracket => {
					tokens.next();

					let expr = parse_expr(input, tokens);

					tokens.assert_next(input, TokenKind::RBracket);

					PrefixExpr::Var(Var::IndexExpr(IndexExpr {
						expr: Box::new(PrefixExpr::Var(Var::Name(Name(tokens.cur().span.as_string(input))))),
						arg: expr,
					}))
				},
				TokenKind::Period => {
					tokens.next();
					tokens.assert_next(input, TokenKind::Name);

					PrefixExpr::Var(Var::Property(Property {
						expr: Box::new(PrefixExpr::Var(Var::Name(Name(tokens.cur().span.as_string(input))))),
						name: Name(tokens.cur().span.as_string(input)),
					}))
				},
				TokenKind::LParen => PrefixExpr::FunctionCall(FunctionCall {
					expr: Box::new(PrefixExpr::Var(Var::Name(Name(tokens.cur().span.as_string(input))))),
					args: parse_args(input, tokens),
				}),
				_ => PrefixExpr::Var(Var::Name(Name(tokens.cur().span.as_string(input)))),
			}
		},
		_ => format_err(&format!("Expected expression but found: {tk}."), tk.span, input),
	}
}

pub fn parse_stat(input: &str, tokens: &mut Tokens) -> Stat {
	// take care of optional semicolon
	let stat = parse_stat_inner(input, tokens);
	if tokens.peek().kind == TokenKind::SemiColon {
		tokens.next();
	}
	stat
}

pub fn parse_stat_inner(input: &str, tokens: &mut Tokens) -> Stat {
	let tk = tokens.peek();
	match tk.kind {
		TokenKind::Break => {
			tokens.next();
			Stat::Break
		},
		TokenKind::Return => Stat::Return(parse_return(input, tokens)),
		TokenKind::Do => Stat::DoBlock(parse_do_block(input, tokens)),
		TokenKind::While => Stat::WhileBlock(parse_while_block(input, tokens)),
		TokenKind::If => Stat::IfBlock(parse_if_block(input, tokens)),
		TokenKind::For => {
			if tokens.peek_n(3).kind == TokenKind::Assign {
				Stat::ForRange(parse_for_range(input, tokens))
			} else {
				Stat::ForIn(parse_for_in(input, tokens))
			}
		},
		TokenKind::Function => Stat::FunctionDef(parse_function_def(input, tokens)),
		TokenKind::Local => {
			if tokens.peek_n(2).kind == TokenKind::Function {
				Stat::LocalFunctionDef(parse_local_function_def(input, tokens))
			} else {
				Stat::LocalAssignment(parse_local_assignment(input, tokens))
			}
		},
		TokenKind::Name => {
			if tokens.peek_n(2).kind == TokenKind::LParen {
				tokens.next();
				Stat::FunctionCall(FunctionCall {
					expr: Box::new(PrefixExpr::Var(Var::Name(Name(tokens.cur().span.as_string(input))))),
					args: parse_args(input, tokens),
				})
			} else {
				Stat::Assignment(parse_assignment(input, tokens))
			}
		},
		_ => format_err(&format!("Expected statement but found: {tk}."), tk.span, input),
	}
}

/// vars `=` explist
pub fn parse_assignment(input: &str, tokens: &mut Tokens) -> Assignment {
	let vars = parse_vars(input, tokens);

	tokens.assert_next(input, TokenKind::Assign);

	let exprs = parse_exprs(input, tokens);

	Assignment { vars, exprs }
}

/// local names [`=` explist]
pub fn parse_local_assignment(input: &str, tokens: &mut Tokens) -> LocalAssignment {
	tokens.assert_next(input, TokenKind::Local);

	let names = parse_names(input, tokens);

	let exprs = if tokens.peek().kind == TokenKind::Assign {
		tokens.next();
		parse_exprs(input, tokens)
	} else {
		vec![]
	};

	LocalAssignment { names, exprs }
}

/// local function Name funcbody
pub fn parse_local_function_def(input: &str, tokens: &mut Tokens) -> LocalFunctionDef {
	tokens.assert_next(input, TokenKind::Local);
	tokens.assert_next(input, TokenKind::Function);
	tokens.assert_next(input, TokenKind::Name);

	let name = Name(tokens.cur().span.as_string(input));
	let body = parse_funcbody(input, tokens);

	LocalFunctionDef { name, body }
}

/// function funcname funcbody
pub fn parse_function_def(input: &str, tokens: &mut Tokens) -> FunctionDef {
	tokens.assert_next(input, TokenKind::Function);

	let name = parse_funcname(input, tokens);
	let body = parse_funcbody(input, tokens);

	FunctionDef { name, body }
}

/// for names in explist do block end
pub fn parse_for_in(input: &str, tokens: &mut Tokens) -> ForIn {
	tokens.assert_next(input, TokenKind::For);

	let names = parse_names(input, tokens);

	tokens.assert_next(input, TokenKind::In);

	let exprs = parse_exprs(input, tokens);
	let block = parse_do_block(input, tokens);

	ForIn { names, exprs, block }
}

/// for Name `=` exp `,` exp [`,` exp] do block end
pub fn parse_for_range(input: &str, tokens: &mut Tokens) -> ForRange {
	tokens.assert_next(input, TokenKind::For);

	tokens.assert_next(input, TokenKind::Name);
	let name = Name(tokens.cur().span.as_string(input));

	tokens.assert_next(input, TokenKind::Assign);
	let exp_start = parse_expr(input, tokens);

	tokens.assert_next(input, TokenKind::Comma);

	let exp_end = parse_expr(input, tokens);

	let exp_step = if tokens.peek().kind == TokenKind::Comma {
		tokens.next();
		Some(parse_expr(input, tokens))
	} else {
		None
	};

	let block = parse_do_block(input, tokens);

	ForRange {
		name,
		exprs: (exp_start, exp_end, exp_step),
		block,
	}
}

/// elseif exp then block
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

/// if exp then block {elseif exp then block} [else block] end
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

/// while exp do block end
pub fn parse_while_block(input: &str, tokens: &mut Tokens) -> WhileBlock {
	tokens.assert_next(input, TokenKind::While);

	let expr = parse_expr(input, tokens);
	let block = parse_do_block(input, tokens);

	WhileBlock { expr, block }
}

/// do block end
pub fn parse_do_block(input: &str, tokens: &mut Tokens) -> Block {
	tokens.assert_next(input, TokenKind::Do);
	let blk = parse_block(input, tokens);
	tokens.assert_next(input, TokenKind::End);
	blk
}

// tokens that can follow after a block
fn block_follow(tk: Token) -> bool {
	matches!(
		tk.kind,
		TokenKind::End | TokenKind::Else | TokenKind::ElseIf | TokenKind::Eof
	)
}

/// block ::= {stat} [laststat]
pub fn parse_block(input: &str, tokens: &mut Tokens) -> Block {
	let mut stats = vec![];

	loop {
		let tk = tokens.peek();
		if block_follow(tk) {
			break;
		} else if tk.kind == TokenKind::Return || tk.kind == TokenKind::Break {
			// These have to be the last statements in a block
			stats.push(parse_stat(input, tokens));

			// TODO: peek here to check it is the last statement, and produce error
			return Block { stats };
		}
		stats.push(parse_stat(input, tokens));
	}
	Block { stats }
}

pub fn parse_return(input: &str, tokens: &mut Tokens) -> Vec<Expr> {
	tokens.assert_next(input, TokenKind::Return);

	if tokens.peek().kind == TokenKind::SemiColon {
		tokens.next();
		return vec![];
	}

	if block_follow(tokens.peek()) {
		return vec![];
	}

	let exprs = parse_exprs(input, tokens);

	if tokens.peek().kind == TokenKind::SemiColon {
		tokens.next();
	}

	exprs
}

/// funcbody ::= `(` [parlist] `)` block end
pub fn parse_funcbody(input: &str, tokens: &mut Tokens) -> FuncBody {
	tokens.assert_next(input, TokenKind::LParen);
	let params = parse_parlist(input, tokens);
	tokens.assert_next(input, TokenKind::RParen);
	let body = parse_block(input, tokens);
	tokens.assert_next(input, TokenKind::End);

	FuncBody { params, body }
}

/// names ::= Name {`,` Name}
pub fn parse_names(input: &str, tokens: &mut Tokens) -> Vec<Name> {
	tokens.assert_next(input, TokenKind::Name);

	let first_name = tokens.cur().span.as_string(input);

	let mut names = vec![Name(first_name)];

	while tokens.peek().kind == TokenKind::Comma {
		tokens.next();

		// don't crash on trailing comma
		if tokens.peek().kind == TokenKind::Name {
			tokens.next();
			names.push(Name(tokens.cur().span.as_string(input)));
		} else {
			break;
		}
	}
	names
}

/// parlist ::= names [`,`]
pub fn parse_parlist(input: &str, tokens: &mut Tokens) -> Vec<Name> {
	match tokens.peek().kind {
		// names
		TokenKind::Name => {
			let names = parse_names(input, tokens);

			// [',']
			if tokens.peek().kind == TokenKind::Comma {
				tokens.next();
			};
			names
		},
		_ => vec![],
	}
}

// TODO: multi line string currently broken
fn parse_string(input: &str, tokens: &mut Tokens) -> String {
	let tk = tokens.next();
	let mut chars = tk.span.as_str(input).chars();
	match chars.next() {
		Some('\'' | '\"') => {
			chars.next_back();
			chars.as_str().to_string() // really?
		},
		_ => format_err(&format!("Malformed string: `{}`.", chars.as_str()), tk.span, input),
	}
}

fn parse_number(input: &str, tokens: &mut Tokens) -> f64 {
	let tk = tokens.next();
	let s = tk.span.as_string(input);
	match s.parse() {
		Ok(num) => num,
		_ => format_err(&format!("Malformed number: `{s}`."), tk.span, input),
	}
}
