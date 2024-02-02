use crate::ast::*;
use crate::lexer::{Lexer, Token, TokenKind};
use crate::token_iter::TokenIter;
use std;

type Result<T> = std::result::Result<T, ()>;

pub fn to_kind(tk: Token) -> TokenKind {
	tk.kind
}

pub fn parse(input: &str) -> Result<Block> {
	let tokens: Vec<_> = Lexer::new(input)
		.filter(|t| match t {
			Token {
				kind: TokenKind::Comment(_),
				..
			} => false,
			_ => true,
		})
		.collect();
	dbg!(&tokens);
	let mut tokens = TokenIter::new(tokens);
	let ast = parse_block(&input, &mut tokens);

	// make sure we are done
	if tokens.next() != None {
		panic!("Expected end of file at {:?}", tokens.cur());
	}
	ast
}

/// funcname ::= Name {`.` Name} [`:` Name]
pub fn parse_funcname(input: &str, tokens: &mut TokenIter) -> Result<FuncName> {
	tokens.assert_next(TokenKind::Ident);

	let mut path = vec![Name(tokens.cur().unwrap().span.as_str(input).to_string())];
	// if next token is period then loop
	while tokens.peek().map(to_kind) == Some(TokenKind::Period) {
		tokens.next();
		match tokens.next() {
			Some(Token {
				kind: TokenKind::Ident, ..
			}) => {
				path.push(Name(tokens.cur().unwrap().span.as_str(input).to_string()));
			},
			_ => return Err(()),
		}
	}
	let mut method: Option<Name> = None;
	if let Some(Token {
		kind: TokenKind::Colon, ..
	}) = tokens.peek()
	{
		tokens.next();
		match tokens.next() {
			Some(Token {
				kind: TokenKind::Ident, ..
			}) => {
				method = Some(Name(tokens.cur().unwrap().span.as_str(input).to_string()));
			},
			_ => return Err(()),
		}
	}
	Ok(FuncName { path, method })
}

pub fn parse_simple_exp(input: &str, tokens: &mut TokenIter) -> Result<Expr> {
	match tokens.cur().map(to_kind) {
		Some(TokenKind::Nil) => Ok(Expr::Nil),
		Some(TokenKind::True) => Ok(Expr::Bool(true)),
		Some(TokenKind::False) => Ok(Expr::Bool(false)),
		Some(TokenKind::String) => Ok(Expr::Str(
			parse_string(tokens.cur().unwrap().span.as_str(input)).to_string(),
		)),
		Some(TokenKind::Number) => {
			let s = tokens.cur().unwrap().span.as_str(input).to_string();
			match s.parse() {
				Ok(num) => Ok(Expr::Num(num)),
				_ => panic!("Malformed number"), // TODO: handle properly
			}
		},
		Some(TokenKind::Function) => {
			tokens.prev();
			parse_functiondef(&input, tokens).map(Expr::FuncDef)
		},
		Some(TokenKind::LCurly) => {
			tokens.prev();
			parse_table_constructor(&input, tokens).map(Expr::Table)
		},
		_ => Err(()),
	}
}

pub fn bin_priority(op: &Option<TokenKind>) -> i32 {
	match op {
		Some(TokenKind::Pow) => 8,
		Some(TokenKind::Mul) | Some(TokenKind::Div) | Some(TokenKind::Mod) => 6,
		Some(TokenKind::Plus) | Some(TokenKind::Minus) => 5,
		Some(TokenKind::Concat) => 4,
		Some(TokenKind::Lt) | Some(TokenKind::Gt) | Some(TokenKind::Lte) | Some(TokenKind::Gte)
		| Some(TokenKind::Eq) | Some(TokenKind::Neq) => 3,
		Some(TokenKind::And) => 2,
		Some(TokenKind::Or) => 1,
		_ => panic!("No priority defined for {:?}", op),
	}
}

const UNARY_PRIORITY: i32 = 7;

pub fn parse_unexp(input: &str, tokens: &mut TokenIter) -> Result<Expr> {
	match tokens.next().map(to_kind) {
		tk @ Some(TokenKind::Minus) | tk @ Some(TokenKind::Not) | tk @ Some(TokenKind::Hash) => {
			let op = match tk {
				Some(TokenKind::Minus) => Unop::Minus,
				Some(TokenKind::Not) => Unop::Not,
				Some(TokenKind::Hash) => Unop::Len,
				_ => return Err(()),
			};

			let exp = parse_sub_expr(&input, tokens, UNARY_PRIORITY)?;
			Ok(Expr::UnExp(UnExp { op, exp: Box::new(exp) }))
		},
		_ => Err(()),
	}
}

/// exp ::= nil | false | true | Numeral | LiteralString | `...` | functiondef |
///         prefixexp | tableconstructor | exp binop exp | unop exp
pub fn parse_expr(input: &str, tokens: &mut TokenIter) -> Result<Expr> {
	parse_sub_expr(&input, tokens, 0)
}

pub fn is_bin_op(token: &Option<TokenKind>) -> bool {
	match token {
		Some(TokenKind::Plus)
		| Some(TokenKind::Minus)
		| Some(TokenKind::Mul)
		| Some(TokenKind::Div)
		| Some(TokenKind::Pow)
		| Some(TokenKind::Mod)
		| Some(TokenKind::Concat)
		| Some(TokenKind::Lt)
		| Some(TokenKind::Lte)
		| Some(TokenKind::Gt)
		| Some(TokenKind::Gte)
		| Some(TokenKind::Eq)
		| Some(TokenKind::Neq)
		| Some(TokenKind::And)
		| Some(TokenKind::Or) => true,
		_ => false,
	}
}

// subexpr ::= (simpleexp | unop subexpr ) { binop subexpr }
// see: https://github.com/lua/lua/blob/2c32bff60987d38a60a58d4f0123f3783da60a63/lparser.c#L1120-L1156
// TODO: left / right priority
pub fn parse_sub_expr(input: &str, tokens: &mut TokenIter, min_priority: i32) -> Result<Expr> {
	// TODO: fix whatever this is
	let mut expression = parse_unexp(&input, tokens)
		.or_else(|_| parse_simple_exp(&input, tokens))
		.or_else(|_| {
			tokens.prev();
			parse_prefix_exp(&input, tokens).map(|x| Expr::PrefixExp(Box::new(x)))
		})?;

	while is_bin_op(&tokens.peek().map(to_kind)) && bin_priority(&tokens.peek().map(to_kind)) > min_priority {
		tokens.next();

		let op = match tokens.cur().map(to_kind) {
			Some(TokenKind::Plus) => BinOp::Plus,
			Some(TokenKind::Minus) => BinOp::Minus,
			Some(TokenKind::Mul) => BinOp::Mul,
			Some(TokenKind::Div) => BinOp::Div,
			Some(TokenKind::Pow) => BinOp::Pow,
			Some(TokenKind::Mod) => BinOp::Mod,
			Some(TokenKind::Concat) => BinOp::Concat,
			Some(TokenKind::Lt) => BinOp::Lt,
			Some(TokenKind::Lte) => BinOp::Lte,
			Some(TokenKind::Gt) => BinOp::Gt,
			Some(TokenKind::Gte) => BinOp::Gte,
			Some(TokenKind::Eq) => BinOp::Eq,
			Some(TokenKind::Neq) => BinOp::Neq,
			Some(TokenKind::And) => BinOp::And,
			Some(TokenKind::Or) => BinOp::Or,
			_ => break,
		};

		let prority = bin_priority(&tokens.cur().map(to_kind));

		let rhs = match parse_sub_expr(&input, tokens, prority) {
			Err(_) => break,
			Ok(rhs) => rhs,
		};

		expression = Expr::BinExp(BinExp {
			op,
			lhs: Box::new(expression),
			rhs: Box::new(rhs),
		})
	}

	Ok(expression)
}

/// field ::= `[` exp `]` `=` exp | Name `=` exp | exp
pub fn parse_field(input: &str, tokens: &mut TokenIter) -> Result<Field> {
	let name_token = tokens.next();
	match name_token.map(to_kind) {
		// Name '=' exp
		Some(TokenKind::Ident) => {
			tokens.assert_next(TokenKind::Assign);

			let expr = parse_expr(&input, tokens)?;

			Ok(Field::NameAssign(
				Name(name_token.unwrap().span.as_str(input).to_string()),
				expr,
			))
		},
		// '[' exp ']' '=' exp
		Some(TokenKind::LBracket) => {
			let lexpr = parse_expr(&input, tokens)?;

			tokens.assert_next(TokenKind::RBracket);

			tokens.assert_next(TokenKind::Assign);

			let rexpr = parse_expr(&input, tokens)?;

			Ok(Field::ExprAssign(lexpr, rexpr))
		},
		// exp
		_ => match parse_expr(&input, tokens) {
			Ok(e) => Ok(Field::PosAssign(e)),
			_ => Err(()),
		},
	}
}

/// tableconstructor ::= `{` [fieldlist] `}`
pub fn parse_table_constructor(input: &str, tokens: &mut TokenIter) -> Result<TableConstructor> {
	match tokens.next().map(to_kind) {
		Some(TokenKind::LCurly) => {
			if let Some(TokenKind::RCurly) = tokens.peek().map(to_kind) {
				tokens.next();
				return Ok(TableConstructor(vec![]));
			};
			let fieldlist = parse_fieldlist(&input, tokens)?;
			match tokens.next().map(to_kind) {
				Some(TokenKind::RCurly) => Ok(TableConstructor(fieldlist)),
				_ => Err(()),
			}
		},
		_ => Err(()),
	}
}

/// varlist ::= var {`,` var}
pub fn parse_varlist(input: &str, tokens: &mut TokenIter) -> Result<Vec<Var>> {
	let mut varlist = vec![];

	if let Ok(var) = parse_var(&input, tokens) {
		varlist.push(var);
	}

	while let Some(TokenKind::Comma) = tokens.peek().map(to_kind) {
		tokens.next();
		match parse_var(&input, tokens) {
			Ok(v) => {
				varlist.push(v);
			},

			// TODO: fix err
			Err(_) => {
				tokens.prev();
				break;
			},
		}
	}

	Ok(varlist)
}

/// explist ::= exp {`,` exp}
pub fn parse_exprlist(input: &str, tokens: &mut TokenIter) -> Result<Vec<Expr>> {
	let mut exprs = vec![];

	if let Ok(expr) = parse_expr(&input, tokens) {
		exprs.push(expr);
	}

	while let Some(TokenKind::Comma) = tokens.peek().map(to_kind) {
		tokens.next();
		match parse_expr(&input, tokens) {
			Ok(expr) => {
				exprs.push(expr);
			},
			// TODO: fix err
			Err(_) => {
				tokens.prev();
				break;
			},
		}
	}

	Ok(exprs)
}

/// args ::=  `(` [explist] `)`
pub fn parse_args(input: &str, tokens: &mut TokenIter) -> Result<Args> {
	match tokens.next().map(to_kind) {
		Some(TokenKind::LParen) => {
			if let Some(TokenKind::RParen) = tokens.peek().map(to_kind) {
				tokens.next();
				return Ok(Args(vec![]));
			}
			let explist = parse_exprlist(&input, tokens)?;
			tokens.assert_next(TokenKind::RParen);
			Ok(Args(explist))
		},
		_ => Err(()),
	}
}

/// var ::=  Name | prefixexp `[` exp `]` | prefixexp `.` Name
/// e.g.:
/// foo
/// bar[0]
/// bar.bizz
pub fn parse_var(input: &str, tokens: &mut TokenIter) -> Result<Var> {
	match tokens.peek().map(to_kind) {
		Some(TokenKind::Ident) => {
			let name_token = tokens.next();

			// TODO: these are completely incomprehensible, fix it
			match tokens.peek().map(to_kind) {
				Some(TokenKind::LBracket) => {
					tokens.next();

					let expr = parse_expr(&input, tokens)?;

					tokens.assert_next(TokenKind::RBracket);

					Ok(Var::IndexExpr(IndexExpr {
						expr: Box::new(PrefixExpr::Var(Var::Name(Name(
							name_token.unwrap().span.as_str(input).to_string(),
						)))),
						arg: expr,
					}))
				},
				Some(TokenKind::Period) => {
					tokens.next();

					if let Some(TokenKind::Ident) = tokens.next().map(to_kind) {
						Ok(Var::PropertyAccess(PropertyAccess {
							expr: Box::new(PrefixExpr::Var(Var::Name(Name(
								name_token.unwrap().span.as_str(input).to_string(),
							)))),
							name: Name(tokens.cur().unwrap().span.as_str(input).to_string()),
						}))
					} else {
						Err(())
					}
				},
				_ => Ok(Var::Name(Name(tokens.cur().unwrap().span.as_str(input).to_string()))),
			}
		},
		Some(_) => {
			let prefixexp = parse_prefix_exp(&input, tokens)?;

			match tokens.peek().map(to_kind) {
				Some(TokenKind::LBracket) => {
					tokens.next();

					let expr = parse_expr(&input, tokens)?;

					tokens.assert_next(TokenKind::RBracket);

					Ok(Var::IndexExpr(IndexExpr {
						expr: Box::new(prefixexp),
						arg: expr,
					}))
				},
				Some(TokenKind::Period) => {
					tokens.next();

					if let Some(TokenKind::Ident) = tokens.next().map(to_kind) {
						Ok(Var::PropertyAccess(PropertyAccess {
							expr: Box::new(prefixexp),
							name: Name(tokens.cur().unwrap().span.as_str(input).to_string()),
						}))
					} else {
						Err(())
					}
				},
				_ => Err(()),
			}
		},
		None => Err(()),
	}
}

/// prefixexp ::= var | functioncall | `(` exp `)`
// functioncall ::=  prefixexp args | prefixexp `:` Name args
// var ::=  Name | prefixexp `[` exp `]` | prefixexp `.` Name
pub fn parse_prefix_exp(input: &str, tokens: &mut TokenIter) -> Result<PrefixExpr> {
	match tokens.peek().map(to_kind) {
		Some(TokenKind::LParen) => {
			tokens.next();
			let expr = parse_expr(&input, tokens).map(PrefixExpr::Expr);
			tokens.assert_next(TokenKind::RParen);
			expr
		},
		Some(TokenKind::Ident) => {
			tokens.next();
			match tokens.peek().map(to_kind) {
				Some(TokenKind::LBracket) => {
					tokens.next();

					let expr = parse_expr(&input, tokens)?;

					tokens.assert_next(TokenKind::RBracket);

					Ok(PrefixExpr::Var(Var::IndexExpr(IndexExpr {
						expr: Box::new(PrefixExpr::Var(Var::Name(Name(
							tokens.cur().unwrap().span.as_str(input).to_string(),
						)))),
						arg: expr,
					})))
				},
				Some(TokenKind::Period) => {
					tokens.next();

					if let Some(TokenKind::Ident) = tokens.next().map(to_kind) {
						Ok(PrefixExpr::Var(Var::PropertyAccess(PropertyAccess {
							expr: Box::new(PrefixExpr::Var(Var::Name(Name(
								tokens.cur().unwrap().span.as_str(input).to_string(),
							)))),
							name: Name(tokens.cur().unwrap().span.as_str(input).to_string()),
						})))
					} else {
						Err(())
					}
				},
				Some(TokenKind::LParen) => Ok(PrefixExpr::FunctionCall(FunctionCall {
					expr: Box::new(PrefixExpr::Var(Var::Name(Name(
						tokens.cur().unwrap().span.as_str(input).to_string(),
					)))),
					args: parse_args(&input, tokens)?,
				})),
				_ => Ok(PrefixExpr::Var(Var::Name(Name(
					tokens.cur().unwrap().span.as_str(input).to_string(),
				)))),
			}
		},
		_ => Err(()),
	}
}

/// stat ::=  `;` |
///         varlist `=` explist |
///         functioncall |
///         label |
///         break |
///         goto Name |
///         do block end |
///         while exp do block end |
///         repeat block until exp |
///         if exp then block {elseif exp then block} [else block] end |
///         for Name `=` exp `,` exp [`,` exp] do block end |
///         for namelist in explist do block end |
///         function funcname funcbody |
///         local function Name funcbody |
///         local namelist [`=` explist]
pub fn parse_stat(input: &str, tokens: &mut TokenIter) -> Result<Stat> {
	match tokens.peek().map(to_kind) {
		Some(TokenKind::SemiColon) => {
			tokens.next();
			Ok(Stat::SemiColon)
		},
		Some(TokenKind::Break) => {
			tokens.next();
			Ok(Stat::Break)
		},
		Some(TokenKind::Do) => parse_do_block(&input, tokens).map(Stat::DoBlock),
		Some(TokenKind::While) => parse_while_block(&input, tokens).map(Stat::WhileBlock),
		Some(TokenKind::If) => parse_if_block(&input, tokens).map(|f| Stat::IfBlock(Box::new(f))),
		Some(TokenKind::For) => {
			tokens.next();
			tokens.next();
			if let Some(TokenKind::Assign) = tokens.peek().map(to_kind) {
				tokens.prev();
				tokens.prev();
				parse_for_range(&input, tokens).map(|f| Stat::ForRange(Box::new(f)))
			} else {
				tokens.prev();
				tokens.prev();
				parse_for_in(&input, tokens).map(Stat::ForIn)
			}
		},
		Some(TokenKind::Function) => parse_function_def(&input, tokens).map(Stat::FunctionDef),
		Some(TokenKind::Local) => {
			tokens.next();
			if let Some(TokenKind::Function) = tokens.peek().map(to_kind) {
				tokens.prev();
				parse_local_function_def(&input, tokens).map(Stat::LocalFunctionDef)
			} else {
				tokens.prev();
				parse_local_assignment(&input, tokens).map(Stat::LocalAssignment)
			}
		},
		Some(TokenKind::Ident) => {
			tokens.next();
			if let Some(TokenKind::LParen) = tokens.peek().map(to_kind) {
				Ok(Stat::FunctionCall(FunctionCall {
					expr: Box::new(PrefixExpr::Var(Var::Name(Name(
						tokens.cur().unwrap().span.as_str(input).to_string(),
					)))),
					args: parse_args(&input, tokens)?,
				}))
			} else {
				tokens.prev();
				parse_assignment(&input, tokens).map(Stat::Assignment)
			}
		},
		other => panic!("not a valid statement: {:?}", other),
	}
}

/// varlist `=` explist
pub fn parse_assignment(input: &str, tokens: &mut TokenIter) -> Result<Assignment> {
	let varlist = parse_varlist(&input, tokens)?;

	if let Some(TokenKind::Assign) = tokens.peek().map(to_kind) {
		tokens.next();
	} else {
		return Err(());
	}

	let exprlist = parse_exprlist(&input, tokens)?;

	Ok(Assignment { varlist, exprlist })
}

/// local namelist [`=` explist]
pub fn parse_local_assignment(input: &str, tokens: &mut TokenIter) -> Result<LocalAssignment> {
	if let Some(TokenKind::Local) = tokens.peek().map(to_kind) {
		tokens.next();
	} else {
		return Err(());
	}

	let namelist = parse_namelist(&input, tokens)?;

	let exprlist = if let Some(TokenKind::Assign) = tokens.peek().map(to_kind) {
		tokens.next();
		Some(parse_exprlist(&input, tokens)?)
	} else {
		None
	};

	Ok(LocalAssignment { namelist, exprlist })
}

/// local function Name funcbody
pub fn parse_local_function_def(input: &str, tokens: &mut TokenIter) -> Result<LocalFunctionDef> {
	if let Some(TokenKind::Local) = tokens.peek().map(to_kind) {
		tokens.next();
	} else {
		return Err(());
	}
	if let Some(TokenKind::Function) = tokens.peek().map(to_kind) {
		tokens.next();
	} else {
		return Err(());
	}

	let name = match tokens.next().map(to_kind) {
		Some(TokenKind::Ident) => Name(tokens.cur().unwrap().span.as_str(input).to_string()),
		_ => return Err(()),
	};

	let body = parse_funcbody(&input, tokens)?;

	Ok(LocalFunctionDef { name, body })
}

/// function funcname funcbody
pub fn parse_function_def(input: &str, tokens: &mut TokenIter) -> Result<FunctionDef> {
	if let Some(TokenKind::Function) = tokens.peek().map(to_kind) {
		tokens.next();
	} else {
		return Err(());
	}

	let name = parse_funcname(&input, tokens)?;

	let body = parse_funcbody(&input, tokens)?;

	Ok(FunctionDef { name, body })
}

/// for namelist in explist do block end
pub fn parse_for_in(input: &str, tokens: &mut TokenIter) -> Result<ForIn> {
	if let Some(TokenKind::For) = tokens.peek().map(to_kind) {
		tokens.next();
	} else {
		return Err(());
	}

	let namelist = parse_namelist(&input, tokens)?;

	if tokens.peek().map(to_kind) == Some(TokenKind::In) {
		tokens.next();
	} else {
		return Err(());
	}

	let exprlist = parse_exprlist(&input, tokens)?;

	let block = parse_do_block(&input, tokens)?;

	Ok(ForIn {
		namelist,
		exprlist,
		block,
	})
}

/// for Name `=` exp `,` exp [`,` exp] do block end
pub fn parse_for_range(input: &str, tokens: &mut TokenIter) -> Result<ForRange> {
	if let Some(TokenKind::For) = tokens.peek().map(to_kind) {
		tokens.next();
	} else {
		return Err(());
	}

	let name = match tokens.next().map(to_kind) {
		Some(TokenKind::Ident) => Name(tokens.cur().unwrap().span.as_str(input).to_string()),
		_ => return Err(()),
	};

	if let Some(TokenKind::Assign) = tokens.peek().map(to_kind) {
		tokens.next();
	} else {
		return Err(());
	}

	let exp_start = parse_expr(&input, tokens)?;

	if let Some(TokenKind::Comma) = tokens.peek().map(to_kind) {
		tokens.next();
	} else {
		return Err(());
	}

	let exp_end = parse_expr(&input, tokens)?;

	let exp_step = if let Some(TokenKind::Comma) = tokens.peek().map(to_kind) {
		tokens.next();
		Some(parse_expr(&input, tokens)?)
	} else {
		None
	};

	let block = parse_do_block(&input, tokens)?;

	Ok(ForRange {
		name,
		exprs: (exp_start, exp_end, exp_step),
		block,
	})
}

/// elseif exp then block
pub fn parse_elseif(input: &str, tokens: &mut TokenIter) -> Result<ElseIf> {
	tokens.assert_next(TokenKind::ElseIf);
	let expr = parse_expr(&input, tokens)?;
	tokens.assert_next(TokenKind::Then);
	let block = parse_block(&input, tokens)?;

	Ok(ElseIf { block, expr })
}

/// else block
pub fn parse_else_block(input: &str, tokens: &mut TokenIter) -> Result<Option<Block>> {
	if let Some(TokenKind::Else) = tokens.peek().map(to_kind) {
		tokens.next();
		Ok(Some(parse_block(&input, tokens)?))
	} else {
		Ok(None)
	}
}

/// if exp then block {elseif exp then block} [else block] end
pub fn parse_if_block(input: &str, tokens: &mut TokenIter) -> Result<IfBlock> {
	tokens.assert_next(TokenKind::If);

	let expr = parse_expr(&input, tokens)?;

	tokens.assert_next(TokenKind::Then);

	let block = parse_block(&input, tokens)?;

	let mut elseif = vec![];

	while tokens.peek().map(to_kind) == Some(TokenKind::ElseIf) {
		match parse_elseif(&input, tokens) {
			Ok(elif) => elseif.push(elif),
			Err(e) => return Err(e),
		};
	}

	let else_blk = parse_else_block(&input, tokens)?;

	tokens.assert_next(TokenKind::End);

	Ok(IfBlock {
		expr,
		block,
		elseif,
		else_blk,
	})
}

/// while exp do block end
pub fn parse_while_block(input: &str, tokens: &mut TokenIter) -> Result<WhileBlock> {
	tokens.assert_next(TokenKind::While);

	let expr = parse_expr(&input, tokens)?;
	let block = parse_do_block(&input, tokens)?;

	Ok(WhileBlock { block, expr })
}

/// do block end
pub fn parse_do_block(input: &str, tokens: &mut TokenIter) -> Result<Block> {
	tokens.assert_next(TokenKind::Do);
	let blk = parse_block(&input, tokens)?;
	tokens.assert_next(TokenKind::End);

	Ok(blk)
}

/// block ::= {stat} [retstat]
pub fn parse_block(input: &str, tokens: &mut TokenIter) -> Result<Block> {
	let mut stats = vec![];

	loop {
		match tokens.peek().map(to_kind) {
			Some(TokenKind::End) | Some(TokenKind::Else) | Some(TokenKind::ElseIf) | None => break,
			other => {
				if other == Some(TokenKind::Return) {
					let retstat = parse_retstat(&input, tokens)?;
					return Ok(Block {
						stats,
						retstat: Some(retstat),
					});
				} else if let Ok(stat) = parse_stat(&input, tokens) {
					stats.push(stat);
				} else {
					panic!("Expected statement at {:?}", other);
				}
			},
		}
	}
	Ok(Block { stats, retstat: None })
}

/// retstat ::= return [explist] [`;`]
pub fn parse_retstat(input: &str, tokens: &mut TokenIter) -> Result<Vec<Expr>> {
	tokens.assert_next(TokenKind::Return);

	let exprlist = parse_exprlist(&input, tokens)?;

	if let Some(TokenKind::SemiColon) = tokens.peek().map(to_kind) {
		tokens.next();
	}
	Ok(exprlist)
}

/// funcbody ::= `(` [parlist] `)` block end
pub fn parse_funcbody(input: &str, tokens: &mut TokenIter) -> Result<FuncBody> {
	tokens.assert_next(TokenKind::LParen);
	let params = parse_parlist(&input, tokens)?;
	tokens.assert_next(TokenKind::RParen);
	let body = parse_block(&input, tokens)?;
	tokens.assert_next(TokenKind::End);

	Ok(FuncBody { params, body })
}

/// functiondef ::= function funcbody
/// stat ::= function funcname funcbody
/// stat ::= local function Name funcbody
pub fn parse_functiondef(input: &str, tokens: &mut TokenIter) -> Result<FunctionDef> {
	if let Some(TokenKind::Function) = tokens.next().map(to_kind) {
		let name = parse_funcname(&input, tokens)?;
		let body = parse_funcbody(&input, tokens)?;

		Ok(FunctionDef { name, body })
	} else {
		Err(())
	}
}

/// namelist ::= Name {`,` Name}
pub fn parse_namelist(input: &str, tokens: &mut TokenIter) -> Result<Vec<Name>> {
	let first_name = match tokens.next().map(to_kind) {
		Some(TokenKind::Ident) => tokens.cur().unwrap().span.as_str(input).to_string(),
		_ => return Err(()),
	};

	let mut names = vec![Name(first_name)];

	while tokens.peek().map(to_kind) == Some(TokenKind::Comma) {
		tokens.next();
		match tokens.next().map(to_kind) {
			Some(TokenKind::Ident) => {
				names.push(Name(tokens.cur().unwrap().span.as_str(input).to_string()));
			},
			_ => return Err(()),
		}
	}

	Ok(names)
}

/// parlist ::= namelist [`,`]
pub fn parse_parlist(input: &str, tokens: &mut TokenIter) -> Result<Params> {
	match tokens.peek().map(to_kind) {
		// namelist
		Some(TokenKind::Ident) => {
			let names = parse_namelist(&input, tokens)?;

			// [',']
			match tokens.peek().map(to_kind) {
				Some(TokenKind::Comma) => {
					tokens.next();
				},
				_ => (),
			};
			Ok(Params { names })
		},
		_ => Ok(Params { names: vec![] }),
	}
}

/// fieldlist ::= field {fieldsep field} [fieldsep]
/// fieldsep ::= `,` | `;`
pub fn parse_fieldlist(input: &str, tokens: &mut TokenIter) -> Result<Vec<Field>> {
	let mut fields = vec![];

	loop {
		let f = parse_field(&input, tokens)?;
		fields.push(f);

		match tokens.peek().map(to_kind) {
			Some(TokenKind::Comma) | Some(TokenKind::SemiColon) => {
				tokens.next();
				continue;
			},
			_ => break,
		}
	}
	Ok(fields)
}

fn parse_string(s: &str) -> &str {
	let mut chars = s.chars();
	match chars.next() {
		Some('\'') | Some('\"') => {
			chars.next_back();
			chars.as_str()
		},
		_ => panic!("Malformed string {}", s),
	}
}
