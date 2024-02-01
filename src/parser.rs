use crate::ast::*;
use crate::iter::{PeekableIterator, TokenIter};
use crate::lexer::{Lexer, Token, TokenKind};
use std;

type Result<T> = std::result::Result<T, ()>;

pub fn to_kind(tk: &Token) -> TokenKind {
	tk.kind.clone()
}

/// parse a given blob of lua text
pub fn parse(text: &str) -> Result<Block> {
	let tokens: Vec<_> = Lexer::new(text)
		.filter(|t| match t {
			Token {
				kind: TokenKind::Comment(_),
				..
			} => false,
			_ => true,
		})
		.collect();
	dbg!(&tokens);
	let mut tokens = TokenIter::new(&tokens);
	parse_block(&mut tokens)
}

/// funcname ::= Name {‘.’ Name} [‘:’ Name]
pub fn parse_funcname(tokens: &mut TokenIter<Token>) -> Result<FuncName> {
	let first_name = tokens.next();
	let first_name = match first_name {
		Some(Token {
			kind: TokenKind::Ident(first_name),
			..
		}) => first_name,
		_ => return Err(()),
	};
	let mut path = vec![Name(first_name.clone())];
	// if next token is period then loop
	while tokens.peek().map(to_kind) == Some(TokenKind::Period) {
		tokens.next();
		match tokens.next() {
			Some(Token {
				kind: TokenKind::Ident(ref content),
				..
			}) => {
				path.push(Name(content.clone()));
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
				kind: TokenKind::Ident(ref name),
				..
			}) => {
				method = Some(Name(name.clone()));
			},
			_ => return Err(()),
		}
	}
	Ok(FuncName { path, method })
}

pub fn parse_simple_exp(tokens: &mut TokenIter<Token>) -> Result<Expr> {
	match tokens.cur().map(to_kind) {
		Some(TokenKind::Nil) => Ok(Expr::Nil),
		Some(TokenKind::True) => Ok(Expr::Bool(true)),
		Some(TokenKind::False) => Ok(Expr::Bool(false)),
		Some(TokenKind::String(s)) => Ok(Expr::Str(s.to_string())),
		Some(TokenKind::Number(n)) => Ok(Expr::Num(n)),
		Some(TokenKind::Function) => {
			tokens.next_back();
			parse_functiondef(tokens).map(Expr::FuncDef)
		},
		Some(TokenKind::LCurly) => {
			tokens.next_back();
			parse_table_constructor(tokens).map(Expr::Table)
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

pub fn parse_unexp(tokens: &mut TokenIter<Token>) -> Result<Expr> {
	match tokens.next().map(to_kind) {
		tk @ Some(TokenKind::Minus) | tk @ Some(TokenKind::Not) | tk @ Some(TokenKind::Hash) => {
			let op = match tk {
				Some(TokenKind::Minus) => Unop::Minus,
				Some(TokenKind::Not) => Unop::Not,
				Some(TokenKind::Hash) => Unop::Len,
				_ => return Err(()),
			};

			let exp = parse_sub_expr(tokens, UNARY_PRIORITY)?;
			Ok(Expr::UnExp(UnExp { op, exp: Box::new(exp) }))
		},
		_ => Err(()),
	}
}

/// exp ::= nil | false | true | Numeral | LiteralString | ‘...’ | functiondef |
///         prefixexp | tableconstructor | exp binop exp | unop exp
pub fn parse_expr(tokens: &mut TokenIter<Token>) -> Result<Expr> {
	parse_sub_expr(tokens, 0)
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
pub fn parse_sub_expr(tokens: &mut TokenIter<Token>, min_priority: i32) -> Result<Expr> {
	let mut expression = parse_unexp(tokens).or_else(|_| parse_simple_exp(tokens)).or_else(|_| {
		tokens.next_back();
		parse_prefix_exp(tokens).map(|x| Expr::PrefixExp(Box::new(x)))
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

		let rhs = match parse_sub_expr(tokens, prority) {
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

/// field ::= ‘[’ exp ‘]’ ‘=’ exp | Name ‘=’ exp | exp
pub fn parse_field(tokens: &mut TokenIter<Token>) -> Result<Field> {
	match tokens.next().map(to_kind) {
		// Name '=' exp
		Some(TokenKind::Ident(name)) => {
			tokens.assert_next(&TokenKind::Assign)?;

			let expr = match parse_expr(tokens) {
				Ok(expr) => expr,
				Err(_) => {
					return Err(());
				},
			};

			Ok(Field::NameAssign(Name(name.clone()), expr))
		},
		// '[' exp ']' '=' exp
		Some(TokenKind::LBracket) => {
			let lexpr = parse_expr(tokens)?;

			tokens.assert_next(&TokenKind::RBracket)?;

			tokens.assert_next(&TokenKind::Assign)?;

			let rexpr = parse_expr(tokens)?;

			Ok(Field::ExprAssign(lexpr, rexpr))
		},
		// exp
		_ => match parse_expr(tokens) {
			Ok(e) => Ok(Field::PosAssign(e)),
			_ => Err(()),
		},
	}
}

/// tableconstructor ::= ‘{’ [fieldlist] ‘}’
pub fn parse_table_constructor(tokens: &mut TokenIter<Token>) -> Result<TableConstructor> {
	match tokens.next().map(to_kind) {
		Some(TokenKind::LCurly) => {
			if let Some(TokenKind::RCurly) = tokens.peek().map(to_kind) {
				tokens.next();
				return Ok(TableConstructor(vec![]));
			};
			let fieldlist = parse_fieldlist(tokens)?;
			match tokens.next().map(to_kind) {
				Some(TokenKind::RCurly) => Ok(TableConstructor(fieldlist)),
				_ => Err(()),
			}
		},
		_ => Err(()),
	}
}

/// varlist ::= var {‘,’ var}
pub fn parse_varlist(tokens: &mut TokenIter<Token>) -> Result<Vec<Var>> {
	let mut varlist = vec![];

	if let Ok(var) = parse_var(tokens) {
		varlist.push(var);
	}

	while let Some(TokenKind::Comma) = tokens.peek().map(to_kind) {
		tokens.next();
		match parse_var(tokens) {
			Ok(v) => {
				varlist.push(v);
			},
			Err(_) => {
				tokens.next_back();
				break;
			},
		}
	}

	Ok(varlist)
}

/// explist ::= exp {‘,’ exp}
pub fn parse_exprlist(tokens: &mut TokenIter<Token>) -> Result<Vec<Expr>> {
	let mut exprs = vec![];

	if let Ok(expr) = parse_expr(tokens) {
		exprs.push(expr);
	}

	while let Some(TokenKind::Comma) = tokens.peek().map(to_kind) {
		tokens.next();
		match parse_expr(tokens) {
			Ok(expr) => {
				exprs.push(expr);
			},
			Err(_) => {
				tokens.next_back();
				break;
			},
		}
	}

	Ok(exprs)
}

/// args ::=  ‘(’ [explist] ‘)’ | tableconstructor | LiteralString
pub fn parse_args(tokens: &mut TokenIter<Token>) -> Result<Args> {
	match tokens.next().map(to_kind) {
		Some(TokenKind::String(s)) => Ok(Args::String(s.to_string())),
		Some(TokenKind::LParen) => {
			if let Some(TokenKind::RParen) = tokens.peek().map(to_kind) {
				tokens.next();
				return Ok(Args::ExprList(vec![]));
			}
			parse_exprlist(tokens).map(Args::ExprList)
		},
		Some(TokenKind::LCurly) => {
			// parse_table_constructor() expects a first token of LCurly
			tokens.next_back();
			parse_table_constructor(tokens).map(Args::TableConstructor)
		},
		_ => Err(()),
	}
}

/// var ::=  Name | prefixexp ‘[’ exp ‘]’ | prefixexp ‘.’ Name
/// e.g.:
/// foo
/// bar[0]
/// bar.bizz
pub fn parse_var(tokens: &mut TokenIter<Token>) -> Result<Var> {
	match tokens.peek().map(to_kind) {
		Some(TokenKind::Ident(ident)) => {
			tokens.next();
			match tokens.peek().map(to_kind) {
				Some(TokenKind::LBracket) => {
					tokens.next();

					let expr = parse_expr(tokens)?;

					tokens.assert_next(&TokenKind::RBracket)?;

					Ok(Var::IndexExpr(IndexExpr {
						expr: Box::new(PrefixExpr::Var(Var::Name(Name(ident.to_string())))),
						arg: expr,
					}))
				},
				Some(TokenKind::Period) => {
					tokens.next();

					if let Some(TokenKind::Ident(name)) = tokens.next().map(to_kind) {
						Ok(Var::PropertyAccess(PropertyAccess {
							expr: Box::new(PrefixExpr::Var(Var::Name(Name(ident.to_string())))),
							name: Name(name.to_string()),
						}))
					} else {
						Err(())
					}
				},
				_ => Ok(Var::Name(Name(ident.to_string()))),
			}
		},
		Some(_) => {
			let prefixexp = parse_prefix_exp(tokens)?;

			match tokens.peek().map(to_kind) {
				Some(TokenKind::LBracket) => {
					tokens.next();

					let expr = parse_expr(tokens)?;

					tokens.assert_next(&TokenKind::RBracket)?;

					Ok(Var::IndexExpr(IndexExpr {
						expr: Box::new(prefixexp),
						arg: expr,
					}))
				},
				Some(TokenKind::Period) => {
					tokens.next();

					if let Some(TokenKind::Ident(name)) = tokens.next().map(to_kind) {
						Ok(Var::PropertyAccess(PropertyAccess {
							expr: Box::new(prefixexp),
							name: Name(name.to_string()),
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

/// prefixexp ::= var | functioncall | ‘(’ exp ‘)’
// functioncall ::=  prefixexp args | prefixexp ‘:’ Name args
// var ::=  Name | prefixexp ‘[’ exp ‘]’ | prefixexp ‘.’ Name
pub fn parse_prefix_exp(tokens: &mut TokenIter<Token>) -> Result<PrefixExpr> {
	match tokens.peek().map(to_kind) {
		Some(TokenKind::LParen) => {
			tokens.next();
			let expr = parse_expr(tokens).map(PrefixExpr::Expr);
			tokens.assert_next(&TokenKind::RParen)?;
			expr
		},
		Some(TokenKind::Ident(ident)) => {
			tokens.next();
			match tokens.peek().map(to_kind) {
				Some(TokenKind::LBracket) => {
					tokens.next();

					let expr = parse_expr(tokens)?;

					tokens.assert_next(&TokenKind::RBracket)?;

					Ok(PrefixExpr::Var(Var::IndexExpr(IndexExpr {
						expr: Box::new(PrefixExpr::Var(Var::Name(Name(ident.to_string())))),
						arg: expr,
					})))
				},
				Some(TokenKind::Period) => {
					tokens.next();

					if let Some(TokenKind::Ident(name)) = tokens.next().map(to_kind) {
						Ok(PrefixExpr::Var(Var::PropertyAccess(PropertyAccess {
							expr: Box::new(PrefixExpr::Var(Var::Name(Name(ident.to_string())))),
							name: Name(name.to_string()),
						})))
					} else {
						Err(())
					}
				},
				Some(TokenKind::LParen) => Ok(PrefixExpr::FunctionCall(FunctionCall {
					expr: Box::new(PrefixExpr::Var(Var::Name(Name(ident.to_string())))),
					args: parse_args(tokens)?,
				})),
				_ => Ok(PrefixExpr::Var(Var::Name(Name(ident.to_string())))),
			}
		},
		_ => Err(()),
	}
}

/// stat ::=  ‘;’ |
///         varlist ‘=’ explist |
///         functioncall |
///         label |
///         break |
///         goto Name |
///         do block end |
///         while exp do block end |
///         repeat block until exp |
///         if exp then block {elseif exp then block} [else block] end |
///         for Name ‘=’ exp ‘,’ exp [‘,’ exp] do block end |
///         for namelist in explist do block end |
///         function funcname funcbody |
///         local function Name funcbody |
///         local namelist [‘=’ explist]
pub fn parse_stat(tokens: &mut TokenIter<Token>) -> Result<Stat> {
	match tokens.peek().map(to_kind) {
		Some(TokenKind::SemiColon) => {
			tokens.next();
			Ok(Stat::SemiColon)
		},
		Some(TokenKind::Break) => {
			tokens.next();
			Ok(Stat::Break)
		},
		Some(TokenKind::Do) => parse_do_block(tokens).map(Stat::DoBlock),
		Some(TokenKind::While) => parse_while_block(tokens).map(Stat::WhileBlock),
		Some(TokenKind::If) => parse_if_block(tokens).map(|f| Stat::IfBlock(Box::new(f))),
		Some(TokenKind::For) => {
			tokens.next();
			tokens.next();
			if let Some(TokenKind::Assign) = tokens.peek().map(to_kind) {
				tokens.next_back();
				tokens.next_back();
				parse_for_range(tokens).map(|f| Stat::ForRange(Box::new(f)))
			} else {
				tokens.next_back();
				tokens.next_back();
				parse_for_in(tokens).map(Stat::ForIn)
			}
		},
		Some(TokenKind::Function) => parse_function_def(tokens).map(Stat::FunctionDef),
		Some(TokenKind::Local) => {
			tokens.next();
			if let Some(TokenKind::Function) = tokens.peek().map(to_kind) {
				tokens.next_back();
				parse_local_function_def(tokens).map(Stat::LocalFunctionDef)
			} else {
				tokens.next_back();
				parse_local_assignment(tokens).map(Stat::LocalAssignment)
			}
		},
		Some(TokenKind::Ident(ident)) => {
			tokens.next();
			if let Some(TokenKind::LParen) = tokens.peek().map(to_kind) {
				Ok(Stat::FunctionCall(FunctionCall {
					expr: Box::new(PrefixExpr::Var(Var::Name(Name(ident.to_string())))),
					args: parse_args(tokens)?,
				}))
			} else {
				tokens.next_back();
				parse_assignment(tokens).map(Stat::Assignment)
			}
		},
		_ => Err(()),
	}
}

/// varlist ‘=’ explist
pub fn parse_assignment(tokens: &mut TokenIter<Token>) -> Result<Assignment> {
	let varlist = parse_varlist(tokens)?;

	if let Some(TokenKind::Assign) = tokens.peek().map(to_kind) {
		tokens.next();
	} else {
		return Err(());
	}

	let exprlist = parse_exprlist(tokens)?;

	Ok(Assignment { varlist, exprlist })
}

/// local namelist [‘=’ explist]
pub fn parse_local_assignment(tokens: &mut TokenIter<Token>) -> Result<LocalAssignment> {
	if let Some(TokenKind::Local) = tokens.peek().map(to_kind) {
		tokens.next();
	} else {
		return Err(());
	}

	let namelist = parse_namelist(tokens)?;

	let exprlist = if let Some(TokenKind::Assign) = tokens.peek().map(to_kind) {
		tokens.next();
		Some(parse_exprlist(tokens)?)
	} else {
		None
	};

	Ok(LocalAssignment { namelist, exprlist })
}

/// local function Name funcbody
pub fn parse_local_function_def(tokens: &mut TokenIter<Token>) -> Result<LocalFunctionDef> {
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
		Some(TokenKind::Ident(name)) => Name(name.to_string()),
		_ => return Err(()),
	};

	let body = parse_funcbody(tokens)?;

	Ok(LocalFunctionDef { name, body })
}

/// function funcname funcbody
pub fn parse_function_def(tokens: &mut TokenIter<Token>) -> Result<FunctionDef> {
	if let Some(TokenKind::Function) = tokens.peek().map(to_kind) {
		tokens.next();
	} else {
		return Err(());
	}

	let name = parse_funcname(tokens)?;

	let body = parse_funcbody(tokens)?;

	Ok(FunctionDef { name, body })
}

/// for namelist in explist do block end
pub fn parse_for_in(tokens: &mut TokenIter<Token>) -> Result<ForIn> {
	if let Some(TokenKind::For) = tokens.peek().map(to_kind) {
		tokens.next();
	} else {
		return Err(());
	}

	let namelist = parse_namelist(tokens)?;

	if let Some(TokenKind::In) = tokens.peek().map(to_kind) {
		tokens.next();
	} else {
		return Err(());
	}

	let exprlist = parse_exprlist(tokens)?;

	let block = parse_do_block(tokens)?;

	Ok(ForIn {
		namelist,
		exprlist,
		block,
	})
}

/// for Name ‘=’ exp ‘,’ exp [‘,’ exp] do block end
pub fn parse_for_range(tokens: &mut TokenIter<Token>) -> Result<ForRange> {
	if let Some(TokenKind::For) = tokens.peek().map(to_kind) {
		tokens.next();
	} else {
		return Err(());
	}

	let name = match tokens.next().map(to_kind) {
		Some(TokenKind::Ident(name)) => Name(name.to_string()),
		_ => return Err(()),
	};

	if let Some(TokenKind::Assign) = tokens.peek().map(to_kind) {
		tokens.next();
	} else {
		return Err(());
	}

	let exp_start = parse_expr(tokens)?;

	if let Some(TokenKind::Comma) = tokens.peek().map(to_kind) {
		tokens.next();
	} else {
		return Err(());
	}

	let exp_end = parse_expr(tokens)?;

	let exp_step = if let Some(TokenKind::Comma) = tokens.peek().map(to_kind) {
		tokens.next();
		Some(parse_expr(tokens)?)
	} else {
		None
	};

	let block = parse_do_block(tokens)?;

	Ok(ForRange {
		name,
		exprs: (exp_start, exp_end, exp_step),
		block,
	})
}

/// elseif exp then block
pub fn parse_elseif(tokens: &mut TokenIter<Token>) -> Result<ElseIf> {
	if let Some(TokenKind::ElseIf) = tokens.peek().map(to_kind) {
		tokens.next();
	} else {
		return Err(());
	}

	let expr = parse_expr(tokens)?;

	if let Some(TokenKind::Then) = tokens.peek().map(to_kind) {
		tokens.next();
	} else {
		return Err(());
	}

	let block = parse_block(tokens)?;

	Ok(ElseIf { block, expr })
}

/// else block
pub fn parse_else_block(tokens: &mut TokenIter<Token>) -> Result<Option<Block>> {
	if let Some(TokenKind::Else) = tokens.peek().map(to_kind) {
		tokens.next();
		Ok(Some(parse_block(tokens)?))
	} else {
		Ok(None)
	}
}

/// if exp then block {elseif exp then block} [else block] end
pub fn parse_if_block(tokens: &mut TokenIter<Token>) -> Result<IfBlock> {
	if let Some(TokenKind::If) = tokens.peek().map(to_kind) {
		tokens.next();
	} else {
		return Err(());
	}

	let expr = parse_expr(tokens)?;

	if let Some(TokenKind::Then) = tokens.peek().map(to_kind) {
		tokens.next();
	} else {
		return Err(());
	}

	let block = parse_block(tokens)?;

	let mut elseif = vec![];

	while let Ok(elif) = parse_elseif(tokens) {
		elseif.push(elif);
	}

	let else_blk = parse_else_block(tokens)?;

	Ok(IfBlock {
		expr,
		block,
		elseif,
		else_blk,
	})
}

/// while exp do block end
pub fn parse_while_block(tokens: &mut TokenIter<Token>) -> Result<WhileBlock> {
	if let Some(TokenKind::While) = tokens.peek().map(to_kind) {
		tokens.next();
	} else {
		return Err(());
	}

	let expr = parse_expr(tokens)?;

	let block = parse_do_block(tokens)?;

	Ok(WhileBlock { block, expr })
}

/// do block end
pub fn parse_do_block(tokens: &mut TokenIter<Token>) -> Result<Block> {
	if let Some(TokenKind::Do) = tokens.peek().map(to_kind) {
		tokens.next();
	} else {
		return Err(());
	}

	let blk = parse_block(tokens)?;

	if let Some(TokenKind::End) = tokens.next().map(to_kind) {
		Ok(blk)
	} else {
		Err(())
	}
}

/// block ::= {stat} [retstat]
pub fn parse_block(tokens: &mut TokenIter<Token>) -> Result<Block> {
	let mut stats = vec![];
	while let Ok(stat) = parse_stat(tokens) {
		stats.push(stat);
	}
	let retstat = parse_retstat(tokens).unwrap_or_default();
	Ok(Block { stats, retstat })
}

/// retstat ::= return [explist] [‘;’]
pub fn parse_retstat(tokens: &mut TokenIter<Token>) -> Result<Option<Vec<Expr>>> {
	match tokens.peek().map(to_kind) {
		Some(TokenKind::Return) => {
			tokens.next();
		},
		_ => return Ok(None),
	}

	let exprlist = parse_exprlist(tokens)?;

	if let Some(TokenKind::SemiColon) = tokens.peek().map(to_kind) {
		tokens.next();
	}
	Ok(Some(exprlist))
}

/// funcbody ::= ‘(’ [parlist] ‘)’ block end
pub fn parse_funcbody(tokens: &mut TokenIter<Token>) -> Result<FuncBody> {
	tokens.assert_next(&TokenKind::LParen)?;
	let params = parse_parlist(tokens)?;
	tokens.assert_next(&TokenKind::RParen)?;

	Ok(FuncBody {
		params,
		body: parse_block(tokens)?,
	})
}

/// functiondef ::= function funcbody
/// stat ::= function funcname funcbody
/// stat ::= local function Name funcbody
pub fn parse_functiondef(tokens: &mut TokenIter<Token>) -> Result<FunctionDef> {
	if let Some(TokenKind::Function) = tokens.next().map(to_kind) {
		let name = parse_funcname(tokens)?;
		let body = parse_funcbody(tokens)?;

		Ok(FunctionDef { name, body })
	} else {
		Err(())
	}
}

/// namelist ::= Name {‘,’ Name}
pub fn parse_namelist(tokens: &mut TokenIter<Token>) -> Result<Vec<Name>> {
	let first_name = match tokens.next().map(to_kind).ok_or_else(|| ())? {
		TokenKind::Ident(name) => name,
		_ => return Err(()),
	};

	let mut names = vec![Name(first_name.clone())];

	while tokens.peek().map(to_kind) == Some(TokenKind::Comma) {
		tokens.next();
		match tokens.next().map(to_kind) {
			Some(TokenKind::Ident(name)) => {
				names.push(Name(name.clone()));
			},
			_ => return Err(()),
		}
	}

	Ok(names)
}

/// parlist ::= namelist [‘,’ ‘...’] | ‘...’
pub fn parse_parlist(tokens: &mut TokenIter<Token>) -> Result<Params> {
	match tokens.peek().map(to_kind) {
		// namelist
		Some(TokenKind::Ident(_)) => {
			let names = parse_namelist(tokens).unwrap_or_default();

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
/// fieldsep ::= ‘,’ | ‘;’
pub fn parse_fieldlist(tokens: &mut TokenIter<Token>) -> Result<Vec<Field>> {
	let mut fields = vec![];

	while let Ok(f) = parse_field(tokens) {
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
