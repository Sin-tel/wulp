use crate::ast::*;
use crate::lexer::{Lexer, Token, TokenKind};
use crate::token_iter::TokenIter;

pub fn parse(input: &str) -> Block {
	let tokens: Vec<_> = Lexer::new(input)
		.filter(|t| match t {
			Token {
				kind: TokenKind::Comment(_),
				..
			} => false,
			_ => true,
		})
		.collect();

	// dbg!(&tokens);
	let mut tokens = TokenIter::new(tokens);
	let ast = parse_block(&input, &mut tokens);

	// make sure we are done
	if tokens.next().kind != TokenKind::Eof {
		panic!("Expected end of file at {:?}", tokens.cur());
	}
	ast
}

/// funcname ::= Name {`.` Name} [`:` Name]
pub fn parse_funcname(input: &str, tokens: &mut TokenIter) -> FuncName {
	tokens.assert_next(TokenKind::Ident);

	let mut path = vec![Name(tokens.cur().span.as_string(input))];
	// if next token is period then loop
	while tokens.peek().kind == (TokenKind::Period) {
		tokens.next();
		tokens.assert_next(TokenKind::Ident);
		path.push(Name(tokens.cur().span.as_string(input)));
	}
	let mut method: Option<Name> = None;
	if tokens.peek().kind == TokenKind::Colon {
		tokens.next();
		tokens.assert_next(TokenKind::Ident);
		method = Some(Name(tokens.cur().span.as_string(input)));
	}
	FuncName { path, method }
}

pub fn parse_simple_exp(input: &str, tokens: &mut TokenIter) -> Option<Expr> {
	match tokens.next().kind {
		TokenKind::Nil => Some(Expr::Nil),
		TokenKind::True => Some(Expr::Bool(true)),
		TokenKind::False => Some(Expr::Bool(false)),
		TokenKind::String => Some(Expr::Str(parse_string(tokens.cur().span.as_str(input)).to_string())),
		TokenKind::Number => {
			let s = tokens.cur().span.as_string(input);
			match s.parse() {
				Ok(num) => Some(Expr::Num(num)),
				_ => panic!("Malformed number"), // TODO: handle properly
			}
		},
		TokenKind::Function => {
			tokens.prev();
			Some(Expr::FuncDef(parse_functiondef(&input, tokens)))
		},
		TokenKind::LCurly => {
			tokens.prev();
			Some(Expr::Table(parse_table_constructor(&input, tokens)))
		},
		_ => None,
	}
}

pub fn bin_priority(op: Token) -> i32 {
	match op.kind {
		TokenKind::Pow => 8,
		TokenKind::Mul | TokenKind::Div | TokenKind::Mod => 6,
		TokenKind::Plus | TokenKind::Minus => 5,
		TokenKind::Concat => 4,
		TokenKind::Lt | TokenKind::Gt | TokenKind::Lte | TokenKind::Gte | TokenKind::Eq | TokenKind::Neq => 3,
		TokenKind::And => 2,
		TokenKind::Or => 1,
		_ => unreachable!(),
	}
}

const UNARY_PRIORITY: i32 = 7;

pub fn is_un_op(token: Token) -> bool {
	match token.kind {
		TokenKind::Minus | TokenKind::Not | TokenKind::Hash => true,
		_ => false,
	}
}

pub fn parse_unexp(input: &str, tokens: &mut TokenIter) -> Expr {
	let op = match tokens.next().kind {
		TokenKind::Minus => Unop::Minus,
		TokenKind::Not => Unop::Not,
		TokenKind::Hash => Unop::Len,
		_ => unreachable!(),
	};

	let exp = parse_sub_expr(&input, tokens, UNARY_PRIORITY);
	Expr::UnExp(UnExp { op, exp: Box::new(exp) })
}

/// exp ::= nil | false | true | Numeral | LiteralString | `...` | functiondef |
///         prefixexp | tableconstructor | exp binop exp | unop exp
pub fn parse_expr(input: &str, tokens: &mut TokenIter) -> Expr {
	parse_sub_expr(&input, tokens, 0)
}

pub fn is_bin_op(token: Token) -> bool {
	match token.kind {
		TokenKind::Plus
		| TokenKind::Minus
		| TokenKind::Mul
		| TokenKind::Div
		| TokenKind::Pow
		| TokenKind::Mod
		| TokenKind::Concat
		| TokenKind::Lt
		| TokenKind::Lte
		| TokenKind::Gt
		| TokenKind::Gte
		| TokenKind::Eq
		| TokenKind::Neq
		| TokenKind::And
		| TokenKind::Or => true,
		_ => false,
	}
}

// subexpr ::= (simpleexp | unop subexpr ) { binop subexpr }
// see: https://github.com/lua/lua/blob/2c32bff60987d38a60a58d4f0123f3783da60a63/lparser.c#L1120-L1156
// TODO: left / right priority
pub fn parse_sub_expr(input: &str, tokens: &mut TokenIter, min_priority: i32) -> Expr {
	let tk = tokens.peek();

	let mut expression = if is_un_op(tk) {
		parse_unexp(&input, tokens)
	} else {
		match parse_simple_exp(&input, tokens) {
			Some(expr) => expr,
			None => {
				tokens.prev(); // go back one token since we tried to parse simple expression before
				Expr::PrefixExp(Box::new(parse_prefix_exp(&input, tokens)))
			},
		}
	};

	while is_bin_op(tokens.peek()) && bin_priority(tokens.peek()) > min_priority {
		let tk = tokens.next();
		let op = match tk.kind {
			TokenKind::Plus => BinOp::Plus,
			TokenKind::Minus => BinOp::Minus,
			TokenKind::Mul => BinOp::Mul,
			TokenKind::Div => BinOp::Div,
			TokenKind::Pow => BinOp::Pow,
			TokenKind::Mod => BinOp::Mod,
			TokenKind::Concat => BinOp::Concat,
			TokenKind::Lt => BinOp::Lt,
			TokenKind::Lte => BinOp::Lte,
			TokenKind::Gt => BinOp::Gt,
			TokenKind::Gte => BinOp::Gte,
			TokenKind::Eq => BinOp::Eq,
			TokenKind::Neq => BinOp::Neq,
			TokenKind::And => BinOp::And,
			TokenKind::Or => BinOp::Or,
			_ => unreachable!(),
		};

		let prority = bin_priority(tk);

		let rhs = parse_sub_expr(&input, tokens, prority);

		expression = Expr::BinExp(BinExp {
			op,
			lhs: Box::new(expression),
			rhs: Box::new(rhs),
		})
	}

	expression
}

/// field ::= `[` exp `]` `=` exp | Name `=` exp | exp
pub fn parse_field(input: &str, tokens: &mut TokenIter) -> Field {
	let name_token = tokens.next();
	match name_token.kind {
		// Name '=' exp
		TokenKind::Ident => {
			tokens.assert_next(TokenKind::Assign);

			let expr = parse_expr(&input, tokens);
			Field::NameAssign(Name(name_token.span.as_string(input)), expr)
		},
		// '[' exp ']' '=' exp
		TokenKind::LBracket => {
			let lexpr = parse_expr(&input, tokens);

			tokens.assert_next(TokenKind::RBracket);
			tokens.assert_next(TokenKind::Assign);

			let rexpr = parse_expr(&input, tokens);
			Field::ExprAssign(lexpr, rexpr)
		},

		_ => Field::PosAssign(parse_expr(&input, tokens)),
	}
}

/// tableconstructor ::= `{` [fieldlist] `}`
pub fn parse_table_constructor(input: &str, tokens: &mut TokenIter) -> TableConstructor {
	match tokens.next().kind {
		TokenKind::LCurly => {
			if tokens.peek().kind == TokenKind::RCurly {
				tokens.next();
				return TableConstructor(vec![]);
			};
			let fieldlist = parse_fieldlist(&input, tokens);
			match tokens.next().kind {
				TokenKind::RCurly => TableConstructor(fieldlist),
				_ => panic!(),
			}
		},
		_ => panic!(),
	}
}

/// varlist ::= var {`,` var}
pub fn parse_varlist(input: &str, tokens: &mut TokenIter) -> Vec<Var> {
	let mut varlist = vec![];

	varlist.push(parse_var(&input, tokens));

	while tokens.peek().kind == TokenKind::Comma {
		tokens.next();
		varlist.push(parse_var(&input, tokens))
	}

	varlist
}

/// explist ::= exp {`,` exp}
pub fn parse_exprlist(input: &str, tokens: &mut TokenIter) -> Vec<Expr> {
	let mut exprs = vec![];

	exprs.push(parse_expr(&input, tokens));

	while tokens.peek().kind == TokenKind::Comma {
		tokens.next();
		exprs.push(parse_expr(&input, tokens));
	}

	exprs
}

/// args ::=  `(` [explist] `)`
pub fn parse_args(input: &str, tokens: &mut TokenIter) -> Args {
	tokens.assert_next(TokenKind::LParen);
	if tokens.peek().kind == TokenKind::RParen {
		tokens.next();
		return Args(vec![]);
	}
	let explist = parse_exprlist(&input, tokens);
	tokens.assert_next(TokenKind::RParen);
	Args(explist)
}

/// var ::=  Name | prefixexp `[` exp `]` | prefixexp `.` Name
/// e.g.:
/// foo
/// bar[0]
/// bar.bizz
pub fn parse_var(input: &str, tokens: &mut TokenIter) -> Var {
	match tokens.peek().kind {
		TokenKind::Ident => {
			let name_token = tokens.next();

			// TODO: these are completely incomprehensible, fix it
			match tokens.peek().kind {
				TokenKind::LBracket => {
					tokens.next();

					let expr = parse_expr(&input, tokens);

					tokens.assert_next(TokenKind::RBracket);

					Var::IndexExpr(IndexExpr {
						expr: Box::new(PrefixExpr::Var(Var::Name(Name(name_token.span.as_string(input))))),
						arg: expr,
					})
				},
				TokenKind::Period => {
					tokens.next();
					tokens.assert_next(TokenKind::Ident);

					Var::Property(Property {
						expr: Box::new(PrefixExpr::Var(Var::Name(Name(name_token.span.as_string(input))))),
						name: Name(tokens.cur().span.as_string(input)),
					})
				},
				_ => Var::Name(Name(tokens.cur().span.as_string(input))),
			}
		},
		_ => {
			let prefixexp = parse_prefix_exp(&input, tokens);

			match tokens.peek().kind {
				TokenKind::LBracket => {
					tokens.next();

					let expr = parse_expr(&input, tokens);

					tokens.assert_next(TokenKind::RBracket);

					Var::IndexExpr(IndexExpr {
						expr: Box::new(prefixexp),
						arg: expr,
					})
				},
				TokenKind::Period => {
					tokens.next();
					tokens.assert_next(TokenKind::Ident);

					Var::Property(Property {
						expr: Box::new(prefixexp),
						name: Name(tokens.cur().span.as_string(input)),
					})
				},
				_ => panic!(),
			}
		},
	}
}

/// prefixexp ::= var | functioncall | `(` exp `)`
// functioncall ::=  prefixexp args | prefixexp `:` Name args
// var ::=  Name | prefixexp `[` exp `]` | prefixexp `.` Name
pub fn parse_prefix_exp(input: &str, tokens: &mut TokenIter) -> PrefixExpr {
	match tokens.peek().kind {
		TokenKind::LParen => {
			tokens.next();
			let expr = PrefixExpr::Expr(parse_expr(&input, tokens));
			tokens.assert_next(TokenKind::RParen);
			expr
		},
		TokenKind::Ident => {
			tokens.next();
			match tokens.peek().kind {
				TokenKind::LBracket => {
					tokens.next();

					let expr = parse_expr(&input, tokens);

					tokens.assert_next(TokenKind::RBracket);

					PrefixExpr::Var(Var::IndexExpr(IndexExpr {
						expr: Box::new(PrefixExpr::Var(Var::Name(Name(tokens.cur().span.as_string(input))))),
						arg: expr,
					}))
				},
				TokenKind::Period => {
					tokens.next();
					tokens.assert_next(TokenKind::Ident);

					PrefixExpr::Var(Var::Property(Property {
						expr: Box::new(PrefixExpr::Var(Var::Name(Name(tokens.cur().span.as_string(input))))),
						name: Name(tokens.cur().span.as_string(input)),
					}))
				},
				TokenKind::LParen => PrefixExpr::FunctionCall(FunctionCall {
					expr: Box::new(PrefixExpr::Var(Var::Name(Name(tokens.cur().span.as_string(input))))),
					args: parse_args(&input, tokens),
				}),
				_ => PrefixExpr::Var(Var::Name(Name(tokens.cur().span.as_string(input)))),
			}
		},
		_ => panic!(),
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
pub fn parse_stat(input: &str, tokens: &mut TokenIter) -> Stat {
	let tk = tokens.peek();
	match tk.kind {
		TokenKind::SemiColon => {
			tokens.next();
			Stat::SemiColon
		},
		TokenKind::Break => {
			tokens.next();
			Stat::Break
		},
		TokenKind::Do => Stat::DoBlock(parse_do_block(&input, tokens)),
		TokenKind::While => Stat::WhileBlock(parse_while_block(&input, tokens)),
		TokenKind::If => Stat::IfBlock(Box::new(parse_if_block(&input, tokens))),
		TokenKind::For => {
			tokens.next();
			tokens.next();
			if tokens.peek().kind == TokenKind::Assign {
				tokens.prev();
				tokens.prev();
				Stat::ForRange(Box::new(parse_for_range(&input, tokens)))
			} else {
				tokens.prev();
				tokens.prev();
				Stat::ForIn(parse_for_in(&input, tokens))
			}
		},
		TokenKind::Function => Stat::FunctionDef(parse_function_def(&input, tokens)),
		TokenKind::Local => {
			tokens.next();
			if tokens.peek().kind == TokenKind::Function {
				tokens.prev();
				Stat::LocalFunctionDef(parse_local_function_def(&input, tokens))
			} else {
				tokens.prev();
				Stat::LocalAssignment(parse_local_assignment(&input, tokens))
			}
		},
		TokenKind::Ident => {
			tokens.next();
			if tokens.peek().kind == TokenKind::LParen {
				Stat::FunctionCall(FunctionCall {
					expr: Box::new(PrefixExpr::Var(Var::Name(Name(tokens.cur().span.as_string(input))))),
					args: parse_args(&input, tokens),
				})
			} else {
				tokens.prev();
				Stat::Assignment(parse_assignment(&input, tokens))
			}
		},
		other => panic!("not a valid statement: {:?}", other),
	}
}

/// varlist `=` explist
pub fn parse_assignment(input: &str, tokens: &mut TokenIter) -> Assignment {
	let varlist = parse_varlist(&input, tokens);

	tokens.assert_next(TokenKind::Assign);

	let exprlist = parse_exprlist(&input, tokens);

	Assignment { varlist, exprlist }
}

/// local namelist [`=` explist]
pub fn parse_local_assignment(input: &str, tokens: &mut TokenIter) -> LocalAssignment {
	tokens.assert_next(TokenKind::Local);

	let namelist = parse_namelist(&input, tokens);

	let exprlist = if tokens.peek().kind == TokenKind::Assign {
		tokens.next();
		Some(parse_exprlist(&input, tokens))
	} else {
		None
	};

	LocalAssignment { namelist, exprlist }
}

/// local function Name funcbody
pub fn parse_local_function_def(input: &str, tokens: &mut TokenIter) -> LocalFunctionDef {
	tokens.assert_next(TokenKind::Local);
	tokens.assert_next(TokenKind::Function);
	tokens.assert_next(TokenKind::Ident);

	let name = Name(tokens.cur().span.as_string(input));
	let body = parse_funcbody(&input, tokens);

	LocalFunctionDef { name, body }
}

/// function funcname funcbody
pub fn parse_function_def(input: &str, tokens: &mut TokenIter) -> FunctionDef {
	tokens.assert_next(TokenKind::Function);

	let name = parse_funcname(&input, tokens);
	let body = parse_funcbody(&input, tokens);

	FunctionDef { name, body }
}

/// for namelist in explist do block end
pub fn parse_for_in(input: &str, tokens: &mut TokenIter) -> ForIn {
	tokens.assert_next(TokenKind::For);

	let namelist = parse_namelist(&input, tokens);

	tokens.assert_next(TokenKind::In);

	let exprlist = parse_exprlist(&input, tokens);
	let block = parse_do_block(&input, tokens);

	ForIn {
		namelist,
		exprlist,
		block,
	}
}

/// for Name `=` exp `,` exp [`,` exp] do block end
pub fn parse_for_range(input: &str, tokens: &mut TokenIter) -> ForRange {
	tokens.assert_next(TokenKind::For);

	tokens.assert_next(TokenKind::Ident);
	let name = Name(tokens.cur().span.as_string(input));

	tokens.assert_next(TokenKind::Assign);
	let exp_start = parse_expr(&input, tokens);

	tokens.assert_next(TokenKind::Comma);

	let exp_end = parse_expr(&input, tokens);

	let exp_step = if tokens.peek().kind == TokenKind::Comma {
		tokens.next();
		Some(parse_expr(&input, tokens))
	} else {
		None
	};

	let block = parse_do_block(&input, tokens);

	ForRange {
		name,
		exprs: (exp_start, exp_end, exp_step),
		block,
	}
}

/// elseif exp then block
pub fn parse_elseif(input: &str, tokens: &mut TokenIter) -> ElseIf {
	tokens.assert_next(TokenKind::ElseIf);
	let expr = parse_expr(&input, tokens);
	tokens.assert_next(TokenKind::Then);
	let block = parse_block(&input, tokens);

	ElseIf { block, expr }
}

/// else block
pub fn parse_else_block(input: &str, tokens: &mut TokenIter) -> Option<Block> {
	if tokens.peek().kind == (TokenKind::Else) {
		tokens.next();
		Some(parse_block(&input, tokens))
	} else {
		None
	}
}

/// if exp then block {elseif exp then block} [else block] end
pub fn parse_if_block(input: &str, tokens: &mut TokenIter) -> IfBlock {
	tokens.assert_next(TokenKind::If);

	let expr = parse_expr(&input, tokens);

	tokens.assert_next(TokenKind::Then);

	let block = parse_block(&input, tokens);

	let mut elseif = vec![];

	while tokens.peek().kind == TokenKind::ElseIf {
		elseif.push(parse_elseif(&input, tokens));
	}

	let else_blk = parse_else_block(&input, tokens);

	tokens.assert_next(TokenKind::End);

	IfBlock {
		expr,
		block,
		elseif,
		else_blk,
	}
}

/// while exp do block end
pub fn parse_while_block(input: &str, tokens: &mut TokenIter) -> WhileBlock {
	tokens.assert_next(TokenKind::While);

	let expr = parse_expr(&input, tokens);
	let block = parse_do_block(&input, tokens);

	WhileBlock { block, expr }
}

/// do block end
pub fn parse_do_block(input: &str, tokens: &mut TokenIter) -> Block {
	tokens.assert_next(TokenKind::Do);
	let blk = parse_block(&input, tokens);
	tokens.assert_next(TokenKind::End);
	blk
}

fn block_follow(tk: Token) -> bool {
	match tk.kind {
		TokenKind::End | TokenKind::Else | TokenKind::ElseIf | TokenKind::Eof => true,
		_ => false,
	}
}

/// block ::= {stat} [retstat]
pub fn parse_block(input: &str, tokens: &mut TokenIter) -> Block {
	let mut stats = vec![];

	loop {
		let tk = tokens.peek();
		if block_follow(tk) {
			break;
		} else if tk.kind == TokenKind::Return {
			let retstat = parse_retstat(&input, tokens);
			return Block {
				stats,
				retstat: Some(retstat),
			};
		} else {
			stats.push(parse_stat(&input, tokens));
		}
	}
	Block { stats, retstat: None }
}

/// retstat ::= return [explist] [;]
pub fn parse_retstat(input: &str, tokens: &mut TokenIter) -> Vec<Expr> {
	tokens.assert_next(TokenKind::Return);

	if tokens.peek().kind == TokenKind::SemiColon {
		tokens.next();
	}

	if block_follow(tokens.peek()) {
		return vec![];
	}

	let exprlist = parse_exprlist(&input, tokens);

	if tokens.peek().kind == TokenKind::SemiColon {
		tokens.next();
	}

	exprlist
}

/// funcbody ::= `(` [parlist] `)` block end
pub fn parse_funcbody(input: &str, tokens: &mut TokenIter) -> FuncBody {
	tokens.assert_next(TokenKind::LParen);
	let params = parse_parlist(&input, tokens);
	tokens.assert_next(TokenKind::RParen);
	let body = parse_block(&input, tokens);
	tokens.assert_next(TokenKind::End);

	FuncBody { params, body }
}

/// functiondef ::= function funcbody
/// stat ::= function funcname funcbody
/// stat ::= local function Name funcbody
pub fn parse_functiondef(input: &str, tokens: &mut TokenIter) -> FunctionDef {
	tokens.assert_next(TokenKind::Function);

	let name = parse_funcname(&input, tokens);
	let body = parse_funcbody(&input, tokens);

	FunctionDef { name, body }
}

/// namelist ::= Name {`,` Name}
pub fn parse_namelist(input: &str, tokens: &mut TokenIter) -> Vec<Name> {
	tokens.assert_next(TokenKind::Ident);

	let first_name = tokens.cur().span.as_string(input);

	let mut names = vec![Name(first_name)];

	while tokens.peek().kind == TokenKind::Comma {
		tokens.next();
		tokens.assert_next(TokenKind::Ident);
		names.push(Name(tokens.cur().span.as_string(input)));
	}

	names
}

/// parlist ::= namelist [`,`]
pub fn parse_parlist(input: &str, tokens: &mut TokenIter) -> Params {
	match tokens.peek().kind {
		// namelist
		TokenKind::Ident => {
			let names = parse_namelist(&input, tokens);

			// [',']
			match tokens.peek().kind {
				TokenKind::Comma => {
					tokens.next();
				},
				_ => (),
			};
			Params { names }
		},
		_ => Params { names: vec![] },
	}
}

/// fieldlist ::= field {fieldsep field} [fieldsep]
/// fieldsep ::= `,` | `;`
pub fn parse_fieldlist(input: &str, tokens: &mut TokenIter) -> Vec<Field> {
	let mut fields = vec![];

	loop {
		let f = parse_field(&input, tokens);
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

// TODO: multi line string currently broken
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
