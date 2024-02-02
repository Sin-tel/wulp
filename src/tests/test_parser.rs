use crate::ast::*;
use crate::lexer::*;
use crate::parser::*;
use crate::token_iter::*;

#[test]
fn test_parse_expr() {
	let p = r#"nil"#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = TokenIter::new(tokens);
	assert_eq!(parse_expr(p, &mut tokens), (Expr::Nil));

	let p = r#"false"#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = TokenIter::new(tokens);
	assert_eq!(parse_expr(p, &mut tokens), (Expr::Bool(false)));

	let p = r#"true"#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = TokenIter::new(tokens);
	assert_eq!(parse_expr(p, &mut tokens), (Expr::Bool(true)));

	let p = r#"10"#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = TokenIter::new(tokens);
	assert_eq!(parse_expr(p, &mut tokens), (Expr::Num(10f64)));
}

#[test]
fn test_parse_unexp() {
	for (s, op) in vec![("-", Unop::Minus), ("not ", Unop::Not), ("#", Unop::Len)] {
		let p = format!("{}foo", s);
		let tokens: Vec<_> = Lexer::new(&p).collect();
		let mut tokens = TokenIter::new(tokens);
		assert_eq!(
			parse_expr(&p, &mut tokens),
			(Expr::UnExp(UnExp {
				op,
				exp: Box::new(Expr::PrefixExp(Box::new(PrefixExpr::Var(Var::Name(Name(
					String::from("foo")
				))))))
			}))
		);
	}
}

#[test]
fn test_parse_binexp() {
	for (s, op) in vec![
		("+", BinOp::Plus),
		("-", BinOp::Minus),
		("*", BinOp::Mul),
		("/", BinOp::Div),
		("^", BinOp::Pow),
		("%", BinOp::Mod),
		("..", BinOp::Concat),
		("<", BinOp::Lt),
		("<=", BinOp::Lte),
		(">", BinOp::Gt),
		(">=", BinOp::Gte),
		("==", BinOp::Eq),
		("~=", BinOp::Neq),
		("and", BinOp::And),
		("or", BinOp::Or),
	] {
		let p = format!("foo {} bar", s);
		let tokens: Vec<_> = Lexer::new(&p).collect();
		let mut tokens = TokenIter::new(tokens);
		assert_eq!(
			parse_expr(&p, &mut tokens),
			(Expr::BinExp(BinExp {
				op,
				lhs: Box::new(Expr::PrefixExp(Box::new(PrefixExpr::Var(Var::Name(Name(
					String::from("foo")
				)))))),
				rhs: Box::new(Expr::PrefixExp(Box::new(PrefixExpr::Var(Var::Name(Name(
					String::from("bar")
				))))))
			}))
		);
	}
}

#[test]
fn test_multi_part_binexpr() {
	let p = "1 + 2 * 3";
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = TokenIter::new(tokens);
	assert_eq!(
		parse_expr(p, &mut tokens),
		(Expr::BinExp(BinExp {
			op: BinOp::Plus,
			lhs: Box::new(Expr::Num(1.0)),
			rhs: Box::new(Expr::BinExp(BinExp {
				op: BinOp::Mul,
				lhs: Box::new(Expr::Num(2.0)),
				rhs: Box::new(Expr::Num(3.0)),
			})),
		}))
	);

	let p = "1 * 2 + 3";
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = TokenIter::new(tokens);
	assert_eq!(
		parse_expr(p, &mut tokens),
		(Expr::BinExp(BinExp {
			op: BinOp::Plus,
			lhs: Box::new(Expr::BinExp(BinExp {
				op: BinOp::Mul,
				lhs: Box::new(Expr::Num(1.0)),
				rhs: Box::new(Expr::Num(2.0)),
			})),
			rhs: Box::new(Expr::Num(3.0)),
		}))
	);

	let p = "1 + 2 + 3";
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = TokenIter::new(tokens);
	assert_eq!(
		parse_expr(p, &mut tokens),
		(Expr::BinExp(BinExp {
			op: BinOp::Plus,
			lhs: Box::new(Expr::BinExp(BinExp {
				op: BinOp::Plus,
				lhs: Box::new(Expr::Num(1.0)),
				rhs: Box::new(Expr::Num(2.0)),
			})),
			rhs: Box::new(Expr::Num(3.0)),
		}))
	);
}

#[test]
fn test_simple_bin() {
	let p = "foo - bar";
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = TokenIter::new(tokens);
	assert_eq!(
		parse_expr(p, &mut tokens),
		(Expr::BinExp(BinExp {
			op: BinOp::Minus,
			lhs: Box::new(Expr::PrefixExp(Box::new(PrefixExpr::Var(Var::Name(Name(
				String::from("foo")
			)))))),
			rhs: Box::new(Expr::PrefixExp(Box::new(PrefixExpr::Var(Var::Name(Name(
				String::from("bar")
			))))))
		}))
	);
}

#[test]
fn test_parse_prefix_exp_parens() {
	let p = r#"foo"#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = TokenIter::new(tokens);

	assert_eq!(
		parse_expr(p, &mut tokens),
		(Expr::PrefixExp(Box::new(PrefixExpr::Var(Var::Name(Name(String::from("foo")))))))
	);

	let p = r#"(foo)"#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = TokenIter::new(tokens);

	assert_eq!(
		parse_prefix_exp(p, &mut tokens),
		(PrefixExpr::Expr(Expr::PrefixExp(Box::new(PrefixExpr::Var(Var::Name(Name(
			String::from("foo")
		)))))))
	);
}

#[test]
fn test_parse_var() {
	let p = r#"foo"#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = TokenIter::new(tokens);

	assert_eq!(parse_var(p, &mut tokens), (Var::Name(Name(String::from("foo")))));

	let p = r#"(foo)[1]"#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = TokenIter::new(tokens);

	assert_eq!(
		parse_var(p, &mut tokens),
		(Var::IndexExpr(IndexExpr {
			expr: Box::new(PrefixExpr::Expr(Expr::PrefixExp(Box::new(PrefixExpr::Var(Var::Name(
				Name(String::from("foo"))
			)),)))),
			arg: Expr::Num(1f64),
		}))
	);
	let p = r#"foo[1]"#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = TokenIter::new(tokens);

	assert_eq!(
		parse_var(p, &mut tokens),
		(Var::IndexExpr(IndexExpr {
			expr: Box::new(PrefixExpr::Var(Var::Name(Name(String::from("foo")))),),
			arg: Expr::Num(1f64),
		}))
	);

	let p = r#"foo.bar"#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = TokenIter::new(tokens);

	assert_eq!(
		parse_var(p, &mut tokens),
		(Var::Property(Property {
			expr: Box::new(PrefixExpr::Var(Var::Name(Name(String::from("foo"))))),
			name: Name(String::from("bar"))
		}))
	);
}

#[test]
fn test_parse_args() {
	let p = r#"()"#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = TokenIter::new(tokens);

	assert_eq!(parse_args(p, &mut tokens), (Args(vec![])));

	let p = r#"(foo, nil, false, 10)"#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = TokenIter::new(tokens);

	assert_eq!(
		parse_args(p, &mut tokens),
		(Args(vec![
			Expr::PrefixExp(Box::new(PrefixExpr::Var(Var::Name(Name(String::from("foo")))))),
			Expr::Nil,
			Expr::Bool(false),
			Expr::Num(10f64),
		]))
	);
}

#[test]
fn test_parse_exprlist() {
	let p = r#"nil, false, true, "str""#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = TokenIter::new(tokens);
	assert_eq!(
		parse_exprlist(p, &mut tokens),
		(vec![
			Expr::Nil,
			Expr::Bool(false),
			Expr::Bool(true),
			Expr::Str(String::from("str"))
		])
	);
}

#[test]
#[should_panic]
fn test_parse_exprlist_fail() {
	let p = r#"nil, false,"#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = TokenIter::new(tokens);
	assert_eq!(parse_exprlist(p, &mut tokens), (vec![Expr::Nil, Expr::Bool(false)]));

	assert_eq!(tokens.next().kind, TokenKind::Comma);
}

#[test]
fn test_parse_varlist() {
	let p = r#"foo, bar, bizz"#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = TokenIter::new(tokens);
	assert_eq!(
		parse_varlist(p, &mut tokens),
		(vec![
			Var::Name(Name(String::from("foo"))),
			Var::Name(Name(String::from("bar"))),
			Var::Name(Name(String::from("bizz"))),
		])
	);
}

#[test]
fn test_parse_expr_func() {
	let p = r#"function foo() return ; end"#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = TokenIter::new(tokens);

	assert_eq!(
		parse_expr(p, &mut tokens),
		(Expr::FuncDef(FunctionDef {
			name: FuncName {
				path: vec![Name(String::from("foo"))],
				method: None,
			},
			body: FuncBody {
				params: Params { names: vec![] },
				body: Block {
					stats: vec![],
					retstat: Some(vec![]),
				}
			}
		}))
	);
}

#[test]
fn test_parse_retstat() {
	let p = r#"return nil, false;"#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = TokenIter::new(tokens);

	assert_eq!(parse_retstat(p, &mut tokens), (vec![Expr::Nil, Expr::Bool(false),]));

	let p = r#"return 10, "foo", true, bar"#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = TokenIter::new(tokens);

	assert_eq!(
		parse_retstat(p, &mut tokens),
		(vec![
			Expr::Num(10f64),
			Expr::Str(String::from("foo")),
			Expr::Bool(true),
			Expr::PrefixExp(Box::new(PrefixExpr::Var(Var::Name(Name(String::from("bar"))))))
		])
	);

	let p = r#"return"#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = TokenIter::new(tokens);

	assert_eq!(parse_retstat(p, &mut tokens), (vec![]));

	let p = r#"return;"#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = TokenIter::new(tokens);

	assert_eq!(parse_retstat(p, &mut tokens), (vec![]));
}

#[test]
fn test_parse_block() {
	let p = r#"return 10, "foo", true, bar"#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = TokenIter::new(tokens);

	assert_eq!(
		parse_block(p, &mut tokens),
		(Block {
			stats: vec![],
			retstat: Some(vec![
				Expr::Num(10f64),
				Expr::Str(String::from("foo")),
				Expr::Bool(true),
				Expr::PrefixExp(Box::new(PrefixExpr::Var(Var::Name(Name(String::from("bar"))))))
			])
		})
	);

	let p = r#"print(foo)"#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = TokenIter::new(tokens);

	assert_eq!(
		parse_block(p, &mut tokens),
		(Block {
			stats: vec![Stat::FunctionCall(FunctionCall {
				expr: Box::new(PrefixExpr::Var(Var::Name(Name(String::from("print"))))),
				args: Args(vec![Expr::PrefixExp(Box::new(PrefixExpr::Var(Var::Name(Name(
					String::from("foo")
				)))))])
			})],
			retstat: None
		})
	);
}

#[test]
fn test_parse_functiondef() {
	let p = r#"function foo() return end"#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = TokenIter::new(tokens);

	assert_eq!(
		parse_functiondef(p, &mut tokens),
		(FunctionDef {
			name: FuncName {
				path: vec![Name(String::from("foo"))],
				method: None,
			},
			body: FuncBody {
				params: Params { names: vec![] },
				body: Block {
					stats: vec![],
					retstat: Some(vec![]),
				}
			}
		})
	);
}

#[test]
fn test_parse_table_constructor() {
	let p = r#"{foo = 'foo', bar = false, bizz = 1}"#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = TokenIter::new(tokens);

	assert_eq!(
		parse_table_constructor(p, &mut tokens),
		(TableConstructor(vec![
			Field::NameAssign(Name(String::from("foo")), Expr::Str(String::from("foo"))),
			Field::NameAssign(Name(String::from("bar")), Expr::Bool(false)),
			Field::NameAssign(Name(String::from("bizz")), Expr::Num(1f64)),
		]))
	);

	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = TokenIter::new(tokens);

	assert_eq!(
		parse_expr(p, &mut tokens),
		(Expr::Table(TableConstructor(vec![
			Field::NameAssign(Name(String::from("foo")), Expr::Str(String::from("foo"))),
			Field::NameAssign(Name(String::from("bar")), Expr::Bool(false)),
			Field::NameAssign(Name(String::from("bizz")), Expr::Num(1f64)),
		])))
	);

	let p = r#"{}"#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = TokenIter::new(tokens);

	assert_eq!(parse_table_constructor(p, &mut tokens), (TableConstructor(vec![])));
}

#[test]
fn test_parse_prefix_exp() {
	let p = "false)";
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = TokenIter::new(tokens);
	assert_eq!(parse_expr(p, &mut tokens), (Expr::Bool(false)),);
	assert_eq!(tokens.next().kind, TokenKind::RParen);

	let p = r#"(false)"#;
	let tokens: Vec<_> = Lexer::new(p).collect();

	let mut tokens = TokenIter::new(tokens);

	assert_eq!(parse_prefix_exp(p, &mut tokens), (PrefixExpr::Expr(Expr::Bool(false))));

	let p = r#"foo(false, true, nil)"#;
	let tokens: Vec<_> = Lexer::new(p).collect();

	let mut tokens = TokenIter::new(tokens);

	assert_eq!(
		parse_prefix_exp(p, &mut tokens),
		(PrefixExpr::FunctionCall(FunctionCall {
			expr: Box::new(PrefixExpr::Var(Var::Name(Name(String::from("foo"))))),
			args: Args(vec![Expr::Bool(false), Expr::Bool(true), Expr::Nil])
		}))
	)
}

#[test]
fn test_parse_parlist() {
	let p = r#"Name,Another_Name"#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = TokenIter::new(tokens);

	assert_eq!(
		parse_parlist(p, &mut tokens),
		(Params {
			names: vec![Name(String::from("Name")), Name(String::from("Another_Name"))],
		})
	);
}

#[test]
fn test_parse_namelist() {
	let p = r#"Name"#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = TokenIter::new(tokens);

	assert_eq!(parse_namelist(p, &mut tokens), (vec![Name(String::from("Name"))]));

	let p = r#"Name,Another_Name"#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = TokenIter::new(tokens);

	assert_eq!(
		parse_namelist(p, &mut tokens),
		(vec![Name(String::from("Name")), Name(String::from("Another_Name"))])
	);
}

#[test]
fn test_parse_stat() {
	let p = r#";"#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = TokenIter::new(tokens);
	assert_eq!(parse_stat(p, &mut tokens), (Stat::SemiColon));

	let p = r#"break"#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = TokenIter::new(tokens);
	assert_eq!(parse_stat(p, &mut tokens), (Stat::Break));

	let p = r#"do end"#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = TokenIter::new(tokens);
	assert_eq!(
		parse_stat(p, &mut tokens),
		(Stat::DoBlock(Block {
			stats: vec![],
			retstat: None
		}))
	);

	let p = r#"while true do end"#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = TokenIter::new(tokens);
	assert_eq!(
		parse_stat(p, &mut tokens),
		(Stat::WhileBlock(WhileBlock {
			expr: Expr::Bool(true),
			block: Block {
				stats: vec![],
				retstat: None
			}
		}))
	);

	let p = r#"if foo then elseif bar then else end"#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = TokenIter::new(tokens);
	assert_eq!(
		parse_stat(p, &mut tokens),
		(Stat::IfBlock(Box::new(IfBlock {
			expr: Expr::PrefixExp(Box::new(PrefixExpr::Var(Var::Name(Name(String::from("foo")))))),
			block: Block {
				stats: vec![],
				retstat: None
			},
			elseif: vec![ElseIf {
				expr: Expr::PrefixExp(Box::new(PrefixExpr::Var(Var::Name(Name(String::from("bar")))))),
				block: Block {
					stats: vec![],
					retstat: None,
				}
			}],
			else_blk: Some(Block {
				stats: vec![],
				retstat: None,
			})
		})))
	);

	let p = r#"for foo = 1, 2, bar do end"#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = TokenIter::new(tokens);
	assert_eq!(
		parse_stat(p, &mut tokens),
		(Stat::ForRange(Box::new(ForRange {
			name: Name(String::from("foo")),
			exprs: (
				Expr::Num(1f64),
				Expr::Num(2f64),
				Some(Expr::PrefixExp(Box::new(PrefixExpr::Var(Var::Name(Name(
					String::from("bar")
				))))))
			),
			block: Block {
				stats: vec![],
				retstat: None
			},
		})))
	);

	let p = r#"for foo, bar in true, nil do end"#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = TokenIter::new(tokens);
	assert_eq!(
		parse_stat(p, &mut tokens),
		(Stat::ForIn(ForIn {
			namelist: vec![Name(String::from("foo")), Name(String::from("bar"))],
			exprlist: vec![Expr::Bool(true), Expr::Nil],
			block: Block {
				stats: vec![],
				retstat: None
			},
		}))
	);

	let p = r#"function foo(a) return end"#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = TokenIter::new(tokens);
	assert_eq!(
		parse_stat(p, &mut tokens),
		(Stat::FunctionDef(FunctionDef {
			name: FuncName {
				path: vec![Name(String::from("foo"))],
				method: None,
			},
			body: FuncBody {
				params: Params {
					names: vec![Name(String::from("a"))],
				},
				body: Block {
					stats: vec![],
					retstat: Some(vec![]),
				}
			}
		}))
	);

	let p = r#"local function bar() return end"#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = TokenIter::new(tokens);
	assert_eq!(
		parse_stat(p, &mut tokens),
		(Stat::LocalFunctionDef(LocalFunctionDef {
			name: Name(String::from("bar")),
			body: FuncBody {
				params: Params { names: vec![] },
				body: Block {
					stats: vec![],
					retstat: Some(vec![]),
				}
			}
		}))
	);

	let p = r#"local foo, bar, buzz = nil, 10, "word""#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = TokenIter::new(tokens);
	assert_eq!(
		parse_stat(p, &mut tokens),
		(Stat::LocalAssignment(LocalAssignment {
			namelist: vec![
				Name(String::from("foo")),
				Name(String::from("bar")),
				Name(String::from("buzz"))
			],
			exprlist: Some(vec![Expr::Nil, Expr::Num(10f64), Expr::Str(String::from("word"))]),
		}))
	);

	let p = r#"foo, bar = true, nil"#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = TokenIter::new(tokens);
	assert_eq!(
		parse_stat(p, &mut tokens),
		(Stat::Assignment(Assignment {
			varlist: vec![
				Var::Name(Name(String::from("foo"))),
				Var::Name(Name(String::from("bar")))
			],
			exprlist: vec![Expr::Bool(true), Expr::Nil],
		}))
	);

	let p = r#"foo("bar")"#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = TokenIter::new(tokens);
	assert_eq!(
		parse_stat(p, &mut tokens),
		(Stat::FunctionCall(FunctionCall {
			expr: Box::new(PrefixExpr::Var(Var::Name(Name(String::from("foo"))))),
			args: Args(vec![Expr::Str(String::from("bar"))])
		}))
	);
}

#[test]
fn test_parse_funcbody() {
	let p = r#"() return end"#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = TokenIter::new(tokens);

	assert_eq!(
		parse_funcbody(p, &mut tokens),
		(FuncBody {
			params: Params { names: vec![] },
			body: Block {
				stats: vec![],
				retstat: Some(vec![]),
			}
		})
	);
}

#[test]
fn test_parse_funcname() {
	let p = r#"abc"#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = TokenIter::new(tokens);

	assert_eq!(
		parse_funcname(p, &mut tokens),
		(FuncName {
			path: vec![Name("abc".to_string())],
			method: None,
		})
	);

	let p = r#"abc.bar1"#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = TokenIter::new(tokens);

	assert_eq!(
		parse_funcname(p, &mut tokens),
		(FuncName {
			path: vec![Name("abc".to_string()), Name("bar1".to_string())],
			method: None,
		})
	);

	let p = r#"abc.bar1:mno"#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = TokenIter::new(tokens);

	assert_eq!(
		parse_funcname(p, &mut tokens),
		(FuncName {
			path: vec![Name("abc".to_string()), Name("bar1".to_string())],
			method: Some(Name("mno".to_string())),
		})
	);
}
