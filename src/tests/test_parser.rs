use crate::ast::*;
use crate::lexer::*;
use crate::parser::*;
use crate::token::*;

#[test]
fn expr() {
	let p = r#"nil"#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = Tokens::new(tokens);
	assert_eq!(parse_expr(p, &mut tokens), (Expr::Nil));

	let p = r#"false"#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = Tokens::new(tokens);
	assert_eq!(parse_expr(p, &mut tokens), (Expr::Bool(false)));

	let p = r#"true"#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = Tokens::new(tokens);
	assert_eq!(parse_expr(p, &mut tokens), (Expr::Bool(true)));

	let p = r#"10"#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = Tokens::new(tokens);
	assert_eq!(parse_expr(p, &mut tokens), (Expr::Num(10f64)));
}

#[test]
fn unexp() {
	for (s, op) in vec![("-", UnOp::Minus), ("not ", UnOp::Not), ("#", UnOp::Len)] {
		let p = format!("{}foo", s);
		let tokens: Vec<_> = Lexer::new(&p).collect();
		let mut tokens = Tokens::new(tokens);
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
fn binexp() {
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
		let mut tokens = Tokens::new(tokens);
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
	let mut tokens = Tokens::new(tokens);
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
	let mut tokens = Tokens::new(tokens);
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
	let mut tokens = Tokens::new(tokens);
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
	let mut tokens = Tokens::new(tokens);
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
fn prefix_exp_parens() {
	let p = r#"foo"#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = Tokens::new(tokens);

	assert_eq!(
		parse_expr(p, &mut tokens),
		(Expr::PrefixExp(Box::new(PrefixExpr::Var(Var::Name(Name(String::from("foo")))))))
	);

	let p = r#"(foo)"#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = Tokens::new(tokens);

	assert_eq!(
		parse_prefix_exp(p, &mut tokens),
		(PrefixExpr::Expr(Expr::PrefixExp(Box::new(PrefixExpr::Var(Var::Name(Name(
			String::from("foo")
		)))))))
	);
}

#[test]
fn var() {
	let p = r#"foo"#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = Tokens::new(tokens);

	assert_eq!(parse_var(p, &mut tokens), (Var::Name(Name(String::from("foo")))));

	let p = r#"(foo)[1]"#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = Tokens::new(tokens);

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
	let mut tokens = Tokens::new(tokens);

	assert_eq!(
		parse_var(p, &mut tokens),
		(Var::IndexExpr(IndexExpr {
			expr: Box::new(PrefixExpr::Var(Var::Name(Name(String::from("foo")))),),
			arg: Expr::Num(1f64),
		}))
	);

	let p = r#"foo.bar"#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = Tokens::new(tokens);

	assert_eq!(
		parse_var(p, &mut tokens),
		(Var::Property(Property {
			expr: Box::new(PrefixExpr::Var(Var::Name(Name(String::from("foo"))))),
			name: Name(String::from("bar"))
		}))
	);
}

#[test]
fn args() {
	let p = r#"()"#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = Tokens::new(tokens);

	assert_eq!(parse_args(p, &mut tokens), vec![]);

	let p = r#"(foo, nil, false, 10)"#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = Tokens::new(tokens);

	assert_eq!(
		parse_args(p, &mut tokens),
		vec![
			Expr::PrefixExp(Box::new(PrefixExpr::Var(Var::Name(Name(String::from("foo")))))),
			Expr::Nil,
			Expr::Bool(false),
			Expr::Num(10f64),
		]
	);
}

#[test]
fn exprlist() {
	let p = r#"nil, false, true, "str""#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = Tokens::new(tokens);
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
fn varlist() {
	let p = r#"foo, bar, bizz"#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = Tokens::new(tokens);
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
fn expr_func() {
	let p = r#"function foo() return ; end"#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = Tokens::new(tokens);

	assert_eq!(
		parse_expr(p, &mut tokens),
		(Expr::FuncDef(FunctionDef {
			name: FuncName {
				path: vec![Name(String::from("foo"))],
				method: None,
			},
			body: FuncBody {
				params: vec![],
				body: Block {
					stats: vec![Stat::Return(vec![])]
				}
			}
		}))
	);
}

#[test]
fn test_return() {
	let p = r#"return nil, false;"#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = Tokens::new(tokens);

	assert_eq!(parse_return(p, &mut tokens), (vec![Expr::Nil, Expr::Bool(false),]));

	let p = r#"return 10, "foo", true, bar"#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = Tokens::new(tokens);

	assert_eq!(
		parse_return(p, &mut tokens),
		(vec![
			Expr::Num(10f64),
			Expr::Str(String::from("foo")),
			Expr::Bool(true),
			Expr::PrefixExp(Box::new(PrefixExpr::Var(Var::Name(Name(String::from("bar"))))))
		])
	);

	let p = r#"return"#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = Tokens::new(tokens);

	assert_eq!(parse_return(p, &mut tokens), (vec![]));

	let p = r#"return;"#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = Tokens::new(tokens);

	assert_eq!(parse_return(p, &mut tokens), (vec![]));
}

#[test]
fn block() {
	let p = r#"return 10, "foo", true, bar"#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = Tokens::new(tokens);

	assert_eq!(
		parse_block(p, &mut tokens),
		(Block {
			stats: vec![Stat::Return(vec![
				Expr::Num(10f64),
				Expr::Str(String::from("foo")),
				Expr::Bool(true),
				Expr::PrefixExp(Box::new(PrefixExpr::Var(Var::Name(Name(String::from("bar"))))))
			])]
		})
	);

	let p = r#"print(foo)"#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = Tokens::new(tokens);

	assert_eq!(
		parse_block(p, &mut tokens),
		(Block {
			stats: vec![Stat::FunctionCall(FunctionCall {
				expr: Box::new(PrefixExpr::Var(Var::Name(Name(String::from("print"))))),
				args: vec![Expr::PrefixExp(Box::new(PrefixExpr::Var(Var::Name(Name(
					String::from("foo")
				)))))]
			})],
		})
	);
}

#[test]
fn functiondef() {
	let p = r#"function foo() return end"#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = Tokens::new(tokens);

	assert_eq!(
		parse_functiondef(p, &mut tokens),
		(FunctionDef {
			name: FuncName {
				path: vec![Name(String::from("foo"))],
				method: None,
			},
			body: FuncBody {
				params: vec![],
				body: Block {
					stats: vec![Stat::Return(vec![])]
				}
			}
		})
	);
}

#[test]
fn table_constructor() {
	let p = r#"{foo = 'foo', bar = false, bizz = 1}"#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = Tokens::new(tokens);

	assert_eq!(
		parse_table_constructor(p, &mut tokens),
		vec![
			Field::NameAssign(Name(String::from("foo")), Expr::Str(String::from("foo"))),
			Field::NameAssign(Name(String::from("bar")), Expr::Bool(false)),
			Field::NameAssign(Name(String::from("bizz")), Expr::Num(1f64)),
		]
	);

	let p = r#"{[bar] = false, [2] = 1}"#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = Tokens::new(tokens);

	assert_eq!(
		parse_expr(p, &mut tokens),
		(Expr::Table(vec![
			Field::ExprAssign(
				Expr::PrefixExp(Box::new(PrefixExpr::Var(Var::Name(Name(String::from("bar")))))),
				Expr::Bool(false)
			),
			Field::ExprAssign(Expr::Num(2f64), Expr::Num(1f64)),
		]))
	);

	let p = r#"{}"#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = Tokens::new(tokens);

	assert_eq!(parse_table_constructor(p, &mut tokens), vec![]);
}

#[test]
fn prefix_exp() {
	// TODO: ???
	let p = "false)";
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = Tokens::new(tokens);
	assert_eq!(parse_expr(p, &mut tokens), (Expr::Bool(false)),);
	assert_eq!(tokens.next().kind, TokenKind::RParen);

	let p = r#"(false)"#;
	let tokens: Vec<_> = Lexer::new(p).collect();

	let mut tokens = Tokens::new(tokens);

	assert_eq!(parse_prefix_exp(p, &mut tokens), (PrefixExpr::Expr(Expr::Bool(false))));

	let p = r#"foo(false, true, nil)"#;
	let tokens: Vec<_> = Lexer::new(p).collect();

	let mut tokens = Tokens::new(tokens);

	assert_eq!(
		parse_prefix_exp(p, &mut tokens),
		(PrefixExpr::FunctionCall(FunctionCall {
			expr: Box::new(PrefixExpr::Var(Var::Name(Name(String::from("foo"))))),
			args: vec![Expr::Bool(false), Expr::Bool(true), Expr::Nil]
		}))
	)
}

#[test]
fn parlist() {
	let p = r#"Name,Another_Name"#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = Tokens::new(tokens);

	assert_eq!(
		parse_parlist(p, &mut tokens),
		vec![Name(String::from("Name")), Name(String::from("Another_Name"))]
	);

	let p = r#"a, b, "#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = Tokens::new(tokens);

	assert_eq!(
		parse_parlist(p, &mut tokens),
		vec![Name(String::from("a")), Name(String::from("b"))],
	);
}

#[test]
fn namelist() {
	let p = r#"Name"#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = Tokens::new(tokens);

	assert_eq!(parse_namelist(p, &mut tokens), (vec![Name(String::from("Name"))]));

	let p = r#"Name,Another_Name"#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = Tokens::new(tokens);

	assert_eq!(
		parse_namelist(p, &mut tokens),
		(vec![Name(String::from("Name")), Name(String::from("Another_Name"))])
	);
}

#[test]
fn stat() {
	// let p = r#";"#;
	// let tokens: Vec<_> = Lexer::new(p).collect();
	// let mut tokens = Tokens::new(tokens);
	// assert_eq!(parse_stat(p, &mut tokens), (Stat::Break));

	let p = r#"break"#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = Tokens::new(tokens);
	assert_eq!(parse_stat(p, &mut tokens), (Stat::Break));

	let p = r#"do end"#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = Tokens::new(tokens);
	assert_eq!(parse_stat(p, &mut tokens), (Stat::DoBlock(Block { stats: vec![] })));

	let p = r#"while true do end"#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = Tokens::new(tokens);
	assert_eq!(
		parse_stat(p, &mut tokens),
		(Stat::WhileBlock(WhileBlock {
			expr: Expr::Bool(true),
			block: Block { stats: vec![] }
		}))
	);

	let p = r#"if foo then elseif bar then else end"#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = Tokens::new(tokens);
	assert_eq!(
		parse_stat(p, &mut tokens),
		(Stat::IfBlock(IfBlock {
			expr: Expr::PrefixExp(Box::new(PrefixExpr::Var(Var::Name(Name(String::from("foo")))))),
			block: Block { stats: vec![] },
			elseif: vec![ElseIf {
				expr: Expr::PrefixExp(Box::new(PrefixExpr::Var(Var::Name(Name(String::from("bar")))))),
				block: Block { stats: vec![] }
			}],
			else_block: Some(Block { stats: vec![] })
		}))
	);

	let p = r#"for foo = 1, 2, bar do end"#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = Tokens::new(tokens);
	assert_eq!(
		parse_stat(p, &mut tokens),
		(Stat::ForRange(ForRange {
			name: Name(String::from("foo")),
			exprs: (
				Expr::Num(1f64),
				Expr::Num(2f64),
				Some(Expr::PrefixExp(Box::new(PrefixExpr::Var(Var::Name(Name(
					String::from("bar")
				))))))
			),
			block: Block { stats: vec![] },
		}))
	);

	let p = r#"for foo, bar in true, nil do end"#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = Tokens::new(tokens);
	assert_eq!(
		parse_stat(p, &mut tokens),
		(Stat::ForIn(ForIn {
			namelist: vec![Name(String::from("foo")), Name(String::from("bar"))],
			exprlist: vec![Expr::Bool(true), Expr::Nil],
			block: Block { stats: vec![] },
		}))
	);

	let p = r#"function foo(a) return; end"#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = Tokens::new(tokens);
	assert_eq!(
		parse_stat(p, &mut tokens),
		(Stat::FunctionDef(FunctionDef {
			name: FuncName {
				path: vec![Name(String::from("foo"))],
				method: None,
			},
			body: FuncBody {
				params: vec![Name(String::from("a"))],

				body: Block {
					stats: vec![Stat::Return(vec![])]
				}
			}
		}))
	);

	let p = r#"local function bar() return end"#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = Tokens::new(tokens);
	assert_eq!(
		parse_stat(p, &mut tokens),
		(Stat::LocalFunctionDef(LocalFunctionDef {
			name: Name(String::from("bar")),
			body: FuncBody {
				params: vec![],
				body: Block {
					stats: vec![Stat::Return(vec![])]
				}
			}
		}))
	);

	let p = r#"local foo, bar, buzz = nil, 10, "word""#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = Tokens::new(tokens);
	assert_eq!(
		parse_stat(p, &mut tokens),
		(Stat::LocalAssignment(LocalAssignment {
			namelist: vec![
				Name(String::from("foo")),
				Name(String::from("bar")),
				Name(String::from("buzz"))
			],
			exprlist: vec![Expr::Nil, Expr::Num(10f64), Expr::Str(String::from("word"))],
		}))
	);

	let p = r#"foo, bar = true, nil"#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = Tokens::new(tokens);
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
	let mut tokens = Tokens::new(tokens);
	assert_eq!(
		parse_stat(p, &mut tokens),
		(Stat::FunctionCall(FunctionCall {
			expr: Box::new(PrefixExpr::Var(Var::Name(Name(String::from("foo"))))),
			args: vec![Expr::Str(String::from("bar"))]
		}))
	);
}

#[test]
fn funcbody() {
	let p = r#"() return end"#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = Tokens::new(tokens);

	assert_eq!(
		parse_funcbody(p, &mut tokens),
		(FuncBody {
			params: vec![],
			body: Block {
				stats: vec![Stat::Return(vec![])]
			}
		})
	);
}

#[test]
fn funcname() {
	let p = r#"abc"#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = Tokens::new(tokens);

	assert_eq!(
		parse_funcname(p, &mut tokens),
		(FuncName {
			path: vec![Name("abc".to_string())],
			method: None,
		})
	);

	let p = r#"abc.bar1"#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = Tokens::new(tokens);

	assert_eq!(
		parse_funcname(p, &mut tokens),
		(FuncName {
			path: vec![Name("abc".to_string()), Name("bar1".to_string())],
			method: None,
		})
	);

	let p = r#"abc.bar1:mno"#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = Tokens::new(tokens);

	assert_eq!(
		parse_funcname(p, &mut tokens),
		(FuncName {
			path: vec![Name("abc".to_string()), Name("bar1".to_string())],
			method: Some(Name("mno".to_string())),
		})
	);
}

#[test]
#[should_panic(expected = "Expected expression but found: end of file.")]
fn exprlist_fail() {
	let p = r#"nil, false,"#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = Tokens::new(tokens);
	parse_exprlist(p, &mut tokens);
}

#[test]
#[should_panic(expected = "Malformed number: `100.00.00`.")]
fn number_fail() {
	let p = r#"a = 100.00.00"#;
	parse(p);
}

#[test]
#[should_panic(expected = "Expected `do` but found `while`.")]
fn expect_token_fail() {
	let p = r#"for x in y while"#;
	parse(p);
}
