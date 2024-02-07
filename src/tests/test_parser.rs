use crate::ast::*;
use crate::lexer::*;
use crate::parser::*;
use crate::token::*;

fn make_tokens(input: &str) -> Tokens {
	let tokens: Vec<_> = Lexer::new(input).collect();
	Tokens::new(tokens)
}

#[test]
fn expr() {
	let p = r#"nil"#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = Tokens::new(tokens);
	assert_eq!(parse_expr(p, &mut tokens), Expr::Nil);

	let p = r#"false"#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = Tokens::new(tokens);
	assert_eq!(parse_expr(p, &mut tokens), Expr::Bool(false));

	let p = r#"true"#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = Tokens::new(tokens);
	assert_eq!(parse_expr(p, &mut tokens), Expr::Bool(true));

	let p = r#"10"#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = Tokens::new(tokens);
	assert_eq!(parse_expr(p, &mut tokens), Expr::Num(10f64));
}

#[test]
fn unexp() {
	for (s, op) in vec![("-", UnOp::Minus), ("not ", UnOp::Not), ("#", UnOp::Len)] {
		let p = format!("{}nil", s);
		let tokens: Vec<_> = Lexer::new(&p).collect();
		let mut tokens = Tokens::new(tokens);
		assert_eq!(
			parse_expr(&p, &mut tokens),
			Expr::UnExp(UnExp {
				op,
				exp: Box::new(Expr::Nil)
			})
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
		let p = format!("true {} false", s);
		let tokens: Vec<_> = Lexer::new(&p).collect();
		let mut tokens = Tokens::new(tokens);
		assert_eq!(
			parse_expr(&p, &mut tokens),
			(Expr::BinExp(BinExp {
				op,
				lhs: Box::new(Expr::Bool(true)),
				rhs: Box::new(Expr::Bool(false))
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
	let p = "true - false";
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = Tokens::new(tokens);
	assert_eq!(
		parse_expr(p, &mut tokens),
		(Expr::BinExp(BinExp {
			op: BinOp::Minus,
			lhs: Box::new(Expr::Bool(true)),
			rhs: Box::new(Expr::Bool(false))
		}))
	);
}

#[test]
fn prefix_exp_parens() {
	let p = r#"foo"#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = Tokens::new(tokens);

	assert_eq!(parse_expr(p, &mut tokens), Expr::Name(Name("foo".to_string())));

	let p = r#"(foo)"#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = Tokens::new(tokens);

	assert_eq!(parse_expr(p, &mut tokens), Expr::Name(Name("foo".to_string())));
}

#[test]
fn args() {
	let p = r#"()"#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = Tokens::new(tokens);

	assert_eq!(parse_args(p, &mut tokens), vec![]);

	let p = r#"(nil, false, 10)"#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = Tokens::new(tokens);

	assert_eq!(
		parse_args(p, &mut tokens),
		vec![Expr::Nil, Expr::Bool(false), Expr::Num(10f64),]
	);
}

#[test]
fn exprs() {
	let p = r#"nil, false, true, "str""#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = Tokens::new(tokens);
	assert_eq!(
		parse_exprs(p, &mut tokens),
		(vec![
			Expr::Nil,
			Expr::Bool(false),
			Expr::Bool(true),
			Expr::Str(String::from("str"))
		])
	);
}

#[test]
fn expr_lambda() {
	let p = r#"function () return ; end"#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = Tokens::new(tokens);

	assert_eq!(
		parse_expr(p, &mut tokens),
		(Expr::Lambda(FuncBody {
			params: vec![],
			body: Block {
				stats: vec![Stat::Return(vec![])]
			}
		}))
	);
}

#[test]
#[should_panic(expected = "Expected `(` but found identifier.")]
fn expect_lambda_fail() {
	let p = r#"local x = function a() return 1 end"#;
	parse(p);
}

#[test]
fn test_return() {
	let p = r#"return nil, false;"#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = Tokens::new(tokens);

	assert_eq!(parse_return(p, &mut tokens), (vec![Expr::Nil, Expr::Bool(false),]));

	let p = r#"return 10, "foo", true, nil"#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = Tokens::new(tokens);

	assert_eq!(
		parse_return(p, &mut tokens),
		(vec![
			Expr::Num(10f64),
			Expr::Str(String::from("foo")),
			Expr::Bool(true),
			Expr::Nil
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
	let p = r#"return 10, "foo", true, nil"#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = Tokens::new(tokens);

	assert_eq!(
		parse_block(p, &mut tokens),
		(Block {
			stats: vec![Stat::Return(vec![
				Expr::Num(10f64),
				Expr::Str(String::from("foo")),
				Expr::Bool(true),
				Expr::Nil
			])]
		})
	);
}

#[test]
fn functiondef() {
	let p = r#"function foo() return end"#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = Tokens::new(tokens);

	assert_eq!(
		parse_function_def(p, &mut tokens),
		(FunctionDef {
			name: vec![Name(String::from("foo"))],
			body: FuncBody {
				params: vec![],
				body: Block {
					stats: vec![Stat::Return(vec![])]
				}
			},
			local: false,
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
			Field::Assign(Name(String::from("foo")), Expr::Str(String::from("foo"))),
			Field::Assign(Name(String::from("bar")), Expr::Bool(false)),
			Field::Assign(Name(String::from("bizz")), Expr::Num(1f64)),
		]
	);

	let p = r#"{}"#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = Tokens::new(tokens);

	assert_eq!(parse_table_constructor(p, &mut tokens), vec![]);

	let p = r#"{'one', false, 3}"#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = Tokens::new(tokens);

	assert_eq!(
		parse_table_constructor(p, &mut tokens),
		vec![
			Field::Expr(Expr::Str(String::from("one"))),
			Field::Expr(Expr::Bool(false)),
			Field::Expr(Expr::Num(3f64)),
		]
	);
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
fn names() {
	let p = r#"Name"#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = Tokens::new(tokens);

	assert_eq!(parse_names(p, &mut tokens), (vec![Name(String::from("Name"))]));

	let p = r#"Name,Another_Name"#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = Tokens::new(tokens);

	assert_eq!(
		parse_names(p, &mut tokens),
		(vec![Name(String::from("Name")), Name(String::from("Another_Name"))])
	);
}

#[test]
fn statement() {
	let p = r#"break"#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = Tokens::new(tokens);
	assert_eq!(parse_statement(p, &mut tokens), (Stat::Break));

	let p = r#"do end"#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = Tokens::new(tokens);
	assert_eq!(parse_statement(p, &mut tokens), Stat::DoBlock(Block { stats: vec![] }));

	let p = r#"while true do end"#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = Tokens::new(tokens);
	assert_eq!(
		parse_statement(p, &mut tokens),
		Stat::WhileBlock(WhileBlock {
			expr: Expr::Bool(true),
			block: Block { stats: vec![] }
		})
	);

	let p = r#"if true then elseif false then else end"#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = Tokens::new(tokens);
	assert_eq!(
		parse_statement(p, &mut tokens),
		Stat::IfBlock(IfBlock {
			expr: Expr::Bool(true),
			block: Block { stats: vec![] },
			elseif: vec![ElseIf {
				expr: Expr::Bool(false),
				block: Block { stats: vec![] }
			}],
			else_block: Some(Block { stats: vec![] })
		})
	);

	let p = r#"for foo = 1, 2, nil do end"#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = Tokens::new(tokens);
	assert_eq!(
		parse_statement(p, &mut tokens),
		Stat::ForRange(ForRange {
			name: Name(String::from("foo")),
			exprs: (Expr::Num(1f64), Expr::Num(2f64), Some(Expr::Nil)),
			block: Block { stats: vec![] },
		})
	);

	let p = r#"for foo, bar in true, nil do end"#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = Tokens::new(tokens);
	assert_eq!(
		parse_statement(p, &mut tokens),
		Stat::ForIn(ForIn {
			names: vec![Name(String::from("foo")), Name(String::from("bar"))],
			exprs: vec![Expr::Bool(true), Expr::Nil],
			block: Block { stats: vec![] },
		})
	);

	let p = r#"function foo(a) return; end"#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = Tokens::new(tokens);
	assert_eq!(
		parse_statement(p, &mut tokens),
		Stat::FunctionDef(FunctionDef {
			name: vec![Name(String::from("foo"))],
			body: FuncBody {
				params: vec![Name(String::from("a"))],

				body: Block {
					stats: vec![Stat::Return(vec![])]
				}
			},
			local: false,
		})
	);

	let p = r#"local function bar() return end"#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = Tokens::new(tokens);
	assert_eq!(
		parse_statement(p, &mut tokens),
		Stat::FunctionDef(FunctionDef {
			name: vec![Name(String::from("bar"))],
			body: FuncBody {
				params: vec![],
				body: Block {
					stats: vec![Stat::Return(vec![])]
				}
			},
			local: true
		})
	);

	let p = r#"local foo, bar, buzz = nil, 10, "word""#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = Tokens::new(tokens);
	assert_eq!(
		parse_statement(p, &mut tokens),
		Stat::Assignment(Assignment {
			vars: vec![
				Expr::Name(Name(String::from("foo"))),
				Expr::Name(Name(String::from("bar"))),
				Expr::Name(Name(String::from("buzz")))
			],
			exprs: vec![Expr::Nil, Expr::Num(10f64), Expr::Str(String::from("word"))],
			local: true,
		})
	);
}

#[test]
fn funcbody() {
	let p = r#"() return end"#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = Tokens::new(tokens);

	assert_eq!(
		parse_funcbody(p, &mut tokens),
		FuncBody {
			params: vec![],
			body: Block {
				stats: vec![Stat::Return(vec![])]
			}
		}
	);
}

#[test]
fn funcname() {
	let p = r#"abc"#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = Tokens::new(tokens);

	assert_eq!(parse_funcname(p, &mut tokens), vec![Name("abc".to_string())],);

	let p = r#"abc.bar1"#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = Tokens::new(tokens);

	assert_eq!(
		parse_funcname(p, &mut tokens),
		vec![Name("abc".to_string()), Name("bar1".to_string())]
	);
}

#[test]
#[should_panic(expected = "Expected expression but found: end of file.")]
fn exprs_fail() {
	let p = r#"nil, false,"#;
	let tokens: Vec<_> = Lexer::new(p).collect();
	let mut tokens = Tokens::new(tokens);
	parse_exprs(p, &mut tokens);
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

#[test]
fn suffix_expr() {
	let p = r#"a"#;
	let mut tokens = make_tokens(p);
	assert_eq!(format!("{:?}", parse_suffix_expr(p, &mut tokens)), "Name(Name(\"a\"))");

	let p = r#"(a)[1]"#;
	let mut tokens = make_tokens(p);
	assert_eq!(
		format!("{:?}", parse_suffix_expr(p, &mut tokens)),
		"SuffixExpr(SuffixExpr { expr: Name(Name(\"a\")), suffix: [Index(Num(1.0))] })"
	);

	let p = r#"a[1]"#;
	let mut tokens = make_tokens(p);
	assert_eq!(
		format!("{:?}", parse_suffix_expr(p, &mut tokens)),
		"SuffixExpr(SuffixExpr { expr: Name(Name(\"a\")), suffix: [Index(Num(1.0))] })"
	);

	let p = r#"a.b.c"#;
	let mut tokens = make_tokens(p);
	assert_eq!(
		format!("{:?}", parse_suffix_expr(p, &mut tokens)),
		"SuffixExpr(SuffixExpr { expr: Name(Name(\"a\")), suffix: [Property(Name(\"b\")), Property(Name(\"c\"))] })"
	);

	let p = r#"a.b(c).d"#;
	let mut tokens = make_tokens(p);
	assert_eq!(
		format!("{:?}", parse_suffix_expr(p, &mut tokens)),
		"SuffixExpr(SuffixExpr { expr: Name(Name(\"a\")), suffix: [Property(Name(\"b\")), Call([Name(Name(\"c\"))]), Property(Name(\"d\"))] })"
	);
}

#[test]
fn stat_assign() {
	let p = r#"a, c = 1, 2"#;
	let mut tokens = make_tokens(p);
	assert_eq!(
		format!("{:?}", parse_statement(p, &mut tokens)),
		"Assignment(Assignment { vars: [Name(Name(\"a\")), Name(Name(\"c\"))], exprs: [Num(1.0), Num(2.0)], local: false })"
	);
}

#[test]
fn stat_call() {
	let p = r#"print("hello", "world")"#;
	let mut tokens = make_tokens(p);
	assert_eq!(
		format!("{:?}", parse_statement(p, &mut tokens)),
		"Call(Call { expr: Name(Name(\"print\")), args: [Str(\"hello\"), Str(\"world\")] })"
	);

	let p = r#"print(test(1))"#;
	let mut tokens = make_tokens(p);
	assert_eq!(
		format!("{:?}", parse_statement(p, &mut tokens)),
		"Call(Call { expr: Name(Name(\"print\")), args: [SuffixExpr(SuffixExpr { expr: Name(Name(\"test\")), suffix: [Call([Num(1.0)])] })] })"
	);
}

#[test]
fn stat_single_local() {
	let p = r#"local x"#;
	let mut tokens = make_tokens(p);
	assert_eq!(
		format!("{:?}", parse_statement(p, &mut tokens)),
		"Assignment(Assignment { vars: [Name(Name(\"x\"))], exprs: [], local: true })"
	);
}
