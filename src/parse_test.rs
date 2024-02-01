use super::ast::*;
use super::iter::*;
use super::lex::*;
use super::parse::*;

use pretty_assertions::assert_eq;

#[test]
fn test_parse_expr() {
    let p = r#"..."#;
    let tokens: Vec<_> = Lexer::new(p).collect();
    let mut tokens = TokenIter::new(&tokens);
    assert_eq!(parse_expr(&mut tokens), Ok(Expr::Dots));

    let p = r#"nil"#;
    let tokens: Vec<_> = Lexer::new(p).collect();
    let mut tokens = TokenIter::new(&tokens);
    assert_eq!(parse_expr(&mut tokens), Ok(Expr::Nil));

    let p = r#"false"#;
    let tokens: Vec<_> = Lexer::new(p).collect();
    let mut tokens = TokenIter::new(&tokens);
    assert_eq!(parse_expr(&mut tokens), Ok(Expr::Bool(false)));

    let p = r#"true"#;
    let tokens: Vec<_> = Lexer::new(p).collect();
    let mut tokens = TokenIter::new(&tokens);
    assert_eq!(parse_expr(&mut tokens), Ok(Expr::Bool(true)));

    let p = r#"10"#;
    let tokens: Vec<_> = Lexer::new(p).collect();
    let mut tokens = TokenIter::new(&tokens);
    assert_eq!(parse_expr(&mut tokens), Ok(Expr::Num(10f64)));
}


#[test]
fn test_parse_unexp() {
    for (s, op) in vec![
        ("-", Unop::Minus),
        ("not ", Unop::Not),
        ("#", Unop::Len),
        ("~", Unop::BitNot),
    ] {
        let p = format!("{}foo", s);
        let tokens: Vec<_> = Lexer::new(&p).collect();
        let mut tokens = TokenIter::new(&tokens);
        assert_eq!(
            parse_expr(&mut tokens),
            Ok(Expr::UnExp(UnExp {
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
        ("//", BinOp::IntDiv),
        ("^", BinOp::Pow),
        ("%", BinOp::Mod),
        ("&", BinOp::BitAnd),
        ("~", BinOp::BitXor),
        ("|", BinOp::BitOr),
        (">>", BinOp::BitShr),
        ("<<", BinOp::BitShl),
        ("..", BinOp::Concat),
        ("<", BinOp::LT),
        ("<=", BinOp::LTE),
        (">", BinOp::GT),
        (">=", BinOp::GTE),
        ("==", BinOp::EQ),
        ("~=", BinOp::NEQ),
        ("and", BinOp::And),
        ("or", BinOp::Or),
    ] {
        let p = format!("foo {} bar", s);
        let tokens: Vec<_> = Lexer::new(&p).collect();
        let mut tokens = TokenIter::new(&tokens);
        assert_eq!(
            parse_expr(&mut tokens),
            Ok(Expr::BinExp(BinExp {
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
    let tokens: Vec<_> = Lexer::new("4 + 3 - 2").collect();
    let mut tokens = TokenIter::new(&tokens);
    // TODO(sbdchd): pretty sure this should be (4 + 3) - 2
    // instead of 4 + (3 - 2)
    assert_eq!(
        parse_expr(&mut tokens),
        Ok(Expr::BinExp(BinExp {
            op: BinOp::Plus,
            lhs: Box::new(Expr::Num(4.0)),
            rhs: Box::new(Expr::BinExp(BinExp {
                op: BinOp::Minus,
                lhs: Box::new(Expr::Num(3.0)),
                rhs: Box::new(Expr::Num(2.0)),
            })),
        }))
    );
}

#[test]
fn test_simple_bin() {
    let tokens: Vec<_> = Lexer::new("foo - bar").collect();
    let mut tokens = TokenIter::new(&tokens);
    assert_eq!(
        parse_expr(&mut tokens),
        Ok(Expr::BinExp(BinExp {
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
    let mut tokens = TokenIter::new(&tokens);

    assert_eq!(
        parse_expr(&mut tokens),
        Ok(Expr::PrefixExp(Box::new(PrefixExpr::Var(Var::Name(Name(
            String::from("foo")
        ))))))
    );

    let p = r#"(foo)"#;
    let tokens: Vec<_> = Lexer::new(p).collect();
    let mut tokens = TokenIter::new(&tokens);

    assert_eq!(
        parse_prefix_exp(&mut tokens),
        Ok(PrefixExpr::Expr(Expr::PrefixExp(Box::new(
            PrefixExpr::Var(Var::Name(Name(String::from("foo"))))
        ))))
    );
}

#[test]
fn test_parse_var() {
    let p = r#"foo"#;
    let tokens: Vec<_> = Lexer::new(p).collect();
    let mut tokens = TokenIter::new(&tokens);

    assert_eq!(
        parse_var(&mut tokens),
        Ok(Var::Name(Name(String::from("foo"))))
    );

    let p = r#"(foo)[1]"#;
    let tokens: Vec<_> = Lexer::new(p).collect();
    let mut tokens = TokenIter::new(&tokens);

    assert_eq!(
        parse_var(&mut tokens),
        Ok(Var::IndexExpr(IndexExpr {
            expr: Box::new(PrefixExpr::Expr(Expr::PrefixExp(Box::new(
                PrefixExpr::Var(Var::Name(Name(String::from("foo")))),
            )))),
            arg: Expr::Num(1f64),
        }))
    );
    let p = r#"foo[1]"#;
    let tokens: Vec<_> = Lexer::new(p).collect();
    let mut tokens = TokenIter::new(&tokens);

    assert_eq!(
        parse_var(&mut tokens),
        Ok(Var::IndexExpr(IndexExpr {
            expr: Box::new(PrefixExpr::Var(Var::Name(Name(String::from("foo")))),),
            arg: Expr::Num(1f64),
        }))
    );

    let p = r#"foo.bar"#;
    let tokens: Vec<_> = Lexer::new(p).collect();
    let mut tokens = TokenIter::new(&tokens);

    assert_eq!(
        parse_var(&mut tokens),
        Ok(Var::PropertyAccess(PropertyAccess {
            expr: Box::new(PrefixExpr::Var(Var::Name(Name(String::from("foo"))))),
            name: Name(String::from("bar"))
        }))
    );
}

#[test]
fn test_parse_args() {
    let p = r#"()"#;
    let tokens: Vec<_> = Lexer::new(p).collect();
    let mut tokens = TokenIter::new(&tokens);

    assert_eq!(parse_args(&mut tokens), Ok(Args::ExprList(vec![])));

    let p = r#"(foo, nil, false, 10)"#;
    let tokens: Vec<_> = Lexer::new(p).collect();
    let mut tokens = TokenIter::new(&tokens);

    assert_eq!(
        parse_args(&mut tokens),
        Ok(Args::ExprList(vec![
            Expr::PrefixExp(Box::new(PrefixExpr::Var(Var::Name(Name(String::from(
                "foo"
            )))))),
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
    let mut tokens = TokenIter::new(&tokens);
    assert_eq!(
        parse_exprlist(&mut tokens),
        Ok(vec![
            Expr::Nil,
            Expr::Bool(false),
            Expr::Bool(true),
            Expr::Str(String::from("str"))
        ])
    );

    let p = r#"nil, false,"#;
    let tokens: Vec<_> = Lexer::new(p).collect();
    let mut tokens = TokenIter::new(&tokens);
    assert_eq!(
        parse_exprlist(&mut tokens),
        Ok(vec![Expr::Nil, Expr::Bool(false)])
    );

    assert_eq!(tokens.next().map(to_kind), Some(TokenKind::Comma));
}

#[test]
fn test_parse_varlist() {
    let p = r#"foo, bar, bizz"#;
    let tokens: Vec<_> = Lexer::new(p).collect();
    let mut tokens = TokenIter::new(&tokens);
    assert_eq!(
        parse_varlist(&mut tokens),
        Ok(vec![
            Var::Name(Name(String::from("foo"))),
            Var::Name(Name(String::from("bar"))),
            Var::Name(Name(String::from("bizz"))),
        ])
    );
}

#[test]
fn test_parse_expr_func() {
    let p = r#"function foo(...) return end"#;
    let tokens: Vec<_> = Lexer::new(p).collect();
    let mut tokens = TokenIter::new(&tokens);

    assert_eq!(
        parse_expr(&mut tokens),
        Ok(Expr::FuncDef(FunctionDef {
            name: FuncName {
                path: vec![Name(String::from("foo"))],
                method: None,
            },
            body: FuncBody {
                params: Params {
                    names: vec![],
                    variadic: true
                },
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
    let mut tokens = TokenIter::new(&tokens);

    assert_eq!(
        parse_retstat(&mut tokens),
        Ok(Some(vec![Expr::Nil, Expr::Bool(false),]))
    );

    let p = r#"return 10, "foo", true, bar"#;
    let tokens: Vec<_> = Lexer::new(p).collect();
    let mut tokens = TokenIter::new(&tokens);

    assert_eq!(
        parse_retstat(&mut tokens),
        Ok(Some(vec![
            Expr::Num(10f64),
            Expr::Str(String::from("foo")),
            Expr::Bool(true),
            Expr::PrefixExp(Box::new(PrefixExpr::Var(Var::Name(Name(String::from(
                "bar"
            ))))))
        ]))
    );

    let p = r#"return"#;
    let tokens: Vec<_> = Lexer::new(p).collect();
    let mut tokens = TokenIter::new(&tokens);

    assert_eq!(parse_retstat(&mut tokens), Ok(Some(vec![])));

    let p = r#"return;"#;
    let tokens: Vec<_> = Lexer::new(p).collect();
    let mut tokens = TokenIter::new(&tokens);

    assert_eq!(parse_retstat(&mut tokens), Ok(Some(vec![])));
}

#[test]
fn test_parse_block() {
    let p = r#"return 10, "foo", true, bar"#;
    let tokens: Vec<_> = Lexer::new(p).collect();
    let mut tokens = TokenIter::new(&tokens);

    assert_eq!(
        parse_block(&mut tokens),
        Ok(Block {
            stats: vec![],
            retstat: Some(vec![
                Expr::Num(10f64),
                Expr::Str(String::from("foo")),
                Expr::Bool(true),
                Expr::PrefixExp(Box::new(PrefixExpr::Var(Var::Name(Name(String::from(
                    "bar"
                ))))))
            ])
        })
    );

    let p = r#"print(foo)"#;
    let tokens: Vec<_> = Lexer::new(p).collect();
    let mut tokens = TokenIter::new(&tokens);

    assert_eq!(
        parse_block(&mut tokens),
        Ok(Block {
            stats: vec![Stat::FunctionCall(FunctionCall {
                expr: Box::new(PrefixExpr::Var(Var::Name(Name(String::from("print"))))),
                args: Args::ExprList(vec![Expr::PrefixExp(Box::new(PrefixExpr::Var(
                    Var::Name(Name(String::from("foo")))
                )))])
            })],
            retstat: None
        })
    );
}

#[test]
fn test_parse_functiondef() {
    let p = r#"function foo(...) return end"#;
    let tokens: Vec<_> = Lexer::new(p).collect();
    let mut tokens = TokenIter::new(&tokens);

    assert_eq!(
        parse_functiondef(&mut tokens),
        Ok(FunctionDef {
            name: FuncName {
                path: vec![Name(String::from("foo"))],
                method: None,
            },
            body: FuncBody {
                params: Params {
                    names: vec![],
                    variadic: true
                },
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
    let mut tokens = TokenIter::new(&tokens);

    assert_eq!(
        parse_table_constructor(&mut tokens),
        Ok(TableConstructor(vec![
            Field::NameAssign(Name(String::from("foo")), Expr::Str(String::from("foo"))),
            Field::NameAssign(Name(String::from("bar")), Expr::Bool(false)),
            Field::NameAssign(Name(String::from("bizz")), Expr::Num(1f64)),
        ]))
    );

    let tokens: Vec<_> = Lexer::new(p).collect();
    let mut tokens = TokenIter::new(&tokens);

    assert_eq!(
        parse_expr(&mut tokens),
        Ok(Expr::Table(TableConstructor(vec![
            Field::NameAssign(Name(String::from("foo")), Expr::Str(String::from("foo"))),
            Field::NameAssign(Name(String::from("bar")), Expr::Bool(false)),
            Field::NameAssign(Name(String::from("bizz")), Expr::Num(1f64)),
        ])))
    );

    let p = r#"{}"#;
    let tokens: Vec<_> = Lexer::new(p).collect();
    let mut tokens = TokenIter::new(&tokens);

    assert_eq!(
        parse_table_constructor(&mut tokens),
        Ok(TableConstructor(vec![]))
    );
}

#[test]
fn test_parse_with_comment() {
    // for now we just filter out comments before we parse
    let p = r#"--[[ example comment ]]--
    ;"#;
    let tokens: Vec<_> = Lexer::new(p).collect();
    let mut tokens = TokenIter::new(&tokens);

    assert_eq!(
        parse_block(&mut tokens),
        Ok(Block {
            stats: vec![],
            retstat: None
        })
    );
}

#[test]
fn test_parse_prefix_exp() {
    let p = "false)";
    let tokens: Vec<_> = Lexer::new(p).collect();
    let mut tokens = TokenIter::new(&tokens);
    assert_eq!(parse_expr(&mut tokens), Ok(Expr::Bool(false)),);
    assert_eq!(tokens.next().map(to_kind), Some(TokenKind::RParen));

    let p = r#"(false)"#;
    let tokens: Vec<_> = Lexer::new(p).collect();

    let mut tokens = TokenIter::new(&tokens);

    assert_eq!(
        parse_prefix_exp(&mut tokens),
        Ok(PrefixExpr::Expr(Expr::Bool(false)))
    );

    let p = r#"foo(false, true, nil)"#;
    let tokens: Vec<_> = Lexer::new(p).collect();

    let mut tokens = TokenIter::new(&tokens);

    assert_eq!(
        parse_prefix_exp(&mut tokens),
        Ok(PrefixExpr::FunctionCall(FunctionCall {
            expr: Box::new(PrefixExpr::Var(Var::Name(Name(String::from("foo"))))),
            args: Args::ExprList(vec![Expr::Bool(false), Expr::Bool(true), Expr::Nil])
        }))
    )
}

#[test]
fn test_parse_parlist() {
    let p = r#"..."#;
    let tokens: Vec<_> = Lexer::new(p).collect();
    let mut tokens = TokenIter::new(&tokens);

    assert_eq!(
        parse_parlist(&mut tokens),
        Ok(Params {
            names: vec![],
            variadic: true
        })
    );

    let p = r#"Name,Another_Name"#;
    let tokens: Vec<_> = Lexer::new(p).collect();
    let mut tokens = TokenIter::new(&tokens);

    assert_eq!(
        parse_parlist(&mut tokens),
        Ok(Params {
            names: vec![
                Name(String::from("Name")),
                Name(String::from("Another_Name"))
            ],
            variadic: false
        })
    );

    let p = r#"Name,Another_Name,..."#;
    let tokens: Vec<_> = Lexer::new(p).collect();
    let mut tokens = TokenIter::new(&tokens);

    assert_eq!(
        parse_parlist(&mut tokens),
        Ok(Params {
            names: vec![
                Name(String::from("Name")),
                Name(String::from("Another_Name"))
            ],
            variadic: true
        })
    );
}

#[test]
fn test_parse_namelist() {
    let p = r#"Name"#;
    let tokens: Vec<_> = Lexer::new(p).collect();
    let mut tokens = TokenIter::new(&tokens);

    assert_eq!(
        parse_namelist(&mut tokens),
        Ok(vec![Name(String::from("Name"))])
    );

    let p = r#"Name,Another_Name"#;
    let tokens: Vec<_> = Lexer::new(p).collect();
    let mut tokens = TokenIter::new(&tokens);

    assert_eq!(
        parse_namelist(&mut tokens),
        Ok(vec![
            Name(String::from("Name")),
            Name(String::from("Another_Name"))
        ])
    );
}

#[test]
fn test_parse_stat() {
    let p = r#";"#;
    let tokens: Vec<_> = Lexer::new(p).collect();
    let mut tokens = TokenIter::new(&tokens);
    assert_eq!(parse_stat(&mut tokens), Ok(Stat::SemiColon));

    let p = r#"::foo::"#;
    let tokens: Vec<_> = Lexer::new(p).collect();
    let mut tokens = TokenIter::new(&tokens);
    assert_eq!(
        parse_stat(&mut tokens),
        Ok(Stat::Label(Name(String::from("foo"))))
    );

    let p = r#"break"#;
    let tokens: Vec<_> = Lexer::new(p).collect();
    let mut tokens = TokenIter::new(&tokens);
    assert_eq!(parse_stat(&mut tokens), Ok(Stat::Break));

    let p = r#"goto foo"#;
    let tokens: Vec<_> = Lexer::new(p).collect();
    let mut tokens = TokenIter::new(&tokens);
    assert_eq!(
        parse_stat(&mut tokens),
        Ok(Stat::Goto(Name(String::from("foo"))))
    );
    let p = r#"do end"#;
    let tokens: Vec<_> = Lexer::new(p).collect();
    let mut tokens = TokenIter::new(&tokens);
    assert_eq!(
        parse_stat(&mut tokens),
        Ok(Stat::DoBlock(Block {
            stats: vec![],
            retstat: None
        }))
    );

    let p = r#"while true do end"#;
    let tokens: Vec<_> = Lexer::new(p).collect();
    let mut tokens = TokenIter::new(&tokens);
    assert_eq!(
        parse_stat(&mut tokens),
        Ok(Stat::WhileBlock(WhileBlock {
            expr: Expr::Bool(true),
            block: Block {
                stats: vec![],
                retstat: None
            }
        }))
    );

    let p = r#"repeat until false"#;
    let tokens: Vec<_> = Lexer::new(p).collect();
    let mut tokens = TokenIter::new(&tokens);
    assert_eq!(
        parse_stat(&mut tokens),
        Ok(Stat::RepeatBlock(RepeatBlock {
            expr: Expr::Bool(false),
            block: Block {
                stats: vec![],
                retstat: None
            }
        }))
    );

    let p = r#"if foo then goto foo elseif bar then else end"#;
    let tokens: Vec<_> = Lexer::new(p).collect();
    let mut tokens = TokenIter::new(&tokens);
    assert_eq!(
        parse_stat(&mut tokens),
        Ok(Stat::IfBlock(Box::new(IfBlock {
            expr: Expr::PrefixExp(Box::new(PrefixExpr::Var(Var::Name(Name(String::from(
                "foo"
            )))))),
            block: Block {
                stats: vec![Stat::Goto(Name(String::from("foo")))],
                retstat: None
            },
            elseif: vec![ElseIf {
                expr: Expr::PrefixExp(Box::new(PrefixExpr::Var(Var::Name(Name(
                    String::from("bar")
                ))))),
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
    let mut tokens = TokenIter::new(&tokens);
    assert_eq!(
        parse_stat(&mut tokens),
        Ok(Stat::ForRange(Box::new(ForRange {
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
    let mut tokens = TokenIter::new(&tokens);
    assert_eq!(
        parse_stat(&mut tokens),
        Ok(Stat::ForIn(ForIn {
            namelist: vec![Name(String::from("foo")), Name(String::from("bar"))],
            exprlist: vec![Expr::Bool(true), Expr::Nil],
            block: Block {
                stats: vec![],
                retstat: None
            },
        }))
    );

    let p = r#"function foo(a, ...) return end"#;
    let tokens: Vec<_> = Lexer::new(p).collect();
    let mut tokens = TokenIter::new(&tokens);
    assert_eq!(
        parse_stat(&mut tokens),
        Ok(Stat::FunctionDef(FunctionDef {
            name: FuncName {
                path: vec![Name(String::from("foo"))],
                method: None,
            },
            body: FuncBody {
                params: Params {
                    names: vec![Name(String::from("a"))],
                    variadic: true
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
    let mut tokens = TokenIter::new(&tokens);
    assert_eq!(
        parse_stat(&mut tokens),
        Ok(Stat::LocalFunctionDef(LocalFunctionDef {
            name: Name(String::from("bar")),
            body: FuncBody {
                params: Params {
                    names: vec![],
                    variadic: false
                },
                body: Block {
                    stats: vec![],
                    retstat: Some(vec![]),
                }
            }
        }))
    );

    let p = r#"local foo, bar, buzz = nil, 10, "word""#;
    let tokens: Vec<_> = Lexer::new(p).collect();
    let mut tokens = TokenIter::new(&tokens);
    assert_eq!(
        parse_stat(&mut tokens),
        Ok(Stat::LocalAssignment(LocalAssignment {
            namelist: vec![
                Name(String::from("foo")),
                Name(String::from("bar")),
                Name(String::from("buzz"))
            ],
            exprlist: Some(vec![
                Expr::Nil,
                Expr::Num(10f64),
                Expr::Str(String::from("word"))
            ]),
        }))
    );

    let p = r#"foo, bar = true, nil"#;
    let tokens: Vec<_> = Lexer::new(p).collect();
    let mut tokens = TokenIter::new(&tokens);
    assert_eq!(
        parse_stat(&mut tokens),
        Ok(Stat::Assignment(Assignment {
            varlist: vec![
                Var::Name(Name(String::from("foo"))),
                Var::Name(Name(String::from("bar")))
            ],
            exprlist: vec![Expr::Bool(true), Expr::Nil],
        }))
    );

    let p = r#"foo("bar")"#;
    let tokens: Vec<_> = Lexer::new(p).collect();
    let mut tokens = TokenIter::new(&tokens);
    assert_eq!(
        parse_stat(&mut tokens),
        Ok(Stat::FunctionCall(FunctionCall {
            expr: Box::new(PrefixExpr::Var(Var::Name(Name(String::from("foo"))))),
            args: Args::ExprList(vec![Expr::Str(String::from("bar"))])
        }))
    );
}
#[test]
fn test_parse_funcbody() {
    let p = r#"() return end"#;
    let tokens: Vec<_> = Lexer::new(p).collect();
    let mut tokens = TokenIter::new(&tokens);

    assert_eq!(
        parse_funcbody(&mut tokens),
        Ok(FuncBody {
            params: Params {
                names: vec![],
                variadic: false
            },
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
    let mut tokens = TokenIter::new(&tokens);

    assert_eq!(
        parse_funcname(&mut tokens),
        Ok(FuncName {
            path: vec![Name("abc".to_string())],
            method: None,
        })
    );

    let p = r#"abc.bar1"#;
    let tokens: Vec<_> = Lexer::new(p).collect();
    let mut tokens = TokenIter::new(&tokens);

    assert_eq!(
        parse_funcname(&mut tokens),
        Ok(FuncName {
            path: vec![Name("abc".to_string()), Name("bar1".to_string())],
            method: None,
        })
    );

    let p = r#"abc.bar1:mno"#;
    let tokens: Vec<_> = Lexer::new(p).collect();
    let mut tokens = TokenIter::new(&tokens);

    assert_eq!(
        parse_funcname(&mut tokens),
        Ok(FuncName {
            path: vec![Name("abc".to_string()), Name("bar1".to_string())],
            method: Some(Name("mno".to_string())),
        })
    );
}

#[test]
fn test_parse_label() {
    let p = r#"::jump_point::"#;
    let tokens: Vec<_> = Lexer::new(p).collect();
    let mut tokens = TokenIter::new(&tokens);

    assert_eq!(parse_label(&mut tokens), Ok(Name("jump_point".to_string())));

    let p = r#"::jump_point"#;
    let tokens: Vec<_> = Lexer::new(p).collect();
    let mut tokens = TokenIter::new(&tokens);

    assert_eq!(parse_label(&mut tokens), Err(()));
}