use crate::lexer::*;

#[test]
fn whitespace() {
	let ws = "  ";

	let mut lex = Lexer::new(ws);

	assert_eq!(lex.next(), None);
	assert_eq!(lex.next(), None);
}

#[test]
fn single_line_comment() {
	let single_line_comment = "-- This is an example lua comment";

	let mut lex = Lexer::new(single_line_comment);

	assert_eq!(
		lex.next(),
		Some(Token {
			kind: TokenKind::Comment(Comment::SingleLine(String::from(" This is an example lua comment"))),
			span: Span { start: 0, end: 34 }
		})
	);
}

#[test]
fn multi_line_comment() {
	let multi_line_comment = "--[[ multi-line comment ]]--";

	let mut lex = Lexer::new(multi_line_comment);

	assert_eq!(
		lex.next(),
		Some(Token {
			kind: TokenKind::Comment(Comment::MultiLine(String::from(" multi-line comment "))),
			span: Span { start: 0, end: 28 }
		})
	)
}

#[test]
fn single_line_string() {
	let single_quote = r#"'example string'"#;
	let mut lex = Lexer::new(single_quote);
	assert_eq!(
		lex.next(),
		Some(Token {
			kind: TokenKind::String(String::from("example string")),
			span: Span { start: 0, end: 16 }
		})
	);

	let double_quote = r#""example string""#;
	let mut lex = Lexer::new(double_quote);
	assert_eq!(
		lex.next(),
		Some(Token {
			kind: TokenKind::String(String::from("example string")),
			span: Span { start: 0, end: 16 }
		})
	);

	let bad_new_line = r#""example string
	""#;
	let mut lex = Lexer::new(bad_new_line);
	assert_eq!(lex.next(), None);
}

#[test]
fn multi_line_string() {
	let multi_line = "[[ This is a multi-line string ]]";

	let mut lex = Lexer::new(multi_line);
	assert_eq!(
		lex.next(),
		Some(Token {
			kind: TokenKind::String(String::from(" This is a multi-line string ")),
			span: Span { start: 0, end: 2 }
		})
	);
}

#[test]
fn basic_assignment() {
	let nil_assignment = r#"x =nil"#;
	let actual: Vec<_> = Lexer::new(nil_assignment).collect();

	assert_eq!(
		actual,
		vec![
			Token {
				kind: TokenKind::Ident(String::from("x")),
				span: Span { start: 0, end: 1 }
			},
			Token {
				kind: TokenKind::Assign,
				span: Span { start: 2, end: 3 }
			},
			Token {
				kind: TokenKind::Nil,
				span: Span { start: 3, end: 6 }
			},
		]
	);

	let p = r#"local _x = 1"#;
	let actual: Vec<_> = Lexer::new(p).collect();

	assert_eq!(
		actual,
		vec![
			Token {
				kind: TokenKind::Local,
				span: Span { start: 0, end: 5 }
			},
			Token {
				kind: TokenKind::Ident(String::from("_x")),
				span: Span { start: 6, end: 8 }
			},
			Token {
				kind: TokenKind::Assign,
				span: Span { start: 9, end: 10 }
			},
			Token {
				kind: TokenKind::Number(1f64),
				span: Span { start: 11, end: 12 }
			},
		]
	);
}

#[test]
fn assignment() {
	let nil_assignment = r#"x = nil"#;
	let lex = Lexer::new(nil_assignment);
	let actual: Vec<_> = lex.collect();
	assert_eq!(
		actual,
		vec![
			Token {
				kind: TokenKind::Ident(String::from("x")),
				span: Span { start: 0, end: 1 }
			},
			Token {
				kind: TokenKind::Assign,
				span: Span { start: 2, end: 3 }
			},
			Token {
				kind: TokenKind::Nil,
				span: Span { start: 4, end: 7 }
			},
		]
	);

	let string_assignment = r#"x = 'foo'"#;

	let lex = Lexer::new(string_assignment);
	let actual: Vec<_> = lex.collect();
	assert_eq!(
		actual,
		vec![
			Token {
				kind: TokenKind::Ident(String::from("x")),
				span: Span { start: 0, end: 1 }
			},
			Token {
				kind: TokenKind::Assign,
				span: Span { start: 2, end: 3 }
			},
			Token {
				kind: TokenKind::String(String::from("foo")),
				span: Span { start: 4, end: 9 }
			},
		]
	);

	let bool_assignment = r#"x = false"#;
	let lex = Lexer::new(bool_assignment);
	let actual: Vec<_> = lex.collect();

	assert_eq!(
		actual,
		vec![
			Token {
				kind: TokenKind::Ident(String::from("x")),
				span: Span { start: 0, end: 1 }
			},
			Token {
				kind: TokenKind::Assign,
				span: Span { start: 2, end: 3 }
			},
			Token {
				kind: TokenKind::False,
				span: Span { start: 4, end: 9 }
			},
		]
	);

	let var_assignment = r#"x = y"#;
	let lex = Lexer::new(var_assignment);
	let actual: Vec<_> = lex.collect();
	assert_eq!(
		actual,
		vec![
			Token {
				kind: TokenKind::Ident(String::from("x")),
				span: Span { start: 0, end: 1 }
			},
			Token {
				kind: TokenKind::Assign,
				span: Span { start: 2, end: 3 }
			},
			Token {
				kind: TokenKind::Ident(String::from("y")),
				span: Span { start: 4, end: 5 }
			},
		]
	);
}

#[test]
fn string_concat() {
	let p = r#"'foo' .. bar"#;

	let actual: Vec<_> = Lexer::new(p).collect();

	assert_eq!(
		actual,
		vec![
			Token {
				kind: TokenKind::String(String::from("foo")),
				span: Span { start: 0, end: 5 }
			},
			Token {
				kind: TokenKind::Concat,
				span: Span { start: 6, end: 8 }
			},
			Token {
				kind: TokenKind::Ident(String::from("bar")),
				span: Span { start: 9, end: 12 }
			},
		]
	)
}

#[test]
fn not_expressions() {
	let p = r#"not true"#;

	let actual: Vec<_> = Lexer::new(p).collect();
	assert_eq!(
		actual,
		vec![
			Token {
				kind: TokenKind::Not,
				span: Span { start: 0, end: 3 }
			},
			Token {
				kind: TokenKind::True,
				span: Span { start: 4, end: 8 }
			},
		]
	)
}

#[test]
fn empty_table() {
	let p = r#"x = {}"#;

	let actual: Vec<_> = Lexer::new(p).collect();

	assert_eq!(
		actual,
		vec![
			Token {
				kind: TokenKind::Ident(String::from("x")),
				span: Span { start: 0, end: 1 }
			},
			Token {
				kind: TokenKind::Assign,
				span: Span { start: 2, end: 3 }
			},
			Token {
				kind: TokenKind::LCurly,
				span: Span { start: 4, end: 5 }
			},
			Token {
				kind: TokenKind::RCurly,
				span: Span { start: 5, end: 6 }
			},
		]
	);
}
