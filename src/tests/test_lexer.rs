use crate::lexer::*;
use crate::span::Span;
use crate::token::*;

#[test]
fn whitespace() {
	let ws = "  ";

	let mut lex = Lexer::new(ws);
	lex.next();

	assert_eq!(lex.next(), None);
}

#[test]
fn single_line_comment() {
	let single_line_comment = "-- This is an example lua comment";

	let mut lex = Lexer::new(single_line_comment);

	assert_eq!(
		lex.next(),
		Some(Token {
			kind: TokenKind::Comment,
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
			kind: TokenKind::Comment,
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
			kind: TokenKind::Str,
			span: Span { start: 0, end: 16 }
		})
	);

	let double_quote = r#""example string""#;
	let mut lex = Lexer::new(double_quote);
	assert_eq!(
		lex.next(),
		Some(Token {
			kind: TokenKind::Str,
			span: Span { start: 0, end: 16 }
		})
	);
}

#[test]
fn multi_line_string() {
	let multi_line = "[[ This is a multi-line string ]]";
	let mut lex = Lexer::new(multi_line);
	assert_eq!(
		lex.next(),
		Some(Token {
			kind: TokenKind::Str,
			span: Span { start: 0, end: 33 }
		})
	);
}

#[test]
fn basic_assignment() {
	use TokenKind::*;

	let nil_assignment = r#"x =nil"#;
	let actual: Vec<_> = Lexer::new(nil_assignment).map(|tk| tk.kind).collect();

	assert_eq!(actual, vec![Name, Assign, Nil, Eof]);

	let p = r#"local _x = 1"#;
	let actual: Vec<_> = Lexer::new(p).map(|tk| tk.kind).collect();

	assert_eq!(actual, vec![Local, Name, Assign, Number, Eof]);
}

#[test]
fn assignment() {
	use TokenKind::*;

	let string_assignment = r#"x = 'foo'"#;

	let lex = Lexer::new(string_assignment);
	let actual: Vec<_> = lex.map(|tk| tk.kind).collect();
	assert_eq!(actual, vec![Name, Assign, Str, Eof]);

	let bool_assignment = r#"x = false"#;
	let lex = Lexer::new(bool_assignment);
	let actual: Vec<_> = lex.map(|tk| tk.kind).collect();

	assert_eq!(actual, vec![Name, Assign, False, Eof]);

	let var_assignment = r#"x = y"#;
	let lex = Lexer::new(var_assignment);
	let actual: Vec<_> = lex.map(|tk| tk.kind).collect();
	assert_eq!(actual, vec![Name, Assign, Name, Eof]);
}

#[test]
fn string_concat() {
	let p = r#"'foo' .. bar"#;

	let actual: Vec<_> = Lexer::new(p).map(|tk| tk.kind).collect();

	use TokenKind::*;
	assert_eq!(actual, vec![Str, Concat, Name, Eof])
}

#[test]
fn not_expressions() {
	let p = r#"not true"#;

	let actual: Vec<_> = Lexer::new(p).map(|tk| tk.kind).collect();

	use TokenKind::*;
	assert_eq!(actual, vec![Not, True, Eof])
}

#[test]
fn empty_table() {
	let p = r#"x = {}"#;

	let actual: Vec<_> = Lexer::new(p).map(|tk| tk.kind).collect();

	use TokenKind::*;
	assert_eq!(actual, vec![Name, Assign, LCurly, RCurly, Eof]);
}

#[test]
#[should_panic(expected = "Failed to close string.")]
fn string_fail() {
	let p = r#"'hello"#;
	let _: Vec<_> = Lexer::new(p).collect();
}

#[test]
#[should_panic(expected = "Failed to close string.")]
fn string_fail2() {
	let p = r#""hello
	""#;
	let _: Vec<_> = Lexer::new(p).collect();
}

#[test]
#[should_panic(expected = "Unexpected token: `$`")]
fn bad_char() {
	let p = r#"let x = $100"#;
	let _: Vec<_> = Lexer::new(p).collect();
}
