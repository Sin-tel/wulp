use crate::lexer::*;
use crate::span::Span;
use crate::token::*;

#[test]
fn whitespace() {
	let ws = "  ";

	let mut lex = LexIter::new(ws);
	lex.next();

	assert_eq!(lex.next(), None);
}

#[test]
fn single_line_comment() {
	let single_line_comment = "// This is an example lua comment";

	let mut lex = LexIter::new(single_line_comment);

	assert_eq!(
		lex.next(),
		Some(Token {
			kind: TokenKind::Comment,
			span: Span { start: 0, end: 34 },
			line: 0
		})
	);
}

#[test]
fn single_line_string() {
	let single_quote = r#"'example string'"#;
	let mut lex = LexIter::new(single_quote);
	assert_eq!(
		lex.next(),
		Some(Token {
			kind: TokenKind::Str,
			span: Span { start: 0, end: 16 },
			line: 0
		})
	);

	let double_quote = r#""example string""#;
	let mut lex = LexIter::new(double_quote);
	assert_eq!(
		lex.next(),
		Some(Token {
			kind: TokenKind::Str,
			span: Span { start: 0, end: 16 },
			line: 0
		})
	);
}

#[test]
fn multi_line_string() {
	let multi_line = r##"#" multi-line	# "" 'raw string "#"##;
	let mut lex = LexIter::new(multi_line);
	assert_eq!(
		lex.next(),
		Some(Token {
			kind: TokenKind::Str,
			span: Span { start: 0, end: 33 },
			line: 0
		})
	);
}

#[test]
fn basic_assignment() {
	use TokenKind::*;

	let nil_assignment = r#"x =nil"#;
	let actual: Vec<_> = LexIter::new(nil_assignment).map(|tk| tk.kind).collect();

	assert_eq!(actual, vec![Name, Assign, Nil]);
}

#[test]
fn assignment() {
	use TokenKind::*;

	let string_assignment = r#"x = 'foo'"#;

	let lex = LexIter::new(string_assignment);
	let actual: Vec<_> = lex.map(|tk| tk.kind).collect();
	assert_eq!(actual, vec![Name, Assign, Str]);

	let bool_assignment = r#"x = false"#;
	let lex = LexIter::new(bool_assignment);
	let actual: Vec<_> = lex.map(|tk| tk.kind).collect();

	assert_eq!(actual, vec![Name, Assign, False]);

	let var_assignment = r#"x = y"#;
	let lex = LexIter::new(var_assignment);
	let actual: Vec<_> = lex.map(|tk| tk.kind).collect();
	assert_eq!(actual, vec![Name, Assign, Name]);
}

#[test]
fn string_concat() {
	let p = r#"'foo' .. bar"#;

	let actual: Vec<_> = LexIter::new(p).map(|tk| tk.kind).collect();

	use TokenKind::*;
	assert_eq!(actual, vec![Str, Concat, Name])
}

#[test]
fn not_expressions() {
	let p = r#"not true"#;

	let actual: Vec<_> = LexIter::new(p).map(|tk| tk.kind).collect();

	use TokenKind::*;
	assert_eq!(actual, vec![Not, True])
}

#[test]
fn empty_table() {
	let p = r#"x = {}"#;

	let actual: Vec<_> = LexIter::new(p).map(|tk| tk.kind).collect();

	use TokenKind::*;
	assert_eq!(actual, vec![Name, Assign, LCurly, RCurly]);
}

#[test]
#[should_panic(expected = "Failed to close string.")]
fn string_fail() {
	let p = r#"'hello"#;
	let _: Vec<_> = LexIter::new(p).collect();
}

#[test]
#[should_panic(expected = "Failed to close string.")]
fn string_fail2() {
	let p = r#""hello
	""#;
	let _: Vec<_> = LexIter::new(p).collect();
}

#[test]
#[should_panic(expected = "Unexpected token: `$`")]
fn bad_char() {
	let p = r#"let x = $100"#;
	let _: Vec<_> = LexIter::new(p).collect();
}
