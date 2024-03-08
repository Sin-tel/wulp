use crate::lexer::*;
use crate::span::Span;
use crate::token::*;

#[cfg(test)]
fn new_lexer(input: &'static str) -> LexIter {
	LexIter::new(input, 0, "test".to_string())
}

#[test]
fn whitespace() {
	let ws = "  ";

	let mut lex = new_lexer(ws);
	lex.next();

	assert_eq!(lex.next(), None);
}

#[test]
fn single_line_comment() {
	let single_line_comment = "// This is an example lua comment";

	let mut lex = new_lexer(single_line_comment);

	assert_eq!(
		lex.next(),
		Some(Token { kind: TokenKind::Comment, span: Span { start: 0, end: 34, file_id: 0 }, line: 0 })
	);
}

#[test]
fn single_line_string() {
	let single_quote = r#"'example string'"#;
	let mut lex = new_lexer(single_quote);
	assert_eq!(lex.next(), Some(Token { kind: TokenKind::Str, span: Span { start: 0, end: 16, file_id: 0 }, line: 0 }));

	let double_quote = r#""example string""#;
	let mut lex = new_lexer(double_quote);
	assert_eq!(lex.next(), Some(Token { kind: TokenKind::Str, span: Span { start: 0, end: 16, file_id: 0 }, line: 0 }));
}

#[test]
fn multi_line_string() {
	let multi_line = r##"#" multi-line	# "" 'raw string "#"##;
	let mut lex = new_lexer(multi_line);
	assert_eq!(lex.next(), Some(Token { kind: TokenKind::Str, span: Span { start: 0, end: 33, file_id: 0 }, line: 0 }));
}

#[test]
fn basic_assignment() {
	use TokenKind::*;

	let nil_assignment = r#"x =nil"#;
	let actual: Vec<_> = new_lexer(nil_assignment).map(|tk| tk.kind).collect();

	assert_eq!(actual, vec![Name, Assign, Nil]);
}

#[test]
fn assignment() {
	use TokenKind::*;

	let string_assignment = r#"x = 'foo'"#;

	let lex = new_lexer(string_assignment);
	let actual: Vec<_> = lex.map(|tk| tk.kind).collect();
	assert_eq!(actual, vec![Name, Assign, Str]);

	let bool_assignment = r#"x = false"#;
	let lex = new_lexer(bool_assignment);
	let actual: Vec<_> = lex.map(|tk| tk.kind).collect();

	assert_eq!(actual, vec![Name, Assign, False]);

	let var_assignment = r#"x = y"#;
	let lex = new_lexer(var_assignment);
	let actual: Vec<_> = lex.map(|tk| tk.kind).collect();
	assert_eq!(actual, vec![Name, Assign, Name]);
}

#[test]
fn string_concat() {
	let p = r#"'foo' .. bar"#;

	let actual: Vec<_> = new_lexer(p).map(|tk| tk.kind).collect();

	use TokenKind::*;
	assert_eq!(actual, vec![Str, Concat, Name]);
}

#[test]
fn not_expressions() {
	let p = r#"not true"#;

	let actual: Vec<_> = new_lexer(p).map(|tk| tk.kind).collect();

	use TokenKind::*;
	assert_eq!(actual, vec![Not, True]);
}

#[test]
fn empty_table() {
	let p = r#"x = {}"#;

	let actual: Vec<_> = new_lexer(p).map(|tk| tk.kind).collect();

	use TokenKind::*;
	assert_eq!(actual, vec![Name, Assign, LCurly, RCurly]);
}

#[test]
fn numbers() {
	let p = r#"1 1.2 1000. .5 -1"#;

	let actual: Vec<_> = new_lexer(p).map(|tk| tk.kind).collect();

	use TokenKind::*;
	assert_eq!(actual, vec![Number, Number, Number, Number, Minus, Number]);
}

#[test]
#[should_panic(expected = "Failed to close string.")]
fn string_fail() {
	let p = r#"'hello"#;
	let _: Vec<_> = new_lexer(p).collect();
}

#[test]
#[should_panic(expected = "Failed to close string.")]
fn string_fail2() {
	let p = r#""hello
	""#;
	let _: Vec<_> = new_lexer(p).collect();
}

#[test]
#[should_panic(expected = "Unexpected token: `$`")]
fn bad_char() {
	let p = r#"let x = $100"#;
	let _: Vec<_> = new_lexer(p).collect();
}
