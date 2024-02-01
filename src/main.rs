mod ast;
mod iter;
mod lex;
mod parse;

#[cfg(test)]
mod lex_test;
#[cfg(test)]
mod parse_test;

use parse::parse;

fn main() {
	let input = r#"
	print('hello world')
	a = a + 14
	bbb
	"#;

	// let input = " print('hello world')";

	let ast = parse(&input);
	println!("{:?}", ast);
}

