mod ast;
mod lexer;
mod parser;
mod span;
mod token_iter;

#[cfg(test)]
mod tests;

fn main() {
	let input = r#"
	while a do end
	"#;

	// let input = " print('hello world')";

	let ast = parser::parse(&input).unwrap();
	dbg!(ast);
}
