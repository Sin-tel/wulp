mod ast;
mod iter;
mod lexer;
mod parser;

#[cfg(test)]
mod tests;

fn main() {
	let input = r#"
	print('hello world')
	a = a + 14
	bbb
	"#;

	// let input = " print('hello world')";

	let ast = parser::parse(&input);
	println!("{:?}", ast);
}

