mod ast;
mod lexer;
mod parser;
mod span;
mod token_iter;

#[cfg(test)]
mod tests;

fn main() {
	let input = r#"
	for x = 1, 10 do
		a = a + 1
		i = g
		print("hello!!!")
	end
	"#;

	// let input = r#"nil, false, true, "str""#;

	// let input = " print('hello world')";

	let ast = parser::parse(&input);
	dbg!(ast);
}
