// use mlua::Lua;

mod ast;
mod lexer;
mod parser;
mod span;
mod token;

#[cfg(test)]
mod tests;

fn main() {
	let input = r#"
	for x = 1, 2 do
		a = a + 1
		i = g
	end
	print("hello!")
	"#;

	let ast = parser::parse(&input);
	dbg!(ast);

	// let lua = Lua::new();
	// lua.load(r#"print("hello world!")"#).exec().unwrap();
}
