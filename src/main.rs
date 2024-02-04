#![warn(clippy::cast_lossless)]
#![warn(clippy::uninlined_format_args)]
#![warn(clippy::semicolon_if_nothing_returned)]
#![warn(clippy::explicit_iter_loop)]
#![warn(clippy::items_after_statements)]
#![warn(clippy::redundant_else)]
#![deny(unreachable_patterns)]
//
// #![warn(clippy::pedantic)]
// #![allow(clippy::similar_names)]
// #![allow(clippy::enum_glob_use)]
// #![allow(clippy::wildcard_imports)]
// #![allow(clippy::too_many_lines)]

mod ast;
mod ast_print;
mod lexer;
mod parser;
mod span;
mod token;
mod visitor;

#[cfg(test)]
mod tests;

fn main() {
	// let input = r#"
	// local x = 1, nil, true, "test", (x + 5*y)
	// local y = {foo = 'foo', bar = false, bizz = 1}
	// print("hello")
	// a = function ()
	// 	print("ok")
	// 	return 0
	// end
	// return - 1
	// "#;

	let input = r#"
	local y = {'one', false, 3}
	return y[1]
	"#;

	let mut ast = parser::parse(input);
	println!("{input}");
	dbg!(&ast);

	let mut printer = ast_print::AstPrinter;
	printer.print_ast(&mut ast);

	let lua = mlua::Lua::new();
	lua.load(input).exec().ok();
	// let ret = lua.load(input).eval::<String>().unwrap();
	// println!("{ret:?}");
}
