#![warn(clippy::cast_lossless)]
#![warn(clippy::uninlined_format_args)]
#![warn(clippy::semicolon_if_nothing_returned)]
#![warn(clippy::explicit_iter_loop)]
#![warn(clippy::items_after_statements)]
#![warn(clippy::redundant_else)]
#![warn(clippy::match_same_arms)]
#![deny(unreachable_patterns)]

// #![warn(clippy::pedantic)]
// #![allow(clippy::similar_names)]
// #![allow(clippy::enum_glob_use)]
// #![allow(clippy::wildcard_imports)]
// #![allow(clippy::too_many_lines)]
// #![allow(clippy::doc_markdown)]

use crate::emit::EmitLua;
use std::fs;

mod ast;
mod ast_print;
mod emit;
mod lexer;
mod parser;
mod span;
mod token;
mod visitor;

#[cfg(test)]
mod tests;

fn main() {
	let input = fs::read_to_string("lua/test.lua").expect("Should have been able to read the file");

	// let input = r#"
	// -- local x = 1, nil, true, "test", (x + 5*y)
	// -- local y = {foo = 'foo', bar = false, bizz = 1}
	// -- print("hello")
	// -- a = function (b)
	// -- 	print("ok")
	// -- 	return 0
	// -- end
	// -- return - 1
	// return a(b,c)
	// "#;

	let mut ast = parser::parse(&input);
	// dbg!(&ast);

	println!("----- input:");
	println!("{input}");

	println!("----- AST:");
	let mut printer = ast_print::AstPrinter;
	printer.print_ast(&mut ast);

	println!("----- emitted code:");
	let code = EmitLua::emit(&mut ast);
	println!("{code}");

	println!("----- execute:");
	let lua = mlua::Lua::new();
	let ret = lua.load(code).eval::<String>();
	println!("{ret:?}");

	// lua.load(input).exec().ok();
}
