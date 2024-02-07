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
use mlua::prelude::LuaResult;
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
	// let input = fs::read_to_string("lua/lists.lua").unwrap();

	let input = r#"
	local var = ((x or y)().y[1])(a,b[1]);
	var = ((x or y)().y[1])(a,b[1]);
	((x or y)().y[1])(a,b[1])
	"#;

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

	let lua = mlua::Lua::new();
	println!("----- execute:");
	let res = lua.load(code).into_function();
	// let res = lua.load(code).eval::<String>();
	display_return(res);

	let res = lua.load(input).into_function();
	// let res = lua.load(input).eval::<String>();
	display_return(res);
}

fn display_return<V: std::fmt::Debug>(res: LuaResult<V>) {
	if let Err(e) = res {
		println!("{}", e);
	} else {
		println!("{:?}", res.unwrap());
	}
}
