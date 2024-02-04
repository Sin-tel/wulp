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

// use mlua::Lua;
use crate::visitor::Visitor;

fn main() {
	let input = r#"
	local a = 1
	print("ok")
	return a
	"#;

	// let input = r#"
	// a = 1
	// f.x(a)
	// "#;

	let mut ast = parser::parse(input);
	// dbg!(&ast);
	println!("{}", input);
	let mut printer = ast_print::AstPrinter {};
	printer.visit_block(&mut ast);

	// let lua = Lua::new();
	// let ret = lua.load(input).eval::<String>().unwrap();
	// println!("{:?}", ret);
}
