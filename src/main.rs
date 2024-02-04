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

// use mlua::Lua;

mod ast;
mod lexer;
mod parser;
mod span;
mod token;
// mod visitor;

#[cfg(test)]
mod tests;

fn main() {
	let input = r#"
	local a = "hello"
	print("ok")
	return a
	"#;

	// let input = r#"
	// a = 1
	// f.x(a)
	// "#;

	let ast = parser::parse(input);
	dbg!(ast);

	// let lua = Lua::new();
	// let ret = lua.load(input).eval::<String>().unwrap();
	// println!("{:?}", ret);
}
