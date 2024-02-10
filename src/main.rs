#![warn(clippy::cast_lossless)]
#![warn(clippy::uninlined_format_args)]
#![warn(clippy::semicolon_if_nothing_returned)]
#![warn(clippy::explicit_iter_loop)]
#![warn(clippy::items_after_statements)]
#![warn(clippy::redundant_else)]
#![warn(clippy::match_same_arms)]
#![deny(unreachable_patterns)]
#![allow(clippy::enum_variant_names)]

// #![warn(clippy::pedantic)]
// #![allow(clippy::similar_names)]
// #![allow(clippy::enum_glob_use)]
// #![allow(clippy::wildcard_imports)]
// #![allow(clippy::too_many_lines)]
// #![allow(clippy::doc_markdown)]

use crate::ast_print::AstPrinter;
use crate::emit::EmitLua;
use crate::scope::ScopeCheck;
use mlua::prelude::LuaResult;
use std::fs;

mod ast;
mod ast_print;
mod emit;
mod lexer;
mod parser;
mod scope;
mod span;
mod token;
mod visitor;

#[cfg(test)]
mod tests;

fn main() -> Result<(), String> {
	let filename = "blua/ambiguous.blua";
	let input = fs::read_to_string(filename).unwrap();

	// let input = r#"
	// "#;

	let mut ast = parser::parse(&input);
	// dbg!(&ast);

	println!("----- AST:");
	AstPrinter::print_ast(&mut ast, &input);

	println!("----- input:");
	println!("{input}");

	let symbol_table = ScopeCheck::visit(&mut ast, &input)?;

	println!("----- emitted code:");
	let code = EmitLua::emit(&mut ast, symbol_table);
	println!("{code}");

	let lua = mlua::Lua::new();
	println!("----- execute:");
	// // let res = lua.load(code).into_function();
	// // let res = lua.load(code).eval::<String>();

	let mut chunk = lua.load(code);
	chunk = chunk.set_name(filename);
	let res = chunk.exec();
	display_return(res);

	Ok(())
}

fn display_return<V: std::fmt::Debug>(res: LuaResult<V>) {
	if let Err(e) = res {
		println!("{e}");
	} else {
		// println!("{:?}", res.unwrap());
	}
}
