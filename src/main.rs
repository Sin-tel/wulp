#![warn(clippy::cast_lossless)]
#![warn(clippy::uninlined_format_args)]
#![warn(clippy::semicolon_if_nothing_returned)]
#![warn(clippy::explicit_iter_loop)]
#![warn(clippy::items_after_statements)]
#![warn(clippy::redundant_else)]
#![warn(clippy::match_same_arms)]
#![warn(clippy::single_match_else)]
#![deny(unreachable_patterns)]
#![allow(clippy::match_like_matches_macro)]
#![allow(clippy::enum_variant_names)]
#![allow(clippy::new_without_default)]

// #![warn(clippy::pedantic)]
// #![allow(clippy::similar_names)]
// #![allow(clippy::enum_glob_use)]
// #![allow(clippy::wildcard_imports)]
// #![allow(clippy::too_many_lines)]
// #![allow(clippy::doc_markdown)]

//
#[allow(unused_imports)]
use crate::ast_print::AstPrinter;
use crate::emit::EmitLua;
use crate::scope::ScopeCheck;
use crate::typecheck::TypeCheck;

use mlua::prelude::LuaResult;
use std::fs;

pub mod ast;
pub mod ast_print;
pub mod emit;
pub mod lexer;
pub mod parser;
pub mod scope;
pub mod span;
pub mod std_lib;
pub mod symbol;
pub mod token;
pub mod ty;
pub mod typecheck;
pub mod visitor;

#[cfg(test)]
mod tests;

fn main() -> Result<(), String> {
	let filename = "blua/test.blua";
	let input = fs::read_to_string(filename).unwrap();

	let mut ast = parser::parse(&input);
	// println!("----- input:");
	// println!("{input}");

	let symbol_table = ScopeCheck::check(&mut ast, &input)?;
	TypeCheck::check(&ast, &input)?;

	// println!("----- AST:");
	// AstPrinter::print_ast(&mut ast, &input);

	println!("----- emitted code:");
	let code = EmitLua::emit(&mut ast, symbol_table);
	println!("{code}");

	println!("----- execute:");
	let lua = mlua::Lua::new();
	let mut chunk = lua.load(code);
	chunk = chunk.set_name(filename);
	let res = chunk.exec();
	// let res = chunk.eval::<String>();
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
