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

// use crate::ast_print::AstPrinter;
use crate::emit::EmitLua;
use crate::parser::Parser;
use crate::scope::ScopeCheck;
use crate::typecheck::TypeCheck;
use anyhow::Result;
use mlua::{prelude::LuaResult, LuaOptions, StdLib};
use std::env;
use std::fs;
use std::path::Path;

pub mod ast;
// pub mod ast_print;
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

fn main() -> Result<()> {
	let filename = "test";

	let (mut ast, files) = Parser::parse(filename)?;

	// println!("----- input:");
	// println!("{input}");

	let symbol_table = ScopeCheck::check(&mut ast, &files)?;
	// for s in symbol_table.symbols.iter() {
	// 	dbg!(s);
	// }

	TypeCheck::check(&mut ast, &files, &symbol_table)?;

	// // println!("----- AST:");
	// // AstPrinter::print_ast(&mut ast, &input);

	println!("----- emitted code:");
	// symbol_table.mangle();
	let code = EmitLua::emit(&mut ast, symbol_table);
	println!("{code}");

	println!("----- execute:");
	env::set_current_dir(Path::new("./lua"))?;
	let lua = mlua::Lua::new_with(StdLib::PACKAGE, LuaOptions::default())?;
	let mut chunk = lua.load(code.clone());
	chunk = chunk.set_name(filename.to_string());
	let res = chunk.exec();
	// let res = chunk.eval::<String>();
	display_return(res, filename);

	// env::set_current_dir(Path::new(".."))?;
	// let mut file = fs::File::create(filename.replace("blua", "lua"))?;
	// file.write_all(code.as_bytes())?;

	Ok(())
}

fn display_return<V: std::fmt::Debug>(res: LuaResult<V>, filename: &str) {
	if let Err(e) = res {
		// display filename properly
		let f_replace = format!("[string \"{filename}\"]");
		let s = e.to_string().replace(&f_replace, filename);
		eprintln!("{s}");
	} else {
		// println!("{:?}", res.unwrap());
	}
}
