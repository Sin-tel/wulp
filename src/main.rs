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

use crate::emit::EmitLua;
use crate::parser::Parser;
use crate::scope::ScopeCheck;
use crate::typecheck::TypeCheck;
use anyhow::Result;
use mlua::{prelude::LuaResult, LuaOptions, StdLib};
use std::env;
use std::fs;
use std::io::Write;
use std::path::Path;

mod ast;
mod emit;
mod lexer;
mod parser;
mod scope;
mod span;
mod symbol;
mod token;
mod ty;
mod typecheck;
mod visitor;

#[cfg(test)]
mod tests;

fn main() -> Result<()> {
	env::set_current_dir(Path::new("./wulp"))?;
	let filename = "test";
	let (mut ast, files) = Parser::parse(filename)?;
	let symbol_table = ScopeCheck::check(&mut ast, &files)?;
	// for s in symbol_table.symbols.iter() {
	// 	dbg!(s);
	// }

	TypeCheck::check(&mut ast, &files, &symbol_table)?;

	// symbol_table.mangle();
	let code = EmitLua::emit(&mut ast, symbol_table);
	// println!("----- emitted code:");
	// println!("{code}");

	println!("----- execute:");
	env::set_current_dir(Path::new("../lua"))?;
	let lua = mlua::Lua::new_with(
		StdLib::PACKAGE | StdLib::STRING | StdLib::MATH | StdLib::IO,
		LuaOptions::default(),
	)?;
	let mut chunk = lua.load(code.clone());
	chunk = chunk.set_name(filename.to_string());
	let res = chunk.exec();
	// let res = chunk.eval::<String>();
	display_return(res, filename);

	env::set_current_dir(Path::new("../out"))?;
	let mut file = fs::File::create(format!("{filename}.lua"))?;
	file.write_all(code.as_bytes())?;

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
