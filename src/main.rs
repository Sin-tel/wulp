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
#![warn(clippy::unnested_or_patterns)]
#![warn(clippy::inconsistent_struct_constructor)]
#![warn(clippy::unused_self)]
#![warn(clippy::needless_borrow)]
#![warn(clippy::match_wildcard_for_single_variants)]
#![warn(clippy::manual_assert)]
#![warn(clippy::manual_let_else)]
//
// #![warn(clippy::pedantic)]
// #![allow(clippy::similar_names)]
// #![allow(clippy::enum_glob_use)]
// #![allow(clippy::wildcard_imports)]
// #![allow(clippy::too_many_lines)]
// #![allow(clippy::doc_markdown)]
// #![allow(clippy::module_name_repetitions)]

use crate::emit::EmitLua;
use crate::parser::Parser;
use crate::scope::ScopeCheck;
use crate::typecheck::TypeCheck;
use anyhow::Result;
use mlua::{prelude::LuaResult, LuaOptions, StdLib};
use std::fs;
use std::io::Write;
use std::path::Path;

mod ast;
mod emit;
mod index;
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
	// let path = Path::new("wulp/tests/ambiguous");
	let path = Path::new("wulp/test");

	let (mut modules, files) = Parser::parse(path)?;
	let symbol_table = ScopeCheck::check(&mut modules, &files)?;
	// for s in symbol_table.symbols.iter() {
	// 	dbg!(s);
	// }

	TypeCheck::check(&mut modules, &files, &symbol_table)?;

	// symbol_table.mangle();
	let code = EmitLua::emit(&mut modules, symbol_table);

	println!("----- execute:");
	let lua = load_lua()?;
	let mut chunk = lua.load(code.clone());
	let path_str = path.to_string_lossy();
	chunk = chunk.set_name(path_str.clone());
	let res = chunk.exec();
	// let res = chunk.eval::<String>();
	display_return(res, &path_str);

	let mut file = fs::File::create(format!("out/{}.lua", path_str.to_string().replace('/', "_")))?;
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

fn load_lua() -> Result<mlua::Lua> {
	Ok(mlua::Lua::new_with(
		StdLib::PACKAGE | StdLib::STRING | StdLib::MATH | StdLib::IO | StdLib::OS | StdLib::BIT | StdLib::JIT,
		LuaOptions::default(),
	)?)
}
