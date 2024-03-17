use crate::emit::EmitLua;
use crate::load_lua;
use crate::parser::Parser;
use crate::scope::ScopeCheck;
use crate::typecheck::TypeCheck;
use anyhow::Result;
use std::path::Path;

#[cfg(test)]
fn compile_test(filename: &str) -> Result<()> {
	let (mut ast, files) = Parser::parse(Path::new(filename))?;
	let symbol_table = ScopeCheck::check(&mut ast, &files)?;
	TypeCheck::check(&mut ast, &files, &symbol_table)?;
	// symbol_table.mangle();
	let code = EmitLua::emit(&mut ast, symbol_table);
	let lua = load_lua()?;
	let mut chunk = lua.load(code);
	chunk = chunk.set_name(filename.to_string());
	chunk.exec()?;

	Ok(())
}

#[test]
fn test_ambiguous_ok() {
	compile_test("wulp/tests/ambiguous_ok").unwrap();
}

#[test]
#[should_panic(expected = "ambiguous syntax")]
fn test_ambiguous_fail() {
	compile_test("wulp/tests/ambiguous_fail").unwrap();
}

#[test]
fn test_array_push() {
	compile_test("wulp/tests/array_push").unwrap();
}

#[test]
#[should_panic(expected = "Expected argument type `int`, found `str`")]
fn test_array_push_fail() {
	compile_test("wulp/tests/array_push_fail").unwrap();
}

#[test]
fn test_assign_call() {
	compile_test("wulp/tests/assign_call").unwrap();
}

#[test]
fn test_assign_index() {
	compile_test("wulp/tests/assign_index").unwrap();
}

#[test]
fn test_assign_ops() {
	compile_test("wulp/tests/assign_ops").unwrap();
}

#[test]
fn test_basic() {
	compile_test("wulp/tests/basic").unwrap();
}

#[test]
fn test_curried() {
	compile_test("wulp/tests/curried").unwrap();
}

#[test]
#[should_panic(expected = "assertion failed!")]
fn test_fail() {
	compile_test("wulp/tests/fail").unwrap();
}

#[test]
fn test_lists() {
	compile_test("wulp/tests/lists").unwrap();
}

#[test]
fn test_module() {
	compile_test("wulp/tests/module_test").unwrap();
}

#[test]
fn test_module_as() {
	compile_test("wulp/tests/module_test_as").unwrap();
}

#[test]
fn test_module_glob() {
	compile_test("wulp/tests/module_test_glob").unwrap();
}

#[test]
fn test_mutual_recursion() {
	compile_test("wulp/tests/mutual_recursion").unwrap();
}

#[test]
#[should_panic]
fn test_occurs_check() {
	compile_test("wulp/tests/occurs_check").unwrap();
}

#[test]
fn test_recurse() {
	compile_test("wulp/tests/recurse").unwrap();
}

#[test]
#[should_panic(expected = "can't find `fact` in this scope")]
fn test_recurse_fail() {
	compile_test("wulp/tests/recurse_fail").unwrap();
}

#[test]
fn test_redef() {
	compile_test("wulp/tests/redef").unwrap();
}

#[test]
#[should_panic(expected = "`a` already defined")]
fn test_redef_fail1() {
	compile_test("wulp/tests/redef_fail1").unwrap();
}

#[test]
#[should_panic(expected = "`b` already defined")]
fn test_redef_fail2() {
	compile_test("wulp/tests/redef_fail2").unwrap();
}

#[test]
#[should_panic(expected = "can't assign to constant `print`")]
fn test_redef_fail3() {
	compile_test("wulp/tests/redef_fail3").unwrap();
}

#[test]
#[should_panic(expected = "can't assign to constant `i`")]
fn test_redef_fail4() {
	compile_test("wulp/tests/redef_fail4").unwrap();
}

#[test]
fn test_struct_def() {
	compile_test("wulp/tests/struct_def").unwrap();
}

#[test]
fn test_unicode() {
	compile_test("wulp/tests/unicode").unwrap();
}
