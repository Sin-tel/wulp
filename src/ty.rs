use crate::ast;
use crate::index::{FileId, SymbolId, TyId};

#[derive(Debug)]
pub enum TyNode {
	Node(TyId),
	Ty(Ty),
}

// type representation in typechecker
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Ty {
	Any,                        // Any
	Unit,                       // ()
	Err,                        // !
	TyVar,                      // type inference variable
	Free,                       // free type variable (generic)
	Module(FileId),             // imported module
	TyName(SymbolId),           // The type of a type literal (e.g. `int`)
	Named(SymbolId, Vec<TyId>), // A named type with its generic parameters
	Fn(Vec<TyId>, TyId),        // function (args, return)
}

// type representation in AST
#[derive(Debug)]
pub enum TyAst {
	Any,                            // `Any`
	Unit,                           // `()`
	Never,                          // `!`
	SelfTy,                         // `self`
	Named(ast::TyName, Vec<TyAst>), // named type with generic parameters
	Array(Box<TyAst>),              // `[Ty]`
	Fn(Vec<TyAst>, Box<TyAst>),     // `fn(args) -> ret`
}
