use crate::ast;

pub type TyId = usize;

#[derive(Debug)]
pub enum TyNode {
	Node(TyId),
	Ty(Ty),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Ty {
	Any,
	Bottom,
	Nil,
	Bool,
	Str,
	Num,
	Int,
	TyVar,          // type variable
	Free,           // free type varaible
	TyName(String), // The type of the name of a type
	Named(String),
	Array(TyId),
	Maybe(TyId),
	Fn(Vec<TyId>, TyId), // args, ret
}

#[derive(Debug)]
pub enum TyAst {
	SelfTy,
	Named(ast::TyName),
	Array(Box<TyAst>),
	Maybe(Box<TyAst>),
	Fn(Vec<TyAst>, Box<TyAst>),
}
