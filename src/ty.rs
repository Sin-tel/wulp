use std::fmt;
// use std::iter::zip;

pub type TyId = usize;
pub type TableId = usize;

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
	TyVar, // type variable
	Free,  // free type varaible
	Instance(TableId),
	Table(TableId),
	Array(TyId),
	Maybe(TyId),
	Fn(Vec<TyId>, TyId), // args, ret
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TyAst {
	Any,
	Nil,
	Bool,
	Str,
	Num,
	Int,
	SelfTy,
	Named(String),
	Table(TableId),
	Array(Box<TyAst>),
	Maybe(Box<TyAst>),
	Fn(Vec<TyAst>, Box<TyAst>),
}

impl fmt::Display for TyAst {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		let s = match self {
			TyAst::Any => "Any".to_string(),
			TyAst::Nil => "nil".to_string(),
			TyAst::Bool => "bool".to_string(),
			TyAst::Str => "str".to_string(),
			TyAst::Num => "num".to_string(),
			TyAst::Int => "int".to_string(),
			TyAst::SelfTy => "self".to_string(),
			TyAst::Table(_) => "table".to_string(),
			TyAst::Named(s) => s.clone(),
			TyAst::Array(ty) => format!("[{ty}]"),
			TyAst::Maybe(ty) => format!("maybe({ty})"),
			TyAst::Fn(args, ret) => {
				let args = args.iter().map(|a| a.to_string()).collect::<Vec<String>>().join(", ");
				format!("fn({args}) -> {ret}")
			},
		};
		write!(f, "{s}")
	}
}
