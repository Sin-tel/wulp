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
	Table(TableId),
	Array(TyId),
	Maybe(TyId),
	Fn(Vec<TyId>, TyId), // args, ret
}

impl fmt::Display for TyNode {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(
			f,
			"{}",
			match self {
				TyNode::Node(id) => format!(">> {id}"),
				TyNode::Ty(ty) => ty.to_string(),
			}
		)
	}
}

impl fmt::Display for Ty {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		let s = match self {
			Ty::Any => "Any".to_string(),
			Ty::Bottom => "Bottom".to_string(),
			Ty::Nil => "nil".to_string(),
			Ty::Bool => "bool".to_string(),
			Ty::Str => "str".to_string(),
			Ty::Num => "num".to_string(),
			Ty::Int => "int".to_string(),
			Ty::TyVar => "T?".to_string(),
			Ty::Free => "Tn".to_string(),
			Ty::Table(_) => "table".to_string(),
			Ty::Array(ty) => format!("[{ty}]"),
			Ty::Maybe(ty) => format!("maybe({ty})"),
			Ty::Fn(args, ret) => {
				let args = args.iter().map(|a| a.to_string()).collect::<Vec<String>>().join(", ");
				format!("fn({args}) -> {ret}")
			},
		};
		write!(f, "{s}")
	}
}
