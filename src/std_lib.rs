use crate::symbol::SymbolId;
use crate::ty::Ty;
use lazy_static::lazy_static;

#[derive(Debug)]
pub struct Item {
	pub name: &'static str,
	pub is_fn_def: bool,
	pub param_ty: Vec<Ty>,
	pub ret_ty: Ty,
	pub id: SymbolId,
}

// TODO: define these in the language itself
lazy_static! {
	pub static ref GLOBALS: Vec<Item> = vec![
		Item {
			name: "print",
			is_fn_def: true,
			param_ty: vec![Ty::Any],
			ret_ty: Ty::Unit,
			id: 5
		},
		// Item {
		// 	name: "assert",
		// 	is_fn_def: true,
		// 	param_ty: vec![Ty::Bool],
		// 	ret_ty: Ty::Unit,
		// 	id: 6
		// },
		// Item {
		// 	name: "error",
		// 	is_fn_def: true,
		// 	param_ty: vec![Ty::Str],
		// 	ret_ty: Ty::Err,
		// 	id: 7
		// },
	];
}
