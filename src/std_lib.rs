use crate::symbol::SymbolId;
use crate::ty::Ty;
use lazy_static::lazy_static;

#[derive(Debug)]
pub struct Item {
	pub name: &'static str,
	pub ty: Ty,
	pub id: SymbolId,
}

// TODO: define these in the language itself
lazy_static! {
	pub static ref GLOBALS: Vec<Item> = vec![
		Item {
			name: "print",
			ty: Ty::Fn(vec![Ty::Any], Box::new(Ty::Nil)),
			id: 1
		},
		Item {
			name: "assert",
			ty: Ty::Fn(vec![Ty::Bool], Box::new(Ty::Nil)),
			id: 2
		},
		Item {
			name: "error",
			ty: Ty::Fn(vec![Ty::Str], Box::new(Ty::Bottom)),
			id: 3
		},
	];
}
