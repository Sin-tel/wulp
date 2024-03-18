use crate::span::InputFile;
use crate::symbol::Symbol;
use crate::ty::TyNode;
use std::fmt;
use std::ops::{Index, IndexMut};

macro_rules! id_type {
	($id:ident, $value:ident) => {
		#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash)]
		pub struct $id(pub u32);

		impl fmt::Display for $id {
			fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
				write!(f, "{}", self.0)
			}
		}
		impl Index<$id> for [$value] {
			type Output = $value;
			fn index(&self, index: $id) -> &$value {
				&self[index.0 as usize]
			}
		}
		impl Index<$id> for Vec<$value> {
			type Output = $value;
			fn index(&self, index: $id) -> &$value {
				&self.as_slice()[index]
			}
		}
		impl IndexMut<$id> for [$value] {
			fn index_mut(&mut self, index: $id) -> &mut $value {
				&mut self[index.0 as usize]
			}
		}
		impl IndexMut<$id> for Vec<$value> {
			fn index_mut(&mut self, index: $id) -> &mut $value {
				&mut self.as_mut_slice()[index]
			}
		}
	};
}

id_type!(TyId, TyNode);
id_type!(SymbolId, Symbol);
id_type!(FileId, InputFile);
