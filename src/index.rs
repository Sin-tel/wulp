// This should be a macro but I just really love copy-pasting things
use crate::span::InputFile;
use crate::symbol::Symbol;
use crate::ty::TyNode;
use std::fmt;
use std::ops::{Index, IndexMut};

#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub struct TyId(pub u32);

impl fmt::Display for TyId {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "{}", self.0)
	}
}

impl Index<TyId> for [TyNode] {
	type Output = TyNode;

	fn index(&self, index: TyId) -> &TyNode {
		&self[index.0 as usize]
	}
}

impl IndexMut<TyId> for [TyNode] {
	fn index_mut(&mut self, index: TyId) -> &mut TyNode {
		&mut self[index.0 as usize]
	}
}

impl Index<TyId> for Vec<TyNode> {
	type Output = TyNode;
	fn index(&self, index: TyId) -> &TyNode {
		&self.as_slice()[index]
	}
}

impl IndexMut<TyId> for Vec<TyNode> {
	fn index_mut(&mut self, index: TyId) -> &mut TyNode {
		&mut self.as_mut_slice()[index]
	}
}

#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub struct SymbolId(pub u32);

impl fmt::Display for SymbolId {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "{}", self.0)
	}
}

impl Index<SymbolId> for [Symbol] {
	type Output = Symbol;

	fn index(&self, index: SymbolId) -> &Symbol {
		&self[index.0 as usize]
	}
}

impl Index<SymbolId> for Vec<Symbol> {
	type Output = Symbol;
	fn index(&self, index: SymbolId) -> &Symbol {
		&self.as_slice()[index]
	}
}

#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub struct FileId(pub u32);

impl fmt::Display for FileId {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "{}", self.0)
	}
}

impl Index<FileId> for [InputFile] {
	type Output = InputFile;

	fn index(&self, index: FileId) -> &InputFile {
		&self[index.0 as usize]
	}
}

impl Index<FileId> for Vec<InputFile> {
	type Output = InputFile;
	fn index(&self, index: FileId) -> &InputFile {
		&self.as_slice()[index]
	}
}
