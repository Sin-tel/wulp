use crate::ast::Ty;

#[derive(Debug)]
pub struct Symbol {
	pub name: String,
	pub is_const: bool,
	pub ty: Ty,
}

impl Symbol {
	pub fn new(name: &str, is_const: bool, ty: Ty) -> Self {
		Self {
			name: name.to_string(),
			is_const,
			ty,
		}
	}
}

#[derive(Debug)]
pub struct SymbolTable {
	symbols: Vec<Symbol>,
}

impl SymbolTable {
	pub fn new() -> Self {
		let mut new = SymbolTable { symbols: Vec::new() };
		new.symbols.push(Symbol::new("UNKNOWN_SYMBOL", true, Ty::Bottom));
		new
	}

	pub fn push(&mut self, symbol: Symbol) -> usize {
		let id = self.symbols.len();
		self.symbols.push(symbol);
		id
	}

	pub fn get(&self, id: usize) -> &Symbol {
		assert!(id != 0);
		&self.symbols[id]
	}

	pub fn get_mut(&mut self, id: usize) -> &mut Symbol {
		assert!(id != 0);
		&mut self.symbols[id]
	}
}
