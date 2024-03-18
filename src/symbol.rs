use crate::index::SymbolId;

#[derive(Debug)]
pub struct Symbol {
	pub name: String,
	pub is_const: bool,
	pub kind: SymbolKind,
}

#[derive(Debug, Eq, PartialEq)]
pub enum SymbolKind {
	Var,
	FnDef,
	Ty,
	GenericTy,
}

impl Symbol {
	#[must_use]
	pub fn new(name: &str) -> Self {
		Self { name: name.to_string(), is_const: false, kind: SymbolKind::Var }
	}

	#[must_use]
	pub fn make_const(mut self) -> Self {
		self.is_const = true;
		self
	}

	#[must_use]
	pub fn fn_def(mut self) -> Self {
		self.is_const = true;
		self.kind = SymbolKind::FnDef;
		self
	}

	#[must_use]
	pub fn ty_def(mut self) -> Self {
		self.is_const = true;
		self.kind = SymbolKind::Ty;
		self
	}

	#[must_use]
	pub fn generic_ty(mut self) -> Self {
		self.is_const = true;
		self.kind = SymbolKind::GenericTy;
		self
	}
}

#[derive(Debug)]
pub struct SymbolTable {
	pub symbols: Vec<Symbol>,
	pub globals: Vec<SymbolId>,
	temp_counter: usize,
}

impl SymbolTable {
	pub fn new() -> Self {
		let mut new = SymbolTable { symbols: Vec::new(), globals: Vec::new(), temp_counter: 0 };
		// id = 0 always points to this
		new.symbols.push(Symbol::new("UNKNOWN_SYMBOL").make_const());
		new
	}

	pub fn push(&mut self, symbol: Symbol) -> SymbolId {
		let id = SymbolId(self.symbols.len().try_into().unwrap());
		self.symbols.push(symbol);
		id
	}

	pub fn fresh_temp(&mut self) -> (SymbolId, String) {
		let name = format!("_t{}", self.temp_counter);
		self.temp_counter += 1;
		let symbol = Symbol::new(&name);

		let id = self.push(symbol);
		(id, name)
	}

	pub fn get(&self, id: SymbolId) -> &Symbol {
		assert!(id != SymbolId(0), "undefined symbol");
		&self.symbols[id]
	}

	#[allow(dead_code)]
	pub fn mangle(&mut self) {
		for (i, s) in self.symbols.iter_mut().enumerate() {
			if !s.is_const && s.kind != SymbolKind::FnDef {
				let mut name = "_v".to_string();
				// name.push_str(&s.name);
				name.push_str(&i.to_string());
				s.name = name;
			}
		}
	}
}
