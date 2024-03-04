pub type SymbolId = usize;

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
}

impl Symbol {
	#[must_use]
	pub fn new(name: &str) -> Self {
		Self {
			name: name.to_string(),
			is_const: false,
			kind: SymbolKind::Var,
		}
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
}

#[derive(Debug)]
pub struct SymbolTable {
	pub symbols: Vec<Symbol>,
	pub globals: Vec<SymbolId>,
	temp_counter: usize,
	pub t_num: SymbolId,
	pub t_int: SymbolId,
	pub t_str: SymbolId,
	pub t_bool: SymbolId,
}

impl SymbolTable {
	pub fn new() -> Self {
		let mut new = SymbolTable {
			symbols: Vec::new(),
			globals: Vec::new(),
			temp_counter: 0,
			t_num: 0,
			t_int: 0,
			t_str: 0,
			t_bool: 0,
		};
		// id = 0 always points to this
		new.symbols.push(Symbol::new("UNKNOWN_SYMBOL").make_const());
		new
	}

	pub fn push(&mut self, symbol: Symbol) -> SymbolId {
		let id = self.symbols.len();
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
		assert!(id != 0, "undefined symbol");
		&self.symbols[id]
	}

	pub fn mangle(&mut self) {
		for s in &mut self.symbols {
			if !s.is_const && s.kind != SymbolKind::FnDef {
				let mut name = "_Z".to_string();
				name.push_str(&s.name);
				s.name = name;
			}
		}
	}
}
