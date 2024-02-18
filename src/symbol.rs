pub type SymbolId = usize;

#[derive(Debug)]
pub struct Symbol {
	pub name: String,
	pub is_const: bool,
	pub is_fn_def: bool,
}

impl Symbol {
	pub fn new(name: &str, is_const: bool, is_fn_def: bool) -> Self {
		Self {
			name: name.to_string(),
			is_const,
			is_fn_def,
		}
	}
}

#[derive(Debug)]
pub struct SymbolTable {
	symbols: Vec<Symbol>,
	pub globals: Vec<SymbolId>,
	temp_counter: usize,
}

impl SymbolTable {
	pub fn new() -> Self {
		let mut new = SymbolTable {
			symbols: Vec::new(),
			globals: Vec::new(),
			temp_counter: 0,
		};
		// id = 0 always points to this
		new.symbols.push(Symbol::new("UNKNOWN_SYMBOL", true, false));
		new
	}

	pub fn push(&mut self, symbol: Symbol) -> SymbolId {
		let id = self.symbols.len();
		self.symbols.push(symbol);
		id
	}

	pub fn fresh_temp(&mut self) -> (SymbolId, String) {
		let name = format!("_tmp{}", self.temp_counter);
		self.temp_counter += 1;
		let symbol = Symbol::new(&name, false, false);

		let id = self.push(symbol);
		(id, name)
	}

	pub fn get(&self, id: SymbolId) -> &Symbol {
		assert!(id != 0);
		&self.symbols[id]
	}
}
