use crate::ast::*;
use crate::span::format_err;
use crate::visitor::{VisitNode, Visitor};
use std::collections::HashMap;

pub const GLOBALS: [&str; 2] = ["print", "assert"];

#[derive(Debug)]
pub struct SymbolTable {
	pub names: Vec<String>,
}

impl SymbolTable {
	pub fn new() -> Self {
		Self {
			names: vec!["UNKNOWN_SYMBOL".to_string()],
		}
	}
}

pub struct ScopeCheck<'a> {
	scope_stack: Vec<HashMap<&'a str, usize>>,
	symbol_table: SymbolTable,
	input: &'a str,
}

impl<'a> ScopeCheck<'a> {
	pub fn visit(ast: &mut Block, input: &'a str) -> SymbolTable {
		let mut this = Self {
			scope_stack: Vec::new(),
			symbol_table: SymbolTable::new(),
			input,
		};

		this.scope_stack.push(HashMap::new());
		for g in GLOBALS {
			this.new_variable(g);
		}
		this.visit_block(ast);
		this.scope_stack.pop();
		assert!(this.scope_stack.is_empty());

		this.symbol_table
	}

	fn new_variable(&mut self, name: &'a str) {
		let id = self.symbol_table.names.len();
		self.symbol_table.names.push(name.to_string());
		let scope = self.scope_stack.last_mut().unwrap();
		scope.insert(name, id);
	}

	fn lookup(&mut self, name: &str) -> Option<usize> {
		let mut id = None;
		// look up identifier in scope, reverse order
		for scope in self.scope_stack.iter().rev() {
			id = scope.get(name);
			if id.is_some() {
				break;
			}
		}
		id.copied()
	}
}

impl<'a> Visitor for ScopeCheck<'a> {
	fn visit_block(&mut self, node: &mut Block) {
		self.scope_stack.push(HashMap::new());
		node.walk(self);
		self.scope_stack.pop();
	}

	fn visit_for_block(&mut self, node: &mut ForBlock) {
		self.scope_stack.push(HashMap::new());

		for n in &node.names {
			let name = n.span.as_str(self.input);
			self.new_variable(name);
		}

		// TODO: elide push in inner block
		node.walk(self);
		self.scope_stack.pop();
	}

	fn visit_fn_def(&mut self, node: &mut FnDef) {
		assert!(node.name.len() == 1);
		let name = node.name[0].span.as_str(self.input);

		// TODO: check that we are not redefining a function?
		self.new_variable(name);
		// node is already local

		node.walk(self);
	}

	fn visit_fn_body(&mut self, node: &mut FnBody) {
		self.scope_stack.push(HashMap::new());
		for n in &node.params {
			let name = n.span.as_str(self.input);
			self.new_variable(name);
		}

		// TODO: elide push in inner block
		node.walk(self);
		self.scope_stack.pop();
	}

	fn visit_assignment(&mut self, node: &mut Assignment) {
		// TODO: loop
		assert!(node.vars.len() == 1);
		assert!(node.exprs.len() == 1);

		// visit rhs first
		let expr = &mut node.exprs[0];
		expr.visit(self);

		// now check if we need to define a new variable for the rhs
		let var = &mut node.vars[0];

		if let Expr::Name(n) = var {
			let name = n.span.as_str(self.input);
			if self.lookup(name) == None {
				// define a new local
				self.new_variable(name);
				node.local = true
			}
		} else {
			// TODO: check here if proper lvalue
			todo!();
		}
		var.visit(self);
	}

	fn visit_name(&mut self, node: &mut Name) {
		assert!(node.id == 0); // make sure we don't visit twice

		let name = node.span.as_str(self.input);
		match self.lookup(name) {
			Some(id) => node.id = id,
			None => {
				let msg = format!("Undefined variable: `{}`.", name);
				format_err(&msg, node.span, self.input);
				panic!("{msg}");
			},
		}
	}
}
