use crate::ast::*;
use crate::span::format_err;
use crate::std_lib::GLOBALS;
use crate::symbol::{Symbol, SymbolId, SymbolTable};
use crate::ty::Ty;
use crate::visitor::{VisitNode, Visitor};
use std::collections::HashMap;

pub struct ScopeCheck<'a> {
	scope_stack: Vec<HashMap<&'a str, SymbolId>>,
	symbol_table: SymbolTable,
	input: &'a str,
	errors: Vec<String>,
}

impl<'a> ScopeCheck<'a> {
	pub fn check(ast: &mut File, input: &'a str) -> Result<SymbolTable, String> {
		let mut this = Self {
			scope_stack: Vec::new(),
			symbol_table: SymbolTable::new(),
			input,
			errors: Vec::new(),
		};

		this.scope_stack.push(HashMap::new());

		for item in GLOBALS.iter() {
			this.new_variable(item.name, true);
			assert_eq!(this.lookup(item.name), Some(item.id));
		}

		this.visit_file(ast);
		this.scope_stack.pop();
		assert!(this.scope_stack.is_empty());

		match this.errors.last() {
			Some(err) => Err(err.to_string()),
			None => Ok(this.symbol_table),
		}
	}

	fn new_variable(&mut self, name: &'a str, is_const: bool) {
		let symbol = Symbol::new(name, is_const);
		let id = self.symbol_table.push(symbol);
		// unwrap: there is always at least one scope
		let scope = self.scope_stack.last_mut().unwrap();
		scope.insert(name, id);
	}

	fn lookup(&mut self, name: &str) -> Option<SymbolId> {
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

	fn make_lvalue(&mut self, var: &mut Expr) -> bool {
		let mut new_def = false;
		match &mut var.kind {
			ExprKind::Name(n) => {
				let name = n.span.as_str(self.input);

				if let Some(id) = self.lookup(name) {
					if self.symbol_table.get(id).is_const {
						let msg = format!("Constant `{name}` already defined.");
						format_err(&msg, n.span, self.input);
						self.errors.push(msg);
					}
				} else {
					self.new_variable(name, false);
					new_def = true;
				}

				var.visit(self);
			},
			ExprKind::SuffixExpr(expr, _) => {
				// indexing and property
				if self.make_lvalue(expr) {
					let msg = format!("Undefined variable: `{}`.", expr.span.as_str(self.input));
					format_err(&msg, expr.span, self.input);
					self.errors.push(msg);
				}
			},
			ExprKind::Call(call) => {
				if self.make_lvalue(&mut call.expr) {
					let msg = format!("Undefined variable: `{}`.", call.expr.span.as_str(self.input));
					format_err(&msg, call.expr.span, self.input);
					self.errors.push(msg);
				}
				for e in &mut call.args {
					e.visit(self);
				}
			},
			_ => unreachable!(),
		}

		new_def
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

		for n in &mut node.names {
			let name = n.span.as_str(self.input);
			// marked as const since we should never assign to loop variable
			self.new_variable(name, true);
			n.visit(self);
		}
		node.expr.visit(self);
		node.block.visit(self);

		self.scope_stack.pop();
	}

	fn visit_fn_def(&mut self, node: &mut FnDef) {
		let name = node.name.span.as_str(self.input);

		let lookup = self.lookup(name);
		if node.path.is_empty() {
			// plain fn def
			if lookup.is_some() {
				let msg = format!("Function `{name}` already defined.");
				format_err(&msg, node.name.span, self.input);
				self.errors.push(msg);
			} else {
				let param_types: Vec<Option<Ty>> = node.body.params.iter().map(|p| p.ty.clone()).collect();
				// function defs are always const
				self.new_variable(name, true);
			}
		} else {
			// fn property on some struct
			if lookup.is_none() {
				let msg = format!("Undefined struct: `{name}`.");
				format_err(&msg, node.name.span, self.input);
				self.errors.push(msg);
			}
		}

		node.walk(self);
	}

	fn visit_fn_body(&mut self, node: &mut FnBody) {
		self.scope_stack.push(HashMap::new());
		for n in &mut node.params {
			let name = n.name.span.as_str(self.input);
			// function args are mutable
			self.new_variable(name, false);
			n.visit(self);
		}
		node.body.visit(self);

		self.scope_stack.pop();
	}

	fn visit_assignment(&mut self, node: &mut Assignment) {
		// visit rhs first
		for e in &mut node.exprs {
			e.visit(self);
		}

		// now check if we need to define a new variable for the rhs
		for var in &mut node.vars {
			match var {
				Var::Expr(e) => {
					let new_def = self.make_lvalue(e);
					node.new_def |= new_def;
				},
				Var::Typed(n) => {
					let name = n.name.span.as_str(self.input);

					if let Some(id) = self.lookup(name) {
						if self.symbol_table.get(id).is_const {
							let msg = format!("Constant `{name}` already defined.");
							format_err(&msg, n.name.span, self.input);
							self.errors.push(msg);
						}
					} else {
						self.new_variable(name, false);
						node.new_def = true;
					}

					n.name.visit(self);
				},
			}
		}
	}

	// TODO: just make this the default order
	fn visit_field(&mut self, node: &mut Field) {
		match node {
			Field::Assign(p, e) => {
				// rhs first
				e.visit(self);
				// this doesn't do anything for now
				p.visit(self);
			},
			Field::Expr(e) => e.visit(self),
			Field::Fn(p, f) => {
				p.visit(self);
				f.visit(self);
			},
		}
	}

	fn visit_name(&mut self, node: &mut Name) {
		assert!(node.id == 0); // make sure we don't visit twice

		let name = node.span.as_str(self.input);
		if let Some(id) = self.lookup(name) {
			node.id = id;
		} else {
			let msg = format!("Undefined variable: `{name}`.");
			format_err(&msg, node.span, self.input);
			self.errors.push(msg);
		}
	}
}
