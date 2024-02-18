use crate::ast::*;
use crate::span::format_err;
use crate::std_lib::GLOBALS;
use crate::symbol::{Symbol, SymbolId, SymbolTable};
use crate::visitor::{VisitNode, Visitor};
use anyhow::{anyhow, Result};
use rustc_hash::FxHashMap;

pub struct ScopeCheck<'a> {
	scope_stack: Vec<FxHashMap<&'a str, SymbolId>>,
	symbol_table: SymbolTable,
	input: &'a str,
	errors: Vec<String>,
}

impl<'a> ScopeCheck<'a> {
	pub fn check(ast: &mut File, input: &'a str) -> Result<SymbolTable> {
		let mut this = Self {
			scope_stack: Vec::new(),
			symbol_table: SymbolTable::new(),
			input,
			errors: Vec::new(),
		};

		this.scope_stack.push(FxHashMap::default());

		for item in GLOBALS.iter() {
			this.new_variable(item.name, true, item.is_fn_def);
			assert_eq!(this.lookup(item.name), Some(item.id));
		}

		this.visit_file(ast);
		this.scope_stack.pop();
		assert!(this.scope_stack.is_empty());

		match this.errors.last() {
			Some(err) => Err(anyhow!("{}", err)),
			None => Ok(this.symbol_table),
		}
	}

	fn new_variable(&mut self, name: &'a str, is_const: bool, is_fn_def: bool) {
		let symbol = Symbol::new(name, is_const, is_fn_def);
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

	fn check_lvalue(&mut self, var: &mut Expr) {
		match &mut var.kind {
			ExprKind::Name(n) => {
				let name = n.span.as_str(self.input);

				if let Some(id) = self.lookup(name) {
					if self.symbol_table.get(id).is_fn_def {
						let msg = format!("Function `{name}` already defined.");
						format_err(&msg, n.span, self.input);
						self.errors.push(msg);
					} else if self.symbol_table.get(id).is_const {
						let msg = format!("Can't assign to constant `{name}`.");
						format_err(&msg, n.span, self.input);
						self.errors.push(msg);
					}
				}
				var.visit(self);
			},
			ExprKind::SuffixExpr(expr, s) => {
				self.check_lvalue(expr);
				for suffix in s {
					match suffix {
						Suffix::Property(_) => todo!(),
						Suffix::Index(e) => e.visit(self),
					};
				}
			},
			ExprKind::Call(call) => {
				self.check_lvalue(&mut call.expr);
				for e in &mut call.args {
					e.visit(self);
				}
			},
			_ => unreachable!(),
		}
	}
}

impl<'a> Visitor for ScopeCheck<'a> {
	fn visit_block(&mut self, node: &mut Block) {
		self.scope_stack.push(FxHashMap::default());
		node.walk(self);
		self.scope_stack.pop();
	}

	fn visit_for_block(&mut self, node: &mut ForBlock) {
		self.scope_stack.push(FxHashMap::default());

		for n in &mut node.names {
			let name = n.span.as_str(self.input);
			// marked as const since we should never assign to loop variable
			self.new_variable(name, true, false);
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
				// function defs are always const
				self.new_variable(name, true, true);
			}
		} else {
			// fn property on some type
			if lookup.is_none() {
				let msg = format!("Undefined type: `{name}`.");
				format_err(&msg, node.name.span, self.input);
				self.errors.push(msg);
				// to suppress further errors, we add a new variable anyway
				// self.new_variable(name, false, false);
				return;
			}
		}

		node.walk(self);
	}

	fn visit_fn_body(&mut self, node: &mut FnBody) {
		self.scope_stack.push(FxHashMap::default());
		for n in &mut node.params {
			let name = n.name.span.as_str(self.input);
			// function args are mutable
			self.new_variable(name, false, false);
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

		// now check if rhs variables exist
		for var in &mut node.vars {
			self.check_lvalue(var);
		}
	}

	fn visit_assign_op(&mut self, node: &mut AssignOp) {
		node.expr.visit(self);

		self.check_lvalue(&mut node.var);
	}

	fn visit_let(&mut self, node: &mut Let) {
		// visit rhs first
		for e in &mut node.exprs {
			e.visit(self);
		}

		// make new variables for lhs
		for n in &mut node.names {
			// TODO: make sure we don't do anything stupid like let x, x = 1, 2
			let name_str = n.name.span.as_str(self.input);
			self.new_variable(name_str, false, false);
			n.name.visit(self);
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
			// to suppress further errors, we add a new variable anyway
			self.new_variable(name, false, false);
		}
	}
}
