use crate::ast::*;
use crate::span::{format_err_f, FileId, InputFile};
use crate::symbol::{Symbol, SymbolId, SymbolKind, SymbolTable};
use crate::visitor::{VisitNode, Visitor};
use anyhow::{anyhow, Result};
use rustc_hash::FxHashMap;

type Scope<'a> = FxHashMap<&'a str, SymbolId>;

pub struct ScopeCheck<'a> {
	scope_stack: Vec<Scope<'a>>,
	symbol_table: SymbolTable,
	input: &'a [InputFile],
	errors: Vec<String>,
	modules: FxHashMap<FileId, Scope<'a>>,
}

pub const INT_SYM: SymbolId = 1;
pub const NUM_SYM: SymbolId = 2;
pub const STR_SYM: SymbolId = 3;
pub const BOOL_SYM: SymbolId = 4;
pub const ARRAY_SYM: SymbolId = 5;

impl<'a> ScopeCheck<'a> {
	pub fn check(modules: &mut [Module], input: &'a [InputFile]) -> Result<SymbolTable> {
		let mut this = Self {
			scope_stack: Vec::new(),
			symbol_table: SymbolTable::new(),
			input,
			errors: Vec::new(),
			modules: FxHashMap::default(),
		};

		// global scope
		this.scope_stack.push(FxHashMap::default());
		{
			// keep in sync with typecheck ~55
			this.new_identifier("int", Symbol::new("int").ty_def());
			assert_eq!(this.lookup("int"), Some(INT_SYM));
			this.new_identifier("num", Symbol::new("num").ty_def());
			assert_eq!(this.lookup("num"), Some(NUM_SYM));
			this.new_identifier("str", Symbol::new("str").ty_def());
			assert_eq!(this.lookup("str"), Some(STR_SYM));
			this.new_identifier("bool", Symbol::new("bool").ty_def());
			assert_eq!(this.lookup("bool"), Some(BOOL_SYM));
			this.new_identifier("array", Symbol::new("array").ty_def());
			assert_eq!(this.lookup("array"), Some(ARRAY_SYM));

			for m in modules {
				if m.global {
					this.visit_module(m);
				} else {
					this.scope_stack.push(FxHashMap::default());
					this.visit_module(m);
					let m_scope = this.scope_stack.pop().unwrap();
					this.modules.insert(m.file_id, m_scope);
				}
			}
			if let Some(f) = this.lookup("main") {
				if this.symbol_table.get(f).kind != SymbolKind::FnDef {
					let msg = "`main` should be a function.".to_string();
					this.errors.push(msg);
				}
			} else {
				let msg = "No `main` function found.".to_string();
				this.errors.push(msg);
			};
		}
		this.scope_stack.pop();

		assert!(this.scope_stack.is_empty());

		match this.errors.last() {
			Some(err) => Err(anyhow!("{}", err)),
			None => Ok(this.symbol_table),
		}
	}

	fn new_identifier(&mut self, name: &'a str, symbol: Symbol) -> SymbolId {
		let id = self.symbol_table.push(symbol);
		// unwrap: there is always at least one scope
		let scope = self.scope_stack.last_mut().unwrap();
		scope.insert(name, id);
		id
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
				let name = n.span.as_str_f(self.input);

				if let Some(id) = self.lookup(name) {
					if self.symbol_table.get(id).kind == SymbolKind::FnDef {
						let msg = format!("`{name}` already defined.");
						format_err_f(&msg, n.span, self.input);
						self.errors.push(msg);
					} else if self.symbol_table.get(id).is_const {
						let msg = format!("can't assign to constant `{name}`.");
						format_err_f(&msg, n.span, self.input);
						self.errors.push(msg);
					}
				}
				var.visit(self);
			},
			ExprKind::SuffixExpr(expr, s) => {
				self.check_lvalue(expr);
				for suffix in s {
					match suffix {
						Suffix::Property(_) => (), // this gets done in typechecker
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
	fn visit_module(&mut self, node: &mut Module) {
		for b in &mut node.items {
			match b {
				Item::FnDef(f) => {
					let name = f.name.span.as_str_f(self.input);
					if f.property.is_none() {
						let lookup = self.lookup(name);
						// plain fn def
						if lookup.is_some() {
							let msg = format!("`{name}` already defined");
							format_err_f(&msg, f.name.span, self.input);
							self.errors.push(msg);
						} else {
							// function defs are always const
							self.new_identifier(name, Symbol::new(name).fn_def());
						}
					}
				},
				Item::Import(s) => match &mut s.kind {
					ImportKind::Glob => {
						let m_id = s.file_id.unwrap();
						// TODO: currently everything is public by default!
						for (k, v) in self.modules.get(&m_id).unwrap() {
							let scope = self.scope_stack.last_mut().unwrap();
							scope.insert(k, *v);
						}
					},
					ImportKind::Alias(name) => {
						let name_str = name.span.as_str_f(self.input);
						self.new_identifier(name_str, Symbol::new(name_str).make_const());
						name.visit(self);
					},
				},
				_ => (),
			}
		}
		node.walk(self);
	}

	fn visit_block(&mut self, node: &mut Block) {
		self.scope_stack.push(FxHashMap::default());

		node.walk(self);
		self.scope_stack.pop();
	}

	fn visit_intrinsic(&mut self, node: &mut Intrinsic) {
		if node.property.is_none() {
			let name = node.name.span.as_str_f(self.input);
			self.new_identifier(name, Symbol::new(name).make_const());
		}
		node.name.visit(self);
		node.ty.visit(self);
	}

	fn visit_for_block(&mut self, node: &mut ForBlock) {
		self.scope_stack.push(FxHashMap::default());

		for n in &mut node.names {
			let name = n.span.as_str_f(self.input);
			// marked as const since we should never assign to loop variable
			self.new_identifier(name, Symbol::new(name).make_const());
			n.visit(self);
		}
		node.expr.visit(self);
		node.block.visit(self);

		self.scope_stack.pop();
	}

	fn visit_struct_def(&mut self, node: &mut StructDef) {
		let name = node.name.span.as_str_f(self.input);
		let lookup = self.lookup(name);
		if lookup.is_some() {
			let msg = format!("Struct `{name}` already defined.");
			format_err_f(&msg, node.name.span, self.input);
			self.errors.push(msg);
		} else {
			self.new_identifier(name, Symbol::new(name).ty_def());
			node.name.visit(self);
			node.table.visit(self);
		}
	}

	fn visit_fn_body(&mut self, node: &mut FnBody) {
		self.scope_stack.push(FxHashMap::default());
		for n in &mut node.params {
			let name = n.name.span.as_str_f(self.input);
			self.new_identifier(name, Symbol::new(name));
			n.visit(self);
		}
		node.body.visit(self);
		self.scope_stack.pop();

		if let Some(ty) = &mut node.ty {
			self.visit_ty(ty);
		}
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
			let name_str = n.name.span.as_str_f(self.input);
			self.new_identifier(name_str, Symbol::new(name_str));
			n.name.visit(self);

			if let Some(ty) = &mut n.ty {
				self.visit_ty(ty);
			}
		}
	}

	fn visit_name(&mut self, node: &mut Name) {
		assert_eq!(node.id, 0); // make sure we don't visit twice

		let name = node.span.as_str_f(self.input);
		if let Some(id) = self.lookup(name) {
			node.id = id;
		} else {
			let msg = format!("can't find `{name}` in this scope");
			format_err_f(&msg, node.span, self.input);
			self.errors.push(msg);
			// to suppress further errors, we add a new variable anyway
			self.new_identifier(name, Symbol::new(name));
		}
	}

	fn visit_ty_name(&mut self, node: &mut TyName) {
		assert_eq!(node.id, 0); // make sure we don't visit twice

		let name = node.span.as_str_f(self.input);
		if let Some(id) = self.lookup(name) {
			node.id = id;
		} else {
			// Currently, generic type variables should be uppercase single characters
			assert!(name.len() == 1);
			assert!(name.chars().next().unwrap().is_ascii_uppercase());
			node.id = self.new_identifier(name, Symbol::new(name).generic_ty());
		}
	}
}
