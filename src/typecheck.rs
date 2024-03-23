use crate::ast;
use crate::ast::*;
use crate::index::{FileId, SymbolId, TyId};
use crate::scope::{ARRAY_SYM, BOOL_SYM, INT_SYM, ITER_SYM, NUM_SYM, STR_SYM};
use crate::span::{format_err_f, format_note_f, InputFile, Span};
use crate::symbol::{SymbolKind, SymbolTable};
use crate::ty::*;
use anyhow::anyhow;
use anyhow::Result;
use rustc_hash::FxHashMap;
use std::iter::zip;

#[derive(Debug)]
struct Struct {
	fields: FxHashMap<String, TyId>,
	static_fields: FxHashMap<String, TyId>,
	required_fields: Vec<String>,
	primitive: bool,
}

#[derive(Debug)]
struct Module {
	fields: FxHashMap<String, TyId>,
}

const ERR_TY: TyId = TyId(0);

type RetPair = Option<(TyId, Span)>;

#[derive(Debug)]
pub struct TypeCheck<'a> {
	input: &'a [InputFile],
	errors: Vec<String>,
	env: FxHashMap<SymbolId, TyId>, // TODO: this can be Vec<Option>
	types: Vec<TyNode>,
	structs: FxHashMap<SymbolId, Struct>,
	modules: FxHashMap<FileId, Module>,
	symbol_table: &'a SymbolTable,
}

impl<'a> TypeCheck<'a> {
	pub fn check(modules: &mut [ast::Module], input: &'a [InputFile], symbol_table: &'a SymbolTable) -> Result<()> {
		let mut this = Self {
			input,
			errors: Vec::new(),
			types: Vec::new(),
			env: FxHashMap::default(),
			structs: FxHashMap::default(),
			modules: FxHashMap::default(),
			symbol_table,
		};

		this.types.push(TyNode::Ty(Ty::Err));
		assert!(this.get_type(ERR_TY) == Ty::Err);

		// keep in sync with scope ~55
		this.new_struct(INT_SYM, true);
		this.new_struct(NUM_SYM, true);
		this.new_struct(STR_SYM, true);
		this.new_struct(BOOL_SYM, true);
		this.new_struct(ARRAY_SYM, true);
		this.new_struct(ITER_SYM, true);

		for m in modules {
			// println!("[checking {}]", input[m.file_id].path.display());
			let new_mod = this.eval_module(m);
			this.modules.insert(m.file_id, new_mod);
		}
		// println!("[env]");
		// for (i, s) in symbol_table.symbols.iter().enumerate().skip(1) {
		// 	if let Some(id) = this.lookup(SymbolId(i.try_into().unwrap())) {
		// 		println!("`{}`: {}", s.name, this.ty_to_string(id));
		// 	}
		// }

		// println!("----- types ");
		// for (i, v) in this.types.iter().enumerate() {
		// 	println!("{i}: {:?} {} {}", v, this.get_parent(i), this.ty_to_string(i));
		// }

		match this.errors.last() {
			Some(err) => Err(anyhow!("{}", err)),
			None => Ok(()),
		}
	}
	fn new_struct(&mut self, id: SymbolId, primitive: bool) -> TyId {
		self.structs.insert(
			id,
			Struct {
				fields: FxHashMap::default(),
				static_fields: FxHashMap::default(),
				required_fields: Vec::new(),
				primitive,
			},
		);
		let ty_id = self.new_ty(Ty::TyName(id));
		self.new_def(id, ty_id);
		ty_id
	}
	fn ty_to_string(&self, id: TyId) -> String {
		let id = self.get_parent(id);
		match self.get_type(id) {
			Ty::Any => "any".to_string(),
			Ty::Unit => "()".to_string(),
			Ty::Err => "error".to_string(),
			Ty::TyVar => format!("T{id}?"),
			Ty::Free => format!("T{id}"),
			Ty::Module(id) => format!("Module: {}", &self.input[id].path.display()),
			Ty::TyName(id) => format!("Type({})", &self.symbol_table.get(id).name),
			Ty::Named(id, assoc) => {
				let name = self.symbol_table.get(id).name.clone();
				if assoc.is_empty() {
					name
				} else {
					let assoc = assoc
						.iter()
						.map(|a| self.ty_to_string(*a))
						.collect::<Vec<String>>()
						.join(", ");
					format!("{name}<{assoc}>")
				}
			},
			Ty::Fn(args, ret) => {
				let args = args
					.iter()
					.map(|a| self.ty_to_string(*a))
					.collect::<Vec<String>>()
					.join(", ");
				format!("fn({args}) -> {}", self.ty_to_string(ret))
			},
		}
	}
	fn convert_ast_ty(&mut self, ty_ast: &TyAst, self_ty: Option<TyId>) -> TyId {
		match ty_ast {
			TyAst::Named(s, s_assoc) => {
				let symbol = self.symbol_table.get(s.id);
				match &symbol.kind {
					SymbolKind::Ty => {
						let mut assoc = Vec::new();
						for a in s_assoc {
							assoc.push(self.convert_ast_ty(a, self_ty));
						}

						self.new_ty(Ty::Named(s.id, assoc))
					},
					SymbolKind::GenericTy => {
						assert!(s_assoc.is_empty());
						if let Some(other) = self.lookup(s.id) {
							other
						} else {
							// if it's the first time we encounter this generic, we just add it to the env
							let ty = self.new_ty(Ty::Free);
							self.new_def(s.id, ty);
							ty
						}
					},
					s => unimplemented!("{s:?}"),
				}
			},
			TyAst::Fn(args, ret) => {
				let mut t_args = Vec::new();
				for a in args {
					let ty = self.convert_ast_ty(a, self_ty);
					t_args.push(ty);
				}
				let ty = self.convert_ast_ty(ret, self_ty);
				self.new_ty(Ty::Fn(t_args, ty))
			},
			TyAst::Array(a) => {
				let inner_ty = self.convert_ast_ty(a, self_ty);
				self.new_ty(Ty::Named(ARRAY_SYM, vec![inner_ty]))
			},
			TyAst::SelfTy => {
				if let Some(ty) = self_ty {
					ty
				} else {
					panic!("can't use self here!");
				}
			},
			TyAst::Unit => self.new_ty(Ty::Unit),
			TyAst::Any => self.new_ty(Ty::Any),
			TyAst::Never => self.new_ty(Ty::Err),
		}
	}

	// add symbol to env with specified type
	fn new_def(&mut self, id: SymbolId, ty_id: TyId) {
		self.env.insert(id, ty_id);
	}

	// new type variable
	fn new_ty(&mut self, ty: Ty) -> TyId {
		let ty_id = TyId(self.types.len().try_into().unwrap());
		self.types.push(TyNode::Ty(ty));
		ty_id
	}

	fn new_named(&mut self, struct_id: SymbolId) -> TyId {
		// TODO: add associated types here
		self.new_ty(Ty::Named(struct_id, Vec::new()))
	}

	// lookup type id in env
	fn lookup(&self, id: SymbolId) -> Option<TyId> {
		// TODO: pass in span and fail here
		self.env.get(&id).copied()
	}

	fn get_parent(&self, mut id: TyId) -> TyId {
		// TODO: path compression
		loop {
			match &self.types[id] {
				TyNode::Node(parent) => id = *parent,
				TyNode::Ty(_) => return id,
			}
		}
	}

	// get concrete type
	fn get_type(&self, id: TyId) -> Ty {
		// TODO: path compression
		let parent_id = self.get_parent(id);
		match &self.types[parent_id] {
			TyNode::Ty(ty) => ty.clone(),
			TyNode::Node(_) => unreachable!(),
		}
	}

	// promote unbound type variables to free variables
	fn promote_free(&mut self, id: TyId) {
		let id = self.get_parent(id);
		match self.get_type(id) {
			Ty::Any | Ty::Err | Ty::Unit | Ty::Free | Ty::TyName(_) | Ty::Module(_) => (),
			Ty::TyVar => {
				let parent_id = self.get_parent(id);
				self.types[parent_id] = TyNode::Ty(Ty::Free);
			},
			Ty::Named(_, assoc) => {
				for a in assoc {
					self.promote_free(a);
				}
			},
			Ty::Fn(args, ret) => {
				for a in args {
					self.promote_free(a);
				}
				self.promote_free(ret);
			},
		}
	}
	fn instantiate(&mut self, id: TyId) -> TyId {
		self.instantiate_inner(id, &mut Vec::new())
	}

	// instantiate free variables to fresh type variables
	fn instantiate_inner(&mut self, id: TyId, subs: &mut Vec<(TyId, TyId)>) -> TyId {
		let ty = self.get_type(id);
		let parent_id = self.get_parent(id);
		match ty {
			Ty::Any | Ty::Err | Ty::Unit | Ty::TyVar | Ty::TyName(_) | Ty::Module(_) => parent_id,
			Ty::Free => {
				// keep track of which variables were already instantiated
				for &(old, new) in subs.iter() {
					if parent_id == old {
						return new;
					}
				}
				let t = self.new_ty(Ty::TyVar);
				subs.push((parent_id, t));
				t
			},
			Ty::Named(id, assoc) => {
				let mut new_assoc = Vec::new();
				for a in assoc {
					new_assoc.push(self.instantiate_inner(a, subs));
				}
				self.new_ty(Ty::Named(id, new_assoc))
			},
			Ty::Fn(args, ret) => {
				let mut new_args = Vec::new();
				for a in args {
					new_args.push(self.instantiate_inner(a, subs));
				}
				let new_ret = self.instantiate_inner(ret, subs);
				self.new_ty(Ty::Fn(new_args, new_ret))
			},
		}
	}

	fn occurs(&mut self, ty: Ty, id: TyId) -> Result<(), ()> {
		match ty {
			Ty::Any | Ty::Err | Ty::Unit | Ty::TyVar | Ty::TyName(_) | Ty::Module(_) | Ty::Free => Ok(()),
			Ty::Named(_, assoc) => {
				for a in assoc {
					if a == id {
						return Err(());
					}
					self.occurs(self.get_type(a), id)?;
				}
				Ok(())
			},
			Ty::Fn(args, ret) => {
				for a in args {
					if a == id {
						return Err(());
					}
					self.occurs(self.get_type(a), id)?;
				}
				if ret == id {
					return Err(());
				}
				self.occurs(self.get_type(ret), id)
			},
		}
	}

	fn unify(&mut self, a_id: TyId, b_id: TyId) -> Result<(), ()> {
		// println!("unify {}, {} ({}, {})", a_id, b_id, self.ty_to_string(a_id), self.ty_to_string(b_id));
		if a_id == b_id {
			Ok(())
		} else {
			match (self.get_type(a_id), self.get_type(b_id)) {
				// (_, Ty::Free) | (Ty::Free, _) => Err(()),
				(_, Ty::Err | Ty::Any) | (Ty::Err | Ty::Any, _) => Ok(()), // bail
				(ty, Ty::TyVar) => {
					self.occurs(ty, b_id)?;
					self.types[b_id] = TyNode::Node(a_id);
					Ok(())
				},
				(Ty::TyVar, ty) => {
					self.occurs(ty, a_id)?;
					self.types[a_id] = TyNode::Node(b_id);
					Ok(())
				},
				(a, b) if a == b => Ok(()),

				(Ty::Named(id_a, assoc_a), Ty::Named(id_b, assoc_b)) => {
					if id_a != id_b {
						return Err(());
					}
					if assoc_a.len() != assoc_b.len() {
						return Err(());
					}
					for (a, b) in zip(assoc_a, assoc_b) {
						self.unify(a, b)?;
					}
					Ok(())
				},
				(Ty::Fn(a_args, a_ret), Ty::Fn(b_args, b_ret)) => {
					if a_args.len() != b_args.len() {
						return Err(());
					}
					for (a, b) in zip(a_args, b_args) {
						self.unify(a, b)?;
					}
					self.unify(a_ret, b_ret)?;

					Ok(())
				},

				_ => Err(()),
			}
		}
	}

	fn unify_return(&mut self, current_pair: RetPair, new_pair: RetPair) -> RetPair {
		if let Some((new_ty, new_span)) = new_pair {
			match current_pair {
				Some((ty, prev_span)) => {
					if self.unify(ty, new_ty).is_ok() {
						Some((ty, new_span))
					} else {
						let msg = format!(
							"incompatible return types `{}` and `{}`",
							self.ty_to_string(new_ty),
							self.ty_to_string(ty)
						);
						format_err_f(&msg, new_span, self.input);
						let msg2 = "previous return value defined here:".to_string();
						format_note_f(&msg2, prev_span, self.input);
						self.errors.push(msg);
						Some((ERR_TY, new_span))
					}
				},
				None => new_pair,
			}
		} else {
			current_pair
		}
	}

	fn eval_module(&mut self, node: &mut ast::Module) -> Module {
		let mut module = Module { fields: FxHashMap::default() };

		for item in &mut node.items {
			match item {
				Item::FnDef(s) => {
					self.hoist_fn_def(s);
				},
				Item::StructDef(s) => {
					let ty = self.eval_struct_def(s);
					let name = self.symbol_table.get(s.name.id).name.clone();
					module.fields.insert(name, ty);
				},
				Item::Intrinsic(s) => {
					if let Some(p) = &mut s.property {
						let struct_id = s.name.id;
						let self_ty = self.new_named(struct_id);
						let new_ty = self.convert_ast_ty(&s.ty, Some(self_ty));
						self.structs
							.get_mut(&struct_id)
							.unwrap()
							.static_fields
							.insert(p.name.clone(), new_ty);
					} else {
						let new_ty = self.convert_ast_ty(&s.ty, None);
						self.new_def(s.name.id, new_ty);
					}
				},
				Item::Import(s) => match &mut s.kind {
					ImportKind::Glob => (),
					ImportKind::From(_) => (),
					ImportKind::Alias(name) => {
						let ty = self.new_ty(Ty::Module(s.file_id.unwrap()));
						self.new_def(name.id, ty);
					},
				},
				Item::InlineLua(_) => (),
			};
		}

		for item in &mut node.items {
			match item {
				Item::FnDef(s) => {
					if let Some(p) = &mut s.property {
						let struct_id = s.name.id;
						let self_ty = self.new_named(struct_id);
						let ty = self.eval_lambda(&mut s.body, p.span, Some(self_ty));
						self.structs
							.get_mut(&struct_id)
							.unwrap()
							.static_fields
							.insert(p.name.clone(), ty);
					} else {
						let ty = self.eval_fn_def(s);
						let name = self.symbol_table.get(s.name.id).name.clone();
						module.fields.insert(name, ty);
					}
				},
				Item::InlineLua(_) | Item::Intrinsic(_) | Item::StructDef(_) | Item::Import(_) => (),
			}
		}

		module
	}

	// Returns the type and span of any return statements
	// Otherwise return None
	// Bool indicates if the block always returns
	fn eval_block(&mut self, block: &mut Block) -> (RetPair, bool) {
		let mut current_pair: RetPair = None;
		for stat in &mut block.stats {
			match stat {
				Stat::Break => return (current_pair, false),
				Stat::Block(block) => {
					let (new_pair, ret) = self.eval_block(block);
					current_pair = self.unify_return(current_pair, new_pair);
					if ret {
						return (current_pair, true);
					}
				},
				Stat::Return(ret) => {
					let new_pair = if ret.exprs.is_empty() {
						Some((self.new_ty(Ty::Unit), ret.span))
					} else {
						// TODO: multiple return
						assert!(ret.exprs.len() == 1);
						Some((self.eval_expr(&mut ret.exprs[0]), ret.span))
					};
					current_pair = self.unify_return(current_pair, new_pair);
					return (current_pair, true);
				},
				Stat::IfBlock(if_block) => {
					// TODO: check bool
					self.eval_expr(&mut if_block.expr);
					let (if_pair, ret) = self.eval_block(&mut if_block.block);
					let mut all_return = ret;
					current_pair = self.unify_return(current_pair, if_pair);

					for elif in &mut if_block.elseif {
						self.eval_expr(&mut elif.expr);
						let (elif_pair, ret) = self.eval_block(&mut elif.block);
						all_return &= ret;
						current_pair = self.unify_return(current_pair, elif_pair);
					}
					if let Some(else_block) = &mut if_block.else_block {
						let (else_pair, ret) = self.eval_block(else_block);
						all_return &= ret;
						current_pair = self.unify_return(current_pair, else_pair);

						if all_return {
							return (current_pair, true);
						}
					}
				},
				Stat::WhileBlock(s) => {
					// TODO: check bool
					self.eval_expr(&mut s.expr);
					let (pair, _) = self.eval_block(&mut s.block);
					current_pair = self.unify_return(current_pair, pair);
				},
				Stat::ForBlock(s) => {
					let ty = self.eval_expr(&mut s.expr);
					assert!(s.names.len() == 1);

					// make new i: U
					let loop_var = self.new_ty(Ty::TyVar);
					self.new_def(s.names[0].id, loop_var);

					// unify iter[T] = iter[U]
					let loop_ty = self.new_ty(Ty::Named(ITER_SYM, vec![loop_var]));
					if self.unify(ty, loop_ty).is_err() {
						let msg = format!("expected iterator but found type `{}`", self.ty_to_string(ty),);
						format_err_f(&msg, s.expr.span, self.input);
						self.errors.push(msg);
					}

					let (pair, _) = self.eval_block(&mut s.block);
					current_pair = self.unify_return(current_pair, pair);
				},
				Stat::Call(s) => {
					let ret = self.eval_fn_call(s);
					// bail if we got an error from a function call
					if self.get_type(ret) == Ty::Err {
						let new_pair = Some((ret, s.expr.span));
						current_pair = self.unify_return(current_pair, new_pair);
						return (current_pair, true);
					}
				},
				Stat::Assignment(s) => self.eval_assignment(s),
				Stat::AssignOp(s) => self.eval_assign_op(s),
				Stat::Let(s) => self.eval_let(s),
			};
		}
		(current_pair, false)
	}

	fn eval_fn_params(&mut self, params: &[NameTy], self_ty: Option<TyId>) -> Vec<TyId> {
		let mut param_ty = Vec::new();

		for (i, p) in params.iter().enumerate() {
			let ty = if self.symbol_table.get(p.name.id).name == "self" {
				// TODO nicer err messages
				assert!(i == 0, "self must be first argument!");
				assert!(p.ty.is_none());
				self_ty.expect("can't use `self` here")
			} else {
				match &p.ty {
					Some(ty) => match ty {
						TyAst::SelfTy => self_ty.expect("can't use `self` here"),
						_ => self.convert_ast_ty(ty, None),
					},
					None => self.new_ty(Ty::TyVar),
				}
			};
			self.new_def(p.name.id, ty);
			param_ty.push(ty);
		}
		param_ty
	}

	fn eval_fn_body(&mut self, node: &mut FnBody, span: Span) -> (TyId, Span) {
		let (mut ret_pair, ret_all) = self.eval_block(&mut node.body);
		// There is an implied 'return ()' at the end of every body
		if !ret_all {
			let nil = self.new_ty(Ty::Unit);
			ret_pair = self.unify_return(ret_pair, Some((nil, span)));
		};
		ret_pair.unwrap()
	}

	fn hoist_fn_def(&mut self, node: &FnDef) {
		if node.property.is_none() {
			let param_ty = self.eval_fn_params(&node.body.params, None);
			let ret_id = match &node.body.ty {
				Some(ty) => self.convert_ast_ty(ty, None),
				None => self.new_ty(Ty::TyVar),
			};
			let fn_ty = self.new_ty(Ty::Fn(param_ty, ret_id));
			self.new_def(node.name.id, fn_ty);
		}
	}

	fn eval_fn_def(&mut self, node: &mut FnDef) -> TyId {
		let fn_id = self.lookup(node.name.id).unwrap();
		let fn_ty = self.get_type(fn_id);
		let Ty::Fn(_, ret_id) = fn_ty else {
			panic!("lookup failed");
		};

		let (ty, prev_span) = self.eval_fn_body(&mut node.body, node.name.span);

		if self.unify(ty, ret_id).is_err() {
			let msg =
				format!("expected return type `{}`, found `{}`", self.ty_to_string(ty), self.ty_to_string(ret_id));
			format_err_f(&msg, prev_span, self.input);
			self.errors.push(msg);
		};

		self.promote_free(fn_id);

		// println!("infer fn def: {}", self.ty_to_string(fn_id));
		fn_id
	}

	// span should refer to the place where the function is defined
	fn eval_lambda(&mut self, node: &mut FnBody, span: Span, self_ty: Option<TyId>) -> TyId {
		let param_ty = self.eval_fn_params(&node.params, self_ty);
		let ret_id = match &node.ty {
			Some(ty) => {
				if let TyAst::SelfTy = ty {
					self_ty.expect("can't use `self` here")
				} else {
					self.convert_ast_ty(ty, None)
				}
			},
			None => self.new_ty(Ty::TyVar),
		};
		let fn_ty = Ty::Fn(param_ty, ret_id);
		let fn_id = self.new_ty(fn_ty);

		let (ty, prev_span) = self.eval_fn_body(node, span);

		if self.unify(ty, ret_id).is_err() {
			let msg =
				format!("expected return type `{}`, found `{}`", self.ty_to_string(ty), self.ty_to_string(ret_id));
			format_err_f(&msg, prev_span, self.input);
			self.errors.push(msg);
		}
		self.promote_free(fn_id);
		// println!("infer lambda: {}", self.ty_to_string(fn_id));
		fn_id
	}

	// tranpose something that looks like instance.call(...) to Class.call(instance, ...)
	// TODO: this is a bit messy!
	// TODO: somehow check if the first parameter is actually "self" before doing this
	fn transpose_call(&mut self, c: &mut Call) {
		if let ExprKind::SuffixExpr(expr, s) = &mut c.expr.kind {
			let mut ty = self.eval_expr(expr);
			let last_suffix = s.pop().expect("should have at least one suffix");
			for suffix in s.iter_mut() {
				ty = self.eval_suffix(ty, expr.span, suffix);
			}
			if let Ty::Named(id, _assoc) = self.get_type(ty) {
				if let Suffix::Property(p) = &last_suffix {
					if self.structs[&id].fields.get(&p.name).is_none() {
						let inst_expr = std::mem::replace(
							expr,
							Box::new(Expr { span: expr.span, kind: ExprKind::Name(Name { span: expr.span, id }) }),
						);
						let inst_s = std::mem::replace(s, vec![last_suffix]);
						c.args.insert(
							0,
							if inst_s.is_empty() {
								*inst_expr
							} else {
								Expr { span: expr.span, kind: ExprKind::SuffixExpr(inst_expr, inst_s) }
							},
						);
						return;
					}
				}
			}
			s.push(last_suffix);
		}
	}

	fn eval_fn_call(&mut self, c: &mut Call) -> TyId {
		self.transpose_call(c);

		let fn_ty_id = self.eval_expr(&mut c.expr);
		// println!("fn_ty_id {}", self.ty_to_string(fn_ty_id));
		let fn_ty = self.instantiate(fn_ty_id);
		match self.get_type(fn_ty) {
			Ty::Fn(params, ret_ty) => {
				if params.len() != c.args.len() {
					let msg = format!("function takes {} argument(s), {} supplied", params.len(), c.args.len());
					format_err_f(&msg, c.expr.span, self.input);
					self.errors.push(msg);
				} else {
					assert!(c.named_args.is_empty(), "named arguments not implemented yet");
					for (p, a) in zip(params, c.args.iter_mut()) {
						let arg_ty = self.eval_expr(a);

						if self.unify(arg_ty, p).is_err() {
							let msg = format!(
								"expected argument type `{}`, found `{}`",
								self.ty_to_string(p),
								self.ty_to_string(arg_ty)
							);
							format_err_f(&msg, a.span, self.input);
							self.errors.push(msg);
						}
					}
				}
				ret_ty
			},
			// constructor
			Ty::TyName(struct_id) => {
				// TODO: instantiate does not take care of generics on struct

				if self.structs[&struct_id].primitive {
					let msg = "primitive type does not have a constructor".to_string();
					format_err_f(&msg, c.expr.span, self.input);
					self.errors.push(msg);
					return ERR_TY;
				}
				if !c.args.is_empty() {
					let msg = "constructor can only contain named arguments".to_string();
					format_err_f(&msg, c.args[0].span, self.input);
					self.errors.push(msg);
					return ERR_TY;
				}
				for k in &self.structs[&struct_id].required_fields {
					let mut check = false;
					for a in &mut c.named_args {
						if &a.name == k {
							check = true;
							break;
						}
					}
					if !check {
						let msg = format!("missing required field `{k}`");
						format_err_f(&msg, c.expr.span, self.input);
						self.errors.push(msg);
						return ERR_TY;
					}
				}
				for a in &mut c.named_args {
					if let Some(&p) = self.structs[&struct_id].fields.get(&a.name) {
						let arg_ty = self.eval_expr(&mut a.expr);
						if self.unify(arg_ty, p).is_err() {
							let msg = format!(
								"field has type `{}`, found `{}`",
								self.ty_to_string(p),
								self.ty_to_string(arg_ty)
							);
							format_err_f(&msg, a.span, self.input);
							self.errors.push(msg);
							return ERR_TY;
						}
					} else {
						let msg = format!("struct does not have a field named `{}`", a.name);
						format_err_f(&msg, a.span, self.input);
						self.errors.push(msg);
						return ERR_TY;
					}
				}
				self.new_named(struct_id)
			},
			Ty::Err => ERR_TY,
			_ => {
				let msg = format!("type `{}` is not callable", self.ty_to_string(fn_ty));
				format_err_f(&msg, c.expr.span, self.input);
				self.errors.push(msg);
				ERR_TY
			},
		}
	}

	fn eval_expr(&mut self, expr: &mut Expr) -> TyId {
		match &mut expr.kind {
			ExprKind::BinExpr(e) => {
				let lhs = self.eval_expr(&mut e.lhs);
				let rhs = self.eval_expr(&mut e.rhs);
				self.eval_bin_op(&e.op, lhs, rhs, expr.span)
			},
			ExprKind::UnExpr(e) => {
				let ty = self.eval_expr(&mut e.expr);
				self.eval_un_op(&e.op, ty, expr.span)
			},
			ExprKind::Array(array) => {
				let mut ty = self.new_ty(Ty::TyVar);
				for e in array {
					let new_ty = self.eval_expr(e);
					ty = if self.unify(ty, new_ty).is_ok() {
						new_ty
					} else {
						let msg = format!(
							"incompatible types in array: `{}` and `{}`",
							self.ty_to_string(new_ty),
							self.ty_to_string(ty)
						);
						format_err_f(&msg, e.span, self.input);
						self.errors.push(msg);
						return ERR_TY;
					}
				}
				self.new_ty(Ty::Named(ARRAY_SYM, vec![ty]))
			},
			ExprKind::Name(e) => {
				let ty_opt = self.lookup(e.id);
				if let Some(ty) = ty_opt {
					ty
				} else {
					let msg = format!("(compiler error) couldn't find type for `{}`", expr.span.as_str_f(self.input));
					format_err_f(&msg, e.span, self.input);
					panic!("{}", &msg);
				}
			},
			ExprKind::SuffixExpr(e, s) => self.eval_suffix_expr(e, s),
			ExprKind::Literal(e) => self.eval_literal(e),
			ExprKind::Call(e) => self.eval_fn_call(e),
			ExprKind::Expr(e) => self.eval_expr(e),
			ExprKind::Lambda(e) => self.eval_lambda(e, expr.span, None),
		}
	}

	fn eval_struct_def(&mut self, node: &mut StructDef) -> TyId {
		let id = node.name.id;
		let ty_id = self.new_struct(id, false);

		for f in &mut node.table.fields {
			let (k, v) = match &mut f.kind {
				FieldKind::Empty => {
					self.structs
						.get_mut(&id)
						.unwrap()
						.required_fields
						.push(f.field.property.name.clone());

					if let Some(ty) = &f.field.ty {
						let new_ty = self.convert_ast_ty(ty, None);
						(f.field.property.name.clone(), new_ty)
					} else {
						let msg = "need type annotation here";
						format_err_f(msg, f.field.property.span, self.input);
						// self.errors.push(msg);
						panic!("{}", &msg);
					}
				},
				FieldKind::Assign(a) => {
					if !a.is_const() {
						let msg = "default field must be a constant expression".to_string();
						format_err_f(&msg, a.span, self.input);
						self.errors.push(msg);
					}
					let mut expr_ty = self.eval_expr(a);
					if let Some(ty) = &f.field.ty {
						let ty_id = self.convert_ast_ty(ty, None);
						if self.unify(ty_id, expr_ty).is_err() {
							let msg = format!(
								"incompatible types `{}` and `{}`",
								self.ty_to_string(ty_id),
								self.ty_to_string(expr_ty)
							);
							format_err_f(&msg, a.span, self.input);
							self.errors.push(msg);
						}
						expr_ty = ty_id;
					}
					(f.field.property.name.clone(), expr_ty)
				},
			};
			self.structs.get_mut(&id).unwrap().fields.insert(k, v);
		}
		ty_id
	}

	fn get_field(&mut self, struct_id: SymbolId, p: &mut Property) -> TyId {
		if let Some(p_id) = self.structs[&struct_id].fields.get(&p.name) {
			*p_id
		} else {
			let msg = format!("no field `{}`", p.name);
			format_err_f(&msg, p.span, self.input);
			self.errors.push(msg);
			ERR_TY
		}
	}

	fn get_static(&mut self, struct_id: SymbolId, p: &mut Property) -> TyId {
		if let Some(p_id) = self.structs[&struct_id].static_fields.get(&p.name) {
			*p_id
		} else {
			let msg = format!("no static field `{}`", p.name);
			format_err_f(&msg, p.span, self.input);
			self.errors.push(msg);
			ERR_TY
		}
	}

	fn get_module_field(&mut self, file_id: FileId, p: &mut Property) -> TyId {
		if let Some(p_id) = self.modules[&file_id].fields.get(&p.name) {
			*p_id
		} else {
			let msg = format!("module does not have field `{}`", p.name);
			format_err_f(&msg, p.span, self.input);
			self.errors.push(msg);
			ERR_TY
		}
	}

	fn eval_suffix_expr(&mut self, expr: &mut Expr, s: &mut Vec<Suffix>) -> TyId {
		let mut ty = self.eval_expr(expr);
		for suffix in s {
			ty = self.eval_suffix(ty, expr.span, suffix);
		}
		ty
	}

	fn eval_suffix(&mut self, expr_ty: TyId, expr_span: Span, suffix: &mut Suffix) -> TyId {
		match suffix {
			Suffix::Property(p) => match self.get_type(expr_ty) {
				Ty::Named(name, _assoc) => self.get_field(name, p),
				Ty::TyName(name) => self.get_static(name, p),
				Ty::Module(file_id) => self.get_module_field(file_id, p),
				Ty::Err => ERR_TY,
				_ => {
					let msg = format!("type `{}` does not allow indexing with `.`", self.ty_to_string(expr_ty));
					format_err_f(&msg, expr_span, self.input);
					self.errors.push(msg);
					ERR_TY
				},
			},
			Suffix::Index(e) => {
				let ty = match self.get_type(expr_ty) {
					Ty::Named(id, assoc) if id == ARRAY_SYM => {
						assert!(assoc.len() == 1);
						assoc[0]
					},
					Ty::Err => ERR_TY,
					_ => {
						let msg = format!("can't index type `{}`", self.ty_to_string(expr_ty));
						format_err_f(&msg, expr_span, self.input);
						self.errors.push(msg);
						return ERR_TY;
					},
				};
				let index_ty = self.eval_expr(e);
				let ty_int = self.new_ty(Ty::Named(INT_SYM, Vec::new()));
				if self.unify(index_ty, ty_int).is_err() {
					let msg = format!("index must be type `int` but found `{}`", self.ty_to_string(index_ty));
					format_err_f(&msg, e.span, self.input);
					self.errors.push(msg);
				}
				ty
			},
		}
	}

	fn eval_let(&mut self, node: &mut Let) {
		assert!(node.exprs.len() == node.names.len());

		let mut rhs = Vec::new();
		for e in &mut node.exprs {
			rhs.push(self.eval_expr(e));
		}
		for (n, rhs_ty) in zip(&node.names, rhs) {
			// check if annotation fits
			if let Some(ty) = &n.ty {
				let ty_id = self.convert_ast_ty(ty, None);
				if self.unify(rhs_ty, ty_id).is_err() {
					let msg = format!(
						"type error, assigning `{}` to `{}`",
						self.ty_to_string(rhs_ty),
						self.ty_to_string(ty_id)
					);
					format_err_f(&msg, node.span, self.input);
					self.errors.push(msg);
				}
				let new_ty = self.convert_ast_ty(ty, None);
				self.new_def(n.name.id, new_ty);
			} else {
				self.new_def(n.name.id, rhs_ty);
			}
		}
	}

	fn eval_lvalue(&mut self, var: &mut Expr) -> TyId {
		match &mut var.kind {
			ExprKind::Name(n) => self.lookup(n.id).expect("lookup failed"),
			ExprKind::SuffixExpr(e, s) => self.eval_suffix_expr(e, s),
			_ => unreachable!(),
		}
	}

	fn eval_assignment(&mut self, node: &mut Assignment) {
		assert!(node.exprs.len() == node.vars.len());

		let mut rhs = Vec::new();
		for e in &mut node.exprs {
			rhs.push(self.eval_expr(e));
		}
		for (var, rhs_ty) in zip(&mut node.vars, rhs) {
			let ty = self.eval_lvalue(var);
			if self.unify(rhs_ty, ty).is_err() {
				let msg =
					format!("type error, assigning `{}` to `{}`", self.ty_to_string(rhs_ty), self.ty_to_string(ty));
				format_err_f(&msg, node.span, self.input);
				self.errors.push(msg);
			};
		}
	}

	fn eval_assign_op(&mut self, node: &mut AssignOp) {
		let rhs = self.eval_expr(&mut node.expr);
		let lhs = self.eval_lvalue(&mut node.var);

		let new_lhs = self.eval_bin_op(&node.op, lhs, rhs, node.span);

		if self.unify(new_lhs, lhs).is_err() {
			let msg = format!("can not assign `{}` to `{}`", self.ty_to_string(new_lhs), self.ty_to_string(lhs));
			format_err_f(&msg, node.span, self.input);
			self.errors.push(msg);
		}
	}

	fn eval_un_op(&mut self, op: &UnOp, id: TyId, span: Span) -> TyId {
		match op {
			UnOp::Neg => {
				if let Ty::Named(name, _) = self.get_type(id) {
					if let Some(&op_impl) = self.structs[&name].static_fields.get("__neg") {
						let ret = self.new_ty(Ty::TyVar);
						let expect = self.new_ty(Ty::Fn(vec![id], ret));
						assert!(self.unify(op_impl, expect).is_ok());
						return ret;
					}
				};
			},
			UnOp::Not => {
				let ty_bool = self.new_ty(Ty::Named(BOOL_SYM, Vec::new()));
				if self.unify(id, ty_bool).is_err() {
					let msg = format!("expected `bool` but found `{}`", self.ty_to_string(id));
					format_err_f(&msg, span, self.input);
					self.errors.push(msg);
				}
				return ty_bool;
			},
		}
		let msg = format!("operator `{op}` cannot by applied to `{}`", self.ty_to_string(id));
		format_err_f(&msg, span, self.input);
		self.errors.push(msg);
		ERR_TY
	}

	fn eval_bin_op(&mut self, op: &BinOp, lhs: TyId, rhs: TyId, span: Span) -> TyId {
		if self.unify(lhs, rhs).is_err() {
			let msg = format!(
				"operator `{op}` cannot by applied to `{}` and `{}`",
				self.ty_to_string(lhs),
				self.ty_to_string(rhs)
			);
			format_err_f(&msg, span, self.input);
			self.errors.push(msg);
			return ERR_TY;
		}
		match op {
			BinOp::And | BinOp::Or => {
				// boolean ops are not overloadable
				let ty_bool = self.new_ty(Ty::Named(BOOL_SYM, Vec::new()));
				if self.unify(lhs, ty_bool).is_err() {
					let msg = format!("expected `bool` but found `{}`", self.ty_to_string(lhs));
					format_err_f(&msg, span, self.input);
					self.errors.push(msg);
				}
				return ty_bool;
			},
			BinOp::Eq | BinOp::Neq => {
				// eq always works, regardless of overloading
				// TODO: if we do overload, we need to check that the signature is ok!
				return self.new_ty(Ty::Named(BOOL_SYM, Vec::new()));
			},
			op => {
				if let Some(op_impl_name) = op.overload_name() {
					if let Ty::Named(name, _) = self.get_type(lhs) {
						if let Some(&op_impl) = self.structs[&name].static_fields.get(op_impl_name) {
							let ret = self.new_ty(Ty::TyVar);
							let expect = self.new_ty(Ty::Fn(vec![lhs, rhs], ret));
							assert!(self.unify(op_impl, expect).is_ok());
							return ret;
						}
					};
				}
			},
		}

		let msg = format!("failed to infer if we can apply `{op}` here");
		format_err_f(&msg, span, self.input);
		self.errors.push(msg);
		ERR_TY
	}

	fn eval_literal(&mut self, l: &Literal) -> TyId {
		match l {
			Literal::Nil => {
				// let ty = self.new_ty(Ty::TyVar);
				// self.new_ty(Ty::Maybe(ty))
				todo!()
			},
			Literal::Num(_) => self.new_ty(Ty::Named(NUM_SYM, Vec::new())),
			Literal::Int(_) => self.new_ty(Ty::Named(INT_SYM, Vec::new())),
			Literal::Str(_) => self.new_ty(Ty::Named(STR_SYM, Vec::new())),
			Literal::Bool(_) => self.new_ty(Ty::Named(BOOL_SYM, Vec::new())),
		}
	}
}
