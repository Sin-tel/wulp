use crate::ast::*;
use crate::span::Span;
use crate::span::{format_err_f, format_note_f, InputFile};
use crate::std_lib::GLOBALS;
use crate::symbol::SymbolId;
use crate::symbol::SymbolTable;
use crate::ty::*;
use anyhow::anyhow;
use anyhow::Result;
use rustc_hash::FxHashMap;
use std::iter::zip;

type TableId = usize;

#[derive(Debug)]
struct Struct {
	fields: FxHashMap<String, TyId>,
	symbol_id: SymbolId,
}

const ERR_TY: TyId = 0;

type RetPair = Option<(TyId, Span)>;

#[derive(Debug)]
pub struct TypeCheck<'a> {
	input: &'a [InputFile],
	errors: Vec<String>,
	env: FxHashMap<SymbolId, TyId>, // TODO: this can be Vec<Option>
	types: Vec<TyNode>,
	structs: Vec<Struct>,
	symbol_table: &'a SymbolTable,
}

impl<'a> TypeCheck<'a> {
	pub fn check(file: &mut File, input: &'a [InputFile], symbol_table: &'a SymbolTable) -> Result<()> {
		let mut this = Self {
			input,
			errors: Vec::new(),
			types: Vec::new(),
			env: FxHashMap::default(),
			structs: Vec::new(),
			symbol_table,
		};

		this.types.push(TyNode::Ty(Ty::Bottom));
		assert!(this.get_type(ERR_TY) == Ty::Bottom);

		for item in GLOBALS.iter() {
			let mut param_ty = Vec::new();
			for p in &item.param_ty {
				param_ty.push(this.new_ty(p.clone()));
			}
			let ret_ty = this.new_ty(item.ret_ty.clone());
			this.new_def(item.id, Ty::Fn(param_ty, ret_ty));
		}

		let (ret, _) = this.eval_block(&mut file.block);

		if let Some((ret_ty, _)) = ret {
			println!("Main return: {}", this.ty_to_string(ret_ty));
		}
		// println!("----- types ");
		// for (i, v) in this.types.iter().enumerate() {
		// 	println!("{i}: {:?} {} {}", v, this.get_parent(i), this.ty_to_string(i));
		// }
		println!("----- env ");
		for (i, s) in symbol_table.symbols.iter().enumerate().skip(1) {
			if let Some(id) = this.lookup(i) {
				println!("`{}`: {}", s.name, this.ty_to_string(id));
			}
		}

		match this.errors.last() {
			Some(err) => Err(anyhow!("{}", err)),
			None => Ok(()),
		}
	}
	fn ty_to_string(&self, id: TyId) -> String {
		let id = self.get_parent(id);
		match self.get_type(id) {
			Ty::Any => "Any".to_string(),
			Ty::Bottom => "Bottom".to_string(),
			Ty::Nil => "nil".to_string(),
			Ty::Bool => "bool".to_string(),
			Ty::Str => "str".to_string(),
			Ty::Num => "num".to_string(),
			Ty::Int => "int".to_string(),
			Ty::TyVar => format!("T{id}?"),
			Ty::Free => format!("T{id}"),
			Ty::Instance(table_id) => format!("instance_{table_id}"),
			Ty::Table(table_id) => {
				let mut s = String::new();
				for (p, ty) in &self.structs[table_id].fields {
					s.push_str(&format!("{} = {}, ", p, self.ty_to_string(*ty)));
				}
				format!("table_{table_id} {{ {s} }}")
			},
			Ty::Array(ty) => format!("[{}]", self.ty_to_string(ty)),
			Ty::Maybe(ty) => format!("maybe({})", self.ty_to_string(ty)),
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
	fn to_ty(&self, ty_ast: &TyAst) -> Ty {
		match ty_ast {
			TyAst::Any => Ty::Any,
			TyAst::Nil => Ty::Nil,
			TyAst::Bool => Ty::Bool,
			TyAst::Str => Ty::Str,
			TyAst::Num => Ty::Num,
			TyAst::Int => Ty::Int,
			TyAst::SelfTy => unreachable!(),
			_ => todo!(),
		}
	}

	// add symbol to env with specified type
	fn new_def(&mut self, id: SymbolId, ty: Ty) -> TyId {
		let ty_id = self.new_ty(ty);
		self.env.insert(id, ty_id);
		ty_id
	}

	// new type variable
	fn new_ty(&mut self, ty: Ty) -> TyId {
		let ty_id = self.types.len();
		self.types.push(TyNode::Ty(ty));
		ty_id
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
			_ => unreachable!(),
		}
	}

	// promote unbound type variables to free variables
	fn promote_free(&mut self, id: TyId) {
		let id = self.get_parent(id);
		match self.get_type(id) {
			Ty::Any
			| Ty::Bottom
			| Ty::Nil
			| Ty::Bool
			| Ty::Str
			| Ty::Num
			| Ty::Int
			| Ty::Free
			| Ty::Instance(_)
			| Ty::Table(_) => (),
			Ty::TyVar => {
				let parent_id = self.get_parent(id);
				self.types[parent_id] = TyNode::Ty(Ty::Free);
			},
			Ty::Array(t) | Ty::Maybe(t) => self.promote_free(t),
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

	// instantiate a type with free variables
	fn instantiate_inner(&mut self, id: TyId, subs: &mut Vec<(TyId, TyId)>) -> TyId {
		let ty = self.get_type(id);
		let parent_id = self.get_parent(id);
		match ty {
			Ty::Any | Ty::Bottom | Ty::Nil | Ty::Bool | Ty::Str | Ty::Num | Ty::Int | Ty::TyVar | Ty::Instance(_) => {
				parent_id
			},
			Ty::Table(table_id) => self.new_ty(Ty::Instance(table_id)),
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
			Ty::Array(t) | Ty::Maybe(t) => self.instantiate_inner(t, subs),
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
			Ty::Any
			| Ty::Bottom
			| Ty::Nil
			| Ty::Bool
			| Ty::Str
			| Ty::Num
			| Ty::Int
			| Ty::TyVar
			| Ty::Instance(_)
			| Ty::Table(_)
			| Ty::Free => Ok(()),
			Ty::Array(t_id) | Ty::Maybe(t_id) => {
				if t_id == id {
					return Err(());
				}
				self.occurs(self.get_type(t_id), id)
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
		println!(
			"unify {}, {} ({}, {})",
			a_id,
			b_id,
			self.ty_to_string(a_id),
			self.ty_to_string(b_id)
		);
		if a_id == b_id {
			Ok(())
		} else {
			match (self.get_type(a_id), self.get_type(b_id)) {
				(_, Ty::Free) | (Ty::Free, _) => panic!("Can not unify free type variables"),
				(_, Ty::Bottom) | (Ty::Bottom, _) | (_, Ty::Any) | (Ty::Any, _) => Ok(()), // bail
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
				(Ty::Array(a), Ty::Array(b)) | (Ty::Maybe(a), Ty::Maybe(b)) => self.unify(a, b),

				(_, Ty::Fn(_, _)) | (Ty::Fn(_, _), _) => todo!(),

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
							"Incompatible return types `{}` and `{}`.",
							self.ty_to_string(new_ty),
							self.ty_to_string(ty)
						);
						format_err_f(&msg, new_span, self.input);
						let msg2 = "Previous return value defined here:".to_string();
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

	// Returns the type and span of any return statements
	// Otherwise return None
	// Bool indicates if the block always returns
	fn eval_block(&mut self, block: &mut Block) -> (RetPair, bool) {
		for stat in &block.stats {
			if let Stat::FnDef(s) = stat {
				self.hoist_fn_def(s);
			}
		}
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
						Some((self.new_ty(Ty::Nil), ret.span))
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
					let loop_var = self.new_def(s.names[0].id, Ty::TyVar);

					// unify iter: [T], [U]
					// for now, only iterate on arrays
					let loop_ty = self.new_ty(Ty::Array(loop_var));
					if self.unify(ty, loop_ty).is_err() {
						panic!("error: currently iteration is only supported on arrays.");
					}

					let (pair, _) = self.eval_block(&mut s.block);
					current_pair = self.unify_return(current_pair, pair);
				},
				Stat::Call(s) => {
					self.eval_fn_call(s);
				},
				Stat::Assignment(s) => self.eval_assignment(s),
				Stat::AssignOp(s) => self.eval_assign_op(s),
				Stat::Let(s) => self.eval_let(s),
				Stat::FnDef(s) => self.eval_fn_def(s),
				Stat::StructDef(s) => self.eval_struct_def(s),
				Stat::Import(_s) => {
					todo!();
					// let table = self.eval_table(&mut s.module);
					// self.new_def(s.alias.id, self.get_type(table));
				},
			};
		}
		(current_pair, false)
	}

	fn eval_fn_params(&mut self, params: &[NameTy], self_ty: Option<Ty>) -> Vec<TyId> {
		let mut param_ty = Vec::new();

		for (i, p) in params.iter().enumerate() {
			let id = if self.symbol_table.get(p.name.id).name == "self" {
				// TODO nicer err messages
				assert!(i == 0, "self must be first argument!");
				assert!(p.ty.is_none());
				// self.lookup(p.name.id).expect("Self argument could not be resolved")
				if let Some(ref ty) = self_ty {
					self.new_def(p.name.id, ty.clone())
				} else {
					panic!("Can not use `self` here")
				}
			} else {
				let ty = match &p.ty {
					Some(ty) => match ty {
						TyAst::SelfTy => self_ty.clone().expect("can't use `self` here"),
						_ => self.to_ty(ty),
					},
					None => Ty::TyVar,
				};
				self.new_def(p.name.id, ty)
			};
			param_ty.push(id);
		}
		param_ty
	}

	fn eval_fn_body(&mut self, node: &mut FnBody, span: Span) -> (TyId, Span) {
		let (mut ret_pair, ret_all) = self.eval_block(&mut node.body);
		// There is an implied 'return nil' at the end of every body
		if !ret_all {
			let nil = self.new_ty(Ty::Nil);
			ret_pair = self.unify_return(ret_pair, Some((nil, span)));
		};
		ret_pair.unwrap()
	}

	fn hoist_fn_def(&mut self, node: &FnDef) {
		let param_ty = self.eval_fn_params(&node.body.params, None);
		let ty = match &node.body.ty {
			Some(ty) => self.to_ty(ty),
			None => Ty::TyVar,
		};
		let ret_id = self.new_ty(ty);
		let fn_ty = Ty::Fn(param_ty, ret_id);
		self.new_def(node.name.id, fn_ty);
	}

	fn eval_fn_def(&mut self, node: &mut FnDef) {
		let fn_id = self.lookup(node.name.id).unwrap();
		let fn_ty = self.get_type(fn_id);
		let ret_id = if let Ty::Fn(_, ret_id) = fn_ty {
			ret_id
		} else {
			panic!("lookup failed");
		};

		let (ty, prev_span) = self.eval_fn_body(&mut node.body, node.name.span);

		if self.unify(ty, ret_id).is_err() {
			let msg = format!(
				"Expected return type `{}`, found `{}`.",
				self.ty_to_string(ty),
				self.ty_to_string(ret_id)
			);
			format_err_f(&msg, prev_span, self.input);
			self.errors.push(msg);
		};

		self.promote_free(fn_id);

		println!("infer fn def: {}", self.ty_to_string(fn_id));
	}

	// span should refer to the place where the function is defined
	fn eval_lambda(&mut self, node: &mut FnBody, span: Span, self_ty: Option<Ty>) -> TyId {
		let param_ty = self.eval_fn_params(&node.params, self_ty.clone());
		let ty = match &node.ty {
			Some(ty) => match ty {
				TyAst::SelfTy => self_ty.expect("can't use `self` here"),
				_ => self.to_ty(ty),
			},
			None => Ty::TyVar,
		};
		let ret_id = self.new_ty(ty);
		let fn_ty = Ty::Fn(param_ty, ret_id);
		let fn_id = self.new_ty(fn_ty);

		let (ty, prev_span) = self.eval_fn_body(node, span); // !

		if self.unify(ty, ret_id).is_err() {
			let msg = format!(
				"Expected return type `{}`, found `{}`.",
				self.ty_to_string(ty),
				self.ty_to_string(ret_id)
			);
			format_err_f(&msg, prev_span, self.input);
			self.errors.push(msg);
		}
		self.promote_free(fn_id);
		println!("infer lambda: {}", self.ty_to_string(fn_id));
		fn_id
	}

	fn eval_fn_call(&mut self, c: &mut Call) -> TyId {
		if let ExprKind::SuffixExpr(expr, s) = &mut c.expr.kind {
			let mut ty = self.eval_expr(expr);
			let last_suffix = s.pop().expect("should have at least one suffix");
			for suffix in s.iter_mut() {
				ty = self.eval_suffix(ty, expr.span, suffix);
			}
			if let Ty::Instance(table_id) = self.get_type(ty) {
				// fix up the AST for method call
				let inst_expr = std::mem::replace(
					expr,
					Box::new(Expr {
						span: expr.span,
						kind: ExprKind::Name(Name {
							span: expr.span,
							id: self.structs[table_id].symbol_id,
						}),
					}),
				);
				let inst_s = std::mem::replace(s, vec![last_suffix]);
				c.args.insert(
					0,
					if inst_s.is_empty() {
						*inst_expr
					} else {
						Expr {
							span: expr.span,
							kind: ExprKind::SuffixExpr(inst_expr, inst_s),
						}
					},
				);
			} else {
				s.push(last_suffix);
			}
		}

		let fn_ty_id = self.eval_expr(&mut c.expr);
		let fn_ty = self.instantiate(fn_ty_id);
		match self.get_type(fn_ty) {
			Ty::Fn(params, ret_ty) => {
				// TODO: get rid of hardcoded "print" here
				if params.len() != c.args.len() && c.expr.span.as_str_f(self.input) != "print" {
					let msg = format!(
						"Function takes {} argument(s), {} supplied.",
						params.len(),
						c.args.len()
					);
					format_err_f(&msg, c.expr.span, self.input);
					self.errors.push(msg);
				} else {
					assert!(c.named_args.is_empty(), "Named arguments not implemented yet!");
					for (p, a) in zip(params, c.args.iter_mut()) {
						let arg_ty = self.eval_expr(a);

						if self.unify(arg_ty, p).is_err() {
							let msg = format!(
								"Expected argument type `{}`, found `{}`",
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
			Ty::Instance(table_id) => {
				if !c.args.is_empty() {
					let msg = "constructor can only contain named arguments".to_string();
					format_err_f(&msg, c.args[0].span, self.input);
					self.errors.push(msg);
				}
				for a in &mut c.named_args {
					if let Some(&p) = self.structs[table_id].fields.get(&a.name) {
						let arg_ty = self.eval_expr(&mut a.expr);
						if self.unify(arg_ty, p).is_err() {
							let msg = format!(
								"Field has type `{}`, found `{}`",
								self.ty_to_string(p),
								self.ty_to_string(arg_ty)
							);
							format_err_f(&msg, a.span, self.input);
							self.errors.push(msg);
						}
					} else {
						let msg = "field doesn't exist".to_string();
						format_err_f(&msg, a.span, self.input);
						self.errors.push(msg);
					}
				}
				fn_ty
			},

			_ => {
				let msg = format!("Type `{}` is not callable.", self.ty_to_string(fn_ty));
				format_err_f(&msg, c.expr.span, self.input);
				self.errors.push(msg);
				ERR_TY
			},
		}
	}

	fn is_const(&mut self, expr: &Expr) -> bool {
		match &expr.kind {
			ExprKind::BinExpr(e) => self.is_const(&e.lhs) && self.is_const(&e.rhs),
			ExprKind::UnExpr(e) => self.is_const(&e.expr),
			ExprKind::Array(array) => {
				let mut c = true;
				for e in array {
					c &= self.is_const(e);
				}
				c
			},
			ExprKind::Name(_) => false,
			ExprKind::SuffixExpr(_, _) => false,
			ExprKind::Literal(_) => true,
			ExprKind::Call(_) => false,
			ExprKind::Expr(e) => self.is_const(e),
			ExprKind::Lambda(_) => true,
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
							"Incompatible types in array: `{}` and `{}`.",
							self.ty_to_string(new_ty),
							self.ty_to_string(ty)
						);
						format_err_f(&msg, e.span, self.input);
						self.errors.push(msg);
						return ERR_TY;
					}
				}
				self.new_ty(Ty::Array(ty))
			},
			ExprKind::Name(e) => {
				let ty_opt = self.lookup(e.id);
				if let Some(ty) = ty_opt {
					ty
				} else {
					let msg = format!(
						"(compiler error) couldn't find type for `{}`",
						expr.span.as_str_f(self.input)
					);
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

	fn eval_struct_def(&mut self, node: &mut StructDef) {
		let table_id = self.new_struct(node.name.id);
		self.new_def(node.name.id, Ty::Table(table_id));
		for f in &mut node.table.fields {
			let (k, v) = match f {
				Field::Assign(p, a) => {
					if !self.is_const(a) {
						let msg = "Default field must be a constant expression".to_string();
						format_err_f(&msg, a.span, self.input);
						self.errors.push(msg);
					}
					(p.name.clone(), self.eval_expr(a))
				},
				Field::Fn(p, f) => (
					p.name.clone(),
					self.eval_lambda(f, p.span, Some(Ty::Instance(table_id))),
				),
			};
			self.structs[table_id].fields.insert(k, v);
		}
	}

	fn new_struct(&mut self, symbol_id: SymbolId) -> TableId {
		let table = Struct {
			fields: FxHashMap::default(),
			symbol_id,
		};
		let table_id = self.structs.len();
		self.structs.push(table);
		table_id
	}

	fn get_property(&mut self, table: TableId, p: &mut Property) -> Option<TyId> {
		self.structs[table].fields.get(&p.name).copied()
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
				Ty::Table(table) | Ty::Instance(table) => {
					if let Some(p_id) = self.get_property(table, p) {
						p_id
					} else {
						let msg = format!("Table doesn't have property `{}`.", p.name);
						format_err_f(&msg, p.span, self.input);
						self.errors.push(msg);
						ERR_TY
					}
				},
				_ => {
					let msg = format!("Can not get property on type `{}`.", self.ty_to_string(expr_ty));
					format_err_f(&msg, expr_span, self.input);
					self.errors.push(msg);
					ERR_TY
				},
			},
			Suffix::Index(e) => {
				let ty = if let Ty::Array(inner_ty) = self.get_type(expr_ty) {
					inner_ty
				} else {
					let msg = format!("Can not index type `{}`.", self.ty_to_string(expr_ty));
					format_err_f(&msg, expr_span, self.input);
					self.errors.push(msg);
					return ERR_TY;
				};
				let index_ty = self.eval_expr(e);
				let ty_int = self.new_ty(Ty::Int);
				if self.unify(index_ty, ty_int).is_err() {
					let msg = format!("Index must be type `int` but found `{}`.", self.ty_to_string(index_ty));
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
				let ty_id = self.new_ty(self.to_ty(ty));
				if self.unify(rhs_ty, ty_id).is_err() {
					let msg = format!(
						"Type error, assigning `{}` to `{}`.",
						self.ty_to_string(rhs_ty),
						self.ty_to_string(ty_id)
					);
					format_err_f(&msg, node.span, self.input);
					self.errors.push(msg);
				}
				self.new_def(n.name.id, self.to_ty(ty));
			} else {
				self.new_def(n.name.id, self.get_type(rhs_ty));
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
				let msg = format!(
					"Type error, assigning `{}` to `{}`.",
					self.ty_to_string(rhs_ty),
					self.ty_to_string(ty)
				);
				format_err_f(&msg, node.span, self.input);
				self.errors.push(msg);
			};
		}
	}

	fn eval_assign_op(&mut self, node: &mut AssignOp) {
		// TODO: refactor to use binop code
		let rhs = self.eval_expr(&mut node.expr);
		let lhs = self.eval_lvalue(&mut node.var);

		let new_lhs = self.eval_bin_op(&mut node.op, lhs, rhs, node.span);

		if self.unify(new_lhs, lhs).is_err() {
			let msg = format!(
				"Cannot assign `{}` to `{}`.",
				self.ty_to_string(new_lhs),
				self.ty_to_string(lhs)
			);
			format_err_f(&msg, node.span, self.input);
			self.errors.push(msg);
		}
	}

	fn eval_un_op(&mut self, op: &UnOp, id: TyId, span: Span) -> TyId {
		let ty = self.get_type(id);
		match op {
			UnOp::Minus => {
				if ty == Ty::Int {
					return id;
				}
				if ty == Ty::Num {
					return id;
				}
			},
			UnOp::Not => {
				if ty == Ty::Bool {
					return id;
				}
			},
		}
		let msg = format!("Operator `{op}` cannot by applied to `{}`.", self.ty_to_string(id));
		format_err_f(&msg, span, self.input);
		self.errors.push(msg);
		ERR_TY
	}

	fn eval_bin_op(&mut self, op: &BinOp, lhs: TyId, rhs: TyId, span: Span) -> TyId {
		if self.unify(lhs, rhs).is_err() {
			let msg = format!(
				"Operator `{op}` cannot by applied to `{}` and `{}`.",
				self.ty_to_string(lhs),
				self.ty_to_string(rhs)
			);
			format_err_f(&msg, span, self.input);
			self.errors.push(msg);
			return ERR_TY;
		}
		let ty = self.get_type(lhs);
		match op {
			BinOp::Plus | BinOp::Minus | BinOp::Mul | BinOp::Pow | BinOp::Mod | BinOp::Div => {
				// TODO: currently div and pow on int have type `(int, int) -> int` but this is wrong!
				if ty == Ty::Int || ty == Ty::Num {
					return lhs;
				}
			},
			BinOp::Gt | BinOp::Lt | BinOp::Gte | BinOp::Lte => {
				if ty == Ty::Int || ty == Ty::Num || ty == Ty::Str {
					return self.new_ty(Ty::Bool);
				}
			},
			BinOp::Concat => {
				if ty == Ty::Str {
					return lhs;
				}
			},
			BinOp::And | BinOp::Or => {
				if ty == Ty::Bool {
					return lhs;
				}
			},
			BinOp::Eq | BinOp::Neq => {
				return self.new_ty(Ty::Bool);
			},
		}

		let msg = format!("Failed to infer if we can apply `{op}` here.");
		format_err_f(&msg, span, self.input);
		self.errors.push(msg);
		ERR_TY
	}

	fn eval_literal(&mut self, l: &Literal) -> TyId {
		match l {
			Literal::Nil => self.new_ty(Ty::Nil),
			Literal::Bool(_) => self.new_ty(Ty::Bool),
			Literal::Num(_) => self.new_ty(Ty::Num),
			Literal::Int(_) => self.new_ty(Ty::Int),
			Literal::Str(_) => self.new_ty(Ty::Str),
		}
	}
}
