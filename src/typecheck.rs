use crate::ast::*;
use crate::scope::BOOL_SYM;
use crate::scope::INT_SYM;
use crate::scope::NUM_SYM;
use crate::scope::STR_SYM;
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

#[derive(Debug)]
struct Struct {
	fields: FxHashMap<String, TyId>,
	methods: FxHashMap<String, TyId>,
	required_fields: Vec<String>,
	symbol_id: SymbolId,
	lang_item: bool,
}

const ERR_TY: TyId = 0;

type RetPair = Option<(TyId, Span)>;

#[derive(Debug)]
pub struct TypeCheck<'a> {
	input: &'a [InputFile],
	errors: Vec<String>,
	env: FxHashMap<SymbolId, TyId>, // TODO: this can be Vec<Option>
	types: Vec<TyNode>,
	structs: FxHashMap<SymbolId, Struct>,
	symbol_table: &'a SymbolTable,
}

impl<'a> TypeCheck<'a> {
	pub fn check(file: &mut File, input: &'a [InputFile], symbol_table: &'a SymbolTable) -> Result<()> {
		let mut this = Self {
			input,
			errors: Vec::new(),
			types: Vec::new(),
			env: FxHashMap::default(),
			structs: FxHashMap::default(),
			symbol_table,
		};

		this.types.push(TyNode::Ty(Ty::Bottom));
		assert!(this.get_type(ERR_TY) == Ty::Bottom);

		this.new_struct(INT_SYM, true);
		this.new_struct(NUM_SYM, true);
		this.new_struct(STR_SYM, true);
		this.new_struct(BOOL_SYM, true);

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
	fn new_struct(&mut self, id: SymbolId, lang_item: bool) {
		self.structs.insert(
			id,
			Struct {
				fields: FxHashMap::default(),
				methods: FxHashMap::default(),
				required_fields: Vec::new(),
				symbol_id: id,
				lang_item,
			},
		);
		self.new_def(id, Ty::TyName(id));
	}
	fn ty_to_string(&self, id: TyId) -> String {
		let id = self.get_parent(id);
		match self.get_type(id) {
			Ty::Any => "Any".to_string(),
			Ty::Bottom => "Bottom".to_string(),
			Ty::Unit => "()".to_string(),
			Ty::Bool => "bool".to_string(),
			Ty::Str => "str".to_string(),
			Ty::Num => "num".to_string(),
			Ty::Int => "int".to_string(),
			Ty::TyVar => format!("T{id}?"),
			Ty::Free => format!("T{id}"),
			Ty::TyName(s) => format!("Type({s})"),
			Ty::Named(s) => format!("Instance {s}"),
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
	fn convert_ast_ty(&mut self, ty_ast: &TyAst) -> Ty {
		match ty_ast {
			TyAst::Named(s) => self.convert_named(s.id),
			TyAst::Fn(args, ret) => {
				let mut t_args = Vec::new();
				for a in args {
					let ty = self.convert_ast_ty(a);
					t_args.push(self.new_ty(ty));
				}
				let ty = self.convert_ast_ty(ret);
				Ty::Fn(t_args, self.new_ty(ty))
			},
			TyAst::Array(a) => {
				let ty = self.convert_ast_ty(a);
				Ty::Array(self.new_ty(ty))
			},
			TyAst::SelfTy => unreachable!(),
			TyAst::Maybe(a) => {
				let ty = self.convert_ast_ty(a);
				Ty::Maybe(self.new_ty(ty))
			},
		}
	}

	fn convert_named(&self, id: SymbolId) -> Ty {
		let s = self.structs.get(&id).unwrap();
		if !s.lang_item {
			Ty::Named(id)
		} else {
			let name = self.symbol_table.get(id).name.as_ref();
			match name {
				"str" => Ty::Str,
				"int" => Ty::Int,
				"num" => Ty::Num,
				"bool" => Ty::Bool,
				e => panic!("Can not make instance of type {}", e),
			}
		}
	}

	fn get_struct_id(&self, ty: TyId) -> SymbolId {
		match self.get_type(ty) {
			Ty::Named(s) => s,
			Ty::Num => NUM_SYM,
			Ty::Int => INT_SYM,
			Ty::Str => STR_SYM,
			Ty::Bool => BOOL_SYM,
			t => unimplemented!("{:?}", t),
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
			| Ty::Unit
			| Ty::Bool
			| Ty::Str
			| Ty::Num
			| Ty::Int
			| Ty::Free
			| Ty::Named(_)
			| Ty::TyName(_) => (),
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

	// instantiate free variables to fresh type variables
	fn instantiate_inner(&mut self, id: TyId, subs: &mut Vec<(TyId, TyId)>) -> TyId {
		let ty = self.get_type(id);
		let parent_id = self.get_parent(id);
		match ty {
			Ty::Any
			| Ty::Bottom
			| Ty::Unit
			| Ty::Bool
			| Ty::Str
			| Ty::Num
			| Ty::Int
			| Ty::TyVar
			| Ty::Named(_)
			| Ty::TyName(_) => parent_id,
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
			| Ty::Unit
			| Ty::Bool
			| Ty::Str
			| Ty::Num
			| Ty::Int
			| Ty::TyVar
			| Ty::TyName(_)
			| Ty::Named(_)
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
				(_, Ty::Free) | (Ty::Free, _) => Err(()),
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

				// TODO: only when coercion ok
				// (_, Ty::Maybe(b)) => self.unify(a_id, b),
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
		for stat in &mut block.stats {
			if let Stat::FnDef(s) = stat {
				self.hoist_fn_def(s);
			}
			if let Stat::StructDef(s) = stat {
				self.eval_struct_def(s);
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
				Stat::FnDef(s) => {
					if let Some(p) = &mut s.property {
						let struct_id = s.name.id;
						let self_ty = self.new_ty(self.convert_named(struct_id));
						let ty = self.eval_lambda(&mut s.body, p.span, Some(self_ty));
						self.structs
							.get_mut(&struct_id)
							.unwrap()
							.methods
							.insert(p.name.clone(), ty);
					} else {
						self.eval_fn_def(s)
					}
				},
				Stat::StructDef(_) => (),
				Stat::Import(_s) => {
					todo!();
				},
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
				self.get_type(self_ty.expect("Can not use `self` here"))
			} else {
				let ty = match &p.ty {
					Some(ty) => match ty {
						TyAst::SelfTy => self.get_type(self_ty.expect("Can not use `self` here")),
						_ => self.convert_ast_ty(ty),
					},
					None => Ty::TyVar,
				};
				ty
			};
			let id = self.new_def(p.name.id, ty);
			param_ty.push(id);
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
			let ty = match &node.body.ty {
				Some(ty) => self.convert_ast_ty(ty),
				None => Ty::TyVar,
			};
			let ret_id = self.new_ty(ty);
			let fn_ty = Ty::Fn(param_ty, ret_id);
			self.new_def(node.name.id, fn_ty);
		}
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
	fn eval_lambda(&mut self, node: &mut FnBody, span: Span, self_ty: Option<TyId>) -> TyId {
		let param_ty = self.eval_fn_params(&node.params, self_ty);
		let ret_id = match &node.ty {
			Some(ty) => match ty {
				TyAst::SelfTy => self_ty.expect("can't use `self` here"),
				_ => {
					let n_ty = self.convert_ast_ty(ty);
					self.new_ty(n_ty)
				},
			},
			None => self.new_ty(Ty::TyVar),
		};
		let fn_ty = Ty::Fn(param_ty, ret_id);
		let fn_id = self.new_ty(fn_ty);

		let (ty, prev_span) = self.eval_fn_body(node, span);

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

			// TODO: this is a bit messy!
			// TODO: do not call this when evaluating lambda field
			if let Ty::TyName(_) = self.get_type(ty) {
				s.push(last_suffix);
			} else {
				dbg!(&ty);
				let name = self.get_struct_id(ty);
				dbg!(&name);
				// fix up the AST for method call
				let inst_expr = std::mem::replace(
					expr,
					Box::new(Expr {
						span: expr.span,
						kind: ExprKind::Name(Name {
							span: expr.span,
							id: self.structs[&name].symbol_id,
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
			}
		}

		let fn_ty_id = self.eval_expr(&mut c.expr);
		println!("fn_ty_id {}", self.ty_to_string(fn_ty_id));
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
			Ty::TyName(struct_name) => {
				// TODO: instantiate does not take care of generics on struct

				if self.structs[&struct_name].lang_item {
					let msg = "builtin type does not have a constructor".to_string();
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
				for k in &self.structs[&struct_name].required_fields {
					let mut check = false;
					for a in &mut c.named_args {
						if &a.name == k {
							check = true;
							break;
						}
					}
					if !check {
						let msg = format!("Missing required field `{k}`");
						format_err_f(&msg, c.expr.span, self.input);
						self.errors.push(msg);
						return ERR_TY;
					}
				}
				for a in &mut c.named_args {
					if let Some(&p) = self.structs[&struct_name].fields.get(&a.name) {
						let arg_ty = self.eval_expr(&mut a.expr);
						if self.unify(arg_ty, p).is_err() {
							let msg = format!(
								"Field has type `{}`, found `{}`",
								self.ty_to_string(p),
								self.ty_to_string(arg_ty)
							);
							format_err_f(&msg, a.span, self.input);
							self.errors.push(msg);
							return ERR_TY;
						}
					} else {
						let msg = format!("Struct does not have a field named `{}`.", a.name);
						format_err_f(&msg, a.span, self.input);
						self.errors.push(msg);
						return ERR_TY;
					}
				}
				self.new_ty(Ty::Named(struct_name))
			},
			Ty::Bottom => ERR_TY,
			_ => {
				let msg = format!("Type `{}` is not callable.", self.ty_to_string(fn_ty));
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
		let id = node.name.id;

		self.new_struct(id, false);

		let self_ty = self.new_ty(self.convert_named(id));
		for f in &mut node.table.fields {
			match &mut f.kind {
				FieldKind::Empty => {
					self.structs
						.get_mut(&id)
						.unwrap()
						.required_fields
						.push(f.field.property.name.clone());

					if let Some(ty) = &f.field.ty {
						let new_ty = self.convert_ast_ty(ty);
						let ty = self.new_ty(new_ty);
						self.structs
							.get_mut(&id)
							.unwrap()
							.fields
							.insert(f.field.property.name.clone(), ty);
					} else {
						let msg = "Need type annotation here";
						format_err_f(msg, f.field.property.span, self.input);
						// self.errors.push(msg);
						panic!("{}", &msg);
					}
				},
				FieldKind::Assign(a) => {
					if !a.is_const() {
						let msg = "Default field must be a constant expression".to_string();
						format_err_f(&msg, a.span, self.input);
						self.errors.push(msg);
					}
					let mut expr_ty = self.eval_expr(a);
					if let Some(ty) = &f.field.ty {
						let new_ty = self.convert_ast_ty(ty);
						let ty_id = self.new_ty(new_ty);
						if self.unify(ty_id, expr_ty).is_err() {
							let msg = format!(
								"Incompatible types `{}` and `{}`",
								self.ty_to_string(ty_id),
								self.ty_to_string(expr_ty)
							);
							format_err_f(&msg, a.span, self.input);
							self.errors.push(msg);
						}
						expr_ty = ty_id;
					}
					self.structs
						.get_mut(&id)
						.unwrap()
						.fields
						.insert(f.field.property.name.clone(), expr_ty);
				},
				FieldKind::Fn(func) => {
					let ty = self.eval_lambda(func, f.field.property.span, Some(self_ty));
					self.structs
						.get_mut(&id)
						.unwrap()
						.methods
						.insert(f.field.property.name.clone(), ty);
				},
			};
		}
	}

	fn get_field(&mut self, struct_id: SymbolId, p: &mut Property) -> TyId {
		if let Some(p_id) = self.structs[&struct_id].fields.get(&p.name) {
			*p_id
		} else {
			let msg = format!("No field `{}`.", p.name);
			format_err_f(&msg, p.span, self.input);
			self.errors.push(msg);
			ERR_TY
		}
	}

	fn get_method(&mut self, struct_id: SymbolId, p: &mut Property) -> TyId {
		if let Some(p_id) = self.structs[&struct_id].methods.get(&p.name) {
			*p_id
		} else {
			let msg = format!("No method `{}`.", p.name);
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
				Ty::Named(name) => self.get_field(name, p),
				Ty::TyName(name) => self.get_method(name, p),
				Ty::Bottom => ERR_TY,
				_ => {
					let msg = format!("Type `{}` does not allow indexing with `.`", self.ty_to_string(expr_ty));
					format_err_f(&msg, expr_span, self.input);
					self.errors.push(msg);
					ERR_TY
				},
			},
			Suffix::Index(e) => {
				let ty = match self.get_type(expr_ty) {
					Ty::Array(inner_ty) => inner_ty,
					Ty::Bottom => ERR_TY,
					_ => {
						let msg = format!("Can not index type `{}`.", self.ty_to_string(expr_ty));
						format_err_f(&msg, expr_span, self.input);
						self.errors.push(msg);
						return ERR_TY;
					},
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
				let new_ty = self.convert_ast_ty(ty);
				let ty_id = self.new_ty(new_ty);
				if self.unify(rhs_ty, ty_id).is_err() {
					let msg = format!(
						"Type error, assigning `{}` to `{}`.",
						self.ty_to_string(rhs_ty),
						self.ty_to_string(ty_id)
					);
					format_err_f(&msg, node.span, self.input);
					self.errors.push(msg);
				}
				let new_ty = self.convert_ast_ty(ty);
				self.new_def(n.name.id, new_ty);
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

		let new_lhs = self.eval_bin_op(&node.op, lhs, rhs, node.span);

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
			BinOp::Plus | BinOp::Minus | BinOp::Mul | BinOp::Mod => {
				if ty == Ty::Int || ty == Ty::Num {
					return lhs;
				}
			},
			BinOp::Pow | BinOp::Div => {
				if ty == Ty::Int || ty == Ty::Num {
					return self.new_ty(Ty::Num);
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
			Literal::Nil => {
				let ty = self.new_ty(Ty::TyVar);
				self.new_ty(Ty::Maybe(ty))
			},
			Literal::Bool(_) => self.new_ty(Ty::Bool),
			Literal::Num(_) => self.new_ty(Ty::Num),
			Literal::Int(_) => self.new_ty(Ty::Int),
			Literal::Str(_) => self.new_ty(Ty::Str),
		}
	}
}
