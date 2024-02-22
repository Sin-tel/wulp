use crate::ast::*;
use crate::span::Span;
use crate::span::{format_err, format_note};
use crate::std_lib::GLOBALS;
use crate::symbol::SymbolId;
use crate::symbol::SymbolTable;
use crate::ty::*;
use anyhow::anyhow;
use anyhow::Result;
use rustc_hash::FxHashMap;
use std::iter::zip;

#[derive(Debug)]
pub struct TypeCheck<'a> {
	input: &'a str,
	errors: Vec<String>,
	env: FxHashMap<SymbolId, TyId>, // TODO: this can be Vec<Option>
	types: Vec<TyNode>,
}

const ERR_TY: TyId = 0;

type RetPair = Option<(TyId, Span)>;

impl<'a> TypeCheck<'a> {
	pub fn check(file: &File, input: &'a str, symbol_table: &SymbolTable) -> Result<()> {
		let mut this = Self {
			input,
			errors: Vec::new(),
			types: Vec::new(),
			env: FxHashMap::default(),
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

		let (ret, _) = this.eval_block(&file.block);

		if let Some((ret_ty, _)) = ret {
			println!("Main return: {}", this.ty_to_string(ret_ty));
		}
		// println!("----- types ");
		// for (i, _) in this.types.iter().enumerate() {
		// 	println!("{i}: {}", this.ty_to_string(i));
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
			Ty::Table(_) => "table".to_string(),
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
		match self.get_type(id) {
			Ty::Any | Ty::Bottom | Ty::Nil | Ty::Bool | Ty::Str | Ty::Num | Ty::Int | Ty::Free => (),
			Ty::TyVar => {
				let parent_id = self.get_parent(id);
				self.types[parent_id] = TyNode::Ty(Ty::Free);
			},
			Ty::Table(_) => todo!(),
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
			Ty::Any | Ty::Bottom | Ty::Nil | Ty::Bool | Ty::Str | Ty::Num | Ty::Int | Ty::TyVar => parent_id,
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
			Ty::Table(_) => todo!(),
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
			Ty::Any | Ty::Bottom | Ty::Nil | Ty::Bool | Ty::Str | Ty::Num | Ty::Int | Ty::TyVar | Ty::Free => Ok(()),
			Ty::Table(_) => todo!(),
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
						format_err(&msg, new_span, self.input);
						let msg2 = "Previous return value defined here:".to_string();
						format_note(&msg2, prev_span, self.input);
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
	fn eval_block(&mut self, block: &Block) -> (RetPair, bool) {
		for stat in &block.stats {
			if let Stat::FnDef(s) = stat {
				self.hoist_fn_def(s);
			}
		}
		let mut current_pair: RetPair = None;
		for stat in &block.stats {
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
						Some((self.eval_expr(&ret.exprs[0]), ret.span))
					};
					current_pair = self.unify_return(current_pair, new_pair);
					return (current_pair, true);
				},
				Stat::IfBlock(if_block) => {
					// TODO: check bool
					self.eval_expr(&if_block.expr);
					let (if_pair, ret) = self.eval_block(&if_block.block);
					let mut all_return = ret;
					current_pair = self.unify_return(current_pair, if_pair);

					for elif in &if_block.elseif {
						self.eval_expr(&elif.expr);
						let (elif_pair, ret) = self.eval_block(&elif.block);
						all_return &= ret;
						current_pair = self.unify_return(current_pair, elif_pair);
					}
					if let Some(else_block) = &if_block.else_block {
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
					self.eval_expr(&s.expr);
					let (pair, _) = self.eval_block(&s.block);
					current_pair = self.unify_return(current_pair, pair);
				},
				Stat::ForBlock(s) => {
					let ty = self.eval_expr(&s.expr);
					assert!(s.names.len() == 1);

					// make new i: U
					let loop_var = self.new_def(s.names[0].id, Ty::TyVar);

					// unify iter: [T], [U]
					// for now, only iterate on arrays
					let loop_ty = self.new_ty(Ty::Array(loop_var));
					if self.unify(ty, loop_ty).is_err() {
						panic!("error: currently iteration is only supported on arrays.");
					}

					let (pair, _) = self.eval_block(&s.block);
					current_pair = self.unify_return(current_pair, pair);
				},
				Stat::Call(s) => {
					self.eval_fn_call(s);
				},
				Stat::Assignment(s) => self.eval_assignment(s),
				Stat::AssignOp(s) => self.eval_assign_op(s),
				Stat::Let(s) => self.eval_let(s),
				Stat::FnDef(s) => self.eval_fn_def(s),
			};
		}
		(current_pair, false)
	}

	fn eval_fn_params(&mut self, params: &[Param]) -> Vec<TyId> {
		let mut param_ty = Vec::new();
		for p in params {
			let ty = match &p.ty {
				Some(ty) => self.to_ty(ty),
				None => Ty::TyVar,
			};
			let id = self.new_def(p.name.id, ty);
			param_ty.push(id);
		}
		param_ty
	}

	fn eval_fn_body(&mut self, node: &FnBody, span: Span) -> (TyId, Span) {
		let (mut ret_pair, ret_all) = self.eval_block(&node.body);
		// There is an implied 'return nil' at the end of every body
		if !ret_all {
			let nil = self.new_ty(Ty::Nil);
			ret_pair = self.unify_return(ret_pair, Some((nil, span)));
		};
		ret_pair.unwrap()
	}

	fn hoist_fn_def(&mut self, node: &FnDef) {
		assert!(node.path.is_empty());

		let param_ty = self.eval_fn_params(&node.body.params);
		let ty = match &node.body.ty {
			Some(ty) => self.to_ty(ty),
			None => Ty::TyVar,
		};
		let ret_id = self.new_ty(ty);
		let fn_ty = Ty::Fn(param_ty, ret_id);
		self.new_def(node.name.id, fn_ty);
	}

	fn eval_fn_def(&mut self, node: &FnDef) {
		let fn_id = self.lookup(node.name.id).unwrap();
		let fn_ty = self.get_type(fn_id);
		let ret_id = if let Ty::Fn(_, ret_id) = fn_ty {
			ret_id
		} else {
			panic!("lookup failed");
		};

		let (ty, prev_span) = self.eval_fn_body(&node.body, node.name.span);

		if self.unify(ty, ret_id).is_err() {
			let msg = format!(
				"Expected return type `{}`, found `{}`.",
				self.ty_to_string(ty),
				self.ty_to_string(ret_id)
			);
			format_err(&msg, prev_span, self.input);
			self.errors.push(msg);
		};

		self.promote_free(fn_id);

		println!("infer fn def: {} {}", fn_id, self.ty_to_string(fn_id));
	}

	// span should refer to the place where the function is defined
	// TODO: get span info from AST and remove this argument
	fn eval_lambda(&mut self, node: &FnBody, span: Span) -> TyId {
		let param_ty = self.eval_fn_params(&node.params);
		let ty = match &node.ty {
			Some(ty) => self.to_ty(ty),
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
			format_err(&msg, prev_span, self.input);
			self.errors.push(msg);
		}
		self.promote_free(fn_id);
		println!("infer lambda: {} {}", fn_id, self.ty_to_string(fn_id));
		fn_id
	}

	fn eval_fn_call(&mut self, c: &Call) -> TyId {
		let fn_ty_id = self.eval_expr(&c.expr);
		let fn_ty = self.instantiate(fn_ty_id);
		if let Ty::Fn(params, ret_ty) = self.get_type(fn_ty) {
			// TODO: get rid of hardcoded "print" here
			if params.len() != c.args.len() && c.expr.span.as_str(self.input) != "print" {
				let msg = format!(
					"Function takes {} argument(s), {} supplied.",
					params.len(),
					c.args.len()
				);
				format_err(&msg, c.expr.span, self.input);
				self.errors.push(msg);
			} else {
				for (p, a) in zip(params, c.args.iter()) {
					let arg_ty = self.eval_expr(a);
					if self.unify(arg_ty, p).is_err() {
						let msg = format!(
							"Expected argument type `{}`, found `{}`",
							self.ty_to_string(p),
							self.ty_to_string(arg_ty)
						);
						format_err(&msg, a.span, self.input);
						self.errors.push(msg);
					}
				}
			}
			ret_ty
		} else {
			let msg = format!("Type `{}` is not callable.", self.ty_to_string(fn_ty));
			format_err(&msg, c.expr.span, self.input);
			self.errors.push(msg);
			ERR_TY
		}
	}

	fn eval_expr(&mut self, expr: &Expr) -> TyId {
		match &expr.kind {
			ExprKind::BinExpr(e) => {
				let lhs = self.eval_expr(&e.lhs);
				let rhs = self.eval_expr(&e.rhs);
				self.eval_bin_op(&e.op, lhs, rhs, expr.span)
			},
			ExprKind::UnExpr(e) => {
				let ty = self.eval_expr(&e.expr);
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
						format_err(&msg, e.span, self.input);
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
						expr.span.as_str(self.input)
					);
					format_err(&msg, e.span, self.input);
					panic!("{}", &msg);
				}
			},
			ExprKind::SuffixExpr(e, s) => self.eval_suffix_expr(e, s),
			ExprKind::Literal(e) => self.eval_literal(e),
			ExprKind::Call(e) => self.eval_fn_call(e),
			ExprKind::Expr(e) => self.eval_expr(e),
			ExprKind::Lambda(e) => self.eval_lambda(e, expr.span),
			// ExprKind::Table(_) => Ty::Table(0), // TODO
			_ => todo!(),
		}
	}

	fn eval_suffix_expr(&mut self, expr: &Expr, s: &Vec<Suffix>) -> TyId {
		let mut ty = self.eval_expr(expr);
		for suffix in s {
			match suffix {
				Suffix::Property(_) => {
					todo!()
					// ty = if let Ty::Table(_id) = ty {
					// 	// Ty::Any
					// } else {
					// 	let msg = format!("Can not index type `{ty}`.");
					// 	format_err(&msg, expr.span, self.input);
					// 	self.errors.push(msg);
					// 	ERR_TY
					// }
				},
				Suffix::Index(e) => {
					ty = match self.get_type(ty) {
						Ty::Array(inner_ty) => inner_ty,
						_ => {
							let msg = format!("Can not index type `{}`.", self.ty_to_string(ty));
							format_err(&msg, expr.span, self.input);
							self.errors.push(msg);
							ERR_TY
						},
					};
					let index_ty = self.eval_expr(e);
					let ty_int = self.new_ty(Ty::Int);
					if self.unify(index_ty, ty_int).is_err() {
						let msg = format!("Index must be type `int` but found `{}`.", self.ty_to_string(index_ty));
						format_err(&msg, e.span, self.input);
						self.errors.push(msg);
					}
				},
			}
		}
		ty
	}

	fn eval_let(&mut self, node: &Let) {
		assert!(node.exprs.len() == node.names.len());

		let mut rhs = Vec::new();
		for e in &node.exprs {
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
					format_err(&msg, node.span, self.input);
					self.errors.push(msg);
				}
				self.new_def(n.name.id, self.to_ty(ty));
			} else {
				self.new_def(n.name.id, self.get_type(rhs_ty).clone());
			}
		}
	}

	fn eval_lvalue(&mut self, var: &Expr) -> TyId {
		match &var.kind {
			ExprKind::Name(n) => self.lookup(n.id).expect("lookup failed"),
			ExprKind::SuffixExpr(e, s) => self.eval_suffix_expr(e, s),
			_ => unreachable!(),
		}
	}

	fn eval_assignment(&mut self, node: &Assignment) {
		assert!(node.exprs.len() == node.vars.len());

		let mut rhs = Vec::new();
		for e in &node.exprs {
			rhs.push(self.eval_expr(e));
		}
		for (var, rhs_ty) in zip(&node.vars, rhs) {
			let ty = self.eval_lvalue(var);
			if self.unify(rhs_ty, ty).is_err() {
				let msg = format!(
					"Type error, assigning `{}` to `{}`.",
					self.ty_to_string(rhs_ty),
					self.ty_to_string(ty)
				);
				format_err(&msg, node.span, self.input);
				self.errors.push(msg);
			};
		}
	}

	fn eval_assign_op(&mut self, node: &AssignOp) {
		// TODO: refactor to use binop code
		let rhs = self.eval_expr(&node.expr);
		let lhs = self.eval_lvalue(&node.var);

		let new_lhs = self.eval_bin_op(&node.op, lhs, rhs, node.span);

		if self.unify(new_lhs, lhs).is_err() {
			let msg = format!(
				"Cannot assign `{}` to `{}`.",
				self.ty_to_string(new_lhs),
				self.ty_to_string(lhs)
			);
			format_err(&msg, node.span, self.input);
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
		format_err(&msg, span, self.input);
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
			format_err(&msg, span, self.input);
			self.errors.push(msg);
			return ERR_TY;
		}
		let ty = self.get_type(lhs);
		match op {
			BinOp::Plus | BinOp::Minus | BinOp::Mul | BinOp::Pow | BinOp::Mod | BinOp::Div => {
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
		format_err(&msg, span, self.input);
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
