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

type RetPair = Option<(TyId, Span)>;

impl<'a> TypeCheck<'a> {
	pub fn check(file: &File, input: &'a str, symbol_table: &SymbolTable) -> Result<()> {
		let mut this = Self {
			input,
			errors: Vec::new(),
			types: Vec::new(),
			env: FxHashMap::default(),
		};

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
			println!("Main return: {}, ({})", this.get_type(ret_ty), ret_ty);
		}

		println!("----- types ");
		for (i, v) in this.types.iter().enumerate() {
			println!("{}: {}", i, v);
		}

		println!("----- env ");
		for (i, s) in symbol_table.symbols.iter().enumerate().skip(1) {
			if let Some(id) = this.lookup(i) {
				println!("{}: {}", s.name, this.get_type(id));
			}
		}

		match this.errors.last() {
			Some(err) => Err(anyhow!("{}", err)),
			None => Ok(()),
		}
	}

	fn new_def(&mut self, id: SymbolId, ty: Ty) -> TyId {
		let ty_id = self.new_ty(ty);
		self.env.insert(id, ty_id);
		ty_id
	}

	fn new_ty(&mut self, ty: Ty) -> TyId {
		let ty_id = self.types.len();
		self.types.push(TyNode::Ty(ty));
		ty_id
	}

	fn unify(&mut self, a_id: TyId, b_id: TyId) -> bool {
		println!(
			"unify {}: {} and {}: {}",
			a_id,
			self.get_type(a_id),
			b_id,
			self.get_type(b_id)
		);
		if a_id == b_id {
			true
		} else {
			match (self.get_type(a_id), self.get_type(b_id)) {
				(_, Ty::TyVar) => {
					self.types[b_id] = TyNode::Node(a_id);
					true
				},
				(Ty::TyVar, _) => {
					self.types[a_id] = TyNode::Node(b_id);
					true
				},

				(Ty::Array(a), Ty::Array(b)) => self.unify(*a, *b),
				(Ty::Maybe(a), Ty::Maybe(b)) => self.unify(*a, *b),
				(a, b) if a == b => true,
				_ => false,
			}
		}
	}

	fn lookup(&self, id: SymbolId) -> Option<TyId> {
		// TODO: pass in span and fail here
		self.env.get(&id).copied()
	}

	fn get_type(&self, mut id: TyId) -> &Ty {
		// TODO: recursively look up parents of typevars
		loop {
			match &self.types[id] {
				TyNode::Node(parent) => id = *parent,
				TyNode::Ty(ty) => return &ty,
			}
		}
	}

	fn unify_return(&mut self, current_pair: RetPair, new_pair: RetPair) -> RetPair {
		if let Some((ref new_ty, new_span)) = new_pair {
			match current_pair {
				Some((ty, prev_span)) => {
					if self.unify(ty, *new_ty) {
						Some((ty, new_span))
					} else {
						let msg = format!("Incompatible return types `{new_ty}` and `{ty}`.");
						format_err(&msg, new_span, self.input);
						let msg2 = "Previous return value defined here:".to_string();
						format_note(&msg2, prev_span, self.input);
						self.errors.push(msg);
						Some((self.new_ty(Ty::Bottom), new_span))
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
					self.eval_expr(&s.expr);
					let (pair, _) = self.eval_block(&s.block);
					current_pair = self.unify_return(current_pair, pair);
				},
				Stat::ForBlock(s) => {
					let ty = self.eval_expr(&s.expr);
					// for now, only iterate on arrays
					// let ty = if let Ty::Array(inner) = self.get_type(ty) {
					// 	inner
					// } else {
					// 	panic!("error: currently iteration is only supported on arrays.");
					// };
					// assert!(s.names.len() == 1);
					// self.new_def(s.names[0].id, Ty::TyVar(*ty));

					// make new i: U
					let loop_var = self.new_def(s.names[0].id, Ty::TyVar);

					// unify iter: [T], [U]
					let loop_ty = self.new_ty(Ty::Array(loop_var));
					self.unify(ty, loop_ty);

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
			// let ty = p.ty.clone().expect("Need parameter annotation");
			if p.ty.is_some() {
				println!("warning: type annotation {} ignored", p.ty.as_ref().unwrap());
			}

			let id = self.new_def(p.name.id, Ty::TyVar);
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

	fn eval_fn_def(&mut self, node: &FnDef) {
		assert!(node.path.is_empty());

		let param_ty = self.eval_fn_params(&node.body.params);
		// let ret_annotation = self.new_ty(node.body.ty.as_ref().expect("Need return type annotation").clone());
		if node.body.ty.is_some() {
			println!("warning: type annotation {} ignored", node.body.ty.as_ref().unwrap());
		}
		let ret_id = self.new_ty(Ty::TyVar);
		let fn_ty = Ty::Fn(param_ty, ret_id);
		let fn_id = self.new_def(node.name.id, fn_ty);

		let (ty, prev_span) = self.eval_fn_body(&node.body, node.name.span);

		dbg!(ty, ret_id);

		if !self.unify(ty, ret_id) {
			let msg = format!(
				"Expected return type `{}`, found `{}`.",
				self.get_type(ty),
				self.get_type(ret_id)
			);
			format_err(&msg, prev_span, self.input);
			self.errors.push(msg);
		};

		println!("infer fn def: {} {}", fn_id, self.get_type(fn_id));
	}

	// span should refer to the place where the function is defined
	// TODO: get span info from AST and remove this argument
	fn eval_lambda(&mut self, node: &FnBody, span: Span) -> TyId {
		todo!()
		// let param_ty = self.eval_fn_params(&node.params);
		// let ret_annotation = node.ty.as_ref();

		// let (ty, prev_span) = self.eval_fn_body(node, span);

		// let ret_ty = if let Some(ret_ty) = ret_annotation {
		// 	if !self.unify(&ty, ret_ty) {
		// 		let msg = format!("Expected return type `{ret_ty}`, found `{ty}`.");
		// 		format_err(&msg, prev_span, self.input);
		// 		self.errors.push(msg);
		// 	}
		// 	ret_ty
		// } else {
		// 	println!("infer fn return: {}", &ty);
		// 	&ty
		// };

		// Ty::Fn(param_ty, Box::new(ret_ty.clone()))
	}

	fn eval_fn_call(&mut self, c: &Call) -> TyId {
		let fn_ty = self.eval_expr(&c.expr);
		if let Ty::Fn(params, ret_ty) = self.get_type(fn_ty).clone() {
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
					// TODO: shouldn't unify here for TypeVars
					if !self.unify(arg_ty, p) {
						let msg = format!("Expected argument type `{p}`, found `{arg_ty}`");
						format_err(&msg, a.span, self.input);
						self.errors.push(msg);
					}
				}
			}
			ret_ty
		} else {
			let msg = format!("Type `{fn_ty}` is not callable.");
			format_err(&msg, c.expr.span, self.input);
			self.errors.push(msg);
			self.new_ty(Ty::Bottom)
		}
	}

	fn eval_expr(&mut self, expr: &Expr) -> TyId {
		let ty = self.eval_expr_inner(expr);
		println!(
			"infer {}: {} ({})",
			&expr.span.as_str(self.input),
			self.get_type(ty),
			ty
		);
		ty
	}

	fn eval_expr_inner(&mut self, expr: &Expr) -> TyId {
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
				// consider empty array `[]` to have type [Bottom]
				let mut ty = self.new_ty(Ty::Bottom);
				for e in array {
					let new_ty = self.eval_expr(e);
					ty = if self.unify(ty, new_ty) {
						new_ty
					} else {
						let msg = format!("Incompatible types in array: `{new_ty}` and `{ty}`.");
						format_err(&msg, e.span, self.input);
						self.errors.push(msg);
						return self.new_ty(Ty::Bottom);
					}
				}
				self.new_ty(Ty::Array(ty))
			},
			ExprKind::Name(e) => {
				let ty_opt = self.lookup(e.id);
				if let Some(ty) = ty_opt {
					ty.clone()
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
					// 	Ty::Bottom
					// }
				},
				Suffix::Index(e) => {
					ty = match self.get_type(ty) {
						Ty::Array(inner_ty) => *inner_ty,
						_ => {
							let msg = format!("Can not index type `{ty}`.");
							format_err(&msg, expr.span, self.input);
							self.errors.push(msg);
							self.new_ty(Ty::Bottom)
						},
					};
					let index_ty = self.eval_expr(e);
					let ty_int = self.new_ty(Ty::Int);
					if !self.unify(index_ty, ty_int) {
						let msg = format!("Index must be type `int` but found `{index_ty}`.");
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
				let ty_id = self.new_ty(ty.clone());
				if !self.unify(rhs_ty, ty_id) {
					let msg = format!("Type error, assigning `{rhs_ty}` to `{ty}`.");
					format_err(&msg, node.span, self.input);
					self.errors.push(msg);
				}
				self.new_def(n.name.id, ty.clone());
			} else {
				self.new_def(n.name.id, self.get_type(rhs_ty).clone());
			}
		}
	}

	fn eval_lvalue(&mut self, var: &Expr) -> TyId {
		match &var.kind {
			ExprKind::Name(n) => self.lookup(n.id).expect("lookup failed").clone(),
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
			if !self.unify(rhs_ty, ty) {
				let msg = format!("Type error, assigning `{rhs_ty}` to `{ty}`.");
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

		if !self.unify(new_lhs, lhs) {
			let msg = format!("Cannot assign `{new_lhs}` to `{lhs}`.");
			format_err(&msg, node.span, self.input);
			self.errors.push(msg);
		}
	}

	fn eval_un_op(&mut self, op: &UnOp, id: TyId, span: Span) -> TyId {
		let ty = self.get_type(id);
		match op {
			UnOp::Minus => {
				if ty == &Ty::Int {
					return id;
				}
				if ty == &Ty::Num {
					return id;
				}
			},
			UnOp::Not => {
				if ty == &Ty::Bool {
					return id;
				}
			},
		}
		let msg = format!("Operator `{op}` cannot by applied to `{ty}`.");
		format_err(&msg, span, self.input);
		self.errors.push(msg);
		self.new_ty(Ty::Bottom)
	}

	fn eval_bin_op(&mut self, op: &BinOp, lhs: TyId, rhs: TyId, span: Span) -> TyId {
		if !self.unify(lhs, rhs) {
			let msg = format!("Operator `{op}` cannot by applied to `{lhs}` and `{rhs}`.");
			format_err(&msg, span, self.input);
			self.errors.push(msg);
			return self.new_ty(Ty::Bottom);
		}
		let ty = self.get_type(lhs);
		match op {
			BinOp::Plus | BinOp::Minus | BinOp::Mul | BinOp::Pow | BinOp::Mod => {
				if ty == &Ty::Int || ty == &Ty::Num {
					return lhs;
				}
			},
			BinOp::Div => {
				todo!();
				// if ty == &Ty::Int || ty == &Ty::Num {
				// 	return self.new_ty(Ty::Num);
				// }
			},
			BinOp::Gt | BinOp::Lt | BinOp::Gte | BinOp::Lte => {
				if ty == &Ty::Int || ty == &Ty::Num || ty == &Ty::Str {
					return self.new_ty(Ty::Bool);
				}
			},
			BinOp::Concat => {
				if ty == &Ty::Str {
					return lhs;
				}
			},
			BinOp::And | BinOp::Or => {
				if ty == &Ty::Bool {
					return lhs;
				}
			},
			BinOp::Eq | BinOp::Neq => {
				return self.new_ty(Ty::Bool);
			},
		}

		let msg = format!("Operator `{op}` cannot by applied to `{lhs}` and `{rhs}`.");
		format_err(&msg, span, self.input);
		self.errors.push(msg);
		return self.new_ty(Ty::Bottom);
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
