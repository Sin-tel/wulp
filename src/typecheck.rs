use crate::ast::*;
use crate::span::Span;
use crate::span::{format_err, format_note};
use crate::std_lib::GLOBALS;
use crate::symbol::SymbolId;
use crate::ty::*;
use anyhow::anyhow;
use anyhow::Result;
use rustc_hash::FxHashMap;
use std::iter::zip;

pub struct TypeCheck<'a> {
	input: &'a str,
	errors: Vec<String>,
	env: FxHashMap<SymbolId, Ty>,
}

type RetPair = Option<(Ty, Span)>;

impl<'a> TypeCheck<'a> {
	pub fn check(file: &File, input: &'a str) -> Result<()> {
		let mut this = Self {
			input,
			errors: Vec::new(),
			env: FxHashMap::default(),
		};

		for item in GLOBALS.iter() {
			this.new_def(item.id, item.ty.clone());
		}

		let ret_ty = this.eval_block(&file.block);

		println!("Returned {ret_ty:?}");

		match this.errors.last() {
			Some(err) => Err(anyhow!("{}", err)),
			None => Ok(()),
		}
	}

	fn new_def(&mut self, id: SymbolId, ty: Ty) {
		self.env.insert(id, ty);
	}

	fn lookup(&self, id: SymbolId) -> Option<&Ty> {
		self.env.get(&id)
	}

	fn join_or_fail(&mut self, current_pair: RetPair, new_pair: RetPair) -> RetPair {
		if let Some((ref new_ty, new_span)) = new_pair {
			match current_pair {
				Some((ty, prev_span)) => {
					let try_join = join(&ty, new_ty);
					if let Some(joined) = try_join {
						Some((joined, new_span))
					} else {
						let msg = format!("Incompatible return types `{new_ty}` and `{ty}`.");
						format_err(&msg, new_span, self.input);
						let msg2 = "Previous return value defined here:".to_string();
						format_note(&msg2, prev_span, self.input);
						self.errors.push(msg);
						Some((Ty::Bottom, new_span))
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
					current_pair = self.join_or_fail(current_pair, new_pair);
					if ret {
						return (current_pair, true);
					}
				},
				Stat::Return(ret) => {
					let new_pair = if ret.exprs.is_empty() {
						Some((Ty::Nil, ret.span))
					} else {
						// TODO: multiple return
						assert!(ret.exprs.len() == 1);
						Some((self.eval_expr(&ret.exprs[0]), ret.span))
					};
					current_pair = self.join_or_fail(current_pair, new_pair);
					return (current_pair, true);
				},
				Stat::IfBlock(if_block) => {
					self.eval_expr(&if_block.expr);
					let (if_pair, ret) = self.eval_block(&if_block.block);
					let mut all_return = ret;
					current_pair = self.join_or_fail(current_pair, if_pair);

					for elif in &if_block.elseif {
						self.eval_expr(&elif.expr);
						let (elif_pair, ret) = self.eval_block(&elif.block);
						all_return &= ret;
						current_pair = self.join_or_fail(current_pair, elif_pair);
					}
					if let Some(else_block) = &if_block.else_block {
						let (else_pair, ret) = self.eval_block(else_block);
						all_return &= ret;
						current_pair = self.join_or_fail(current_pair, else_pair);

						if all_return {
							return (current_pair, true);
						}
					}
				},
				Stat::WhileBlock(s) => {
					self.eval_expr(&s.expr);
					let (pair, _) = self.eval_block(&s.block);
					current_pair = self.join_or_fail(current_pair, pair);
				},
				Stat::ForBlock(s) => {
					let ty = self.eval_expr(&s.expr);
					// for now, only iterate on arrays
					let ty = if let Ty::Array(inner) = ty {
						inner
					} else {
						panic!("error: currently iteration is only supported on arrays.");
					};
					assert!(s.names.len() == 1);
					self.new_def(s.names[0].id, *ty);

					let (pair, _) = self.eval_block(&s.block);
					current_pair = self.join_or_fail(current_pair, pair);
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

	fn eval_fn_def(&mut self, node: &FnDef) {
		// TODO: de-dup with eval lambda
		assert!(node.path.is_empty());

		let mut arg_ty = Vec::new();
		for p in &node.body.params {
			let ty = p.ty.clone().expect("Need parameter annotation");
			self.new_def(p.name.id, ty.clone());
			arg_ty.push(ty);
		}

		let ret_annotation = node.body.ty.as_ref().expect("Need return type annotation");
		let fn_ty = Ty::Fn(arg_ty, Box::new(ret_annotation.clone()));
		self.new_def(node.name.id, fn_ty);

		let (mut ret_pair, ret_all) = self.eval_block(&node.body.body);
		// There is an implied 'return nil' at the end of every body
		if !ret_all {
			ret_pair = self.join_or_fail(ret_pair, Some((Ty::Nil, node.name.span)));
		};
		let (ty, prev_span) = ret_pair.unwrap();

		if !subtype(&ty, ret_annotation) {
			let msg = format!("Expected return type `{ret_annotation}`, found `{ty}`.");
			format_err(&msg, prev_span, self.input);
			self.errors.push(msg);
		};
	}

	// span should refer to the place where the function is defined
	// TODO: get span info from AST and remove this argument
	fn eval_lambda(&mut self, node: &FnBody, span: Span) -> Ty {
		// build function type from signature
		let mut arg_ty = Vec::new();
		for p in &node.params {
			let ty = p.ty.clone().expect("Need parameter annotation");
			self.new_def(p.name.id, ty.clone());
			arg_ty.push(ty);
		}

		let ret_annotation = node.ty.as_ref();

		// check return type of the block
		let (mut ret_pair, ret_all) = self.eval_block(&node.body);
		// There is an implied 'return nil' at the end of every body
		if !ret_all {
			ret_pair = self.join_or_fail(ret_pair, Some((Ty::Nil, span)));
		};
		let (ty, prev_span) = ret_pair.unwrap();

		let ret_ty = if let Some(ret_ty) = ret_annotation {
			if !subtype(&ty, ret_ty) {
				let msg = format!("Expected return type `{ret_ty}`, found `{ty}`.");
				format_err(&msg, prev_span, self.input);
				self.errors.push(msg);
			}
			ret_ty
		} else {
			println!("infer fn return: {}", &ty);
			&ty
		};

		Ty::Fn(arg_ty, Box::new(ret_ty.clone()))
	}

	fn eval_fn_call(&mut self, c: &Call) -> Ty {
		let fn_ty = self.eval_expr(&c.expr);
		if let Ty::Fn(params, ret_ty) = fn_ty {
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
					if !subtype(&arg_ty, &p) {
						let msg = format!("Expected argument type `{p}`, found `{arg_ty}`");
						format_err(&msg, a.span, self.input);
						self.errors.push(msg);
					}
				}
			}
			*ret_ty.clone()
		} else {
			let msg = format!("Type `{fn_ty}` is not callable.");
			format_err(&msg, c.expr.span, self.input);
			self.errors.push(msg);
			Ty::Bottom
		}
	}

	fn eval_expr(&mut self, expr: &Expr) -> Ty {
		let ty = self.eval_expr_inner(expr);
		println!("infer {}: {}", &expr.span.as_str(self.input), &ty);
		ty
	}

	fn eval_expr_inner(&mut self, expr: &Expr) -> Ty {
		match &expr.kind {
			ExprKind::BinExpr(e) => {
				let lhs = self.eval_expr(&e.lhs);
				let rhs = self.eval_expr(&e.rhs);
				match e.op {
					BinOp::Plus | BinOp::Minus | BinOp::Mul | BinOp::Pow | BinOp::Mod => {
						if subtype(&lhs, &Ty::Int) && subtype(&rhs, &Ty::Int) {
							return Ty::Int;
						}
						if subtype(&lhs, &Ty::Num) && subtype(&rhs, &Ty::Num) {
							return Ty::Num;
						}
					},
					BinOp::Div => {
						if subtype(&lhs, &Ty::Num) && subtype(&rhs, &Ty::Num) {
							return Ty::Num;
						}
					},
					BinOp::Gt | BinOp::Lt | BinOp::Gte | BinOp::Lte => {
						if subtype(&lhs, &Ty::Num) && subtype(&rhs, &Ty::Num) {
							return Ty::Bool;
						}
						if subtype(&lhs, &Ty::Str) && subtype(&rhs, &Ty::Str) {
							return Ty::Bool;
						}
					},
					BinOp::Concat => {
						if subtype(&lhs, &Ty::Str) && subtype(&rhs, &Ty::Str) {
							return Ty::Bool;
						}
					},
					BinOp::And | BinOp::Or => {
						if subtype(&lhs, &Ty::Bool) && subtype(&rhs, &Ty::Bool) {
							return Ty::Bool;
						}
					},
					BinOp::Eq | BinOp::Neq => {
						if lhs == rhs {
							return Ty::Bool;
						}
					},
				}
				let msg = format!("Operator `{}` cannot by applied to `{}` and `{}`.", e.op, lhs, rhs);
				format_err(&msg, expr.span, self.input);
				self.errors.push(msg);
				Ty::Bottom
			},
			ExprKind::UnExpr(e) => {
				let ty = self.eval_expr(&e.expr);
				match e.op {
					UnOp::Minus => {
						if subtype(&ty, &Ty::Int) {
							return Ty::Int;
						}
						if subtype(&ty, &Ty::Num) {
							return Ty::Num;
						}
					},
					UnOp::Not => {
						if subtype(&ty, &Ty::Bool) {
							return Ty::Bool;
						}
					},
				}
				let msg = format!("Operator `{}` cannot by applied to `{}`.", e.op, ty);
				format_err(&msg, expr.span, self.input);
				self.errors.push(msg);
				Ty::Bottom
			},
			ExprKind::Array(array) => {
				// consider empty array `[]` to have type [Bottom]
				let mut ty = Ty::Bottom;
				for e in array {
					let new_ty = self.eval_expr(e);
					let try_join = join(&ty, &new_ty);
					ty = if let Some(joined) = try_join {
						joined
					} else {
						let msg = format!("Incompatible types in array: `{new_ty}` and `{ty}`.");
						format_err(&msg, e.span, self.input);
						self.errors.push(msg);
						return Ty::Bottom;
					}
				}
				Ty::Array(Box::new(ty))
			},
			ExprKind::Name(e) => {
				let ty_opt = self.lookup(e.id);
				if let Some(ty) = ty_opt {
					ty.clone()
				} else {
					let msg = format!("couldn't find type for `{}`", expr.span.as_str(self.input));
					format_err(&msg, e.span, self.input);
					panic!("{}", &msg);
				}
			},
			ExprKind::SuffixExpr(e, s) => self.eval_suffix_expr(e, s),
			ExprKind::Literal(e) => self.eval_literal(e),
			ExprKind::Call(e) => self.eval_fn_call(e),
			ExprKind::Expr(e) => self.eval_expr(e),
			ExprKind::Lambda(e) => self.eval_lambda(e, expr.span),
			e => unimplemented!("{e:?}"),
		}
	}

	fn eval_suffix_expr(&mut self, expr: &Expr, s: &Vec<Suffix>) -> Ty {
		let mut ty = self.eval_expr(expr);
		for suffix in s {
			match suffix {
				Suffix::Property(_) => todo!(),
				Suffix::Index(e) => {
					ty = match ty {
						Ty::Array(inner_ty) => *inner_ty,
						Ty::Bottom => Ty::Bottom,
						_ => {
							let msg = format!("Can not index type `{ty}`.");
							format_err(&msg, expr.span, self.input);
							self.errors.push(msg);
							Ty::Bottom
						},
					};
					let index_ty = self.eval_expr(e);
					if !subtype(&index_ty, &Ty::Int) {
						let msg = format!("Index must be type `int` but found `{index_ty}`.");
						format_err(&msg, e.span, self.input);
						self.errors.push(msg);
					}
				},
			}
		}
		ty
	}

	fn eval_literal(&self, l: &Literal) -> Ty {
		match l {
			Literal::Nil => Ty::Nil,
			Literal::Bool(_) => Ty::Bool,
			Literal::Num(_) => Ty::Num,
			Literal::Int(_) => Ty::Int,
			Literal::Str(_) => Ty::Str,
		}
	}

	fn eval_let(&mut self, node: &Let) {
		assert!(node.exprs.len() == node.names.len());

		let mut rhs = Vec::new();
		for e in &node.exprs {
			rhs.push(self.eval_expr(e).clone());
		}
		for (n, rhs_ty) in zip(&node.names, rhs) {
			// check if annotation fits
			if let Some(ty) = &n.ty {
				if !subtype(&rhs_ty, ty) {
					let msg = format!("Type error, assigning `{rhs_ty}` to `{ty}`.");
					format_err(&msg, node.span, self.input);
					self.errors.push(msg);
				}
				self.new_def(n.name.id, ty.clone());
			} else {
				self.new_def(n.name.id, rhs_ty);
			}
		}
	}

	fn eval_lvalue(&mut self, var: &Expr) -> Ty {
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
			if !subtype(&rhs_ty, &ty) {
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

		if lhs == Ty::Bottom {
			return;
		}

		match node.op {
			BinOp::Plus | BinOp::Minus | BinOp::Mul | BinOp::Pow | BinOp::Mod => {
				if lhs == Ty::Int && subtype(&rhs, &Ty::Int) {
					return;
				}
				if lhs == Ty::Num && subtype(&rhs, &Ty::Num) {
					return;
				}
			},
			BinOp::Div => {
				if lhs == Ty::Num && subtype(&rhs, &Ty::Num) {
					return;
				}
			},
			BinOp::Concat => {
				if subtype(&lhs, &Ty::Str) && subtype(&rhs, &Ty::Str) {
					return;
				}
			},
			_ => unreachable!(),
		}
		let msg = format!("Operator `{}` cannot by applied to `{}` and `{}`.", node.op, lhs, rhs);
		format_err(&msg, node.span, self.input);
		self.errors.push(msg);
	}
}
