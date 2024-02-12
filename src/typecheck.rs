use crate::ast::*;
use crate::span::format_err;
use crate::symbol::SymbolTable;
use crate::visitor::Visitor;
use std::cmp::Ordering;
use std::iter::zip;

pub struct TypeCheck<'a> {
	input: &'a str,
	symbol_table: &'a mut SymbolTable,
	errors: Vec<String>,
}

impl<'a> TypeCheck<'a> {
	pub fn check(ast: &mut File, input: &'a str, symbol_table: &'a mut SymbolTable) -> Result<(), String> {
		let mut this = Self {
			input,
			symbol_table,
			errors: Vec::new(),
		};

		this.visit_file(ast);

		match this.errors.last() {
			Some(err) => Err(err.to_string()),
			None => Ok(()),
		}
	}

	fn infer_expr(&mut self, expr: &mut Expr) -> Ty {
		match &mut expr.kind {
			ExprKind::BinExpr(e) => {
				let lhs = self.infer_expr(&mut e.lhs);
				let rhs = self.infer_expr(&mut e.rhs);
				match e.op {
					BinOp::Plus | BinOp::Minus | BinOp::Mul | BinOp::Pow | BinOp::Mod => {
						if lhs <= Ty::Int && rhs <= Ty::Int {
							return Ty::Int;
						}
						if lhs <= Ty::Num && rhs <= Ty::Num {
							return Ty::Num;
						}
					},
					BinOp::Div => {
						if lhs <= Ty::Num && rhs <= Ty::Num {
							return Ty::Num;
						}
					},
					BinOp::Gt | BinOp::Lt | BinOp::Gte | BinOp::Lte => {
						if lhs <= Ty::Num && rhs <= Ty::Num {
							return Ty::Bool;
						}
					},
					BinOp::Concat => {
						if lhs <= Ty::Str && rhs <= Ty::Str {
							return Ty::Bool;
						}
					},
					BinOp::And | BinOp::Or => {
						if lhs <= Ty::Bool && rhs <= Ty::Bool {
							return Ty::Bool;
						}
					},
					BinOp::Eq | BinOp::Neq => {
						if lhs == rhs {
							return Ty::Bool;
						}
					},
				}
				let msg = format!("Type error for operator `{:?}`: {:?}, {:?}", e.op, &lhs, &rhs);
				format_err(&msg, expr.span, self.input);
				Ty::Bottom
			},
			ExprKind::UnExpr(e) => {
				let ty = self.infer_expr(&mut e.expr);
				match e.op {
					UnOp::Minus => {
						if ty <= Ty::Int {
							return Ty::Int;
						}
						if ty <= Ty::Num {
							return Ty::Num;
						}
					},
					UnOp::Not => {
						if ty <= Ty::Bool {
							return Ty::Bool;
						}
					},
				}
				let msg = format!("Type error for operator `{:?}`: {:?}", e.op, ty);
				format_err(&msg, expr.span, self.input);
				Ty::Bottom
			},
			ExprKind::Name(n) => self.symbol_table.get(n.id).ty.clone(),
			ExprKind::Literal(l) => self.infer_literal(l),
			ExprKind::Call(c) => self.infer_fn_call(c),
			ExprKind::Lambda(l) => self.infer_fn_body(l),
			e => unimplemented!("{e:?}"),
		}
	}

	fn infer_fn_body(&mut self, node: &mut FnBody) -> Ty {
		// first typecheck the body
		self.visit_block(&mut node.body);

		// build function type from signature
		let arg_ty = node.params.iter().map(|p| p.ty.clone()).collect();
		let fn_ty = Ty::Fn(arg_ty, Box::new(node.ty.clone()));

		// check return type of the block
		self.check_block(&mut node.body, &node.ty);

		fn_ty
	}

	fn infer_fn_call(&mut self, c: &mut Call) -> Ty {
		match &c.expr.kind {
			ExprKind::Name(n) => {
				let fn_ty = &self.symbol_table.get(n.id).ty.clone();
				if let Ty::Fn(params, ret_ty) = fn_ty {
					for (p, a) in zip(params, c.args.iter_mut()) {
						let arg_ty = self.infer_expr(a);
						if !is_subtype(&arg_ty, p) {
							let msg = format!("Expected type {p:?}, but got {arg_ty:?}");
							format_err(&msg, a.span, self.input);
						}
					}
					*ret_ty.clone()
				} else {
					let msg = format!("Type {fn_ty:?} is not callable.");
					format_err(&msg, c.expr.span, self.input);
					Ty::Bottom
				}
			},
			e => unimplemented!("{e:?}"),
		}
	}

	fn check_block(&mut self, b: &mut Block, expect: &Ty) {
		for stat in &mut b.stats {
			match stat {
				Stat::Return(exprs) => {
					// TODO: if is empty then it returns nil
					assert!(exprs.len() == 1);
					let other = self.infer_expr(&mut exprs[0]);
					if !is_subtype(&other, expect) {
						let msg = format!("Expected return type {expect:?}, but found {other:?}");
						format_err(&msg, exprs[0].span, self.input);
					}
					return;
				},
				Stat::Block(_) => unimplemented!(),
				Stat::WhileBlock(_) => unimplemented!(),
				Stat::IfBlock(_) => {
					// todo!()
				},
				Stat::ForBlock(_) => unimplemented!(),
				_ => (),
			}
		}

		if !is_subtype(expect, &Ty::Nil) {
			let msg = format!("Expected return type {expect:?}, but found Nil");
			// format_err(&msg, node.name.span, self.input);
			// TODO: get span
			eprintln!("error: {}", &msg);
		}
	}

	fn infer_literal(&self, l: &Literal) -> Ty {
		match l {
			Literal::Nil => Ty::Nil,
			Literal::Bool(_) => Ty::Bool,
			Literal::Num(_) => Ty::Num,
			Literal::Int(_) => Ty::Int,
			Literal::Str(_) => Ty::Str,
		}
	}
}

impl<'a> Visitor for TypeCheck<'a> {
	fn visit_fn_call(&mut self, node: &mut Call) {
		// ignore return type
		self.infer_fn_call(node);
	}

	fn visit_fn_def(&mut self, node: &mut FnDef) {
		assert!(node.path.is_empty());
		self.infer_fn_body(&mut node.body);
	}

	fn visit_assignment(&mut self, node: &mut Assignment) {
		assert!(node.exprs.len() == node.vars.len());

		let mut rhs = Vec::new();
		for e in &mut node.exprs {
			rhs.push(self.infer_expr(e).clone());
		}

		for (var, ty) in zip(&mut node.vars, rhs) {
			match var {
				Var::Expr(e) => {
					if let Expr {
						span: _,
						kind: ExprKind::Name(n),
						..
					} = e
					{
						// TODO: check if types are compatible
						self.symbol_table.get_mut(n.id).ty = ty;
					} else {
						todo!();
					}
				},
				Var::Typed(t) => {
					// ty is already in symbol table from scope pass
					if !is_subtype(&ty, &t.ty) {
						let msg = format!("Type error, assigning {:?} to {:?}.", ty, t.ty);
						format_err(&msg, node.span, self.input);
					}
				},
			};
		}
	}

	fn visit_expr(&mut self, expr: &mut Expr) {
		let ty = self.infer_expr(expr);
		println!("infer: {} -> {:?}", &expr.span.as_str(self.input), &ty);
	}
}

// Defines the subtype relationship
// The partial order `X <= Y` means `X` is a subtype of `Y`
fn is_subtype(a: &Ty, b: &Ty) -> bool {
	match a.partial_cmp(b) {
		None | Some(Ordering::Greater) => false,
		_ => true,
	}
}

impl PartialOrd for Ty {
	fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
		use Ordering::*;
		if self == other {
			Some(Equal)
		} else {
			#[allow(clippy::match_same_arms)]
			match (self, other) {
				// The bottom type is a subtype of all other types
				(Ty::Bottom, _) => Some(Less),
				(_, Ty::Bottom) => Some(Greater),

				// All types are a subtype of Any
				(_, Ty::Any) => Some(Less),
				(Ty::Any, _) => Some(Greater),

				// Int is a subtype of Num
				(Ty::Int, Ty::Num) => Some(Less),
				(Ty::Num, Ty::Int) => Some(Greater),
				_ => None,
			}
		}
	}
}
