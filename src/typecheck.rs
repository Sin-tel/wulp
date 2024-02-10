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

	fn eval_expr(&self, expr: &mut Expr) -> &Ty {
		match &mut expr.kind {
			ExprKind::BinExpr(e) => {
				let lhs = self.eval_expr(&mut e.lhs);
				let rhs = self.eval_expr(&mut e.rhs);
				let ty = if let Some(ty) = coerce(lhs, rhs) {
					ty
				} else {
					let msg = format!("Type error for operator `{:?}`: {:?}, {:?}", e.op, lhs, rhs);
					format_err(&msg, expr.span, self.input);
					return &Ty::Bottom;
				};
				if ty == &Ty::Bottom {
					return &Ty::Bottom;
				}
				match e.op {
					BinOp::Plus | BinOp::Minus | BinOp::Mul | BinOp::Div | BinOp::Pow | BinOp::Mod => {
						if ty == &Ty::Number {
							return &Ty::Number;
						}
					},
					BinOp::Gt | BinOp::Lt | BinOp::Gte | BinOp::Lte => {
						if ty == &Ty::Number {
							return &Ty::Bool;
						}
					},
					BinOp::Concat => {
						if ty == &Ty::Str {
							return &Ty::Str;
						}
					},
					BinOp::And | BinOp::Or => {
						if ty == &Ty::Bool {
							return &Ty::Bool;
						}
					},
					BinOp::Eq | BinOp::Neq => {
						return &Ty::Bool;
					},
				}
				let msg = format!("Type error for operator `{:?}`: {:?}, {:?}", e.op, lhs, rhs);
				format_err(&msg, expr.span, self.input);
				&Ty::Bottom
			},
			ExprKind::UnExpr(e) => {
				let ty = self.eval_expr(&mut e.expr);
				if ty == &Ty::Bottom {
					return ty;
				}
				match e.op {
					UnOp::Minus => {
						if ty == &Ty::Number {
							return ty;
						}
					},
					UnOp::Not => {
						if ty == &Ty::Bool {
							return ty;
						}
					},
				}
				let msg = format!("Type error for operator `{:?}`: {:?}", e.op, ty);
				format_err(&msg, expr.span, self.input);
				&Ty::Bottom
			},
			ExprKind::Name(n) => &self.symbol_table.get(n.id).ty,
			ExprKind::Literal(l) => self.eval_literal(l),
			e => unimplemented!("{e:?}"),
		}
	}

	fn eval_literal(&self, l: &Literal) -> &Ty {
		match l {
			Literal::Nil => &Ty::Nil,
			Literal::Bool(_) => &Ty::Bool,
			Literal::Number(_) => &Ty::Number,
			Literal::Str(_) => &Ty::Str,
		}
	}
}

impl<'a> Visitor for TypeCheck<'a> {
	fn visit_assignment(&mut self, node: &mut Assignment) {
		assert!(node.exprs.len() == node.vars.len());

		let mut rhs = Vec::new();
		for e in &mut node.exprs {
			rhs.push(self.eval_expr(e).clone());
		}

		for (var, ty) in zip(&mut node.vars, rhs) {
			if let Expr {
				span: _,
				kind: ExprKind::Name(n),
				..
			} = var
			{
				self.symbol_table.get_mut(n.id).ty = ty;
			} else {
				todo!();
			}
		}
	}

	fn visit_expr(&mut self, expr: &mut Expr) {
		let ty = self.eval_expr(expr);
		println!("expr: {:?}", &expr);
		println!("type: {:?}", &ty);
	}
}

use Ordering::*;

fn coerce<'a>(ty1: &'a Ty, ty2: &'a Ty) -> Option<&'a Ty> {
	let ord = ty1.partial_cmp(ty2);

	match ord {
		Some(o) => match o {
			Greater | Equal => Some(ty1),
			Less => Some(ty2),
		},
		None => None,
	}
}

impl PartialOrd for Ty {
	fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
		if self == other {
			Some(Equal)
		} else {
			match (self, other) {
				(_, Ty::Bottom) => Some(Greater),
				(Ty::Bottom, _) => Some(Less),

				(_, Ty::Any) => Some(Greater),
				(Ty::Any, _) => Some(Less),

				_ => None,
			}
		}
	}
}
