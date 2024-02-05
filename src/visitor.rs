use crate::ast::*;

pub trait Visitor: Sized {
	fn visit_block(&mut self, node: &mut Block) {
		node.visit(self);
	}
	fn visit_stat(&mut self, node: &mut Stat) {
		node.visit(self);
	}
	fn visit_local_assignment(&mut self, node: &mut LocalAssignment) {
		node.visit(self);
	}
	fn visit_assignment(&mut self, node: &mut Assignment) {
		node.visit(self);
	}
	fn visit_function_call(&mut self, node: &mut FunctionCall) {
		node.visit(self);
	}
	fn visit_if_block(&mut self, node: &mut IfBlock) {
		node.visit(self);
	}
	fn visit_function_body(&mut self, node: &mut FuncBody) {
		node.visit(self);
	}
	fn visit_expr(&mut self, node: &mut Expr) {
		node.visit(self);
	}
	fn visit_bin_expr(&mut self, node: &mut BinExp) {
		node.visit(self);
	}
	fn visit_un_expr(&mut self, node: &mut UnExp) {
		node.visit(self);
	}
	fn visit_suffix_expr(&mut self, node: &mut SuffixExpr) {
		node.visit(self);
	}
	fn visit_suffix(&mut self, node: &mut Suffix) {
		node.visit(self);
	}
	fn visit_field(&mut self, node: &mut Field) {
		node.visit(self);
	}
	fn visit_name(&mut self, _node: &mut Name) {}
}

pub trait VisitNode<V: Visitor> {
	fn visit(&mut self, v: &mut V);
}

impl<V: Visitor> VisitNode<V> for Block {
	fn visit(&mut self, v: &mut V) {
		for s in &mut self.stats {
			v.visit_stat(s);
		}
	}
}

impl<V: Visitor> VisitNode<V> for Stat {
	fn visit(&mut self, v: &mut V) {
		match self {
			Stat::LocalAssignment(s) => v.visit_local_assignment(s),
			Stat::Assignment(s) => v.visit_assignment(s),
			Stat::FunctionCall(s) => v.visit_function_call(s),
			Stat::IfBlock(s) => v.visit_if_block(s),
			Stat::Return(exprs) => {
				for e in exprs.iter_mut() {
					v.visit_expr(e);
				}
			},
			s => unimplemented!("{s:?}"),
		}
	}
}

impl<V: Visitor> VisitNode<V> for IfBlock {
	fn visit(&mut self, v: &mut V) {
		v.visit_expr(&mut self.expr);
		v.visit_block(&mut self.block);
		for _e in &mut self.elseif {
			unimplemented!();
		}
		if let Some(b) = &mut self.else_block {
			v.visit_block(b);
		}
	}
}

impl<V: Visitor> VisitNode<V> for LocalAssignment {
	fn visit(&mut self, v: &mut V) {
		for name in &mut self.names {
			v.visit_name(name);
		}
		for expr in &mut self.exprs {
			v.visit_expr(expr);
		}
	}
}

impl<V: Visitor> VisitNode<V> for Assignment {
	fn visit(&mut self, v: &mut V) {
		for e in &mut self.vars {
			v.visit_expr(e);
		}
		for e in &mut self.exprs {
			v.visit_expr(e);
		}
	}
}

impl<V: Visitor> VisitNode<V> for FunctionCall {
	fn visit(&mut self, v: &mut V) {
		v.visit_expr(&mut self.expr);
		for e in &mut self.args {
			v.visit_expr(e);
		}
	}
}

impl<V: Visitor> VisitNode<V> for FuncBody {
	fn visit(&mut self, v: &mut V) {
		for e in &mut self.params {
			v.visit_name(e);
		}
		v.visit_block(&mut self.body);
	}
}

impl<V: Visitor> VisitNode<V> for Expr {
	fn visit(&mut self, v: &mut V) {
		match self {
			Expr::Lambda(e) => v.visit_function_body(e),
			Expr::BinExp(e) => v.visit_bin_expr(e),
			Expr::UnExp(e) => v.visit_un_expr(e),
			Expr::Name(e) => v.visit_name(e),
			Expr::SuffixExpr(e) => v.visit_suffix_expr(e),
			Expr::Table(t) => {
				for e in t {
					v.visit_field(e);
				}
			},
			e => unimplemented!("{e:?}"),
		}
	}
}

impl<V: Visitor> VisitNode<V> for BinExp {
	fn visit(&mut self, v: &mut V) {
		v.visit_expr(&mut self.lhs);
		v.visit_expr(&mut self.rhs);
	}
}

impl<V: Visitor> VisitNode<V> for UnExp {
	fn visit(&mut self, v: &mut V) {
		v.visit_expr(&mut self.exp);
	}
}

impl<V: Visitor> VisitNode<V> for SuffixExpr {
	fn visit(&mut self, v: &mut V) {
		v.visit_expr(&mut self.expr);
		for s in &mut self.suffix {
			v.visit_suffix(s);
		}
	}
}

impl<V: Visitor> VisitNode<V> for Suffix {
	fn visit(&mut self, v: &mut V) {
		match self {
			Suffix::Property(e) => v.visit_name(e),
			Suffix::Index(e) => v.visit_expr(e),
			Suffix::Call(args) => {
				for e in args {
					v.visit_expr(e);
				}
			},
		}
	}
}

impl<V: Visitor> VisitNode<V> for Field {
	fn visit(&mut self, v: &mut V) {
		match self {
			Field::Assign(n, e) => {
				v.visit_name(n);
				v.visit_expr(e);
			},
			Field::Expr(e) => v.visit_expr(e),
			Field::ExprAssign(_, _) => unimplemented!(),
		}
	}
}
