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
	fn visit_prefix_expr(&mut self, node: &mut PrefixExpr) {
		node.visit(self);
	}
	fn visit_var(&mut self, node: &mut Var) {
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
			Stat::Return(exprs) => {
				for e in exprs.iter_mut() {
					v.visit_expr(e);
				}
			},
			s => unimplemented!("{s:?}"),
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
		for var in &mut self.vars {
			v.visit_var(var);
		}
		for expr in &mut self.exprs {
			v.visit_expr(expr);
		}
	}
}

impl<V: Visitor> VisitNode<V> for FunctionCall {
	fn visit(&mut self, v: &mut V) {
		v.visit_prefix_expr(&mut self.expr);
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
			Expr::PrefixExp(e) => v.visit_prefix_expr(e),
			Expr::Lambda(e) => v.visit_function_body(e),
			Expr::Table(e) => unimplemented!("{e:?}"),
			Expr::BinExp(e) => v.visit_bin_expr(e),
			Expr::UnExp(e) => v.visit_un_expr(e),
			_ => (),
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

impl<V: Visitor> VisitNode<V> for PrefixExpr {
	fn visit(&mut self, v: &mut V) {
		match self {
			PrefixExpr::Var(e) => v.visit_var(e),
			PrefixExpr::FunctionCall(e) => v.visit_function_call(e),
			PrefixExpr::Expr(e) => v.visit_expr(e),
		}
	}
}

impl<V: Visitor> VisitNode<V> for Var {
	fn visit(&mut self, v: &mut V) {
		match self {
			Var::Name(e) => v.visit_name(e),
			Var::IndexExpr(e) => unimplemented!("{e:?}"),
			Var::Property(e) => unimplemented!("{e:?}"),
		}
	}
}
