use crate::ast::*;

pub trait Visitor: Sized {
	fn visit_block(&mut self, node: &mut Block) {
		node.walk(self);
	}
	fn visit_stat(&mut self, node: &mut Stat) {
		node.walk(self);
	}
	fn visit_assignment(&mut self, node: &mut Assignment) {
		node.walk(self);
	}
	fn visit_fn_def(&mut self, node: &mut FnDef) {
		node.walk(self);
	}
	fn visit_fn_call(&mut self, node: &mut Call) {
		node.walk(self);
	}
	fn visit_if_block(&mut self, node: &mut IfBlock) {
		node.walk(self);
	}
	fn visit_for_block(&mut self, node: &mut ForBlock) {
		node.walk(self);
	}
	fn visit_while_block(&mut self, node: &mut WhileBlock) {
		node.walk(self);
	}
	fn visit_fn_body(&mut self, node: &mut FnBody) {
		node.walk(self);
	}
	fn visit_expr(&mut self, node: &mut Expr) {
		node.walk(self);
	}
	fn visit_suffix(&mut self, node: &mut Suffix) {
		node.walk(self);
	}
	fn visit_field(&mut self, node: &mut Field) {
		node.walk(self);
	}
	// leaf nodes
	fn visit_name(&mut self, _node: &mut Name) {}
	fn visit_property(&mut self, _node: &mut Property) {}
	fn visit_literal(&mut self, _node: &mut Literal) {}
}

pub trait VisitNode<V: Visitor> {
	fn walk(&mut self, _: &mut V) {
		unreachable!()
	}
	fn visit(&mut self, _: &mut V) {
		unreachable!()
	}
}

impl<V: Visitor> VisitNode<V> for Block {
	fn visit(&mut self, v: &mut V) {
		v.visit_block(self);
	}
	fn walk(&mut self, v: &mut V) {
		for s in &mut self.stats {
			v.visit_stat(s);
		}
	}
}

impl<V: Visitor> VisitNode<V> for Stat {
	fn visit(&mut self, v: &mut V) {
		v.visit_stat(self);
	}
	fn walk(&mut self, v: &mut V) {
		match self {
			Stat::Assignment(s) => v.visit_assignment(s),
			Stat::FnDef(s) => v.visit_fn_def(s),
			Stat::Call(s) => v.visit_fn_call(s),
			Stat::IfBlock(s) => v.visit_if_block(s),
			Stat::ForBlock(s) => v.visit_for_block(s),
			Stat::Block(s) => v.visit_block(s),
			Stat::WhileBlock(s) => v.visit_while_block(s),
			Stat::Break => (),
			Stat::Return(exprs) => {
				for e in exprs.iter_mut() {
					v.visit_expr(e);
				}
			},
		}
	}
}

impl<V: Visitor> VisitNode<V> for IfBlock {
	fn visit(&mut self, v: &mut V) {
		v.visit_if_block(self);
	}
	fn walk(&mut self, v: &mut V) {
		v.visit_expr(&mut self.expr);
		v.visit_block(&mut self.block);
		for e in &mut self.elseif {
			v.visit_expr(&mut e.expr);
			v.visit_block(&mut e.block);
		}
		if let Some(b) = &mut self.else_block {
			v.visit_block(b);
		}
	}
}

impl<V: Visitor> VisitNode<V> for ForBlock {
	fn visit(&mut self, v: &mut V) {
		v.visit_for_block(self);
	}
	fn walk(&mut self, v: &mut V) {
		for n in &mut self.names {
			v.visit_name(n);
		}
		for e in &mut self.exprs {
			v.visit_expr(e);
		}
		v.visit_block(&mut self.block);
	}
}

impl<V: Visitor> VisitNode<V> for WhileBlock {
	fn visit(&mut self, v: &mut V) {
		v.visit_while_block(self);
	}
	fn walk(&mut self, v: &mut V) {
		v.visit_expr(&mut self.expr);
		v.visit_block(&mut self.block);
	}
}

impl<V: Visitor> VisitNode<V> for Assignment {
	fn visit(&mut self, v: &mut V) {
		v.visit_assignment(self);
	}
	fn walk(&mut self, v: &mut V) {
		for e in &mut self.vars {
			v.visit_expr(e);
		}
		for e in &mut self.exprs {
			v.visit_expr(e);
		}
	}
}

impl<V: Visitor> VisitNode<V> for FnDef {
	fn visit(&mut self, v: &mut V) {
		v.visit_fn_def(self);
	}
	fn walk(&mut self, v: &mut V) {
		v.visit_name(&mut self.name);
		for p in &mut self.path {
			v.visit_property(p);
		}
		v.visit_fn_body(&mut self.body);
	}
}

impl<V: Visitor> VisitNode<V> for Call {
	fn visit(&mut self, v: &mut V) {
		v.visit_fn_call(self);
	}
	fn walk(&mut self, v: &mut V) {
		v.visit_expr(&mut self.expr);
		for e in &mut self.args {
			v.visit_expr(e);
		}
	}
}

impl<V: Visitor> VisitNode<V> for FnBody {
	fn visit(&mut self, v: &mut V) {
		v.visit_fn_body(self);
	}
	fn walk(&mut self, v: &mut V) {
		for e in &mut self.params {
			v.visit_name(e);
		}
		v.visit_block(&mut self.body);
	}
}

impl<V: Visitor> VisitNode<V> for Expr {
	fn visit(&mut self, v: &mut V) {
		v.visit_expr(self);
	}
	fn walk(&mut self, v: &mut V) {
		match &mut self.kind {
			ExprKind::Literal(e) => v.visit_literal(e),
			ExprKind::Name(e) => v.visit_name(e),
			ExprKind::Lambda(e) => v.visit_fn_body(e),
			ExprKind::Expr(e) => v.visit_expr(e),
			ExprKind::Call(e) => v.visit_fn_call(e),
			ExprKind::BinExpr(e) => {
				v.visit_expr(&mut e.lhs);
				v.visit_expr(&mut e.rhs);
			},
			ExprKind::UnExpr(e) => {
				v.visit_expr(&mut e.expr);
			},
			ExprKind::SuffixExpr(e, suffix) => {
				v.visit_expr(e);
				for s in suffix {
					v.visit_suffix(s);
				}
			},
			ExprKind::Table(t) => {
				for e in t {
					v.visit_field(e);
				}
			},
		}
	}
}

impl<V: Visitor> VisitNode<V> for Suffix {
	fn visit(&mut self, v: &mut V) {
		v.visit_suffix(self);
	}
	fn walk(&mut self, v: &mut V) {
		match self {
			Suffix::Property(e) => v.visit_property(e),
			Suffix::Index(e) => v.visit_expr(e),
		}
	}
}

impl<V: Visitor> VisitNode<V> for Field {
	fn visit(&mut self, v: &mut V) {
		v.visit_field(self);
	}
	fn walk(&mut self, v: &mut V) {
		match self {
			Field::Assign(p, e) => {
				v.visit_property(p);
				v.visit_expr(e);
			},
			Field::Expr(e) => v.visit_expr(e),
			Field::Fn(p, f) => {
				v.visit_property(p);
				v.visit_fn_body(f);
			},
		}
	}
}

impl<V: Visitor> VisitNode<V> for Name {
	fn visit(&mut self, v: &mut V) {
		v.visit_name(self);
	}
}

impl<V: Visitor> VisitNode<V> for Property {
	fn visit(&mut self, v: &mut V) {
		v.visit_property(self);
	}
}
