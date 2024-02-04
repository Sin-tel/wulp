use crate::ast::*;

pub trait Visitor: Sized {
	// entry point
	fn visit_block(&mut self, block: &mut Block) {
		walk_block(self, block);
	}

	fn visit_stat(&mut self, stat: &mut Stat) {
		walk_stat(self, stat);
	}
	fn visit_local_assignment(&mut self, local_assignment: &mut LocalAssignment) {
		walk_local_assignment(self, local_assignment);
	}
	fn visit_function_call(&mut self, function_call: &mut FunctionCall) {
		walk_function_call(self, function_call);
	}
	fn visit_expr(&mut self, expr: &mut Expr) {
		walk_expr(self, expr);
	}
	fn visit_prefix_expr(&mut self, expr: &mut PrefixExpr) {
		walk_prefix_expr(self, expr);
	}
	fn visit_var(&mut self, var: &mut Var) {
		walk_var(self, var);
	}
	fn visit_name(&mut self, _name: &mut Name) {
		// leaf node
	}
}

pub fn walk_block<V: Visitor>(v: &mut V, block: &mut Block) {
	for s in block.stats.iter_mut() {
		v.visit_stat(s);
	}
}

pub fn walk_stat<V: Visitor>(v: &mut V, stat: &mut Stat) {
	match stat {
		Stat::LocalAssignment(s) => v.visit_local_assignment(s),
		Stat::FunctionCall(s) => v.visit_function_call(s),
		Stat::Return(exprs) => {
			for e in exprs.iter_mut() {
				v.visit_expr(e)
			}
		},
		_ => unimplemented!(),
	}
}

pub fn walk_local_assignment<V: Visitor>(v: &mut V, local_assignment: &mut LocalAssignment) {
	for name in local_assignment.names.iter_mut() {
		v.visit_name(name);
	}
	for expr in local_assignment.exprs.iter_mut() {
		v.visit_expr(expr);
	}
}

pub fn walk_function_call<V: Visitor>(v: &mut V, function_call: &mut FunctionCall) {
	v.visit_prefix_expr(&mut function_call.expr);
	for e in function_call.args.iter_mut() {
		v.visit_expr(e);
	}
}

pub fn walk_expr<V: Visitor>(v: &mut V, expr: &mut Expr) {
	match expr {
		Expr::PrefixExp(e) => v.visit_prefix_expr(e),
		Expr::FuncDef(_) => unimplemented!(),
		Expr::Table(_) => unimplemented!(),
		Expr::BinExp(_) => unimplemented!(),
		Expr::UnExp(_) => unimplemented!(),
		_ => (),
	}
}

pub fn walk_prefix_expr<V: Visitor>(v: &mut V, expr: &mut PrefixExpr) {
	match expr {
		PrefixExpr::Var(e) => v.visit_var(e),
		PrefixExpr::FunctionCall(_) => unimplemented!(),
		PrefixExpr::Expr(_) => unimplemented!(),
	}
}
pub fn walk_var<V: Visitor>(v: &mut V, var: &mut Var) {
	match var {
		Var::Name(e) => v.visit_name(e),
		Var::IndexExpr(_) => unimplemented!(),
		Var::Property(_) => unimplemented!(),
	}
}
