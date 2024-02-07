use crate::ast::*;
use crate::visitor::*;

pub struct EmitLua {
	code: String,
	indent_level: usize,
}

impl EmitLua {
	pub fn emit(block: &mut Block) -> String {
		let mut this = Self {
			code: String::new(),
			indent_level: 0,
		};
		// this.visit_block(block);
		block.walk(&mut this);
		this.code
	}

	fn push_list<V>(&mut self, list: &mut [V], punctuation: &str)
	where
		V: VisitNode<Self>,
	{
		let mut list = list.iter_mut();
		if let Some(e) = list.next() {
			e.visit(self);
			for e in list {
				self.code.push_str(punctuation);
				e.visit(self);
			}
		}
	}
	fn indent(&mut self) {
		self.code.push_str(&"  ".repeat(self.indent_level));
	}
}

impl Visitor for EmitLua {
	fn visit_block(&mut self, node: &mut Block) {
		self.indent_level += 1;
		node.walk(self);
		self.indent_level -= 1;
	}
	fn visit_if_block(&mut self, node: &mut IfBlock) {
		self.code.push_str("if ");
		self.visit_expr(&mut node.expr);
		self.code.push_str(" then\n");

		self.visit_block(&mut node.block);

		if let Some(b) = &mut node.else_block {
			self.indent();
			self.code.push_str("else\n");

			self.visit_block(b);
		}
		self.indent();
		self.code.push_str("end");
	}
	fn visit_assignment(&mut self, node: &mut Assignment) {
		if node.local {
			self.code.push_str("local ");
		}
		self.push_list(&mut node.vars, ", ");
		if node.exprs.len() > 0 {
			self.code.push_str(" = ");
			self.push_list(&mut node.exprs, ", ");
		}
	}
	fn visit_function_def(&mut self, node: &mut FunctionDef) {
		if node.local {
			self.code.push_str("local ");
		}
		self.code.push_str("function ");

		self.push_list(&mut node.name, ".");
		self.visit_function_body(&mut node.body);
	}
	fn visit_function_call(&mut self, node: &mut Call) {
		self.visit_expr(&mut node.expr);
		self.code.push('(');
		self.push_list(&mut node.args, ", ");
		self.code.push(')');
	}
	fn visit_stat(&mut self, node: &mut Stat) {
		self.indent();
		match node {
			Stat::Return(exprs) => {
				self.code.push_str("return ");
				self.push_list(exprs, ", ");
			},
			_ => {
				node.walk(self);
			},
		};
		self.code.push(';');
		self.code.push('\n');
	}
	fn visit_expr(&mut self, node: &mut Expr) {
		match node {
			Expr::Lambda(_) => {
				self.code.push_str("function");
				node.walk(self);
			},
			Expr::Expr(_) => {
				self.code.push('(');
				node.walk(self);
				self.code.push(')');
			},
			Expr::Table(t) => {
				self.code.push('{');
				self.push_list(t, ", ");
				self.code.push('}');
			},
			_ => node.walk(self),
		}
	}
	fn visit_literal(&mut self, node: &mut Literal) {
		match node {
			Literal::Nil => self.code.push_str("nil"),
			Literal::Number(s) => self.code.push_str(&s.to_string()),
			Literal::Str(s) => {
				// TODO: this is kind of a hack lol
				self.code.push_str(&format!("{s:?}"));
			},
			Literal::Bool(s) => self.code.push_str(&s.to_string()),
		}
	}
	fn visit_bin_expr(&mut self, node: &mut BinExp) {
		self.visit_expr(&mut node.lhs);
		self.code.push(' ');
		self.code.push_str(&node.op.to_string());
		self.code.push(' ');
		self.visit_expr(&mut node.rhs);
	}
	fn visit_un_expr(&mut self, node: &mut UnExp) {
		self.code.push_str(&node.op.to_string());
		self.visit_expr(&mut node.exp);
	}
	fn visit_suffix_expr(&mut self, node: &mut SuffixExpr) {
		node.walk(self);
	}
	fn visit_suffix(&mut self, node: &mut Suffix) {
		match node {
			Suffix::Property(_) => {
				self.code.push('.');
				node.walk(self);
			},
			Suffix::Index(_) => {
				self.code.push('[');
				node.walk(self);
				self.code.push(']');
			},
		};
	}

	fn visit_function_body(&mut self, node: &mut FuncBody) {
		self.code.push('(');
		self.push_list(&mut node.params, ", ");
		self.code.push_str(")\n");

		self.visit_block(&mut node.body);

		self.indent();
		self.code.push_str("end");
	}

	fn visit_field(&mut self, node: &mut Field) {
		match node {
			Field::Assign(n, e) => {
				self.visit_name(n);
				self.code.push_str(" = ");
				self.visit_expr(e);
			},
			Field::Expr(e) => {
				self.visit_expr(e);
			},
		}
	}

	fn visit_name(&mut self, node: &mut Name) {
		self.code.push_str(&node.0);
	}
}
