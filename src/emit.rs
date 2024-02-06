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
		block.visit(&mut this);
		this.code
	}
	fn comma_list_expr(&mut self, list: &mut [Expr]) {
		if let Some((last, elements)) = list.split_last_mut() {
			for e in elements {
				self.visit_expr(e);
				self.code.push_str(", ");
			}
			self.visit_expr(last);
		}
	}
	fn comma_list_name(&mut self, list: &mut [Name]) {
		if let Some((last, elements)) = list.split_last_mut() {
			for e in elements {
				self.visit_name(e);
				self.code.push_str(", ");
			}
			self.visit_name(last);
		}
	}
	fn indent(&mut self) {
		self.code.push_str(&"  ".repeat(self.indent_level));
	}
}

impl Visitor for EmitLua {
	fn visit_block(&mut self, node: &mut Block) {
		self.indent_level += 1;
		node.visit(self);
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
		self.comma_list_expr(&mut node.vars);
		self.code.push_str(" = ");
		self.comma_list_expr(&mut node.exprs);
	}
	fn visit_function_call(&mut self, node: &mut FunctionCall) {
		node.visit(self);
	}
	fn visit_stat(&mut self, node: &mut Stat) {
		self.indent();
		match node {
			Stat::Return(exprs) => {
				self.code.push_str("return ");
				self.comma_list_expr(exprs);
			},
			_ => {
				node.visit(self);
			},
		};
		self.code.push('\n');
	}
	fn visit_expr(&mut self, node: &mut Expr) {
		match node {
			Expr::Nil => self.code.push_str("nil"),
			Expr::Num(s) => self.code.push_str(&s.to_string()),
			Expr::Str(s) => {
				// TODO: this is kind of a hack lol
				self.code.push_str(&format!("{s:?}"));
			},
			Expr::Bool(s) => self.code.push_str(&s.to_string()),
			Expr::Lambda(_) => {
				self.code.push_str("function");
				node.visit(self);
			},
			Expr::Name(_) => node.visit(self),
			Expr::SuffixExpr(_) => node.visit(self),
			Expr::BinExp(_) => node.visit(self),
			e => unimplemented!("{e:?}"),
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
		node.visit(self);
	}

	fn visit_suffix(&mut self, node: &mut Suffix) {
		match node {
			Suffix::Property(_) => {
				self.code.push('.');
				node.visit(self);
			},
			Suffix::Index(_) => unimplemented!(),
			Suffix::Call(args) => {
				self.code.push('(');
				self.comma_list_expr(args);
				self.code.push(')');
			},
		};
	}

	fn visit_function_body(&mut self, node: &mut FuncBody) {
		self.code.push('(');
		self.comma_list_name(&mut node.params);
		self.code.push_str(")\n");

		// node.visit(self);
		self.visit_block(&mut node.body);

		self.code.push_str("end");
	}

	fn visit_field(&mut self, node: &mut Field) {
		node.visit(self);
	}

	fn visit_name(&mut self, node: &mut Name) {
		self.code.push_str(&node.0);
	}
}
