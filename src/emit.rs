use crate::ast::*;
use crate::symbol::SymbolTable;
use crate::visitor::*;

pub struct EmitLua {
	code: String,
	indent_level: usize,
	symbol_table: SymbolTable,
}

impl EmitLua {
	pub fn emit(ast: &mut File, symbol_table: SymbolTable) -> String {
		let mut this = Self {
			code: String::new(),
			indent_level: 0,
			symbol_table,
		};
		this.visit_file(ast);
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
		self.code.push_str(&"\t".repeat(self.indent_level));
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

		for elseif in &mut node.elseif {
			self.indent();
			self.code.push_str("elseif ");
			self.visit_expr(&mut elseif.expr);
			self.code.push_str(" then\n");
			self.visit_block(&mut elseif.block);
		}
		if let Some(b) = &mut node.else_block {
			self.indent();
			self.code.push_str("else\n");
			self.visit_block(b);
		}
		self.indent();
		self.code.push_str("end");
	}
	fn visit_while_block(&mut self, node: &mut WhileBlock) {
		self.code.push_str("while ");
		self.visit_expr(&mut node.expr);
		self.code.push_str(" do\n");

		self.visit_block(&mut node.block);

		self.indent();
		self.code.push_str("end");
	}
	fn visit_for_block(&mut self, node: &mut ForBlock) {
		// TODO: emit the correct kind of iterator depending on the type of expr
		self.code.push_str("for ");
		// self.push_list(&mut node.names, ", ");

		assert!(node.names.len() == 1);
		// TODO: for now we only ever emit ipairs. fix up later to work with tables
		self.code.push_str("_, ");
		self.visit_name(&mut node.names[0]);

		// self.code.push_str(" in ");
		self.code.push_str(" in ipairs(");
		self.visit_expr(&mut node.expr);
		self.code.push(')');
		self.code.push_str(" do\n");

		self.visit_block(&mut node.block);

		self.indent();
		self.code.push_str("end");
	}
	fn visit_assignment(&mut self, node: &mut Assignment) {
		if node.new_def {
			self.code.push_str("local ");
		}
		self.push_list(&mut node.vars, ", ");
		if !node.exprs.is_empty() {
			self.code.push_str(" = ");
			self.push_list(&mut node.exprs, ", ");
		}
	}
	fn visit_fn_def(&mut self, node: &mut FnDef) {
		self.code.push_str("local ");
		self.code.push_str("function ");

		self.visit_name(&mut node.name);
		for p in &mut node.path {
			self.code.push('.');
			self.visit_property(p);
		}
		self.visit_fn_body(&mut node.body);
	}
	fn visit_fn_call(&mut self, node: &mut Call) {
		self.visit_expr(&mut node.expr);
		self.code.push('(');
		self.push_list(&mut node.args, ", ");
		self.code.push(')');
	}
	fn visit_stat(&mut self, node: &mut Stat) {
		self.indent();
		match node {
			Stat::Return(ret) => {
				self.code.push_str("return ");
				self.push_list(&mut ret.exprs, ", ");
			},
			Stat::Break => {
				self.code.push_str("break");
			},
			Stat::Block(_) => {
				self.code.push_str("do\n");
				node.walk(self);
				self.indent();
				self.code.push_str("end");
			},
			_ => {
				node.walk(self);
			},
		};
		self.code.push(';');
		self.code.push('\n');
	}
	fn visit_expr(&mut self, node: &mut Expr) {
		match &mut node.kind {
			ExprKind::BinExpr(e) => {
				self.visit_expr(&mut e.lhs);
				self.code.push(' ');
				self.code.push_str(emit_binop(&e.op));
				self.code.push(' ');
				self.visit_expr(&mut e.rhs);
			},
			ExprKind::UnExpr(e) => {
				self.code.push_str(emit_unop(&e.op));
				self.code.push(' ');
				self.visit_expr(&mut e.expr);
			},
			ExprKind::Lambda(_) => {
				self.code.push_str("function");
				node.walk(self);
			},
			ExprKind::Expr(_) => {
				self.code.push('(');
				node.walk(self);
				self.code.push(')');
			},
			ExprKind::Table(t) => {
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
			Literal::Num(s) => self.code.push_str(&s.to_string()),
			Literal::Int(s) => self.code.push_str(&s.to_string()),
			Literal::Str(s) => {
				// TODO: do proper escape sequences
				self.code.push_str(&format!("{s:?}"));
			},
			Literal::Bool(s) => self.code.push_str(&s.to_string()),
		}
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

	fn visit_fn_body(&mut self, node: &mut FnBody) {
		// todo, no newline for short functions
		self.code.push('(');
		self.push_list(&mut node.params, ", ");
		self.code.push_str(")\n");

		self.visit_block(&mut node.body);

		self.indent();
		self.code.push_str("end");
	}

	fn visit_field(&mut self, node: &mut Field) {
		match node {
			Field::Assign(p, e) => {
				self.visit_property(p);
				self.code.push_str(" = ");
				self.visit_expr(e);
			},
			Field::Expr(e) => {
				self.visit_expr(e);
			},
			Field::Fn(p, f) => {
				self.visit_property(p);
				self.code.push_str(" = function");
				self.visit_fn_body(f);
			},
		}
	}

	fn visit_name(&mut self, node: &mut Name) {
		self.code.push_str(&self.symbol_table.get(node.id).name);
	}

	fn visit_property(&mut self, node: &mut Property) {
		self.code.push_str(&node.name);
	}
}

fn emit_binop(op: &BinOp) -> &'static str {
	match op {
		BinOp::Pow => "^",
		BinOp::Mul => "*",
		BinOp::Div => "/",
		BinOp::Mod => "%",
		BinOp::Plus => "+",
		BinOp::Minus => "-",
		BinOp::Concat => "..",
		BinOp::Lt => "<",
		BinOp::Gt => ">",
		BinOp::Lte => "<=",
		BinOp::Gte => ">=",
		BinOp::Eq => "==",
		BinOp::Neq => "~=",
		BinOp::And => "and",
		BinOp::Or => "or",
	}
}

fn emit_unop(op: &UnOp) -> &'static str {
	match op {
		UnOp::Minus => "-",
		UnOp::Not => "not",
	}
}
