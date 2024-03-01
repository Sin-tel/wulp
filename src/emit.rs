use crate::ast::*;
use crate::symbol::SymbolTable;
use crate::visitor::*;
use std::mem;

pub struct EmitLua {
	code: String,
	statement: String,
	indent_level: usize,
	symbol_table: SymbolTable,
	patch_temp_lvalue: bool,
	hoist_fn_def: bool,
}

impl EmitLua {
	pub fn emit(ast: &mut File, symbol_table: SymbolTable) -> String {
		let mut this = Self {
			statement: String::new(),
			code: String::new(),
			indent_level: 0,
			symbol_table,
			patch_temp_lvalue: false,
			hoist_fn_def: false,
		};
		this.code.push_str(include_str!("../lua/std_preamble.lua"));
		ast.block.visit(&mut this);

		// this.code = this.code.replace("\n", " ");
		// this.code = this.code.replace(";", "");
		// this.code = this.code.replace("\t", "");

		this.code
	}

	fn put_statement(&mut self) {
		if !self.statement.is_empty() {
			self.code.push_str(&mem::take(&mut self.statement));
			self.code.push(';');
			self.code.push('\n');
		}
	}

	fn push_list<V>(&mut self, list: &mut [V], punctuation: &str)
	where
		V: VisitNode<Self>,
	{
		let mut list = list.iter_mut();
		if let Some(e) = list.next() {
			e.visit(self);
			for e in list {
				self.statement.push_str(punctuation);
				e.visit(self);
			}
		}
	}
	fn indent(&mut self) {
		self.statement.push_str(&"\t".repeat(self.indent_level - 1));
	}
	fn replace_with_temp(&mut self, expr: &mut Expr) {
		// bail out if it's not necessary
		match expr.kind {
			ExprKind::Literal(_) | ExprKind::Name(_) => return,
			_ => (),
		}

		let (id, name) = self.symbol_table.fresh_temp();

		// swap out the current statement we're building and emit temporary
		let stat = mem::take(&mut self.statement);
		{
			self.statement.push_str("local ");
			self.statement.push_str(&name);
			self.statement.push_str(" = ");
			self.patch_temp_lvalue = false;
			self.visit_expr(expr);
			self.patch_temp_lvalue = true;
			self.put_statement();
		}
		self.statement = stat;

		let span = expr.span;
		*expr = Expr {
			span,
			kind: ExprKind::Name(Name { id, span }),
		}
	}
	fn emit_fn_local(&mut self, node: &mut FnDef) {
		self.statement.push_str("local ");
		self.visit_name(&mut node.name);
	}
	fn emit_fn_def(&mut self, node: &mut FnDef) {
		self.statement.push_str("function ");
		self.visit_name(&mut node.name);
		self.visit_fn_body(&mut node.body);
	}
}

impl Visitor for EmitLua {
	fn visit_block(&mut self, node: &mut Block) {
		self.indent_level += 1;
		self.hoist_fn_def = true;
		for b in &mut node.stats {
			if let Stat::FnDef(f) = b {
				self.indent();
				self.emit_fn_local(f);
				self.put_statement();
			}
		}
		for b in &mut node.stats {
			if let Stat::FnDef(f) = b {
				self.indent();
				self.emit_fn_def(f);
				self.put_statement();
			}
		}
		self.hoist_fn_def = false;
		node.walk(self);
		self.indent_level -= 1;
	}
	fn visit_if_block(&mut self, node: &mut IfBlock) {
		self.statement.push_str("if ");
		self.visit_expr(&mut node.expr);
		self.statement.push_str(" then\n");

		self.visit_block(&mut node.block);

		for elseif in &mut node.elseif {
			self.indent();
			self.statement.push_str("elseif ");
			self.visit_expr(&mut elseif.expr);
			self.statement.push_str(" then\n");
			self.visit_block(&mut elseif.block);
		}
		if let Some(b) = &mut node.else_block {
			self.indent();
			self.statement.push_str("else\n");
			self.visit_block(b);
		}
		self.indent();
		self.statement.push_str("end");
	}
	fn visit_while_block(&mut self, node: &mut WhileBlock) {
		self.statement.push_str("while ");
		self.visit_expr(&mut node.expr);
		self.statement.push_str(" do\n");

		self.visit_block(&mut node.block);

		self.indent();
		self.statement.push_str("end");
	}
	fn visit_for_block(&mut self, node: &mut ForBlock) {
		// TODO: emit the correct kind of iterator depending on the type of expr

		assert!(node.names.len() == 1);
		self.statement.push_str("for ");
		self.visit_name(&mut node.names[0]);

		self.statement.push_str(" in iter.wrap(iter.array(");
		self.visit_expr(&mut node.expr);
		self.statement.push_str(")) do\n");

		self.visit_block(&mut node.block);

		self.indent();
		self.statement.push_str("end");
	}
	fn visit_assignment(&mut self, node: &mut Assignment) {
		self.push_list(&mut node.vars, ", ");
		if !node.exprs.is_empty() {
			self.statement.push_str(" = ");
			self.push_list(&mut node.exprs, ", ");
		}
	}
	fn visit_let(&mut self, node: &mut Let) {
		self.statement.push_str("local ");
		self.push_list(&mut node.names, ", ");
		if !node.exprs.is_empty() {
			self.statement.push_str(" = ");
			self.push_list(&mut node.exprs, ", ");
		}
	}
	fn visit_fn_def(&mut self, _node: &mut FnDef) {
		// nop
	}
	fn visit_fn_call(&mut self, node: &mut Call) {
		self.visit_expr(&mut node.expr);
		self.statement.push('(');
		self.push_list(&mut node.args, ", ");
		self.statement.push(')');
	}
	fn visit_stat(&mut self, node: &mut Stat) {
		self.indent();
		match node {
			Stat::Return(ret) => {
				self.statement.push_str("return ");
				self.push_list(&mut ret.exprs, ", ");
			},
			Stat::Break => {
				self.statement.push_str("break");
			},
			Stat::Block(b) => {
				self.statement.push_str("do\n");
				b.visit(self);
				self.indent();
				self.statement.push_str("end");
			},
			Stat::Import(s) => {
				// TODO: emit this as a block instead of a table
				// struct and fn defs should be in export table
				self.statement.push_str("local ");
				s.alias.visit(self);
				self.statement.push_str(" = ");
				self.statement.push('{');
				self.push_list(&mut s.module.fields, ", ");
				self.statement.push('}');
			},
			Stat::AssignOp(s) => {
				// Copy any evaluations to a temp var
				self.patch_temp_lvalue = true;
				self.visit_expr(&mut s.var);
				self.patch_temp_lvalue = false;

				self.statement.push_str(" = ");
				self.visit_expr(&mut s.var);

				self.statement.push(' ');
				self.statement.push_str(emit_binop(&s.op));
				self.statement.push(' ');
				self.visit_expr(&mut s.expr);
			},
			Stat::StructDef(t) => {
				self.statement.push_str("local ");
				let name_str = self.symbol_table.get(t.name.id).name.clone();
				self.statement.push_str(&name_str);
				self.statement.push_str(" = {");
				for f in &mut t.table.fields {
					match f {
						Field::Assign(_, _) => (),
						Field::Fn(_, _) => {
							self.visit_field(f);
							self.statement.push_str(",\n");
						},
					}
				}
				self.statement.push('}');
				self.put_statement();
				self.statement.push_str(&format!(
					"setmetatable({0}, {{\n\
					\t__call = function(_, args)\n\
					\t\tlocal new = {{",
					&name_str
				));
				for f in &mut t.table.fields {
					match f {
						Field::Assign(_, _) => {
							self.visit_field(f);
							self.statement.push_str(", ");
						},
						Field::Fn(_, _) => (),
					}
				}
				self.statement.push_str("}\n\t\treturn new\n\tend\n})");
			},
			Stat::WhileBlock(_)
			| Stat::IfBlock(_)
			| Stat::ForBlock(_)
			| Stat::Assignment(_)
			| Stat::Let(_)
			| Stat::Call(_) => {
				node.walk(self);
			},
			Stat::FnDef(_) => (), // already done
		};

		self.put_statement();
	}
	fn visit_expr(&mut self, node: &mut Expr) {
		if self.patch_temp_lvalue {
			if let ExprKind::Call(_) = &node.kind {
				self.replace_with_temp(node);
			}
		};

		match &mut node.kind {
			ExprKind::BinExpr(e) => {
				self.visit_expr(&mut e.lhs);
				self.statement.push(' ');
				self.statement.push_str(emit_binop(&e.op));
				self.statement.push(' ');
				self.visit_expr(&mut e.rhs);
			},
			ExprKind::UnExpr(e) => {
				self.statement.push_str(emit_unop(&e.op));
				self.statement.push(' ');
				self.visit_expr(&mut e.expr);
			},
			ExprKind::Lambda(_) => {
				self.statement.push_str("function");
				node.walk(self);
			},
			ExprKind::Expr(_) => {
				self.statement.push('(');
				node.walk(self);
				self.statement.push(')');
			},
			ExprKind::Array(t) => {
				self.statement.push('{');
				if !t.is_empty() {
					self.statement.push_str("[0]=");
					self.push_list(t, ", ");
					self.statement.push_str(", ");
				}
				self.statement.push_str("n=");
				self.statement.push_str(&t.len().to_string());
				self.statement.push('}');
			},
			ExprKind::Literal(_) | ExprKind::Name(_) | ExprKind::Call(_) | ExprKind::SuffixExpr(_, _) => {
				node.walk(self);
			},
		}
	}
	fn visit_literal(&mut self, node: &mut Literal) {
		match node {
			Literal::Nil => self.statement.push_str("nil"),
			Literal::Num(s) => self.statement.push_str(&s.to_string()),
			Literal::Int(s) => self.statement.push_str(&s.to_string()),
			Literal::Str(s) => {
				// TODO: do proper escape sequences
				self.statement.push_str(&format!("{s:?}"));
			},
			Literal::Bool(s) => self.statement.push_str(&s.to_string()),
		}
	}

	fn visit_suffix(&mut self, node: &mut Suffix) {
		match node {
			Suffix::Property(_) => {
				self.statement.push('.');
				node.walk(self);
			},
			Suffix::Index(e) => {
				self.statement.push('[');
				if self.patch_temp_lvalue {
					self.replace_with_temp(e);
				}
				node.walk(self);
				self.statement.push(']');
			},
		};
	}

	fn visit_fn_body(&mut self, node: &mut FnBody) {
		// TODO: no newline for short functions
		self.statement.push('(');
		self.push_list(&mut node.params, ", ");
		self.statement.push_str(")\n");

		self.visit_block(&mut node.body);

		self.indent();
		self.statement.push_str("end");
	}

	fn visit_field(&mut self, node: &mut Field) {
		match node {
			Field::Assign(p, e) => {
				self.visit_property(p);
				self.statement.push_str(" = ");
				self.visit_expr(e);
			},
			Field::Fn(p, f) => {
				self.visit_property(p);
				self.statement.push_str(" = function");
				self.visit_fn_body(f);
			},
		}
	}

	fn visit_name(&mut self, node: &mut Name) {
		self.statement.push_str(&self.symbol_table.get(node.id).name);
	}

	fn visit_property(&mut self, node: &mut Property) {
		self.statement.push_str(&node.name);
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
