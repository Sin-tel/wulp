use crate::ast::*;
use crate::visitor::*;
use debug_tree::*;

pub struct AstPrinter;

impl AstPrinter {
	pub fn print_ast(&mut self, block: &mut Block) {
		defer_print!();
		add_branch!("ast");
		block.visit(self);
	}
}

impl Visitor for AstPrinter {
	fn visit_block(&mut self, node: &mut Block) {
		add_branch!("block");
		node.visit(self);
	}
	fn visit_if_block(&mut self, node: &mut IfBlock) {
		add_branch!("IF");
		node.visit(self);
	}
	fn visit_assignment(&mut self, node: &mut Assignment) {
		add_branch!("{}", if node.local { "local assign" } else { "assign" });
		node.visit(self);
	}
	fn visit_function_call(&mut self, node: &mut FunctionCall) {
		add_branch!("function call");
		node.visit(self);
	}
	fn visit_stat(&mut self, node: &mut Stat) {
		let branch = match node {
			Stat::Return(_) => Some("return"),
			_ => None,
		};
		if let Some(br) = branch {
			add_branch!("{br}");
			node.visit(self);
		} else {
			node.visit(self);
		}
	}
	fn visit_expr(&mut self, node: &mut Expr) {
		match node {
			Expr::Nil => add_leaf!("nil"),
			Expr::Num(s) => add_leaf!("{} (number)", s),
			Expr::Str(s) => add_leaf!("\"{}\" (string)", s),
			Expr::Bool(s) => add_leaf!("{} (bool)", s),
			Expr::Lambda(_) => {
				add_branch!("lambda");
				node.visit(self);
			},
			Expr::Table(_) => {
				add_branch!("table");
				node.visit(self);
			},
			_ => node.visit(self),
		}
	}

	fn visit_bin_expr(&mut self, node: &mut BinExp) {
		add_branch!("`{}` (binop)", node.op);
		node.visit(self);
	}

	fn visit_un_expr(&mut self, node: &mut UnExp) {
		add_branch!("`{}` (unop)", node.op);
		node.visit(self);
	}

	fn visit_suffix_expr(&mut self, node: &mut SuffixExpr) {
		add_branch!("suffixed");
		node.visit(self);
	}

	fn visit_suffix(&mut self, node: &mut Suffix) {
		let s = match node {
			Suffix::Property(_) => "property",
			Suffix::Index(_) => "index",
			Suffix::Call(_) => "call args",
		};
		add_branch!("{s}");
		node.visit(self);
	}
	fn visit_field(&mut self, node: &mut Field) {
		add_branch!("field");
		node.visit(self);
	}

	fn visit_name(&mut self, node: &mut Name) {
		add_leaf!("{} (identifier)", node.0);
	}
}
