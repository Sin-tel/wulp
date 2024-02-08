use crate::ast::*;
use crate::visitor::*;
use debug_tree::*;

pub struct AstPrinter;

impl AstPrinter {
	pub fn print_ast(&mut self, block: &mut Block) {
		defer_print!();
		add_branch!("ast");
		block.walk(self);
	}
}

impl Visitor for AstPrinter {
	fn visit_block(&mut self, node: &mut Block) {
		add_branch!("block");
		node.walk(self);
	}
	fn visit_if_block(&mut self, node: &mut IfBlock) {
		add_branch!("if");
		node.walk(self);
	}
	fn visit_for_block(&mut self, node: &mut ForBlock) {
		add_branch!("for");
		node.walk(self);
	}
	fn visit_assignment(&mut self, node: &mut Assignment) {
		add_branch!("{}", if node.local { "local assign" } else { "assign" });
		node.walk(self);
	}
	fn visit_fn_def(&mut self, node: &mut FnDef) {
		add_branch!("{}", if node.local { "local function" } else { "function" });
		node.walk(self);
	}
	fn visit_fn_call(&mut self, node: &mut Call) {
		add_branch!("call");
		node.walk(self);
	}
	fn visit_stat(&mut self, node: &mut Stat) {
		let branch = match node {
			Stat::Return(_) => Some("return"),
			_ => None,
		};
		if let Some(br) = branch {
			add_branch!("{br}");
			node.walk(self);
		} else {
			node.walk(self);
		}
	}
	fn visit_expr(&mut self, node: &mut Expr) {
		match node {
			Expr::Lambda(_) => {
				add_branch!("lambda");
				node.walk(self);
			},
			Expr::Table(_) => {
				add_branch!("table");
				node.walk(self);
			},
			_ => node.walk(self),
		}
	}
	fn visit_literal(&mut self, node: &mut Literal) {
		match node {
			Literal::Nil => add_leaf!("nil"),
			Literal::Number(s) => add_leaf!("{} (number)", s),
			Literal::Str(s) => add_leaf!("{:?} (string)", s),
			Literal::Bool(s) => add_leaf!("{} (bool)", s),
		}
	}

	fn visit_bin_expr(&mut self, node: &mut BinExpr) {
		add_branch!("`{}` (binop)", node.op);
		node.walk(self);
	}

	fn visit_un_expr(&mut self, node: &mut UnExpr) {
		add_branch!("`{}` (unop)", node.op);
		node.walk(self);
	}

	fn visit_suffix_expr(&mut self, node: &mut SuffixExpr) {
		add_branch!("suffixed");
		node.walk(self);
	}

	fn visit_suffix(&mut self, node: &mut Suffix) {
		let s = match node {
			Suffix::Property(_) => "property",
			Suffix::Index(_) => "index",
		};
		add_branch!("{s}");
		node.walk(self);
	}
	fn visit_field(&mut self, node: &mut Field) {
		if let Field::Assign(_, _) = node {
			add_branch!("assign");
			node.walk(self);
		} else {
			node.walk(self);
		}
	}

	fn visit_name(&mut self, _node: &mut Name) {
		// add_leaf!("{} (identifier)", node.id);
		add_leaf!("identifier");
	}
}
