use crate::ast::*;
use crate::visitor::*;
use crate::InputFiles;
use debug_tree::*;

pub struct AstPrinter<'a> {
	input: &'a InputFiles,
}

impl<'a> AstPrinter<'a> {
	pub fn print_ast(ast: &mut File, input: &'a InputFiles) {
		let mut printer = Self { input };
		defer_print!();
		add_branch!("ast");
		ast.walk(&mut printer);
	}
}

impl Visitor for AstPrinter<'_> {
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
		add_branch!("assign");
		node.walk(self);
	}
	fn visit_let(&mut self, node: &mut Let) {
		add_branch!("let");
		node.walk(self);
	}
	fn visit_fn_def(&mut self, node: &mut FnDef) {
		add_branch!("function");
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
		let source = node.span.as_string(self.input);
		match node.kind {
			ExprKind::BinExpr(_) => {
				add_branch!("{} (binop)", source);
				node.walk(self);
			},
			ExprKind::UnExpr(_) => {
				add_branch!("{} (unop)", source);
				node.walk(self);
			},
			ExprKind::SuffixExpr(_, _) => {
				add_branch!("{}", source);
				node.walk(self);
			},
			ExprKind::Lambda(_) => {
				add_branch!("lambda");
				node.walk(self);
			},
			ExprKind::Table(_) => {
				add_branch!("table");
				node.walk(self);
			},
			_ => node.walk(self),
		}
	}
	fn visit_literal(&mut self, node: &mut Literal) {
		match node {
			Literal::Nil => add_leaf!("nil"),
			Literal::Num(s) => add_leaf!("{} (num)", s),
			Literal::Int(s) => add_leaf!("{} (int)", s),
			Literal::Str(s) => add_leaf!("{:?} (string)", s),
			Literal::Bool(s) => add_leaf!("{} (bool)", s),
		}
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
			add_branch!("field assign");
			node.walk(self);
		} else {
			node.walk(self);
		}
	}

	fn visit_name(&mut self, node: &mut Name) {
		add_leaf!("{} (identifier)", node.span.as_str(self.input));
	}
	fn visit_property(&mut self, node: &mut Property) {
		add_leaf!("{} (property)", node.name);
	}
}
