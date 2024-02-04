use crate::ast::*;
use crate::visitor::*;
use debug_tree::*;

pub struct AstPrinter;

impl Visitor for AstPrinter {
	fn visit_block(&mut self, block: &mut Block) {
		defer_print!();
		add_branch!("block");
		walk_block(self, block);
	}
	fn visit_local_assignment(&mut self, local_assignment: &mut LocalAssignment) {
		add_branch!("local assign");
		walk_local_assignment(self, local_assignment);
	}
	fn visit_assignment(&mut self, assignment: &mut Assignment) {
		add_branch!("assign");
		walk_assignment(self, assignment);
	}
	fn visit_function_call(&mut self, function_call: &mut FunctionCall) {
		add_branch!("function call");
		walk_function_call(self, function_call);
	}
	fn visit_stat(&mut self, stat: &mut Stat) {
		match stat {
			Stat::Return(_) => {
				{
					add_branch!("return");
					walk_stat(self, stat);
				};
			},
			_ => {
				walk_stat(self, stat);
			},
		}
	}
	fn visit_name(&mut self, name: &mut Name) {
		add_leaf!("{}", name.0);
	}
	fn visit_expr(&mut self, expr: &mut Expr) {
		match expr {
			Expr::Str(s) => add_leaf!("str \"{}\"", s),
			Expr::Num(s) => add_leaf!("num {}", s),
			_ => (),
		}
		walk_expr(self, expr);
	}
}
