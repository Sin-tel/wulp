use crate::ast::*;
use crate::fs;
use crate::lexer::Lexer;
use crate::span::FileId;
use crate::span::InputFile;
use crate::span::Span;
use crate::span::{format_err, format_warning};
use crate::token::{Token, TokenKind};
use crate::ty::TyAst;

#[derive(Debug)]
pub struct Parser<'a> {
	input: &'a str,
	file_id: FileId,
	next_id: FileId,
	tokens: Lexer<'a>,
	filename: String,
	files: Vec<InputFile>,
}

impl<'a> Parser<'a> {
	pub fn parse(filename: &str) -> (File, Vec<InputFile>) {
		let filename = format!("blua/{filename}.blua");
		let input = fs::read_to_string(filename.clone()).unwrap();
		let mut files = Vec::new();
		let file_id = 0;
		let next_id = 1;
		let mut this = Parser {
			input: &input,
			file_id,
			next_id,
			tokens: Lexer::new(&input, file_id, filename.clone()),
			filename: filename.clone(),
			files: Vec::new(),
		};
		let ast = this.parse_file();

		// make sure we are done
		let tk = this.tokens.next();
		if tk.kind != TokenKind::Eof {
			this.error(format!("Expected end of file but found: {tk}."), tk.span);
		}
		files.append(&mut this.files);
		files.push({
			InputFile {
				contents: input,
				filename,
				id: file_id,
			}
		});
		files.sort_by_key(|f| f.id);
		for (i, v) in files.iter().enumerate() {
			assert_eq!(i, v.id);
		}
		(ast, files)
	}

	fn parse_module(&mut self, filename: &str) -> Table {
		let filename = format!("blua/{filename}.blua");
		let input = fs::read_to_string(filename.clone()).unwrap();
		let file_id = self.next_id;
		let next_id = self.next_id + 1;
		let mut parser = Parser {
			input: &input,
			file_id,
			next_id,
			tokens: Lexer::new(&input, file_id, filename.clone()),
			filename: filename.clone(),
			files: Vec::new(),
		};
		let fields = parser.parse_fields();

		// make sure we are done
		let tk = parser.tokens.next();
		if tk.kind != TokenKind::Eof {
			parser.error(format!("Expected end of file but found: {tk}."), tk.span);
		}

		self.next_id = parser.next_id;
		self.files.append(&mut parser.files);
		self.files.push({
			InputFile {
				contents: input,
				filename,
				id: file_id,
			}
		});
		Table { fields }
	}

	fn error(&mut self, msg: String, span: Span) -> ! {
		format_err(&msg, span, self.input, &self.filename);
		panic!("{msg}");
	}

	fn parse_import(&mut self) -> Import {
		match self.tokens.next().kind {
			TokenKind::Import => {
				let span = self.assert_next(TokenKind::Name).span;
				let filename = span.as_string(self.input);

				let alias = if self.tokens.peek().kind == TokenKind::As {
					self.tokens.next();
					self.parse_name()
				} else {
					Name { id: 0, span }
				};

				let module = self.parse_module(&filename);

				Import {
					filename,
					alias,
					module,
				}
			},
			TokenKind::From => todo!(),
			_ => unreachable!(),
		}
	}

	// Block and statement rules

	fn parse_file(&mut self) -> File {
		let stats = self.parse_stat_list();
		File { block: Block { stats } }
	}

	/// `{` block `}` | statement
	fn parse_block(&mut self) -> Block {
		match self.tokens.peek().kind {
			TokenKind::LCurly => {
				self.tokens.next();
				let stats = self.parse_stat_list();
				self.assert_next(TokenKind::RCurly);
				Block { stats }
			},
			_ => Block {
				stats: vec![self.parse_statement()],
			},
		}
	}

	fn parse_stat_list(&mut self) -> Vec<Stat> {
		let mut stats = Vec::new();
		loop {
			match self.tokens.peek().kind {
				TokenKind::RCurly | TokenKind::Eof => break,
				TokenKind::Return | TokenKind::Break => {
					// These have to be the last statements in a block
					// TODO: peek here to check it is the last statement, and produce error
					stats.push(self.parse_statement());
					return stats;
				},
				_ => stats.push(self.parse_statement()),
			}
		}
		stats
	}

	fn parse_statement(&mut self) -> Stat {
		let stat = self.parse_statement_inner();
		// take care of optional semicolon
		if self.tokens.peek().kind == TokenKind::SemiColon {
			self.tokens.next();
		}
		stat
	}

	fn parse_statement_inner(&mut self) -> Stat {
		let tk = self.tokens.peek();
		match tk.kind {
			TokenKind::Break => {
				self.tokens.next();
				Stat::Break
			},
			TokenKind::Import | TokenKind::From => Stat::Import(self.parse_import()),
			TokenKind::Return => Stat::Return(self.parse_return()),
			TokenKind::Let => Stat::Let(self.parse_let()),
			TokenKind::Struct => Stat::StructDef(self.parse_struct_def()),
			TokenKind::LCurly => Stat::Block(self.parse_block()),
			TokenKind::While => Stat::WhileBlock(self.parse_while_block()),
			TokenKind::If => Stat::IfBlock(self.parse_if_block()),
			TokenKind::For => Stat::ForBlock(self.parse_for_block()),
			TokenKind::Fn => Stat::FnDef(self.parse_fn_def()),
			_ => {
				// Parse a suffix expression, then check if a `=`, `,` or `:` follows to parse (multiple) assignment.
				// If not, it should be a function call.
				let suffix_expr = self.parse_suffix_expr();
				match self.tokens.peek().kind {
					TokenKind::Assign | TokenKind::Comma | TokenKind::Colon => {
						Stat::Assignment(self.parse_assignment(suffix_expr))
					},
					TokenKind::AssignPlus
					| TokenKind::AssignMinus
					| TokenKind::AssignMul
					| TokenKind::AssignDiv
					| TokenKind::AssignMod
					| TokenKind::AssignPow
					| TokenKind::AssignConcat => Stat::AssignOp(self.parse_assign_op(suffix_expr)),
					_ => {
						if let ExprKind::Call(e) = suffix_expr.kind {
							return Stat::Call(e);
						}
						// TODO: make this more informative
						let tk = self.tokens.next();
						self.error(format!("Expected `=` but found: `{tk}`."), tk.span);
					},
				}
			},
		}
	}

	fn parse_assignment(&mut self, first: Expr) -> Assignment {
		let start = first.span;
		let mut vars = vec![first];

		while self.tokens.peek().kind == TokenKind::Comma {
			self.tokens.next();
			vars.push(self.parse_suffix_expr());
		}

		// lhs vars can not be a Call, since those arent lvalues.
		for var in &vars {
			if let ExprKind::Call(_) = var.kind {
				self.error("Can not assign to a function call.".to_string(), var.span);
			}
		}
		self.assert_next(TokenKind::Assign);

		let exprs = self.parse_exprs();

		let span = Span::join(start, exprs.last().unwrap().span);

		Assignment { vars, exprs, span }
	}

	fn parse_assign_op(&mut self, var: Expr) -> AssignOp {
		let op = if let Some(op) = self.tokens.peek().assign_to_bin() {
			self.tokens.next();
			op
		} else {
			unreachable!();
		};

		let expr = self.parse_expr();

		let span = Span::join(var.span, expr.span);

		AssignOp { var, expr, op, span }
	}

	fn parse_let(&mut self) -> Let {
		let start = self.tokens.peek().span;
		self.assert_next(TokenKind::Let);

		let mut names = Vec::new();
		loop {
			let name = self.parse_name();
			let ty = if self.tokens.peek().kind == TokenKind::Colon {
				self.tokens.next();
				Some(self.parse_type())
			} else {
				None
			};
			names.push(NameTy { name, ty });

			if self.tokens.peek().kind == TokenKind::Assign {
				break;
			}
			self.assert_next(TokenKind::Comma);
		}
		self.assert_next(TokenKind::Assign);

		let exprs = self.parse_exprs();
		let span = Span::join(start, exprs.last().unwrap().span);
		Let { names, exprs, span }
	}

	fn parse_struct_def(&mut self) -> StructDef {
		self.assert_next(TokenKind::Struct);

		let name = self.parse_name();

		self.assert_next(TokenKind::Assign);
		self.assert_next(TokenKind::LCurly);
		let fields = self.parse_fields();
		self.assert_next(TokenKind::RCurly);

		let table = Table { fields };

		StructDef { name, table }
	}

	fn parse_fn_def(&mut self) -> FnDef {
		self.assert_next(TokenKind::Fn);

		let name = self.parse_name();
		let body = self.parse_fn_body();

		FnDef { name, body }
	}

	fn parse_fn_body(&mut self) -> FnBody {
		let params = self.parse_parlist();

		let ty = if self.tokens.peek().kind == TokenKind::Arrow {
			self.tokens.next();
			Some(self.parse_type())
		} else {
			None
		};

		let body = self.parse_block();

		FnBody { params, body, ty }
	}

	fn parse_for_block(&mut self) -> ForBlock {
		self.assert_next(TokenKind::For);

		let names = self.parse_names();

		self.assert_next(TokenKind::In);

		let expr = self.parse_expr();
		let block = self.parse_block();

		ForBlock { names, expr, block }
	}

	fn parse_if_block(&mut self) -> IfBlock {
		self.assert_next(TokenKind::If);

		let expr = self.parse_expr();

		let block = self.parse_block();

		let mut elseif = Vec::new();

		while self.tokens.peek().kind == TokenKind::ElseIf {
			elseif.push(self.parse_elseif());
		}

		let else_block = self.parse_else_block();

		IfBlock {
			expr,
			block,
			elseif,
			else_block,
		}
	}

	fn parse_elseif(&mut self) -> ElseIf {
		self.assert_next(TokenKind::ElseIf);
		let expr = self.parse_expr();
		let block = self.parse_block();

		ElseIf { expr, block }
	}

	fn parse_else_block(&mut self) -> Option<Block> {
		if self.tokens.peek().kind == (TokenKind::Else) {
			self.tokens.next();
			Some(self.parse_block())
		} else {
			None
		}
	}

	fn parse_while_block(&mut self) -> WhileBlock {
		self.assert_next(TokenKind::While);

		let expr = self.parse_expr();
		let block = self.parse_block();

		WhileBlock { expr, block }
	}

	fn parse_return(&mut self) -> Return {
		let mut span = self.tokens.peek().span;
		self.assert_next(TokenKind::Return);

		let mut exprs = Vec::new();
		match self.tokens.peek().kind {
			TokenKind::RCurly | TokenKind::Eof | TokenKind::SemiColon => (),
			_ => {
				exprs = self.parse_exprs();
				span.end = exprs.last().unwrap().span.end;
			},
		};
		Return { span, exprs }
	}

	// Expression rules

	fn parse_expr(&mut self) -> Expr {
		self.parse_sub_expr(0)
	}

	// TODO: left / right priority
	fn parse_sub_expr(&mut self, min_priority: i32) -> Expr {
		let mut expression = match self.parse_unexp() {
			Some(expr) => expr,
			None => self.parse_simple_expr(),
		};

		while let Some(op) = self.tokens.peek().as_bin_op() {
			let priority = op.priority();
			if priority <= min_priority {
				break;
			}
			self.tokens.next();

			let lhs = expression;
			let rhs = self.parse_sub_expr(priority);

			expression = Expr {
				span: Span::join(lhs.span, rhs.span),
				kind: ExprKind::BinExpr(BinExpr {
					op,
					lhs: Box::new(lhs),
					rhs: Box::new(rhs),
				}),
			};
		}

		expression
	}

	fn parse_simple_expr(&mut self) -> Expr {
		match self.tokens.peek().kind {
			TokenKind::Nil => {
				let span = self.tokens.next().span;
				Expr {
					span,
					kind: ExprKind::Literal(Literal::Nil),
				}
			},
			TokenKind::True => {
				let span = self.tokens.next().span;
				Expr {
					span,
					kind: ExprKind::Literal(Literal::Bool(true)),
				}
			},
			TokenKind::False => {
				let span = self.tokens.next().span;
				Expr {
					span,
					kind: ExprKind::Literal(Literal::Bool(false)),
				}
			},
			TokenKind::Fn => {
				// TODO: we now use `fn` for the span of the lambda, which is kind of lame
				let span = self.tokens.next().span;
				Expr {
					span,
					kind: ExprKind::Lambda(self.parse_fn_body()),
				}
			},
			TokenKind::Str => self.parse_string(),
			TokenKind::Number => self.parse_number(),
			TokenKind::LBracket => self.parse_array_constructor(),
			_ => self.parse_suffix_expr(),
		}
	}

	fn parse_unexp(&mut self) -> Option<Expr> {
		let tk = self.tokens.peek();
		match tk.as_un_op() {
			Some(op) => {
				self.tokens.next();
				let expr = self.parse_sub_expr(op.priority());
				Some(Expr {
					span: Span::join(tk.span, expr.span),
					kind: ExprKind::UnExpr(UnExpr {
						op,
						expr: Box::new(expr),
					}),
				})
			},
			None => None,
		}
	}

	fn parse_suffix_expr(&mut self) -> Expr {
		let mut primary = self.parse_primary_expr();

		let mut suffix = Vec::new();

		loop {
			match self.tokens.peek().kind {
				TokenKind::Period => {
					self.tokens.next();
					suffix.push(Suffix::Property(self.parse_property()));
				},
				TokenKind::LBracket => {
					self.tokens.next();
					let expr = self.parse_expr();
					self.assert_next(TokenKind::RBracket);
					suffix.push(Suffix::Index(expr));
				},
				TokenKind::LParen => {
					// Build ast node and continue
					let start_line = self.tokens.peek().line;

					self.assert_next(TokenKind::LParen);
					let args = self.parse_args();
					let end = self.assert_next(TokenKind::RParen).span;

					// check for ambiguous function call on next line
					// TODO: also for {} !
					let tk = self.tokens.peek();
					if tk.kind == TokenKind::LParen && tk.line != start_line {
						let msg = "Ambiguous syntax.";
						format_warning(msg, tk.span, self.input, &self.filename);
						eprintln!("note: Add a semicolon to the previous statement if this is intentional.");
						panic!("{msg}");
					}
					let old_suffix = std::mem::take(&mut suffix);

					let expr = self.new_suffix_expr(primary, old_suffix);

					primary = Expr {
						span: Span::join(expr.span, end),
						kind: ExprKind::Call(Call {
							expr: Box::new(expr),
							args,
						}),
					};
				},
				_ => break,
			}
		}
		self.new_suffix_expr(primary, suffix)
	}

	// if suffix is empty just emit a single expr
	fn new_suffix_expr(&mut self, expr: Expr, suffix: Vec<Suffix>) -> Expr {
		match suffix.last() {
			None => expr,
			Some(s) => {
				let end = match s {
					Suffix::Property(p) => p.span.end,
					// add one to compensate for the ending `]`
					Suffix::Index(e) => e.span.end + 1,
				};
				Expr {
					span: Span::new(expr.span.start, end, self.file_id),
					kind: ExprKind::SuffixExpr(Box::new(expr), suffix),
				}
			},
		}
	}

	fn parse_primary_expr(&mut self) -> Expr {
		match self.tokens.peek().kind {
			TokenKind::Name => {
				let name = self.parse_name();
				Expr {
					span: name.span,
					kind: ExprKind::Name(name),
				}
			},
			TokenKind::LParen => {
				let start = self.assert_next(TokenKind::LParen).span;
				let inner = self.parse_expr();
				let end = self.assert_next(TokenKind::RParen).span;
				Expr {
					span: Span::join(start, end),
					kind: ExprKind::Expr(Box::new(inner)),
				}
			},
			_ => {
				let tk = self.tokens.next();
				self.error(format!("Expected expression but found: `{tk}`."), tk.span);
			},
		}
	}

	/// Constructors

	fn parse_array_constructor(&mut self) -> Expr {
		let start = self.assert_next(TokenKind::LBracket).span;
		let exprs = self.parse_array_fields();
		let end = self.assert_next(TokenKind::RBracket).span;
		Expr {
			span: Span::join(start, end),
			kind: ExprKind::Array(exprs),
		}
	}

	fn parse_fields(&mut self) -> Vec<Field> {
		if (self.tokens.peek().kind == TokenKind::RCurly) | (self.tokens.peek().kind == TokenKind::Eof) {
			return Vec::new();
		};

		let mut fields = Vec::new();
		loop {
			if (self.tokens.peek().kind == TokenKind::RCurly) | (self.tokens.peek().kind == TokenKind::Eof) {
				break;
			}
			let f = self.parse_field();
			fields.push(f);

			match self.tokens.peek().kind {
				TokenKind::SemiColon => {
					self.tokens.next();
					continue;
				},
				_ => (),
			}
		}
		fields
	}

	fn parse_array_fields(&mut self) -> Vec<Expr> {
		if self.tokens.peek().kind == TokenKind::RBracket {
			return Vec::new();
		};

		let mut fields = Vec::new();
		loop {
			if self.tokens.peek().kind == TokenKind::RBracket {
				break;
			}
			let f = self.parse_expr();
			fields.push(f);

			match self.tokens.peek().kind {
				TokenKind::Comma | TokenKind::SemiColon => {
					self.tokens.next();
					continue;
				},
				_ => (),
			}
		}
		fields
	}

	fn parse_field(&mut self) -> Field {
		match self.tokens.peek().kind {
			TokenKind::Name => {
				let property = self.parse_property();
				self.assert_next(TokenKind::Assign);
				let expr = self.parse_expr();

				Field::Assign(property, expr)
			},
			TokenKind::Fn => {
				self.tokens.next();

				let name = self.parse_property();
				let body = self.parse_fn_body();

				Field::Fn(name, body)
			},
			_ => {
				let tk = self.tokens.next();
				self.error(format!("Expected field but found: `{tk}`."), tk.span);
			},
		}
	}

	fn parse_args(&mut self) -> Vec<Expr> {
		if self.tokens.peek().kind == TokenKind::RParen {
			return Vec::new();
		}
		self.parse_exprs()
	}

	fn parse_exprs(&mut self) -> Vec<Expr> {
		let mut exprs = Vec::new();

		exprs.push(self.parse_expr());

		while self.tokens.peek().kind == TokenKind::Comma {
			self.tokens.next();
			exprs.push(self.parse_expr());
		}

		exprs
	}

	fn parse_names(&mut self) -> Vec<Name> {
		let mut names = vec![self.parse_name()];

		while self.tokens.peek().kind == TokenKind::Comma {
			self.tokens.next();

			// don't crash on trailing comma
			if self.tokens.peek().kind == TokenKind::Name {
				names.push(self.parse_name());
			} else {
				break;
			}
		}
		names
	}

	fn parse_parlist(&mut self) -> Vec<Param> {
		self.assert_next(TokenKind::LParen);

		let mut params = Vec::new();

		if self.tokens.peek().kind == TokenKind::RParen {
			self.tokens.next();
			return params;
		}

		loop {
			if self.tokens.peek().kind == TokenKind::RParen {
				break;
			}
			let name = self.parse_name();
			let ty = if self.tokens.peek().kind == TokenKind::Colon {
				self.tokens.next();
				Some(self.parse_type())
			} else {
				None
			};
			params.push(Param { name, ty });
			if self.tokens.peek().kind == TokenKind::RParen {
				break;
			}
			self.assert_next(TokenKind::Comma);
		}

		// allow trailing comma
		if self.tokens.peek().kind == TokenKind::Comma {
			self.tokens.next();
		}

		self.assert_next(TokenKind::RParen);

		params
	}

	// Simple terminals

	// TODO: multi line string currently broken
	fn parse_string(&mut self) -> Expr {
		let span = self.tokens.next().span;
		let mut chars = span.as_str(self.input).chars();
		let lit = match chars.next() {
			Some('\'' | '\"') => {
				chars.next_back();
				Literal::Str(chars.as_str().to_string())
			},
			Some('#') => {
				chars.next();
				chars.next_back();
				chars.next_back();
				Literal::Str(chars.as_str().to_string())
			},
			// TODO: if lexer is working properly, this should be unreachable
			_ => {
				self.error(format!("Malformed string: `{}`.", chars.as_str()), span);
			},
		};

		Expr {
			span,
			kind: ExprKind::Literal(lit),
		}
	}

	fn parse_number(&mut self) -> Expr {
		let span = self.tokens.next().span;
		let s = span.as_string(self.input);
		if let Ok(num) = s.parse() {
			Expr {
				span,
				kind: ExprKind::Literal(Literal::Int(num)),
			}
		} else if let Ok(num) = s.parse() {
			Expr {
				span,
				kind: ExprKind::Literal(Literal::Num(num)),
			}
		} else {
			self.error(format!("Malformed number: `{s}`."), span);
		}
	}

	fn parse_name(&mut self) -> Name {
		let span = self.tokens.peek().span;
		self.assert_next(TokenKind::Name);
		Name { id: 0, span }
	}

	fn parse_property(&mut self) -> Property {
		let span = self.tokens.peek().span;
		let name = span.as_string(self.input);
		self.assert_next(TokenKind::Name);
		Property { span, name }
	}

	fn parse_type(&mut self) -> TyAst {
		// TODO: fn types
		let tk = self.tokens.next();
		match tk.kind {
			TokenKind::Nil => TyAst::Nil,
			TokenKind::TyNum => TyAst::Num,
			TokenKind::TyInt => TyAst::Int,
			TokenKind::TyStr => TyAst::Str,
			TokenKind::TyBool => TyAst::Bool,
			TokenKind::LBracket => {
				// Array type
				todo!()
				// let ty = TyAst::Array(Box::new(self.parse_type()));
				// self.assert_next(TokenKind::RBracket);
				// ty
			},
			TokenKind::Fn => {
				// Function type
				todo!()
				// let mut arg_ty = Vec::new();
				// self.assert_next(TokenKind::LParen);
				// loop {
				// 	if self.tokens.peek().kind == TokenKind::RParen {
				// 		break;
				// 	}
				// 	arg_ty.push(self.parse_type());
				// 	if self.tokens.peek().kind == TokenKind::RParen {
				// 		break;
				// 	}
				// 	self.assert_next(TokenKind::Comma);
				// }
				// self.assert_next(TokenKind::RParen);
				// self.assert_next(TokenKind::Arrow);
				// let ret_ty = self.parse_type();

				// TyAst::Fn(arg_ty, Box::new(ret_ty))
			},
			TokenKind::TyMaybe => {
				todo!()
				// self.assert_next(TokenKind::LParen);
				// let inner = self.parse_type();
				// self.assert_next(TokenKind::RParen);
				// TyAst::Maybe(Box::new(inner))
			},
			_ => {
				self.error(format!("Expected type but found `{}`.", tk.kind), tk.span);
			},
		}
	}

	fn assert_next(&mut self, expect: TokenKind) -> Token {
		let tk = self.tokens.next();
		if tk.kind != expect {
			self.error(format!("Expected `{}` but found `{}`.", expect, tk.kind), tk.span);
		}
		tk
	}
}
