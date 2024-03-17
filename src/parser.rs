use crate::ast::*;
use crate::fs;
use crate::lexer::Lexer;
use crate::span::{format_err, format_warning, FileId, InputFile, Span};
use crate::token::{Token, TokenKind};
use crate::ty::TyAst;
use anyhow::{Context, Result};
use std::path::{Path, PathBuf};

#[derive(Debug)]
pub struct Parser<'a> {
	input: &'a str,
	file_id: FileId,
	tokens: Lexer<'a>,
	path: PathBuf,
}

impl<'a> Parser<'a> {
	pub fn parse(path: &Path) -> Result<(Vec<Module>, Vec<InputFile>)> {
		let mut files = Vec::new();
		let mut modules = Vec::new();

		// load standard library first
		Self::parse_file(Path::new("std/prelude"), &mut files, &mut modules, true)?;
		Self::parse_file(path, &mut files, &mut modules, true)?;

		Ok((modules, files))
	}

	pub fn parse_file(
		file_path: &Path,
		files: &mut Vec<InputFile>,
		modules: &mut Vec<Module>,
		global: bool,
	) -> Result<FileId> {
		// recursively parse imports
		// TODO: check cycles
		// TODO: don't parse same file twice

		let file_id = files.len();
		let (mut module, file) = Self::parse_module(file_path, file_id, global)?;
		files.push(file);
		for item in &mut module.items {
			if let Item::Import(Import { path, file_id, .. }) = item {
				let i_file_id = Self::parse_file(path, files, modules, false)?;
				*file_id = Some(i_file_id);
			}
		}
		modules.push(module);
		Ok(file_id)
	}

	pub fn parse_module(path: &Path, file_id: FileId, global: bool) -> Result<(Module, InputFile)> {
		let mut path = path.to_path_buf();
		path.set_extension("wulp");
		let input = fs::read_to_string(path.clone()).with_context(|| format!("could not find {}", path.display()))?;
		let mut this =
			Parser { input: &input, file_id, tokens: Lexer::new(&input, file_id, path.clone()), path: path.clone() };
		let items = this.parse_item_list();

		// make sure we are done
		let tk = this.tokens.next();
		if tk.kind != TokenKind::Eof {
			this.error(format!("expected end of file but found: {tk}"), tk.span);
		}

		let module = Module { items, file_id, global };
		Ok((module, InputFile { contents: input, path }))
	}

	fn error(&mut self, msg: String, span: Span) -> ! {
		format_err(&msg, span, self.input, &self.path);
		panic!("{msg}");
	}

	fn parse_import(&mut self) -> Import {
		match self.tokens.next().kind {
			TokenKind::Import => {
				let (path, span) = self.parse_path();

				if self.tokens.peek().kind == TokenKind::As {
					self.tokens.next();
					let name = self.parse_name();
					Import { path, file_id: None, kind: ImportKind::Alias(name) }
				} else {
					assert!(!span.as_str(self.input).contains('.'));
					let name = Name { id: 0, span };
					Import { path, file_id: None, kind: ImportKind::Alias(name) }
				}
			},
			TokenKind::From => {
				let (path, _) = self.parse_path();

				self.assert_next(TokenKind::Import);
				self.assert_next(TokenKind::Mul); // glob

				Import { path, file_id: None, kind: ImportKind::Glob }
			},
			_ => unreachable!(),
		}
	}

	fn parse_path(&mut self) -> (PathBuf, Span) {
		// imports are relative, so get path of current file
		let mut path = self.path.clone();
		path.pop();

		let mut span = self.tokens.next().span;

		path.push(span.as_str(self.input));
		while self.tokens.peek().kind == TokenKind::Period {
			self.tokens.next();
			let next_span = self.tokens.next().span;
			span = Span::join(span, next_span);
			path.push(next_span.as_str(self.input));
		}

		(path, span)
	}

	fn parse_directive(&mut self) -> Option<Item> {
		self.assert_next(TokenKind::Hash);
		let span = self.assert_next(TokenKind::Name).span;

		let name_str = span.as_string(self.input);
		match name_str.as_ref() {
			"intrinsic" => {
				let name = self.parse_name();
				let mut property = None;
				if self.tokens.peek().kind == TokenKind::Period {
					self.tokens.next();
					property = Some(self.parse_property());
				}
				self.assert_next(TokenKind::Colon);
				let ty = self.parse_type();
				let lua_def = if self.tokens.peek().kind == TokenKind::Assign {
					self.tokens.next();
					let lua_span = self.assert_next(TokenKind::Str).span;
					Some(self.parse_string_literal(lua_span))
				} else {
					None
				};
				Some(Item::Intrinsic(Intrinsic { name, property, ty, lua_def }))
			},
			"lua" => {
				let lua_span = self.assert_next(TokenKind::Str).span;
				let puts = self.parse_string_literal(lua_span);
				Some(Item::InlineLua(puts))
			},
			s => self.error(format!("unknown directive `#{s}`"), span),
		}
	}

	// Block and statement rules

	/// `{` block `}` | statement
	fn parse_block(&mut self) -> Block {
		match self.tokens.peek().kind {
			TokenKind::LCurly => {
				self.tokens.next();
				let stats = self.parse_stat_list();
				self.assert_next(TokenKind::RCurly);
				Block { stats }
			},
			_ => Block { stats: vec![self.parse_statement()] },
		}
	}

	fn parse_item_list(&mut self) -> Vec<Item> {
		let mut items = Vec::new();
		loop {
			match self.tokens.peek().kind {
				TokenKind::Hash => {
					let s = self.parse_directive();
					if let Some(s) = s {
						items.push(s);
					}
				},
				TokenKind::Eof => break,
				_ => items.push(self.parse_item()),
			}
		}
		items
	}

	fn parse_item(&mut self) -> Item {
		let item = self.parse_item_inner();
		// take care of optional semicolon
		if self.tokens.peek().kind == TokenKind::SemiColon {
			self.tokens.next();
		}
		item
	}

	fn parse_item_inner(&mut self) -> Item {
		let tk = self.tokens.peek();
		match tk.kind {
			TokenKind::Import | TokenKind::From => Item::Import(self.parse_import()),
			TokenKind::Struct => Item::StructDef(self.parse_struct_def()),
			TokenKind::Fn => Item::FnDef(self.parse_fn_def()),
			TokenKind::Hash => unreachable!(),
			_ => self.error(format!("expected item but found: `{tk}`"), tk.span),
		}
	}

	fn parse_stat_list(&mut self) -> Vec<Stat> {
		let mut stats = Vec::new();
		loop {
			match self.tokens.peek().kind {
				TokenKind::RCurly => break,
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
			TokenKind::Return => Stat::Return(self.parse_return()),
			TokenKind::Let => Stat::Let(self.parse_let()),
			TokenKind::LCurly => Stat::Block(self.parse_block()),
			TokenKind::While => Stat::WhileBlock(self.parse_while_block()),
			TokenKind::If => Stat::IfBlock(self.parse_if_block()),
			TokenKind::For => Stat::ForBlock(self.parse_for_block()),
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
						self.error(format!("expected `=` but found: `{tk}`"), tk.span);
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

		// lhs vars can't be a Call, since those arent lvalues.
		for var in &vars {
			if let ExprKind::Call(_) = var.kind {
				self.error("can't assign to a function call".to_string(), var.span);
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
			let ty = self.parse_option_ty();
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

		self.assert_next(TokenKind::LCurly);
		let fields = self.parse_fields();
		self.assert_next(TokenKind::RCurly);
		let table = Table { fields };

		StructDef { name, table }
	}

	fn parse_fn_def(&mut self) -> FnDef {
		self.assert_next(TokenKind::Fn);

		let name = self.parse_name();
		let mut property = None;

		if self.tokens.peek().kind == TokenKind::Period {
			self.tokens.next();
			property = Some(self.parse_property());
		}

		let body = self.parse_fn_body();

		FnDef { name, property, body }
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

		IfBlock { expr, block, elseif, else_block }
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
			None => self.parse_suffix_expr(),
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
				kind: ExprKind::BinExpr(BinExpr { op, lhs: Box::new(lhs), rhs: Box::new(rhs) }),
			};
		}

		expression
	}

	fn parse_unexp(&mut self) -> Option<Expr> {
		let tk = self.tokens.peek();
		match tk.as_un_op() {
			Some(op) => {
				self.tokens.next();
				let expr = self.parse_sub_expr(op.priority());
				Some(Expr {
					span: Span::join(tk.span, expr.span),
					kind: ExprKind::UnExpr(UnExpr { op, expr: Box::new(expr) }),
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
					let (args, named_args) = self.parse_args();
					let end = self.assert_next(TokenKind::RParen).span;

					// check for ambiguous function call on next line
					// TODO: also for {} !
					let tk = self.tokens.peek();
					if tk.kind == TokenKind::LParen && tk.line != start_line {
						let msg = "ambiguous syntax";
						format_warning(msg, tk.span, self.input, &self.path);
						eprintln!("note: Add a semicolon to the previous statement if this is intentional");
						panic!("{msg}");
					}
					let old_suffix = std::mem::take(&mut suffix);

					let expr = self.new_suffix_expr(primary, old_suffix);

					primary = Expr {
						span: Span::join(expr.span, end),
						kind: ExprKind::Call(Call { expr: Box::new(expr), args, named_args }),
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
			TokenKind::Nil => {
				let span = self.tokens.next().span;
				Expr { span, kind: ExprKind::Literal(Literal::Nil) }
			},
			TokenKind::True => {
				let span = self.tokens.next().span;
				Expr { span, kind: ExprKind::Literal(Literal::Bool(true)) }
			},
			TokenKind::False => {
				let span = self.tokens.next().span;
				Expr { span, kind: ExprKind::Literal(Literal::Bool(false)) }
			},
			TokenKind::Fn => {
				// TODO: we now use `fn` for the span of the lambda, which is kind of lame
				let span = self.tokens.next().span;
				Expr { span, kind: ExprKind::Lambda(self.parse_fn_body()) }
			},
			TokenKind::Str => self.parse_string(),
			TokenKind::Number | TokenKind::HexNumber | TokenKind::BinNumber => self.parse_number(),
			TokenKind::LBracket => self.parse_array_constructor(),
			TokenKind::Name => {
				let name = self.parse_name();
				Expr { span: name.span, kind: ExprKind::Name(name) }
			},
			TokenKind::LParen => {
				let start = self.assert_next(TokenKind::LParen).span;
				let inner = self.parse_expr();
				let end = self.assert_next(TokenKind::RParen).span;
				Expr { span: Span::join(start, end), kind: ExprKind::Expr(Box::new(inner)) }
			},
			_ => {
				let tk = self.tokens.next();
				self.error(format!("expected expression but found: `{tk}`"), tk.span);
			},
		}
	}

	/// Constructors

	fn parse_array_constructor(&mut self) -> Expr {
		let start = self.assert_next(TokenKind::LBracket).span;
		let exprs = self.parse_array_fields();
		let end = self.assert_next(TokenKind::RBracket).span;
		Expr { span: Span::join(start, end), kind: ExprKind::Array(exprs) }
	}

	fn parse_fields(&mut self) -> Vec<Field> {
		// TODO: don't need EoF here?
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
				TokenKind::SemiColon | TokenKind::Comma => {
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
		if self.tokens.peek().kind == TokenKind::Name {
			let property = self.parse_property();
			let ty = self.parse_option_ty();
			if self.tokens.peek().kind == TokenKind::Assign {
				self.tokens.next();
				let expr = self.parse_expr();
				Field { field: PropertyTy { property, ty }, kind: FieldKind::Assign(expr) }
			} else {
				Field { field: PropertyTy { property, ty }, kind: FieldKind::Empty }
			}
		} else {
			let tk = self.tokens.next();
			self.error(format!("expected field but found: `{tk}`"), tk.span);
		}
	}

	fn parse_args(&mut self) -> (Vec<Expr>, Vec<NamedArg>) {
		// TODO check all named arguments come after the non-named ones
		let mut args = Vec::new();
		let mut named_args = Vec::new();
		if self.tokens.peek().kind == TokenKind::RParen {
			return (args, named_args);
		}

		loop {
			let expr = self.parse_expr();
			if self.tokens.peek().kind == TokenKind::Assign {
				self.tokens.next();
				if let ExprKind::Name(name) = expr.kind {
					let expr = self.parse_expr();
					let span = expr.span;
					named_args.push(NamedArg {
						name: name.span.as_string(self.input),
						expr,
						span: Span::join(name.span, span),
					});
				} else {
					panic!("lhs must be a name")
				}
			} else {
				args.push(expr);
			}

			if self.tokens.peek().kind == TokenKind::Comma {
				self.tokens.next();
			} else {
				break;
			}
		}

		(args, named_args)
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

	fn parse_parlist(&mut self) -> Vec<NameTy> {
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
			let ty = self.parse_option_ty();
			params.push(NameTy { name, ty });
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
		let span = self.assert_next(TokenKind::Str).span;
		let lit = self.parse_string_literal(span);
		Expr { span, kind: ExprKind::Literal(Literal::Str(lit)) }
	}

	fn parse_string_literal(&mut self, span: Span) -> String {
		let mut chars = span.as_str(self.input).chars();
		let lit = match chars.next() {
			Some('\'' | '\"') => {
				chars.next_back();
				chars.as_str().to_string()
			},
			Some('#') => {
				chars.next();
				chars.next_back();
				chars.next_back();
				chars.as_str().to_string()
			},
			// TODO: if lexer is working properly, this should be unreachable
			_ => {
				self.error(format!("malformed string: `{}`", chars.as_str()), span);
			},
		};

		lit
	}

	fn parse_number(&mut self) -> Expr {
		let tk = self.tokens.next();
		let span = tk.span;
		let s = span.as_string(self.input);
		match tk.kind {
			TokenKind::Number => {
				let s = str::replace(&s, "_", "");
				if let Ok(num) = s.parse::<u32>() {
					return Expr { span, kind: ExprKind::Literal(Literal::Int(num as i32)) };
				} else if let Ok(num) = s.parse::<f64>() {
					return Expr { span, kind: ExprKind::Literal(Literal::Num(num)) };
				}
			},
			TokenKind::HexNumber => {
				let s = str::replace(&s, "_", "");
				if let Some(s) = s.strip_prefix("0x") {
					if let Ok(num) = u32::from_str_radix(s, 16) {
						return Expr { span, kind: ExprKind::Literal(Literal::Int(num as i32)) };
					}
				}
			},
			TokenKind::BinNumber => {
				if let Some(s) = s.strip_prefix("0b") {
					let s = &str::replace(s, "_", "");
					if let Ok(num) = u32::from_str_radix(s, 2) {
						return Expr { span, kind: ExprKind::Literal(Literal::Int(num as i32)) };
					}
				}
			},
			_ => unreachable!(),
		}
		// TODO: better error message when literal doesn't fit in i32
		self.error(format!("malformed number: `{s}`"), span);
	}

	fn parse_name(&mut self) -> Name {
		let span = self.tokens.peek().span;
		self.assert_next(TokenKind::Name);
		Name { id: 0, span }
	}

	fn parse_ty_name(&mut self) -> TyName {
		let span = self.tokens.peek().span;
		self.assert_next(TokenKind::Name);
		TyName { id: 0, span }
	}

	fn parse_property(&mut self) -> Property {
		let span = self.tokens.peek().span;
		let name = span.as_string(self.input);
		self.assert_next(TokenKind::Name);
		Property { span, name }
	}

	fn parse_option_ty(&mut self) -> Option<TyAst> {
		if self.tokens.peek().kind == TokenKind::Colon {
			self.tokens.next();
			Some(self.parse_type())
		} else {
			None
		}
	}

	fn parse_type(&mut self) -> TyAst {
		// TODO: fn types
		let tk = self.tokens.peek();
		match tk.kind {
			TokenKind::Name => {
				let name = self.parse_ty_name();
				let name_str = name.span.as_string(self.input);
				match name_str.as_ref() {
					"self" => TyAst::SelfTy,
					"any" => TyAst::Any,
					_ => TyAst::Named(name),
				}
			},
			TokenKind::LBracket => {
				// Array `[ty]`

				self.tokens.next();
				let ty = TyAst::Array(Box::new(self.parse_type()));
				self.assert_next(TokenKind::RBracket);
				ty
			},
			TokenKind::Fn => {
				// Function `fn(arg, arg) -> ret`

				self.tokens.next();
				let mut arg_ty = Vec::new();
				self.assert_next(TokenKind::LParen);
				loop {
					if self.tokens.peek().kind == TokenKind::RParen {
						break;
					}
					arg_ty.push(self.parse_type());
					if self.tokens.peek().kind == TokenKind::RParen {
						break;
					}
					self.assert_next(TokenKind::Comma);
				}
				self.assert_next(TokenKind::RParen);
				self.assert_next(TokenKind::Arrow);
				let ret_ty = self.parse_type();

				TyAst::Fn(arg_ty, Box::new(ret_ty))
			},
			TokenKind::Maybe => {
				// maybe `maybe(ty)`

				self.tokens.next();
				self.assert_next(TokenKind::LParen);
				let inner = self.parse_type();
				self.assert_next(TokenKind::RParen);
				TyAst::Maybe(Box::new(inner))
			},
			TokenKind::LParen => {
				self.tokens.next();
				self.assert_next(TokenKind::RParen);
				TyAst::Unit
			},
			TokenKind::Bang => {
				self.tokens.next();
				TyAst::Never
			},

			_ => {
				self.error(format!("expected type but found `{}`", tk.kind), tk.span);
			},
		}
	}

	fn assert_next(&mut self, expect: TokenKind) -> Token {
		let tk = self.tokens.next();
		if tk.kind != expect {
			self.error(format!("expected `{}` but found `{}`", expect, tk.kind), tk.span);
		}
		tk
	}
}
