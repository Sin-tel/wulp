use crate::span::{format_err, FileId, Span};
use crate::token::*;
use std::iter::zip;

#[derive(Debug)]
pub struct Lexer<'a> {
	lex_iter: LexIter<'a>,
	peeked: Option<Token>,
}

// This is mostly copypasta from peekable in std
impl<'a> Lexer<'a> {
	pub fn new(input: &'a str, id: FileId, filename: String) -> Self {
		Lexer {
			lex_iter: LexIter::new(input, id, filename),
			peeked: None,
		}
	}
	// I don't want to return Option<Token>
	#[allow(clippy::should_implement_trait)]
	pub fn next(&mut self) -> Token {
		match self.peeked.take() {
			Some(v) => v,
			None => self.filter_next(),
		}
	}
	pub fn peek(&mut self) -> Token {
		if let Some(token) = self.peeked {
			token
		} else {
			let token = self.filter_next();
			self.peeked = Some(token);
			token
		}
	}
	fn filter_next(&mut self) -> Token {
		// consume token, filter out comment, add end of file
		match self.lex_iter.next() {
			Some(Token {
				kind: TokenKind::Comment,
				..
			}) => self.filter_next(),
			Some(tk) => tk,
			None => Token {
				kind: TokenKind::Eof,
				span: Span::at(self.lex_iter.cursor - 1, self.lex_iter.id), // point to last character
				line: self.lex_iter.line,
			},
		}
	}
}

#[derive(Debug)]
pub struct LexIter<'a> {
	input: &'a str,
	bytes: &'a [u8],
	id: FileId,
	filename: String,
	pub cursor: usize,
	pub line: usize,
	handle_escape: bool,
}

impl<'a> LexIter<'a> {
	pub fn new(input: &'a str, id: FileId, filename: String) -> Self {
		LexIter {
			input,
			bytes: input.as_bytes(),
			id,
			filename,
			cursor: 0,
			line: 0,
			handle_escape: false,
		}
	}

	fn newtoken(&self, kind: TokenKind, start: usize, end: usize) -> Token {
		Token {
			kind,
			span: Span::new(start, end, self.id),
			line: self.line,
		}
	}

	fn eat_chars(&mut self, n: usize) {
		for _ in 0..n {
			self.eat_char();
		}
	}

	fn eat_char(&mut self) -> Option<char> {
		let c = self.bytes.get(self.cursor);
		if c == Some(&b'\n') {
			self.line += 1;
		}
		self.cursor += 1;

		c.map(|b| *b as char)
	}

	fn peek_is_number(&mut self) -> bool {
		if let Some(c) = self.peek_char() {
			return c.is_ascii_digit();
		}
		false
	}

	fn cur_char(&self) -> Option<char> {
		self.bytes.get(self.cursor).map(|b| *b as char)
	}

	fn peek_char(&self) -> Option<char> {
		self.bytes.get(self.cursor + 1).map(|b| *b as char)
	}

	pub fn match_chars(&mut self, other: &str) -> bool {
		if self.bytes.len() < other.len() {
			return false;
		}
		for (c1, c2) in zip(self.bytes[self.cursor..].iter(), other.bytes()) {
			if c1 != &c2 {
				return false;
			}
		}
		true
	}

	fn single_line_comment(&mut self) -> Token {
		let start = self.cursor;
		self.eat_chars(2);
		loop {
			if self.cur_char() == Some('\n') {
				break;
			}
			if self.eat_char().is_none() {
				break;
			}
		}
		let end = self.cursor;
		self.newtoken(TokenKind::Comment, start, end)
	}

	// '\'' CONTENT '\'' | ''' CONTENT '"'
	fn single_line_string(&mut self) -> Token {
		let start = self.cursor;
		let closing = self.eat_char();
		loop {
			match self.eat_char() {
				Some(e) if Some(e) == closing && !self.handle_escape => break,
				Some('\\') => self.handle_escape = true,
				Some('\n') | None => {
					let msg = "Failed to close string.";
					format_err(
						msg,
						Span::new(start, self.cursor - 1, self.id),
						self.input,
						&self.filename,
					);
					panic!("{msg}");
				},
				_ => self.handle_escape = false,
			}
		}
		let end = self.cursor;
		self.newtoken(TokenKind::Str, start, end)
	}

	// '#"' CONTENT '"#'
	fn multi_line_string(&mut self) -> Option<Token> {
		let start = self.cursor;
		self.eat_chars(2);
		loop {
			if self.match_chars("\"#") {
				self.eat_chars(2);
				let end = self.cursor;
				break Some(self.newtoken(TokenKind::Str, start, end));
			}
			match self.eat_char() {
				Some(_) => (),
				None => break None,
			}
		}
	}

	// [A-z][A-z0-9]
	fn identifier(&mut self) -> Token {
		let mut s = String::new();
		let start = self.cursor;

		// TODO: fix ident start only with letter
		while let Some(n) = self.cur_char() {
			match n {
				'A'..='Z' | 'a'..='z' | '0'..='9' | '_' => {
					if let Some(c) = self.eat_char() {
						s.push(c);
					} else {
						break;
					}
				},
				_ => break,
			}
		}
		let end = self.cursor;

		let span = Span::new(start, end, self.id);

		let kind = match s.as_str() {
			"import" => TokenKind::Import,
			"as" => TokenKind::As,
			"from" => TokenKind::From,
			"false" => TokenKind::False,
			"true" => TokenKind::True,
			"nil" => TokenKind::Nil,
			"not" => TokenKind::Not,
			"for" => TokenKind::For,
			"in" => TokenKind::In,
			"fn" => TokenKind::Fn,
			"let" => TokenKind::Let,
			"struct" => TokenKind::Struct,
			"break" => TokenKind::Break,
			"return" => TokenKind::Return,
			"while" => TokenKind::While,
			"or" => TokenKind::Or,
			"and" => TokenKind::And,
			"if" => TokenKind::If,
			"elseif" => TokenKind::ElseIf,
			"else" => TokenKind::Else,
			"maybe" => TokenKind::Maybe,
			_ => TokenKind::Name,
		};

		Token {
			kind,
			span,
			line: self.line,
		}
	}

	fn number(&mut self) -> Token {
		let start = self.cursor;
		let mut s = String::new();
		let mut dot = false;

		while let Some(n) = self.cur_char() {
			match n {
				'0'..='9' => {
					if let Some(c) = self.eat_char() {
						s.push(c);
					} else {
						unreachable!()
					}
				},
				'.' => {
					if dot {
						break;
					}
					if let Some(c) = self.eat_char() {
						s.push(c);
						dot = true;
					} else {
						unreachable!()
					}
				},
				_ => break,
			}
		}

		let span = Span::new(start, self.cursor, self.id);

		Token {
			kind: TokenKind::Number,
			span,
			line: self.line,
		}
	}
}

impl Iterator for LexIter<'_> {
	type Item = Token;
	fn next(&mut self) -> Option<Token> {
		if let Some(c) = self.cur_char() {
			use TokenKind::*;
			let start = self.cursor;
			let next = self.peek_char();

			match c {
				'\'' | '"' => Some(self.single_line_string()),
				'#' if next == Some('"') => self.multi_line_string(),
				'#' => {
					self.eat_char();
					let end = self.cursor;
					Some(self.newtoken(Hash, start, end))
				},
				'=' if next == Some('=') => {
					self.eat_chars(2);
					let end = self.cursor;
					Some(self.newtoken(Eq, start, end))
				},
				'=' => {
					self.eat_char();
					let end = self.cursor;
					Some(self.newtoken(Assign, start, end))
				},
				';' => {
					self.eat_char();
					let end = self.cursor;
					Some(self.newtoken(SemiColon, start, end))
				},
				'[' => {
					self.eat_char();
					let end = self.cursor;
					Some(self.newtoken(LBracket, start, end))
				},
				']' => {
					self.eat_char();
					let end = self.cursor;
					Some(self.newtoken(RBracket, start, end))
				},
				'A'..='Z' | 'a'..='z' | '_' => Some(self.identifier()),
				' ' | '\t' | '\n' | '\r' => {
					self.eat_char();
					self.next()
				},
				'.' if self.peek_is_number() => Some(self.number()),
				'0'..='9' => Some(self.number()),
				'/' if next == Some('/') => Some(self.single_line_comment()),
				'-' if next == Some('>') => {
					self.eat_chars(2);
					let end = self.cursor;
					Some(self.newtoken(Arrow, start, end))
				},
				'-' if next == Some('=') => {
					self.eat_chars(2);
					let end = self.cursor;
					Some(self.newtoken(AssignMinus, start, end))
				},
				'-' => {
					self.eat_char();
					let end = self.cursor;
					Some(self.newtoken(Minus, start, end))
				},
				'(' => {
					self.eat_char();
					let end = self.cursor;
					Some(self.newtoken(LParen, start, end))
				},
				')' => {
					self.eat_char();
					let end = self.cursor;
					Some(self.newtoken(RParen, start, end))
				},
				'{' => {
					self.eat_char();
					let end = self.cursor;
					Some(self.newtoken(LCurly, start, end))
				},
				'}' => {
					self.eat_char();
					let end = self.cursor;
					Some(self.newtoken(RCurly, start, end))
				},
				',' => {
					self.eat_char();
					let end = self.cursor;
					Some(self.newtoken(Comma, start, end))
				},
				'.' if next == Some('.') => {
					self.eat_chars(2);
					if self.cur_char() == Some('=') {
						self.eat_char();
						let end = self.cursor;
						Some(self.newtoken(AssignConcat, start, end))
					} else {
						let end = self.cursor;
						Some(self.newtoken(Concat, start, end))
					}
				},
				'.' => {
					self.eat_char();
					let end = self.cursor;
					Some(self.newtoken(Period, start, end))
				},
				':' => {
					self.eat_char();
					let end = self.cursor;
					Some(self.newtoken(Colon, start, end))
				},
				'<' if next == Some('=') => {
					self.eat_chars(2);
					let end = self.cursor;
					Some(self.newtoken(Lte, start, end))
				},
				'<' => {
					self.eat_char();
					let end = self.cursor;
					Some(self.newtoken(Lt, start, end))
				},
				'>' if next == Some('=') => {
					self.eat_chars(2);
					let end = self.cursor;
					Some(self.newtoken(Gte, start, end))
				},
				'>' => {
					self.eat_char();
					let end = self.cursor;
					Some(self.newtoken(Gt, start, end))
				},
				'+' if next == Some('=') => {
					self.eat_chars(2);
					let end = self.cursor;
					Some(self.newtoken(AssignPlus, start, end))
				},
				'+' => {
					self.eat_char();
					let end = self.cursor;
					Some(self.newtoken(Plus, start, end))
				},
				'*' if next == Some('=') => {
					self.eat_chars(2);
					let end = self.cursor;
					Some(self.newtoken(AssignMul, start, end))
				},
				'*' => {
					self.eat_char();
					let end = self.cursor;
					Some(self.newtoken(Mul, start, end))
				},
				'/' if next == Some('=') => {
					self.eat_chars(2);
					let end = self.cursor;
					Some(self.newtoken(AssignDiv, start, end))
				},
				'/' => {
					self.eat_char();
					let end = self.cursor;
					Some(self.newtoken(Div, start, end))
				},
				'%' if next == Some('=') => {
					self.eat_chars(2);
					let end = self.cursor;
					Some(self.newtoken(AssignMod, start, end))
				},
				'%' => {
					self.eat_char();
					let end = self.cursor;
					Some(self.newtoken(Mod, start, end))
				},
				'^' if next == Some('=') => {
					self.eat_chars(2);
					let end = self.cursor;
					Some(self.newtoken(AssignPow, start, end))
				},
				'^' => {
					self.eat_char();
					let end = self.cursor;
					Some(self.newtoken(Pow, start, end))
				},
				'!' if next == Some('=') => {
					self.eat_chars(2);
					let end = self.cursor;
					Some(self.newtoken(Neq, start, end))
				},
				'!' => {
					self.eat_char();
					let end = self.cursor;
					Some(self.newtoken(Bang, start, end))
				},
				tk => {
					let msg = format!("Unexpected token: `{tk}`");
					format_err(&msg, Span::at(self.cursor, self.id), self.input, &self.filename);
					panic!("{msg}");
				},
			}
		} else {
			None
		}
	}
}
