use crate::span::format_err;
use crate::span::Span;
use crate::token::*;
use std::iter::zip;

#[derive(Debug)]
pub struct Lexer<'a> {
	lexer: LexIter<'a>,
	peeked: Option<Token>,
}

// This is mostly copypasta from peekable in std
impl<'a> Lexer<'a> {
	pub fn new(input: &'a str) -> Self {
		Lexer {
			lexer: LexIter::new(input),
			peeked: None,
		}
	}
	pub fn next(&mut self) -> Token {
		match self.peeked.take() {
			Some(v) => v,
			None => self.filter_next(),
		}
	}
	pub fn peek(&mut self) -> Token {
		match self.peeked {
			Some(token) => token,
			None => {
				let token = self.filter_next();
				self.peeked = Some(token);
				token
			},
		}
	}
	fn filter_next(&mut self) -> Token {
		// consume token, filter out comment, add end of file
		match self.lexer.next() {
			Some(Token {
				kind: TokenKind::Comment,
				..
			}) => self.filter_next(),
			Some(tk) => tk,
			None => Token {
				kind: TokenKind::Eof,
				span: Span::at(self.lexer.cursor - 1), // point to last character
			},
		}
	}
}

#[derive(Debug)]
pub struct LexIter<'a> {
	input: &'a str,
	bytes: &'a [u8],
	pub cursor: usize,
	handle_escape: bool,
}

impl<'a> LexIter<'a> {
	pub fn new(input: &'a str) -> Self {
		LexIter {
			input,
			bytes: input.as_bytes(),
			cursor: 0,
			handle_escape: false,
		}
	}

	fn eat_chars(&mut self, n: usize) {
		self.cursor += n;
	}

	fn eat_char(&mut self) -> Option<char> {
		let c = self.bytes.get(self.cursor);
		self.cursor += 1;

		c.map(|b| *b as char)
	}

	fn peek_is_number(&mut self) -> bool {
		if let Some(c) = self.next_char() {
			return c.is_ascii_digit();
		}
		false
	}

	fn cur_char(&self) -> Option<char> {
		self.bytes.get(self.cursor).map(|b| *b as char)
	}

	fn next_char(&self) -> Option<char> {
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
		Token {
			kind: TokenKind::Comment,
			span: Span { start, end },
		}
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
					format_err(msg, Span::new(start, self.cursor - 1), self.input);
					panic!("{msg}");
				},
				_ => self.handle_escape = false,
			}
		}
		let end = self.cursor;
		Token {
			kind: TokenKind::Str,
			span: Span { start, end },
		}
	}

	// '[[' CONTENT ']]'
	fn multi_line_string(&mut self) -> Option<Token> {
		let start = self.cursor;
		self.eat_chars(2);
		loop {
			if self.match_chars("]]") {
				self.eat_chars(2);
				let end = self.cursor;
				break Some(Token {
					kind: TokenKind::Str,
					span: Span { start, end },
				});
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

		let span = Span { start, end };

		let kind = match s.as_str() {
			"false" => TokenKind::False,
			"true" => TokenKind::True,
			"nil" => TokenKind::Nil,
			"not" => TokenKind::Not,
			"for" => TokenKind::For,
			"in" => TokenKind::In,
			"fn" => TokenKind::Fn,
			"break" => TokenKind::Break,
			"return" => TokenKind::Return,
			"while" => TokenKind::While,
			"or" => TokenKind::Or,
			"and" => TokenKind::And,
			"if" => TokenKind::If,
			"elseif" => TokenKind::ElseIf,
			"else" => TokenKind::Else,
			_ => TokenKind::Name,
		};

		Token { kind, span }
	}

	// ^-?[0-9](\.[0-9])?
	fn number(&mut self) -> Token {
		let start = self.cursor;
		let mut s = String::new();

		// we only want a minus sign at the front
		// ^-
		if self.cur_char() == Some('-') {
			if let Some(c) = self.eat_char() {
				s.push(c);
			}
		}

		while let Some(n) = self.cur_char() {
			match n {
				'0'..='9' | '.' => {
					if let Some(c) = self.eat_char() {
						s.push(c);
					} else {
						break;
					}
				},
				_ => break,
			}
		}

		let span = Span {
			start,
			end: self.cursor,
		};

		Token {
			kind: TokenKind::Number,
			span,
		}
	}
}

fn newtoken(kind: TokenKind, start: usize, end: usize) -> Token {
	Token {
		kind,
		span: Span { start, end },
	}
}

impl Iterator for LexIter<'_> {
	type Item = Token;
	fn next(&mut self) -> Option<Token> {
		if let Some(c) = self.cur_char() {
			use TokenKind::*;
			let start = self.cursor;
			let next = self.next_char();

			match c {
				'\'' | '"' => Some(self.single_line_string()),
				'[' if next == Some('[') => self.multi_line_string(),
				'=' if next == Some('=') => {
					self.eat_chars(2);
					let end = self.cursor;
					Some(newtoken(Eq, start, end))
				},
				'=' => {
					self.eat_char();
					let end = self.cursor;
					Some(newtoken(Assign, start, end))
				},
				';' => {
					self.eat_char();
					let end = self.cursor;
					Some(newtoken(SemiColon, start, end))
				},
				'[' => {
					self.eat_char();
					let end = self.cursor;
					Some(newtoken(LBracket, start, end))
				},
				']' => {
					self.eat_char();
					let end = self.cursor;
					Some(newtoken(RBracket, start, end))
				},
				'A'..='Z' | 'a'..='z' | '_' => Some(self.identifier()),
				' ' | '\t' | '\n' | '\r' => {
					self.eat_char();
					self.next()
				},
				'.' if self.peek_is_number() => Some(self.number()),
				'0'..='9' => Some(self.number()),
				'/' if next == Some('/') => Some(self.single_line_comment()),
				'-' => {
					if self.peek_is_number() {
						return Some(self.number());
					}
					self.eat_char();
					let end = self.cursor;
					Some(newtoken(Minus, start, end))
				},
				'(' => {
					self.eat_char();
					let end = self.cursor;
					Some(newtoken(LParen, start, end))
				},
				')' => {
					self.eat_char();
					let end = self.cursor;
					Some(newtoken(RParen, start, end))
				},
				'{' => {
					self.eat_char();
					let end = self.cursor;
					Some(newtoken(LCurly, start, end))
				},
				'}' => {
					self.eat_char();
					let end = self.cursor;
					Some(newtoken(RCurly, start, end))
				},
				',' => {
					self.eat_char();
					let end = self.cursor;
					Some(newtoken(Comma, start, end))
				},
				'.' if next == Some('.') => {
					self.eat_chars(2);
					let end = self.cursor;
					Some(newtoken(Concat, start, end))
				},
				'.' => {
					self.eat_char();
					let end = self.cursor;
					Some(newtoken(Period, start, end))
				},
				':' => {
					self.eat_char();
					let end = self.cursor;
					Some(newtoken(Colon, start, end))
				},
				'<' if next == Some('=') => {
					self.eat_chars(2);
					let end = self.cursor;
					Some(newtoken(Lte, start, end))
				},
				'<' => {
					self.eat_char();
					let end = self.cursor;
					Some(newtoken(Lt, start, end))
				},
				'>' if next == Some('=') => {
					self.eat_chars(2);
					let end = self.cursor;
					Some(newtoken(Gte, start, end))
				},
				'>' => {
					self.eat_char();
					let end = self.cursor;
					Some(newtoken(Gt, start, end))
				},
				'+' => {
					self.eat_char();
					let end = self.cursor;
					Some(newtoken(Plus, start, end))
				},
				'#' => {
					self.eat_char();
					let end = self.cursor;
					Some(newtoken(Hash, start, end))
				},
				'*' => {
					self.eat_char();
					let end = self.cursor;
					Some(newtoken(Mul, start, end))
				},
				'/' => {
					self.eat_char();
					let end = self.cursor;
					Some(newtoken(Div, start, end))
				},
				'%' => {
					self.eat_char();
					let end = self.cursor;
					Some(newtoken(Mod, start, end))
				},
				'^' => {
					self.eat_char();
					let end = self.cursor;
					Some(newtoken(Pow, start, end))
				},
				'!' if next == Some('=') => {
					self.eat_chars(2);
					let end = self.cursor;
					Some(newtoken(Neq, start, end))
				},
				tk => {
					let msg = format!("Unexpected token: `{tk}`");
					format_err(&msg, Span::at(self.cursor), self.input);
					panic!("{msg}");
				},
			}
		} else {
			None
		}
	}
}
