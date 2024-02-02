use crate::lexer::{Token, TokenKind};
use crate::span::Span;

#[derive(Debug)]
pub struct TokenIter {
	index: usize,
	tokens: Vec<Token>,
}

impl TokenIter {
	pub fn new(mut tokens: Vec<Token>) -> TokenIter {
		tokens.push(Token {
			span: Span::at(0),
			kind: TokenKind::Eof,
		});
		TokenIter { index: 0, tokens }
	}
	pub fn peek(&mut self) -> Token {
		*self.tokens.get(self.index).unwrap()
	}
	// pub fn peek_n(&mut self, n: usize) -> Option<Token> {
	// 	self.tokens.get(self.index + n).copied()
	// }
	pub fn next(&mut self) -> Token {
		let tk = *self.tokens.get(self.index).unwrap();
		self.index += 1;
		tk
	}
	// TODO: try to get rid of this
	pub fn prev(&mut self) -> Option<Token> {
		if self.index == 0 {
			panic!("Trying to read negative character index (THIS SHOULD NOT HAPPEN!)")
		} else {
			self.index -= 1;
			self.tokens.get(self.index).copied()
		}
	}
	pub fn assert_next(&mut self, knd: TokenKind) {
		if self.next().kind == knd {
			()
		} else {
			panic!("Expected: {:?}", knd)
		}
	}
	pub fn cur(&mut self) -> Option<Token> {
		if self.index == 0 {
			return None;
		} else {
			self.tokens.get(self.index - 1).copied()
		}
	}
}
