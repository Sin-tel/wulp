use crate::lexer::{Token, TokenKind};

#[derive(Debug)]
pub struct TokenIter {
	index: usize,
	tokens: Vec<Token>,
}

impl TokenIter {
	pub fn new(tokens: Vec<Token>) -> TokenIter {
		TokenIter { index: 0, tokens }
	}
	pub fn peek(&mut self) -> Option<Token> {
		self.tokens.get(self.index).copied()
	}
	// pub fn peek_n(&mut self, n: usize) -> Option<Token> {
	// 	self.tokens.get(self.index + n).copied()
	// }
	pub fn next(&mut self) -> Option<Token> {
		let tk = self.tokens.get(self.index).copied();
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
		match self.next() {
			Some(Token { kind, .. }) => {
				if kind == knd {
					()
				} else {
					panic!("Expected: {:?}", knd)
				}
			},
			None => panic!("Expected: {:?}", knd),
		}
	}
	pub fn cur(&mut self) -> Option<Token> {
		self.tokens.get(self.index - 1).copied()
	}
}
