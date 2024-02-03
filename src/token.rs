use crate::ast::{BinOp, UnOp};
use crate::span::Span;
use core::fmt;

#[derive(PartialEq, Debug, Copy, Clone)]
pub struct Token {
	pub kind: TokenKind,
	pub span: Span,
}

#[derive(PartialEq, Debug, Copy, Clone)]
pub enum TokenKind {
	And,
	Break,
	Do,
	Else,
	ElseIf,
	End,
	For,
	Function,
	If,
	In,
	Local,
	Nil,
	Not,
	Or,
	Return,
	Then,
	True,
	False,
	While,
	Concat,
	Period,
	LParen,
	RParen,
	LCurly,
	RCurly,
	LBracket,
	RBracket,
	Comma,
	Plus,
	Minus,
	Mul,
	Div,
	Mod,
	Pow,
	Assign,
	Eq,
	Neq,
	Gte,
	Lte,
	Lt,
	Gt,
	Hash,
	SemiColon,
	Colon,
	Str,
	Number,
	Ident,
	Comment,
	Eof,
}

impl Token {
	pub fn to_bin_op(&mut self) -> Option<BinOp> {
		match self.kind {
			TokenKind::Plus => Some(BinOp::Plus),
			TokenKind::Minus => Some(BinOp::Minus),
			TokenKind::Mul => Some(BinOp::Mul),
			TokenKind::Div => Some(BinOp::Div),
			TokenKind::Pow => Some(BinOp::Pow),
			TokenKind::Mod => Some(BinOp::Mod),
			TokenKind::Concat => Some(BinOp::Concat),
			TokenKind::Lt => Some(BinOp::Lt),
			TokenKind::Lte => Some(BinOp::Lte),
			TokenKind::Gt => Some(BinOp::Gt),
			TokenKind::Gte => Some(BinOp::Gte),
			TokenKind::Eq => Some(BinOp::Eq),
			TokenKind::Neq => Some(BinOp::Neq),
			TokenKind::And => Some(BinOp::And),
			TokenKind::Or => Some(BinOp::Or),
			_ => None,
		}
	}

	pub fn to_un_op(&mut self) -> Option<UnOp> {
		match self.kind {
			TokenKind::Minus => Some(UnOp::Minus),
			TokenKind::Not => Some(UnOp::Not),
			TokenKind::Hash => Some(UnOp::Len),
			_ => None,
		}
	}
}

#[derive(Debug)]
pub struct Tokens {
	index: usize,
	tokens: Vec<Token>,
}

impl Tokens {
	pub fn new(mut tokens: Vec<Token>) -> Tokens {
		tokens.push(Token {
			span: Span::at(0),
			kind: TokenKind::Eof,
		});
		Tokens { index: 0, tokens }
	}

	pub fn peek(&mut self) -> Token {
		self.tokens[self.index]
	}

	pub fn peek_n(&mut self, n: usize) -> Token {
		assert!(n > 1);
		self.tokens[self.index + (n - 1)]
	}

	pub fn next(&mut self) -> Token {
		let tk = self.tokens[self.index];
		self.index += 1;
		tk
	}
	pub fn assert_next(&mut self, knd: TokenKind) {
		if self.next().kind == knd {
			()
		} else {
			panic!("Expected: {:?}", knd)
		}
	}
	pub fn cur(&mut self) -> Token {
		if self.index == 0 {
			// This should not happen
			panic!("Trying to read negative token index.");
		} else {
			self.tokens[self.index - 1]
		}
	}
}

impl fmt::Display for Token {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "{:?}", self.kind)
	}
}

impl fmt::Display for Tokens {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		for token in self.tokens.iter() {
			write!(f, "{} ", token)?
		}
		Ok(())
	}
}
