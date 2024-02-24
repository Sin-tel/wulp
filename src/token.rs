use crate::ast::{BinOp, UnOp};
use crate::span::Span;
use core::fmt;

#[derive(PartialEq, Debug, Copy, Clone)]
pub struct Token {
	pub kind: TokenKind,
	pub span: Span,
	pub line: usize,
}

#[derive(PartialEq, Debug, Copy, Clone)]
pub enum TokenKind {
	And,
	Break,
	Else,
	ElseIf,
	For,
	Fn,
	If,
	In,
	Let,
	Nil,
	Not,
	Or,
	Return,
	While,
	True,
	False,
	Import,
	As,
	From,

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
	Concat,
	Assign,
	AssignPlus,
	AssignMinus,
	AssignMul,
	AssignDiv,
	AssignMod,
	AssignPow,
	AssignConcat,
	Eq,
	Neq,
	Gte,
	Lte,
	Lt,
	Gt,
	SemiColon,
	Colon,
	Arrow,
	Str,
	Number,
	Name,
	Comment,
	TyNum,
	TyInt,
	TyStr,
	TyBool,
	TyMaybe,
	Eof,
}

impl Token {
	pub fn as_bin_op(&self) -> Option<BinOp> {
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

	pub fn assign_to_bin(&self) -> Option<BinOp> {
		match self.kind {
			TokenKind::AssignPlus => Some(BinOp::Plus),
			TokenKind::AssignMinus => Some(BinOp::Minus),
			TokenKind::AssignMul => Some(BinOp::Mul),
			TokenKind::AssignDiv => Some(BinOp::Div),
			TokenKind::AssignPow => Some(BinOp::Pow),
			TokenKind::AssignMod => Some(BinOp::Mod),
			TokenKind::AssignConcat => Some(BinOp::Concat),
			_ => None,
		}
	}

	pub fn as_un_op(&self) -> Option<UnOp> {
		match self.kind {
			TokenKind::Minus => Some(UnOp::Minus),
			TokenKind::Not => Some(UnOp::Not),
			_ => None,
		}
	}
}

impl fmt::Display for TokenKind {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		use TokenKind::*;
		write!(
			f,
			"{}",
			match self {
				And => "and",
				Break => "break",
				Else => "else",
				ElseIf => "elseif",
				For => "for",
				Fn => "fn",
				If => "if",
				In => "in",
				Nil => "nil",
				Not => "not",
				Or => "or",
				Return => "return",
				True => "true",
				False => "false",
				While => "while",
				Let => "let",
				Import => "import",
				As => "as",
				From => "from",

				Period => ".",
				LParen => "(",
				RParen => ")",
				LCurly => "{",
				RCurly => "}",
				LBracket => "[",
				RBracket => "]",
				Comma => ",",
				Plus => "+",
				Minus => "-",
				Mul => "*",
				Div => "/",
				Mod => "%",
				Pow => "^",
				Concat => "..",
				Assign => "=",
				AssignPlus => "+=",
				AssignMinus => "-=",
				AssignMul => "*=",
				AssignDiv => "/=",
				AssignMod => "%=",
				AssignPow => "^=",
				AssignConcat => "..=",
				Eq => "==",
				Neq => "!=",
				Gte => ">=",
				Lte => "<=",
				Lt => "<",
				Gt => ">",
				SemiColon => ";",
				Colon => ":",
				Arrow => "->",
				Str => "string",
				Number => "number",
				Name => "identifier",
				Comment => "comment",
				TyNum => "num",
				TyInt => "int",
				TyStr => "str",
				TyBool => "bool",
				TyMaybe => "maybe",
				Eof => "end of file",
			}
		)
	}
}

impl fmt::Display for Token {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "{}", self.kind)
	}
}
