use crate::span::Span;
use crate::symbol::SymbolId;
use crate::ty::Ty;
use std::fmt;

#[derive(Debug, PartialEq)]
pub struct File {
	pub block: Block,
}

#[derive(Debug, PartialEq)]
pub struct Block {
	pub stats: Vec<Stat>,
}

#[derive(Debug, PartialEq)]
pub enum Stat {
	Assignment(Assignment),
	Let(Let),
	Call(Call),
	Block(Block),
	WhileBlock(WhileBlock),
	IfBlock(IfBlock),
	ForBlock(ForBlock),
	FnDef(FnDef),
	Break,
	Return(Return),
}

#[derive(Debug, PartialEq)]
pub struct Return {
	pub span: Span,
	pub exprs: Vec<Expr>,
}

#[derive(Debug, PartialEq)]
pub struct IfBlock {
	pub expr: Expr,
	pub block: Block,
	pub elseif: Vec<ElseIf>,
	pub else_block: Option<Block>,
}

#[derive(Debug, PartialEq)]
pub struct ElseIf {
	pub expr: Expr,
	pub block: Block,
}

#[derive(Debug, PartialEq)]
pub struct WhileBlock {
	pub expr: Expr,
	pub block: Block,
}

#[derive(Debug, PartialEq)]
pub struct ForBlock {
	pub names: Vec<Name>,
	pub expr: Expr,
	pub block: Block,
}

#[derive(Debug, PartialEq)]
pub struct Assignment {
	pub vars: Vec<Var>,
	pub exprs: Vec<Expr>,
	pub span: Span,
}

#[derive(Debug, PartialEq)]
pub struct Var {
	pub expr: Expr,
	// pub ty: Option<Ty>,
}

#[derive(Debug, PartialEq)]
pub struct Let {
	pub names: Vec<NameTy>,
	pub exprs: Vec<Expr>,
	pub span: Span,
}

#[derive(Debug, PartialEq)]
pub struct NameTy {
	pub name: Name,
	pub ty: Option<Ty>,
}

#[derive(Debug, PartialEq)]
pub struct Expr {
	pub span: Span,
	pub kind: ExprKind,
}

#[derive(Debug, PartialEq)]
pub enum ExprKind {
	Name(Name),
	Literal(Literal),
	BinExpr(BinExpr),
	UnExpr(UnExpr),
	Call(Call),
	Lambda(FnBody),
	Array(Vec<Expr>),
	Table(Vec<Field>),
	SuffixExpr(Box<Expr>, Vec<Suffix>),
	Expr(Box<Expr>), // bracketed expression
}

#[derive(Debug, PartialEq)]
pub enum Suffix {
	Property(Property),
	Index(Expr),
}

#[derive(Debug, PartialEq)]
pub struct Call {
	pub expr: Box<Expr>,
	pub args: Vec<Expr>,
}

#[derive(Debug, PartialEq)]
pub struct FnDef {
	pub name: Name,
	pub path: Vec<Property>,
	pub body: FnBody,
}

#[derive(Debug, PartialEq)]
pub struct FnBody {
	pub params: Vec<Param>,
	pub body: Block,
	pub ty: Option<Ty>, // return type
}

#[derive(Debug, PartialEq)]
pub struct Param {
	pub name: Name,
	pub ty: Option<Ty>,
}

#[derive(Debug, PartialEq)]
pub enum Field {
	Assign(Property, Expr),
	Fn(Property, FnBody),
}

#[derive(PartialEq, Debug)]
pub struct Name {
	pub span: Span,
	pub id: SymbolId,
}

#[derive(PartialEq, Debug)]
pub struct Property {
	pub span: Span,
	pub name: String,
}

#[derive(Debug, PartialEq)]
pub struct BinExpr {
	pub op: BinOp,
	pub lhs: Box<Expr>,
	pub rhs: Box<Expr>,
}

#[derive(Debug, PartialEq)]
pub struct UnExpr {
	pub op: UnOp,
	pub expr: Box<Expr>,
}

#[derive(Debug, PartialEq)]
pub enum Literal {
	Nil,
	Bool(bool),
	Num(f64),
	Int(i32),
	Str(String),
}

#[derive(Debug, PartialEq)]
pub enum BinOp {
	Plus,
	Minus,
	Mul,
	Div,
	Pow,
	Mod,
	Concat,
	Lt,
	Lte,
	Gt,
	Gte,
	Eq,
	Neq,
	And,
	Or,
}

#[derive(Debug, PartialEq)]
pub enum UnOp {
	Minus,
	Not,
}

impl BinOp {
	pub fn priority(&self) -> i32 {
		match self {
			BinOp::Pow => 8,
			BinOp::Mul | BinOp::Div | BinOp::Mod => 6,
			BinOp::Plus | BinOp::Minus => 5,
			BinOp::Concat => 4,
			BinOp::Lt | BinOp::Gt | BinOp::Lte | BinOp::Gte | BinOp::Eq | BinOp::Neq => 3,
			BinOp::And => 2,
			BinOp::Or => 1,
		}
	}
}

impl UnOp {
	pub fn priority(&self) -> i32 {
		7
	}
}

impl fmt::Display for BinOp {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(
			f,
			"{}",
			match self {
				BinOp::Pow => "^",
				BinOp::Mul => "*",
				BinOp::Div => "/",
				BinOp::Mod => "%",
				BinOp::Plus => "+",
				BinOp::Minus => "-",
				BinOp::Concat => "..",
				BinOp::Lt => "<",
				BinOp::Gt => ">",
				BinOp::Lte => "<=",
				BinOp::Gte => ">=",
				BinOp::Eq => "==",
				BinOp::Neq => "!=",
				BinOp::And => "and",
				BinOp::Or => "or",
			}
		)
	}
}

impl fmt::Display for UnOp {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(
			f,
			"{}",
			match self {
				UnOp::Minus => "-",
				UnOp::Not => "not",
			}
		)
	}
}
