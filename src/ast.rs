use crate::span::Span;
use crate::symbol::SymbolId;
use crate::ty::TyAst;
use std::fmt;

#[derive(Debug)]
pub struct File {
	pub block: Block,
}

#[derive(Debug)]
pub struct Block {
	pub stats: Vec<Stat>,
}

#[derive(Debug)]
pub enum Stat {
	Assignment(Assignment),
	AssignOp(AssignOp),
	Let(Let),
	Call(Call),
	Block(Block),
	WhileBlock(WhileBlock),
	IfBlock(IfBlock),
	ForBlock(ForBlock),
	FnDef(FnDef),
	Break,
	Return(Return),
	Import(Import),
}

#[derive(Debug)]
pub struct Import {
	pub name: String,
	pub alias: String,
}

#[derive(Debug)]
pub struct Return {
	pub span: Span,
	pub exprs: Vec<Expr>,
}

#[derive(Debug)]
pub struct IfBlock {
	pub expr: Expr,
	pub block: Block,
	pub elseif: Vec<ElseIf>,
	pub else_block: Option<Block>,
}

#[derive(Debug)]
pub struct ElseIf {
	pub expr: Expr,
	pub block: Block,
}

#[derive(Debug)]
pub struct WhileBlock {
	pub expr: Expr,
	pub block: Block,
}

#[derive(Debug)]
pub struct ForBlock {
	pub names: Vec<Name>,
	pub expr: Expr,
	pub block: Block,
}

#[derive(Debug)]
pub struct Assignment {
	pub vars: Vec<Expr>,
	pub exprs: Vec<Expr>,
	pub span: Span,
}

#[derive(Debug)]
pub struct AssignOp {
	pub var: Expr,
	pub expr: Expr,
	pub op: BinOp,
	pub span: Span,
}

#[derive(Debug)]
pub struct Let {
	pub names: Vec<NameTy>,
	pub exprs: Vec<Expr>,
	pub span: Span,
}

#[derive(Debug)]
pub struct NameTy {
	pub name: Name,
	pub ty: Option<TyAst>,
}

#[derive(Debug)]
pub struct Expr {
	pub span: Span,
	pub kind: ExprKind,
}

#[derive(Debug)]
pub enum ExprKind {
	Name(Name),
	Literal(Literal),
	BinExpr(BinExpr),
	UnExpr(UnExpr),
	Call(Call),
	Lambda(FnBody),
	Array(Vec<Expr>),
	Table(Table),
	SuffixExpr(Box<Expr>, Vec<Suffix>),
	Expr(Box<Expr>), // bracketed expression
}

#[derive(Debug)]
pub struct Table {
	pub fields: Vec<Field>,
}

#[derive(Debug)]
pub enum Suffix {
	Property(Property),
	Index(Expr),
}

#[derive(Debug)]
pub struct Call {
	pub expr: Box<Expr>,
	pub args: Vec<Expr>,
}

#[derive(Debug)]
pub struct FnDef {
	pub name: Name,
	pub body: FnBody,
}

#[derive(Debug)]
pub struct FnBody {
	pub params: Vec<Param>,
	pub body: Block,
	pub ty: Option<TyAst>, // return type
}

#[derive(Debug)]
pub struct Param {
	pub name: Name,
	pub ty: Option<TyAst>,
}

#[derive(Debug)]
pub enum Field {
	Assign(Property, Expr),
	Fn(Property, FnBody),
}

#[derive(Debug)]
pub struct Name {
	pub span: Span,
	pub id: SymbolId,
}

#[derive(Debug)]
pub struct Property {
	pub span: Span,
	pub name: String,
}

#[derive(Debug)]
pub struct BinExpr {
	pub op: BinOp,
	pub lhs: Box<Expr>,
	pub rhs: Box<Expr>,
}

#[derive(Debug)]
pub struct UnExpr {
	pub op: UnOp,
	pub expr: Box<Expr>,
}

#[derive(Debug)]
pub enum Literal {
	Nil,
	Bool(bool),
	Num(f64),
	Int(i32),
	Str(String),
}

#[derive(Debug)]
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

#[derive(Debug)]
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
