use crate::span::Span;

#[derive(Debug, PartialEq)]
pub struct Block {
	pub stats: Vec<Stat>,
}

#[derive(Debug, PartialEq)]
pub enum Stat {
	Assignment(Assignment),
	Call(Call),
	Block(Block),
	WhileBlock(WhileBlock),
	IfBlock(IfBlock),
	ForBlock(ForBlock),
	FnDef(FnDef),
	Break,
	Return(Vec<Expr>),
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
	pub exprs: Vec<Expr>,
	pub block: Block,
}

#[derive(Debug, PartialEq)]
pub struct Assignment {
	pub vars: Vec<Expr>,
	pub exprs: Vec<Expr>,
	pub local: bool,
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
	Lambda(FnBody),
	Table(Vec<Field>),
	SuffixExpr(SuffixExpr),
	Call(Call),
	Expr(Box<Expr>), // bracketed expression
}

#[derive(Debug, PartialEq)]
pub enum Literal {
	Nil,
	Bool(bool),
	Number(f64),
	Str(String),
}

#[derive(Debug, PartialEq)]
pub struct SuffixExpr {
	pub expr: Box<Expr>,
	pub suffix: Vec<Suffix>,
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
	pub name: Vec<Name>,
	pub body: FnBody,
	pub local: bool,
}

#[derive(Debug, PartialEq)]
pub struct FnBody {
	pub params: Vec<Name>,
	pub body: Block,
}

#[derive(Debug, PartialEq)]
pub enum Field {
	Assign(Property, Expr),
	Expr(Expr),
}

#[derive(PartialEq, Debug)]
pub struct Name {
	pub span: Span,
	pub id: usize,
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
	Len,
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
