use crate::index::{FileId, SymbolId};
use crate::span::Span;
use crate::ty::TyAst;
use std::fmt;
use std::path::PathBuf;

#[derive(Debug)]
pub struct Module {
	pub items: Vec<Item>,
	pub file_id: FileId,
	pub global: bool,
}

#[derive(Debug)]
pub enum Item {
	Intrinsic(Intrinsic),
	InlineLua(String),
	Import(Import),
	FnDef(FnDef),
	StructDef(StructDef),
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
	Return(Return),
	Break,
}

#[derive(Debug)]
pub struct Intrinsic {
	pub name: Name,
	pub property: Option<Property>,
	pub ty: TyAst,
	pub lua_def: Option<String>,
}

#[derive(Debug)]
pub struct Import {
	pub path: PathBuf,
	pub file_id: Option<FileId>,
	pub kind: ImportKind,
}

#[derive(Debug)]
pub enum ImportKind {
	Glob,
	Alias(Name),
	From(Vec<Name>),
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
	pub named_args: Vec<NamedArg>,
}

#[derive(Debug)]
pub struct NamedArg {
	pub name: String,
	pub expr: Expr,
	pub span: Span,
}

#[derive(Debug)]
pub struct FnDef {
	pub name: Name,
	pub property: Option<Property>,
	pub body: FnBody,
}

#[derive(Debug)]
pub struct StructDef {
	pub name: Name,
	pub table: Table,
}

#[derive(Debug)]
pub struct FnBody {
	pub params: Vec<NameTy>,
	pub body: Block,
	pub ty: Option<TyAst>, // return type
}

#[derive(Debug)]
pub struct Field {
	pub field: PropertyTy,
	pub kind: FieldKind,
}

#[derive(Debug)]
pub enum FieldKind {
	Empty,
	Assign(Expr),
}

#[derive(Debug)]
pub struct NameTy {
	pub name: Name,
	pub ty: Option<TyAst>,
}

#[derive(Debug)]
pub struct Name {
	pub span: Span,
	pub id: SymbolId,
}

#[derive(Debug)]
pub struct TyName {
	pub span: Span,
	pub id: SymbolId,
}

#[derive(Debug)]
pub struct PropertyTy {
	pub property: Property,
	pub ty: Option<TyAst>,
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
	Add,
	Sub,
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
	Neg,
	Not,
}

impl Expr {
	pub fn is_const(&self) -> bool {
		match &self.kind {
			ExprKind::BinExpr(e) => e.lhs.is_const() && e.rhs.is_const(),
			ExprKind::UnExpr(e) => e.expr.is_const(),
			ExprKind::Array(array) => {
				let mut c = true;
				for e in array {
					c &= e.is_const();
				}
				c
			},
			ExprKind::Expr(e) => e.is_const(),
			ExprKind::Literal(_) | ExprKind::Lambda(_) => true,
			ExprKind::Name(_) | ExprKind::SuffixExpr(_, _) | ExprKind::Call(_) => false,
		}
	}
}
impl BinOp {
	pub fn priority(&self) -> i32 {
		match self {
			BinOp::Pow => 8,
			BinOp::Mul | BinOp::Div | BinOp::Mod => 6,
			BinOp::Add | BinOp::Sub => 5,
			BinOp::Concat => 4,
			BinOp::Lt | BinOp::Gt | BinOp::Lte | BinOp::Gte | BinOp::Eq | BinOp::Neq => 3,
			BinOp::And => 2,
			BinOp::Or => 1,
		}
	}

	pub fn overload_name(&self) -> Option<&'static str> {
		match self {
			BinOp::Add => Some("__add"),
			BinOp::Sub => Some("__sub"),
			BinOp::Mul => Some("__mul"),
			BinOp::Div => Some("__div"),
			BinOp::Mod => Some("__mod"),
			BinOp::Pow => Some("__pow"),
			BinOp::Concat => Some("__concat"),
			BinOp::Eq | BinOp::Neq => Some("__eq"),
			BinOp::Lt | BinOp::Gt => Some("__lt"),
			BinOp::Lte | BinOp::Gte => Some("__le"),
			_ => None,
		}
	}
}

impl UnOp {
	#[allow(clippy::unused_self)]
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
				BinOp::Add => "+",
				BinOp::Sub => "-",
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
				UnOp::Neg => "-",
				UnOp::Not => "not",
			}
		)
	}
}
