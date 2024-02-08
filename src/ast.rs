use crate::span::Span;
use std::fmt;

/// block -> {stat} [laststat]
#[derive(Debug, PartialEq)]
pub struct Block {
	pub stats: Vec<Stat>,
}

/// stat -> vars `=` expr_list
///       | functioncall
///       | do block end
///       | while expr do block end
///       | repeat block until expr
///       | if expr then block {elseif expr then block} [else block] end
///       | for Name `=` expr `,` expr [`,` exp] do block end
///       | for names in expr_list do block end
///       | function fn_name fn_body
/// laststat -> return [expr_list] | break
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

/// if expr then block {elseif expr then block} [else block] end
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

/// while expr do block end
#[derive(Debug, PartialEq)]
pub struct WhileBlock {
	pub expr: Expr,
	pub block: Block,
}

/// for names in expr_list do block end
#[derive(Debug, PartialEq)]
pub struct ForBlock {
	pub names: Vec<Name>,
	pub exprs: Vec<Expr>,
	pub block: Block,
}

/// vars '=' expr_list
#[derive(Debug, PartialEq)]
pub struct Assignment {
	pub vars: Vec<Expr>,
	pub exprs: Vec<Expr>,
	pub local: bool,
}

/// expr ->  literal
///       |  tableconstructor | FUNCTION body | suffix_expr
///       |  expr binop expr | unop expr | fn_call
/// tableconstructor -> `{` [fieldlist] `}`
#[derive(Debug, PartialEq)]
pub enum Expr {
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

/// literal -> nil | Bool | Numeral | String
#[derive(Debug, PartialEq)]
pub enum Literal {
	Nil,
	Bool(bool),
	Number(f64),
	Str(String),
}

/// suffix_expr -> primary_expr { suffix }
/// primary_expr -> Name | '(' expr ')'
#[derive(Debug, PartialEq)]
pub struct SuffixExpr {
	pub expr: Box<Expr>,
	pub suffix: Vec<Suffix>,
}

/// suffix -> `.` Name
///         | `[` expr `]`
#[derive(Debug, PartialEq)]
pub enum Suffix {
	Property(Name),
	Index(Expr),
}

/// fn_call -> suffix_expr args
/// args ->  `(` [expr_list] `)`
#[derive(Debug, PartialEq)]
pub struct Call {
	pub expr: Box<Expr>,
	pub args: Vec<Expr>,
}

/// function fn_name fn_body
#[derive(Debug, PartialEq)]
pub struct FnDef {
	pub name: Vec<Name>,
	pub body: FnBody,
	pub local: bool,
}

/// fn_body -> `(` [parlist] `)` block end
/// parlist -> names [`,`]
#[derive(Debug, PartialEq)]
pub struct FnBody {
	pub params: Vec<Name>,
	pub body: Block,
}

/// field -> expr | Name `=` expr
#[derive(Debug, PartialEq)]
pub enum Field {
	Assign(Name, Expr),
	Expr(Expr),
}

/// Name
#[derive(PartialEq, Debug)]
pub struct Name {
	pub id: usize,
	pub span: Span,
}

/// expr binop expr
#[derive(Debug, PartialEq)]
pub struct BinExpr {
	pub op: BinOp,
	pub lhs: Box<Expr>,
	pub rhs: Box<Expr>,
}

/// unop expr
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

/// unop -> `-` | not | `#` | `~`
#[derive(Debug, PartialEq)]
pub enum UnOp {
	Minus,
	Not,
	Len,
}

impl UnOp {
	pub fn priority(&self) -> i32 {
		7
	}
}

// These are used in the lua code gen!
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
				BinOp::Neq => "~=",
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
				UnOp::Len => "#",
			}
		)
	}
}
