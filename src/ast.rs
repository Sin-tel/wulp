/// Name
#[derive(PartialEq, Debug)]
pub struct Name(pub String);

/// funcname ::= Name {`.` Name} [`:` Name]
#[derive(Debug, PartialEq)]
pub struct FuncName {
	pub path: Vec<Name>,
	pub method: Option<Name>,
}

/// binop ::= `+` | `-` | `*` | `/` | `^` | `%` | `..` |
/// 	 `<` | `<=` | `>` | `>=` | `==` | `~=` |
/// 	 and | or
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

/// unop ::= `-` | not | `#` | `~`
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

/// exp binop exp
#[derive(Debug, PartialEq)]
pub struct BinExp {
	pub op: BinOp,
	pub lhs: Box<Expr>,
	pub rhs: Box<Expr>,
}

/// unop exp
#[derive(Debug, PartialEq)]
pub struct UnExp {
	pub op: UnOp,
	pub exp: Box<Expr>,
}

/// exp ::= nil | false | true | Numeral | LiteralString | functiondef |
///         prefixexp | tableconstructor | exp binop exp | unop exp
#[derive(Debug, PartialEq)]
pub enum Expr {
	Nil,
	Bool(bool),
	Num(f64),
	Str(String),
	FuncDef(FunctionDef),
	PrefixExp(Box<PrefixExpr>),
	Table(TableConstructor),
	BinExp(BinExp),
	UnExp(UnExp),
}

/// field ::= `[` exp `]` | `=` exp | Name `=` exp | exp
#[derive(Debug, PartialEq)]
pub enum Field {
	NameAssign(Name, Expr),
	ExprAssign(Expr, Expr),
	PosAssign(Expr),
}

/// tableconstructor ::= `{` [fieldlist] `}`
#[derive(Debug, PartialEq)]
pub struct TableConstructor(pub Vec<Field>);

/// args ::=  `(` [explist] `)`
#[derive(Debug, PartialEq)]
pub struct Args(pub Vec<Expr>);

/// functioncall ::=  prefixexp args | prefixexp `:` Name args
#[derive(Debug, PartialEq)]
pub struct FunctionCall {
	pub expr: Box<PrefixExpr>,
	pub args: Args,
}

/// prefixexp `[` exp `]`
#[derive(Debug, PartialEq)]
pub struct IndexExpr {
	pub expr: Box<PrefixExpr>,
	pub arg: Expr,
}

/// prefixexp `.` Name
#[derive(Debug, PartialEq)]
pub struct Property {
	pub expr: Box<PrefixExpr>,
	pub name: Name,
}

/// var ::=  Name | prefixexp `[` exp `]` | prefixexp `.` Name
#[derive(Debug, PartialEq)]
pub enum Var {
	Name(Name),
	IndexExpr(IndexExpr),
	Property(Property),
}

/// prefixexp ::= var | functioncall | `(` exp `)`
#[derive(Debug, PartialEq)]
pub enum PrefixExpr {
	Var(Var),
	FunctionCall(FunctionCall),
	Expr(Expr),
}

/// block ::= {stat} [retstat]
#[derive(Debug, PartialEq)]
pub struct Block {
	pub stats: Vec<Stat>,
	pub retstat: Option<Vec<Expr>>,
}

/// varlist '=' explist
#[derive(Debug, PartialEq)]
pub struct Assignment {
	pub varlist: Vec<Var>,
	pub exprlist: Vec<Expr>,
}

/// local namelist [`=` explist]
#[derive(Debug, PartialEq)]
pub struct LocalAssignment {
	pub namelist: Vec<Name>,
	pub exprlist: Option<Vec<Expr>>, //TODO: do we need option here?
}

/// while exp do block end
#[derive(Debug, PartialEq)]
pub struct WhileBlock {
	pub expr: Expr,
	pub block: Block,
}

/// elseif exp then block
#[derive(Debug, PartialEq)]
pub struct ElseIf {
	pub expr: Expr,
	pub block: Block,
}

/// if exp then block {elseif exp then block} [else block] end
#[derive(Debug, PartialEq)]
pub struct IfBlock {
	pub expr: Expr,
	pub block: Block,
	pub elseif: Vec<ElseIf>,
	pub else_blk: Option<Block>,
}

/// for Name `=` exp `,` exp [`,` exp] do block end
#[derive(Debug, PartialEq)]
pub struct ForRange {
	pub name: Name,
	pub exprs: (Expr, Expr, Option<Expr>),
	pub block: Block,
}

/// for namelist in explist do block end
#[derive(Debug, PartialEq)]
pub struct ForIn {
	pub namelist: Vec<Name>,
	pub exprlist: Vec<Expr>,
	pub block: Block,
}

/// stat ::=  `;` |
///         varlist `=` explist |
///         functioncall |
///         label |
///         break |
///         do block end |
///         while exp do block end |
///         if exp then block {elseif exp then block} [else block] end |
///         for Name `=` exp `,` exp [`,` exp] do block end |
///         for namelist in explist do block end |
///         function funcname funcbody |
///         local function Name funcbody |
///         local namelist [`=` explist]
#[derive(Debug, PartialEq)]
pub enum Stat {
	SemiColon,
	Assignment(Assignment), // varlist '=' explist
	FunctionCall(FunctionCall),
	Break,
	DoBlock(Block),
	WhileBlock(WhileBlock),
	IfBlock(Box<IfBlock>),
	ForRange(Box<ForRange>),
	ForIn(ForIn),
	FunctionDef(FunctionDef),
	LocalFunctionDef(LocalFunctionDef),
	LocalAssignment(LocalAssignment),
}

/// funcbody ::= `(` [parlist] `)` block end
#[derive(Debug, PartialEq)]
pub struct FuncBody {
	pub params: Params,
	pub body: Block,
}

/// local function Name funcbody
#[derive(Debug, PartialEq)]
pub struct LocalFunctionDef {
	pub name: Name,
	pub body: FuncBody,
}

/// function funcname funcbody
#[derive(Debug, PartialEq)]
pub struct FunctionDef {
	pub name: FuncName,
	pub body: FuncBody,
}

/// parlist ::= namelist [`,`]
#[derive(Debug, PartialEq)]
pub struct Params {
	pub names: Vec<Name>,
}
