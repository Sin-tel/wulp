use std::fmt;
// use std::iter::zip;

pub type TyId = usize;
pub type TableId = usize;

#[derive(Debug)]
pub enum TyNode {
	Node(TyId),
	Ty(Ty),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Ty {
	Any,
	Bottom,
	Nil,
	Bool,
	Str,
	Num,
	Int,
	TyVar,
	Table(TableId),
	Array(TyId),
	Maybe(TyId),
	Fn(Vec<TyId>, TyId), // args, ret
}

impl fmt::Display for TyNode {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(
			f,
			"{}",
			match self {
				TyNode::Node(id) => format!(">> {id}"),
				TyNode::Ty(ty) => ty.to_string(),
			}
		)
	}
}

// Join (union) of two types
// e.g. join(Nil, Str) -> Maybe<Str>
// if the join fails we return None,
// caller should produce an error and transform it into Any
// pub fn join(a: &Ty, b: &Ty) -> Option<Ty> {
// 	// TODO: currently doesn't work properly over variant types
// 	if subtype(a, b) {
// 		Some(b.clone())
// 	} else if subtype(b, a) {
// 		Some(a.clone())
// 	} else if a == &Ty::Nil {
// 		Some(Ty::Maybe(Box::new(b.clone())))
// 	} else if b == &Ty::Nil {
// 		Some(Ty::Maybe(Box::new(a.clone())))
// 	} else {
// 		None
// 	}
// }

// // Subtype relationship between types
// pub fn subtype(a: &Ty, b: &Ty) -> bool {
// 	if a == b {
// 		true
// 	} else {
// 		#[allow(clippy::match_same_arms)]
// 		match (a, b) {
// 			// The bottom type is a subtype of all other types
// 			(Ty::Bottom, _) => true,

// 			// All types are a subtype of Any
// 			(_, Ty::Any) => true,

// 			// covariant
// 			(Ty::Maybe(x), Ty::Maybe(y)) => subtype(x, y),
// 			(Ty::Array(x), Ty::Array(y)) => subtype(x, y),

// 			// Nil is a subtype of Maybe<T>
// 			(Ty::Nil, Ty::Maybe(_)) => true,
// 			// X is a subtype of Maybe<Y> if X is a subtype of Y
// 			(x, Ty::Maybe(y)) => subtype(x, y),

// 			(Ty::Fn(x_args, x_ret), Ty::Fn(y_args, y_ret)) => {
// 				if x_args.len() != y_args.len() {
// 					false
// 				} else {
// 					let mut ok = true;
// 					for (x, y) in zip(x_args, y_args) {
// 						// contravariant
// 						ok &= subtype(y, x);
// 					}
// 					// covariant
// 					ok && subtype(x_ret, y_ret)
// 				}
// 			},
// 			// Int is a subtype of Num
// 			(Ty::Int, Ty::Num) => true,
// 			_ => false,
// 		}
// 	}
// }

impl fmt::Display for Ty {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		let s = match self {
			Ty::Any => "Any".to_string(),
			Ty::Bottom => "Bottom".to_string(),
			Ty::Nil => "nil".to_string(),
			Ty::Bool => "bool".to_string(),
			Ty::Str => "str".to_string(),
			Ty::Num => "num".to_string(),
			Ty::Int => "int".to_string(),
			Ty::TyVar => format!("T"),
			Ty::Table(_) => "table".to_string(),
			Ty::Array(ty) => format!("[{ty}]"),
			Ty::Maybe(ty) => format!("maybe({ty})"),
			Ty::Fn(args, ret) => {
				let args = args.iter().map(|a| a.to_string()).collect::<Vec<String>>().join(", ");
				format!("fn({args}) -> {ret}")
			},
		};
		write!(f, "{s}")
	}
}

// #[cfg(test)]
// fn maybe(ty: Ty) -> Ty {
// 	return Ty::Maybe(Box::new(ty));
// }
// #[cfg(test)]
// fn array(ty: Ty) -> Ty {
// 	return Ty::Array(Box::new(ty));
// }
// #[cfg(test)]
// fn func(arg: Ty, ret: Ty) -> Ty {
// 	return Ty::Fn(vec![arg], Box::new(ret));
// }

// #[test]
// fn test_subtyping() {
// 	use Ty::*;
// 	assert!(subtype(&Int, &maybe(Num)));
// 	assert!(subtype(&maybe(Int), &maybe(Num)));

// 	assert!(!subtype(&maybe(Int), &Num));
// 	assert!(!subtype(&maybe(Str), &Num));
// 	assert!(!subtype(&Str, &maybe(maybe(Num))));

// 	assert!(subtype(&Nil, &maybe(maybe(Str))));

// 	assert!(subtype(&array(Int), &array(Num)));
// 	assert!(subtype(&array(array(array(Int))), &array(array(array(Num)))));
// 	assert!(!subtype(&array(array(Num)), &array(array(array(Num)))));
// 	assert!(!subtype(&array(array(array(Num))), &array(array(Num))));

// 	// variance
// 	assert!(subtype(&func(Int, Int), &func(Int, Int)));
// 	assert!(subtype(&func(Num, Int), &func(Int, Int)));
// 	assert!(!subtype(&func(Int, Int), &func(Num, Int)));
// 	assert!(subtype(&func(Int, Int), &func(Int, Num)));
// 	assert!(!subtype(&func(Int, Num), &func(Int, Int)));

// 	assert!(!subtype(&func(Int, Int), &Fn(vec![Int, Int], Box::new(Int))));
// }

// #[test]
// fn test_join() {
// 	use Ty::*;
// 	assert_eq!(join(&Int, &Num), Some(Num));
// 	assert_eq!(join(&Int, &Any), Some(Any));
// 	assert_eq!(join(&Int, &Str), None);

// 	assert_eq!(join(&Int, &Nil), Some(maybe(Int)));
// 	assert_eq!(join(&Nil, &Str), Some(maybe(Str)));
// 	assert_eq!(join(&Nil, &maybe(Str)), Some(maybe(Str)));
// 	assert_eq!(join(&Str, &maybe(Str)), Some(maybe(Str)));

// 	assert_eq!(join(&array(Int), &array(Num)), Some(array(Num)));
// 	assert_eq!(join(&array(Str), &array(Num)), None);
// 	assert_eq!(join(&array(Int), &Nil), Some(maybe(array(Int))));
// }
