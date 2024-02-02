#[derive(PartialEq, Debug, Copy, Clone)]
pub struct Span {
	pub start: usize,
	pub end: usize,
}

impl Span {
	// pub fn empty() -> Self {
	// 	Self { start: 0, end: 0 }
	// }

	pub fn line_col(&self, input: &str) -> (usize, usize, usize, usize) {
		let (l1, c1) = line_col(input, self.start);
		let (l2, c2) = line_col(input, self.end);
		(l1, c1, l2, c2)
	}

	pub fn as_str<'a>(&self, input: &'a str) -> &'a str {
		&input[self.start..self.end]
	}

	// pub fn join(s1: Self, s2: Self) -> Self {
	// 	let start = s1.start;
	// 	let end = s2.end;
	// 	Self { start, end }
	// }
}

// https://github.com/pest-parser/pest/blob/1e407663b4aeef32e481643d0c45d834799af404/pest/src/position.rs
fn line_col(input: &str, pos: usize) -> (usize, usize) {
	let slice = &input[..pos];

	let prec_ln = memchr::memrchr(b'\n', slice.as_bytes());
	if let Some(prec_nl_pos) = prec_ln {
		let lines = bytecount::count(slice[..=prec_nl_pos].as_bytes(), b'\n') + 1;
		(lines, slice[prec_nl_pos..].chars().count())
	} else {
		(1, slice.chars().count() + 1)
	}
}
