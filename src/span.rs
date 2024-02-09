use std::cmp;

#[derive(PartialEq, Debug, Copy, Clone)]
pub struct Span {
	pub start: usize,
	pub end: usize,
}

impl Span {
	pub fn new(start: usize, end: usize) -> Self {
		Self { start, end }
	}
	pub fn at(c: usize) -> Self {
		Self { start: c, end: c + 1 }
	}
	pub fn join(s1: Self, s2: Self) -> Self {
		Self {
			start: cmp::min(s1.start, s2.start),
			end: cmp::max(s1.end, s2.end),
		}
	}

	pub fn line_col(&self, input: &str) -> (usize, usize, usize, usize) {
		let (l1, c1) = line_col(input, self.start);
		let (l2, c2) = line_col(input, self.end);
		(l1, c1, l2, c2)
	}

	pub fn as_str<'a>(&self, input: &'a str) -> &'a str {
		&input[self.start..self.end]
	}

	pub fn as_string(&self, input: &str) -> String {
		self.as_str(input).to_string()
	}
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

pub fn format_err(message: &str, span: Span, input: &str) {
	// TODO: this only works properly if the span is one line

	let (startl, startc, endl, endc) = span.line_col(input);

	let mut linepos = startl;

	let mut spaces = String::new();
	for _ in linepos.to_string().chars() {
		spaces.push(' ');
	}

	// TODO fix the filename
	eprintln!("error: src\\file:{startl}: {message}");
	eprintln!("{spaces} |");

	for l in input.lines().skip(startl - 1) {
		let mut underline = String::new();
		for c in l.chars().take(startc - 1) {
			match c {
				'\t' => underline.push('\t'),
				_ => underline.push(' '),
			}
		}

		for _ in 0..(endc - startc) {
			underline.push('^');
		}

		eprintln!("{linepos} | {l}");
		eprintln!("{spaces} | {underline}");
		linepos += 1;
		if linepos >= endl {
			break;
		}
	}
	// eprintln!("{} |", spaces);
	eprintln!();

	// panic!("{}", message);
}
