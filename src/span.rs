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
// both start at zero! add one for displaying
fn line_col(input: &str, pos: usize) -> (usize, usize) {
	let slice = &input[..pos];

	let prec_ln = memchr::memrchr(b'\n', slice.as_bytes());
	if let Some(prec_nl_pos) = prec_ln {
		let lines = bytecount::count(slice[..=prec_nl_pos].as_bytes(), b'\n');
		(lines, slice[prec_nl_pos..].chars().count() - 1)
	} else {
		(0, slice.chars().count())
	}
}

pub fn format_err(message: &str, span: Span, input: &str) {
	print_message(message, span, input, "error");
}

pub fn format_warning(message: &str, span: Span, input: &str) {
	print_message(message, span, input, "warning");
}

pub fn format_note(message: &str, span: Span, input: &str) {
	print_message(message, span, input, "note");
}

pub fn print_message(message: &str, span: Span, input: &str, level: &'static str) {
	let (startl, startc, endl, endc) = span.line_col(input);

	let width = (endl + 1).to_string().chars().count();
	let spaces = " ".repeat(width);

	// TODO fix the filename
	eprintln!("{level}: blua\\test.blua:{}: {message}", startl + 1);
	eprintln!("{spaces} |");

	for (lc, l) in input.lines().skip(startl).enumerate() {
		if l.is_empty() {
			continue;
		}
		let mut start = startc;
		let mut end = endc;
		if lc > 0 {
			start = 0
		}
		if lc < end {
			end = l.len()
		}

		let mut underline = String::new();
		for c in l.chars().take(start) {
			match c {
				'\t' => underline.push('\t'),
				_ => underline.push(' '),
			}
		}
		for _ in 0..(end - start) {
			underline.push('^');
		}

		let linepos = lc + startl + 1;
		eprintln!("{linepos:width$} | {l}", width = width);
		eprintln!("{spaces} | {underline}");
		// linepos += 1;
		if lc >= endl - startl {
			break;
		}
	}
	// eprintln!("{} |", spaces);
	eprintln!();

	// panic!("{}", message);
}
