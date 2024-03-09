use std::cmp;
use std::path::Path;
use std::path::PathBuf;

pub type FileId = usize;

#[derive(Debug)]
pub struct InputFile {
	pub contents: String,
	pub path: PathBuf,
}

#[derive(PartialEq, Debug, Copy, Clone)]
pub struct Span {
	pub start: usize,
	pub end: usize,
	pub file_id: FileId,
}

impl Span {
	pub fn new(start: usize, end: usize, file_id: FileId) -> Self {
		Self { start, end, file_id }
	}
	pub fn at(c: usize, file_id: FileId) -> Self {
		Self { start: c, end: c + 1, file_id }
	}
	pub fn join(s1: Self, s2: Self) -> Self {
		assert!(s1.file_id == s2.file_id);
		Self { start: cmp::min(s1.start, s2.start), end: cmp::max(s1.end, s2.end), file_id: s1.file_id }
	}

	pub fn line_col(&self, input: &str) -> (usize, usize, usize, usize) {
		let (l1, c1) = line_col(input, self.start);
		let (l2, c2) = line_col(input, self.end);
		(l1, c1, l2, c2)
	}

	pub fn as_str<'a>(&self, input: &'a str) -> &'a str {
		&input[self.start..self.end]
		// &input.file_list[self.file_id].contents[self.start..self.end]
		// &input.contents[self.start..self.end]
	}

	pub fn as_str_f<'a>(&self, files: &'a [InputFile]) -> &'a str {
		&files[self.file_id].contents[self.start..self.end]
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

pub fn format_err_f(message: &str, span: Span, files: &[InputFile]) {
	print_message(message, span, &files[span.file_id].contents, &files[span.file_id].path, "error");
}

pub fn format_note_f(message: &str, span: Span, files: &[InputFile]) {
	print_message(message, span, &files[span.file_id].contents, &files[span.file_id].path, "note");
}

#[allow(dead_code)]
pub fn format_warning_f(message: &str, span: Span, files: &[InputFile]) {
	print_message(message, span, &files[span.file_id].contents, &files[span.file_id].path, "warning");
}

pub fn format_err(message: &str, span: Span, input: &str, path: &Path) {
	print_message(message, span, input, path, "error");
}

pub fn format_warning(message: &str, span: Span, input: &str, path: &Path) {
	print_message(message, span, input, path, "warning");
}

#[allow(dead_code)]
pub fn format_note(message: &str, span: Span, input: &str, path: &Path) {
	print_message(message, span, input, path, "note");
}

pub fn print_message(message: &str, span: Span, input: &str, path: &Path, level: &'static str) {
	let contents = input;

	let (startl, startc, endl, endc) = span.line_col(contents);

	let width = (endl + 1).to_string().chars().count();
	let spaces = " ".repeat(width);

	// TODO fix the path
	eprintln!("{level}: {}:{}: {message}", path.to_string_lossy(), startl + 1);
	eprintln!("{spaces} |");

	for (lc, l) in contents.lines().skip(startl).enumerate() {
		if l.is_empty() {
			continue;
		}
		let mut start = startc;
		let mut end = endc;
		if lc > 0 {
			start = 0;
		}
		if lc < endl - startl {
			end = l.len();
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
		eprintln!("{linepos:width$} | {l}");
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
