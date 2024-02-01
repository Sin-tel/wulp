// for specifics on syntax, see:
// https://www.lua.org/manual/5.3/manual.html

#[derive(PartialEq, Debug, Clone)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

#[derive(PartialEq, Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

#[derive(PartialEq, Debug, Clone)]
pub enum TokenKind {
    And,
    Break,
    Do,
    Else,
    ElseIf,
    End,
    For,
    Function,
    If,
    In,
    Local,
    Nil,
    Not,
    Or,
    Return,
    Then,
    True,
    False,
    While,
    IntDiv,
    Concat,
    Dots,
    Period,
    LParen,
    RParen,
    LCurly,
    RCurly,
    LBracket,
    RBracket,
    Comma,
    Plus,
    Minus,
    Mul,
    Div,
    Mod,
    Pow,
    Assign,
    Eq,
    Neq,
    Gte,
    Lte,
    Lt,
    Gt,
    Hash,
    SemiColon,
    Colon,
    String(String),
    Number(f64),
    Ident(String),
    Comment(Comment),
}

#[derive(PartialEq, Debug, Clone)]
pub enum Comment {
    SingleLine(String),
    MultiLine(String),
}

#[derive(Debug)]
pub struct Lexer {
    input: String,
    cursor: usize,
}

impl Lexer {
    pub fn new(input: &str) -> Self {
        Lexer {
            input: String::from(input),
            cursor: 0,
        }
    }

    fn eat_chars(&mut self, n: usize) {
        self.cursor += n;
    }

    fn eat_char(&mut self) -> Option<char> {
        let c = self.input.chars().nth(self.cursor);
        self.cursor += 1;
        c
    }

    fn peek(&self, n: usize) -> Option<&str> {
        if self.input.len() <= self.cursor + n {
            None
        } else {
            Some(&self.input[self.cursor..self.cursor + n])
        }
    }

    fn peek_is_number(&mut self) -> bool {
        if let Some(c) = self.next_char() {
            return match c {
                '0'..='9' => true,
                _ => false,
            };
        }

        false
    }

    fn next_char_is_number(&self) -> bool {
        match self.next_char() {
            Some('0'..='9') => true,
            _ => false,
        }
    }

    fn cur_char(&self) -> Option<char> {
        self.input.chars().nth(self.cursor)
    }

    fn next_char(&self) -> Option<char> {
        self.input.chars().nth(self.cursor + 1)
    }

    pub fn match_chars(&mut self, other: &str) -> bool {
        if self.input.len() < other.len() {
            return false;
        }
        for (i, c) in self.input.chars().skip(self.cursor).enumerate() {
            if Some(c) != other.chars().nth(i) {
                return false;
            }
        }
        true
    }

    // '--[[' CONTENT ']]--'
    fn multi_line_comment(&mut self) -> Option<Token> {
        let start = self.cursor;

        self.eat_chars(4);

        let mut comment = String::new();

        loop {
            if self.match_chars("]]--") {
                self.eat_chars(4);
                let end = self.cursor;
                break Some(Token {
                    kind: TokenKind::Comment(Comment::MultiLine(comment)),
                    span: Span { start, end },
                });
            } else if let Some(c) = self.eat_char() {
                comment.push(c);
            } else {
                break None;
            }
        }
    }

    // '--' CONTENT '\n'?
    fn single_line_comment(&mut self) -> Option<Token> {
        let start = self.cursor;
        self.eat_chars(2);
        let mut comment = String::new();
        loop {
            if Some('\n') == self.cur_char() {
                break;
            }
            if let Some(c) = self.eat_char() {
                comment.push(c);
            } else {
                break;
            }
        }
        let end = self.cursor;
        Some(Token {
            kind: TokenKind::Comment(Comment::SingleLine(comment)),
            span: Span { start, end },
        })
    }

    // '\'' CONTENT '\'' | ''' CONTENT '"'
    fn single_line_string(&mut self) -> Option<Token> {
        let start = self.cursor;
        let closing = self.eat_char();
        let mut s = String::new();
        loop {
            match self.eat_char() {
                Some(e) if Some(e) == closing => break,
                Some(sc) if sc == '\n' => return None,
                Some(sc) => s.push(sc),
                None => return None,
            }
        }
        let end = self.cursor;
        Some(Token {
            kind: TokenKind::String(s),
            span: Span { start, end },
        })
    }

    // '[[' CONTENT ']]'
    fn multi_line_string(&mut self) -> Option<Token> {
        let start = self.cursor;
        self.eat_chars(2);
        let mut s = String::new();
        let end = self.cursor;
        loop {
            if self.match_chars("]]") {
                self.eat_chars(2);
                break Some(Token {
                    kind: TokenKind::String(s),
                    span: Span { start, end },
                });
            }
            match self.eat_char() {
                Some(sc) => s.push(sc),
                None => break None,
            }
        }
    }

    // [A-z][A-z0-9]
    fn identifier(&mut self) -> Option<Token> {
        let mut s = String::new();
        let start = self.cursor;

        // TODO(sbdchd): this should handle the case where the ident starts with a letter.
        // Right now we just don't allow numbers in idents
        while let Some(n) = self.cur_char() {
            match n {
                'A'..='Z' | 'a'..='z' | '0'..='9' | '_' => {
                    if let Some(c) = self.eat_char() {
                        s.push(c);
                    } else {
                        break;
                    }
                }
                _ => break,
            }
        }
        let end = self.cursor;

        let span = Span { start, end };

        let kind = match s.as_str() {
            "false" => TokenKind::False,
            "true" => TokenKind::True,
            "nil" => TokenKind::Nil,
            "not" => TokenKind::Not,
            "for" => TokenKind::For,
            "do" => TokenKind::Do,
            "in" => TokenKind::In,
            "function" => TokenKind::Function,
            "break" => TokenKind::Break,
            "return" => TokenKind::Return,
            "while" => TokenKind::While,
            "or" => TokenKind::Or,
            "and" => TokenKind::And,
            "end" => TokenKind::End,
            "if" => TokenKind::If,
            "then" => TokenKind::Then,
            "elseif" => TokenKind::ElseIf,
            "else" => TokenKind::Else,
            "local" => TokenKind::Local,
            _ => TokenKind::Ident(s),
        };

        Some(Token { kind, span })
    }

    // ^-?[0-9](\.[0-9])?
    fn number(&mut self) -> Option<Token> {
        let start = self.cursor;
        let mut s = String::new();

        // we only want a minus sign at the front
        // ^-
        if self.cur_char() == Some('-') {
            if let Some(c) = self.eat_char() {
                s.push(c);
            }
        }

        while let Some(n) = self.cur_char() {
            match n {
                '0'..='9' | '.' => {
                    if let Some(c) = self.eat_char() {
                        s.push(c);
                    } else {
                        break;
                    }
                }
                _ => break,
            }
        }

        let span = Span {
            start,
            end: self.cursor,
        };

        match s.parse() {
            Ok(num) => Some(Token {
                kind: TokenKind::Number(num),
                span,
            }),
            _ => panic!("Malformed number"),  // TODO: handle properly
        }
    }
}

impl Iterator for Lexer {
    type Item = Token;
    fn next(&mut self) -> Option<Token> {
        if let Some("--[[") = &self.peek(4) {
            self.multi_line_comment()
        } else if let Some(c) = self.cur_char() {
            let start = self.cursor;
            let next = self.next_char();
            match c {
                '\'' | '"' => self.single_line_string(),
                '[' if next == Some('[') => self.multi_line_string(),
                '=' if next == Some('=') => {
                    self.eat_chars(2);
                    let end = self.cursor;
                    Some(Token {
                        kind: TokenKind::Eq,
                        span: Span { start, end },
                    })
                }
                '=' => {
                    self.eat_char();
                    let end = self.cursor;
                    Some(Token {
                        kind: TokenKind::Assign,
                        span: Span { start, end },
                    })
                }
                ';' => {
                    self.eat_char();
                    let end = self.cursor;
                    Some(Token {
                        kind: TokenKind::SemiColon,
                        span: Span { start, end },
                    })
                }
                '[' => {
                    self.eat_char();
                    let end = self.cursor;
                    Some(Token {
                        kind: TokenKind::LBracket,
                        span: Span { start, end },
                    })
                }
                ']' => {
                    self.eat_char();
                    let end = self.cursor;
                    Some(Token {
                        kind: TokenKind::RBracket,
                        span: Span { start, end },
                    })
                }
                'A'..='Z' | 'a'..='z' | '_' => self.identifier(),
                ' ' | '\t' | '\n'  => {
                    self.eat_char();
                    self.next()
                }
                '.' if self.next_char_is_number() => self.number(),
                '0'..='9' => self.number(),
                '-' if next == Some('-') => self.single_line_comment(),
                '-' => {
                    if self.peek_is_number() {
                        return self.number();
                    }
                    self.eat_char();
                    let end = self.cursor;
                    Some(Token {
                        kind: TokenKind::Minus,
                        span: Span { start, end },
                    })
                }
                '(' => {
                    self.eat_char();
                    let end = self.cursor;
                    Some(Token {
                        kind: TokenKind::LParen,
                        span: Span { start, end },
                    })
                }
                ')' => {
                    self.eat_char();
                    let end = self.cursor;
                    Some(Token {
                        kind: TokenKind::RParen,
                        span: Span { start, end },
                    })
                }
                '{' => {
                    self.eat_char();
                    let end = self.cursor;
                    Some(Token {
                        kind: TokenKind::LCurly,
                        span: Span { start, end },
                    })
                }
                '}' => {
                    self.eat_char();
                    let end = self.cursor;
                    Some(Token {
                        kind: TokenKind::RCurly,
                        span: Span { start, end },
                    })
                }
                ',' => {
                    self.eat_char();
                    let end = self.cursor;
                    Some(Token {
                        kind: TokenKind::Comma,
                        span: Span { start, end },
                    })
                }
                '.' if next == Some('.') => {
                    self.eat_chars(2);
                    if self.cur_char() == Some('.') {
                        self.eat_char();
                        let end = self.cursor;
                        Some(Token {
                            kind: TokenKind::Dots,
                            span: Span { start, end },
                        })
                    } else {
                        let end = self.cursor;
                        Some(Token {
                            kind: TokenKind::Concat,
                            span: Span { start, end },
                        })
                    }
                }
                '.' => {
                    self.eat_char();
                    let end = self.cursor;
                    Some(Token {
                        kind: TokenKind::Period,
                        span: Span { start, end },
                    })
                }
                ':' => {
                    self.eat_char();
                    let end = self.cursor;
                    Some(Token {
                        kind: TokenKind::Colon,
                        span: Span { start, end },
                    })
                }
                '<' if next == Some('=') => {
                    self.eat_chars(2);
                    let end = self.cursor;
                    Some(Token {
                        kind: TokenKind::Lte,
                        span: Span { start, end },
                    })
                }
                '<' => {
                    self.eat_char();
                    let end = self.cursor;
                    Some(Token {
                        kind: TokenKind::Lt,
                        span: Span { start, end },
                    })
                }
                '>' if next == Some('=') => {
                    self.eat_chars(2);
                    let end = self.cursor;
                    Some(Token {
                        kind: TokenKind::Gte,
                        span: Span { start, end },
                    })
                }
                '>' => {
                    self.eat_char();
                    let end = self.cursor;
                    Some(Token {
                        kind: TokenKind::Gt,
                        span: Span { start, end },
                    })
                }
                '+' => {
                    self.eat_char();
                    let end = self.cursor;
                    Some(Token {
                        kind: TokenKind::Plus,
                        span: Span { start, end },
                    })
                }
                '#' => {
                    self.eat_char();
                    let end = self.cursor;
                    Some(Token {
                        kind: TokenKind::Hash,
                        span: Span { start, end },
                    })
                }
                '*' => {
                    self.eat_char();
                    let end = self.cursor;
                    Some(Token {
                        kind: TokenKind::Mul,
                        span: Span { start, end },
                    })
                }
                '/' if next == Some('/') => {
                    self.eat_chars(2);
                    let end = self.cursor;
                    Some(Token {
                        kind: TokenKind::IntDiv,
                        span: Span { start, end },
                    })
                }
                '/' => {
                    self.eat_char();
                    let end = self.cursor;
                    Some(Token {
                        kind: TokenKind::Div,
                        span: Span { start, end },
                    })
                }
                '%' => {
                    self.eat_char();
                    let end = self.cursor;
                    Some(Token {
                        kind: TokenKind::Mod,
                        span: Span { start, end },
                    })
                }
                '^' => {
                    self.eat_char();
                    let end = self.cursor;
                    Some(Token {
                        kind: TokenKind::Pow,
                        span: Span { start, end },
                    })
                }
                '~' if next == Some('=') => {
                    self.eat_chars(2);
                    let end = self.cursor;
                    Some(Token {
                        kind: TokenKind::Neq,
                        span: Span { start, end },
                    })
                }
                unknown => {
                    panic!("Unexpected token: {:?}", unknown); // TODO: handle properly
                }
            }
        } else {
            None
        }
    }
}
