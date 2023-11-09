use std::borrow::Cow;

use crate::toml::{Ctx, Error, Pos, Quote, Range, Token, TokenType};

#[cfg(test)]
mod test;

macro_rules! recover_on {
    ($parser:expr, $tokens:pat, $label:lifetime) => {{
        loop {
            match $parser.peek().ty {
                $tokens => {
                    break $label;
                }
                _ => {
                    $parser.next();
                }
            }
        }
    }};
}

#[derive(Debug)]
pub struct Parser<'a> {
    tokens: std::iter::Peekable<std::vec::IntoIter<Token<'a>>>,
    last: Token<'a>,
}

impl<'a> Parser<'a> {
    fn new(tokens: Vec<Token<'a>>) -> Self {
        let last = tokens.last().expect("there has to be an EOF token").clone();
        let tokens = tokens.into_iter().peekable();
        Self { tokens, last }
    }

    fn next(&mut self) -> Token<'a> {
        self.tokens.next().unwrap_or(self.last.clone())
    }

    fn peek(&mut self) -> &Token<'a> {
        self.tokens.peek().unwrap_or(&self.last)
    }

    fn peek_mut(&mut self) -> &mut Token<'a> {
        self.tokens.peek_mut().unwrap_or(&mut self.last)
    }

    fn eat_newlines(&mut self) {
        while self.peek().ty == TokenType::Newline {
            self.next();
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Ast<'a> {
    Assignment(Assignment<'a>),
    Table(Table<'a>),
    Array(Array<'a>),
}

#[derive(Debug, PartialEq)]
pub struct Table<'a> {
    pub header: TableHeader<'a>,
    pub assignments: Vec<Assignment<'a>>,
}

#[derive(Debug, PartialEq)]
pub struct TableHeader<'a> {
    l_par: Pos,
    key: Option<Key<'a>>,
    r_par: Option<Pos>,
}

#[derive(Debug, PartialEq)]
pub struct Array<'a> {
    pub header: ArrayHeader<'a>,
    pub assignments: Vec<Assignment<'a>>,
}

#[derive(Debug, PartialEq)]
pub struct ArrayHeader<'a> {
    l_pars: (Pos, Pos),
    key: Option<Key<'a>>,
    r_pars: (Option<Pos>, Option<Pos>),
}

#[derive(Debug, PartialEq)]
pub struct Assignment<'a> {
    pub key: Key<'a>,
    pub eq: Pos,
    pub val: Value<'a>,
}

#[derive(Debug, PartialEq)]
pub enum Key<'a> {
    One(Ident<'a>),
    Dotted(Vec<DottedIdent<'a>>),
}

#[derive(Debug, PartialEq)]
pub struct DottedIdent<'a> {
    pub ident: Ident<'a>,
    pub dot: Option<Pos>,
}

impl Key<'_> {
    pub fn range(&self) -> Range {
        match self {
            Key::One(i) => i.lit_range,
            Key::Dotted(_) => todo!(),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Ident<'a> {
    pub lit: &'a str,
    pub lit_range: Range,
    pub text: Cow<'a, str>,
    pub text_range: Range,
    pub kind: IdentKind,
}

impl<'a> Ident<'a> {
    fn from_plain_lit(lit: &'a str, range: Range) -> Self {
        Ident {
            lit,
            lit_range: range,
            text: Cow::Borrowed(lit),
            text_range: range,
            kind: IdentKind::Plain,
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum IdentKind {
    Plain,
    String(Quote),
}

// TODO: date and time
#[derive(Debug, PartialEq)]
pub enum Value<'a> {
    String(StringVal<'a>),
    Int(IntVal<'a>),
    Float(FloatVal<'a>),
    Bool(BoolVal),
    InlineTable(InlineTable<'a>),
    InlineArray(InlineArray<'a>),
    Invalid(&'a str, Range),
}

impl Value<'_> {
    pub fn range(&self) -> Range {
        match self {
            Value::String(s) => s.lit_range,
            Value::Int(i) => i.lit_range,
            Value::Float(f) => f.lit_range,
            Value::Bool(b) => b.lit_range,
            Value::InlineTable(t) => t.range(),
            Value::InlineArray(a) => a.range(),
            Value::Invalid(_, r) => *r,
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct StringVal<'a> {
    pub lit: &'a str,
    pub lit_range: Range,
    pub text: Cow<'a, str>,
    pub text_range: Range,
    pub quote: Quote,
}

#[derive(Debug, PartialEq)]
pub struct IntVal<'a> {
    pub lit: &'a str,
    pub lit_range: Range,
    pub val: i64,
}

impl<'a> IntVal<'a> {
    pub fn new(lit: &'a str, lit_range: Range, val: i64) -> Self {
        Self {
            lit,
            lit_range,
            val,
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct FloatVal<'a> {
    pub lit: &'a str,
    pub lit_range: Range,
    pub val: f64,
}

impl<'a> FloatVal<'a> {
    pub fn new(lit: &'a str, lit_range: Range, val: f64) -> Self {
        Self {
            lit,
            lit_range,
            val,
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct BoolVal {
    pub lit_range: Range,
    pub val: bool,
}

impl BoolVal {
    pub fn new(lit_range: Range, val: bool) -> Self {
        Self { lit_range, val }
    }
}

#[derive(Debug, PartialEq)]
pub struct InlineTable<'a> {
    pub l_par: Pos,
    pub assignments: Vec<InlineTableAssignment<'a>>,
    pub r_par: Option<Pos>,
}

impl InlineTable<'_> {
    pub fn range(&self) -> Range {
        let start = self.l_par;
        let end = self
            .r_par
            .or_else(|| self.assignments.last().map(|a| a.range().end))
            .unwrap_or_else(|| self.l_par.plus(1));
        Range { start, end }
    }
}

#[derive(Debug, PartialEq)]
pub struct InlineTableAssignment<'a> {
    pub key: Key<'a>,
    pub eq: Pos,
    pub val: Value<'a>,
    pub comma: Option<Pos>,
}

impl InlineTableAssignment<'_> {
    pub fn range(&self) -> Range {
        let start = self.key.range().start;
        let end = self
            .comma
            .map(|c| c.plus(1))
            .unwrap_or_else(|| self.val.range().end);
        Range { start, end }
    }
}

#[derive(Debug, PartialEq)]
pub struct InlineArray<'a> {
    pub l_par: Pos,
    pub values: Vec<InlineArrayValue<'a>>,
    pub r_par: Option<Pos>,
}

impl InlineArray<'_> {
    pub fn range(&self) -> Range {
        let start = self.l_par;
        let end = self
            .r_par
            .or_else(|| self.values.last().map(|a| a.range().end))
            .unwrap_or_else(|| self.l_par.plus(1));
        Range { start, end }
    }
}

#[derive(Debug, PartialEq)]
pub struct InlineArrayValue<'a> {
    pub val: Value<'a>,
    pub comma: Option<Pos>,
}

impl InlineArrayValue<'_> {
    pub fn range(&self) -> Range {
        let start = self.val.range().start;
        let end = self
            .comma
            .map(|c| c.plus(1))
            .unwrap_or_else(|| self.val.range().end);
        Range { start, end }
    }
}

enum Header<'a> {
    Table(Table<'a>),
    Array(Array<'a>),
}

impl<'a> Header<'a> {
    fn into_ast(self) -> Ast<'a> {
        match self {
            Header::Table(t) => Ast::Table(t),
            Header::Array(a) => Ast::Array(a),
        }
    }
}

impl Ctx {
    // TODO: require newlines after toplevel assignments, array- and table headers.
    pub fn parse<'a>(&mut self, tokens: Vec<Token<'a>>) -> Result<Vec<Ast<'a>>, Error> {
        let mut parser = Parser::new(tokens);
        let mut asts = Vec::new();
        let mut last_header = None;

        loop {
            match parser.peek() {
                token if token.ty == TokenType::SquareLeft => {
                    let l_table_square = token.range;
                    parser.next();

                    let l_array_square = match parser.peek() {
                        t if t.ty == TokenType::SquareLeft => Some(parser.next().range),
                        _ => None,
                    };

                    let key = match self.parse_key(&mut parser) {
                        Ok(k) => Some(k),
                        Err(e) => {
                            self.errors.push(e);
                            loop {
                                match parser.peek().ty {
                                    TokenType::SquareRight
                                    | TokenType::Newline
                                    | TokenType::EOF => break,
                                    _ => {
                                        parser.next();
                                    }
                                }
                            }
                            None
                        }
                    };

                    let r_array_square = l_array_square.and_then(|_| match parser.peek() {
                        t if t.ty == TokenType::SquareRight => Some(parser.next().range.start),
                        t => {
                            self.errors
                                .push(Error::ExpectedRightSquareFound(t.ty.to_string(), t.range));
                            None
                        }
                    });

                    let r_table_square = match parser.peek() {
                        t if t.ty == TokenType::SquareRight => Some(parser.next().range.start),
                        t => {
                            self.errors
                                .push(Error::ExpectedRightSquareFound(t.ty.to_string(), t.range));
                            None
                        }
                    };

                    let header = match l_array_square {
                        Some(l_array_square) => {
                            let header = ArrayHeader {
                                l_pars: (l_table_square.start, l_array_square.start),
                                key,
                                r_pars: (r_array_square, r_table_square),
                            };
                            Header::Array(Array {
                                header,
                                assignments: Vec::new(),
                            })
                        }
                        None => {
                            let header = TableHeader {
                                l_par: l_table_square.start,
                                key,
                                r_par: r_table_square,
                            };
                            Header::Table(Table {
                                header,
                                assignments: Vec::new(),
                            })
                        }
                    };

                    match &mut last_header {
                        Some(last) => {
                            let last = std::mem::replace(last, header);
                            asts.push(last.into_ast());
                        }
                        None => last_header = Some(header),
                    }
                    continue;
                }
                t if t.ty == TokenType::Newline => {
                    parser.next();
                    continue;
                }
                t if t.ty == TokenType::EOF => break,
                _ => (),
            }

            let key = match self.parse_key(&mut parser) {
                Ok(k) => k,
                Err(_e) => todo!("push error and try to recover"),
            };

            // TODO: somehow try to recover, probably on newline
            let eq = match parser.next() {
                t if t.ty == TokenType::Equal => t.range.start,
                _ => todo!("error"),
            };

            let val = match self.parse_value(&mut parser) {
                Ok(v) => v,
                Err(e) => {
                    self.errors.push(e);
                    break;
                }
            };

            let assignment = Assignment { key, eq, val };
            match &mut last_header {
                Some(Header::Table(t)) => t.assignments.push(assignment),
                Some(Header::Array(a)) => a.assignments.push(assignment),
                None => asts.push(Ast::Assignment(assignment)),
            }
        }

        if let Some(last) = last_header {
            asts.push(last.into_ast());
        }

        Ok(asts)
    }

    fn parse_key<'a>(&mut self, parser: &mut Parser<'a>) -> Result<Key<'a>, Error> {
        let mut idents = Vec::new();
        loop {
            let token = parser.peek_mut();
            let ident = match &mut token.ty {
                TokenType::String {
                    quote,
                    lit,
                    text,
                    text_range,
                } => Ident {
                    lit_range: token.range,
                    lit,
                    text: std::mem::take(text),
                    text_range: *text_range,
                    kind: IdentKind::String(*quote),
                },
                TokenType::LiteralOrIdent(lit) => {
                    let invalid_char = lit
                        .char_indices()
                        .find(|(_, c)| !matches!(c, 'a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '-'));

                    if let Some((i, c)) = invalid_char {
                        let mut pos = token.range.start;
                        pos.char += i as u32;
                        self.errors.push(Error::InvalidCharInIdentifier(c, pos));
                    }

                    Ident::from_plain_lit(lit, token.range)
                }
                TokenType::Comment(_) => todo!(),
                TokenType::SquareLeft => todo!(),
                TokenType::SquareRight => todo!(),
                TokenType::CurlyLeft => todo!(),
                TokenType::CurlyRight => todo!(),
                TokenType::Equal => {
                    return Err(Error::ExpectedKey(token.ty.to_string(), token.range))
                }
                TokenType::Comma => todo!(),
                TokenType::Dot => todo!(),
                TokenType::Newline => todo!(),
                TokenType::Invalid(_) => todo!(),
                TokenType::EOF => todo!(),
            };
            parser.next();

            match parser.peek() {
                t if t.ty == TokenType::Dot => {
                    let dot = Some(t.range.start);
                    idents.push(DottedIdent { ident, dot });
                    parser.next();
                }
                _ => {
                    if idents.is_empty() {
                        return Ok(Key::One(ident));
                    } else {
                        idents.push(DottedIdent { ident, dot: None });
                        return Ok(Key::Dotted(idents));
                    }
                }
            }
        }
    }

    fn parse_value<'a>(&mut self, parser: &mut Parser<'a>) -> Result<Value<'a>, Error> {
        let token = parser.peek();
        let value = match &token.ty {
            TokenType::String { .. } => {
                let token = parser.next();
                let TokenType::String {
                    quote,
                    lit,
                    text,
                    text_range,
                } = token.ty
                else {
                    unreachable!()
                };

                Value::String(StringVal {
                    lit,
                    lit_range: token.range,
                    text,
                    text_range,
                    quote,
                })
            }
            TokenType::LiteralOrIdent(_) => {
                let token = parser.next();
                let TokenType::LiteralOrIdent(lit) = token.ty else {
                    unreachable!()
                };

                match lit {
                    "true" => Value::Bool(BoolVal::new(token.range, true)),
                    "false" => Value::Bool(BoolVal::new(token.range, false)),
                    "nan" => Value::Float(FloatVal::new(lit, token.range, f64::NAN)),
                    "+nan" => Value::Float(FloatVal::new(lit, token.range, f64::NAN)),
                    "-nan" => Value::Float(FloatVal::new(lit, token.range, -f64::NAN)),
                    "inf" => Value::Float(FloatVal::new(lit, token.range, f64::INFINITY)),
                    "+inf" => Value::Float(FloatVal::new(lit, token.range, f64::INFINITY)),
                    "-inf" => Value::Float(FloatVal::new(lit, token.range, f64::NEG_INFINITY)),
                    _ => match parse_num_or_date(lit, token.range) {
                        Ok(PartialValue::PrefixedInt(i)) => {
                            Value::Int(IntVal::new(lit, token.range, i))
                        }
                        Ok(PartialValue::Int(i)) => self.try_to_parse_fractional_part_of_float(
                            parser,
                            lit,
                            token.range,
                            Some(i),
                        )?,
                        Ok(PartialValue::OverflowOrFloat) => self
                            .try_to_parse_fractional_part_of_float(
                                parser,
                                lit,
                                token.range,
                                None,
                            )?,
                        Ok(PartialValue::FloatWithExp) => match lit.replace('_', "").parse() {
                            Ok(f) => Value::Float(FloatVal::new(lit, token.range, f)),
                            Err(e) => todo!("push error {e}"),
                        },
                        Err(e) => {
                            self.errors.push(e);
                            Value::Invalid(lit, token.range)
                        }
                    },
                }
            }
            TokenType::Comment(_) => todo!(),
            TokenType::SquareLeft => {
                let l_par = token.range.start;
                parser.next();

                let mut values = Vec::new();
                'inline_array: loop {
                    if matches!(parser.peek().ty, TokenType::SquareRight | TokenType::EOF) {
                        break;
                    }

                    parser.eat_newlines();
                    let val = match self.parse_value(parser) {
                        Ok(v) => v,
                        Err(e) => {
                            self.errors.push(e);

                            recover_on!(parser, TokenType::SquareRight | TokenType::EOF, 'inline_array)
                        }
                    };

                    parser.eat_newlines();
                    let mut value = InlineArrayValue { val, comma: None };
                    match parser.peek() {
                        t if t.ty == TokenType::Comma => {
                            value.comma = Some(parser.next().range.start);
                        }
                        t if t.ty == TokenType::SquareRight || t.ty == TokenType::EOF => {
                            values.push(value);
                            break;
                        }
                        _ => {
                            // TODO: maybe try to eat dot or similar character
                            let pos = value.val.range().end;
                            self.errors.push(Error::ExpectedComma(pos));
                            // try to continue
                        }
                    };

                    values.push(value);
                }

                parser.eat_newlines();
                let r_par = match parser.peek() {
                    t if t.ty == TokenType::SquareRight => Some(parser.next().range.start),
                    t => {
                        self.errors
                            .push(Error::ExpectedRightSquareFound(t.ty.to_string(), t.range));
                        None
                    }
                };

                Value::InlineArray(InlineArray {
                    l_par,
                    values,
                    r_par,
                })
            }
            TokenType::SquareRight => todo!(),
            TokenType::CurlyLeft => {
                let l_par = token.range.start;
                parser.next();

                let mut assignments = Vec::new();
                'inline_table: loop {
                    if matches!(parser.peek().ty, TokenType::CurlyRight | TokenType::EOF) {
                        break;
                    }
                    let key = match self.parse_key(parser) {
                        Ok(k) => k,
                        Err(e) => {
                            self.errors.push(e);
                            recover_on!(parser, TokenType::CurlyRight | TokenType::Newline | TokenType::EOF, 'inline_table)
                        }
                    };

                    // TODO: somehow try to recover, probably on newline
                    let eq = match parser.peek() {
                        t if t.ty == TokenType::Equal => parser.next().range.start,
                        t => {
                            self.errors
                                .push(Error::ExpectedEqFound(t.ty.to_string(), t.range));
                            recover_on!(parser, TokenType::CurlyRight | TokenType::Newline | TokenType::EOF, 'inline_table)
                        }
                    };

                    let val = match self.parse_value(parser) {
                        Ok(v) => v,
                        Err(e) => {
                            self.errors.push(e);
                            recover_on!(parser, TokenType::CurlyRight | TokenType::Newline | TokenType::EOF, 'inline_table)
                        }
                    };

                    let mut assignment = InlineTableAssignment {
                        key,
                        eq,
                        val,
                        comma: None,
                    };
                    match parser.peek() {
                        t if t.ty == TokenType::Comma => {
                            assignment.comma = Some(parser.next().range.start);
                        }
                        t if t.ty == TokenType::CurlyRight || t.ty == TokenType::EOF => {
                            assignments.push(assignment);
                            break;
                        }
                        _ => {
                            // TODO: maybe try to eat dot or similar character
                            let pos = assignment.val.range().end;
                            self.errors.push(Error::ExpectedComma(pos));
                            // try to continue
                        }
                    };

                    assignments.push(assignment);
                }

                let r_par = match parser.peek() {
                    t if t.ty == TokenType::CurlyRight => Some(parser.next().range.start),
                    t => {
                        self.errors
                            .push(Error::ExpectedRightCurlyFound(t.ty.to_string(), t.range));
                        None
                    }
                };

                Value::InlineTable(InlineTable {
                    l_par,
                    assignments,
                    r_par,
                })
            }
            TokenType::CurlyRight => todo!(),
            TokenType::Equal => {
                return Err(Error::ExpectedValue(token.ty.to_string(), token.range))
            }
            TokenType::Comma => todo!(),
            TokenType::Dot => todo!(),
            TokenType::Newline => todo!(),
            TokenType::Invalid(_) => todo!(),
            TokenType::EOF => todo!(),
        };

        Ok(value)
    }

    fn try_to_parse_fractional_part_of_float<'a>(
        &mut self,
        parser: &mut Parser<'a>,
        int_lit: &'a str,
        int_range: Range,
        int_val: Option<i64>,
    ) -> Result<Value<'a>, Error> {
        // Check this is actually a floating point literal separated by a dot
        match parser.peek() {
            t if t.ty == TokenType::Dot && int_range.end == t.range.start => {
                parser.next();
            }
            _ => match int_val {
                Some(val) => return Ok(Value::Int(IntVal::new(int_lit, int_range, val))),
                None => todo!("integer overflow error"),
            },
        };

        let frac;
        let frac_lit = match parser.peek().ty {
            TokenType::LiteralOrIdent(..) => {
                frac = parser.next();
                let TokenType::LiteralOrIdent(frac_lit) = frac.ty else {
                    unreachable!();
                };

                let int_end = int_lit.as_bytes().as_ptr_range().end;
                let frac_start = frac_lit.as_bytes().as_ptr_range().start;
                // SAFETY: we know there is a dot directly after int_lit.
                let dot_end = unsafe { int_end.add(1) };
                if dot_end == frac_start {
                    frac_lit
                } else {
                    todo!("error")
                }
            }
            _ => todo!("error"),
        };

        // SAFETY: the first and second literal reference the same string and
        // are only separated by a single dot. See above.
        let lit = unsafe {
            let ptr = int_lit.as_ptr();
            let len = int_lit.len() + 1 + frac_lit.len();
            let slice = std::slice::from_raw_parts(ptr, len);
            std::str::from_utf8_unchecked(slice)
        };

        let val = match lit.replace('_', "").parse() {
            Ok(v) => v,
            Err(e) => todo!("error {e}"),
        };

        let range = Range {
            start: int_range.start,
            end: frac.range.end,
        };

        // return as to not advance the parser
        Ok(Value::Float(FloatVal::new(lit, range, val)))
    }
}

/// A possibly only partially parsed value
enum PartialValue {
    /// An integer that is prefixed by either `0b`, `0o`, or `0x`.
    PrefixedInt(i64),
    /// A valid decimal integer, but could also be the integer part of a float.
    Int(i64),
    /// Possibly the integer part of a float, otherwise an error.
    OverflowOrFloat,
    /// A float with an exponent. There can't be a fractional part after this.
    FloatWithExp,
}

/// Parse all integers adhering to the toml spec, or the integer part of a float and it's
/// exponent.
fn parse_num_or_date<'a>(literal: &'a str, range: Range) -> Result<PartialValue, Error> {
    #[derive(PartialEq, Eq)]
    enum NumParseState {
        Int,
        OverflowOrFloat,
    }

    let mut chars = literal.char_indices().peekable();
    let c = match chars.next() {
        None => todo!("error"),
        Some((_, c)) => c,
    };

    let mut parse_state = NumParseState::Int;
    let mut int_accum;

    match c {
        '0' => match chars.next() {
            Some((_, 'b')) => {
                let val = parse_integer_literal::<1>(chars)?;
                return Ok(PartialValue::PrefixedInt(val));
            }
            Some((_, 'o')) => {
                let val = parse_integer_literal::<3>(chars)?;
                return Ok(PartialValue::PrefixedInt(val));
            }
            Some((_, 'x')) => {
                let val = parse_integer_literal::<4>(chars)?;
                return Ok(PartialValue::PrefixedInt(val));
            }
            Some((i, radix)) => todo!("invalid radix {i} {radix}"),
            None => {
                return Ok(PartialValue::Int(0));
            }
        },
        '1'..='9' => {
            int_accum = (c as u32 - '0' as u32) as i64;
        }
        _ => todo!("error"),
    }

    loop {
        let Some((_, c)) = chars.next() else { break };

        match c {
            '0'..='9' => {
                match parse_state {
                    // the literal seems to be an integer
                    NumParseState::Int => {
                        let digit = (c as u32) - ('0' as u32);
                        let (val, overflow) = int_accum.overflowing_mul(10);
                        if overflow {
                            parse_state = NumParseState::OverflowOrFloat;
                        } else {
                            int_accum = val;
                            int_accum += digit as i64;
                        }
                    }
                    // The literal would overflow if it was an int, but it could be a float.
                    NumParseState::OverflowOrFloat => {}
                }
            }
            'e' | 'E' => {
                if let Some((_, '-' | '+')) = chars.peek() {
                    chars.next();
                }

                let mut last_underscore = false;
                for i in 0.. {
                    let c = match chars.next() {
                        Some((_, c)) => c,
                        None => break,
                    };

                    last_underscore = false;

                    match c {
                        '0'..='9' => {}
                        '_' => {
                            if i == 0 {
                                todo!("push error");
                            }
                            last_underscore = true;
                            continue;
                        }
                        _ => todo!("error"),
                    }
                }

                if last_underscore {
                    todo!("push error")
                }

                return Ok(PartialValue::FloatWithExp);
            }
            '_' => continue,
            _ => todo!("error invalid character"),
        }
    }

    match parse_state {
        NumParseState::Int => Ok(PartialValue::Int(int_accum)),
        NumParseState::OverflowOrFloat => Ok(PartialValue::OverflowOrFloat),
    }
}

fn parse_integer_literal<const BITS: u32>(
    mut chars: impl Iterator<Item = (usize, char)>,
) -> Result<i64, Error> {
    let max_value: u32 = 2u32.pow(BITS);
    let mut accum: i64 = 0;
    let mut last_underscore = false;

    for i in 0.. {
        let c = match chars.next() {
            Some((_, c)) => c,
            None if i == 0 => todo!("error"),
            None => break,
        };

        last_underscore = false;

        let digit = match c {
            '0'..='9' => {
                let n = (c as u32) - ('0' as u32);
                if n >= max_value {
                    todo!("error")
                }
                n
            }
            'a'..='f' => {
                let n = 10 + (c as u32) - ('a' as u32);
                if n >= max_value {
                    todo!("error")
                }
                n
            }
            'A'..='F' => {
                let n = 10 + (c as u32) - ('A' as u32);
                if n >= max_value {
                    todo!("error")
                }
                n
            }
            '_' => {
                if i == 0 {
                    todo!("error")
                }
                last_underscore = true;
                continue;
            }
            _ => todo!("error"),
        };

        let (val, overflow) = accum.overflowing_shl(BITS);
        if overflow {
            todo!("return error")
        }

        accum = val;
        accum += digit as i64;
    }

    if last_underscore {
        todo!("error")
    }

    Ok(accum)
}
