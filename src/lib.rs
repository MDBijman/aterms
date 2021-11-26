extern crate nom;
use core::fmt::Write;
use indenter::{indented, Format};
use nom::{
    branch::alt,
    bytes::complete::{tag, take, take_while1},
    character::complete::{anychar, char, multispace0, none_of},
    combinator::{map, map_opt, map_res, opt, verify},
    error::{ParseError, VerboseError},
    multi::{fold_many0, separated_list0},
    number::complete::double,
    sequence::{delimited, pair, preceded, separated_pair, tuple},
    IResult, Parser,
};
use std::fmt;
use std::fs;
use std::hash::{Hash, Hasher};
use std::mem;

#[derive(Debug, Clone, Hash)]
pub struct Annotations {
    pub elems: Vec<Term>,
}

impl Annotations {
    pub fn empty() -> Annotations {
        Annotations { elems: Vec::new() }
    }

    pub fn from(a: Vec<Term>) -> Annotations {
        Annotations { elems: a }
    }
}

impl PartialEq for Annotations {
    fn eq(&self, other: &Annotations) -> bool {
        if self.elems.len() != other.elems.len() {
            return false;
        };

        for elem in self.elems.iter() {
            if !other.elems.contains(&elem) {
                return false;
            }
        }

        true
    }
}

#[derive(Debug, Clone, Hash)]
pub enum Term {
    RTerm(RTerm),
    STerm(STerm),
    NTerm(NTerm),
    TTerm(TTerm),
    LTerm(LTerm),
}

impl PartialEq for Term {
    fn eq(&self, other: &Term) -> bool {
        match (self, other) {
            (Term::RTerm(lr), Term::RTerm(rr)) => lr == rr,
            (Term::STerm(lr), Term::STerm(rr)) => lr == rr,
            (Term::NTerm(lr), Term::NTerm(rr)) => lr == rr,
            (Term::TTerm(lr), Term::TTerm(rr)) => lr == rr,
            (Term::LTerm(lr), Term::LTerm(rr)) => lr == rr,
            (_, _) => false,
        }
    }
}

impl Term {
    pub fn new_anot_rec_term(n: &str, t: Vec<Term>, a: Vec<Term>) -> Term {
        Term::RTerm(RTerm {
            constructor: n.to_string(),
            terms: t,
            annotations: Annotations::from(a),
        })
    }

    pub fn new_rec_term(n: &str, t: Vec<Term>) -> Term {
        Term::RTerm(RTerm {
            constructor: n.to_string(),
            terms: t,
            annotations: Annotations::empty(),
        })
    }

    pub fn new_anot_string_term(n: &str, a: Vec<Term>) -> Term {
        Term::STerm(STerm {
            value: n.to_string(),
            annotations: Annotations::from(a),
        })
    }

    pub fn new_string_term(n: &str) -> Term {
        Term::STerm(STerm {
            value: n.to_string(),
            annotations: Annotations::empty(),
        })
    }

    pub fn new_anot_number_term(n: f64, a: Vec<Term>) -> Term {
        Term::NTerm(NTerm {
            value: n,
            annotations: Annotations::from(a),
        })
    }

    pub fn new_number_term(n: f64) -> Term {
        Term::NTerm(NTerm {
            value: n,
            annotations: Annotations::empty(),
        })
    }

    pub fn new_anot_list_term(t: Vec<Term>, a: Vec<Term>) -> Term {
        Term::LTerm(LTerm {
            terms: t,
            annotations: Annotations::from(a),
        })
    }

    pub fn new_list_term(t: Vec<Term>) -> Term {
        Term::LTerm(LTerm {
            terms: t,
            annotations: Annotations::empty(),
        })
    }

    pub fn new_anot_tuple_term(t: Vec<Term>, a: Vec<Term>) -> Term {
        Term::TTerm(TTerm {
            terms: t,
            annotations: Annotations::from(a),
        })
    }

    pub fn new_tuple_term(t: Vec<Term>) -> Term {
        Term::TTerm(TTerm {
            terms: t,
            annotations: Annotations::empty(),
        })
    }

    pub fn get_annotations(&self) -> &Annotations {
        match self {
            Term::RTerm(r) => &r.annotations,
            Term::STerm(s) => &s.annotations,
            Term::NTerm(n) => &n.annotations,
            Term::TTerm(t) => &t.annotations,
            Term::LTerm(l) => &l.annotations,
        }
    }

    pub fn add_annotation(&mut self, annotation: Term) {
        match self {
            Term::RTerm(a) => a.annotations.elems.push(annotation),
            Term::STerm(a) => a.annotations.elems.push(annotation),
            Term::NTerm(a) => a.annotations.elems.push(annotation),
            Term::TTerm(a) => a.annotations.elems.push(annotation),
            Term::LTerm(a) => a.annotations.elems.push(annotation),
        }
    }

    pub fn clear_annotations(&mut self) {
        match self {
            Term::RTerm(a) => a.annotations.elems.clear(),
            Term::STerm(a) => a.annotations.elems.clear(),
            Term::NTerm(a) => a.annotations.elems.clear(),
            Term::TTerm(a) => a.annotations.elems.clear(),
            Term::LTerm(a) => a.annotations.elems.clear(),
        }
    }
}

// Recursive Term
#[derive(Debug, Clone, Hash)]
pub struct RTerm {
    pub constructor: String,
    pub terms: Vec<Term>,
    pub annotations: Annotations,
}

impl PartialEq for RTerm {
    fn eq(&self, other: &RTerm) -> bool {
        self.constructor == other.constructor && self.terms == other.terms
    }
}

// String Term
#[derive(Debug, Clone, Hash)]
pub struct STerm {
    pub value: String,
    pub annotations: Annotations,
}

impl PartialEq for STerm {
    fn eq(&self, other: &STerm) -> bool {
        self.value == other.value
    }
}

// Numerical Term
#[derive(Debug, Clone)]
pub struct NTerm {
    pub value: f64,
    pub annotations: Annotations,
}

impl PartialEq for NTerm {
    fn eq(&self, other: &NTerm) -> bool {
        self.value == other.value
    }
}

fn integer_decode(val: f64) -> (u64, i16, i8) {
    let bits: u64 = unsafe { mem::transmute(val) };
    let sign: i8 = if bits >> 63 == 0 { 1 } else { -1 };
    let mut exponent: i16 = ((bits >> 52) & 0x7ff) as i16;
    let mantissa = if exponent == 0 {
        (bits & 0xfffffffffffff) << 1
    } else {
        (bits & 0xfffffffffffff) | 0x10000000000000
    };

    exponent -= 1023 + 52;
    (mantissa, exponent, sign)
}

impl Hash for NTerm {
    fn hash<H: Hasher>(&self, state: &mut H) {
        integer_decode(self.value).hash(state)
    }
}

// Tuple Term
#[derive(Debug, Clone, Hash)]
pub struct TTerm {
    pub terms: Vec<Term>,
    pub annotations: Annotations,
}

impl PartialEq for TTerm {
    fn eq(&self, other: &TTerm) -> bool {
        self.terms == other.terms
    }
}

impl TTerm {
    pub fn new() -> TTerm {
        TTerm {
            terms: Vec::new(),
            annotations: Annotations::empty(),
        }
    }
}

// List Term
#[derive(Debug, Clone, Hash)]
pub struct LTerm {
    pub terms: Vec<Term>,
    pub annotations: Annotations,
}

impl PartialEq for LTerm {
    fn eq(&self, other: &LTerm) -> bool {
        self.terms == other.terms
    }
}

impl LTerm {
    pub fn tail(&self) -> Term {
        Term::LTerm(LTerm {
            terms: self.terms[1..].iter().cloned().collect::<Vec<_>>(),
            annotations: Annotations::empty(),
        })
    }

    pub fn head(&self) -> Term {
        self.terms[0].clone()
    }
}

// Helpers

fn ws<'a, O, E: ParseError<&'a str>, F: Parser<&'a str, O, E>>(f: F) -> impl Parser<&'a str, O, E> {
    delimited(multispace0, f, multispace0)
}

fn u16_hex(input: &str) -> ParseResult<u16> {
    map_res(take(4usize), |s| u16::from_str_radix(s, 16))(input)
}

fn unicode_escape(input: &str) -> ParseResult<char> {
    map_opt(
        alt((
            // Not a surrogate
            map(verify(u16_hex, |cp| !(0xD800..0xE000).contains(cp)), |cp| {
                cp as u32
            }),
            // See https://en.wikipedia.org/wiki/UTF-16#Code_points_from_U+010000_to_U+10FFFF for details
            map(
                verify(
                    separated_pair(u16_hex, tag("\\u"), u16_hex),
                    |(high, low)| (0xD800..0xDC00).contains(high) && (0xDC00..0xE000).contains(low),
                ),
                |(high, low)| {
                    let high_ten = (high as u32) - 0xD800;
                    let low_ten = (low as u32) - 0xDC00;
                    (high_ten << 10) + low_ten + 0x10000
                },
            ),
        )),
        // Could be probably replaced with .unwrap() or _unchecked due to the verify checks
        std::char::from_u32,
    )(input)
}

fn character(input: &str) -> ParseResult<char> {
    let (input, c) = none_of("\"")(input)?;
    if c == '\\' {
        alt((
            map_res(anychar, |c| {
                Ok(match c {
                    '"' | '\\' | '/' => c,
                    'b' => '\x08',
                    'f' => '\x0C',
                    'n' => '\n',
                    'r' => '\r',
                    't' => '\t',
                    _ => return Err(()),
                })
            }),
            preceded(char('u'), unicode_escape),
        ))(input)
    } else {
        Ok((input, c))
    }
}

pub fn parse_string_literal(i: &str) -> ParseResult<String> {
    delimited(
        char('"'),
        fold_many0(character, String::new(), |mut string, c| {
            string.push(c);
            string
        }),
        char('"'),
    )(i)
}

type MyParseError<'a> = VerboseError<&'a str>;
type ParseResult<'a, O> = IResult<&'a str, O, MyParseError<'a>>;

/*
* Parsers
*/

pub fn parse_number_term(i: &str) -> ParseResult<Term> {
    map(ws(double), |d| Term::new_number_term(d))(i)
}

pub fn parse_string_term(i: &str) -> ParseResult<Term> {
    map(ws(parse_string_literal), |s| Term::new_string_term(&s))(i)
}

pub fn parse_tuple_term(i: &str) -> ParseResult<Term> {
    map(
        delimited(
            char('('),
            ws(separated_list0(ws(char(',')), parse_term)),
            char(')'),
        ),
        |ts| Term::new_tuple_term(ts),
    )(i)
}

pub fn parse_list_term(i: &str) -> ParseResult<Term> {
    map(
        pair(
            delimited(
                ws(char('[')),
                ws(separated_list0(ws(char(',')), parse_term)),
                ws(char(']')),
            ),
            opt(delimited(
                ws(char('{')),
                ws(separated_list0(ws(char(',')), parse_term)),
                ws(char('}')),
            )),
        ),
        |(list, maybe_annots)| match maybe_annots {
            Some(annots) => Term::new_anot_list_term(list, annots),
            None => Term::new_list_term(list),
        },
    )(i)
}

pub fn parse_recursive_term(i: &str) -> ParseResult<Term> {
    map(
        tuple((
            take_while1(char::is_alphanumeric),
            delimited(
                ws(char('(')),
                ws(separated_list0(ws(char(',')), parse_term)),
                ws(char(')')),
            ),
            opt(delimited(
                ws(char('{')),
                ws(separated_list0(ws(char(',')), parse_term)),
                ws(char('}')),
            )),
        )),
        |(constructor, subterms, maybe_annots)| match maybe_annots {
            Some(annots) => Term::new_anot_rec_term(constructor, subterms, annots),
            None => Term::new_rec_term(constructor, subterms),
        },
    )(i)
}

pub fn parse_term(i: &str) -> ParseResult<Term> {
    alt((
        parse_recursive_term,
        parse_list_term,
        parse_string_term,
        parse_number_term,
        parse_tuple_term,
    ))(i)
}

pub fn parse_term_from_string(i: &str) -> Result<Term, String> {
    let r = parse_term(i);

    match r {
        Ok((_, t)) => Ok(t),
        _ => Err(String::from("Parse error")),
    }
}

pub fn parse_term_from_file(path: &String) -> Result<Term, String> {
    let f = fs::read_to_string(path).unwrap();
    parse_term_from_string(f.as_str())
}

/*
* Display implementations
*/

impl fmt::Display for Annotations {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.elems.is_empty() {
            Ok(())
        } else {
            write!(f, "{{")?;
            let mut first = true;
            for v in self.elems.iter() {
                if !first {
                    write!(f, ", ")?;
                }
                first = false;

                write!(f, "{}", v)?;
            }
            write!(f, "}}")?;
            Ok(())
        }
    }
}

impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Term::RTerm(rt) => write!(f, "{}", rt),
            Term::STerm(st) => write!(f, "{}", st),
            Term::NTerm(nt) => write!(f, "{}", nt),
            Term::TTerm(tt) => write!(f, "{}", tt),
            Term::LTerm(lt) => write!(f, "{}", lt),
        }
    }
}

impl fmt::Display for RTerm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.terms.len() {
            0 => {
                write!(f, "{}()", self.constructor)
            }
            1 => {
                write!(f, "{}({})", self.constructor, self.terms[0])
            }
            _ => {
                write!(f, "{}(\n  ", self.constructor)?;
                let mut out = String::new();
                let mut first = true;
                for term in &self.terms {
                    if first {
                        out += format!("{}\n", term).as_str();
                        first = false;
                    } else {
                        out += format!(", {}\n", term).as_str();
                    }
                }
                out += ")";
                write!(
                    indented(f).with_format(Format::Uniform { indentation: "  " }),
                    "{}",
                    out
                )
            }
        }
    }
}

impl fmt::Display for STerm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.value)
    }
}

impl fmt::Display for NTerm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl fmt::Display for TTerm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "(")?;
        let mut first = true;
        for term in &self.terms {
            if !first {
                write!(f, ", ")?;
            }
            first = false;

            write!(f, "{}", term)?;
        }
        write!(f, ")")
    }
}

impl fmt::Display for LTerm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.terms.len() {
            0 => write!(f, "[]"),
            1 => write!(f, "[{}]", self.terms[0]),
            _ => {
                write!(f, "[\n  ")?;
                let mut out = String::new();
                let mut first = true;
                for term in &self.terms {
                    if first {
                        out += format!("{}\n", term).as_str();
                        first = false;
                    } else {
                        out += format!(", {}\n", term).as_str();
                    }
                }
                out += "]";
                write!(
                    indented(f).with_format(Format::Uniform { indentation: "  " }),
                    "{}",
                    out
                )
            }
        }
    }
}

pub fn ext_parse_recursive_term<'a, F, G, O, E: ParseError<&'a str>>(
    f1: F,
    o1: G,
) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
    F: Parser<&'a str, O, E> + Clone,
    G: Fn(&str, Vec<O>, Vec<O>) -> O,
{
    move |i| {
        map(
            tuple((
                take_while1(char::is_alphanumeric),
                delimited(
                    char('('),
                    ws(separated_list0(ws(char(',')), f1.clone())),
                    char(')'),
                ),
                delimited(char('{'), ws(separated_list0(ws(char(',')), f1.clone())), char('}')),
            )),
            |(con, ts, annots)| o1(con, ts, annots),
        )(i)
    }
}

pub fn ext_parse_tuple_term<'a, F, G, O, E: ParseError<&'a str>>(
    f1: F,
    o1: G,
) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
    F: Parser<&'a str, O, E> + Clone,
    G: Fn(Vec<O>, Vec<O>) -> O,
{
    move |i| {
        map(
            pair(
                delimited(
                    char('('),
                    ws(separated_list0(ws(char(',')), f1.clone())),
                    char(')'),
                ),
                delimited(char('{'), ws(separated_list0(ws(char(',')), f1.clone())), char('}')),
            ),
            |(ts, annots)| o1(ts, annots),
        )(i)
    }
}

pub fn ext_parse_list_term<'a, F, G, O, E: ParseError<&'a str>>(
    f1: F,
    o1: G,
) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
    F: Parser<&'a str, O, E> + Clone,
    G: Fn(Vec<O>, Vec<O>) -> O,
{
    move |i| {
        map(
            pair(
                delimited(
                    char('['),
                    ws(separated_list0(ws(char(',')), f1.clone())),
                    char(']'),
                ),
                delimited(char('{'), ws(separated_list0(ws(char(',')), f1.clone())), char('}')),
            ),
            |(ts, annots)| o1(ts, annots),
        )(i)
    }
}
