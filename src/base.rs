use core::fmt::Write;
use indenter::{indented, Format};
use nom::{branch::alt, error::VerboseError, IResult};
use std::fmt;
use std::fs;
use std::hash::{Hash, Hasher};

use crate::extensible::*;
use crate::shared::*;

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
    pub fn new_anot_rec_term(n: &str, t: Vec<Term>, a: Annotations) -> Term {
        Term::RTerm(RTerm {
            constructor: n.to_string(),
            terms: t,
            annotations: a,
        })
    }

    pub fn new_rec_term(n: &str, t: Vec<Term>) -> Term {
        Term::RTerm(RTerm {
            constructor: n.to_string(),
            terms: t,
            annotations: Annotations::empty(),
        })
    }

    pub fn new_anot_string_term(n: &str, a: Annotations) -> Term {
        Term::STerm(STerm {
            value: n.to_string(),
            annotations: a,
        })
    }

    pub fn new_string_term(n: &str) -> Term {
        Term::STerm(STerm {
            value: n.to_string(),
            annotations: Annotations::empty(),
        })
    }

    pub fn new_anot_number_term(n: f64, a: Annotations) -> Term {
        Term::NTerm(NTerm {
            value: n,
            annotations: a,
        })
    }

    pub fn new_number_term(n: f64) -> Term {
        Term::NTerm(NTerm {
            value: n,
            annotations: Annotations::empty(),
        })
    }

    pub fn new_anot_list_term(t: Vec<Term>, a: Annotations) -> Term {
        Term::LTerm(LTerm {
            terms: t,
            annotations: a,
        })
    }

    pub fn new_list_term(t: Vec<Term>) -> Term {
        Term::LTerm(LTerm {
            terms: t,
            annotations: Annotations::empty(),
        })
    }

    pub fn new_anot_tuple_term(t: Vec<Term>, a: Annotations) -> Term {
        Term::TTerm(TTerm {
            terms: t,
            annotations: a,
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

    pub fn as_rterm(&self) -> Option<&RTerm> {
        if let Self::RTerm(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub fn as_sterm(&self) -> Option<&STerm> {
        if let Self::STerm(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub fn as_nterm(&self) -> Option<&NTerm> {
        if let Self::NTerm(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub fn as_tterm(&self) -> Option<&TTerm> {
        if let Self::TTerm(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub fn as_lterm(&self) -> Option<&LTerm> {
        if let Self::LTerm(v) = self {
            Some(v)
        } else {
            None
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
type MyParseError<'a> = VerboseError<&'a str>;
type ParseResult<'a, O> = IResult<&'a str, O, MyParseError<'a>>;

/*
* Parsers
*/

pub fn parse_term<'a>(i: &'a str) -> ParseResult<Term> {
    alt((
        parse_recursive_term(parse_word, parse_term, |constructor, terms, annos| {
            Term::new_anot_rec_term(constructor, terms, Annotations::from(annos))
        }),
        parse_tuple_term(parse_term, |terms, annos| {
            Term::new_anot_tuple_term(terms, Annotations::from(annos))
        }),
        parse_list_term(parse_term, |elems, annos| {
            Term::new_anot_list_term(elems, Annotations::from(annos))
        }),
        parse_string_term(parse_term, |s, annos| {
            Term::new_anot_string_term(&s, Annotations::from(annos))
        }),
        parse_number_term(parse_term, |n, annos| {
            Term::new_anot_number_term(n, Annotations::from(annos))
        }),
    ))(i)
}

pub fn parse_term_from_string(i: &str) -> Result<Term, String> {
    let r = parse_term(i);

    match r {
        Ok((_, t)) => Ok(t),
        Err(nom::Err::Error(e)) => Err(nom::error::convert_error(i, e)),
        Err(nom::Err::Failure(e)) => Err(nom::error::convert_error(i, e)),
        Err(nom::Err::Incomplete(_)) => Err(String::from("Incomplete error reported")),
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
                write!(f, "{}(){}", self.constructor, self.annotations)
            }
            1 => {
                write!(
                    f,
                    "{}({}){}",
                    self.constructor, self.terms[0], self.annotations
                )
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
                    "{}{}",
                    out,
                    self.annotations
                )
            }
        }
    }
}

impl fmt::Display for STerm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}{}", self.value, self.annotations)
    }
}

impl fmt::Display for NTerm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}", self.value, self.annotations)
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
        write!(f, "){}", self.annotations)
    }
}

impl fmt::Display for LTerm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.terms.len() {
            0 => write!(f, "[]{}", self.annotations),
            1 => write!(f, "[{}]{}", self.terms[0], self.annotations),
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
                    "{}{}",
                    out,
                    self.annotations
                )
            }
        }
    }
}
