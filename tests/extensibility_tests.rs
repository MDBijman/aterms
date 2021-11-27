use aterms::extensible::*;
use nom::branch::alt;
use nom::IResult;

enum MyTerm {
    Tuple(Vec<MyTerm>, Vec<MyTerm>),
    List(Vec<MyTerm>, Vec<MyTerm>),
    Rec(String, Vec<MyTerm>, Vec<MyTerm>),
    String(String, Vec<MyTerm>),
    Number(f64, Vec<MyTerm>),

    Var(String, Vec<MyTerm>),
}

impl MyTerm {
    fn from_tuple(elems: Vec<MyTerm>, annots: Vec<MyTerm>) -> Self {
        MyTerm::Tuple(elems, annots)
    }

    fn from_list(elems: Vec<MyTerm>, annots: Vec<MyTerm>) -> Self {
        MyTerm::List(elems, annots)
    }

    fn from_recursive(con: &str, elems: Vec<MyTerm>, annots: Vec<MyTerm>) -> Self {
        MyTerm::Rec(String::from(con), elems, annots)
    }

    fn from_string(con: String, annots: Vec<MyTerm>) -> Self {
        MyTerm::String(con, annots)
    }

    fn from_number(val: f64, annots: Vec<MyTerm>) -> Self {
        MyTerm::Number(val, annots)
    }
}

fn my_parse_term<'a>(i: &'a str) -> IResult<&'a str, MyTerm> {
    alt((
        parse_recursive_term(
            aterms::shared::parse_word,
            my_parse_term,
            MyTerm::from_recursive,
        ),
        parse_tuple_term(my_parse_term, MyTerm::from_tuple),
        parse_list_term(my_parse_term, MyTerm::from_list),
        parse_string_term(my_parse_term, MyTerm::from_string),
        parse_number_term(my_parse_term, MyTerm::from_number),
    ))(i)
}

#[test]
fn parse_extensible_default() {
    assert!(my_parse_term("Term()").is_ok());
    assert!(my_parse_term("Term(a)").is_err());
}

use aterms::shared::{character, parse_string_literal};
use nom::combinator::map;
use nom::multi::fold_many0;
use nom::sequence::pair;

fn my_parse_term_with_var<'a>(i: &'a str) -> IResult<&'a str, MyTerm> {
    alt((
        parse_recursive_term(
            aterms::shared::parse_word,
            my_parse_term_with_var,
            MyTerm::from_recursive,
        ),
        parse_tuple_term(my_parse_term_with_var, MyTerm::from_tuple),
        parse_list_term(my_parse_term_with_var, MyTerm::from_list),
        parse_string_term(my_parse_term_with_var, MyTerm::from_string),
        parse_number_term(my_parse_term_with_var, MyTerm::from_number),
        map(
            pair(
                fold_many0(character, String::new(), |mut string, c| {
                    string.push(c);
                    string
                }),
                parse_annotations(my_parse_term_with_var),
            ),
            |(string, annots)| MyTerm::Var(string, annots),
        ),
    ))(i)
}

#[test]
fn parse_extensible_with_var() {
    assert!(my_parse_term_with_var("Term()").is_ok());
    assert!(my_parse_term_with_var("Term(a)").is_ok());
}
