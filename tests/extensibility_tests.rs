use aterms::*;
use nom::branch::alt;
use nom::IResult;

enum MyTerm {
    Tuple(Vec<MyTerm>, Vec<MyTerm>),
    List(Vec<MyTerm>, Vec<MyTerm>),
    Rec(String, Vec<MyTerm>, Vec<MyTerm>),
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
}

fn my_parse_term<'a>(i: &'a str) -> IResult<&'a str, MyTerm> {
    alt((
        ext_parse_tuple_term(my_parse_term, MyTerm::from_tuple),
        ext_parse_list_term(my_parse_term, MyTerm::from_list),
        ext_parse_recursive_term(my_parse_term, MyTerm::from_recursive),
    ))(i)
}

#[test]
fn parse_extensible() {
    my_parse_term("Term()");
}
