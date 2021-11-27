use nom::{
    character::complete::char,
    combinator::{map, opt},
    error::ParseError,
    multi::separated_list0,
    number::complete::double,
    sequence::{delimited, pair, preceded, tuple},
    IResult, Parser,
};

use crate::shared::*;

pub fn parse_annotations<'a, F, O, E: ParseError<&'a str>>(
    f1: F,
) -> impl FnMut(&'a str) -> IResult<&'a str, Vec<O>, E>
where
    F: Parser<&'a str, O, E> + Clone,
{
    move |i| {
        map(
            opt(delimited(
                ws(char('{')),
                ws(separated_list0(ws(char(',')), f1.clone())),
                ws(char('}')),
            )),
            |annots| match annots {
                Some(a) => a,
                None => vec![],
            },
        )(i)
    }
}

pub fn parse_recursive_term_no_annotations<'a, F, G, H, C, O, E: ParseError<&'a str>>(
    f_con: H,
    f_rec: F,
    res_map: G,
) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
    H: Parser<&'a str, C, E> + Clone,
    F: Parser<&'a str, O, E> + Clone,
    G: Fn(C, Vec<O>) -> O,
{
    move |i| {
        map(
            pair(
                f_con.clone(),
                delimited(
                    ws(char('(')),
                    ws(separated_list0(ws(char(',')), f_rec.clone())),
                    ws(char(')')),
                ),
            ),
            |(con, ts)| res_map(con, ts),
        )(i)
    }
}

pub fn parse_recursive_term<'a, F, G, H, C, O, E: ParseError<&'a str>>(
    f_con: H,
    f_rec: F,
    res_map: G,
) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
    H: Parser<&'a str, C, E> + Clone,
    F: Parser<&'a str, O, E> + Clone,
    G: Fn(C, Vec<O>, Vec<O>) -> O,
{
    move |i| {
        map(
            tuple((
                f_con.clone(),
                delimited(
                    ws(char('(')),
                    ws(separated_list0(ws(char(',')), f_rec.clone())),
                    ws(char(')')),
                ),
                parse_annotations(f_rec.clone()),
            )),
            |(con, ts, annots)| res_map(con, ts, annots),
        )(i)
    }
}

pub fn parse_tuple_term_no_annotations<'a, F, G, O, E: ParseError<&'a str>>(
    f1: F,
    o1: G,
) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
    F: Parser<&'a str, O, E> + Clone,
    G: Fn(Vec<O>) -> O,
{
    move |i| {
        map(
            delimited(
                ws(char('(')),
                ws(separated_list0(ws(char(',')), f1.clone())),
                preceded(opt(ws(char(','))), ws(char(')'))),
            ),
            |ts| o1(ts),
        )(i)
    }
}

pub fn parse_tuple_term<'a, F, G, O, E: ParseError<&'a str>>(
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
                    ws(char('(')),
                    ws(separated_list0(ws(char(',')), f1.clone())),
                    preceded(opt(ws(char(','))), ws(char(')'))),
                ),
                parse_annotations(f1.clone()),
            ),
            |(ts, annots)| o1(ts, annots),
        )(i)
    }
}

pub fn parse_list_term_no_annotations<'a, F, G, O, E: ParseError<&'a str>>(
    f1: F,
    o1: G,
) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
    F: Parser<&'a str, O, E> + Clone,
    G: Fn(Vec<O>) -> O,
{
    move |i| {
        map(
            delimited(
                ws(char('[')),
                ws(separated_list0(ws(char(',')), f1.clone())),
                ws(char(']')),
            ),
            |ts| o1(ts),
        )(i)
    }
}

pub fn parse_list_term<'a, F, G, O, E: ParseError<&'a str>>(
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
                    ws(char('[')),
                    ws(separated_list0(ws(char(',')), f1.clone())),
                    ws(char(']')),
                ),
                parse_annotations(f1.clone()),
            ),
            |(ts, annots)| o1(ts, annots),
        )(i)
    }
}

pub fn parse_string_term_no_annotations<'a, G, O, E: ParseError<&'a str>>(
    o1: G,
) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
    G: Fn(String) -> O,
    E: nom::error::FromExternalError<&'a str, std::num::ParseIntError>
        + nom::error::FromExternalError<&'a str, ()>,
{
    move |i| map(parse_string_literal, |ts| o1(ts))(i)
}

pub fn parse_string_term<'a, F, G, O, E: ParseError<&'a str>>(
    f1: F,
    o1: G,
) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
    F: Parser<&'a str, O, E> + Clone,
    G: Fn(String, Vec<O>) -> O,
    E: nom::error::FromExternalError<&'a str, std::num::ParseIntError>
        + nom::error::FromExternalError<&'a str, ()>,
{
    move |i| {
        map(
            pair(parse_string_literal, parse_annotations(f1.clone())),
            |(ts, annots)| o1(ts, annots),
        )(i)
    }
}

pub fn parse_number_term_no_annotations<'a, G, O, E: ParseError<&'a str>>(
    o1: G,
) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
    G: Fn(f64) -> O,
{
    move |i| map(double, |ts| o1(ts))(i)
}

pub fn parse_number_term<'a, F, G, O, E: ParseError<&'a str>>(
    f1: F,
    o1: G,
) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
    F: Parser<&'a str, O, E> + Clone,
    G: Fn(f64, Vec<O>) -> O,
{
    move |i| {
        map(
            pair(ws(double), parse_annotations(f1.clone())),
            |(ts, annots)| o1(ts, annots),
        )(i)
    }
}
