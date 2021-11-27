use nom::{IResult, Parser, branch::alt, bytes::complete::{tag, take, take_while1}, character::complete::{anychar, char, multispace0, none_of}, combinator::{map, map_opt, map_res, verify}, error::ParseError, multi::fold_many0, sequence::{delimited, preceded, separated_pair, terminated}};
use std::mem;

#[allow(dead_code)]
pub fn dbg_in<'a, O, E: ParseError<&'a str>, F: Parser<&'a str, O, E>>(
    mut f: F,
) -> impl Parser<&'a str, O, E>
where
    O: std::fmt::Debug,
    E: std::fmt::Debug,
{
    move |input: &'a str| {
        println!("[dbg] {}", input);
        f.parse(input)
    }
}

#[allow(dead_code)]
pub fn dbg_out<'a, O, E: ParseError<&'a str>, F: Parser<&'a str, O, E>>(
    mut f: F,
) -> impl Parser<&'a str, O, E>
where
    O: std::fmt::Debug,
    E: std::fmt::Debug,
{
    move |input: &'a str| {
        let r = f.parse(input);
        println!("[dbg] {:?}", r);
        r
    }
}

pub fn integer_decode(val: f64) -> (u64, i16, i8) {
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

pub fn ws_before<'a, O, E: ParseError<&'a str>, F: Parser<&'a str, O, E>>(
    f: F,
) -> impl Parser<&'a str, O, E> {
    preceded(multispace0, f)
}

pub fn ws_after<'a, O, E: ParseError<&'a str>, F: Parser<&'a str, O, E>>(
    f: F,
) -> impl Parser<&'a str, O, E> {
    terminated(f, multispace0)
}

pub fn ws<'a, O, E: ParseError<&'a str>, F: Parser<&'a str, O, E>>(
    f: F,
) -> impl Parser<&'a str, O, E> {
    delimited(multispace0, f, multispace0)
}

pub fn u16_hex<'a, E>(input: &'a str) -> IResult<&'a str, u16, E>
where
    E: ParseError<&'a str> + nom::error::FromExternalError<&'a str, std::num::ParseIntError>,
{
    map_res(take(4usize), |s| u16::from_str_radix(s, 16))(input)
}

pub fn unicode_escape<'a, E>(input: &'a str) -> IResult<&'a str, char, E>
where
    E: ParseError<&'a str> + nom::error::FromExternalError<&'a str, std::num::ParseIntError>,
{
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

pub fn character<'a, E>(input: &'a str) -> IResult<&'a str, char, E>
where
    E: ParseError<&'a str>
        + nom::error::FromExternalError<&'a str, std::num::ParseIntError>
        + nom::error::FromExternalError<&'a str, ()>,
{
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

pub fn parse_string_literal<'a, E>(i: &'a str) -> IResult<&'a str, String, E>
where
    E: ParseError<&'a str>
        + nom::error::FromExternalError<&'a str, std::num::ParseIntError>
        + nom::error::FromExternalError<&'a str, ()>,
{
    delimited(
        char('"'),
        fold_many0(character, String::new(), |mut string, c| {
            string.push(c);
            string
        }),
        char('"'),
    )(i)
}

pub fn parse_word<'a, E>(i: &'a str) -> IResult<&'a str, &'a str, E>
where
    E: ParseError<&'a str>
        + nom::error::FromExternalError<&'a str, std::num::ParseIntError>
        + nom::error::FromExternalError<&'a str, ()>,
{
    take_while1(char::is_alphanumeric)(i)
}
