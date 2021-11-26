use aterms::*;

#[test]
fn test_parse() {
    let input = "Term()";

    let output = parse_term(input);

    assert!(output.is_ok());
}

