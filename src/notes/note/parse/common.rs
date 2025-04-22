use nom::{IResult, bytes::complete::take_while1};

pub(super) fn parse_symbol(input: &str) -> IResult<&str, &str> {
    take_while1(valid_char)(input)
}

fn valid_char(c: char) -> bool {
    c.is_alphanumeric() || "!$%&*/:<=>?^_~+-".contains(c)
}
