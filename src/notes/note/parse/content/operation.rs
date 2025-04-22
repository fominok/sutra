use comrak::nodes::{AstNode, NodeValue};
use nom::{
    IResult, Parser, bytes::complete::tag, character::complete::multispace1, combinator::map,
    multi::separated_list0,
};

use crate::notes::note::{Statement, StatementBody, parse::common::parse_symbol};

pub(super) fn parse_statement<'a>(node: &'a AstNode<'a>) -> Option<Statement<'a>> {
    if matches!(node.data.borrow().value, NodeValue::Paragraph) {
        if let Some(text_node) = node.first_child() {
            let (_, (name, args)) = parse_paragraph(text_node.data.borrow().value.text()?).ok()?;

            return Some(Statement {
                node,
                body: StatementBody { name, args },
            });
        }
    }

    None
}

fn parse_paragraph(input: &str) -> IResult<&str, (String, Vec<String>)> {
    map(
        (
            tag("@"),
            parse_symbol,
            multispace1,
            separated_list0(multispace1, parse_symbol),
        ),
        |(_, name, _, args): (_, &str, _, _)| {
            (
                name.to_owned(),
                args.into_iter().map(|s| s.to_owned()).collect(),
            )
        },
    )
    .parse(input)
}
