use comrak::nodes::{AstNode, NodeValue};
use nom::{
    IResult, Parser,
    bytes::complete::tag,
    character::complete::{alphanumeric1, multispace0, multispace1},
    combinator::{map, opt, recognize},
    multi::{separated_list0, separated_list1},
    sequence::pair,
};

use crate::notes::note::{Tag, Tags};

pub(super) fn parse_tags<'a>(node: &'a AstNode<'a>) -> Option<Tags<'a>> {
    if let NodeValue::Paragraph = node.data.borrow().value {
        let text_data = node.first_child().map(|c| &c.data)?;

        if let NodeValue::Text(input) = &text_data.borrow().value {
            return map(
                (
                    opt(tag("tags:")),
                    multispace0,
                    separated_list0(
                        multispace1.or(recognize(pair(tag(","), multispace0))),
                        parse_tag,
                    ),
                ),
                |(_, _, tags)| tags,
            )
            .parse(input)
            .ok()
            .and_then(|(_, tags)| {
                (!tags.is_empty()).then_some(Tags {
                    node,
                    tags: tags.into_iter().collect(),
                })
            });
        }
    }

    None
}

fn parse_tag(input: &str) -> IResult<&str, Tag> {
    map(
        (
            tag("#"),
            map(
                separated_list1(tag("/"), alphanumeric1),
                |nested_tags: Vec<&str>| Tag {
                    nested_tags: nested_tags.into_iter().map(str::to_string).collect(),
                },
            ),
        ),
        |(_, tag)| tag,
    )
    .parse(input)
}

#[cfg(test)]
pub(crate) fn util_parse_tag(input: &str) -> Tag {
    parse_tag(input).expect("bad test data").1
}
