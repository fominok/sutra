use std::iter::Peekable;

use comrak::{
    Arena,
    nodes::{AstNode, NodeHeading, NodeValue},
};

use super::{content::parse_content, tags::parse_tags};
use crate::{
    notes::{Todo, note::Heading},
    util::PeekExt,
};

fn heading_pred<'a, 'b>(node: &'b &'a AstNode<'a>) -> bool {
    matches!(node.data.borrow().value, NodeValue::Heading(_))
}

pub(super) fn parse_heading<'a, I: Iterator<Item = &'a AstNode<'a>>>(
    todos_arena: &'a Arena<Todo<'a>>,
    nodes: &mut Peekable<I>,
) -> Option<Heading<'a>> {
    if let Some(node) = nodes.scroll_until(heading_pred) {
        if let NodeValue::Heading(NodeHeading { level, .. }) = node.data.borrow().value {
            let title = node
                .first_child()
                .and_then(|n| match &n.data.borrow().value {
                    NodeValue::Text(x) => Some(x.clone()),
                    _ => None,
                })
                .unwrap_or_else(|| String::new());

            let mut tags = None;
            let mut content = Vec::new();

            for node in nodes.take_until(heading_pred) {
                if let Some(parsed_tags) = parse_tags(node) {
                    tags = Some(parsed_tags);
                } else if let Some(parsed_content) = parse_content(todos_arena, node) {
                    content.push(parsed_content);
                }
            }

            let mut sub_headings = Vec::new();

            while nodes
                .peek()
                .map(|n| match n.data.borrow().value {
                    NodeValue::Heading(NodeHeading {
                        level: sub_level, ..
                    }) => sub_level > level,
                    _ => false,
                })
                .unwrap_or_default()
            {
                if let Some(sub_heading) = parse_heading(todos_arena, nodes) {
                    sub_headings.push(sub_heading);
                }
            }

            return Some(Heading {
                node,
                title,
                tags,
                content,
                sub_headings,
            });
        }
    }

    None
}
