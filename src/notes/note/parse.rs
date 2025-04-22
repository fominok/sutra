use comrak::{Arena, nodes::AstNode};
use heading::parse_heading;

use super::{Note, Todo};

mod common;
mod content;
mod heading;
mod tags;

#[cfg(test)]
pub(super) use tags::util_parse_tag;

pub(super) fn parse_note<'a>(
    md_arena: &'a Arena<AstNode<'a>>,
    todos_arena: &'a Arena<Todo<'a>>,
    root: &'a AstNode<'a>,
) -> Option<Note<'a>> {
    let mut headings = Vec::new();
    let mut nodes = root.children().peekable();

    while let Some(heading) = parse_heading(todos_arena, &mut nodes) {
        headings.push(heading);
    }

    (!headings.is_empty()).then_some(Note {
        root,
        headings,
        md_arena,
    })
}
