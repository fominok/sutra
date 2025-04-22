use comrak::{Arena, nodes::AstNode};

pub(crate) fn parse_str<'a>(arena: &'a Arena<AstNode<'a>>, buffer: &str) -> &'a AstNode<'a> {
    let mut options = comrak::Options::default();
    options.extension.tasklist = true;

    comrak::parse_document(arena, buffer, &options)
}
