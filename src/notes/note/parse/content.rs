mod operation;
mod todo;

use comrak::{
    Arena,
    nodes::{AstNode, NodeList, NodeValue},
};
use operation::parse_statement;
use todo::parse_todo;

use crate::notes::note::{Content, ContentBody, Paragraph, Todo};

pub(super) fn parse_content<'a>(
    todos_arena: &'a Arena<Todo<'a>>,
    node: &'a AstNode<'a>,
) -> Option<Content<'a>> {
    match node.data.borrow().value {
        NodeValue::Paragraph => Some(Content {
            body: parse_statement(node)
                .map(ContentBody::Statement)
                .unwrap_or(ContentBody::Paragraph(Paragraph { node })),
        }),
        NodeValue::List(NodeList {
            is_task_list: true, ..
        }) => {
            let todos = node
                .children()
                .filter_map(parse_todo)
                .map(|todo| &*todos_arena.alloc(todo))
                .collect::<Vec<_>>();
            (!todos.is_empty()).then_some(Content {
                body: ContentBody::Todos(todos),
            })
        }
        _ => None,
    }
}
