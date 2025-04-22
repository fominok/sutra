mod parse;

use std::{
    cell::{Cell, Ref, RefCell, RefMut},
    collections::{HashSet, VecDeque},
    fmt::{self, Display, Write},
};

use chrono::NaiveDate;
use comrak::{
    Arena,
    nodes::{AstNode, NodeValue},
};
use derive_builder::Builder;
use itertools::Itertools;
use parse::parse_note;

use super::TODO_DEFAULT_HEADING;
use crate::md;

/// Structured note on top of Markdown AST with Sutra's DSL.
pub(crate) struct Note<'a> {
    root: &'a AstNode<'a>,
    md_arena: &'a Arena<AstNode<'a>>,
    headings: Vec<Heading<'a>>,
}

impl fmt::Debug for Note<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Note")
            .field("headings", &self.headings)
            .finish()
    }
}

impl<'a> Note<'a> {
    pub(super) fn parse(
        md_arena: &'a Arena<AstNode<'a>>,
        todos_arena: &'a Arena<Todo<'a>>,
        root_node: &'a AstNode<'a>,
    ) -> Option<Self> {
        parse_note(md_arena, todos_arena, root_node)
    }

    pub(super) fn as_bytes(&self) -> Vec<u8> {
        let mut options = comrak::Options::default();
        options.extension.tasklist = true;

        let mut out = Vec::new();

        comrak::format_commonmark(&self.root, &options, &mut out)
            .expect("cannot format back to markdown, this is a bug");

        out
    }

    pub(super) fn new(md_arena: &'a Arena<AstNode<'a>>) -> Self {
        Self {
            root: comrak::parse_document(md_arena, "", &Default::default()),
            headings: Default::default(),
            md_arena,
        }
    }

    pub(crate) fn content_traverse<'h>(&'h self) -> ContentTraverse<'a, 'h> {
        ContentTraverse::new(&self.headings)
    }

    pub(crate) fn get_or_create_heading<'h>(&'h mut self, title: &str) -> HeadingSpan<'a, 'h> {
        let idx_opt = self
            .headings
            .iter_mut()
            .enumerate()
            .find(|(_, h)| h.title == title)
            .map(|(idx, _)| idx);

        if let Some(idx) = idx_opt {
            let (left, right) = self.headings.split_at_mut(idx + 1);
            let start: &'h mut Heading<'a> = &mut left[idx];
            let end: Option<&'h mut Heading<'a>> = right.first_mut();
            return HeadingSpan {
                md_arena: self.md_arena,
                start,
                end,
            };
        } else {
            let heading_node = md::parse_str(self.md_arena, &format!("# {}", TODO_DEFAULT_HEADING))
                .first_child()
                .expect("the heading is valid");

            self.root.append(heading_node);

            self.headings.push(Heading {
                node: heading_node,
                title: TODO_DEFAULT_HEADING.to_owned(),
                tags: Default::default(),
                content: Default::default(),
                sub_headings: Default::default(),
            });
            HeadingSpan {
                md_arena: self.md_arena,
                start: self.headings.last_mut().expect("just inserted"),
                end: None,
            }
        }
    }
}

/// Wrapper around one or optionally two headings to have boundaries since
/// contents under a heading is made of siblings instead of children in Comrak
/// AST.
pub(crate) struct HeadingSpan<'a, 'h> {
    md_arena: &'a Arena<AstNode<'a>>,
    start: &'h mut Heading<'a>,
    end: Option<&'h mut Heading<'a>>,
}

impl<'a, 'h> HeadingSpan<'a, 'h> {
    pub(crate) fn add_todo(
        &mut self,
        todos_arena: &'a Arena<Todo<'a>>,
        body: TodoBody,
    ) -> &'a Todo<'a> {
        let todo_list_ast = body.to_ast_list(self.md_arena);

        let todo: &'a Todo<'a> = todos_arena.alloc(Todo {
            node: todo_list_ast
                .first_child()
                .expect("a todo item must exist")
                .into(),
            removed: false.into(),
            body: body.into(),
        });

        self.start.content.push(Content {
            body: ContentBody::Todos(vec![todo]),
        });

        // We try to put a fresh list made of a new todo in the contents section of the
        // heading. In spreads from the heading node to a next heading, either
        // same level or subheading.
        if let Some(subheading_node) = self.start.sub_headings.first().map(|sh| sh.node) {
            subheading_node.insert_before(todo_list_ast);
        } else {
            if let Some(end) = &mut self.end {
                end.node.insert_before(todo_list_ast);
            } else {
                // There are no headings after (or within) the one of interest
                self.start
                    .node
                    .parent()
                    .expect("there must be a parent over heading, the document")
                    .append(todo_list_ast);
            }
        }

        todo
    }
}

pub(crate) struct Tags<'a> {
    node: &'a AstNode<'a>,
    tags: HashSet<Tag>,
}

impl fmt::Debug for Tags<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Tags").field("tags", &self.tags).finish()
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub(crate) struct Tag {
    nested_tags: Vec<String>,
}

impl Tag {
    pub(crate) fn join(mut self, other: Tag) -> Self {
        self.nested_tags.extend(other.nested_tags.into_iter());
        self
    }

    pub(crate) fn new(nested_tags: Vec<String>) -> Self {
        Self { nested_tags }
    }
}

pub(crate) struct Heading<'a> {
    node: &'a AstNode<'a>,
    title: String,
    tags: Option<Tags<'a>>,
    content: Vec<Content<'a>>,
    sub_headings: Vec<Heading<'a>>,
}

#[derive(Debug, Default, Clone)]
pub(crate) struct TagsStack<'a, 'h> {
    tags_by_level: Vec<&'h Option<Tags<'a>>>,
}

impl<'a, 'h> TagsStack<'a, 'h> {
    fn push(&mut self, tags: &'h Option<Tags<'a>>) {
        self.tags_by_level.push(tags);
    }

    fn pop(&mut self) {
        self.tags_by_level.pop();
    }

    pub(crate) fn build_tags(&self) -> Option<HashSet<Tag>> {
        match self.tags_by_level.len() {
            0 => None,
            1 => self.tags_by_level[0].as_ref().map(|t| t.tags.clone()),
            _ => {
                let Some(mut acc) = self.tags_by_level[0].as_ref().map(|t| t.tags.clone()) else {
                    return Default::default();
                };

                for level in &self.tags_by_level[1..] {
                    if level.is_none() {
                        continue;
                    }
                    let mut new_acc = HashSet::new();
                    for tag_a in acc.into_iter() {
                        for tag_b in level.into_iter().map(|l| l.tags.iter()).flatten().cloned() {
                            if tag_a != tag_b {
                                new_acc.insert(tag_a.clone().join(tag_b));
                            }
                        }
                    }
                    acc = new_acc;
                }

                Some(acc)
            }
        }
    }
}

pub(crate) struct ContentTraverse<'a, 'h> {
    parent_path: Vec<&'h str>,
    queue: VecDeque<&'h Heading<'a>>,
    tags_stack: TagsStack<'a, 'h>,
    left_on_height: Vec<usize>,
}

type HeadingPath<'h, 'i> = &'i [&'h str];
type Title<'h> = &'h str;

impl<'a, 'h> ContentTraverse<'a, 'h> {
    pub(crate) fn new(headings: impl IntoIterator<Item = &'h Heading<'a>>) -> Self {
        let queue: VecDeque<_> = headings.into_iter().collect();
        Self {
            parent_path: Vec::new(),
            tags_stack: Default::default(),
            left_on_height: vec![queue.len()],
            queue,
        }
    }

    pub(crate) fn next<'i>(
        &'i mut self,
    ) -> Option<(
        HeadingPath<'h, 'i>,
        Title<'h>,
        TagsStack<'a, 'h>,
        &'h [Content<'a>],
    )> {
        let heading = self.queue.pop_front()?;
        let mut current_height_left = self.left_on_height.last_mut()?;

        while *current_height_left == 0 {
            self.left_on_height.pop();
            self.parent_path.pop();
            self.tags_stack.pop();
            current_height_left = self.left_on_height.last_mut()?;
        }

        *current_height_left -= 1;

        let parent_path_length = self.left_on_height.len() - 1;

        let tags_stack = if !heading.sub_headings.is_empty() {
            self.parent_path.push(&heading.title);
            self.left_on_height.push(heading.sub_headings.len());

            heading
                .sub_headings
                .iter()
                .rev()
                .for_each(|h| self.queue.push_front(h));
            self.tags_stack.push(&heading.tags);
            self.tags_stack.clone()
        } else {
            let mut stack = self.tags_stack.clone();
            stack.push(&heading.tags);
            stack
        };

        Some((
            &self.parent_path[..parent_path_length],
            &heading.title,
            tags_stack,
            &heading.content,
        ))
    }
}

impl fmt::Debug for Heading<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Heading")
            .field("title", &self.title)
            .field("tags", &self.tags)
            .field("content", &self.content)
            .field("sub_headings", &self.sub_headings)
            .finish()
    }
}

#[derive(Debug)]
pub(crate) struct Content<'a> {
    body: ContentBody<'a>,
}

impl<'a, 'c> TryInto<&'c [&'a Todo<'a>]> for &'c Content<'a> {
    type Error = ();

    fn try_into(self) -> Result<&'c [&'a Todo<'a>], Self::Error> {
        let body: &'c ContentBody = &self.body;

        if let ContentBody::Todos(todos) = body {
            Ok(todos)
        } else {
            Err(())
        }
    }
}

#[derive(Debug)]
enum ContentBody<'a> {
    Paragraph(Paragraph<'a>),
    Todos(Vec<&'a Todo<'a>>),
    Statement(Statement<'a>),
}

pub(crate) struct Statement<'a> {
    node: &'a AstNode<'a>,
    body: StatementBody,
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) struct StatementBody {
    name: String,
    args: Vec<String>,
}

impl Display for StatementBody {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.name)?;
        for arg in self.args.iter() {
            f.write_char(' ')?;
            f.write_str(arg)?;
        }

        Ok(())
    }
}

impl fmt::Debug for Statement<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Statement")
            .field("name", &self.body.name)
            .field("args", &self.body.args)
            .finish()
    }
}

pub(crate) struct Paragraph<'a> {
    node: &'a AstNode<'a>,
}

impl Paragraph<'_> {
    pub(crate) fn text(&self) -> Option<Ref<str>> {
        self.node.first_child().map(|n| {
            Ref::map(n.data.borrow(), |d| match &d.value {
                NodeValue::Text(text) => text.as_str(),
                _ => "",
            })
        })
    }
}

impl fmt::Debug for Paragraph<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Paragraph")
            .field("text", &self.text().as_deref())
            .finish()
    }
}

pub(crate) struct Todo<'a> {
    node: Cell<&'a AstNode<'a>>,
    removed: Cell<bool>,
    body: RefCell<TodoBody>,
}

#[derive(Builder)]
#[builder(setter(strip_option))]
pub(crate) struct TodoBody {
    pub(crate) title: String,
    #[builder(default)]
    pub(crate) status: Status,
    #[builder(default)]
    pub(crate) delay: Option<Interval>,
    #[builder(default)]
    pub(crate) advance: Option<Interval>,
    #[builder(default)]
    pub(crate) every: Option<Interval>,
    #[builder(default)]
    pub(crate) since: Option<NaiveDate>,
    #[builder(default)]
    pub(crate) adhoc: bool,
    #[builder(setter(skip))]
    pub(crate) statements: Option<Vec<StatementBody>>,
}

impl TodoBody {
    pub(crate) fn to_ast_list<'a>(&self, arena: &'a Arena<AstNode<'a>>) -> &'a AstNode<'a> {
        let TodoBody {
            title,
            delay,
            advance,
            every,
            since,
            adhoc,
            statements,
            status,
            ..
        }: &TodoBody = &self;

        let mut params = every
            .into_iter()
            .map(|x| x.to_string())
            .chain(
                delay
                    .into_iter()
                    .map(|x| format!("delay {}", x.to_string())),
            )
            .chain(
                advance
                    .into_iter()
                    .map(|x| format!("advance {}", x.to_string())),
            )
            .chain(
                since
                    .into_iter()
                    .map(|x| format!("since {}", x.to_string())),
            )
            .chain(adhoc.then(|| "adhoc".to_owned()))
            .chain(statements.iter().flatten().map(|x| x.to_string()));

        let params_body_str = params.join(", ");

        let params_str = if params_body_str.is_empty() {
            "".to_owned()
        } else {
            format!(" @{params_body_str}")
        };

        md::parse_str(
            arena,
            &format!("- [{}] {title}{params_str}", status.as_char()),
        )
        .first_child()
        .expect("a list was provided...")
    }

    pub(crate) fn to_ast_item<'a>(&self, arena: &'a Arena<AstNode<'a>>) -> &'a AstNode<'a> {
        self.to_ast_list(arena)
            .first_child()
            .expect("a povided list has a task item")
    }
}

impl<'a> Todo<'a> {
    pub(crate) fn body(&self) -> Ref<TodoBody> {
        self.body.borrow()
    }

    pub(crate) fn body_mut(&self) -> RefMut<TodoBody> {
        self.body.borrow_mut()
    }

    pub(crate) fn title(&self) -> Ref<str> {
        Ref::map(self.body.borrow(), |body| body.title.as_str())
    }

    pub(crate) fn remove(&self) {
        self.removed.set(true);
    }

    pub(crate) fn is_removed(&self) -> bool {
        self.removed.get()
    }

    pub(crate) fn update_ast(&self, arena: &'a Arena<AstNode<'a>>) {
        let node = self.node.get();

        if self.removed.get() {
            node.detach();
        } else {
            let new_todo_node = self.body().to_ast_item(arena);

            if let Some(sibling) = node.previous_sibling() {
                sibling.insert_after(new_todo_node);
            } else {
                node.parent()
                    .expect("todo must have a parent")
                    .prepend(new_todo_node);
            }

            let desc = node.descendants().collect::<Vec<_>>();
            desc.into_iter().for_each(|d| d.detach());
            node.detach();

            self.node.set(new_todo_node);
        }
    }

    pub(crate) fn remove_params(&self) {
        self.set_delay(None);
        self.set_advance(None);
        self.set_every(None);
        self.set_since(None);
        self.set_adhoc(false);
        self.set_statements(None);
    }

    pub(crate) fn set_delay(&self, delay: Option<Interval>) {
        self.body.borrow_mut().delay = delay;
    }

    pub(crate) fn set_advance(&self, advance: Option<Interval>) {
        self.body.borrow_mut().advance = advance;
    }

    pub(crate) fn set_every(&self, every: Option<Interval>) {
        self.body.borrow_mut().every = every;
    }

    pub(crate) fn set_since(&self, since: Option<NaiveDate>) {
        self.body.borrow_mut().since = since;
    }

    pub(crate) fn set_adhoc(&self, adhoc: bool) {
        self.body.borrow_mut().adhoc = adhoc;
    }

    pub(crate) fn set_statements(&self, statements: Option<Vec<StatementBody>>) {
        self.body.borrow_mut().statements = statements;
    }

    pub(crate) fn get_adhoc(&self) -> bool {
        self.body.borrow().adhoc
    }
}

#[derive(Debug, Default, PartialEq, Eq, Clone, Copy)]
pub(crate) enum Status {
    #[default]
    Open,
    Done,
    Skipped,
    Completed,
}

impl Status {
    pub(crate) fn as_char(&self) -> char {
        match self {
            Status::Open => ' ',
            Status::Done => 'x',
            Status::Skipped => 's',
            Status::Completed => 'c',
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub(crate) struct Interval {
    pub value: u32,
    pub unit: IntervalUnit,
}

impl Display for Interval {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.value == 1 {
            self.unit.fmt(f)?;
        } else {
            f.write_fmt(format_args!("{} {}s", self.value, self.unit))?;
        }

        Ok(())
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub(crate) enum IntervalUnit {
    Day,
    Week,
    Month,
    Year,
}

impl Display for IntervalUnit {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            IntervalUnit::Day => "day",
            IntervalUnit::Week => "week",
            IntervalUnit::Month => "month",
            IntervalUnit::Year => "year",
        })
    }
}

impl fmt::Debug for Todo<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let body = self.body.borrow();
        f.debug_struct("Todo")
            .field("title", &body.title)
            .field("status", &body.status)
            .field("delay", &body.delay)
            .field("every", &body.every)
            .field("since", &body.since)
            .field("adhoc", &body.adhoc)
            .field("statements", &body.statements)
            .finish()
    }
}

#[cfg(test)]
mod tests {
    use parse::util_parse_tag;

    use super::*;
    use crate::md;

    fn get_test_note<'a>(
        md_arena: &'a Arena<AstNode<'a>>,
        todos_arena: &'a Arena<Todo<'a>>,
    ) -> Note<'a> {
        Note::parse(
            md_arena,
            todos_arena,
            md::parse_str(
                md_arena,
                "
# One

#toptag #toptag2

## Two

#belowtag #very/nested

### Three

#another

@some op

## TwoTwo

nice

# OneOne

#toptag3

### Three

#again/nested

- [ ] do something @another op, every week

#### Four

#bottom

this is a content

## Two",
            ),
        )
        .unwrap()
    }

    #[test]
    fn content_traverse() {
        fn assert_tags<'a>(left: &Option<HashSet<Tag>>, right: impl IntoIterator<Item = &'a str>) {
            let right_set: HashSet<Tag> = right.into_iter().map(util_parse_tag).collect();

            assert_eq!(left.as_ref(), Some(&right_set));
        }

        let md_arena = Arena::new();
        let todos_arena = Arena::new();

        let note = get_test_note(&md_arena, &todos_arena);

        let mut ct = ContentTraverse::new(&note.headings);

        let mut result = Vec::new();

        while let Some(h) = ct.next() {
            let mut path = h.0.to_vec();
            path.push(h.1);
            result.push((path, h.2.build_tags(), h.3));
        }

        assert_eq!(result[0].0.as_slice(), ["One"]);
        assert_tags(&result[0].1, ["#toptag", "#toptag2"]);
        assert!(matches!(result[0].2, []));

        assert_eq!(result[1].0.as_slice(), ["One", "Two"]);
        assert_tags(
            &result[1].1,
            [
                "#toptag/belowtag",
                "#toptag2/belowtag",
                "#toptag/very/nested",
                "#toptag2/very/nested",
            ],
        );
        assert!(matches!(result[1].2, []));

        assert_eq!(result[2].0.as_slice(), ["One", "Two", "Three"]);
        assert_tags(
            &result[2].1,
            [
                "#toptag/belowtag/another",
                "#toptag2/belowtag/another",
                "#toptag/very/nested/another",
                "#toptag2/very/nested/another",
            ],
        );
        if let [
            Content {
                body:
                    ContentBody::Statement(Statement {
                        body: StatementBody { name, args },
                        ..
                    }),
            },
        ] = &mut result[2].2
        {
            assert_eq!(name, "some");
            assert_eq!(args.as_slice(), ["op"])
        } else {
            panic!("expected statement");
        };

        assert_eq!(result[3].0.as_slice(), ["One", "TwoTwo"]);
        assert_tags(&result[3].1, ["#toptag", "#toptag2"]);
        if let [
            Content {
                body: ContentBody::Paragraph(paragraph),
            },
        ] = &mut result[3].2
        {
            assert_eq!(&*paragraph.text().unwrap(), "nice");
        } else {
            panic!("expected paragraph");
        };

        assert_eq!(result[4].0.as_slice(), ["OneOne"]);
        assert_tags(&result[4].1, ["#toptag3"]);
        assert!(matches!(result[4].2, []));

        assert_eq!(result[5].0.as_slice(), ["OneOne", "Three"]);
        assert_tags(&result[5].1, ["#toptag3/again/nested"]);
        if let [
            Content {
                body: ContentBody::Todos(todos),
            },
        ] = &mut result[5].2
        {
            assert_eq!(todos.len(), 1);
            let todo = &todos[0];
            let todo_body = todo.body.borrow();
            assert_eq!(todo_body.title, "do something");
            assert_eq!(todo_body.status, Status::Open);
            assert_eq!(
                todo_body.every,
                Some(Interval {
                    value: 1,
                    unit: IntervalUnit::Week
                })
                .into()
            );
            assert_eq!(
                todo_body.statements,
                Some(vec![StatementBody {
                    name: "another".to_owned(),
                    args: vec!["op".to_owned()]
                }])
                .into()
            )
        } else {
            panic!("expected todo");
        };

        assert_eq!(result[6].0.as_slice(), ["OneOne", "Three", "Four"]);
        assert_tags(&result[6].1, ["#toptag3/again/nested/bottom"]);
        if let [
            Content {
                body: ContentBody::Paragraph(paragraph),
            },
        ] = &mut result[6].2
        {
            assert_eq!(&*paragraph.text().unwrap(), "this is a content");
        } else {
            panic!("expected paragraph");
        };

        assert_eq!(result[7].0.as_slice(), ["OneOne", "Two"]);
        assert_tags(&result[7].1, ["#toptag3"]);
        assert!(matches!(result[7].2, []));
    }

    #[test]
    fn tag_stack() {
        let md_arena = Arena::new();
        let todos_arena = Arena::new();

        let note = get_test_note(&md_arena, &todos_arena);

        let mut stack = TagsStack {
            tags_by_level: Vec::new(),
        };

        let mut current_heading = &note.headings[0];
        loop {
            stack.tags_by_level.push(&current_heading.tags);
            if let Some(sub) = current_heading.sub_headings.first() {
                current_heading = sub;
            } else {
                break;
            }
        }

        assert_eq!(
            stack.build_tags(),
            [
                Tag {
                    nested_tags: vec![
                        "toptag".to_owned(),
                        "belowtag".to_owned(),
                        "another".to_owned()
                    ]
                },
                Tag {
                    nested_tags: vec![
                        "toptag".to_owned(),
                        "very".to_owned(),
                        "nested".to_owned(),
                        "another".to_owned()
                    ]
                },
                Tag {
                    nested_tags: vec![
                        "toptag2".to_owned(),
                        "belowtag".to_owned(),
                        "another".to_owned()
                    ]
                },
                Tag {
                    nested_tags: vec![
                        "toptag2".to_owned(),
                        "very".to_owned(),
                        "nested".to_owned(),
                        "another".to_owned()
                    ]
                },
            ]
            .into_iter()
            .collect::<HashSet<_>>()
            .into()
        );
    }

    #[test]
    fn update_todo_node() {
        let md_arena = Arena::new();
        let todos_arena = Arena::new();

        let note = Note::parse(
            &md_arena,
            &todos_arena,
            md::parse_str(
                &md_arena,
                "
# One

- [ ] do something @another op, every week, since 2025-05-05",
            ),
        )
        .unwrap();

        let mut ct = note.content_traverse();

        while let Some((_, _, _, contents)) = ct.next() {
            if let Some(todo) = contents
                .into_iter()
                .filter_map(|c| match &c.body {
                    ContentBody::Todos(t) => Some(t.into_iter()),
                    _ => None,
                })
                .flatten()
                .next()
            {
                todo.remove_params();
                todo.update_ast(&md_arena);

                break;
            }
        }

        assert_eq!(
            note.as_bytes(),
            "# One

- [ ] do something
"
            .as_bytes()
        );
    }
}
