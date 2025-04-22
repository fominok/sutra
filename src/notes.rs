//! Structured notes.

mod cache;
mod note;

pub(crate) use cache::NotesCache;
pub(crate) use note::{
    Heading, Interval, IntervalUnit, Note, Status, Tag, Todo, TodoBody, TodoBodyBuilder,
};

pub(crate) const TODO_DEFAULT_HEADING: &str = "Todo";
