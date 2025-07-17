use std::{
    cell::Ref,
    collections::{BTreeMap, HashMap, HashSet, VecDeque},
    iter,
};

use anyhow::{Result, bail};
use chrono::{Duration, Months, NaiveDate};
use comrak::{Arena, nodes::AstNode};

use super::NotesCache;
use crate::{
    TODAY,
    notes::{
        Interval, IntervalUnit, Note, Status, TODO_DEFAULT_HEADING, Tag, Todo, TodoBody,
        TodoBodyBuilder,
    },
    ui::prompt::{self, PromptSelect},
    vault::{DatedFileIdentifier, NamedFileIdentifier, NoteIdentifier},
};

const LOOK_AHEAD_TIMES: usize = 1;

const LOOK_AHEAD_INTERVAL: Interval = Interval {
    value: 2,
    unit: IntervalUnit::Month,
};

#[cfg_attr(test, faux::create)]
#[derive(Default)]
pub(super) struct EdgeCasesPrompt {}

fn path_str(path: &[String]) -> String {
    path.join("/")
}

#[cfg_attr(test, faux::methods)]
impl EdgeCasesPrompt {
    pub fn named_todo_has_params(
        &self,
        id: &NamedFileIdentifier,
        path: &[String],
        title: &str,
    ) -> EdgeCaseResolution<RemoveParamsNamedResolution> {
        prompt::run(&format!(
            "Todo item at {id}/{}/{title} has params, but only daily notes allow it",
            path_str(path)
        ))
        .unwrap_or(EdgeCaseResolution::Exit)
    }

    pub fn dated_todo_no_recurrence_has_params(
        &self,
        todo_date: &NaiveDate,
        path: &[String],
        title: &str,
    ) -> EdgeCaseResolution<RemoveParamsDatedResolution> {
        prompt::run(&format!(
            "Todo item at {todo_date}/{}/{title} has params, but there is no ongoing recurrence \
             for it",
            path_str(path)
        ))
        .unwrap_or(EdgeCaseResolution::Exit)
    }

    pub fn dated_todo_with_recurrence_misaligned_since(
        &self,
        todo_date: &NaiveDate,
        path: &[String],
        title: &str,
    ) -> EdgeCaseResolution<UpdateSinceDatedResolution> {
        prompt::run(&format!(
            "Todo item at {todo_date}/{}/{title} is a part of a recurrence other than the one in \
             `since` param",
            path_str(path)
        ))
        .unwrap_or(EdgeCaseResolution::Exit)
    }

    pub fn dated_todo_with_recurrence_conflicting_params(
        &self,
        todo_date: &NaiveDate,
        path: &[String],
        title: &str,
    ) -> EdgeCaseResolution<KeepOneParamDatedResolution> {
        prompt::run(&format!(
            "Todo item at {todo_date}/{}/{title} has an ambiguous mix of params",
            path_str(path)
        ))
        .unwrap_or(EdgeCaseResolution::Exit)
    }

    pub fn dated_todo_goes_early(
        &self,
        todo_date: &NaiveDate,
        title: &str,
    ) -> EdgeCaseResolution<AdjustRecurrentTodo> {
        prompt::run(&format!(
            "Todo item at {todo_date}/{title} goes ahead of the grid",
        ))
        .unwrap_or(EdgeCaseResolution::Exit)
    }

    pub fn dated_todo_with_recurrence_misaligned_advance(
        &self,
        todo_date: &NaiveDate,
        title: &str,
    ) -> EdgeCaseResolution<AdjustRecurrentTodo> {
        prompt::run(&format!(
            "Todo item at {todo_date}/{title} goes ahead of the grid with a wrong `advance` param",
        ))
        .unwrap_or(EdgeCaseResolution::Exit)
    }
}

#[derive(Debug, Clone)]
pub(super) enum EdgeCaseResolution<E> {
    Delete,
    Extra(E),
    Exit,
}

impl<E: PromptSelect> PromptSelect for EdgeCaseResolution<E> {
    fn variants() -> impl Iterator<Item = (char, &'static str, Self)> {
        iter::once(('d', "Delete", EdgeCaseResolution::Delete))
            .chain(E::variants().map(|(c, s, v)| (c, s, EdgeCaseResolution::Extra(v))))
            .chain(iter::once(('e', "Exit", EdgeCaseResolution::Exit)))
    }
}

#[derive(Debug, Clone)]
pub(super) struct RemoveParamsNamedResolution;

impl PromptSelect for RemoveParamsNamedResolution {
    fn variants() -> impl Iterator<Item = (char, &'static str, Self)> {
        iter::once(('r', "Remove params", RemoveParamsNamedResolution))
    }
}

#[derive(Debug, Clone)]
pub(super) struct RemoveParamsDatedResolution;

impl PromptSelect for RemoveParamsDatedResolution {
    fn variants() -> impl Iterator<Item = (char, &'static str, Self)> {
        iter::once(('r', "Remove params", RemoveParamsDatedResolution))
    }
}

#[derive(Debug, Clone)]
pub(super) struct UpdateSinceDatedResolution;

impl PromptSelect for UpdateSinceDatedResolution {
    fn variants() -> impl Iterator<Item = (char, &'static str, Self)> {
        iter::once(('u', "Update `since`", UpdateSinceDatedResolution))
    }
}

#[derive(Debug, Clone)]
pub(super) enum KeepOneParamDatedResolution {
    AdHoc,
    Advance,
    Delay,
}

impl PromptSelect for KeepOneParamDatedResolution {
    fn variants() -> impl Iterator<Item = (char, &'static str, Self)> {
        iter::once((
            'h',
            "Keep `adhoc` param",
            KeepOneParamDatedResolution::AdHoc,
        ))
        .chain(iter::once((
            'a',
            "Keep `advance` param",
            KeepOneParamDatedResolution::Advance,
        )))
        .chain(iter::once((
            'e',
            "Keep `delay` param",
            KeepOneParamDatedResolution::Delay,
        )))
    }
}

#[derive(Debug, Clone)]
pub(super) enum AdjustRecurrentTodo {
    AdHoc,
    Advance,
    NudgeFollowing,
}

impl PromptSelect for AdjustRecurrentTodo {
    fn variants() -> impl Iterator<Item = (char, &'static str, Self)> {
        iter::once((
            'h',
            "Set `adhoc` param to keep the grid as is",
            AdjustRecurrentTodo::AdHoc,
        ))
        .chain(iter::once((
            'a',
            "Set `advance` param to adjust the grid",
            AdjustRecurrentTodo::Advance,
        )))
        .chain(iter::once((
            'n',
            "Nudge this and following events forward to stay on grid",
            AdjustRecurrentTodo::NudgeFollowing,
        )))
    }
}

fn get_todo_date(date: NaiveDate, todo: &Todo) -> NaiveDate {
    let body = todo.body();
    if let Some(delay) = body.delay {
        add_interval(date, &delay)
    } else if let Some(advance) = body.advance {
        sub_interval(date, &advance)
    } else {
        date
    }
}

#[derive(Debug, Default)]
pub(super) struct EventsView<'a> {
    named_todos: BTreeMap<NamedFileIdentifier, Vec<BatchTodosView<'a>>>,
    dated_todos: BTreeMap<NaiveDate, Vec<&'a Todo<'a>>>,
    dated_declaration_contexts: BTreeMap<TodoId, TodoDeclarationContext>,
    recurrent_stats: HashMap<String, BTreeMap<NaiveDate, RecurrentStats<'a>>>,
}

#[derive(Debug)]
struct TodoDeclarationContext {
    path: Vec<String>,
    tags: Option<HashSet<Tag>>,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
struct TodoId(NaiveDate, String);

pub(crate) struct TodoWithContext<'a, 'v> {
    pub(crate) note_id: NoteIdentifier,
    pub(crate) path: &'v [String],
    pub(crate) tags: Option<&'v HashSet<Tag>>,
    pub(crate) todo: &'v Todo<'a>,
}

impl<'a, 'v> TodoWithContext<'a, 'v> {
    pub(crate) fn tags_lock(&self) -> TagsLock<'v> {
        TagsLock {
            tags: self.tags,
            todo: self.todo.body(),
        }
    }
}

pub(crate) struct TagsLock<'a> {
    tags: Option<&'a HashSet<Tag>>,
    todo: Ref<'a, TodoBody>,
}

impl<'a> TagsLock<'a> {
    pub(crate) fn iter<'s>(&'s self) -> Box<dyn Iterator<Item = &'s Tag> + 's> {
        match (&self.todo.tags, self.tags) {
            (None, None) => Box::new(iter::empty()),
            (None, Some(h)) => Box::new(h.iter()),
            (Some(h), None) => Box::new(h.iter()),
            (Some(h1), Some(h2)) => Box::new(h1.union(h2)),
        }
    }
}

impl<'a> EventsView<'a> {
    pub(super) fn iter_named_todos<'v>(&'v self) -> impl Iterator<Item = TodoWithContext<'a, 'v>> {
        self.named_todos.iter().flat_map(|(id, views)| {
            views.iter().flat_map(|view| {
                view.iter().map(|(path, tags, todo)| TodoWithContext {
                    path,
                    tags,
                    todo,
                    note_id: id.clone().into(),
                })
            })
        })
    }

    pub(super) fn iter_dated_todos<'v>(
        &'v self,
    ) -> impl DoubleEndedIterator<Item = TodoWithContext<'a, 'v>> {
        self.dated_todos.iter().flat_map(|(date, todos)| {
            todos.iter().map(|todo| {
                let date = date.clone();
                let ctx: Option<&TodoDeclarationContext> = self.dated_declaration_contexts.get(
                    &TodoId(todo.body().since.unwrap_or(date), todo.title().to_owned()),
                ); // TODO: Cow maybe
                TodoWithContext {
                    path: ctx.map(|c| c.path.as_slice()).unwrap_or_default(),
                    tags: ctx.and_then(|c| c.tags.as_ref()),
                    todo: *todo,
                    note_id: NoteIdentifier::new_dated(date),
                }
            })
        })
    }

    fn find_closest_rule(
        &mut self,
        date: NaiveDate,
        title: &str,
    ) -> Option<(&NaiveDate, &mut RecurrentStats<'a>)> {
        self.recurrent_stats
            .get_mut(title)?
            .range_mut(..=date)
            .last()
            .filter(|(_, stats)| !stats.completed)
    }

    fn add_dated_todo_with_ongoing_recurrence(
        prompt: &EdgeCasesPrompt,
        md_arena: &'a Arena<AstNode<'a>>,
        recurrence_date: &NaiveDate,
        recurrence_stats: &mut RecurrentStats<'a>,
        todo_date: NaiveDate,
        path: &[String],
        todo: &'a Todo<'a>,
    ) -> Result<()> {
        // Check that `since` parameter is aligned
        if todo
            .body()
            .since
            .map(|s| *recurrence_date != s)
            .unwrap_or(true)
        {
            let resolution =
                prompt.dated_todo_with_recurrence_misaligned_since(&todo_date, path, &todo.title());
            // `since` parameter points to a wrong recurrence starting point or is missing
            match resolution {
                EdgeCaseResolution::Delete => {
                    todo.remove();
                    todo.update_ast(md_arena);
                }
                EdgeCaseResolution::Extra(UpdateSinceDatedResolution) => {
                    todo.set_since(Some(recurrence_date.clone()));
                    todo.update_ast(md_arena);
                }
                EdgeCaseResolution::Exit => bail!("Aborting..."),
            }
        }

        let mut lock = todo.body_mut();
        let TodoBody {
            title,
            delay,
            advance,
            adhoc,
            status,
            ..
        }: &mut TodoBody = &mut lock;

        let status: Status = *status;

        // Check that `adhoc`, `delay` and `advance` presence is mutually exclusive
        if !matches!(
            (*adhoc, delay.as_ref(), advance.as_ref()),
            (true, None, None)
                | (false, Some(_), None)
                | (false, None, Some(_))
                | (false, None, None)
        ) {
            // ambiguous mix of params
            match prompt.dated_todo_with_recurrence_conflicting_params(&todo_date, path, title) {
                EdgeCaseResolution::Delete => {
                    todo.remove();
                }
                EdgeCaseResolution::Extra(KeepOneParamDatedResolution::AdHoc) => {
                    *advance = None;
                    *delay = None;
                }
                EdgeCaseResolution::Extra(KeepOneParamDatedResolution::Advance) => {
                    *adhoc = false;
                    *delay = None;
                }
                EdgeCaseResolution::Extra(KeepOneParamDatedResolution::Delay) => {
                    *adhoc = false;
                    *advance = None;
                }
                EdgeCaseResolution::Exit => bail!("Aborting..."),
            }

            drop(lock);
            todo.update_ast(md_arena);
        }

        if status == Status::Completed {
            recurrence_stats.completed = true;
        }

        recurrence_stats.queue.push_back((todo_date, todo));

        Ok(())
    }

    fn add_dated_todo_without_ongoing_recurrence(
        &mut self,
        cache: &mut NotesCache<'a>,
        prompt: &EdgeCasesPrompt,
        todo_date: NaiveDate,
        path: Vec<String>,
        heading_tags: Option<HashSet<Tag>>,
        todo: &'a Todo<'a>,
    ) -> Result<()> {
        let lock = todo.body();
        let TodoBody {
            delay,
            advance,
            since,
            adhoc,
            title,
            tags,
            ..
        }: &TodoBody = &lock;

        // All parameters except optionally setting a recurrence are redundant:
        if delay.is_some() || advance.is_some() || *adhoc || since.is_some() {
            match prompt.dated_todo_no_recurrence_has_params(&todo_date, &path, title) {
                EdgeCaseResolution::Delete => {
                    drop(lock);
                    todo.remove();
                    todo.update_ast(cache.get_md_arena());
                }
                EdgeCaseResolution::Extra(RemoveParamsDatedResolution) => {
                    drop(lock);
                    todo.remove_params();
                    todo.update_ast(cache.get_md_arena());
                }
                EdgeCaseResolution::Exit => bail!("Aborting..."),
            }
        }

        let TodoBody {
            every,
            title,
            status,
            tags,
            ..
        }: &TodoBody = &todo.body();

        if let Some(every) = every {
            // Create a recurrence rule
            self.recurrent_stats
                .entry(title.clone())
                .or_default()
                .insert(
                    todo_date,
                    RecurrentStats {
                        every: every.clone(),
                        next_date: add_interval(todo_date, every),
                        times_ahead_left: LOOK_AHEAD_TIMES,
                        queue: Default::default(),
                        last_date: todo_date,
                        completed: *status == Status::Completed,
                    },
                );
        }

        let tags = match (heading_tags, tags) {
            (Some(h), None) => Some(h),
            (None, Some(h)) => Some(h.clone()),
            (Some(mut h1), Some(h2)) => Some({
                h1.extend(h2.iter().cloned());
                h1
            }),
            (None, None) => None,
        };

        self.dated_todos.entry(todo_date).or_default().push(todo);
        self.dated_declaration_contexts.insert(
            TodoId(todo_date, title.clone()),
            TodoDeclarationContext { path, tags },
        );

        Ok(())
    }

    fn add_dated_todo(
        &mut self,
        cache: &mut NotesCache<'a>,
        prompt: &EdgeCasesPrompt,
        id: &DatedFileIdentifier,
        path: &[String],
        tags: Option<HashSet<Tag>>,
        todo: &'a Todo<'a>,
    ) -> Result<()> {
        let closest_rule = self.find_closest_rule(id.0, &todo.title());
        if let Some((recurrence_date, recurrence_stats)) = closest_rule {
            Self::add_dated_todo_with_ongoing_recurrence(
                prompt,
                cache.get_md_arena(),
                recurrence_date,
                recurrence_stats,
                id.0,
                path,
                todo,
            )?;
        } else {
            self.add_dated_todo_without_ongoing_recurrence(
                cache,
                prompt,
                id.0,
                path.to_owned(),
                tags,
                todo,
            )?;
        }

        Ok(())
    }

    fn scroll_recurrent_rules<'c>(
        &mut self,
        cache: &'c mut NotesCache<'a>,
        prompt: &EdgeCasesPrompt,
        scroll_until: &NaiveDate,
    ) -> Result<()> {
        // Each todo identified by title can have multiple non-overlapping recurrence
        // series that we identified by start date
        for (todo_title, recurrence_series) in self.recurrent_stats.iter_mut() {
            for (todo_start_date, stats) in recurrence_series.iter_mut() {
                // Start scrolling through one recurrence series.
                // The algorithm goes from start date until reaching completion mark or reaching
                // the limit of incompleted occurences in future or looking too far into the
                // future, the latter two is a subject for configuration (todo).
                let mut completed = false;

                while !completed && stats.times_ahead_left > 0 && &stats.next_date <= scroll_until {
                    let current_processing_date = stats.next_date;

                    // With the next date known a new todo entry is created
                    // unless it exists in the queue, if it
                    // comes early it shall use `advance` or `adhoc` params
                    let (date, todo) = match stats.queue.front() {
                        Some((todo_date, _)) if todo_date <= &current_processing_date => {
                            let (todo_date, todo) = stats.queue.pop_front().expect("checked above");

                            if todo_date < current_processing_date {
                                // Check if adhoc or advance is set, so it is already resolved one
                                // way or another:
                                let advance: Option<Interval> = todo.body().advance;
                                let resolution = if let Some(advance) = advance {
                                    let expected = Interval {
                                        value: (current_processing_date - todo_date).num_days()
                                            as u32,
                                        unit: IntervalUnit::Day,
                                    };
                                    (advance != expected).then(|| {
                                        prompt.dated_todo_with_recurrence_misaligned_advance(
                                            &todo_date,
                                            &todo.title(),
                                        )
                                    })
                                } else if !todo.get_adhoc() {
                                    Some(prompt.dated_todo_goes_early(&todo_date, &todo.title()))
                                } else {
                                    None
                                };

                                match resolution {
                                    Some(EdgeCaseResolution::Delete) => {
                                        todo.remove();
                                        todo.update_ast(cache.get_md_arena());
                                        continue;
                                    }
                                    Some(EdgeCaseResolution::Extra(AdjustRecurrentTodo::AdHoc)) => {
                                        todo.set_adhoc(true);
                                        todo.update_ast(cache.get_md_arena());
                                    }
                                    Some(EdgeCaseResolution::Extra(
                                        AdjustRecurrentTodo::Advance,
                                    )) => {
                                        todo.set_advance(Some(Interval {
                                            value: (current_processing_date - todo_date).num_days()
                                                as u32,
                                            unit: IntervalUnit::Day,
                                        }));
                                        todo.update_ast(cache.get_md_arena());
                                    }
                                    Some(EdgeCaseResolution::Extra(
                                        AdjustRecurrentTodo::NudgeFollowing,
                                    )) => {
                                        // This is most complicated resolution as it does
                                        // adjustments on multiple notes and redefines the queue.
                                        // 1. The grid stays the same,
                                        // 2. In fact "nudging" is creating todo items on grid as in
                                        //    normal scenario
                                        // 3. Those unfinished recurrent events off the grid shall
                                        //    be deleted as there will be their adjusted versions
                                        // 4. If somehow there are finished todos we want to
                                        //    preserve that info by making them adhoc
                                        for (_, todo) in stats.queue.drain(..) {
                                            let body = todo.body();
                                            if !body.adhoc && body.status != Status::Completed {
                                                // Adhoc todos and/or completed are left alone,
                                                // otherwise they shall be deleted
                                                todo.remove();
                                                todo.update_ast(cache.get_md_arena());
                                            }
                                        }

                                        // Proceed with current iteration once again with an empty
                                        // queue and on-grid date
                                        continue;
                                    }
                                    Some(EdgeCaseResolution::Exit) => bail!("Aborting..."),
                                    None => {}
                                }
                            }

                            (todo_date, todo)
                        }
                        _ => {
                            if current_processing_date > *TODAY {
                                stats.times_ahead_left -= 1;
                            }

                            let note = cache.get_note(NoteIdentifier::new_dated(
                                current_processing_date.into(),
                            ))?;
                            let mut note_ref = note.borrow_mut();
                            let mut todo_heading =
                                note_ref.get_or_create_heading(TODO_DEFAULT_HEADING);
                            (
                                current_processing_date,
                                todo_heading.add_todo(
                                    cache.get_todos_arena(),
                                    TodoBodyBuilder::default()
                                        .title(todo_title.clone())
                                        .since(todo_start_date.clone())
                                        .build()?,
                                ),
                            )
                        }
                    };

                    let TodoBody {
                        status,
                        advance,
                        delay,
                        adhoc,
                        ..
                    }: &TodoBody = &todo.body();
                    if status == &Status::Completed {
                        completed = true;
                    }

                    // Next date is computed by adding an interval, but the grid might have shifted
                    // in case of advances and delays;
                    if let Some(_) = advance {
                        // The grid is now shifted to be earlier
                        stats.next_date = add_interval(date, &stats.every);
                    } else if let Some(delay) = delay {
                        // Asked to try this todo occurence again in an interval, that means
                        // shifting the grid to the right by this interval
                        stats.next_date = add_interval(date, delay);
                    } else if *adhoc {
                        // With adhoc the date to process stays the same
                    } else {
                        // When none of the above it's on grid which means no
                        // adjustments and stays relative to the current grid
                        stats.next_date = add_interval(current_processing_date, &stats.every);
                    }

                    self.dated_todos.entry(date).or_default().push(todo);
                }
            }
        }

        Ok(())
    }
}

#[derive(Debug)]
struct RecurrentStats<'a> {
    every: Interval,
    next_date: NaiveDate,
    times_ahead_left: usize,
    queue: VecDeque<(NaiveDate, &'a Todo<'a>)>,
    last_date: NaiveDate,
    completed: bool,
}

fn add_interval(date: NaiveDate, interval: &Interval) -> NaiveDate {
    match interval.unit {
        IntervalUnit::Day => date + Duration::days(interval.value as i64),
        IntervalUnit::Week => date + Duration::weeks(interval.value as i64),
        IntervalUnit::Month => date
            .checked_add_months(Months::new(interval.value))
            .expect("out of time bounds"),
        IntervalUnit::Year => date
            .checked_add_months(Months::new(interval.value * 12))
            .expect("out of time bounds"),
    }
}

fn sub_interval(date: NaiveDate, interval: &Interval) -> NaiveDate {
    match interval.unit {
        IntervalUnit::Day => date - Duration::days(interval.value as i64),
        IntervalUnit::Week => date - Duration::weeks(interval.value as i64),
        IntervalUnit::Month => date
            .checked_sub_months(Months::new(interval.value))
            .expect("out of time bounds"),
        IntervalUnit::Year => date
            .checked_sub_months(Months::new(interval.value * 12))
            .expect("out of time bounds"),
    }
}

pub(super) fn scroll_events<'a, 'c>(
    cache: &'c mut NotesCache<'a>,
    prompt: &EdgeCasesPrompt,
    named: impl IntoIterator<Item = NamedFileIdentifier>,
    dated: impl IntoIterator<Item = DatedFileIdentifier>,
) -> Result<EventsView<'a>> {
    let mut view = EventsView::default();

    // Clean up and add named todos to the result view
    for id in named.into_iter() {
        let note = cache.get_note(id.clone().into())?;
        let todos_views = todos_in_note(&note.borrow());

        for todos_view in todos_views.iter() {
            for todo in &todos_view.todos {
                fix_named_todo(cache, prompt, &id, todos_view.path.as_slice(), todo)?;
            }
        }

        view.named_todos.insert(id, todos_views);
    }

    let scroll_until = add_interval(*TODAY, &LOOK_AHEAD_INTERVAL);

    // Clean up and add dated todos to the result view if they have no recurrence
    // rules or they establish a new rule; if there is an ongoing recurrence for
    // a todo identified by name then it will be put in a queue of recurrence
    // stats as they might have occured too early and the recurrence rule
    // implies more todos to be inserted in between.
    for id in dated.into_iter().take_while(|d| d.0 <= scroll_until) {
        let note = cache.get_note(id.clone().into())?;
        let todos_views = todos_in_note(&note.borrow());

        for todos_view in todos_views.iter() {
            for todo in &todos_view.todos {
                view.add_dated_todo(
                    cache,
                    prompt,
                    &id,
                    todos_view.path.as_slice(),
                    todos_view.tags.clone(),
                    todo,
                )?;
            }
        }
    }

    // Process recurrent rules and associated queues to finalize dated todos part of
    // the result event view
    view.scroll_recurrent_rules(cache, prompt, &scroll_until)?;

    Ok(view)
}

fn fix_named_todo<'a>(
    cache: &mut NotesCache<'a>,
    prompt: &EdgeCasesPrompt,
    id: &NamedFileIdentifier,
    path: &[String],
    todo: &Todo<'a>,
) -> Result<()> {
    let lock = todo.body();
    let TodoBody {
        title,
        delay,
        advance,
        every,
        since,
        adhoc,
        statements,
        ..
    }: &TodoBody = &lock;

    if delay.is_some()
        || advance.is_some()
        || every.is_some()
        || since.is_some()
        || statements.is_some()
        || *adhoc
    {
        match prompt.named_todo_has_params(id, path, title) {
            EdgeCaseResolution::Delete => {
                drop(lock);
                todo.remove();
                todo.update_ast(cache.get_md_arena());
            }
            EdgeCaseResolution::Extra(RemoveParamsNamedResolution) => {
                drop(lock);
                todo.remove_params();
                todo.update_ast(cache.get_md_arena());
            }
            EdgeCaseResolution::Exit => bail!("Aborting..."),
        }
    }
    Ok(())
}

fn todos_in_note<'a>(note: &Note<'a>) -> Vec<BatchTodosView<'a>> {
    let mut result = Vec::new();
    let mut ct = note.content_traverse();

    while let Some((path, title, tags_stack, content)) = ct.next() {
        for c in content {
            if let Some(todos) = TryInto::<&[&Todo]>::try_into(c).ok() {
                let mut full_path: Vec<String> = path.into_iter().map(|s| s.to_string()).collect();
                full_path.push(title.to_owned());

                result.push(BatchTodosView {
                    todos: todos.to_vec(),
                    tags: tags_stack.build_tags(),
                    path: full_path,
                });
            }
        }
    }

    result
}

#[derive(Debug)]
struct BatchTodosView<'a> {
    todos: Vec<&'a Todo<'a>>,
    tags: Option<HashSet<Tag>>,
    path: Vec<String>,
}

impl<'a> BatchTodosView<'a> {
    pub(super) fn iter(
        &self,
    ) -> impl Iterator<Item = (&[String], Option<&HashSet<Tag>>, &'a Todo<'a>)> {
        self.todos.iter().filter_map(|t| {
            (!t.is_removed()).then_some((self.path.as_slice(), self.tags.as_ref(), *t))
        })
    }
}

#[cfg(test)]
mod tests {
    use std::cell::RefCell;

    use comrak::{Arena, nodes::AstNode};
    use tempfile::TempDir;

    use super::*;
    use crate::{
        notes::Status,
        vault::{NoteIdentifier, Vault},
    };

    struct TestContext<'a> {
        md_arena: Arena<AstNode<'a>>,
        notes_arena: Arena<RefCell<Note<'a>>>,
        todos_arena: Arena<Todo<'a>>,
        vault: Vault,
        _dir: TempDir,
    }

    impl<'a> TestContext<'a> {
        fn new() -> Self {
            let dir = TempDir::new().expect("cannot create a tempdir");
            Self {
                md_arena: Default::default(),
                notes_arena: Default::default(),
                todos_arena: Default::default(),
                vault: Vault::new(dir.path().to_path_buf()),
                _dir: dir,
            }
        }

        fn add_note(&self, id: &NoteIdentifier, data: &str) {
            self.vault
                .write(id, data.as_bytes())
                .expect("cannot add note");
        }
    }

    // Because of how lifetimes are used for arena reference, 'a becomes invariant
    // and cannot be returned with a method, so we're falling back so syntax level
    macro_rules! make_cache {
        ($ctx:ident) => {
            NotesCache::new(
                &$ctx.md_arena,
                &$ctx.notes_arena,
                &$ctx.todos_arena,
                &$ctx.vault,
            )
        };
    }

    fn named_test_helper(prompt: &EdgeCasesPrompt, data: &str) {
        let ctx = TestContext::new();
        let mut cache = make_cache!(ctx);

        let id = NoteIdentifier::new_named("test".to_owned());

        ctx.add_note(&id, data);

        let (named, dated) = ctx.vault.grouped_notes().expect("cannot access vault");

        let events_view =
            scroll_events(&mut cache, prompt, named, dated).expect("error scrolling events");

        let mut scrolled_todos = events_view
            .named_todos
            .get(&id.into_named().unwrap())
            .into_iter()
            .map(|todos| todos.iter())
            .flatten();

        let scrolled_todo = scrolled_todos.next().expect("expected at least one todo");

        assert_eq!(
            scrolled_todo.tags,
            Some(
                [Tag::new(vec![
                    "nicetag".to_owned(),
                    "deeper".to_owned(),
                    "another".to_owned()
                ])]
                .into()
            )
        );

        assert_eq!(
            scrolled_todo.path,
            vec!["Todo".to_owned(), "Nested".to_owned()]
        );

        let TodoBody {
            title,
            status,
            delay,
            advance,
            every,
            since,
            adhoc,
            statements,
            ..
        }: &TodoBody = &scrolled_todo.todos.first().expect("must be a todo").body();

        assert_eq!(title, "do something");
        assert_eq!(*status, Status::Open);
        assert_eq!(*delay, None);
        assert_eq!(*advance, None);
        assert_eq!(*every, None);
        assert_eq!(*since, None);
        assert_eq!(*adhoc, false);
        assert!(statements.is_none());
    }

    #[test]
    fn named_todo_simple() {
        let prompt = EdgeCasesPrompt::faux();
        named_test_helper(
            &prompt,
            "
# Todo

#nicetag/deeper

Some content

## Nested

#another

- [ ] do something",
        );
    }

    #[test]
    fn named_todo_redundant_params_delete() {
        let mut prompt = EdgeCasesPrompt::faux();

        faux::when!(prompt.named_todo_has_params()).then_return(EdgeCaseResolution::Delete);

        let ctx = TestContext::new();
        let mut cache = make_cache!(ctx);

        let id = NoteIdentifier::new_named("test".to_owned());

        ctx.add_note(
            &id,
            "
# Todo

#nicetag/deeper

Some content

## Nested

#another

- [ ] do something @every day, since 2025-05-05, also action here",
        );

        let (named, dated) = ctx.vault.grouped_notes().expect("cannot access vault");

        let events_view =
            scroll_events(&mut cache, &prompt, named, dated).expect("error scrolling events");

        assert!(events_view.iter_named_todos().next().is_none());
    }

    #[test]
    fn named_todo_redundant_params_cleanup() {
        let mut prompt = EdgeCasesPrompt::faux();

        faux::when!(prompt.named_todo_has_params())
            .then_return(EdgeCaseResolution::Extra(RemoveParamsNamedResolution));

        named_test_helper(
            &mut prompt,
            "
# Todo

#nicetag/deeper

Some content

## Nested

#another

- [ ] do something @every day, since 2025-05-05, also action here",
        );
    }

    #[test]
    fn dated_todo_new_recurrence() {
        let prompt = EdgeCasesPrompt::faux();

        let ctx = TestContext::new();
        let mut cache = make_cache!(ctx);

        let id = NoteIdentifier::new_named("2020-02-02".to_owned());

        ctx.add_note(
            &id,
            "
# Todo

#nicetag/deeper

Some content

## Nested

#another

- [ ] do something @every 2 weeks",
        );

        let (named, dated) = ctx.vault.grouped_notes().expect("cannot access vault");

        let events_view =
            scroll_events(&mut cache, &prompt, named, dated).expect("error scrolling events");

        let start_date = id.into_dated().expect("must be a valid date").0;
        let interval = Interval {
            value: 2,
            unit: IntervalUnit::Week,
        };

        let todo = &events_view
            .dated_todos
            .get(&start_date)
            .expect("a todo vec expected")[0];
        assert_eq!(&*todo.title(), "do something");
        assert_eq!(todo.body().since, None);

        let next_date = add_interval(start_date, &interval);
        let todo = &events_view
            .dated_todos
            .get(&next_date)
            .expect("a todo vec expected")[0];
        assert_eq!(&*todo.title(), "do something");
        assert_eq!(todo.body().since, Some(start_date));

        // Must be two additional todos
        assert_eq!(
            events_view
                .dated_todos
                .range(TODAY.succ_opt().expect("there must be tomorrow")..)
                .filter_map(|(_, v)| v.get(0))
                .count(),
            2
        );
    }

    #[test]
    fn dated_todo_recurrence_conflicting_params() {
        let mut prompt = EdgeCasesPrompt::faux();
        faux::when!(prompt.dated_todo_with_recurrence_conflicting_params()).then_return(
            EdgeCaseResolution::Extra(KeepOneParamDatedResolution::AdHoc),
        );

        let ctx = TestContext::new();
        let mut cache = make_cache!(ctx);

        let id = NoteIdentifier::new_named("2020-02-02".to_owned());

        ctx.add_note(
            &id,
            "
# Todo

#nicetag/deeper

Some content

## Nested

#another

- [ ] do something @every month",
        );

        let next_date = NaiveDate::from_ymd_opt(2020, 5, 2).expect("expected a date");
        ctx.add_note(
            &NoteIdentifier::new_dated(next_date),
            "
# Todo

- [ ] do something @since 2020-02-02, delay 3 days, adhoc",
        );

        let (named, dated) = ctx.vault.grouped_notes().expect("cannot access vault");

        let events_view =
            scroll_events(&mut cache, &prompt, named, dated).expect("error scrolling events");

        let todo = &events_view
            .dated_todos
            .get(&next_date)
            .expect("a todo vec expected")[0];
        assert_eq!(&*todo.title(), "do something");
        assert_eq!(todo.body().delay, None);
        assert!(todo.body().adhoc);
    }

    #[test]
    fn dated_todo_recurrence_goes_early_adhoc() {
        let next_date = NaiveDate::from_ymd_opt(2020, 4, 28).expect("expected a date");

        let mut prompt = EdgeCasesPrompt::faux();
        faux::when!(prompt.dated_todo_goes_early(next_date, "do something"))
            .then_return(EdgeCaseResolution::Extra(AdjustRecurrentTodo::AdHoc));

        let ctx = TestContext::new();
        let mut cache = make_cache!(ctx);

        let start_date = NaiveDate::from_ymd_opt(2020, 2, 2).expect("expected a valid date");
        let id = NoteIdentifier::new_dated(start_date);

        ctx.add_note(
            &id,
            "
# Todo

#nicetag/deeper

Some content

## Nested

#another

- [ ] do something @every month",
        );

        ctx.add_note(
            &NoteIdentifier::new_dated(next_date),
            "
# Todo

- [ ] do something @since 2020-02-02",
        );

        let (named, dated) = ctx.vault.grouped_notes().expect("cannot access vault");

        let events_view =
            scroll_events(&mut cache, &prompt, named, dated).expect("error scrolling events");

        let interval = Interval {
            value: 1,
            unit: IntervalUnit::Month,
        };

        let todo = &events_view
            .dated_todos
            .get(&next_date)
            .expect("a todo vec expected")[0];
        assert_eq!(&*todo.title(), "do something");
        assert_eq!(todo.body().delay, None);
        assert!(todo.body().adhoc);

        let next_on_grid = add_interval(start_date, &interval);
        let todo = &events_view
            .dated_todos
            .get(&next_on_grid)
            .expect("a todo vec expected")[0];
        assert_eq!(&*todo.title(), "do something");
        assert_eq!(todo.body().delay, None);
        assert!(!todo.body().adhoc);

        let next_on_grid = add_interval(next_on_grid, &interval);
        let todo = &events_view
            .dated_todos
            .get(&next_on_grid)
            .expect("a todo vec expected")[0];
        assert_eq!(&*todo.title(), "do something");
        assert_eq!(todo.body().delay, None);
        assert!(!todo.body().adhoc);
    }

    #[test]
    fn dated_todo_recurrence_goes_early_advance() {
        let next_date = NaiveDate::from_ymd_opt(2020, 4, 28).expect("expected a date");

        let mut prompt = EdgeCasesPrompt::faux();
        faux::when!(prompt.dated_todo_goes_early(next_date, "do something"))
            .then_return(EdgeCaseResolution::Extra(AdjustRecurrentTodo::Advance));

        let ctx = TestContext::new();
        let mut cache = make_cache!(ctx);

        let start_date = NaiveDate::from_ymd_opt(2020, 2, 2).expect("expected a valid date");
        let id = NoteIdentifier::new_dated(start_date);

        ctx.add_note(
            &id,
            "
# Todo

#nicetag/deeper

Some content

## Nested

#another

- [ ] do something @every month",
        );

        ctx.add_note(
            &NoteIdentifier::new_dated(next_date),
            "
# Todo

- [ ] do something @since 2020-02-02",
        );

        let (named, dated) = ctx.vault.grouped_notes().expect("cannot access vault");

        let events_view =
            scroll_events(&mut cache, &prompt, named, dated).expect("error scrolling events");

        let interval = Interval {
            value: 1,
            unit: IntervalUnit::Month,
        };

        let todo = &events_view
            .dated_todos
            .get(&next_date)
            .expect("a todo vec expected")[0];
        assert_eq!(&*todo.title(), "do something");
        assert_eq!(
            todo.body().advance,
            Some(Interval {
                value: 4,
                unit: IntervalUnit::Day
            })
        );
        assert!(!todo.body().adhoc);

        let next_on_grid = add_interval(next_date, &interval);
        let todo = &events_view
            .dated_todos
            .get(&next_on_grid)
            .expect("a todo vec expected")[0];
        assert_eq!(&*todo.title(), "do something");
        assert_eq!(todo.body().delay, None);
        assert_eq!(todo.body().advance, None);
        assert!(!todo.body().adhoc);
    }

    #[test]
    fn dated_todo_recurrence_misaligned_advance() {
        let next_date = NaiveDate::from_ymd_opt(2020, 4, 28).expect("expected a date");

        let mut prompt = EdgeCasesPrompt::faux();
        faux::when!(
            prompt.dated_todo_with_recurrence_misaligned_advance(next_date, "do something")
        )
        .then_return(EdgeCaseResolution::Extra(AdjustRecurrentTodo::Advance));

        let ctx = TestContext::new();
        let mut cache = make_cache!(ctx);

        let start_date = NaiveDate::from_ymd_opt(2020, 2, 2).expect("expected a valid date");
        let id = NoteIdentifier::new_dated(start_date);

        ctx.add_note(
            &id,
            "
# Todo

#nicetag/deeper

Some content

## Nested

#another

- [ ] do something @every month",
        );

        ctx.add_note(
            &NoteIdentifier::new_dated(next_date),
            "
# Todo

- [ ] do something @since 2020-02-02, advance 10 days",
        );

        let (named, dated) = ctx.vault.grouped_notes().expect("cannot access vault");

        let events_view =
            scroll_events(&mut cache, &prompt, named, dated).expect("error scrolling events");

        let interval = Interval {
            value: 1,
            unit: IntervalUnit::Month,
        };

        let todo = &events_view
            .dated_todos
            .get(&next_date)
            .expect("a todo vec expected")[0];
        assert_eq!(&*todo.title(), "do something");
        assert_eq!(
            todo.body().advance,
            Some(Interval {
                value: 4,
                unit: IntervalUnit::Day
            })
        );
        assert!(!todo.body().adhoc);

        let next_on_grid = add_interval(next_date, &interval);
        let todo = &events_view
            .dated_todos
            .get(&next_on_grid)
            .expect("a todo vec expected")[0];
        assert_eq!(&*todo.title(), "do something");
        assert_eq!(todo.body().delay, None);
        assert_eq!(todo.body().advance, None);
        assert!(!todo.body().adhoc);
    }

    #[test]
    fn dated_todo_recurrence_delay() {
        let prompt = EdgeCasesPrompt::faux();

        let ctx = TestContext::new();
        let mut cache = make_cache!(ctx);

        let interval = Interval {
            value: 1,
            unit: IntervalUnit::Month,
        };
        let start_date = NaiveDate::from_ymd_opt(2020, 2, 2).expect("expected a valid date");
        let next_date = add_interval(start_date, &interval);

        let id = NoteIdentifier::new_dated(start_date);

        ctx.add_note(
            &id,
            "
# Todo

#nicetag/deeper

Some content

## Nested

#another

- [ ] do something @every month",
        );

        ctx.add_note(
            &NoteIdentifier::new_dated(next_date),
            "
# Todo

- [ ] do something @since 2020-02-02, delay 3 days",
        );

        let (named, dated) = ctx.vault.grouped_notes().expect("cannot access vault");

        let events_view =
            scroll_events(&mut cache, &prompt, named, dated).expect("error scrolling events");

        let delay = Interval {
            value: 3,
            unit: IntervalUnit::Day,
        };
        let todo = &events_view
            .dated_todos
            .get(&next_date)
            .expect("a todo vec expected")[0];
        assert_eq!(&*todo.title(), "do something");
        assert_eq!(todo.body().delay, Some(delay));
        assert!(!todo.body().adhoc);

        let next_on_grid = add_interval(next_date, &delay);
        let todo = &events_view
            .dated_todos
            .get(&next_on_grid)
            .expect("a todo vec expected")[0];
        assert_eq!(&*todo.title(), "do something");
        assert_eq!(todo.body().delay, None);
        assert_eq!(todo.body().advance, None);
        assert!(!todo.body().adhoc);

        let next_on_grid = add_interval(next_on_grid, &interval);
        let todo = &events_view
            .dated_todos
            .get(&next_on_grid)
            .expect("a todo vec expected")[0];
        assert_eq!(&*todo.title(), "do something");
        assert_eq!(todo.body().delay, None);
        assert_eq!(todo.body().advance, None);
        assert!(!todo.body().adhoc);
    }

    #[test]
    fn dated_todo_nudge() {
        let start_date = NaiveDate::from_ymd_opt(2020, 2, 2).expect("expected a valid date");
        let interval = Interval {
            value: 1,
            unit: IntervalUnit::Month,
        };
        let delay_date = add_interval(add_interval(start_date, &interval), &interval);
        let first_broken_date = add_interval(add_interval(delay_date, &interval), &interval);

        let mut prompt = EdgeCasesPrompt::faux();
        faux::when!(prompt.dated_todo_goes_early(first_broken_date, "do something")).then_return(
            EdgeCaseResolution::Extra(AdjustRecurrentTodo::NudgeFollowing),
        );

        let ctx = TestContext::new();
        let mut cache = make_cache!(ctx);

        let id = NoteIdentifier::new_dated(start_date);

        ctx.add_note(
            &id,
            "
# Todo

- [ ] do something @every month",
        );

        let (named, dated) = ctx.vault.grouped_notes().expect("cannot access vault");

        scroll_events(&mut cache, &prompt, named, dated).expect("error scrolling events");

        cache.persist_notes().expect("unable to persist notes");

        // Starting over with previously persisted data
        let mut cache = make_cache!(ctx);

        let id = NoteIdentifier::new_dated(delay_date);

        ctx.add_note(
            &id,
            "
# Todo

- [ ] do something @since 2020-02-02, delay 3 days",
        );

        // The date where we place a note with adhoc param is on the old grid, relative
        // to the note with a delay introduced; that would require each following note
        // to be delayed (a new grid), but with adhoc param the next note we're checking
        // shall stay at the old place
        let adhoc_date = add_interval(delay_date, &interval);

        let id = NoteIdentifier::new_dated(adhoc_date);

        ctx.add_note(
            &id,
            "
# Todo

- [ ] do something @since 2020-02-02, adhoc",
        );

        let delay = Interval {
            value: 3,
            unit: IntervalUnit::Day,
        };

        let (named, dated) = ctx.vault.grouped_notes().expect("cannot access vault");

        scroll_events(&mut cache, &prompt, named, dated).expect("error scrolling events");

        // Since the resolution is set to nudging, that means first items would be on
        // grid until the delay is hit, the next one is expected after the delay
        // and the rest goes with the adjusted grid. Old grid's items shall be
        // cleaned up except one that was marked as adhoc.

        let mut cache = make_cache!(ctx);

        let (named, dated) = ctx.vault.grouped_notes().expect("cannot access vault");

        let events_view =
            scroll_events(&mut cache, &prompt, named, dated).expect("error scrolling events");

        let mut todos_iter = events_view.dated_todos.iter();

        let (date, todos) = todos_iter.next().expect("expected initial todo");
        assert_eq!(date, &start_date);
        assert_eq!(todos.len(), 1);
        assert_eq!(&*todos[0].title(), "do something");
        assert!(!todos[0].is_removed());
        assert!(!todos[0].get_adhoc());
        assert!(todos[0].body().advance.is_none());
        assert!(todos[0].body().delay.is_none());

        let current_date = add_interval(*date, &interval);
        let (date, todos) = todos_iter.next().expect("expected next on grid todo");
        assert_eq!(date, &current_date);
        assert_eq!(todos.len(), 1);
        assert_eq!(&*todos[0].title(), "do something");
        assert!(!todos[0].is_removed());
        assert!(!todos[0].get_adhoc());
        assert!(todos[0].body().advance.is_none());
        assert!(todos[0].body().delay.is_none());

        let current_date = add_interval(*date, &interval);
        let (date, todos) = todos_iter.next().expect("expected next on grid todo");
        assert_eq!(date, &current_date);
        assert_eq!(todos.len(), 1);
        assert_eq!(&*todos[0].title(), "do something");
        assert!(!todos[0].is_removed());
        assert!(!todos[0].get_adhoc());
        assert!(todos[0].body().advance.is_none());
        assert_eq!(todos[0].body().delay, Some(delay));

        // This todo is off the previous grid but with delay applied
        let current_date = add_interval(*date, &delay);
        let (date, todos) = todos_iter.next().expect("expected next on grid todo");
        assert_eq!(date, &current_date);
        assert_eq!(todos.len(), 1);
        assert_eq!(&*todos[0].title(), "do something");
        assert!(!todos[0].is_removed());
        assert!(!todos[0].get_adhoc());
        assert!(todos[0].body().advance.is_none());
        assert!(todos[0].body().delay.is_none());

        // Next todos are on delayed grid now, except one marked as adhoc that shall
        // stay on the old grid
        let (received_adhoc_date, todos) = todos_iter.next().expect("expected next on grid todo");
        assert_eq!(received_adhoc_date, &adhoc_date);
        assert_eq!(todos.len(), 1);
        assert_eq!(&*todos[0].title(), "do something");
        assert!(!todos[0].is_removed());
        assert!(todos[0].get_adhoc());
        assert!(todos[0].body().advance.is_none());
        assert!(todos[0].body().delay.is_none());

        let current_date = add_interval(*date, &interval);
        let (date, todos) = todos_iter.next().expect("expected next on grid todo");
        assert_eq!(date, &current_date);
        assert_eq!(todos.len(), 1);
        assert_eq!(&*todos[0].title(), "do something");
        assert!(!todos[0].is_removed());
        assert!(!todos[0].get_adhoc());
        assert!(todos[0].body().advance.is_none());
        assert!(todos[0].body().delay.is_none());

        let current_date = add_interval(*date, &interval);
        let (date, todos) = todos_iter.next().expect("expected next on grid todo");
        assert_eq!(date, &current_date);
        assert_eq!(todos.len(), 1);
        assert_eq!(&*todos[0].title(), "do something");
        assert!(!todos[0].is_removed());
        assert!(!todos[0].get_adhoc());
        assert!(todos[0].body().advance.is_none());
        assert!(todos[0].body().delay.is_none());
    }

    #[test]
    fn completion_and_starting_over() {
        let prompt = EdgeCasesPrompt::faux();

        let ctx = TestContext::new();
        let mut cache = make_cache!(ctx);

        let first_interval = Interval {
            value: 1,
            unit: IntervalUnit::Month,
        };
        let first_start_date = NaiveDate::from_ymd_opt(2020, 2, 2).expect("expected a valid date");
        let end_date = NaiveDate::from_ymd_opt(2020, 6, 2).expect("expected a valid date");
        let next_start_date = NaiveDate::from_ymd_opt(2020, 8, 8).expect("expected a valid date");
        let next_interval = Interval {
            value: 1,
            unit: IntervalUnit::Week,
        };

        ctx.add_note(
            &NoteIdentifier::new_dated(first_start_date),
            "
# Todo

#nicetag/deeper

Some content

## Nested

#another

- [ ] do something @every month",
        );

        ctx.add_note(
            &NoteIdentifier::new_dated(end_date),
            "
# Todo

- [c] do something @since 2020-02-02",
        );

        ctx.add_note(
            &NoteIdentifier::new_dated(next_start_date),
            "
# Todo

- [ ] do something @every week",
        );

        let (named, dated) = ctx.vault.grouped_notes().expect("cannot access vault");

        let events_view =
            scroll_events(&mut cache, &prompt, named, dated).expect("error scrolling events");

        assert_eq!(
            &*events_view
                .dated_todos
                .get(&first_start_date)
                .expect("expected todo")[0]
                .title(),
            "do something"
        );
        assert_eq!(
            &*events_view
                .dated_todos
                .get(&add_interval(first_start_date, &first_interval))
                .expect("expected todo")[0]
                .title(),
            "do something"
        );
        assert_eq!(
            &*events_view
                .dated_todos
                .get(&end_date)
                .expect("expected todo")[0]
                .title(),
            "do something"
        );
        // Must be nothing after completion
        assert!(
            events_view
                .dated_todos
                .get(&add_interval(end_date, &first_interval))
                .is_none()
        );

        // And resumed as good as new with a different interval
        assert_eq!(
            &*events_view
                .dated_todos
                .get(&next_start_date)
                .expect("expected todo")[0]
                .title(),
            "do something"
        );
        assert_eq!(
            &*events_view
                .dated_todos
                .get(&add_interval(next_start_date, &next_interval))
                .expect("expected todo")[0]
                .title(),
            "do something"
        );
    }

    #[test]
    fn dated_inherited_tags_and_path() {
        let prompt = EdgeCasesPrompt::faux();

        let ctx = TestContext::new();
        let mut cache = make_cache!(ctx);

        let interval = Interval {
            value: 1,
            unit: IntervalUnit::Month,
        };
        let start_date = NaiveDate::from_ymd_opt(2020, 2, 2).expect("expected a valid date");

        ctx.add_note(
            &NoteIdentifier::new_dated(start_date),
            "
# Todo

#nicetag/deeper

Some content

## Nested

tags: #another #yetanother


- [ ] do something @every month",
        );

        let (named, dated) = ctx.vault.grouped_notes().expect("cannot access vault");

        let events_view =
            scroll_events(&mut cache, &prompt, named, dated).expect("error scrolling events");

        let mut iter = events_view.iter_dated_todos();

        let TodoWithContext {
            path,
            tags,
            todo,
            note_id,
        } = iter.next().expect("expected todo");
        let date = note_id.into_dated().unwrap().0;
        assert_eq!(date, start_date);
        assert_eq!(path, ["Todo", "Nested"]);
        assert_eq!(
            tags,
            Some(
                &[
                    Tag::new(vec![
                        "nicetag".to_owned(),
                        "deeper".to_owned(),
                        "another".to_owned()
                    ]),
                    Tag::new(vec![
                        "nicetag".to_owned(),
                        "deeper".to_owned(),
                        "yetanother".to_owned()
                    ])
                ]
                .into_iter()
                .collect()
            )
        );
        assert_eq!(&*todo.title(), "do something");

        // Next reccurence entry inherits original path and tags because those are lost
        // on automatic successor creation but can be useful for filtering the agenda
        let TodoWithContext {
            path,
            tags,
            todo,
            note_id,
        } = iter.next().expect("expected todo");
        let date = note_id.into_dated().unwrap().0;
        assert_eq!(date, add_interval(start_date, &interval));
        assert_eq!(path, ["Todo", "Nested"]);
        assert_eq!(
            tags,
            Some(
                &[
                    Tag::new(vec![
                        "nicetag".to_owned(),
                        "deeper".to_owned(),
                        "another".to_owned()
                    ]),
                    Tag::new(vec![
                        "nicetag".to_owned(),
                        "deeper".to_owned(),
                        "yetanother".to_owned()
                    ])
                ]
                .into_iter()
                .collect()
            )
        );
        assert_eq!(&*todo.title(), "do something");
    }
}
