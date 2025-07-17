use std::{collections::BTreeMap, iter};

use anyhow::Result;
use chrono::{Datelike, NaiveDate, TimeDelta};
use itertools::Itertools;
use ratatui::{
    DefaultTerminal, Frame,
    buffer::Buffer,
    crossterm::event::{self, Event, KeyCode, KeyEvent, KeyEventKind, KeyModifiers},
    layout::{Constraint, Flex, Layout, Margin, Rect},
    style::Stylize,
    symbols::border,
    text::{Line, Span, Text, ToSpan, ToText},
    widgets::{
        Block, Borders, Clear, List, ListState, Paragraph, Scrollbar, ScrollbarOrientation,
        ScrollbarState, StatefulWidget, Widget,
    },
};

use crate::{
    TODAY,
    agenda::TodoWithContext,
    notes::{Interval, IntervalUnit, Todo, TodoBody},
    vault::NamedFileIdentifier,
};

pub(crate) fn show_agenda<'v, 'a: 'v>(
    dated_todos: impl DoubleEndedIterator<Item = TodoWithContext<'a, 'v>>,
    named_todos: impl Iterator<Item = TodoWithContext<'a, 'v>>,
) {
    let mut terminal = ratatui::init();

    let result = App::new(dated_todos, named_todos).run(&mut terminal);

    ratatui::restore();

    result.expect("TUI error");
}

type DatedTodos<'a, 'v> = BTreeMap<NaiveDate, Vec<TodoWithContext<'a, 'v>>>;

type NamedTodos<'a, 'v> = BTreeMap<String, Vec<TodoWithContext<'a, 'v>>>;

struct App<'a, 'v> {
    exit: bool,
    grid_adjustment: bool,
    show_help: bool,
    agenda_state: AgendaWidgetState,
    dated_todos: DatedTodos<'a, 'v>,
    named_todos: NamedTodos<'a, 'v>,
}

impl<'a, 'v> App<'a, 'v> {
    fn new(
        dated_todos_iter: impl DoubleEndedIterator<Item = TodoWithContext<'a, 'v>>,
        named_todos_iter: impl Iterator<Item = TodoWithContext<'a, 'v>>,
    ) -> Self {
        let mut dated_todos: DatedTodos = Default::default();
        let mut named_todos: NamedTodos = Default::default();

        let mut today_idx = 0;
        let mut dated_count = 0;

        for (date, todos) in dated_todos_iter
            .rev()
            .filter_map(|todo| match todo.note_id.as_dated() {
                Some(date) if todo.todo.is_open() => Some((date.0, todo)),
                _ => None,
            })
            .chunk_by(|(date, _)| *date)
            .into_iter()
        {
            let todos_vec: Vec<_> = todos.map(|(_, todos)| todos).collect();

            if date <= *TODAY {
                today_idx = dated_count;
            }

            dated_count += todos_vec.len();

            dated_todos.insert(date, todos_vec);
        }

        for (name, todos) in named_todos_iter
            .filter_map(|todo| match todo.note_id.as_named() {
                Some(name) if todo.todo.is_open() => Some((name.0.clone(), todo)),
                _ => None,
            })
            .chunk_by(|(name, _)| name.clone())
            .into_iter()
        {
            let todos_vec: Vec<_> = todos.map(|(_, todos)| todos).collect();

            named_todos.insert(name.clone(), todos_vec);
        }

        App {
            exit: false,
            grid_adjustment: false,
            show_help: false,
            agenda_state: AgendaWidgetState::new(&dated_todos, &named_todos, today_idx),
            dated_todos,
            named_todos,
        }
    }

    fn run(&mut self, terminal: &mut DefaultTerminal) -> Result<()> {
        while !self.exit {
            terminal.draw(|frame| self.draw(frame))?;
            self.handle_events()?;
        }
        Ok(())
    }

    fn draw(&mut self, frame: &mut Frame) {
        let title = Line::from(format!("{} {}", TODAY.format("%A, %B"), TODAY.day()));
        let instructions = Line::from(vec![
            "Help ".into(),
            "h".blue().bold(),
            " | ".bold(),
            format!("[{}] ", self.grid_adjustment.then_some('x').unwrap_or(' ')).green(),
            "Grid adjustment ".into(),
            "g".blue().bold(),
            " | ".bold(),
            "Quit ".into(),
            "q ".blue().bold(),
        ]);
        let block = Block::bordered()
            .title(title.centered())
            .title_bottom(instructions.centered())
            .borders(Borders::TOP | Borders::BOTTOM);

        let area = frame.area();
        let buffer = frame.buffer_mut();

        AgendaWidget::new(&self.dated_todos, &self.named_todos).render(
            block.inner(area),
            buffer,
            &mut self.agenda_state,
        );
        block.render(area, buffer);

        if self.show_help {
            let block = Block::bordered().title("Help");
            let area = popup_area(area, 90, 80);
            frame.render_widget(Clear, area);
            frame.render_widget(block, area);
        }
    }

    fn handle_events(&mut self) -> Result<()> {
        match event::read()? {
            Event::Key(key_event) if key_event.kind == KeyEventKind::Press => {
                self.handle_key_event(key_event)
            }
            _ => {}
        };
        Ok(())
    }

    fn get_selected_todo(&self) -> Option<&TodoWithContext<'a, 'v>> {
        let idx = self.agenda_state.selected_idx;

        self.dated_todos
            .values()
            .rev()
            .chain(self.named_todos.values())
            .flatten()
            .nth(idx)
    }

    fn handle_key_event(&mut self, key_event: KeyEvent) {
        if !self.show_help {
            match key_event {
                KeyEvent {
                    code: KeyCode::Char('q'),
                    ..
                } => self.exit(),
                KeyEvent {
                    code: KeyCode::Char('j'),
                    modifiers: KeyModifiers::NONE,
                    ..
                }
                | KeyEvent {
                    code: KeyCode::Down,
                    modifiers: KeyModifiers::NONE,
                    ..
                }
                | KeyEvent {
                    code: KeyCode::Char('n'),
                    modifiers: KeyModifiers::CONTROL,
                    ..
                } => self
                    .agenda_state
                    .select_next(&self.dated_todos, &self.named_todos),
                KeyEvent {
                    code: KeyCode::Char('k'),
                    modifiers: KeyModifiers::NONE,
                    ..
                }
                | KeyEvent {
                    code: KeyCode::Up,
                    modifiers: KeyModifiers::NONE,
                    ..
                }
                | KeyEvent {
                    code: KeyCode::Char('p'),
                    modifiers: KeyModifiers::CONTROL,
                    ..
                } => self
                    .agenda_state
                    .select_previous(&self.dated_todos, &self.named_todos),
                KeyEvent {
                    code: KeyCode::Char(' '),
                    modifiers: KeyModifiers::NONE,
                    ..
                } => {
                    if let Some(todo) = self.get_selected_todo() {
                        todo.todo.toggle_done();
                        if self.grid_adjustment {
                            if let Some(date) = todo.note_id.as_dated() {
                                adjust_todo(date.0, todo.todo);
                            }
                        }
                    }
                }
                KeyEvent {
                    code: KeyCode::Char('c'),
                    modifiers: KeyModifiers::NONE,
                    ..
                } => {
                    if let Some(todo) = self.get_selected_todo() {
                        todo.todo.toggle_completed();
                    }
                }
                KeyEvent {
                    code: KeyCode::Char('g'),
                    modifiers: KeyModifiers::NONE,
                    ..
                } => {
                    self.grid_adjustment = !self.grid_adjustment;
                }
                KeyEvent {
                    code: KeyCode::Char('h'),
                    modifiers: KeyModifiers::NONE,
                    ..
                } => {
                    self.show_help = true;
                }
                _ => {}
            }
        } else {
            self.show_help = false;
        }
    }

    fn exit(&mut self) {
        self.exit = true;
    }
}

struct AgendaWidget<'a, 'v, 's> {
    dated_todos: &'s DatedTodos<'a, 'v>,
    named_todos: &'s NamedTodos<'a, 'v>,
}

impl<'a, 'v, 's> AgendaWidget<'a, 'v, 's> {
    fn new(dated_todos: &'s DatedTodos<'a, 'v>, named_todos: &'s NamedTodos<'a, 'v>) -> Self {
        Self {
            dated_todos,
            named_todos,
        }
    }
}

struct AgendaWidgetState {
    list_state: ListState,
    scroll_state: ScrollbarState,
    selected_idx: usize,
    last_idx: usize,
}

impl AgendaWidgetState {
    fn new<'a, 'v>(
        dated_todos: &DatedTodos<'a, 'v>,
        named_todos: &NamedTodos<'a, 'v>,
        selected_idx: usize,
    ) -> Self {
        let mut state = Self {
            selected_idx,
            list_state: Default::default(),
            scroll_state: Default::default(),
            last_idx: 0,
        };

        state.update_states(dated_todos, named_todos);

        state
    }

    fn select_next<'a, 'v>(
        &mut self,
        dated_todos: &DatedTodos<'a, 'v>,
        named_todos: &NamedTodos<'a, 'v>,
    ) {
        if self.selected_idx < self.last_idx {
            self.selected_idx += 1;
            self.update_states(dated_todos, named_todos);
        }
    }

    fn select_previous<'a, 'v>(
        &mut self,
        dated_todos: &DatedTodos<'a, 'v>,
        named_todos: &NamedTodos<'a, 'v>,
    ) {
        if self.selected_idx > 0 {
            self.selected_idx -= 1;
            self.update_states(dated_todos, named_todos);
        }
    }

    fn update_states<'a, 'v>(
        &mut self,
        dated_todos: &DatedTodos<'a, 'v>,
        named_todos: &NamedTodos<'a, 'v>,
    ) {
        // Compute line number from todo occurence index:
        let mut last_idx = 0;
        let mut skipped_idx = 0;
        let mut list_lines_acc = 1; // Using 1 as 0 is taken by the first section name

        let mut found = None;
        for todos in dated_todos.values().rev().chain(named_todos.values()) {
            let section_length = todos.len();
            if found.is_none() && self.selected_idx < (skipped_idx + section_length) {
                found = Some(list_lines_acc + self.selected_idx - skipped_idx);
            }
            if found.is_none() {
                skipped_idx += section_length;
            }

            list_lines_acc += 2 + section_length; // 1 for separator, 1 for section name
            last_idx += section_length;
        }

        if let Some(line) = found {
            self.scroll_state = self
                .scroll_state
                .content_length(list_lines_acc - 1)
                .position(line);
            self.list_state.select(Some(line));
        }

        self.last_idx = last_idx - 1;
    }
}

impl<'a, 'v, 's> StatefulWidget for AgendaWidget<'a, 'v, 's> {
    type State = AgendaWidgetState;

    fn render(self, area: Rect, buf: &mut Buffer, state: &mut Self::State) {
        let list = List::new(
            self.dated_todos
                .iter()
                .rev()
                .flat_map(|(date, todos)| {
                    iter::once(date.to_span().bold())
                        .chain(todos.iter().map(|t| Span::raw(render_todo(t))))
                        .chain(iter::once("\n".to_span()))
                })
                .chain(self.named_todos.iter().flat_map(|(name, todos)| {
                    iter::once(name.to_span().bold()).chain(
                        todos
                            .iter()
                            .map(|t| Span::raw(render_todo(t)))
                            .chain(iter::once("\n".to_span())),
                    )
                })),
        )
        .scroll_padding(10)
        .highlight_symbol(">>");

        let scrollbar = Scrollbar::new(ScrollbarOrientation::VerticalRight);

        StatefulWidget::render(list, area, buf, &mut state.list_state);
        scrollbar.render(area, buf, &mut state.scroll_state);
    }
}

fn render_todo(todo: &TodoWithContext) -> String {
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
    }: &TodoBody = &todo.todo.body();

    let tags_str = todo.tags_lock().iter().map(|t| t.to_string()).join(" ");

    let adjustment_str = if let Some(delay) = delay {
        format!("(delay {delay})")
    } else if let Some(advance) = advance {
        format!("(advance {advance})")
    } else {
        "".to_string()
    };

    format!(
        " [{}] {title} {tags_str} {adjustment_str}",
        status.as_char()
    )
}

fn popup_area(area: Rect, percent_x: u16, percent_y: u16) -> Rect {
    let vertical = Layout::vertical([Constraint::Percentage(percent_y)]).flex(Flex::Center);
    let horizontal = Layout::horizontal([Constraint::Percentage(percent_x)]).flex(Flex::Center);
    let [area] = vertical.areas(area);
    let [area] = horizontal.areas(area);
    area
}

fn adjust_todo(todo_date: NaiveDate, todo: &Todo) {
    if todo.is_open() {
        todo.set_advance(None);
        todo.set_delay(None);
    } else {
        let delta = (todo_date - *TODAY).num_days();
        if delta.is_positive() {
            // Done in advance
            todo.set_advance(Some(Interval {
                value: delta as u32,
                unit: IntervalUnit::Day,
            }));
        } else {
            // Done with delay
            todo.set_delay(Some(Interval {
                value: (-delta) as u32,
                unit: IntervalUnit::Day,
            }));
        }
    }
}
