use std::collections::BTreeMap;

use anyhow::Result;
use chrono::{Datelike, NaiveDate};
use ratatui::{
    DefaultTerminal, Frame,
    buffer::Buffer,
    crossterm::event::{self, Event, KeyCode, KeyEvent, KeyEventKind},
    layout::Rect,
    style::Stylize,
    symbols::border,
    text::{Line, Text},
    widgets::{Block, Borders, Paragraph, ScrollbarState, StatefulWidget, Widget},
};

use crate::TODAY;

pub(crate) fn show_agenda() {
    let mut terminal = ratatui::init();

    let result = App::new().run(&mut terminal);

    ratatui::restore();

    result.expect("TUI error");
}

struct App {
    exit: bool,
    counter: usize,
}

impl App {
    fn new() -> Self {
        App {
            exit: false,
            counter: 0,
        }
    }

    fn run(&mut self, terminal: &mut DefaultTerminal) -> Result<()> {
        while !self.exit {
            terminal.draw(|frame| self.draw(frame))?;
            self.handle_events()?;
        }
        Ok(())
    }

    fn draw(&self, frame: &mut Frame) {
        frame.render_widget(self, frame.area());
    }

    fn handle_events(&mut self) -> Result<()> {
        match event::read()? {
            // it's important to check that the event is a key press event as
            // crossterm also emits key release and repeat events on Windows.
            Event::Key(key_event) if key_event.kind == KeyEventKind::Press => {
                self.handle_key_event(key_event)
            }
            _ => {}
        };
        Ok(())
    }

    fn handle_key_event(&mut self, key_event: KeyEvent) {
        match key_event.code {
            KeyCode::Char('q') => self.exit(),
            _ => {}
        }
    }

    fn exit(&mut self) {
        self.exit = true;
    }
}

impl Widget for &App {
    fn render(self, area: Rect, buf: &mut Buffer) {
        let title = Line::from(format!("{} {}", TODAY.format("%A, %B"), TODAY.day()));
        let instructions = Line::from(vec![
            "Up ".into(),
            "↑ /k/<C-p>".blue().bold(),
            " | ".bold(),
            "Down ".into(),
            "↓ /j/<C-n>".blue().bold(),
            " | ".bold(),
            "Done ".into(),
            "<Space>".blue().bold(),
            " | ".bold(),
            "Skip ".into(),
            "s".blue().bold(),
            " | ".bold(),
            "Complete recurrence ".into(),
            "c".blue().bold(),
            " | ".bold(),
            "Quit ".into(),
            "q ".blue().bold(),
        ]);
        let block = Block::bordered()
            .title(title.centered())
            .title_bottom(instructions.centered())
            .borders(Borders::TOP | Borders::BOTTOM);

        Paragraph::new(Text::from("lol"))
            .centered()
            .block(block)
            .render(area, buf);
    }
}

struct AgendaWidget {
    items: BTreeMap<NaiveDate, Vec<String>>,
}

struct AgendaWidgetState {
    line: usize,
}

impl StatefulWidget for AgendaWidget {
    type State = AgendaWidgetState;

    fn render(self, area: Rect, buf: &mut Buffer, state: &mut Self::State) {
        // let paragraph = Paragraph::new(items.clone())
        //     .scroll((vertical_scroll as u16, 0))
        //     .block(Block::new().borders(Borders::RIGHT)); // to show a background for the scrollbar

        // let scrollbar = Scrollbar::new(ScrollbarOrientation::VerticalRight)
        //     .begin_symbol(Some("↑"))
        //     .end_symbol(Some("↓"));
    }
}
