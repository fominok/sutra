use std::io::{Write, stdout};

use crossterm::{
    ExecutableCommand,
    cursor::{MoveTo, MoveToColumn, MoveUp},
    event::{KeyCode, KeyModifiers},
    terminal::{Clear, ClearType},
};

const TERM_ERR_MSG: &str = "Terminal error";

pub(crate) trait PromptSelect {
    fn variants() -> impl Iterator<Item = (char, &'static str, Self)>;
}

pub(crate) fn run<T: PromptSelect + Clone>(prompt: &str) -> Option<T> {
    let variants: Vec<_> = T::variants().collect();

    crossterm::terminal::enable_raw_mode().expect(TERM_ERR_MSG);

    print!("{prompt}: ");

    let col = crossterm::cursor::position().expect(TERM_ERR_MSG).0;

    print!("\n\r");

    for (c, s, _) in variants.iter() {
        println!("[{c}] {s}\r");
    }

    let row = crossterm::cursor::position().expect(TERM_ERR_MSG).1 - variants.len() as u16 - 1;

    let result: Option<&(char, &str, T)> = loop {
        match crossterm::event::read()
            .expect(TERM_ERR_MSG)
            .as_key_press_event()
        {
            Some(event) => match (event.code, event.modifiers) {
                (KeyCode::Esc, _) => break None,
                (KeyCode::Char('c') | KeyCode::Char('d'), KeyModifiers::CONTROL) => {
                    break None;
                }
                (KeyCode::Char(ch), KeyModifiers::NONE) => {
                    if let Some(found) = variants.iter().find(|(c, ..)| *c == ch) {
                        break Some(found);
                    }
                }
                _ => {}
            },
            None => {}
        }
    };

    let mut stdout = stdout();

    for _ in 0..variants.len() {
        stdout
            .execute(MoveUp(1))
            .and_then(|t| t.execute(MoveToColumn(0)))
            .and_then(|t| t.execute(Clear(ClearType::CurrentLine)))
            .expect(TERM_ERR_MSG);
    }

    if let Some((_, s, _)) = result {
        stdout.execute(MoveTo(col, row)).expect(TERM_ERR_MSG);
        write!(stdout, "{s}\n\r").expect(TERM_ERR_MSG);
    }

    crossterm::terminal::disable_raw_mode().expect(TERM_ERR_MSG);

    result.map(|(_, _, v)| v.clone())
}
