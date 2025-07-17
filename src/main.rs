mod agenda;
mod cli;
mod md;
mod notes;
mod ui;
mod util;
mod vault;

use std::cell::LazyCell;

use anyhow::Result;
use chrono::{Local, NaiveDate};
use clap::Parser as _;
use cli::{Args, Command};

const TODAY: LazyCell<NaiveDate> = LazyCell::new(|| Local::now().date_naive());

fn main() -> Result<()> {
    let cli = Args::parse();

    let default_path = util::make_path("~/Documents/notes")?;

    match cli.command {
        Some(Command::Agenda) | None => agenda::agenda_screen(default_path)?,
    }

    Ok(())
}
