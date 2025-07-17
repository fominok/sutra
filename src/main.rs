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

// use tracing_appender::rolling::RollingFileAppender;
// use tracing_subscriber::{Registry, layer::SubscriberExt};

const TODAY: LazyCell<NaiveDate> = LazyCell::new(|| Local::now().date_naive());

fn main() -> Result<()> {
    // let file_appender: RollingFileAppender =
    // tracing_appender::rolling::daily("logs", "trace.log");

    // let file_layer = tracing_subscriber::fmt::layer()
    //     .with_writer(file_appender)
    //     .with_ansi(false)
    //     .with_span_events(
    //         tracing_subscriber::fmt::format::FmtSpan::NEW
    //             | tracing_subscriber::fmt::format::FmtSpan::CLOSE,
    //     );

    // let subscriber = Registry::default().with(file_layer);
    // tracing::subscriber::set_global_default(subscriber).expect("setting tracing
    // default failed");

    let cli = Args::parse();

    let default_path = util::make_path("~/Documents/notes")?;

    match cli.command {
        Some(Command::Agenda) | None => agenda::agenda_screen(default_path)?,
    }

    Ok(())
}
