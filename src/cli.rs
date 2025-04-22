/// Text-based organizer
#[derive(clap::Parser, Debug)]
#[command(version = "0.1", about = "Text based organizer via Markdown", long_about = None)]
pub(super) struct Args {
    #[command(subcommand)]
    pub command: Option<Command>,
}

#[derive(Debug, clap::Subcommand)]
pub(super) enum Command {
    /// Manage todos collected across note files at one place
    Agenda,
}
