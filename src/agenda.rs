mod scroll;

use std::path::PathBuf;

use anyhow::Result;
use comrak::Arena;
pub(crate) use scroll::TodoWithContext;
use scroll::{EdgeCasesPrompt, scroll_events};

use crate::{notes::NotesCache, ui, vault::Vault};

pub(crate) fn agenda_screen(path: PathBuf) -> Result<()> {
    let vault = Vault::new(path);
    let md_arena = Arena::new();
    let notes_arena = Arena::new();
    let headings_arena = Arena::new();

    let mut cache = NotesCache::new(&md_arena, &notes_arena, &headings_arena, &vault);

    let (named, dated) = vault.grouped_notes()?;

    let prompt = EdgeCasesPrompt::default();

    let view = scroll_events(&mut cache, &prompt, named, dated)?;

    ui::show_agenda(view.iter_dated_todos(), view.iter_named_todos());

    cache.persist_notes()?;

    Ok(())
}
