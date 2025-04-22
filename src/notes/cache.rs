use std::{
    cell::RefCell,
    collections::{HashMap, hash_map::Entry},
};

use anyhow::Result;
use comrak::{Arena, nodes::AstNode};

use super::{Todo, note::Note};
use crate::{
    md,
    vault::{FileIdentifier, Vault},
};

/// In-memory storage of processed notes that are possibly uncommitted.
pub(crate) struct NotesCache<'a> {
    md_arena: &'a Arena<AstNode<'a>>,
    notes_arena: &'a Arena<RefCell<Note<'a>>>,
    todos_arena: &'a Arena<Todo<'a>>,
    notes_index: HashMap<FileIdentifier, &'a RefCell<Note<'a>>>,
    vault: &'a Vault,
}

impl<'a> NotesCache<'a> {
    pub(crate) fn new(
        md_arena: &'a Arena<AstNode<'a>>,
        notes_arena: &'a Arena<RefCell<Note<'a>>>,
        todos_arena: &'a Arena<Todo<'a>>,
        vault: &'a Vault,
    ) -> Self {
        Self {
            md_arena,
            notes_arena,
            todos_arena,
            vault,
            notes_index: Default::default(),
        }
    }

    pub(crate) fn get_md_arena(&self) -> &'a Arena<AstNode<'a>> {
        self.md_arena
    }

    pub(crate) fn get_todos_arena(&self) -> &'a Arena<Todo<'a>> {
        self.todos_arena
    }

    pub(crate) fn persist_notes(&self) -> Result<()> {
        for (id, note) in self.notes_index.iter() {
            self.vault.write(id, &note.borrow().as_bytes())?;
        }

        Ok(())
    }

    pub(crate) fn get_note(&mut self, id: FileIdentifier) -> Result<&'a RefCell<Note<'a>>> {
        match self.notes_index.entry(id) {
            Entry::Occupied(entry) => Ok(entry.into_mut()),
            Entry::Vacant(entry) => {
                let contents = self.vault.read_string(entry.key())?;

                let note = Note::parse(
                    &self.md_arena,
                    &self.todos_arena,
                    md::parse_str(self.md_arena, &contents),
                )
                .unwrap_or_else(|| Note::new(&self.md_arena));

                let note_ref = self.notes_arena.alloc(note.into());

                Ok(entry.insert_entry(note_ref).into_mut())
            }
        }
    }
}
