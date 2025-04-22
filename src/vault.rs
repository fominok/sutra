//! Notes storage access.
use std::{
    borrow::Cow,
    cmp,
    fmt::{self, Display},
    fs::{self, OpenOptions},
    io::Write,
    path::{Path, PathBuf},
};

use anyhow::{Context, Result};
use chrono::{Datelike, Month, NaiveDate};
use glob::glob;
use typed_path::PlatformEncoding;

use crate::TODAY;

const DATE_FMT: &str = "%Y-%m-%d";

/// Note files storage.
#[derive(Debug)]
pub(crate) struct Vault {
    path: PathBuf,
}

impl Vault {
    /// Create a new vault instance.
    pub(crate) fn new(path: PathBuf) -> Self {
        Self { path }
    }

    /// Iterate over all vault files ordered by date and followed by non dated
    /// notes.
    pub(crate) fn grouped_notes(
        &self,
    ) -> Result<(Vec<NamedFileIdentifier>, Vec<DatedFileIdentifier>)> {
        let mut named = Vec::new();
        let mut dated = Vec::new();

        for id in glob_notes(&self.path, GlobPattern::All)
            .map(|r| r.and_then(FileIdentifier::strip_from_path))
        {
            let id = id?;
            match id.0 {
                FileIdentifierVariant::Named(n) => named.push(n),
                FileIdentifierVariant::Dated(d) => dated.push(d),
            }
        }

        Ok((named, dated))
    }

    /// Iterate over vault files for a specified month ordered by date.
    pub(crate) fn iter_month_notes(
        &self,
        month: Month,
    ) -> impl Iterator<Item = Result<DatedFileIdentifier>> {
        glob_notes(&self.path, GlobPattern::Month(month)).filter_map(|r| {
            r.and_then(FileIdentifier::strip_from_path)
                .map(|id| id.into_dated())
                .transpose()
        })
    }

    pub(crate) fn write(&self, id: &FileIdentifier, data: &[u8]) -> Result<()> {
        let mut file = OpenOptions::new()
            .write(true)
            .create(true)
            .truncate(true)
            .open(id.to_path(self))?;

        file.write_all(data)?;

        Ok(())
    }

    pub(crate) fn read_string(&self, id: &FileIdentifier) -> Result<String> {
        let path = id.to_path(self);
        if path.is_file() {
            fs::read_to_string(path).context("unable to read file contents")
        } else {
            Ok("".to_owned())
        }
    }
}

enum GlobPattern {
    All,
    Month(Month),
}

impl GlobPattern {
    fn as_string(&self) -> String {
        match self {
            GlobPattern::All => "*".to_owned(),
            GlobPattern::Month(month) => {
                let year = if TODAY.month() < *month as u32 {
                    // In this case talking about the previous year
                    TODAY.year() - 1
                } else {
                    TODAY.year()
                };

                format!("{year}-{:02}-*", *month as u32 + 1)
            }
        }
    }
}

fn glob_notes(path: &Path, pattern: GlobPattern) -> impl Iterator<Item = Result<PathBuf>> {
    let pattern_str = pattern.as_string();
    let t_path = typed_path::PathBuf::<PlatformEncoding>::from(path.as_os_str().as_encoded_bytes());
    let unix_path_string = if t_path.has_unix_encoding() {
        path.to_string_lossy()
    } else {
        let unix_path = t_path.with_unix_encoding();
        Cow::Owned(unix_path.to_string_lossy().into_owned())
    };

    glob(&format!("{unix_path_string}/{pattern_str}.md"))
        .expect("we don't expect malformed patterns")
        .map(|gr| gr.context("glob error"))
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct FileIdentifier(FileIdentifierVariant);

impl FileIdentifier {
    pub(crate) fn new_named(name: String) -> Self {
        Self(
            if let Some(date) = NaiveDate::parse_from_str(&name, DATE_FMT).ok() {
                FileIdentifierVariant::Dated(DatedFileIdentifier(date))
            } else {
                FileIdentifierVariant::Named(NamedFileIdentifier(name))
            },
        )
    }

    pub(crate) fn new_dated(date: NaiveDate) -> Self {
        Self(FileIdentifierVariant::Dated(DatedFileIdentifier(date)))
    }

    pub(crate) fn inner(&self) -> &FileIdentifierVariant {
        &self.0
    }

    fn to_path(&self, vault: &Vault) -> PathBuf {
        let name: Cow<'_, _> = match &self.0 {
            FileIdentifierVariant::Named(NamedFileIdentifier(name)) => name.into(),
            FileIdentifierVariant::Dated(DatedFileIdentifier(date)) => {
                date.format(DATE_FMT).to_string().into()
            }
        };
        let mut path = vault.path.join(AsRef::<str>::as_ref(&name));
        path.set_extension("md");
        path
    }

    fn strip_from_path(path: PathBuf) -> Result<Self> {
        Ok(FileIdentifier::new_named(
            path.file_stem()
                .context("bad file name")?
                .to_string_lossy()
                .into_owned(),
        ))
    }
}

impl PartialOrd for FileIdentifier {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        self.0.partial_cmp(&other.0)
    }
}

impl Ord for FileIdentifier {
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        self.partial_cmp(other).expect("total order")
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) enum FileIdentifierVariant {
    Named(NamedFileIdentifier),
    Dated(DatedFileIdentifier),
}

impl FileIdentifier {
    pub(crate) fn into_dated(self) -> Option<DatedFileIdentifier> {
        match self.0 {
            FileIdentifierVariant::Named(_) => None,
            FileIdentifierVariant::Dated(id) => Some(id),
        }
    }

    pub(crate) fn into_named(self) -> Option<NamedFileIdentifier> {
        match self.0 {
            FileIdentifierVariant::Named(id) => Some(id),
            FileIdentifierVariant::Dated(_) => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub(crate) struct NamedFileIdentifier(String);

impl Display for NamedFileIdentifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.0)
    }
}

impl From<NamedFileIdentifier> for FileIdentifier {
    fn from(value: NamedFileIdentifier) -> Self {
        Self(FileIdentifierVariant::Named(value))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct DatedFileIdentifier(pub(crate) NaiveDate);

impl From<DatedFileIdentifier> for FileIdentifier {
    fn from(value: DatedFileIdentifier) -> Self {
        Self(FileIdentifierVariant::Dated(value))
    }
}

impl PartialOrd for FileIdentifierVariant {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        Some(match (self, other) {
            (Self::Dated(_), Self::Named(_)) => cmp::Ordering::Less,
            (Self::Dated(ld), Self::Dated(rd)) => ld.0.cmp(&rd.0),
            (Self::Named(_), Self::Dated(_)) => cmp::Ordering::Greater,
            (Self::Named(ln), Self::Named(rn)) => ln.0.cmp(&rn.0),
        })
    }
}

impl Ord for FileIdentifierVariant {
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        self.partial_cmp(other).expect("total order")
    }
}
