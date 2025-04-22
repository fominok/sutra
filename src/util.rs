use std::{iter::Peekable, path::PathBuf};

use anyhow::Result;

pub(crate) fn make_path(s: &str) -> Result<PathBuf> {
    let expanded = shellexpand::full(s)?.into_owned();

    Ok(PathBuf::from(expanded))
}

pub(crate) trait PeekExt {
    type Item;
    type Iter: Iterator<Item = Self::Item>;
    type TakeUntil<'s, F>
    where
        Self::Iter: 's,
        F: Fn(&Self::Item) -> bool;

    /// Skips values  until criteria match
    fn scroll_until(&mut self, f: impl Fn(&Self::Item) -> bool) -> Option<Self::Item>;

    /// Return iterator of values until the criteria match.
    /// This alters the original iterator without consuming it.
    fn take_until<F: Fn(&Self::Item) -> bool>(
        &mut self,
        f: F,
    ) -> TakeUntil<Self::Iter, Self::Item, F>;
}

pub(crate) struct TakeUntil<'s, I: Iterator<Item = IT>, IT, F: Fn(&IT) -> bool> {
    iter: &'s mut Peekable<I>,
    f: F,
}

impl<'s, I: Iterator<Item = IT>, IT, F: Fn(&IT) -> bool> Iterator for TakeUntil<'s, I, IT, F> {
    type Item = IT;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next_if(|n| !(self.f)(n))
    }
}

impl<IT, I: Iterator<Item = IT>> PeekExt for Peekable<I> {
    type Item = IT;
    type Iter = I;
    type TakeUntil<'s, F>
        = TakeUntil<'s, Self, IT, F>
    where
        I: 's,
        F: Fn(&Self::Item) -> bool;

    fn scroll_until(&mut self, f: impl Fn(&Self::Item) -> bool) -> Option<Self::Item> {
        while let Some(_) = self.next_if(|n| !f(n)) {}

        self.next()
    }

    fn take_until<F: Fn(&Self::Item) -> bool>(
        &mut self,
        f: F,
    ) -> TakeUntil<Self::Iter, Self::Item, F> {
        TakeUntil { iter: self, f }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn scroll_until() {
        let mut iter = (0..10).peekable();
        assert!(matches!(iter.scroll_until(|n| *n == 5), Some(5)));
        assert!(matches!(iter.scroll_until(|n| *n == 11), None));
    }

    #[test]
    fn take_until() {
        let mut iter = (0..10).peekable();

        iter.scroll_until(|n| *n == 5);

        assert_eq!(iter.take_until(|n| *n == 8).collect::<Vec<_>>(), vec![6, 7]);
    }
}
