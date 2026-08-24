// SPDX-FileCopyrightText: [2023] Serokell <hi@serokell.io>
//
// SPDX-License-Identifier: MIT

//! Representation for typed Michelson `list 'a` values.

use rpds::Vector;

use crate::ast::{RcTypedValue, TypedValue};

/// A representation of a Michelson list.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct MichelsonList<T>(Vector<T>);

impl<'a> MichelsonList<RcTypedValue<'a>> {
    /// Remove an element from the start of the list. O(log n); the returned
    /// handle is a refcount bump, never a payload copy.
    pub fn uncons(&mut self) -> Option<RcTypedValue<'a>> {
        let res = self.0.last().cloned();
        self.0.drop_last_mut();
        res
    }
}

impl<T> MichelsonList<T> {
    /// Construct a new empty list.
    pub fn new() -> Self {
        MichelsonList(Vector::new())
    }

    /// Add an element to the start of the list.
    pub fn cons(&mut self, x: T) {
        self.0.push_back_mut(x)
    }

    /// Get the list length, i.e. the number of elements.
    #[allow(clippy::len_without_is_empty)]
    pub fn len(&self) -> usize {
        self.0.len()
    }

    /// Construct an iterator over references to the list elements.
    pub fn iter(&self) -> Iter<'_, T> {
        // delegate to `impl IntoIterator for &MichelsonList`
        self.into_iter()
    }

    /// Consume the list and yield the elements it exclusively owned, leaving
    /// shared elements and the subtrees holding them untouched.
    ///
    /// This is what the iterative `Drop` needs: a list that another value still
    /// holds has no dying element, and a list produced by mutating a shared one
    /// owns only the handful of nodes it had to copy. Draining with [Self::uncons]
    /// instead visits all n elements and copy-on-writes the spine of each,
    /// which is work proportional to the length of a list nobody is really
    /// freeing. The order in which elements are yielded is unspecified.
    pub(crate) fn drain_owned(self) -> impl Iterator<Item = T> {
        self.0.drain_owned()
    }
}

impl<T> Default for MichelsonList<T> {
    fn default() -> Self {
        Self::new()
    }
}

/// Owning iterator for [MichelsonList]. Holds the list and unconses one
/// element per step, so nothing is materialised up front and each yielded
/// handle is released by the list as it is handed out.
pub struct IntoIter<'a>(MichelsonList<RcTypedValue<'a>>);

impl<'a> Iterator for IntoIter<'a> {
    type Item = RcTypedValue<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.uncons()
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let n = self.0.len();
        (n, Some(n))
    }
}

impl ExactSizeIterator for IntoIter<'_> {}

/// Non-owning iterator for [MichelsonList].
//
// NB: `rpds::vector::Iter` is parameterised by the shared-pointer kind, and
// `RcK` lives in `archery`, which is not one of our dependencies. Naming the
// associated type instead pins the very same iterator (`Vector<T>` defaults to
// `RcK`) without pulling `archery` into `Cargo.toml`.
pub struct Iter<'a, T>(std::iter::Rev<<&'a Vector<T> as IntoIterator>::IntoIter>);

impl<'a, T> Iterator for Iter<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.0.size_hint()
    }
}

impl<T> ExactSizeIterator for Iter<'_, T> {}

/// Iterating from the tail is free: the backing `Vec` is already stored
/// tail-first, so [Iter] is a `Rev` over it. Lets a caller that must queue the
/// elements onto a LIFO worklist push them back-to-front without collecting
/// them first (see [`TypedValue::update_big_maps`]).
impl<T> DoubleEndedIterator for Iter<'_, T> {
    fn next_back(&mut self) -> Option<Self::Item> {
        self.0.next_back()
    }
}

impl<'a> IntoIterator for MichelsonList<RcTypedValue<'a>> {
    type IntoIter = IntoIter<'a>;
    type Item = RcTypedValue<'a>;
    fn into_iter(self) -> Self::IntoIter {
        IntoIter(self)
    }
}

impl<'a, T> IntoIterator for &'a MichelsonList<T> {
    type IntoIter = Iter<'a, T>;
    type Item = &'a T;
    fn into_iter(self) -> Self::IntoIter {
        Iter(self.0.iter().rev())
    }
}

/// Construct a `MichelsonList<T>` from `Vec<T>`. O(n).
impl<T> From<Vec<T>> for MichelsonList<T> {
    fn from(value: Vec<T>) -> Self {
        MichelsonList(value.into_iter().rev().collect())
    }
}

/// Construct a `MichelsonList<RcTypedValue>` from `Vec<TypedValue>`. O(n).
impl<'a> From<Vec<TypedValue<'a>>> for MichelsonList<RcTypedValue<'a>> {
    fn from(mut value: Vec<TypedValue<'a>>) -> Self {
        value.reverse();
        MichelsonList(value.into_iter().map(RcTypedValue::new).collect())
    }
}

/// Construct a `MichelsonList<T>` from an iterator. O(n).
impl<T> FromIterator<T> for MichelsonList<T> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        MichelsonList::from(Vec::from_iter(iter))
    }
}

/// Construct a `MichelsonList<RcTypedValue>` from an iterator over
/// `TypedValue`. O(n).
impl<'a> FromIterator<TypedValue<'a>> for MichelsonList<RcTypedValue<'a>> {
    fn from_iter<I: IntoIterator<Item = TypedValue<'a>>>(iter: I) -> Self {
        MichelsonList::from(Vec::from_iter(iter))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn cons() {
        let mut lst = MichelsonList::new();
        let expected = vec![1, 2, 3].into();
        lst.cons(3);
        lst.cons(2);
        lst.cons(1);
        assert_eq!(lst, expected);
    }

    #[test]
    fn len() {
        assert_eq!(MichelsonList::<i32>::from_iter(1..=42).len(), 42);
    }

    #[test]
    fn uncons() {
        let mut lst = MichelsonList::from(ints(1..=3));
        assert_eq!(lst.uncons(), Some(RcTypedValue::new(TypedValue::int(1))));
        assert_eq!(lst.uncons(), Some(RcTypedValue::new(TypedValue::int(2))));
        assert_eq!(lst.uncons(), Some(RcTypedValue::new(TypedValue::int(3))));
        assert_eq!(lst.uncons(), None);
    }

    #[test]
    fn into_iter() {
        let lst = MichelsonList::from(ints(1..=3));
        assert_eq!(
            lst.into_iter().collect::<Vec<_>>(),
            ints(1..=3)
                .into_iter()
                .map(RcTypedValue::new)
                .collect::<Vec<_>>()
        );

        // The list drops each element as it hands it out, so the caller gets
        // sole ownership.
        for elt in MichelsonList::from(ints(1..=3)) {
            assert_eq!(elt.strong_count(), 1);
        }
    }

    #[test]
    fn from_iter() {
        assert_eq!(
            MichelsonList::<i32>::from_iter(1..=3),
            MichelsonList::<i32>::from(vec![1, 2, 3])
        );
    }

    #[test]
    fn default() {
        assert_eq!(MichelsonList::default(), MichelsonList::<()>::new());
    }

    #[test]
    fn drain_owned_yields_every_element_of_a_sole_owner() {
        let list = MichelsonList::<RcTypedValue>::from_iter(ints(0..N));
        assert_eq!(list.drain_owned().count(), N as usize);
    }

    #[test]
    fn drain_owned_yields_nothing_of_a_shared_list() {
        let list = MichelsonList::<RcTypedValue>::from_iter(ints(0..N));

        // Required for the test, even if list isn't used, we still need to clone
        // to show shared.drain_owned does not traverse anything
        #[allow(clippy::redundant_clone)]
        let shared = list.clone();

        // Nothing is dying: `list` still holds every element.
        assert_eq!(shared.drain_owned().count(), 0);
    }

    /// The drop-path property this whole design exists for: the transient a
    /// mutating instruction leaves behind (`DUP; CONS`, and the same shape for
    /// `UPDATE`) owns only what it had to copy, so draining it costs a handful
    /// of steps rather than one per element. Before `drain_owned` this walk
    /// visited all `N` of them — work proportional to a list nobody was
    /// freeing, against the flat price of `CONS`.
    #[test]
    fn drain_owned_of_a_transient_does_not_scale_with_length() {
        let list = MichelsonList::from_iter(ints(0..N));

        let mut transient = list.clone();
        transient.cons(RcTypedValue::new(TypedValue::int(-1)));

        // Only the freshly consed element is exclusively the transient's; the
        // rest are still held by `list`. In particular this must not grow with
        // `N` — a leaf holds at most 32 elements, so the bound is the size of
        // the path `cons` copied, not the length of the list.
        let drained = transient.drain_owned().count();
        assert!(drained <= 32, "drained {drained} elements, expected O(1)");
        assert_eq!(list.len(), N as usize);
    }

    /// Long enough that a per-element walk is unmistakable against an
    /// ownership-bounded one.
    const N: i32 = 10_000;

    /// The lists here hold `RcTypedValue`, so the fixtures are `Int` values
    /// rather than bare integers.
    fn ints(range: impl IntoIterator<Item = i32>) -> Vec<TypedValue<'static>> {
        range.into_iter().map(TypedValue::int).collect()
    }
}
