// SPDX-FileCopyrightText: [2023] Serokell <hi@serokell.io>
//
// SPDX-License-Identifier: MIT

//! Representation for typed Michelson `list 'a` values.

use std::rc::Rc;

use rpds::Vector;

/// A representation of a Michelson list.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct MichelsonList<T>(Vector<T>);

impl<T> MichelsonList<Rc<T>> {
    /// Remove an element from the start of the list. O(log n); the returned
    /// `Rc` is a refcount bump, never a payload copy.
    pub fn uncons(&mut self) -> Option<Rc<T>> {
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
}

impl<T> Default for MichelsonList<T> {
    fn default() -> Self {
        Self::new()
    }
}

/// Owning iterator for [MichelsonList]. Holds the list and unconses one
/// element per step, so nothing is materialised up front and each yielded
/// `Rc` is released by the list as it is handed out.
pub struct IntoIter<T>(MichelsonList<Rc<T>>);

impl<T> Iterator for IntoIter<T> {
    type Item = Rc<T>;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.uncons()
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let n = self.0.len();
        (n, Some(n))
    }
}

impl<T> ExactSizeIterator for IntoIter<T> {}

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
/// them first (see [`crate::ast::TypedValue::update_big_maps`]).
impl<T> DoubleEndedIterator for Iter<'_, T> {
    fn next_back(&mut self) -> Option<Self::Item> {
        self.0.next_back()
    }
}

impl<T> IntoIterator for MichelsonList<Rc<T>> {
    type IntoIter = IntoIter<T>;
    type Item = Rc<T>;
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

/// Construct a `MichelsonList<Rc<T>>` from `Vec<T>`. O(n).
impl<T> From<Vec<T>> for MichelsonList<std::rc::Rc<T>> {
    fn from(mut value: Vec<T>) -> Self {
        value.reverse();
        MichelsonList(value.into_iter().map(std::rc::Rc::new).collect())
    }
}

/// Construct a `MichelsonList<T>` from an iterator. O(n).
impl<T> FromIterator<T> for MichelsonList<T> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        MichelsonList::from(Vec::from_iter(iter))
    }
}

/// Construct a `MichelsonList<Rc<T>>` from an iterator. O(n).
impl<T> FromIterator<T> for MichelsonList<std::rc::Rc<T>> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
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
        let mut lst = MichelsonList::<Rc<i32>>::from(vec![1, 2, 3]);
        assert_eq!(lst.uncons(), Some(Rc::new(1)));
        assert_eq!(lst.uncons(), Some(Rc::new(2)));
        assert_eq!(lst.uncons(), Some(Rc::new(3)));
        assert_eq!(lst.uncons(), None);
    }

    #[test]
    fn into_iter() {
        let lst = MichelsonList::<Rc<i32>>::from(vec![1, 2, 3]);
        assert_eq!(
            lst.into_iter().collect::<Vec<_>>(),
            vec![Rc::new(1), Rc::new(2), Rc::new(3)]
        );

        // The list drops each element as it hands it out, so the caller gets
        // sole ownership.
        for elt in MichelsonList::<Rc<i32>>::from(vec![1, 2, 3]) {
            assert_eq!(Rc::strong_count(&elt), 1);
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
}
