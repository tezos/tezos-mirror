/******************************************************************************/
/*                                                                            */
/* SPDX-License-Identifier: MIT                                               */
/* Copyright (c) [2023] Serokell <hi@serokell.io>                             */
/*                                                                            */
/******************************************************************************/

use std::ops::{Index, IndexMut};
use std::slice::SliceIndex;

use crate::ast::*;

pub type TypeStack = Stack<Type>;
pub type IStack<'a> = Stack<TypedValue<'a>>;

/// Possibly failed type stack. Stacks are considered failed after
/// always-failing instructions. A failed stack can be unified (in terms of
/// typechecking) with any other stack.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum FailingTypeStack {
    Ok(TypeStack),
    Failed,
}

impl FailingTypeStack {
    /// Try to access a mutable reference to the underlying `TypeStack`, return
    /// `err` if the stack is failed.
    pub fn access_mut<E>(&mut self, err: E) -> Result<&mut TypeStack, E> {
        match self {
            FailingTypeStack::Ok(ok) => Ok(ok),
            FailingTypeStack::Failed => Err(err),
        }
    }
}

/// Construct a `Stack` with the given content. Note that stack top is the
/// _rightmost_ element.
#[macro_export]
macro_rules! stk {
    [$($args:tt)*] => {
        $crate::stack::TopIsLast::from(vec![$($args)*]).0
    };
}

/// Construct a `FailingTypeStack` with the given content. Note that stack top is the
/// _rightmost_ element. Called `tc_stk` as it's used in the typechecker.
#[macro_export]
macro_rules! tc_stk {
    [$($args:tt)*] => {
        $crate::stack::FailingTypeStack::Ok($crate::stack::stk![$($args)*])
    };
}

pub use {stk, tc_stk};

/// A stack abstraction based on `Vec`.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Stack<T>(Vec<T>);

impl<T> Stack<T> {
    /// Allocate a new empty stack.
    pub fn new() -> Stack<T> {
        Stack::stack_from_vec(Vec::new())
    }

    /// Construct a `Stack` from `Vec` by using the vec as the backing store
    /// directly. Last `Vec` element will end up on the top of the stack. O(1)
    /// complexity.
    fn stack_from_vec(data: Vec<T>) -> Self {
        Stack(data)
    }

    /// Convert stack index to vec index.
    fn vec_index(&self, i: usize) -> usize {
        let len = self.len();
        len.checked_sub(i + 1).expect("out of bounds stack access")
    }

    /// Push an element onto the top of the stack.
    pub fn push(&mut self, elt: T) {
        self.0.push(elt)
    }

    /// Pop an element off the top of the stack.
    pub fn pop(&mut self) -> Option<T> {
        self.0.pop()
    }

    /// Get the stack's element count.
    #[allow(clippy::len_without_is_empty)]
    pub fn len(&self) -> usize {
        self.0.len()
    }

    /// Removes the specified number of elements from the top of the stack in
    /// bulk.
    ///
    /// Panics if the `size` is larger than length of the stack.
    pub fn drop_top(&mut self, size: usize) {
        let len = self.len();
        self.0
            .truncate(len.checked_sub(size).expect("size too large in drop_top"));
    }

    /// Borrow the stack content as an immutable slice. Note that stack top is
    /// the _rightmost_ element.
    pub fn as_slice(&self) -> &[T] {
        self.0.as_slice()
    }

    /// Split off the top `size` elements of the stack into a new `Stack`.
    ///
    /// Panics if the `size` is larger than length of the stack.
    pub fn split_off(&mut self, size: usize) -> Stack<T> {
        let len = self.len();
        Self::stack_from_vec(
            self.0
                .split_off(len.checked_sub(size).expect("size too large in split_off")),
        )
    }

    /// Move elements from `other` to the top of the stack. New stack top is the
    /// top of `other`. Note that elements are moved out of `other`.
    pub fn append(&mut self, other: &mut Stack<T>) {
        self.0.append(&mut other.0)
    }

    /// Swap two elements in the stack, identified by their index from the top,
    /// with `0` being the top.
    pub fn swap(&mut self, i1: usize, i2: usize) {
        let i1v = self.vec_index(i1);
        let i2v = self.vec_index(i2);
        self.0.swap(i1v, i2v)
    }

    /// Iterator over the stack content, starting from the top.
    pub fn iter(&self) -> impl Iterator<Item = &T> {
        self.0.iter().rev()
    }
}

/// Newtype for specifying the order of elements in a `Stack` vs elements in
/// a `Vec`/slice. Used in the `From` trait for `Stack`. _First_ element of
/// the `Vec` will end up at the _top_ of the stack. `from()` conversion has
/// O(n) complexity. See also `TopIsLast<T>`.
///
/// `from_iter()` implementation is slightly inefficient, still O(n), but
/// the constant is a bit higher than necessary. If you're worried about
/// efficiency, consider using `TopIsLast` with an explicit `rev()`.
pub struct TopIsFirst<T>(pub Stack<T>);

/// Newtype for specifying the order of elements in a `Stack` vs elements in
/// a `Vec`/slice. Used in the `From` trait for `Stack`. _First_ element of
/// the `Vec` will end up at the _bottom_ of the stack. `from()` conversion
/// has O(1) complexity for vectors, O(n) for slices since those have to be
/// cloned. See also `TopIsFirst<T>`
pub struct TopIsLast<T>(pub Stack<T>);

impl<T> From<Vec<T>> for TopIsFirst<T> {
    fn from(mut data: Vec<T>) -> Self {
        data.reverse();
        Self(Stack::stack_from_vec(data))
    }
}

impl<T> From<Vec<T>> for TopIsLast<T> {
    fn from(data: Vec<T>) -> Self {
        Self(Stack::stack_from_vec(data))
    }
}

macro_rules! boilerplate {
    ($nm:tt) => {
        impl<T: Clone> From<&[T]> for $nm<T> {
            fn from(data: &[T]) -> Self {
                Vec::from(data).into()
            }
        }

        impl<T, const N: usize> From<[T; N]> for $nm<T> {
            fn from(data: [T; N]) -> Self {
                Vec::from(data).into()
            }
        }

        impl<T> FromIterator<T> for $nm<T> {
            fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
                Self::from(Vec::from_iter(iter))
            }
        }
    };
}

boilerplate!(TopIsFirst);
boilerplate!(TopIsLast);

impl<T> Default for Stack<T> {
    fn default() -> Self {
        Stack::new()
    }
}

impl<T> Index<usize> for Stack<T> {
    type Output = <usize as SliceIndex<[T]>>::Output;

    /// Index into the stack. The top's index is `0`. Returns an immutable
    /// reference to the element.
    fn index(&self, index: usize) -> &Self::Output {
        self.0.index(self.vec_index(index))
    }
}

impl<T> IndexMut<usize> for Stack<T> {
    /// Index into the stack. The top's index is `0`. Returns a mutable
    /// reference to the element.
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        let i = self.vec_index(index);
        self.0.index_mut(i)
    }
}

pub struct IntoIter<T>(std::iter::Rev<std::vec::IntoIter<T>>);

impl<T> Iterator for IntoIter<T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.0.size_hint()
    }
}

impl<T> IntoIterator for Stack<T> {
    type IntoIter = IntoIter<T>;

    type Item = T;

    fn into_iter(self) -> Self::IntoIter {
        IntoIter(self.0.into_iter().rev())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn stk_macro() {
        assert_eq!(stk![1, 2, 3, 4].as_slice(), [1, 2, 3, 4]);
        assert_eq!(stk![1; 5].as_slice(), [1, 1, 1, 1, 1]);
    }

    #[test]
    fn conversion_from_vec() {
        assert_eq!(
            TopIsLast::from(vec![1, 2, 3, 4]).0.as_slice(),
            [1, 2, 3, 4] // NB: top is right
        );
        assert_eq!(TopIsLast::from(vec![1, 2, 3, 4]).0.pop(), Some(4));
        assert_eq!(
            TopIsFirst::from(vec![1, 2, 3, 4]).0.as_slice(),
            [4, 3, 2, 1] // NB: top is right
        );
        assert_eq!(TopIsFirst::from(vec![1, 2, 3, 4]).0.pop(), Some(1));
    }

    #[test]
    fn conversion_from_slice() {
        let s: &[u32] = &[1, 2, 3, 4];
        assert_eq!(
            TopIsLast::from(s).0.as_slice(),
            [1, 2, 3, 4] // NB: top is right
        );
        assert_eq!(
            TopIsFirst::from(s).0.as_slice(),
            [4, 3, 2, 1] // NB: top is right
        );
    }

    #[test]
    fn conversion_from_array() {
        let a: [u32; 4] = [1, 2, 3, 4];
        assert_eq!(
            TopIsLast::from(a).0.as_slice(),
            [1, 2, 3, 4] // NB: top is right
        );
        assert_eq!(
            TopIsFirst::from(a).0.as_slice(),
            [4, 3, 2, 1] // NB: top is right
        );
    }

    #[test]
    fn conversion_from_iter() {
        let it = 1..=4;
        assert_eq!(
            TopIsLast::from_iter(it.clone()).0.as_slice(),
            [1, 2, 3, 4] // NB: top is right
        );
        assert_eq!(
            TopIsFirst::from_iter(it).0.as_slice(),
            [4, 3, 2, 1] // NB: top is right
        );
    }

    #[test]
    fn push() {
        let mut stk = Stack::new();
        stk.push(1);
        stk.push(2);
        stk.push(3);
        assert_eq!(stk, stk![1, 2, 3]);
    }

    #[test]
    fn pop() {
        let mut stk = stk![1, 2, 3];
        assert_eq!(stk.pop(), Some(3));
        assert_eq!(stk.pop(), Some(2));
        assert_eq!(stk.pop(), Some(1));
        assert_eq!(stk.pop(), None);
    }

    #[test]
    fn len() {
        let mut stk = stk![1, 2, 3];
        assert_eq!(stk.len(), 3);
        stk.push(42);
        assert_eq!(stk.len(), 4);
    }

    #[test]
    fn drop_top() {
        let mut stk = stk![1, 2, 3, 4];
        stk.drop_top(3);
        assert_eq!(stk, stk![1]);
    }

    #[test]
    #[should_panic(expected = "size too large in drop_top")]
    fn drop_top_out_of_bounds() {
        let mut stk = stk![1, 2, 3, 4];
        stk.drop_top(42);
    }

    #[test]
    fn as_slice() {
        let stk = stk![1, 2, 3, 4];
        assert!(matches!(stk.as_slice(), [1, 2, 3, 4]));
    }

    #[test]
    fn split_off() {
        let mut stk = stk![1, 2, 3, 4, 5];
        let stk2 = stk.split_off(3);
        assert_eq!(stk2, stk![3, 4, 5]);
        assert_eq!(stk, stk![1, 2]);
    }

    #[test]
    #[should_panic(expected = "size too large in split_off")]
    fn split_off_out_of_bounds() {
        let mut stk = stk![1, 2, 3, 4, 5];
        stk.split_off(42);
    }

    #[test]
    fn append() {
        let mut stk1 = stk![1, 2, 3];
        let mut stk2 = stk![4, 5];
        stk1.append(&mut stk2);
        assert_eq!(stk1, stk![1, 2, 3, 4, 5]);
        assert_eq!(stk2, stk![]);
    }

    #[test]
    fn index() {
        let stk = stk![1, 2, 3, 4, 5];
        assert_eq!(stk[0], 5);
        assert_eq!(stk[4], 1);
    }

    #[test]
    #[should_panic(expected = "out of bounds stack access")]
    fn index_out_of_bounds() {
        let stk = stk![1, 2, 3, 4, 5];
        assert_eq!(stk[7], 5); // panics
    }

    #[test]
    fn index_mut() {
        let mut stk = stk![1, 2, 3, 4, 5];
        stk[2] = 42;
        assert_eq!(stk, stk![1, 2, 42, 4, 5]);
    }

    #[test]
    fn default() {
        assert_eq!(Stack::<()>::default(), stk![]);
    }
}
