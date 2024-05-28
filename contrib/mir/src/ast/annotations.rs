/******************************************************************************/
/*                                                                            */
/* SPDX-License-Identifier: MIT                                               */
/* Copyright (c) [2023] Serokell <hi@serokell.io>                             */
/*                                                                            */
/******************************************************************************/

//! Tezos annotations on a [Micheline][crate::ast::Micheline] nodes and
//! utilities for working with them.

use std::borrow::Cow;

/// A single Micheline annotation. Annotations are optionally-owned, meaning
/// they should use references when feasible, but can use owned heap-allocated
/// values when necessary.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Annotation<'a> {
    /// Special annotation, i.e. `@%`, `@%%` or `%@` verbatim.
    Special(Cow<'a, str>),
    /// Field annotation, e.g. `%foo`. The inner value does not contain the
    /// leading `%`.
    Field(Cow<'a, str>),
    /// Variable annotation, e.g. `@foo`. The inner value does not contain the
    /// leading `@`.
    Variable(Cow<'a, str>),
    /// Type annotation, e.g. `:foo`. The inner value does not contain the
    /// leading `:`.
    Type(Cow<'a, str>),
}

impl Annotation<'_> {
    /// Convert the inner value of [Annotation] to an owned [String].
    pub fn into_owned(self) -> Annotation<'static> {
        match self {
            Annotation::Special(s) => Annotation::Special(Cow::Owned(s.into_owned())),
            Annotation::Field(s) => Annotation::Field(Cow::Owned(s.into_owned())),
            Annotation::Variable(s) => Annotation::Variable(Cow::Owned(s.into_owned())),
            Annotation::Type(s) => Annotation::Type(Cow::Owned(s.into_owned())),
        }
    }
}

impl std::fmt::Display for Annotation<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Annotation::Special(s) => write!(f, "{s}"),
            Annotation::Field(s) => write!(f, "%{s}"),
            Annotation::Variable(s) => write!(f, "@{s}"),
            Annotation::Type(s) => write!(f, ":{s}"),
        }
    }
}

/// A structure holding all Tezos annotations on a [Micheline][crate::ast::Micheline] node.
#[derive(Clone, Eq, PartialEq)]
pub struct Annotations<'a>(Vec<Annotation<'a>>);

/// Constant corresponding to no annotations.
pub const NO_ANNS: Annotations = Annotations::new();

/// Errors that can happen when working with [Annotations].
#[derive(Debug, Clone, Eq, PartialEq, thiserror::Error)]
pub enum AnnotationError {
    /// Expected at most one field annotation, but found multiple.
    #[error("unexpected second field annotation: {0}")]
    TooManyFieldAnns(String),
}

impl Default for Annotations<'_> {
    fn default() -> Self {
        Self::new()
    }
}

impl std::fmt::Debug for Annotations<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

/// A newtype wrapping a field annotation, like `%foo`. This newtype is used to
/// enforce some invariants on the type level. It's impossible to construct
/// manually, except in tests.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct FieldAnnotation<'a>(Cow<'a, str>);

impl<'a> FieldAnnotation<'a> {
    /// View the field annotation contents as a [str] slice. The leading `%` is
    /// _not_ included.
    pub fn as_str(&self) -> &str {
        &self.0
    }

    /// Convert the field annotation into the inner [`Cow`]`<'a, str>`.
    pub fn into_cow(self) -> Cow<'a, str> {
        self.0
    }

    #[cfg(test)]
    pub fn from_str_unchecked(s: &'a str) -> Self {
        FieldAnnotation(Cow::Borrowed(s))
    }
}

impl<'a> Annotations<'a> {
    /// Create a new list empty of annotations.
    pub const fn new() -> Self {
        Annotations(Vec::new())
    }

    /// Check if the list of annotations is empty.
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    /// Get the number of annotations in the list.
    pub fn len(&self) -> usize {
        self.0.len()
    }

    /// Returns an iterator over the list of annotaions.
    pub fn iter(&self) -> impl Iterator<Item = &Annotation> {
        self.0.iter()
    }

    /// Get at most one field annotation from the list. If there aren't any
    /// field annotations in the list, returns `Ok(None)`. If there is exactly
    /// one field annotation, returns `Ok(Some(field_annotation))`. If there are
    /// more than one, returns `Err(`[`AnnotationError::TooManyFieldAnns`]`)`
    pub fn get_single_field_ann(&self) -> Result<Option<FieldAnnotation<'a>>, AnnotationError> {
        use Annotation::*;
        let mut res = None;
        for i in &self.0 {
            match i {
                Special(..) | Type(..) | Variable(..) => (),
                Field(s) => {
                    if res.is_none() {
                        res = Option::Some(FieldAnnotation(s.clone()));
                    } else {
                        return Err(AnnotationError::TooManyFieldAnns(s.to_string()));
                    }
                }
            }
        }
        Ok(res)
    }
}

impl<'a, T> From<T> for Annotations<'a>
where
    T: AsRef<[Annotation<'a>]>,
{
    fn from(x: T) -> Self {
        Annotations(Vec::from(x.as_ref()))
    }
}

impl<'a> FromIterator<Annotation<'a>> for Annotations<'a> {
    fn from_iter<T: IntoIterator<Item = Annotation<'a>>>(iter: T) -> Self {
        Annotations(Vec::from_iter(iter))
    }
}

/// Returns all annotations in the same order in which they were given
/// initially.
impl<'a> IntoIterator for &'a Annotations<'a> {
    type Item = &'a Annotation<'a>;
    type IntoIter = std::slice::Iter<'a, Annotation<'a>>;
    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}
