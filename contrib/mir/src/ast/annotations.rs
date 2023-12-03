/******************************************************************************/
/*                                                                            */
/* SPDX-License-Identifier: MIT                                               */
/* Copyright (c) [2023] Serokell <hi@serokell.io>                             */
/*                                                                            */
/******************************************************************************/

use std::borrow::Cow;

use crate::lexer::Annotation;

#[derive(Clone, Eq, PartialEq)]
pub struct Annotations<'a>(Vec<Annotation<'a>>);

pub const NO_ANNS: Annotations = Annotations::new();

#[derive(Debug, Clone, Eq, PartialEq, thiserror::Error)]
pub enum AnnotationError {
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

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct FieldAnnotation<'a>(Cow<'a, str>);

impl<'a> FieldAnnotation<'a> {
    pub fn as_str(&self) -> &str {
        &self.0
    }

    pub fn into_cow(self) -> Cow<'a, str> {
        self.0
    }

    #[cfg(test)]
    pub fn from_str_unchecked(s: &'a str) -> Self {
        FieldAnnotation(Cow::Borrowed(s))
    }
}

impl<'a> Annotations<'a> {
    pub const fn new() -> Self {
        Annotations(Vec::new())
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn iter(&self) -> impl Iterator<Item = &Annotation> {
        self.0.iter()
    }

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
