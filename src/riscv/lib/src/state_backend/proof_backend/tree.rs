// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Module for tree utils like types, traversals.
//!
//! All the traversals implemented in this module should be the same to maintain consistency,
//! which is required for serialisation / deserialisation

/// Generic tree structure used to model the [`super::proof::MerkleProof`],
/// as well as the full & partial shapes of a [`super::merkle::MerkleTree`].
#[derive(Clone, Debug)]
pub enum Tree<A> {
    Node(Vec<Self>),
    Leaf(A),
}

impl<A> Tree<A> {
    /// Iterates over all subtrees in a [`Tree`] in a pre-order DFS traversal.
    pub fn subtree_iterator(&self) -> impl Iterator<Item = &Tree<A>> {
        let mut stack = vec![self];

        std::iter::from_fn(move || {
            let subtree = stack.pop()?;

            if let Tree::Node(children) = subtree {
                stack.extend(children.iter().rev());
            }

            Some(subtree)
        })
    }
}
