// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Module for tree utils like types, traversals.
//!
//! All the traversals implemented in this module should be the same to maintain consistency,
//! which is required for serialisation / deserialisation

use itertools::Itertools;

/// Generic tree structure used to model the [`super::proof::MerkleProof`],
/// as well as the full & partial shapes of a [`super::merkle::MerkleTree`].
#[derive(Clone, Debug, PartialEq)]
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

/// Intermediary either-like type for implementing [`Tree::modify_shape`]
#[derive(Clone)]
pub enum ModifyResult<T, L> {
    /// Current subtree should be replaced with a node containing the given children.
    /// and traversal should continue recursively.
    NodeContinue(Vec<T>),
    /// Current subtree is replaced by a leaf containing the given data.
    LeafStop(L),
}

impl<A> Tree<A> {
    /// Modify the shape of a given [`Tree`].
    ///
    /// Provide a `modify` function which is called for every subtree in a pre-order DFS traversal.
    /// The function returns a [`ModifyResult::LeafStop`] if the subtree will become a leaf
    /// or a [`ModifyResult::NodeContinue`] if the subtree will become a non-leaf node
    /// and will be further traversed by this algorithm.
    pub fn modify_shape<B, E, F: FnMut(Tree<A>) -> Result<ModifyResult<Tree<A>, B>, E>>(
        self,
        modify: &mut F,
    ) -> Result<Tree<B>, E> {
        match modify(self)? {
            // TODO: RV-290 Replace with an iterative algorithm to not overflow the stack
            ModifyResult::LeafStop(new_data) => Ok(Tree::Leaf(new_data)),
            ModifyResult::NodeContinue(children) => Ok(Tree::Node(
                children
                    .into_iter()
                    .map(|c| c.modify_shape(modify))
                    .try_collect()?,
            )),
        }
    }

    /// Perform a shape-preserving map operation over a [`Tree`].
    ///
    /// Note: The traversal order should correspond with the one in [`Tree::modify_shape`] and [`tree_iterator`]
    pub fn map<B, E, F: FnMut(A) -> Result<B, E>>(self, map: &mut F) -> Result<Tree<B>, E> {
        // TODO: RV-290 Change to non-recursive implementation to avoid stackoverflow
        let tree = match self {
            Tree::Leaf(data) => Tree::Leaf(map(data)?),
            Tree::Node(children) => Tree::Node(
                children
                    .into_iter()
                    .map(|child| child.map(map))
                    .try_collect()?,
            ),
        };
        Ok(tree)
    }
}
