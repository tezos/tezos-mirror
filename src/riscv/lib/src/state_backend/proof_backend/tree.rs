// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Module for tree utils like types, traversals.
//!
//! All the traversals implemented in this module should be the same to maintain consistency,
//! which is required for serialisation / deserialisation

use std::convert::Infallible;

/// Generic tree structure used to model the [`super::proof::MerkleProof`],
/// as well as the full & partial shapes of a [`super::merkle::MerkleTree`].
#[derive(Clone, Debug, PartialEq)]
pub enum Tree<A> {
    Node(Vec<Self>),
    Leaf(A),
}

/// Used in [`impl_modify_map_collect`]
impl<D> From<D> for Tree<D> {
    fn from(value: D) -> Self {
        Tree::Leaf(value)
    }
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

    /// Modify the shape of a given [`Tree`].
    ///
    /// Provide a `modify` function which is called for every subtree in a pre-order DFS traversal.
    /// The function returns a [`ModifyResult::LeafStop`] if the subtree will become a leaf
    /// or a [`ModifyResult::NodeContinue`] if the subtree will become a non-leaf node
    /// and will be further traversed by this algorithm.
    pub fn modify_shape<B, E, F: FnMut(Tree<A>) -> Result<ModifyResult<(), Tree<A>, B>, E>>(
        self,
        modify: &mut F,
    ) -> Result<Tree<B>, E> {
        // map & collect are "default"
        impl_modify_map_collect(
            self,
            modify,
            |data| Ok(data),
            |(), children| Ok(Tree::Node(children)),
        )
    }

    /// Perform a shape-preserving map operation over a [`Tree`].
    ///
    /// Note: The traversal order should correspond with the one in [`Tree::modify_shape`] and [`Tree::subtree_iterator`].
    pub fn map<B, E, F: FnMut(A) -> Result<B, E>>(self, map: &mut F) -> Result<Tree<B>, E> {
        // modify & collect are "default"
        impl_modify_map_collect(
            self,
            |subtree| match subtree {
                Tree::Node(vec) => Ok(ModifyResult::NodeContinue((), vec)),
                Tree::Leaf(data) => Ok(ModifyResult::LeafStop(data)),
            },
            map,
            |(), children| Ok(Tree::Node(children)),
        )
    }

    /// Borrows each leaf in the tree.
    pub fn each_ref(&self) -> Tree<&A> {
        impl_modify_map_collect::<_, _, _, Infallible, _, _, _, _, _>(
            self,
            |subtree| match subtree {
                Tree::Node(vec) => Ok(ModifyResult::NodeContinue(
                    (),
                    // Obtain references to each sub tree.
                    vec.iter().collect::<Vec<_>>(),
                )),
                Tree::Leaf(data) => Ok(ModifyResult::LeafStop(data)),
            },
            Ok,
            |(), children| Ok(Tree::Node(children)),
        )
        .unwrap()
    }
}

/// Intermediary either-like type for implementing [`impl_modify_map_collect`]
#[derive(Clone)]
pub enum ModifyResult<D, N, L> {
    /// Current subtree should be replaced with a node containing the given children,
    /// and an auxiliary data for extra context if needed.
    /// Traversal should continue recursively.
    NodeContinue(D, Vec<N>),
    /// Current subtree is replaced by a leaf containing the given data.
    LeafStop(L),
}

/// Perform generic modify_map_collect
///
/// This is done in 3 steps while traversing the tree in a pre-order DFS traversal:
/// 1. Apply `modify` on current subtree: This operation changes the structure of the current
///    subtree before traversing its children.
/// 2. When encountering leaves, `map` is called to transform a leaf from `A` to `B` type.
///    This is done on children of subtrees which have been traversed after `modify` was called.
/// 3. After modifying & mapping the children of a node, the `collect` method gathers the newly
///    modified & mapped subtrees to create the new subtree.
pub fn impl_modify_map_collect<
    InterimLeafData, // InterimLeafData -> FinalLeafData  when applying `map`
    FinalLeafData,   // [FinalLeafData] -> FinalLeafData when applying `collect`
    AuxTreeData,     // Type of auxiliary data held for a subtree
    Err,             // Error type when applying `modify` / `map` / `collect`
    InputTree,
    OutputTree: From<FinalLeafData>,
    TreeModifier: FnMut(InputTree) -> Result<ModifyResult<AuxTreeData, InputTree, InterimLeafData>, Err>,
    LeafMapper: FnMut(InterimLeafData) -> Result<FinalLeafData, Err>,
    Collector: FnMut(AuxTreeData, Vec<OutputTree>) -> Result<OutputTree, Err>,
>(
    root: InputTree,
    mut modify: TreeModifier,
    mut map: LeafMapper,
    mut collect: Collector,
) -> Result<OutputTree, Err> {
    enum ProcessEvents<ProcessEvent, CollectAuxTreeData> {
        Node(ProcessEvent),
        Collect(CollectAuxTreeData, usize),
    }

    let mut process = vec![ProcessEvents::Node(root)];
    let mut done: Vec<OutputTree> = vec![];

    while let Some(event) = process.pop() {
        match event {
            ProcessEvents::Node(subtree) => match modify(subtree)? {
                ModifyResult::LeafStop(data) => {
                    // Instead of pushing a single leaf process on the Process-queue,
                    // map the data and append it directly to the Done-queue
                    done.push(OutputTree::from(map(data)?));
                }
                ModifyResult::NodeContinue(node_data, children) => {
                    // the only case where we push further process events in the process queue
                    // We have to first push a collect event to know how many children should be collected when forming back the current subtree
                    process.push(ProcessEvents::Collect(node_data, children.len()));

                    process.extend(
                        children
                            .into_iter()
                            .rev()
                            .map(|child| ProcessEvents::Node(child)),
                    );
                }
            },
            ProcessEvents::Collect(node_data, count) => {
                // We need to reconstruct a subtree which is made of `count` children
                // No panic: We are guaranted count < done.len() since every Collect(size)
                // corresponds to size nodes pushed to Done-queue
                let children = done.split_off(done.len() - count);
                done.push(collect(node_data, children)?);
            }
        }
    }

    // No Panic: We only add a single node as root at the beginning of the algorithm
    // which corresponds to this last node in the Done-queue
    let new_root = done.pop().unwrap();

    debug_assert!(done.is_empty());

    Ok(new_root)
}
