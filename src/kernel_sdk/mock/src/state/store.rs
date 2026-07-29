// SPDX-FileCopyrightText: 2022-2023 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

//! Mock runtime store - the durable key-value tree.
use std::collections::BTreeMap;
use std::rc::Rc;
use tezos_smart_rollup_core::STORE_HASH_SIZE;

/// The durable key-value tree backed by irmin in the real PVM.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub(crate) struct Store {
    pub(crate) durable: Rc<Node>,
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub(crate) struct Node {
    pub(crate) value: Option<Rc<Vec<u8>>>,
    /// Ordered by step, so that iterating a node is deterministic.
    pub(crate) inner: Rc<BTreeMap<String, Rc<Self>>>,
}

pub(crate) const VALUE_NAME: &str = "@";

impl Node {
    fn print(&self, prefix: &str, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(v) = &self.value {
            writeln!(f, "{} {}", prefix, hex::encode(v.as_ref()))?;
        }

        for (k, v) in self.inner.iter() {
            let prefix = format!("{prefix}/{k}");
            v.print(&prefix, f)?;
        }

        Ok(())
    }

    /// Hash of the subtree rooted at this node: a fold over its value and its
    /// children, taken in step order.
    ///
    /// Not memoised, so each call is linear in the size of the subtree.
    pub(crate) fn subtree_hash(&self) -> [u8; STORE_HASH_SIZE] {
        let mut preimage = Vec::new();

        // Tagged, so an absent value cannot hash like a present empty one.
        match &self.value {
            Some(value) => {
                preimage.push(1);
                preimage.extend_from_slice(&(value.len() as u64).to_le_bytes());
                preimage.extend_from_slice(value);
            }
            None => preimage.push(0),
        }

        preimage.extend_from_slice(&(self.inner.len() as u64).to_le_bytes());
        for (step, child) in self.inner.iter() {
            // Length-prefixed, so steps ("a", "bc") cannot hash as ("ab", "c").
            preimage.extend_from_slice(&(step.len() as u64).to_le_bytes());
            preimage.extend_from_slice(step.as_bytes());
            preimage.extend_from_slice(&child.subtree_hash());
        }

        tezos_crypto_rs::blake2b::digest_256(&preimage)
    }
}

impl std::fmt::Display for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.print("", f)
    }
}

impl Store {
    pub fn node_from_path(&self, path: &str) -> Option<&Rc<Node>> {
        let steps = path_steps(path);
        let mut node = &self.durable;

        for next in steps {
            node = node.inner.get(next)?;
        }

        Some(node)
    }

    pub fn node_from_path_mut(&mut self, path: &str) -> Option<&mut Node> {
        let steps = path_steps(path);
        let mut node = Rc::make_mut(&mut self.durable);

        for next in steps {
            let inner = Rc::make_mut(&mut node.inner);
            let inner = inner.get_mut(next)?;
            node = Rc::make_mut(inner);
        }

        Some(node)
    }

    pub fn node_insert(&mut self, path: &str, mut insertee: Rc<Node>) {
        let steps = path_steps(path);
        let mut node = &mut self.durable;

        for next in steps {
            if !node.inner.contains_key(next) {
                Rc::make_mut(&mut Rc::make_mut(node).inner)
                    .insert(next.to_string(), Default::default());
            }

            node = Rc::make_mut(&mut Rc::make_mut(node).inner)
                .get_mut(next)
                .unwrap();
        }

        std::mem::swap(node, &mut insertee);
    }

    pub fn node_delete(&mut self, path: &str) {
        let steps = path.split('/').skip(1);
        let mut node = &mut self.durable;
        let mut steps = steps.collect::<Vec<_>>();

        while !steps.is_empty() {
            for i in 0..(steps.len() - 1) {
                let next = steps.get(i).unwrap();

                if !node.inner.contains_key(*next) {
                    return;
                }

                node = Rc::make_mut(&mut Rc::make_mut(node).inner)
                    .get_mut(*next)
                    .unwrap();
            }

            Rc::make_mut(&mut Rc::make_mut(node).inner).remove(*steps.last().unwrap());

            if node.as_ref() != &Node::default() {
                return;
            }

            // Parent node is empty, and needs deleting
            steps.pop();
            node = &mut self.durable;
        }
    }

    pub fn get_value(&self, path: &str) -> &Vec<u8> {
        self.maybe_get_value(path)
            .unwrap_or_else(|| panic!("MockRuntime: value not found at {path}"))
    }

    pub fn maybe_get_value(&self, path: &str) -> Option<&Rc<Vec<u8>>> {
        self.node_from_path(path).and_then(|n| n.value.as_ref())
    }

    pub fn set_value(&mut self, path: &str, value: Vec<u8>) {
        if let Some(node) = self.node_from_path_mut(path) {
            node.value = Some(Rc::new(value));
            Rc::make_mut(&mut node.inner)
                .insert(VALUE_NAME.to_string(), Default::default());
        } else {
            let mut node = Node {
                inner: Default::default(),
                value: Some(Rc::new(value)),
            };
            Rc::make_mut(&mut node.inner)
                .insert(VALUE_NAME.to_string(), Default::default());
            self.node_insert(path, Rc::new(node));
        }
    }

    pub fn delete_value(&mut self, path: &str) {
        if let Some(node) = self.node_from_path_mut(path) {
            node.value = None;
            Rc::make_mut(&mut node.inner).remove(VALUE_NAME);
        };
    }

    pub fn has_entry(&self, path: &str) -> bool {
        self.node_from_path(path)
            .map(|n| n.value.is_some())
            .unwrap_or(false)
    }
}

fn path_steps(path: &str) -> impl Iterator<Item = &'_ str> {
    path.split('/').skip(1)
}
