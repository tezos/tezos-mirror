// SPDX-FileCopyrightText: 2022-2023 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT

//! Mock runtime store - the container for host state.
use std::collections::HashMap;
use tezos_smart_rollup_core::PREIMAGE_HASH_SIZE;

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub(crate) struct Store {
    pub(crate) durable: Node,
    preimages: HashMap<[u8; PREIMAGE_HASH_SIZE], Vec<u8>>,
    outbox: HashMap<u32, Vec<Vec<u8>>>,
    inbox: HashMap<u32, Vec<Vec<u8>>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub(crate) struct Node {
    pub(crate) value: Option<Vec<u8>>,
    pub(crate) inner: HashMap<String, Box<Self>>,
}

pub(crate) const VALUE_NAME: &str = "@";

impl Node {
    fn print(&self, prefix: &str, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(v) = &self.value {
            writeln!(f, "{} {}", prefix, hex::encode(v))?;
        }

        let mut keys: Vec<_> = self.inner.iter().collect();
        keys.sort_by(|(k1, _), (k2, _)| k1.cmp(k2));

        for (k, v) in keys.iter() {
            let prefix = format!("{}/{}", prefix, k);
            v.print(&prefix, f)?;
        }

        Ok(())
    }
}

impl std::fmt::Display for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.print("", f)
    }
}

impl Store {
    pub fn outbox_at(&self, level: u32) -> &[Vec<u8>] {
        self.outbox.get(&level).map(|o| o.as_ref()).unwrap_or(&[])
    }

    pub fn outbox_insert(&mut self, level: u32, message: Vec<u8>) {
        if let Some(l) = self.outbox.get_mut(&level) {
            l.push(message);
        } else {
            self.outbox.insert(level, vec![message]);
        }
    }

    pub fn node_from_path(&self, path: &str) -> Option<&Node> {
        let steps = path_steps(path);
        let mut node = &self.durable;

        for next in steps {
            node = node.inner.get(next)?;
        }

        Some(node)
    }

    pub fn node_from_path_mut(&mut self, path: &str) -> Option<&mut Node> {
        let steps = path_steps(path);
        let mut node = &mut self.durable;

        for next in steps {
            node = node.inner.get_mut(next)?;
        }

        Some(node)
    }

    pub fn node_insert(&mut self, path: &str, mut insertee: Node) {
        let steps = path_steps(path);
        let mut node = &mut self.durable;

        for next in steps {
            if !node.inner.contains_key(next) {
                node.inner.insert(next.to_string(), Box::default());
            }

            node = node.inner.get_mut(next).unwrap();
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

                node = node.inner.get_mut(*next).unwrap();
            }

            node.inner.remove(*steps.last().unwrap());

            if node != &Node::default() {
                return;
            }

            // Parent node is empty, and needs deleting
            steps.pop();
            node = &mut self.durable;
        }
    }

    pub fn get_value(&self, path: &str) -> Vec<u8> {
        self.maybe_get_value(path)
            .unwrap_or_else(|| panic!("MockRuntime: value not found at {}", path))
    }

    pub fn maybe_get_value(&self, path: &str) -> Option<Vec<u8>> {
        self.node_from_path(path).and_then(|n| n.value.clone())
    }

    pub fn set_value(&mut self, path: &str, value: Vec<u8>) {
        if let Some(mut node) = self.node_from_path_mut(path) {
            node.value = Some(value);
            node.inner.insert(VALUE_NAME.to_string(), Box::default());
        } else {
            let mut node = Node {
                inner: HashMap::default(),
                value: Some(value),
            };
            node.inner.insert(VALUE_NAME.to_string(), Box::default());
            self.node_insert(path, node);
        }
    }

    pub fn has_entry(&self, path: &str) -> bool {
        self.node_from_path(path)
            .map(|n| n.value.is_some())
            .unwrap_or(false)
    }

    pub fn add_preimage(&mut self, preimage: Vec<u8>) -> [u8; PREIMAGE_HASH_SIZE] {
        let hash_with_prefix =
            tezos_smart_rollup_encoding::dac::pages::make_preimage_hash(&preimage)
                .unwrap();

        self.preimages.insert(hash_with_prefix, preimage);
        hash_with_prefix
    }

    pub fn retrieve_preimage(&self, hash: &[u8; PREIMAGE_HASH_SIZE]) -> &[u8] {
        self.preimages
            .get(hash)
            .expect("Cannot retrieve preimage")
            .as_ref()
    }
}

fn path_steps(path: &str) -> impl Iterator<Item = &'_ str> {
    path.split('/').skip(1)
}
