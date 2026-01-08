// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use tezos_smart_rollup_host::path::{concat, OwnedPath, Path, PathError, RefPath};

// TODO: https://gitlab.com/tezos/tezos/-/issues/7867: add the missing paths

// Instead of using directly the paths, we construct a Context object that holds the
// path to the context and does the concatenations.
// This will prevent '/tezlink/context' to appear at multiple place like '/evm/world_state'
pub struct Context {
    path: OwnedPath,
}

impl Context {
    pub fn from(root: &impl Path) -> Result<Self, PathError> {
        let context = RefPath::assert_from(b"/context");
        let path = concat(root, &context)?;
        Ok(Self { path })
    }

    #[cfg(test)]
    pub fn init_context() -> Self {
        let path = RefPath::assert_from(b"/tezlink/context");
        Self {
            path: OwnedPath::from(path),
        }
    }
}

pub mod contracts {
    use tezos_smart_rollup_host::path::{concat, OwnedPath, PathError, RefPath};

    use super::Context;

    const ROOT: RefPath = RefPath::assert_from(b"/contracts");

    const INDEX: RefPath = RefPath::assert_from(b"/index");

    const GLOBAL_COUNTER: RefPath = RefPath::assert_from(b"/global_counter");

    pub fn root(context: &Context) -> Result<OwnedPath, PathError> {
        concat(&context.path, &ROOT)
    }

    pub fn index(context: &Context) -> Result<OwnedPath, PathError> {
        concat(&root(context)?, &INDEX)
    }

    pub fn global_counter(context: &Context) -> Result<OwnedPath, PathError> {
        concat(&root(context)?, &GLOBAL_COUNTER)
    }
}
