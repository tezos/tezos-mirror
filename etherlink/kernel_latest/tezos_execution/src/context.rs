// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use tezos_smart_rollup_host::path::{OwnedPath, RefPath};

// TODO: https://gitlab.com/tezos/tezos/-/issues/7867: add the missing paths

// This path should be the only one to refers to '/tezlink/context'
const CONTEXT_PATH: RefPath = RefPath::assert_from(b"/tezlink/context");

// Instead of using directly the paths, we construct a Context object that holds the
// path to the context and does the concatenations.
// This will prevent '/tezlink/context' to appear at multiple place like '/evm/world_state'
pub struct Context {
    path: OwnedPath,
}

impl Context {
    #[allow(dead_code)]
    pub fn init_context() -> Self {
        Context {
            path: CONTEXT_PATH.into(),
        }
    }
}

pub mod contracts {
    use tezos_smart_rollup_host::path::{concat, OwnedPath, PathError, RefPath};

    use super::Context;

    #[allow(dead_code)]
    const ROOT: RefPath = RefPath::assert_from(b"/contracts");

    #[allow(dead_code)]
    const INDEX: RefPath = RefPath::assert_from(b"/index");

    #[allow(dead_code)]
    const GLOBAL_COUNTER: RefPath = RefPath::assert_from(b"/global_counter");

    pub fn root(context: &Context) -> Result<OwnedPath, PathError> {
        concat(&context.path, &ROOT)
    }

    #[allow(dead_code)]
    pub fn index(context: &Context) -> Result<OwnedPath, PathError> {
        concat(&root(context)?, &INDEX)
    }

    #[allow(dead_code)]
    pub fn global_counter(context: &Context) -> Result<OwnedPath, PathError> {
        concat(&root(context)?, &GLOBAL_COUNTER)
    }
}
