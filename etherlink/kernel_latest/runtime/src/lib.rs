// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

pub mod extensions;
pub mod runtime;
pub mod safe_storage;
// TODO: This will be soon removed whenever the first transition will be made for
// a given keyspace root.
// As soon as the snapshot module will be made public, remove the dead code
// annotation.
#[allow(dead_code)]
mod snapshot;
