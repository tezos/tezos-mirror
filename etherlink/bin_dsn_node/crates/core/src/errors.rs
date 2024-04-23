// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use thiserror::Error;

// TODO: Merge error and errors modules together
// TODO: Move to separate crate

#[derive(Debug, Error)]
pub enum Error {
    #[error("Host component missing in URI")]
    UriHostMissing,

    #[error("Port component missing in URI")]
    UriPortMissing,

    #[error("Received SIGINT")]
    SigInt,

    #[error("Received SIGTERM")]
    SigTerm,
}
