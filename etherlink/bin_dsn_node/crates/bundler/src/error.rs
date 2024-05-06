// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use ethers::providers::ProviderError;

#[derive(Debug, thiserror::Error)]
pub enum BundlerError {
    #[error("Http provider error: {0}")]
    Provider(#[from] ProviderError),
}
