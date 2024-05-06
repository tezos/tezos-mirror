// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use ethers::prelude::Http;
use ethers::providers::{Middleware, Provider};
use ethers::types::Bytes;
use url::Url;

use crate::error::BundlerError;

#[derive(Clone)]
pub struct BundlerClient {
    provider: Provider<Http>,
}

impl BundlerClient {
    pub fn new(sequencer_url: Url) -> Self {
        Self {
            provider: Provider::new(Http::new(sequencer_url)),
        }
    }

    pub async fn bundle_raw_transaction(
        &self,
        raw_transaction: &[u8],
    ) -> Result<[u8; 32], BundlerError> {
        // TODO: send transaction requests to the bundler runner
        // Bundler runner will make sure the encrypted transaction is applied and re-send it
        // in case there's a problem with nonce or others.
        let res = self
            .provider
            .send_raw_transaction(Bytes::from(raw_transaction.to_vec()))
            .await?;

        Ok(res.tx_hash().0)
    }
}
