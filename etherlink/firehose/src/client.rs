// SPDX-FileCopyrightText: 2024 Trilitech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Calling out to the etherlink node.
//!
//! We use subset of the ethereum RPCs needed for firehose to operate.

use anyhow::Result;
use ethers::prelude::*;
use ethers::providers::{Http, Middleware, Provider};
use ethers::types::U256;

use crate::config::Config;

/// Client for etherlink RPCs
#[derive(Debug)]
pub struct Client {
    client: SignerMiddleware<Provider<Http>, LocalWallet>,
    controller: LocalWallet,
}

impl Client {
    pub fn new(config: &Config) -> Result<Self> {
        let controller = config.controller();
        let provider =
            Provider::<Http>::try_from(config.endpoint())?.with_signer(controller.clone());

        Ok(Self {
            client: provider,
            controller,
        })
    }

    pub async fn gas_price(&self) -> Result<U256> {
        let price = self.client.get_gas_price().await?;
        Ok(price)
    }

    pub async fn controller_balance(&self) -> Result<U256> {
        let balance = self
            .client
            .get_balance(self.controller.address(), None)
            .await?;
        Ok(balance)
    }

    pub async fn chain_id(&self) -> Result<U256> {
        let chain_id = self.client.get_chainid().await?;
        Ok(chain_id)
    }

    pub fn controller_address(&self) -> H160 {
        self.controller.address()
    }
}
