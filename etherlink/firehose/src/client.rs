// SPDX-FileCopyrightText: 2024 Trilitech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Calling out to the etherlink node.
//!
//! We use subset of the ethereum RPCs needed for firehose to operate.

use anyhow::Result;
use ethers::core::types::TransactionRequest;
use ethers::prelude::*;
use ethers::providers::{Http, Middleware, Provider};
use ethers::types::transaction::eip2718::TypedTransaction;
use ethers::types::U256;
use tokio::try_join;

use std::time::Duration;

use crate::config::Config;

const DEFAULT_POLL_INTERVAL: Duration = Duration::from_millis(200);
const DEFAULT_CONFIRMATIONS: usize = 1;

/// Client for etherlink RPCs
#[derive(Debug)]
pub struct Client {
    client: Provider<Http>,
    controller: LocalWallet,
    chain_id: u64,
}

impl Client {
    pub async fn new(config: &Config) -> Result<Self> {
        let provider =
            Provider::<Http>::try_from(config.endpoint())?.interval(DEFAULT_POLL_INTERVAL);

        let chain_id = provider.get_chainid().await?.as_u64();

        let controller = config.controller().with_chain_id(chain_id);

        Ok(Self {
            client: provider,
            controller,
            chain_id,
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

    pub fn chain_id(&self) -> u64 {
        self.chain_id
    }

    pub fn controller_address(&self) -> H160 {
        self.controller.address()
    }

    pub async fn nonce(&self, account: H160) -> Result<U256> {
        let nonce = self.client.get_transaction_count(account, None).await?;
        Ok(nonce)
    }

    pub async fn controller_xtz_transfer(&self, to: H160, amount: U256) -> Result<()> {
        let receipt = self.transfer_xtz(&self.controller, to, amount).await?;
        println!("{receipt:#?}");
        Ok(())
    }

    async fn transfer_xtz(
        &self,
        from_wallet: &LocalWallet,
        to: H160,
        amount: U256,
    ) -> Result<TransactionReceipt> {
        let from = from_wallet.address();

        let (gas_price, nonce) = try_join!(self.gas_price(), self.nonce(from))?;

        let mut tx: TypedTransaction = TransactionRequest::new()
            .from(from)
            .to(to)
            .value(amount)
            .gas_price(gas_price)
            .nonce(nonce)
            .chain_id(self.chain_id)
            .into();

        let gas_limit = self.client.estimate_gas(&tx, None).await?;
        tx.set_gas(gas_limit);

        let sig = from_wallet.sign_transaction_sync(&tx)?;
        let raw_tx = tx.rlp_signed(&sig);

        let receipt = self
            .client
            .send_raw_transaction(raw_tx)
            .await?
            .confirmations(DEFAULT_CONFIRMATIONS)
            .await?;

        receipt.ok_or_else(|| anyhow::anyhow!("failed to gather receipt"))
    }
}
