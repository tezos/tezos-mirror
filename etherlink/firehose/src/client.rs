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
use futures::Future;
use tokio::try_join;

use std::sync::Arc;
use std::time::Duration;

use crate::config::Config;

const DEFAULT_POLL_INTERVAL: Duration = Duration::from_millis(100);
const DEFAULT_CONFIRMATIONS: usize = 0;

/// Client for etherlink RPCs
#[derive(Debug)]
pub struct Client {
    client: Arc<SignerMiddleware<Provider<Http>, LocalWallet>>,
    pub(crate) controller: LocalWallet,
    chain_id: u64,
}

impl Client {
    pub async fn new(config: &Config) -> Result<Self> {
        let provider =
            Provider::<Http>::try_from(config.endpoint())?.interval(DEFAULT_POLL_INTERVAL);

        let chain_id = provider.get_chainid().await?.as_u64();
        let controller = config.controller().with_chain_id(chain_id);

        let provider = provider.with_signer(controller.clone());

        Ok(Self {
            client: provider.into(),
            controller,
            chain_id,
        })
    }

    pub fn provider(&self) -> Arc<impl Middleware> {
        self.client.clone()
    }

    pub async fn gas_price(&self) -> Result<U256> {
        let price = self.client.get_gas_price().await?;
        Ok(price)
    }

    pub async fn controller_balance(&self) -> Result<U256> {
        let balance = self.balance(self.controller.address()).await?;
        Ok(balance)
    }

    pub async fn balance(&self, address: H160) -> Result<U256> {
        let balance = self.client.get_balance(address, None).await?;
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
        let receipt = self
            .transfer_xtz(&self.controller, to, amount, |address| self.nonce(address))
            .await?;
        println!(
            "{to} funded with {amount} | hash: {:?}",
            receipt.transaction_hash
        );
        Ok(())
    }

    pub async fn transfer_xtz<T: Future<Output = Result<U256>>>(
        &self,
        from_wallet: &LocalWallet,
        to: H160,
        amount: U256,
        get_nonce: impl Fn(H160) -> T,
    ) -> Result<TransactionReceipt> {
        self.send(from_wallet, Some(to), Some(amount), None, get_nonce)
            .await
    }

    pub async fn transfer_all_xtz(
        &self,
        from_wallet: &LocalWallet,
        to: H160,
    ) -> Result<(TransactionReceipt, U256)> {
        let from = from_wallet.address();

        let (gas_price, nonce, balance) =
            try_join!(self.gas_price(), self.nonce(from), self.balance(from))?;

        if balance.is_zero() {
            anyhow::bail!("Account {from} contains no XTZ");
        }

        let mut tx: TypedTransaction = TransactionRequest::new()
            .from(from)
            .to(to)
            .value(U256::zero())
            .gas_price(gas_price)
            .nonce(nonce)
            .chain_id(self.chain_id)
            .into();

        let gas_limit = self.client.estimate_gas(&tx, None).await?;
        tx.set_gas(gas_limit);

        let fees = gas_limit * gas_price;
        if fees >= balance {
            anyhow::bail!("Account {from} balance {balance} too low to pay for fees {fees}");
        }

        tx.set_value(balance - fees);

        let sig = from_wallet.sign_transaction_sync(&tx)?;
        let raw_tx = tx.rlp_signed(&sig);

        let receipt = self
            .client
            .send_raw_transaction(raw_tx)
            .await?
            .confirmations(DEFAULT_CONFIRMATIONS)
            .await?
            .ok_or_else(|| anyhow::anyhow!("failed to gather receipt"))?;

        Ok((receipt, balance - fees))
    }

    pub async fn send<T: Future<Output = Result<U256>>>(
        &self,
        from_wallet: &LocalWallet,
        to: Option<H160>,
        value: Option<U256>,
        data: Option<Bytes>,
        get_nonce: impl Fn(H160) -> T,
    ) -> Result<TransactionReceipt> {
        let from = from_wallet.address();

        let (gas_price, nonce) = try_join!(self.gas_price(), get_nonce(from))?;

        let mut tx: TypedTransaction = TransactionRequest::new()
            .from(from)
            .gas_price(gas_price)
            .nonce(nonce)
            .chain_id(self.chain_id)
            .into();

        if let Some(to) = to {
            tx.set_to(to);
        }
        if let Some(value) = value {
            tx.set_value(value);
        }
        if let Some(data) = data {
            tx.set_data(data);
        }

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

    pub async fn call(
        &self,
        to: Option<H160>,
        value: Option<U256>,
        data: Option<Bytes>,
    ) -> Result<Bytes> {
        let gas_price = self.gas_price().await?;

        let mut tx: TypedTransaction = TransactionRequest::new()
            .gas_price(gas_price)
            .chain_id(self.chain_id)
            .to(to.unwrap_or_default())
            .value(value.unwrap_or_default())
            .into();

        if let Some(data) = data {
            tx.set_data(data);
        }

        let bytes = self.client.call(&tx, None).await?;

        Ok(bytes)
    }
}
