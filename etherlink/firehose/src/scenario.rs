// SPDX-FileCopyrightText: 2024 Trilitech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Scenarios for various different stress-tests that can be performed.

use anyhow::Result;
use ethers::prelude::*;
use futures::stream::FuturesUnordered;
use futures::StreamExt;
use tokio::time::Instant;

use std::future::Future;
use std::pin::Pin;

use crate::client::Client;
use crate::config::Config;
use crate::contracts::ERC20;

pub struct Setup<'a> {
    client: &'a Client,
    workers: Vec<(H160, LocalWallet)>,
    erc20: Option<H160>,
}

impl<'a> Setup<'a> {
    pub fn new(config: &'a Config, client: &'a Client, workers: usize) -> Result<Self> {
        if config.workers().len() < workers {
            anyhow::bail!(
                "Required {workers}, configuration contains {}",
                config.workers().len()
            );
        }

        let workers = config
            .workers()
            .iter()
            .take(workers)
            .cloned()
            .map(Into::into)
            .map(|worker: LocalWallet| (worker.address(), worker))
            .collect();

        Ok(Self {
            client,
            workers,
            erc20: config.erc20,
        })
    }

    pub async fn fund_workers_xtz(&self, amount: U256) -> Result<()> {
        println!("[SCENARIO] fund workers (xtz) with {amount}");

        for (worker, _) in self.workers.iter() {
            let balance = self.client.balance(*worker).await?;

            if balance >= amount {
                println!("{worker} already funded with {balance}");
                continue;
            }

            self.client.controller_xtz_transfer(*worker, amount).await?;
        }

        println!("[SCENARIO] fund workers (xtz) done");
        Ok(())
    }

    pub async fn fund_workers_erc20(&self, amount: U256) -> Result<()> {
        println!("[SCENARIO] fund workers (erc20) with {amount}");

        for (worker, account) in self.workers.iter() {
            let balance = self.balance_erc20(*worker).await?;

            if balance >= amount {
                println!("{worker} already funded with {balance}");
                continue;
            }

            self.mint_erc20(account, amount).await?;
        }

        println!("[SCENARIO] fund workers (erc20) done");
        Ok(())
    }

    /// Perform xtz transfers from the workers back to the controller account,
    /// aiming to match the requested tps.
    ///
    /// Each transfer is only 1 wei, by default.
    pub async fn xtz_transfers<T: Future<Output = Result<U256>>>(
        &self,
        get_nonce: impl Fn(H160) -> T + std::marker::Copy,
        payload_size: usize,
    ) -> Result<()> {
        println!(
            "[SCENARIO] xtz transfers with {} workers",
            self.workers.len()
        );

        let to = self.client.controller_address();

        let mut tasks = FuturesUnordered::<
            Pin<
                Box<
                    dyn Future<
                        Output = std::result::Result<
                            (TransactionReceipt, LocalWallet),
                            LocalWallet,
                        >,
                    >,
                >,
            >,
        >::new();

        macro_rules! queue_transfer {
            ($worker:ident) => {
                let task = Box::pin(async move {
                    let size = payload_size;
                    let mut vec: Vec<u8> = Vec::with_capacity(size);
                    vec.resize(size, 1);
                    let payload = Some(Bytes::from(vec));
                    let receipt = self
                        .client
                        .send(&$worker, Some(to), Some(U256::one()), payload, get_nonce)
                        .await
                        .map_err(|_| $worker.clone())?;
                    Ok((receipt, $worker))
                });
                tasks.push(task);
            };
        }

        for (_, worker) in self.workers.iter().cloned() {
            queue_transfer!(worker);
        }

        let mut total_failed = 0;
        let mut total_transfers = 0 as f32;
        let start_time = Instant::now();
        let mut gas_price = None;

        while let Some(result) = tasks.next().await {
            let worker = match result {
                Ok((receipt, worker)) => {
                    gas_price = receipt.effective_gas_price;
                    worker
                }
                Err(worker) => {
                    total_failed += 1;
                    worker
                }
            };
            total_transfers += 1.0;
            let tps = total_transfers / Instant::now().duration_since(start_time).as_secs_f32();

            print!("\rTPS {tps}\t\tfailed {total_failed}/{total_transfers} total | gas_price: {gas_price:?}\t\t\t");

            queue_transfer!(worker);
        }

        Ok(())
    }

    /// Perform erc20 transfers from the workers back to the controller account,
    /// aiming to match the requested tps.
    ///
    /// Each transfer is only 1 token, by default.
    pub async fn erc20_transfers(&self) -> Result<()> {
        println!(
            "[SCENARIO] erc20 transfers with {} workers",
            self.workers.len()
        );

        let to = self.client.controller_address();

        let mut tasks = FuturesUnordered::<
            Pin<
                Box<
                    dyn Future<
                        Output = std::result::Result<
                            (TransactionReceipt, LocalWallet),
                            LocalWallet,
                        >,
                    >,
                >,
            >,
        >::new();

        macro_rules! queue_transfer {
            ($worker:ident) => {
                let task = Box::pin(async move {
                    let receipt = self
                        .transfer_erc20(&$worker, U256::one(), to)
                        .await
                        .map_err(|_| $worker.clone())?;
                    Ok((receipt, $worker))
                });
                tasks.push(task);
            };
        }

        for (_, worker) in self.workers.iter().cloned() {
            queue_transfer!(worker);
        }

        let mut total_failed = 0;
        let mut total_transfers = 0 as f32;
        let start_time = Instant::now();
        let mut gas_price = None;

        while let Some(result) = tasks.next().await {
            let worker = match result {
                Ok((receipt, worker)) => {
                    gas_price = receipt.effective_gas_price;
                    worker
                }
                Err(worker) => {
                    total_failed += 1;
                    worker
                }
            };
            total_transfers += 1.0;
            let tps = total_transfers / Instant::now().duration_since(start_time).as_secs_f32();

            print!("\rTPS {tps}\t\tfailed {total_failed}/{total_transfers} total | gas_price: {gas_price:?}\t\t\t");

            queue_transfer!(worker);
        }

        Ok(())
    }

    pub async fn defund_workers_xtz(&self) -> Result<()> {
        println!(
            "[SCENARIO] transfer all xtz from {} workers",
            self.workers.len()
        );

        let to = self.client.controller_address();

        for (_, worker) in self.workers.iter() {
            let result = self.client.transfer_all_xtz(worker, to).await;
            match result {
                Ok((receipt, amount)) => println!(
                    "controller funded with {} from {} | hash: {:?}",
                    amount, receipt.from, receipt.transaction_hash
                ),
                Err(e) => println!("{e}"),
            }
        }

        Ok(())
    }

    pub async fn deploy_erc20(&self) -> Result<H160> {
        println!("[SCENARIO] Deploying ERC20 contract.");

        let provider = self.client.provider();
        let deploy = ERC20::deploy(provider, ())?.deployer.tx;

        let receipt = self
            .client
            .send(
                &self.client.controller,
                None,
                None,
                deploy.data().cloned(),
                |address| self.client.nonce(address),
            )
            .await?;

        let address = receipt
            .contract_address
            .expect("Contract should have been created");

        println!("Deployed ERC20 to {address:?}");

        Ok(address)
    }

    pub async fn mint_and_transfer_erc20(&self, amount: U256, to: H160) -> Result<()> {
        let controller = &self.client.controller;

        let balance = self.balance_erc20(controller.address()).await?;

        if balance < amount {
            self.mint_erc20(controller, amount - balance).await?;
        }

        let receipt = self.transfer_erc20(controller, amount, to).await?;

        println!(
            "Transferred {amount} ERC20 tokens to {to:?} | hash: {:?}",
            receipt.transaction_hash
        );
        Ok(())
    }

    pub async fn mint_erc20(&self, account: &LocalWallet, amount: U256) -> Result<()> {
        println!("[SCENARIO] Minting ERC20 tokens.");
        let Some(contract_address) = self.erc20 else {
            anyhow::bail!("ERC20 contract not deployed");
        };

        let provider = self.client.provider();
        let contract = ERC20::new(contract_address, provider);

        let data = contract.mint(amount).tx.data().cloned();

        let receipt = self
            .client
            .send(account, Some(contract_address), None, data, |address| {
                self.client.nonce(address)
            })
            .await?;

        let balance = self.balance_erc20(account.address()).await?;

        println!(
            "Minted {amount} ERC20 to {:?}, new balance {balance:?} | hash: {:?}",
            account.address(),
            receipt.transaction_hash
        );

        Ok(())
    }

    pub async fn transfer_erc20(
        &self,
        account: &LocalWallet,
        amount: U256,
        to: H160,
    ) -> Result<TransactionReceipt> {
        let Some(contract_address) = self.erc20 else {
            anyhow::bail!("ERC20 contract not deployed");
        };

        let provider = self.client.provider();
        let contract = ERC20::new(contract_address, provider);

        let data = contract.transfer(to, amount).tx.data().cloned();

        let receipt = self
            .client
            .send(account, Some(contract_address), None, data, |address| {
                self.client.nonce(address)
            })
            .await?;

        Ok(receipt)
    }

    pub async fn balance_erc20(&self, account: H160) -> Result<U256> {
        let Some(contract_address) = self.erc20 else {
            anyhow::bail!("ERC20 contract not deployed");
        };

        let provider = self.client.provider();
        let contract = ERC20::new(contract_address, provider);

        let call = contract.balance_of(account);
        let data = call.tx.data().cloned();

        let answer = self.client.call(Some(contract_address), None, data).await?;

        let balance = decode_function_data(&call.function, answer.as_ref(), false)?;

        Ok(balance)
    }
}
