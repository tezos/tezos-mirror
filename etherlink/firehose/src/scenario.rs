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

pub struct Setup<'a> {
    client: &'a Client,
    workers: Vec<(H160, LocalWallet)>,
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

        Ok(Self { client, workers })
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

    /// Perform xtz transfers from the workers back to the controller account,
    /// aiming to match the requested tps.
    ///
    /// Each transfer is only 1 wei, by default.
    pub async fn xtz_transfers(&self) -> Result<()> {
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
                    let receipt = self
                        .client
                        .transfer_xtz(&$worker, to, U256::one())
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

            print!("\rTPS {tps}\t\tfailed {total_failed}/{total_transfers} total\t\t| gas_price: {gas_price:?}\t\t\t");

            queue_transfer!(worker);
        }

        Ok(())
    }
}
