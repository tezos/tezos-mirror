// SPDX-FileCopyrightText: 2024 Trilitech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Scenarios for various different stress-tests that can be performed.

use anyhow::Result;
use ethers::prelude::*;

use crate::client::Client;
use crate::config::Config;

pub struct Setup<'a> {
    client: &'a Client,
    workers: Vec<(H160, LocalWallet)>,
}

impl<'a> Setup<'a> {
    pub fn new(config: &'a Config, client: &'a Client, required_workers: usize) -> Result<Self> {
        if config.workers().len() < required_workers {
            anyhow::bail!(
                "Required {required_workers}, configuration contains {}",
                config.workers().len()
            );
        }

        let workers = config
            .workers()
            .iter()
            .take(required_workers)
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
}
