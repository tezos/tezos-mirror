// SPDX-FileCopyrightText: 2024 Trilitech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Setup & maintenance of the firehose configuration.
//!
//! The following is maintained in the firehose config:
//! - etherlink RPC URL
//! - L2 _controller_ account that kicks off a run
//! - L2 _helper_ accounts that will be making transfers during a run
//!
//! The config lives at `$XDG_CONFIG_DIR/etherlink/firehose.json` by default.

use anyhow::Result;
use ethers::core::k256::elliptic_curve::SecretKey;
use ethers::core::k256::Secp256k1;
use ethers::core::rand::thread_rng;
use ethers::prelude::*;
use serde::{Deserialize, Serialize};
use tokio::fs::{read, write};
use tokio::task::spawn_blocking;

use std::path::{Path, PathBuf};

use crate::client::Client;

#[derive(Debug, Deserialize, Serialize)]
pub struct Config {
    endpoint: String,
    controller: Account,
    workers: Vec<Account>,
    pub erc20: Option<H160>,
}

impl Config {
    pub async fn configure(
        path: &(impl AsRef<Path> + std::fmt::Debug),
        endpoint: String,
        controller: Option<String>,
    ) -> Result<()> {
        let config = Self::load(path).await;

        let config = match (config, controller) {
            (Err(_), None) => {
                let sk = SecretKey::random(&mut thread_rng());
                Config {
                    endpoint,
                    controller: Account { sk },
                    workers: vec![],
                    erc20: None,
                }
            }
            (Ok(mut config), None) => {
                config.endpoint = endpoint;
                config
            }
            (Ok(mut config), Some(sk)) => {
                let sk = eth_sk_from_str(sk)?;
                if sk != config.controller.sk {
                    config.controller.sk = sk;
                };
                config.endpoint = endpoint;
                config
            }
            (_, Some(sk)) => {
                let sk = eth_sk_from_str(sk)?;
                Config {
                    endpoint,
                    controller: Account { sk },
                    workers: vec![],
                    erc20: None,
                }
            }
        };

        config.save(path).await?;
        Ok(())
    }

    pub async fn load(path: &impl AsRef<Path>) -> Result<Self> {
        let config = read(path).await?;
        let config = serde_json::from_slice(config.as_ref())?;
        Ok(config)
    }

    pub async fn save(&self, path: &impl AsRef<Path>) -> Result<()> {
        let config = serde_json::to_vec_pretty(self)?;
        write(path, config).await?;
        Ok(())
    }

    /// `$XDG_CONFIG_DIR/etherlink/firehose.json`
    pub async fn config_path() -> Result<PathBuf> {
        let path = spawn_blocking(move || {
            xdg::BaseDirectories::with_prefix("etherlink")
                .map(|s| s.place_config_file("firehose.json"))
        })
        .await???;

        Ok(path)
    }

    /// Ensure configuration contains at least the required number of worker accounts.
    pub fn generate_workers(&mut self, required: usize) {
        if self.workers.len() >= required {
            return;
        }

        self.workers.reserve(required - self.workers.len());
        for _ in 0..required {
            let sk = SecretKey::random(&mut thread_rng());
            self.workers.push(Account { sk })
        }
    }

    pub fn endpoint(&self) -> &str {
        self.endpoint.as_str()
    }

    pub fn controller(&self) -> LocalWallet {
        self.controller.clone().into()
    }

    pub fn workers(&self) -> &[Account] {
        self.workers.as_slice()
    }

    /// Remove any workers with no XTZ balance
    pub async fn cleanup_workers(&mut self, client: &Client) -> Result<()> {
        let mut workers = Vec::with_capacity(self.workers.len());

        for worker in self.workers.iter().cloned() {
            let wallet: LocalWallet = worker.clone().into();
            let balance = client.balance(wallet.address()).await?;
            if !balance.is_zero() {
                workers.push(worker);
            }
        }

        println!(
            "Deleting {} empty worker accounts",
            self.workers.len() - workers.len()
        );

        self.workers = workers;

        Ok(())
    }
}

/// An account, formed of a secret key and nonce
#[derive(Debug, Clone, Deserialize, Serialize)]
#[serde(into = "AccountRepr", try_from = "AccountRepr")]
pub struct Account {
    sk: SecretKey<Secp256k1>,
}

impl From<Account> for LocalWallet {
    fn from(val: Account) -> Self {
        LocalWallet::from(val.sk.clone())
    }
}

// Account representation used when serializing/deserializing
#[derive(Debug, Clone, Deserialize, Serialize)]
struct AccountRepr {
    sk: String,
}

impl From<Account> for AccountRepr {
    fn from(a: Account) -> Self {
        Self {
            sk: hex::encode(a.sk.to_bytes()),
        }
    }
}

impl TryFrom<AccountRepr> for Account {
    type Error = anyhow::Error;

    fn try_from(a: AccountRepr) -> Result<Self> {
        let sk = eth_sk_from_str(a.sk)?;
        Ok(Self { sk })
    }
}

fn eth_sk_from_str(sk: impl AsRef<str>) -> Result<SecretKey<Secp256k1>> {
    let sk = match hex::decode(sk.as_ref()) {
        Ok(sk) => SecretKey::from_slice(sk.as_ref())?,
        Err(err) => anyhow::bail!("Expected hex for private key: {err}",),
    };

    Ok(sk)
}
