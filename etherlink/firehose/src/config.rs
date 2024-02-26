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
use serde::{Deserialize, Serialize};
use tokio::fs::{read, write};
use tokio::task::spawn_blocking;

use std::path::{Path, PathBuf};

#[derive(Debug, Deserialize, Serialize)]
pub struct Config {
    endpoint: String,
    coordinator: Account,
}

impl Config {
    pub async fn configure(
        path: &(impl AsRef<Path> + std::fmt::Debug),
        endpoint: String,
        coordinator: Option<String>,
    ) -> Result<()> {
        let config = Self::load(path).await;

        let config = match (config, coordinator) {
            (Err(_), None) => {
                let sk = SecretKey::random(&mut thread_rng());
                Config {
                    endpoint,
                    coordinator: Account { sk, nonce: 0 },
                }
            }
            (Ok(mut config), None) => {
                config.endpoint = endpoint;
                config
            }
            (Ok(mut config), Some(sk)) => {
                let sk = eth_sk_from_str(sk)?;
                if sk != config.coordinator.sk {
                    config.coordinator.sk = sk;
                    config.coordinator.nonce = 0;
                };
                config.endpoint = endpoint;
                config
            }
            (_, Some(sk)) => {
                let sk = eth_sk_from_str(sk)?;
                Config {
                    endpoint,
                    coordinator: Account { sk, nonce: 0 },
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
}

/// An account, formed of a secret key and nonce
#[derive(Debug, Clone, Deserialize, Serialize)]
#[serde(into = "AccountRepr", try_from = "AccountRepr")]
pub struct Account {
    sk: SecretKey<Secp256k1>,
    nonce: u64,
}

// Account representation used when serializing/deserializing
#[derive(Debug, Clone, Deserialize, Serialize)]
struct AccountRepr {
    sk: String,
    nonce: u64,
}

impl From<Account> for AccountRepr {
    fn from(a: Account) -> Self {
        Self {
            sk: hex::encode(a.sk.to_bytes()),
            nonce: a.nonce,
        }
    }
}

impl TryFrom<AccountRepr> for Account {
    type Error = anyhow::Error;

    fn try_from(a: AccountRepr) -> Result<Self> {
        let sk = eth_sk_from_str(a.sk)?;
        Ok(Self { sk, nonce: a.nonce })
    }
}

fn eth_sk_from_str(sk: impl AsRef<str>) -> Result<SecretKey<Secp256k1>> {
    let sk = match hex::decode(sk.as_ref()) {
        Ok(sk) => SecretKey::from_slice(sk.as_ref())?,
        Err(err) => anyhow::bail!("Expected hex for private key: {err}",),
    };

    Ok(sk)
}
