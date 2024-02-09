use crate::{
    delayed_inbox::DelayedInbox,
    storage::{read_delayed_transaction_bridge, sequencer},
};
use tezos_crypto_rs::hash::ContractKt1Hash;
use tezos_evm_logging::{log, Level::*};
use tezos_smart_rollup_debug::Runtime;
use tezos_smart_rollup_encoding::public_key::PublicKey;

pub enum ConfigurationMode {
    Proxy,
    Sequencer {
        delayed_bridge: ContractKt1Hash,
        delayed_inbox: Box<DelayedInbox>,
        sequencer: PublicKey,
    },
}

pub struct Configuration {
    pub mode: ConfigurationMode,
}

impl Default for Configuration {
    fn default() -> Self {
        Self {
            mode: ConfigurationMode::Proxy,
        }
    }
}

impl std::fmt::Display for Configuration {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self {
                mode: ConfigurationMode::Proxy,
            } => write!(f, "Proxy"),
            Self {
                mode:
                    ConfigurationMode::Sequencer {
                        delayed_bridge,
                        delayed_inbox: _, // Ignoring delayed_inbox
                        sequencer,
                    },
            } => write!(
                f,
                "Sequencer {{ delayed_bridge: {:?}, sequencer: {:?} }}",
                delayed_bridge, sequencer
            ),
        }
    }
}

#[derive(Debug, PartialEq, Clone, Default)]
pub struct TezosContracts {
    pub ticketer: Option<ContractKt1Hash>,
    pub admin: Option<ContractKt1Hash>,
    pub sequencer_admin: Option<ContractKt1Hash>,
}

impl std::fmt::Display for TezosContracts {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Ticketer is {:?}. Administrator is {:?}. Sequencer administrator is {:?}.",
            self.ticketer, self.admin, self.sequencer_admin,
        )
    }
}
fn contains(contract: &Option<ContractKt1Hash>, expected: &ContractKt1Hash) -> bool {
    contract.as_ref().map_or(false, |kt1| kt1 == expected)
}

impl TezosContracts {
    pub fn is_admin(&self, contract: &ContractKt1Hash) -> bool {
        contains(&self.admin, contract)
    }
    pub fn is_sequencer_admin(&self, contract: &ContractKt1Hash) -> bool {
        contains(&self.sequencer_admin, contract)
    }
    pub fn is_ticketer(&self, contract: &ContractKt1Hash) -> bool {
        contains(&self.ticketer, contract)
    }
}

pub fn fetch_configuration<Host: Runtime>(host: &mut Host) -> Configuration {
    let sequencer = sequencer(host).unwrap_or_default();
    match sequencer {
        Some(sequencer) => {
            let delayed_bridge = read_delayed_transaction_bridge(host)
                // The sequencer must declare a delayed transaction bridge. This
                // default value is only to facilitate the testing.
                .unwrap_or_else(|| {
                    ContractKt1Hash::from_base58_check(
                        "KT18amZmM5W7qDWVt2pH6uj7sCEd3kbzLrHT",
                    )
                    .unwrap()
                });
            match DelayedInbox::new(host) {
                Ok(delayed_inbox) => Configuration {
                    mode: ConfigurationMode::Sequencer {
                        delayed_bridge,
                        delayed_inbox: Box::new(delayed_inbox),
                        sequencer,
                    },
                },
                Err(err) => {
                    log!(host, Fatal, "The kernel failed to created the delayed inbox, reverting configuration to proxy ({:?})", err);
                    Configuration::default()
                }
            }
        }
        None => Configuration::default(),
    }
}
