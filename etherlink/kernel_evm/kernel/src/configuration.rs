use crate::{
    delayed_inbox::DelayedInbox,
    storage::{
        read_admin, read_delayed_transaction_bridge, read_kernel_governance,
        read_kernel_security_governance, read_sequencer_governance, read_ticketer,
        sequencer,
    },
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

impl std::fmt::Display for ConfigurationMode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ConfigurationMode::Proxy => write!(f, "Proxy"),
            ConfigurationMode::Sequencer {
                delayed_bridge,
                delayed_inbox: _, // Ignoring delayed_inbox
                sequencer,
            } => write!(
                f,
                "Sequencer {{ delayed_bridge: {:?}, sequencer: {:?} }}",
                delayed_bridge, sequencer
            ),
        }
    }
}

pub struct Configuration {
    pub tezos_contracts: TezosContracts,
    pub mode: ConfigurationMode,
}

impl Default for Configuration {
    fn default() -> Self {
        Self {
            tezos_contracts: TezosContracts::default(),
            mode: ConfigurationMode::Proxy,
        }
    }
}

impl std::fmt::Display for Configuration {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Tezos Contracts: {}, Mode: {}",
            &self.tezos_contracts, &self.mode
        )
    }
}

#[derive(Debug, PartialEq, Clone, Default)]
pub struct TezosContracts {
    pub ticketer: Option<ContractKt1Hash>,
    pub admin: Option<ContractKt1Hash>,
    pub sequencer_governance: Option<ContractKt1Hash>,
    pub kernel_governance: Option<ContractKt1Hash>,
    pub kernel_security_governance: Option<ContractKt1Hash>,
}

impl std::fmt::Display for TezosContracts {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let TezosContracts {
            ticketer,
            admin,
            sequencer_governance,
            kernel_governance,
            kernel_security_governance,
        } = self;
        write!(
            f,
            "Ticketer is {:?}. Administrator is {:?}. Sequencer governance is {:?}. Kernel governance is {:?}. Kernel security governance is {:?}.",
            ticketer, admin, sequencer_governance, kernel_governance, kernel_security_governance
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
    pub fn is_sequencer_governance(&self, contract: &ContractKt1Hash) -> bool {
        contains(&self.sequencer_governance, contract)
    }
    pub fn is_ticketer(&self, contract: &ContractKt1Hash) -> bool {
        contains(&self.ticketer, contract)
    }
    pub fn is_kernel_governance(&self, contract: &ContractKt1Hash) -> bool {
        contains(&self.kernel_governance, contract)
    }

    pub fn is_kernel_security_governance(&self, contract: &ContractKt1Hash) -> bool {
        contains(&self.kernel_security_governance, contract)
    }
}

fn fetch_tezos_contracts(host: &mut impl Runtime) -> TezosContracts {
    // 1. Fetch the kernel's ticketer, returns `None` if it is badly
    //    encoded or absent.
    let ticketer = read_ticketer(host);
    // 2. Fetch the kernel's administrator, returns `None` if it is badly
    //    encoded or absent.
    let admin = read_admin(host);
    // 3. Fetch the sequencer governance, returns `None` if it is badly
    //    encoded or absent.
    let sequencer_governance = read_sequencer_governance(host);
    // 4. Fetch the kernel_governance contract, returns `None` if it is badly
    //    encoded or absent.
    let kernel_governance = read_kernel_governance(host);
    // 5. Fetch the kernel_security_governance contract, returns `None` if it is badly
    //    encoded or absent.
    let kernel_security_governance = read_kernel_security_governance(host);

    TezosContracts {
        ticketer,
        admin,
        sequencer_governance,
        kernel_governance,
        kernel_security_governance,
    }
}

pub fn fetch_configuration<Host: Runtime>(host: &mut Host) -> Configuration {
    let tezos_contracts = fetch_tezos_contracts(host);

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
                    tezos_contracts,
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
        None => Configuration {
            tezos_contracts,
            mode: ConfigurationMode::Proxy,
        },
    }
}
