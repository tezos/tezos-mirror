// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use evm::Config;
use tezos_evm_runtime::runtime::Runtime;

use crate::read_evm_version;

#[derive(Default)]
pub enum EVMVersion {
    Shanghai,
    #[default]
    Cancun,
}

impl EVMVersion {
    fn from_u32(x: u32) -> Option<Self> {
        match x {
            0 => Some(EVMVersion::Shanghai),
            1 => Some(EVMVersion::Cancun),
            _ => None,
        }
    }

    fn to_u32(&self) -> u32 {
        match self {
            EVMVersion::Shanghai => 0,
            EVMVersion::Cancun => 1,
        }
    }

    pub fn to_le_bytes(&self) -> [u8; 4] {
        self.to_u32().to_le_bytes()
    }

    pub fn from_le_bytes(bytes: [u8; 4]) -> Self {
        Self::from_u32(u32::from_le_bytes(bytes)).unwrap_or_default()
    }

    pub fn to_config(&self) -> Config {
        let config = match self {
            EVMVersion::Shanghai => Config::shanghai(),
            EVMVersion::Cancun => Config::cancun(),
        };

        // The current implementation doesn't support EVM's call stack limit of 1024.
        // We need to set a lower limit until we have switched to a head-based
        // recursive calls.
        //
        // TODO: When this limitation is removed, some evm evaluation tests needs
        // to be reactivated. As well as tests `call_too_deep_not_revert` and
        // `multiple_call_all_the_way_to_1024` in the evm execution crate.
        //
        // NB:
        // For backward compatibility reason, this code cannot be put as a shared
        // dependency between the different kernels we have.
        Config {
            call_stack_limit: 256,
            ..config
        }
    }

    #[cfg(any(test, feature = "execution-test-utils"))]
    // Only to be used for test, to easily update all the unit tests with the
    // same configuration.
    // Code in production should read the feature flag to get the configuration.
    pub fn current_test_config() -> Config {
        Self::to_config(&EVMVersion::default())
    }
}

pub fn fetch_evm_configuration<Host: Runtime>(host: &mut Host) -> Config {
    let evm_conf = read_evm_version(host);
    EVMVersion::to_config(&evm_conf)
}
