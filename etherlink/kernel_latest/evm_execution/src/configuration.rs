// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

#[cfg(test)]
use evm::Config;

#[derive(Default)]
pub enum EVMVersion {
    #[default]
    Prague,
}

impl EVMVersion {
    #[cfg(test)]
    pub fn to_config(&self) -> Config {
        let config = match self {
            EVMVersion::Prague => Config::prague(),
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

    #[cfg(test)]
    // Only to be used for test, to easily update all the unit tests with the
    // same configuration.
    // Code in production should read the feature flag to get the configuration.
    pub fn current_test_config() -> Config {
        Self::to_config(&EVMVersion::default())
    }
}
