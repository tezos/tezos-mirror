// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use revm::primitives::hardfork::SpecId;
use tezos_smart_rollup_keyspace::{Key, KeySpace, KeySpaceError};

const EVM_VERSION_KEY: Key = Key::from_static(b"/evm_version");

// Note: Glamsterdam: there is no Amsterdam variant yet.
#[derive(Default, Clone, Copy, PartialEq, Debug)]
#[repr(u32)]
pub enum EVMVersion {
    Shanghai = 0,
    Cancun = 1,
    Prague = 2,
    #[default]
    Osaka = 3,
}

impl EVMVersion {
    pub fn to_le_bytes(&self) -> [u8; 4] {
        (*self as u32).to_le_bytes()
    }

    pub fn from_le_bytes(bytes: [u8; 4]) -> Self {
        match u32::from_le_bytes(bytes) {
            0 => EVMVersion::Shanghai,
            1 => EVMVersion::Cancun,
            2 => EVMVersion::Prague,
            3 => EVMVersion::Osaka,
            _ => EVMVersion::default(),
        }
    }
}

impl From<EVMVersion> for SpecId {
    fn from(evm_version: EVMVersion) -> Self {
        match evm_version {
            EVMVersion::Shanghai => SpecId::SHANGHAI,
            EVMVersion::Cancun => SpecId::CANCUN,
            EVMVersion::Prague => SpecId::PRAGUE,
            EVMVersion::Osaka => SpecId::OSAKA,
        }
    }
}

/// Writes `evm_version` in `evm_world_state`.
///
/// Fails when the key space rejects the write.
pub fn store_evm_version(
    evm_world_state: &mut impl KeySpace,
    evm_version: EVMVersion,
) -> Result<(), KeySpaceError> {
    evm_world_state.set(&EVM_VERSION_KEY, evm_version.to_le_bytes())
}

/// Returns the EVM version stored in `evm_world_state`.
///
/// When the key is absent, writes [`EVMVersion::default`] there and returns
/// it. When the stored bytes are not a known version, returns the default and
/// leaves them in place. Fails only when the write of the default fails.
pub fn read_or_set_evm_version(
    evm_world_state: &mut impl KeySpace,
) -> Result<EVMVersion, KeySpaceError> {
    match evm_world_state.get(&EVM_VERSION_KEY) {
        Some(bytes) => Ok(bytes
            .try_into()
            .map(EVMVersion::from_le_bytes)
            .unwrap_or_default()),
        None => {
            let evm_version = EVMVersion::default();
            store_evm_version(evm_world_state, evm_version)?;
            Ok(evm_version)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tezos_evm_runtime::runtime::MockKernelHost;
    use tezos_evm_runtime::runtime_keyspaces::RuntimeKeyspaces;
    use tezos_smart_rollup_host::{path::RefPath, storage::StorageV1};

    const LEGACY_EVM_VERSION: RefPath =
        RefPath::assert_from(b"/evm/world_state/evm_version");

    #[test]
    fn evm_version_uses_the_world_state_keyspace() {
        let mut host = MockKernelHost::default();
        let mut keyspaces = RuntimeKeyspaces::init(&mut host).unwrap();
        let version = EVMVersion::Cancun;

        store_evm_version(keyspaces.evm_world_state_mut(), version).unwrap();

        assert_eq!(
            keyspaces
                .host()
                .store_read_all(&LEGACY_EVM_VERSION)
                .unwrap(),
            version.to_le_bytes()
        );
        assert_eq!(
            read_or_set_evm_version(keyspaces.evm_world_state_mut()).unwrap(),
            version
        );
    }

    #[test]
    fn missing_evm_version_is_defaulted_in_the_world_state_keyspace() {
        let mut host = MockKernelHost::default();
        let mut keyspaces = RuntimeKeyspaces::init(&mut host).unwrap();
        let default = EVMVersion::default();

        assert_eq!(
            read_or_set_evm_version(keyspaces.evm_world_state_mut()).unwrap(),
            default
        );
        assert_eq!(
            keyspaces
                .host()
                .store_read_all(&LEGACY_EVM_VERSION)
                .unwrap(),
            default.to_le_bytes()
        );
    }

    #[test]
    fn malformed_evm_version_defaults_without_overwriting_storage() {
        let mut host = MockKernelHost::default();
        let mut keyspaces = RuntimeKeyspaces::init(&mut host).unwrap();
        keyspaces
            .host_mut()
            .store_write_all(&LEGACY_EVM_VERSION, &[1, 2, 3])
            .unwrap();

        assert_eq!(
            read_or_set_evm_version(keyspaces.evm_world_state_mut()).unwrap(),
            EVMVersion::default()
        );
        assert_eq!(
            keyspaces
                .host()
                .store_read_all(&LEGACY_EVM_VERSION)
                .unwrap(),
            [1, 2, 3]
        );
    }
}
