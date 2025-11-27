// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use revm::primitives::hardfork::SpecId;
use tezos_evm_runtime::runtime::Runtime;
use tezos_smart_rollup_host::{path::RefPath, runtime::RuntimeError};

// Path to the EVM version.
const EVM_VERSION: RefPath = RefPath::assert_from(b"/evm/evm_version");

#[derive(Default, Clone, Copy)]
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

pub fn store_evm_version(
    host: &mut impl Runtime,
    evm_version: &EVMVersion,
) -> Result<(), RuntimeError> {
    host.store_write(&EVM_VERSION, &evm_version.to_le_bytes(), 0)
}

pub fn read_evm_version(host: &mut impl Runtime) -> EVMVersion {
    let evm_version = host.store_read_all(&EVM_VERSION);
    match evm_version {
        Ok(evm_version) => match evm_version.as_slice().try_into().ok() {
            Some(evm_version) => EVMVersion::from_le_bytes(evm_version),
            None => EVMVersion::default(),
        },
        Err(_) => {
            let _ = store_evm_version(host, &EVMVersion::default());
            EVMVersion::default()
        }
    }
}
