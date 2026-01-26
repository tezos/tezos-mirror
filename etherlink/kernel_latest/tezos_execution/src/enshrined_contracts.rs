// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use mir::{
    ast::{BinWriter, Entrypoint, Micheline},
    context::CtxTrait,
};
use tezos_crypto_rs::hash::ContractKt1Hash;
use tezos_tezlink::operation_result::TransferError;

#[derive(Debug, PartialEq)]
pub enum EnshrinedContracts {
    TezosXGateway,
}

/// prefix used to do a first quick check to eliminate most non enshrined contracts
const ENSHRINED_PREFIX: [u8; 6] = [2, 90, 121, 0, 0, 0];
// KT18oDJJKXMKhfE1bSuAPGp92pYcwVDiqsPw
const GATEWAY_ADDRESS: &[u8] = &[
    2, 90, 121, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
];

// Should be as transparent/cheap as possible for none-native contract (the
// direct path)
pub fn from_kt1(kt1: &ContractKt1Hash) -> Option<EnshrinedContracts> {
    let bytes = kt1.to_bytes().ok()?;
    // let's escape early if possible
    if !bytes.starts_with(&ENSHRINED_PREFIX) {
        return None;
    }
    match bytes.as_slice() {
        GATEWAY_ADDRESS => Some(EnshrinedContracts::TezosXGateway),
        _ => None,
    }
}

pub fn is_enshrined(kt1: &ContractKt1Hash) -> bool {
    from_kt1(kt1).is_some()
}

pub(crate) fn execute_enshrined_contract<'a>(
    _contract: EnshrinedContracts,
    _entrypoint: &Entrypoint,
    _value: Micheline<'a>,
    _ctx: &mut impl CtxTrait<'a>,
) -> Result<(), TransferError> {
    // TODO L2-708
    Ok(())
}

pub(crate) fn get_enshrined_contract_entrypoint(
    _contract: EnshrinedContracts,
) -> Option<std::collections::HashMap<Entrypoint, mir::ast::Type>> {
    // TODO L2-708
    None
}

#[cfg(test)]
mod tests {
    use tezos_crypto_rs::hash::{ContractKt1Hash, HashTrait};

    use crate::enshrined_contracts::*;

    const GATEWAY_KT1: &str = "KT18oDJJKXMKhfE1bSuAPGp92pYcwVDiqsPw";

    #[test]
    fn test_gateway() {
        let contract = ContractKt1Hash::try_from_bytes(GATEWAY_ADDRESS).unwrap();
        assert![is_enshrined(&contract)];
        assert![contract.to_base58_check().as_str() == GATEWAY_KT1];
        assert![from_kt1(&contract) == Some(EnshrinedContracts::TezosXGateway)];
    }
}
