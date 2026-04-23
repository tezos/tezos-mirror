// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use alloy_sol_types::{sol, SolInterface, SolValue};
use evm_types::CustomPrecompileError;
use revm::{
    interpreter::{CallInputs, Gas, InterpreterResult},
    primitives::Bytes,
};
use tezos_crypto_rs::{
    public_key::PublicKey, signature::Signature, PublicKeySignatureVerifier,
};
use tezos_data_encoding::nom::NomReader;

use crate::precompiles::{constants::VERIFY_TEZOS_SIGNATURE_BASE_COST, guard::charge};

sol! {
    contract VerifyTezosSignature {
        function verifyTezosSignature(
            bytes publicKey,
            bytes32 hash,
            bytes signature
        ) external view returns (bool);
    }
}

pub(crate) fn verify_tezos_signature_precompile(
    calldata: &[u8],
    inputs: &CallInputs,
) -> Result<InterpreterResult, CustomPrecompileError> {
    let mut gas = Gas::new(inputs.gas_limit);
    charge(&mut gas, VERIFY_TEZOS_SIGNATURE_BASE_COST)?;

    let Ok(VerifyTezosSignature::VerifyTezosSignatureCalls::verifyTezosSignature(call)) =
        VerifyTezosSignature::VerifyTezosSignatureCalls::abi_decode(calldata)
    else {
        return Err(CustomPrecompileError::Revert(
            "invalid input encoding".to_string(),
        ));
    };

    let Ok(public_key) = PublicKey::nom_read_exact(&call.publicKey) else {
        return Err(CustomPrecompileError::Revert(
            "invalid public key encoding".to_string(),
        ));
    };

    let signature_bytes: &[u8] = &call.signature;
    let Ok(signature) = Signature::try_from(signature_bytes) else {
        return Err(CustomPrecompileError::Revert(
            "invalid signature encoding".to_string(),
        ));
    };

    let valid = public_key
        .verify_signature(&signature, call.hash.as_ref())
        .unwrap_or(false);

    let output = valid.abi_encode();

    Ok(InterpreterResult::new(
        revm::interpreter::InstructionResult::Return,
        Bytes::from(output),
        gas,
    ))
}
