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
    hash::P256Signature, public_key::PublicKey, signature::Signature,
    PublicKeySignatureVerifier,
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
            gas,
        ));
    };

    let Ok(public_key) = PublicKey::nom_read_exact(&call.publicKey) else {
        return Err(CustomPrecompileError::Revert(
            "invalid public key encoding".to_string(),
            gas,
        ));
    };

    let signature_bytes: &[u8] = &call.signature;
    let Ok(signature) = Signature::try_from(signature_bytes) else {
        return Err(CustomPrecompileError::Revert(
            "invalid signature encoding".to_string(),
            gas,
        ));
    };

    let valid = public_key
        .verify_signature(&signature, call.hash.as_ref())
        .unwrap_or(false)
        // ECDSA is malleable: for a P-256 (tz3) signer the high-S twin
        // (r, n - s) verifies just as well as the canonical (r, s). A contract
        // that keys anti-replay on the signature bytes (through this EIP-1271
        // path) would be defeated by presenting that twin, so reject the
        // non-canonical high-S form here. The shared P-256 verifier keeps
        // accepting high-S so tz3 accounts can still sign ordinary,
        // counter-protected operations.
        && match &public_key {
            PublicKey::P256(_) => P256Signature::try_from(call.signature.to_vec())
                .map(|sig| sig.is_canonical_low_s())
                .unwrap_or(false),
            _ => true,
        };

    let output = valid.abi_encode();

    Ok(InterpreterResult::new(
        revm::interpreter::InstructionResult::Return,
        Bytes::from(output),
        gas,
    ))
}
