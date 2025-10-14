// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use alloy_sol_types::{sol, SolInterface};
use revm::{
    context::{Block, ContextTr},
    interpreter::{Gas, InputsImpl, InstructionResult, InterpreterResult},
    primitives::{alloy_primitives::IntoLogData, Bytes, Log, U256},
};
use tezos_crypto_rs::{
    public_key::PublicKey, signature::Signature, PublicKeySignatureVerifier,
};
use tezos_data_encoding::nom::NomReader;
use tezos_smart_rollup_encoding::timestamp::Timestamp;

use crate::{
    database::DatabasePrecompileStateChanges,
    journal::Journal,
    precompiles::{
        change_sequencer_key::ChangeSequencerKey::ChangeSequencerKeyCalls,
        constants::{
            CHANGE_SEQUENCER_KEY_PRECOMPILE_ADDRESS, SEQUENCER_UPGRADE_DELAY,
            UPGRADE_SEQUENCER_PRECOMPILE_BASE_COST,
        },
        error::CustomPrecompileError,
        guard::out_of_gas,
    },
    storage::sequencer_key_change::SequencerKeyChange,
};

sol! {
    contract ChangeSequencerKey {
        function change_sequencer_key(
            bytes publicKey,
            bytes signature
        ) external;
    }

    event ChangeSequencerKeyEvent(
        string oldPublicKey,
        string newPublicKey,
        string signature
    );
}

pub(crate) fn change_sequencer_key_precompile<CTX, DB>(
    input: &[u8],
    context: &mut CTX,
    is_static: bool,
    transfer: &InputsImpl,
    gas_limit: u64,
) -> Result<InterpreterResult, CustomPrecompileError>
where
    CTX: ContextTr,
    CTX: ContextTr<Db = DB, Journal = Journal<DB>>,
    DB: DatabasePrecompileStateChanges,
{
    if Some(transfer.target_address) != transfer.bytecode_address {
        return Err(CustomPrecompileError::Revert(
            "DELEGATECALLs and CALLCODEs are not allowed".to_string(),
        ));
    }

    if transfer.target_address != CHANGE_SEQUENCER_KEY_PRECOMPILE_ADDRESS {
        return Err(CustomPrecompileError::Revert(String::from(
            "invalid transfer target address",
        )));
    }

    if is_static {
        return Err(CustomPrecompileError::Revert(String::from(
            "STATICCALLs are not allowed",
        )));
    }

    let mut gas = Gas::new(gas_limit);

    if !gas.record_cost(UPGRADE_SEQUENCER_PRECOMPILE_BASE_COST) {
        return Ok(out_of_gas(gas_limit));
    }

    let Ok(function_call) = ChangeSequencerKeyCalls::abi_decode(input) else {
        return Err(CustomPrecompileError::Revert(String::from(
            "invalid input encoding",
        )));
    };

    match function_call {
        // We are verifying that the signature passed as parameter is valid
        // and has been signed by the current sequencer key.
        ChangeSequencerKeyCalls::change_sequencer_key(call) => {
            let Ok(public_key) = PublicKey::nom_read_exact(&call.publicKey) else {
                return Err(CustomPrecompileError::Revert(String::from(
                    "invalid public key encoding",
                )));
            };

            // Nom read exact isn't compliant with the enconding on OCAML.
            let signature_bytes: &[u8] = &call.signature;
            let Ok(signature) = Signature::try_from(signature_bytes) else {
                return Err(CustomPrecompileError::Revert(String::from(
                    "invalid signature encoding",
                )));
            };

            if context.db().governance_sequencer_upgrade_exists()? {
                return Err(CustomPrecompileError::Revert(String::from(
                    "can't override an existing governance sequencer upgrade",
                )));
            }

            let Ok(sequencer_key) = context.db().sequencer() else {
                return Err(CustomPrecompileError::Revert(String::from(
                    "failed to read current sequencer key",
                )));
            };

            if sequencer_key
                .verify_signature(&signature, &call.publicKey)
                .is_err()
            {
                return Err(CustomPrecompileError::Revert(String::from(
                    "invalid signature",
                )));
            }

            let update_timestamp = context
                .block()
                .timestamp()
                .saturating_add(U256::from(SEQUENCER_UPGRADE_DELAY));
            let Ok(update_timestamp_i64): Result<i64, _> = update_timestamp.try_into()
            else {
                return Err(CustomPrecompileError::Revert(String::from(
                    "invalid update timestamp",
                )));
            };

            let public_key_b58 = public_key.to_b58check();
            context
                .journal_mut()
                .store_sequencer_key_change(SequencerKeyChange::new(
                    public_key,
                    Timestamp::from(update_timestamp_i64),
                ));

            let log_data = ChangeSequencerKeyEvent {
                oldPublicKey: sequencer_key.to_b58check(),
                newPublicKey: public_key_b58,
                signature: signature.to_base58_check(),
            };
            let log = Log {
                address: CHANGE_SEQUENCER_KEY_PRECOMPILE_ADDRESS,
                data: log_data.into_log_data(),
            };
            context.journal_mut().log(log);
        }
    }

    let result = InterpreterResult {
        result: InstructionResult::Return,
        gas,
        output: Bytes::new(),
    };
    Ok(result)
}
