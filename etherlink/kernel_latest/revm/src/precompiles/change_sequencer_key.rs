// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use alloy_sol_types::{sol, SolInterface};
use evm_types::{CustomPrecompileError, IntoWithRemainder};
use revm::{
    context::{Block, Cfg, ContextTr},
    interpreter::{CallInputs, Gas, InstructionResult, InterpreterResult},
    primitives::{alloy_primitives::IntoLogData, Bytes, Log, U256},
};
use tezos_crypto_rs::{
    public_key::PublicKey, signature::Signature, PublicKeySignatureVerifier,
};
use tezos_data_encoding::nom::NomReader;
use tezos_smart_rollup_encoding::timestamp::Timestamp;

use tezos_smart_rollup_host::storage::StorageV1;
use tezosx_interfaces::Registry;

use crate::{
    database::EtherlinkVMDB,
    journal::Journal,
    precompiles::{
        change_sequencer_key::ChangeSequencerKey::ChangeSequencerKeyCalls,
        constants::{
            CHANGE_SEQUENCER_KEY_PRECOMPILE_ADDRESS, SEQUENCER_KEY_CHANGE_SIGN_TAG,
            SEQUENCER_UPGRADE_DELAY, UPGRADE_SEQUENCER_PRECOMPILE_BASE_COST,
        },
        guard::charge,
    },
};
use evm_types::{DatabasePrecompileStateChanges, SequencerKeyChange};

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

pub(crate) fn change_sequencer_key_precompile<'j, CTX, Host, R>(
    calldata: &[u8],
    context: &mut CTX,
    inputs: &CallInputs,
) -> Result<InterpreterResult, CustomPrecompileError>
where
    Host: StorageV1 + 'j,
    R: Registry + 'j,
    CTX: ContextTr<Db = EtherlinkVMDB<'j, Host, R>, Journal = Journal<'j, Host, R>>,
{
    let mut gas = Gas::new(inputs.gas_limit);

    if inputs.target_address != inputs.bytecode_address {
        return Err(CustomPrecompileError::Revert(
            "DELEGATECALLs and CALLCODEs are not allowed".to_string(),
            gas,
        ));
    }

    if inputs.target_address != CHANGE_SEQUENCER_KEY_PRECOMPILE_ADDRESS {
        return Err(CustomPrecompileError::Revert(
            String::from("invalid transfer target address"),
            gas,
        ));
    }

    if inputs.is_static {
        return Err(CustomPrecompileError::Revert(
            String::from("STATICCALLs are not allowed"),
            gas,
        ));
    }

    charge(&mut gas, UPGRADE_SEQUENCER_PRECOMPILE_BASE_COST)?;

    let Ok(function_call) = ChangeSequencerKeyCalls::abi_decode(calldata) else {
        return Err(CustomPrecompileError::Revert(
            String::from("invalid input encoding"),
            gas,
        ));
    };

    match function_call {
        // We are verifying that the signature passed as parameter is valid
        // and has been signed by the current sequencer key.
        ChangeSequencerKeyCalls::change_sequencer_key(call) => {
            let Ok(public_key) = PublicKey::nom_read_exact(&call.publicKey) else {
                return Err(CustomPrecompileError::Revert(
                    String::from("invalid public key encoding"),
                    gas,
                ));
            };

            // Nom read exact isn't compliant with the enconding on OCAML.
            let signature_bytes: &[u8] = &call.signature;
            let Ok(signature) = Signature::try_from(signature_bytes) else {
                return Err(CustomPrecompileError::Revert(
                    String::from("invalid signature encoding"),
                    gas,
                ));
            };

            if context
                .db()
                .governance_sequencer_upgrade_exists()
                .map_err(|e| e.into_with_remainder(gas))?
            {
                return Err(CustomPrecompileError::Revert(
                    String::from(
                        "can't override an existing governance sequencer upgrade",
                    ),
                    gas,
                ));
            }

            let Ok(sequencer_key) = context.db().sequencer() else {
                return Err(CustomPrecompileError::Revert(
                    String::from("failed to read current sequencer key"),
                    gas,
                ));
            };

            // The sequencer does not sign the raw new key but the new key bound
            // to this chain and to the current change counter. This makes the
            // captured `(publicKey, signature)` calldata single-use: the counter
            // is bumped as soon as a key change is stored (see the commit path in
            // `EtherlinkVMDB`), not only when it is applied, so resubmitting the
            // same calldata fails signature verification. Consequently a pending
            // change can be safely overwritten by its owner (sign against the new
            // counter with the still-current key) -- which is why no "a change is
            // already pending" guard is needed -- while a third party cannot
            // replay it to reset the delay, nor replay it on another chain or
            // after the key cycled back to a former value. Counter and chain_id
            // are recomputed by the kernel and are *not* part of the calldata, so
            // they cannot be forged by the caller.
            //
            // The payload is prefixed with `SEQUENCER_KEY_CHANGE_SIGN_TAG`, a
            // unique domain-separation label, so a signature captured over any
            // other message the sequencer key signs (e.g. an RLP-encoded
            // blueprint chunk) can never be replayed as a change signature.
            let chain_id = context.cfg().chain_id();
            // Read through the journal (not `db()` directly) so a second inner
            // call in the same transaction sees the in-memory value; within a
            // transaction this equals the durable counter, so the value signed
            // against is unchanged.
            let change_counter = context
                .journal()
                .sequencer_change_counter()
                .map_err(|e| e.into_with_remainder(gas))?;

            let mut signed_payload = Vec::with_capacity(
                SEQUENCER_KEY_CHANGE_SIGN_TAG.len()
                    + call.publicKey.len()
                    + 2 * U256::BYTES,
            );
            signed_payload.extend_from_slice(SEQUENCER_KEY_CHANGE_SIGN_TAG);
            signed_payload.extend_from_slice(&call.publicKey);
            signed_payload.extend_from_slice(
                &U256::from(chain_id).to_be_bytes::<{ U256::BYTES }>(),
            );
            signed_payload
                .extend_from_slice(&change_counter.to_be_bytes::<{ U256::BYTES }>());

            if !sequencer_key
                .verify_signature(&signature, &signed_payload)
                .unwrap_or(false)
            {
                return Err(CustomPrecompileError::Revert(
                    String::from("invalid signature"),
                    gas,
                ));
            }

            let update_timestamp = context
                .block()
                .timestamp()
                .saturating_add(U256::from(SEQUENCER_UPGRADE_DELAY));
            let Ok(update_timestamp_i64): Result<i64, _> = update_timestamp.try_into()
            else {
                return Err(CustomPrecompileError::Revert(
                    String::from("invalid update timestamp"),
                    gas,
                ));
            };

            let public_key_b58 = public_key.to_b58check();
            // Store the change and bump the replay counter (to `change_counter
            // + 1`) atomically in the layered state, so a reverting transaction
            // rolls back both. Bumping at store-time invalidates the signature
            // just verified above, making the captured calldata single-use.
            context
                .journal_mut()
                .store_sequencer_key_change(
                    SequencerKeyChange::new(
                        public_key,
                        Timestamp::from(update_timestamp_i64),
                    ),
                    change_counter,
                )
                .map_err(|e| e.into_with_remainder(gas))?;

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
