// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

//! Forwarding contract for Tezos aliases of EVM addresses.
//!
//! When an EVM address receives a Tezos alias (KT1), this Michelson
//! contract is deployed at that KT1 so that any tez sent to it is
//! automatically forwarded to the original EVM address via the
//! TezosXGateway enshrined contract's generic `%call` entrypoint
//! (an HTTP POST to `http://ethereum/<address>` with an empty body).
//!
//! The contract:
//! ```michelson
//! parameter unit ;
//! storage string ;   # native EVM address (e.g. "0x...")
//! code { CDR ;
//!        DUP ;
//!        PUSH string "http://ethereum/" ;
//!        CONCAT ;
//!        NONE (contract bytes) ;
//!        PUSH nat 1 ;
//!        PAIR ;
//!        PUSH bytes 0x ;
//!        PAIR ;
//!        NIL (pair string string) ;
//!        PAIR ;
//!        SWAP ;
//!        PAIR ;
//!        PUSH address "KT18oDJJKXMKhfE1bSuAPGp92pYcwVDiqsPw" ;
//!        CONTRACT %call (pair string (pair (list (pair string string))
//!                       (pair bytes (pair nat (option (contract bytes)))))) ;
//!        IF_NONE { PUSH string "gateway" ; FAILWITH } {} ;
//!        SWAP ;
//!        BALANCE ;
//!        SWAP ;
//!        TRANSFER_TOKENS ;
//!        NIL operation ;
//!        SWAP ;
//!        CONS ;
//!        PAIR }
//! ```

use mir::ast::micheline::Micheline;
use mir::gas::{Gas, OutOfGas};
use tezos_smart_rollup_host::storage::StorageV1;
use tezosx_interfaces::TezosXRuntimeError;

/// Micheline-encoded forwarding contract code.
///
/// Generated with:
/// ```sh
/// octez-client --mode mockup convert script \
///   'parameter unit ; storage string ; code { CDR ; DUP ; \
///    PUSH string "http://ethereum/" ; CONCAT ; \
///    NONE (contract bytes) ; PUSH nat 1 ; PAIR ; \
///    PUSH bytes 0x ; PAIR ; NIL (pair string string) ; PAIR ; \
///    SWAP ; PAIR ; \
///    PUSH address "KT18oDJJKXMKhfE1bSuAPGp92pYcwVDiqsPw" ; \
///    CONTRACT %call (pair string (pair (list (pair string string)) \
///                   (pair bytes (pair nat (option (contract bytes)))))) ; \
///    IF_NONE { PUSH string "gateway" ; FAILWITH } {} ; \
///    SWAP ; BALANCE ; SWAP ; TRANSFER_TOKENS ; \
///    NIL operation ; SWAP ; CONS ; PAIR }' \
///   from michelson to binary
/// ```
const FORWARDER_CODE_HEX: &str = "02000000d90500036c05010368050202000000ca03170321074303680100000010687474703a2f2f657468657265756d2f031a053e055a03690743036200010342074303690a000000000342053d0765036803680342034c03420743036e01000000244b5431386f444a4a4b584d4b6866453162537541504770393270596377564469717350770655076503680765055f07650368036807650369076503620563055a0369000000052563616c6c072f02000000120743036801000000076761746577617903270200000000034c0315034c034d053d036d034c031b0342";

/// Returns the Micheline-encoded forwarding contract code.
pub fn forwarder_code() -> Result<Vec<u8>, hex::FromHexError> {
    hex::decode(FORWARDER_CODE_HEX)
}

/// Micheline-encode an EVM address string as the contract's initial storage.
/// Outer `Result` reports gas exhaustion; inner reports serialization
/// failures. The caller passes its own gas counter so the encoding cost
/// is charged against the operation's budget.
pub fn forwarder_storage(
    native_evm_address: &str,
    gas: &mut Gas,
) -> Result<Result<Vec<u8>, tezos_data_encoding::enc::BinError>, OutOfGas> {
    Micheline::from(native_evm_address).encode(gas)
}

/// Seed the shared alias implementation slot with the canonical forwarder code
/// if the slot is empty, leaving an already-populated slot untouched.
///
/// Idempotent, so it is safe to call from both seeding points: the Michelson
/// runtime activation (fresh networks) and the storage migration
/// (already-deployed networks). The slot itself lives in `tezos_execution`
/// ([`tezos_execution::account_storage::read_alias_implementation`]); this
/// wrapper supplies the Michelson-runtime-specific forwarder code, which is why
/// it lives next to it here rather than in the generic execution layer.
pub fn init_alias_implementation(
    host: &mut impl StorageV1,
) -> Result<(), TezosXRuntimeError> {
    use tezos_execution::account_storage::{
        read_alias_implementation, write_alias_implementation,
    };
    let seeded = read_alias_implementation(host)?;
    if seeded.is_none() {
        let code = forwarder_code().map_err(|e| {
            TezosXRuntimeError::Custom(format!(
                "decoding forwarder code from hex failed: {e}"
            ))
        })?;
        write_alias_implementation(host, &code)?;
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn forwarder_code_is_valid_hex() {
        let code = forwarder_code().expect("FORWARDER_CODE_HEX is a valid hex constant");
        assert!(!code.is_empty());
        // Micheline sequence tag
        assert_eq!(code[0], 0x02);
    }

    #[test]
    fn forwarder_storage_encodes_string() {
        let storage = forwarder_storage("0xabcdef", &mut Gas::default())
            .unwrap()
            .unwrap();
        // Micheline string tag = 0x01, then 4-byte length, then string bytes
        assert_eq!(storage[0], 0x01);
        let len = u32::from_be_bytes([storage[1], storage[2], storage[3], storage[4]]);
        assert_eq!(len as usize, "0xabcdef".len());
        assert_eq!(&storage[5..], b"0xabcdef");
    }

    #[test]
    fn forwarder_storage_empty_address() {
        let storage = forwarder_storage("", &mut Gas::default()).unwrap().unwrap();
        assert_eq!(storage, vec![0x01, 0x00, 0x00, 0x00, 0x00]);
    }

    #[test]
    fn alias_implementation_seed_and_roundtrip() {
        use tezos_evm_runtime::runtime::MockKernelHost;
        use tezos_execution::account_storage::{
            read_alias_implementation, write_alias_implementation,
        };

        let mut host = MockKernelHost::default();

        // Absent before seeding.
        assert!(read_alias_implementation(&host).unwrap().is_none());

        // Seeding installs the canonical forwarder code.
        init_alias_implementation(&mut host).unwrap();
        let forwarder = forwarder_code().unwrap();
        assert_eq!(
            read_alias_implementation(&host).unwrap(),
            Some(forwarder.clone())
        );

        // Seeding again is a no-op: an already-populated slot is untouched.
        write_alias_implementation(&mut host, b"already here").unwrap();
        init_alias_implementation(&mut host).unwrap();
        assert_eq!(
            read_alias_implementation(&host).unwrap(),
            Some(b"already here".to_vec())
        );

        // An explicit write overwrites: the O(1) upgrade primitive.
        write_alias_implementation(&mut host, &forwarder).unwrap();
        assert_eq!(read_alias_implementation(&host).unwrap(), Some(forwarder));
    }

    /// End-to-end check that the entrypoints RPC (`get_contract_entrypoint`),
    /// which goes through `code()`, resolves a code-less alias KT1 to the
    /// *real* shared forwarder and exposes its `default : unit` entrypoint.
    ///
    /// Uses `forwarder_code()` rather than a stand-in script, so it pins the
    /// actual entrypoint surface that ships.
    #[test]
    fn alias_entrypoints_resolve_to_real_forwarder() {
        use mir::ast::{AddressHash, Entrypoint, Type};
        use mir::gas::Gas;
        use tezos_crypto_rs::{blake2b, hash::ContractKt1Hash};
        use tezos_evm_runtime::runtime::MockKernelHost;
        use tezos_execution::account_storage::write_alias_implementation;
        use tezos_execution::context::record_origin;
        use tezos_execution::get_contract_entrypoint;
        use tezosx_interfaces::{AliasInfo, Origin, RuntimeId};

        let mut host = MockKernelHost::default();
        // Aliases resolve under the production Michelson accounts path
        // `/tez/tez_accounts`, pinned by the `TEZOS_ACCOUNTS_ROOT` constant the
        // account helpers use, so this pins the real composition rather than a
        // mixed layout that never ships.

        // Seed the shared slot with the real forwarder code.
        write_alias_implementation(&mut host, &forwarder_code().unwrap()).unwrap();

        // A code-less KT1 classified as an alias (no /data/code written).
        let kt1 = ContractKt1Hash::from(blake2b::digest_160(b"alias-entrypoint-test"));
        record_origin(
            &mut host,
            &kt1,
            &Origin::Alias(AliasInfo {
                runtime: RuntimeId::Ethereum,
                native_address: b"0xabc".to_vec(),
            }),
        )
        .unwrap();

        // The entrypoints RPC resolves through code() to the shared forwarder.
        let entrypoints =
            get_contract_entrypoint(&host, &AddressHash::Kt1(kt1), &mut Gas::default())
                .expect("alias entrypoints resolve to the shared forwarder");
        assert_eq!(entrypoints.get(&Entrypoint::default()), Some(&Type::Unit));
    }

    /// Upgrading the real shipped forwarder to a superset that keeps
    /// `default : unit` and `storage string` is accepted. Pins the production
    /// storage type and default surface against the upgrade invariants (the
    /// `upgrade_*` tests in `tezos_execution` use toy `storage unit` scripts;
    /// this lives here because `tezos_execution` cannot reach `forwarder_code`).
    #[test]
    fn upgrade_real_forwarder_to_superset_is_accepted() {
        use mir::gas::Gas;
        use tezos_evm_runtime::runtime::MockKernelHost;
        use tezos_execution::account_storage::{
            read_alias_implementation, write_alias_implementation,
        };
        use tezos_execution::upgrade_alias_implementation;

        let mut host = MockKernelHost::default();
        write_alias_implementation(&mut host, &forwarder_code().unwrap()).unwrap();

        // Keep `default : unit` (annotated `%default`) and `storage string`,
        // add a `foo` entrypoint.
        let superset = mir::parser::Parser::new()
            .parse_top_level(
                "parameter (or (unit %default) (string %foo)); storage string; \
                 code { CDR; NIL operation; PAIR }",
            )
            .unwrap()
            .encode(&mut Gas::default())
            .unwrap()
            .unwrap();

        upgrade_alias_implementation(&mut host, &superset, &mut Gas::default())
            .expect("a superset of the real forwarder must be accepted");
        assert_eq!(read_alias_implementation(&host).unwrap(), Some(superset));
    }
}
