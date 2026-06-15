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
}
