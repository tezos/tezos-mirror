// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

//! Forwarding contract for Tezos aliases of EVM addresses.
//!
//! When an EVM address receives a Tezos alias (KT1), this Michelson
//! contract is deployed at that KT1 so that any tez sent to it is
//! automatically forwarded to the original EVM address via the
//! TezosXGateway enshrined contract.
//!
//! The contract:
//! ```michelson
//! parameter unit ;
//! storage string ;   # native EVM address (e.g. "0x...")
//! code { CDR ;
//!        DUP ;
//!        PUSH address "KT18oDJJKXMKhfE1bSuAPGp92pYcwVDiqsPw" ;
//!        CONTRACT string ;
//!        IF_NONE { PUSH string "gateway" ; FAILWITH } {} ;
//!        BALANCE ;
//!        DIG 2 ;
//!        TRANSFER_TOKENS ;
//!        NIL operation ;
//!        SWAP ;
//!        CONS ;
//!        PAIR }
//! ```

use mir::ast::micheline::Micheline;

/// Micheline-encoded forwarding contract code.
///
/// Generated with:
/// ```sh
/// octez-client --mode mockup convert script \
///   'parameter unit ; storage string ; code { CDR ; DUP ; \
///    PUSH address "KT18oDJJKXMKhfE1bSuAPGp92pYcwVDiqsPw" ; \
///    CONTRACT string ; \
///    IF_NONE { PUSH string "gateway" ; FAILWITH } {} ; \
///    BALANCE ; DIG 2 ; TRANSFER_TOKENS ; \
///    NIL operation ; SWAP ; CONS ; PAIR }' \
///   from michelson to binary
/// ```
const FORWARDER_CODE_HEX: &str = "02000000740500036c0501036805020200000065031703210743036e01000000244b5431386f444a4a4b584d4b68664531625375415047703932705963775644697173507705550368072f02000000120743036801000000076761746577617903270200000000031505700002034d053d036d034c031b0342";

/// Returns the Micheline-encoded forwarding contract code.
pub fn forwarder_code() -> Result<Vec<u8>, hex::FromHexError> {
    hex::decode(FORWARDER_CODE_HEX)
}

/// Micheline-encode an EVM address string as the contract's initial storage.
pub fn forwarder_storage(
    native_evm_address: &str,
) -> Result<Vec<u8>, tezos_data_encoding::enc::BinError> {
    Micheline::from(native_evm_address).encode()
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
        let storage = forwarder_storage("0xabcdef").unwrap();
        // Micheline string tag = 0x01, then 4-byte length, then string bytes
        assert_eq!(storage[0], 0x01);
        let len = u32::from_be_bytes([storage[1], storage[2], storage[3], storage[4]]);
        assert_eq!(len as usize, "0xabcdef".len());
        assert_eq!(&storage[5..], b"0xabcdef");
    }

    #[test]
    fn forwarder_storage_empty_address() {
        let storage = forwarder_storage("").unwrap();
        assert_eq!(storage, vec![0x01, 0x00, 0x00, 0x00, 0x00]);
    }
}
