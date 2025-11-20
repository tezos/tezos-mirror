// SPDX-FileCopyrightText: 2022-2023 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2022-2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

//! Types & encodings for the *inbox-half* of the *L1/L2 communication protocol*
//!
//! In *general*, this module is a re-implementation of the tezos-protocol
//! [inbox message repr].
//!
//! [inbox message repr]: <https://gitlab.com/tezos/tezos/-/blob/9028b797894a5d9db38bc61a20abb793c3778316/src/proto_alpha/lib_protocol/sc_rollup_inbox_message_repr.mli>

use crypto::base58::FromBase58CheckError;
use crypto::hash::ContractTz1Hash;
use num_bigint::{BigInt, TryFromBigIntError};
use thiserror::Error;

use tezos_smart_rollup_encoding::michelson::ticket::StringTicket;

pub mod external;
pub mod sendable;

pub use self::external::*;

/// Representation of a string-ticket deposit into the rollup.
#[derive(Debug, PartialEq, Eq)]
pub struct InboxDeposit {
    /// The destination account of the deposit.
    pub destination: ContractTz1Hash,
    /// The ticket - including amount - to be deposited.
    pub ticket: StringTicket,
}

/// Error converting from a Michelson payload to [InboxDeposit].
#[derive(Debug, Error)]
pub enum DepositFromInternalPayloadError {
    /// Invalid destination
    #[error("Invalid Layer2 address {0}")]
    InvalidDestination(#[from] FromBase58CheckError),
    /// Invalid Ticket repr
    #[error("ticket amount out of range of u64 {0}")]
    InvalidAmount(TryFromBigIntError<BigInt>),
}

#[cfg(test)]
mod test {
    use crypto::hash::ContractKt1Hash;
    use crypto::hash::HashTrait;

    use tezos_protocol::contract::Contract;
    use tezos_smart_rollup_encoding::{
        inbox::InboxMessage,
        inbox::InternalInboxMessage,
        inbox::Transfer,
        michelson::ticket::StringTicket,
        michelson::{MichelsonPair, MichelsonString},
        public_key_hash::PublicKeyHash,
        smart_rollup::SmartRollupAddress,
    };

    // Ports of *internal_inbox_message* tests from
    // <https://gitlab.com/tezos/tezos/-/blob/a7a99d13b19121bfc9353426b545c3fb7f91da28/src/proto_alpha/lib_protocol/test/unit/test_sc_rollup_management_protocol.ml>
    //
    // Needed to ensure binary-compatability with messages from Layer 1.

    type Expr = MichelsonPair<MichelsonString, StringTicket>;

    fn test_encode_decode(expected_bytes: Vec<u8>, inbox_message: InboxMessage<Expr>) {
        let bytes = assert_encode_decode_inbox_message(inbox_message);
        assert_eq!(bytes, expected_bytes);
    }

    #[test]
    fn test_encode_decode_internal_transfer() {
        // binary encoding produced by lightly-modified (to print encoded data) protocol test
        // representing a `Pair ( string, string ticket )`
        let expected_bytes = vec![
            // Inbox message start
            0, // Internal tag
            0, // Transfer tag
            // Payload
            7, // Prim_2
            7, // Pair tag,
            1, // String tag
            0, 0, 0, b'$', // String size
            b't', b'z', b'4', b'M', b'S', b'f', b'Z', b's', b'n', b'6', b'k', // tz4 (1)
            b'M', b'D', b'c', b'z', b'S', b'h', b'y', b'8', b'P', b'M', b'e', // tz4 (2)
            b'B', b'6', b'2', b'8', b'T', b'N', b'u', b'k', b'n', b'9', b'h', // tz4 (3)
            b'i', b'2', b'K',  // tz3 (4)
            7,     // Prim_2
            7,     // Pair tag
            b'\n', // Bytes tag
            0, 0, 0, 22, // Bytes length
            // KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq
            1, 209, 163, b'|', 8, 138, 18, b'!', 182, b'6', 187, b'_', 204, 179, b'^', 5, 24, 16,
            b'8', 186, b'|', // kt1 end
            0,    // Padding
            7,    // Prim_2
            7,    // Pair
            1,    // String tag
            0, 0, 0, 3, // String size
            b'r', b'e', b'd', // string contents
            0,    // int encoding tag
            1,    // amount
            // Sender KT1BuEZtb68c1Q4yjtckcNjGELqWt56Xyesc - contract hash
            b'$', b'f', b'g', 169, b'1', 254, 11, 210, 251, 28, 182, 4, 247, 20, b'`', 30, 136,
            b'(', b'E', b'P', // end kt1
            // Source tz1RjtZUVeLhADFHDL8UwDZA6vjWWhojpu5w
            0, // PKH - Ed25519 tag
            b'B', 236, b'v', b'_', b'\'', 0, 19, b'N', 158, 14, 254, 137, 208, b'3', 142, b'.',
            132, b'<', b'S', 220, // end tz1
            // Destination sr1UX4Euo29Fd5bhmZypQffZJwy9M3A1D4Mb
            246, 144, 126, 197, 72, 77, 70, 203, 171, 146, 47, 210, 213, 225, 165, 143, 212, 162,
            185, 251, // end sr1
        ];

        let l2_address = "tz4MSfZsn6kMDczShy8PMeB628TNukn9hi2K".into();

        let ticket_creator = Contract::from_b58check("KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq")
            .expect("valid Kt1 address");

        let ticket =
            StringTicket::new(ticket_creator, MichelsonString("red".to_string()), 1).unwrap();

        let sender = ContractKt1Hash::from_b58check("KT1BuEZtb68c1Q4yjtckcNjGELqWt56Xyesc")
            .expect("valid Kt1 address");

        let source = PublicKeyHash::from_b58check("tz1RjtZUVeLhADFHDL8UwDZA6vjWWhojpu5w")
            .expect("valid tz1 address");

        let destination = SmartRollupAddress::from_b58check("sr1UX4Euo29Fd5bhmZypQffZJwy9M3A1D4Mb")
            .expect("valid sr1 address");

        let inbox_message = InboxMessage::Internal(InternalInboxMessage::Transfer(Transfer {
            payload: MichelsonPair(MichelsonString(l2_address), ticket),
            sender,
            source,
            destination,
        }));

        test_encode_decode(expected_bytes, inbox_message)
    }

    fn assert_encode_decode_inbox_message(
        message: InboxMessage<MichelsonPair<MichelsonString, StringTicket>>,
    ) -> Vec<u8> {
        let mut encoded = Vec::new();
        assert!(message.serialize(&mut encoded).is_ok());

        let decoded =
            InboxMessage::<MichelsonPair<MichelsonString, StringTicket>>::parse(encoded.as_slice())
                .expect("Deserialization to succeed")
                .1;

        let mut encoded_twice = Vec::new();
        assert!(decoded.serialize(&mut encoded_twice).is_ok());

        assert_eq!(encoded, encoded_twice);

        encoded
    }
}
