// SPDX-FileCopyrightText: 2022-2023 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Constructing inbox messages for sending to the kernel.

use tezos_data_encoding::{enc::BinWriter, encoding::HasEncoding};

use tezos_smart_rollup_encoding::inbox::InternalInboxMessage;
use tezos_smart_rollup_encoding::michelson::Michelson;

pub use super::external::sendable::ExternalInboxMessage;
use super::{DepositFromInternalPayloadError, InboxDeposit};

/// Sendable `InboxMessage`.
#[derive(HasEncoding, BinWriter)]
pub enum InboxMessage<Expr>
where
    Expr: Michelson + TryInto<InboxDeposit, Error = DepositFromInternalPayloadError>,
{
    /// Message sent from an L1 smart-contract.
    Internal(InternalInboxMessage<Expr>),
    /// Message of arbitrary bytes, in a format specific to the kernel.
    ///
    /// The containing operation will be sent by an implicit account - but will
    /// deserialize to a structure representing *transactions* & *withdrawals* between
    /// and from **Layer2Tz4* addresses respectively.
    External(ExternalInboxMessage),
}

#[cfg(test)]
mod test {
    use super::ExternalInboxMessage;
    use super::InboxMessage;
    use crate::inbox::external::v1::sendable::Batch;
    use crate::inbox::external::v1::Operation;
    use crate::inbox::external::ParsedExternalInboxMessage;
    use proptest::collection;
    use proptest::prelude::*;
    use tezos_data_encoding::enc::BinWriter;
    use tezos_smart_rollup_encoding::inbox::InboxMessage as ParsedInboxMessage;
    use tezos_smart_rollup_encoding::michelson::ticket::StringTicket;
    use tezos_smart_rollup_encoding::michelson::{MichelsonPair, MichelsonString};

    proptest! {
        #[test]
        fn sendable_v1_external_inbox_encode_decode(
            operations in collection::vec(Operation::arb_with_signer(), 1..5),
            remaining_input in any::<Vec<u8>>(),
        ) {
            // Arrange
            let batch = Batch::new(operations);


            let inbox_message =
                InboxMessage::<MichelsonPair<MichelsonString,StringTicket>>::External(
                ExternalInboxMessage::OpList(batch) );

            let mut encoded = Vec::new();
            inbox_message.bin_write(&mut encoded).unwrap();
            encoded.extend_from_slice(remaining_input.as_slice());

            // Act + Assert
            let (remaining, inbox_message) =
                ParsedInboxMessage::<MichelsonPair<MichelsonString,StringTicket>>::parse(encoded.as_slice())
                .expect("Parsing of inbox message failed");

            // `ExternalInboxMessage` is special, in the sense that it is encoded Variadically,
            // ie is not prefixed by any sort of length - hence we expect no remaining bytes here.
            assert!(remaining.is_empty(), "ExternalInboxMessage parsing should consume all bytes");

            if let ParsedInboxMessage::External(encoded) = inbox_message {
                let (remaining, _external) = ParsedExternalInboxMessage::parse(encoded)
                    .expect("Parsing external inbox message should work");

                assert_eq!(remaining_input.as_slice(), remaining, "ParsedExternalInboxMessage::parse consumed too many bytes.")
            } else {
                panic!("Expected ExternalInboxMessage");
            }
        }
    }
}
