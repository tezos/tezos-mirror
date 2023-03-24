// SPDX-FileCopyrightText: 2022-2023 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

//! Types & encodings for the *outbox-half* of the *L1/L2 communication protocol*
//!
//! In *general*, this module is a re-implementation of the tezos-protocol
//! [outbox message repr].
//!
//! We have, however, a *specialised* [OutboxMessageTransaction::parameters]. The
//! communication protocol allows payload to be any michelson `script_expr`.
//!
//! [outbox message repr]: <https://gitlab.com/tezos/tezos/-/blob/80b2cccb9c663dde2d86a6c94806fc149b7d1ef3/src/proto_alpha/lib_protocol/sc_rollup_outbox_message_repr.ml>

use tezos_data_encoding::enc::BinWriter;
use tezos_data_encoding::encoding::HasEncoding;
use tezos_data_encoding::nom::NomReader;

use crate::contract::Contract;
use crate::entrypoint::Entrypoint;
use crate::michelson::Michelson;

/// Outbox message, sent by the kernel as tezos-encoded bytes.
///
/// Encoded as a dynamic list of [OutboxMessageTransaction], with **no** case tag.
#[derive(Debug, PartialEq, Eq, HasEncoding, NomReader, BinWriter)]
pub enum OutboxMessage<Expr: Michelson> {
    /// List of outbox transactions that must succeed together.
    AtomicTransactionBatch(OutboxMessageTransactionBatch<Expr>),
}

/// A batch of [`OutboxMessageTransaction`].
#[derive(Debug, PartialEq, Eq, HasEncoding, BinWriter, NomReader)]
pub struct OutboxMessageTransactionBatch<Expr: Michelson> {
    #[encoding(dynamic, list)]
    batch: Vec<OutboxMessageTransaction<Expr>>,
}

impl<Expr: Michelson> OutboxMessageTransactionBatch<Expr> {
    /// Returns the number of transactions in the batch.
    pub fn len(&self) -> usize {
        self.batch.len()
    }

    /// Returns whether the batch is empty.
    pub fn is_empty(&self) -> bool {
        self.batch.is_empty()
    }
}

impl<Expr: Michelson> core::ops::Index<usize> for OutboxMessageTransactionBatch<Expr> {
    type Output = OutboxMessageTransaction<Expr>;

    fn index(&self, index: usize) -> &Self::Output {
        self.batch.index(index)
    }
}

impl<Expr: Michelson> From<Vec<OutboxMessageTransaction<Expr>>>
    for OutboxMessageTransactionBatch<Expr>
{
    fn from(batch: Vec<OutboxMessageTransaction<Expr>>) -> Self {
        Self { batch }
    }
}

/// Outbox message transaction, part of the outbox message.
///
/// Encoded as:
/// ```ocaml
/// (obj3
///   (req "parameters" Script_repr.expr_encoding)
///   (req "destination" Contract_repr.originated_encoding)
///   (req "entrypoint" Entrypoint_repr.simple_encoding))
/// ```
#[derive(Debug, PartialEq, Eq, HasEncoding, BinWriter, NomReader)]
pub struct OutboxMessageTransaction<Expr: Michelson> {
    /// Micheline-encoded payload, sent to the destination contract.
    pub parameters: Expr,
    /// The destination smart-contract.
    ///
    /// Protocol side this is a `Contract_hash` (aka `ContractKT1Hash`), but encoded as
    /// `Contract.originated_encoding`.
    pub destination: Contract,
    /// The entrypoint of the destination that will be called.
    pub entrypoint: Entrypoint,
}

#[cfg(test)]
mod test {
    use crate::michelson::ticket::StringTicket;

    use super::*;

    // first byte is union tag (currently always `0`, next four are list size of batch)
    const ENCODED_OUTBOX_MESSAGE_PREFIX: [u8; 5] = [0, 0, 0, 0, 152];

    const ENCODED_TRANSACTION_ONE: [u8; 74] = [
        7, 7, // Prim pair tags
        // Contract KT1 hash
        b'\n', 0, 0, 0, 22, 1, 209, 163, b'|', 8, 138, 18, b'!', 182, b'6', 187, b'_',
        204, 179, b'^', 5, 24, 16, b'8', 186, b'|', 0, // Padding
        7, 7, // Prim pair tags,
        // String
        1, // tag
        0, 0, 0, 3, // size,
        b'r', b'e', b'd', // contents,
        0, 1, // nat size + value
        // Destination
        1, // originated tag
        36, 102, 103, 169, 49, 254, 11, 210, 251, 28, 182, 4, 247, 20, 96, 30, 136, 40,
        69, 80, // end originated
        0,  // padding
        //  entrypoint
        0, 0, 0, 7, b'd', b'e', b'f', b'a', b'u', b'l', b't',
    ];

    const ENCODED_TRANSACTION_TWO: [u8; 78] = [
        // String Ticket
        7, 7, // prim pair tags
        // Contract KT1 hash
        b'\n', 0, 0, 0, 22, 1, b'$', b'f', b'g', 169, b'1', 254, 11, 210, 251, 28, 182, 4,
        247, 20, b'`', 30, 136, b'(', b'E', b'P', 0, // padding
        7, 7, // prim pair tags
        // String contents
        1, 0, 0, 0, 6, b'y', b'e', b'l', b'l', b'o', b'w', // end contents
        0,    // Nat tag
        137, 5, // Z(329)
        // Destination
        1, 21, 237, 173, b'\'', 159, b'U', 226, 254, b'@', 17, 222, b'm', b',', b'$', 253,
        245, 27, 242, b'%', 197, 0, // Entrypoint
        0, 0, 0, 7, // Entrypoint size
        b'a', b'n', b'o', b't', b'h', b'e', b'r', // Entrypoint name
    ];

    #[test]
    fn encode_transaction() {
        let mut bin = vec![];
        transaction_one().bin_write(&mut bin).unwrap();

        assert_eq!(&ENCODED_TRANSACTION_ONE, bin.as_slice());
    }

    #[test]
    fn decode_transaction() {
        let (remaining, decoded) =
            OutboxMessageTransaction::nom_read(ENCODED_TRANSACTION_TWO.as_slice())
                .unwrap();

        assert!(remaining.is_empty());
        assert_eq!(transaction_two(), decoded);
    }

    #[test]
    fn encode_outbox_message() {
        let mut expected = ENCODED_OUTBOX_MESSAGE_PREFIX.to_vec();
        expected.extend_from_slice(ENCODED_TRANSACTION_ONE.as_slice());
        expected.extend_from_slice(ENCODED_TRANSACTION_TWO.as_slice());

        let message = OutboxMessage::AtomicTransactionBatch(
            vec![transaction_one(), transaction_two()].into(),
        );

        let mut bin = vec![];
        message.bin_write(&mut bin).unwrap();

        assert_eq!(expected, bin);
    }

    #[test]
    fn decode_outbox_message() {
        let mut bytes = ENCODED_OUTBOX_MESSAGE_PREFIX.to_vec();
        bytes.extend_from_slice(ENCODED_TRANSACTION_TWO.as_slice());
        bytes.extend_from_slice(ENCODED_TRANSACTION_ONE.as_slice());

        let expected = OutboxMessage::AtomicTransactionBatch(
            vec![transaction_two(), transaction_one()].into(),
        );

        let (remaining, message) = OutboxMessage::nom_read(bytes.as_slice()).unwrap();

        assert!(remaining.is_empty());
        assert_eq!(expected, message);
    }

    #[test]
    fn decode_outbox_message_err_on_invalid_prefix() {
        let mut bytes = ENCODED_OUTBOX_MESSAGE_PREFIX.to_vec();
        // prefix too long, missing message
        bytes.extend_from_slice(ENCODED_TRANSACTION_ONE.as_slice());
        // garbage (to be ignored) tail
        bytes.extend_from_slice([10; 1000].as_slice());

        assert!(matches!(
            OutboxMessage::<StringTicket>::nom_read(bytes.as_slice()),
            Err(_)
        ));
    }

    fn transaction_one() -> OutboxMessageTransaction<StringTicket> {
        let ticket = StringTicket::new(
            Contract::from_b58check("KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq").unwrap(),
            "red".to_string(),
            1_u64,
        )
        .unwrap();
        make_transaction(ticket, "KT1BuEZtb68c1Q4yjtckcNjGELqWt56Xyesc", "default")
    }

    fn transaction_two() -> OutboxMessageTransaction<StringTicket> {
        let ticket = StringTicket::new(
            Contract::from_b58check("KT1BuEZtb68c1Q4yjtckcNjGELqWt56Xyesc").unwrap(),
            "yellow".to_string(),
            329_u64,
        )
        .unwrap();
        make_transaction(ticket, "KT1AaiUqbT3NmQts2w7ofY4vJviVchztiW4y", "another")
    }

    fn make_transaction(
        ticket: StringTicket,
        destination: &str,
        entrypoint: &str,
    ) -> OutboxMessageTransaction<StringTicket> {
        let parameters = ticket;
        let destination = Contract::from_b58check(destination).unwrap();
        let entrypoint = Entrypoint::try_from(entrypoint.to_string()).unwrap();

        OutboxMessageTransaction {
            parameters,
            destination,
            entrypoint,
        }
    }
}
