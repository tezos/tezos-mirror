// SPDX-FileCopyrightText: 2022-2023 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Generation of arbitrary operations for testing.
use crate::{fake_hash::arb_kt1, inbox::Signer};
use crypto::hash::HashTrait;
use tezos_smart_rollup_encoding::{entrypoint::Entrypoint, michelson::ticket::StringTicket};

use super::{Operation, OperationContent};
use crypto::hash::ContractTz1Hash;
use crypto::hash::HashType;
use crypto::hash::SecretKeyEd25519;
use crypto::hash::SeedEd25519;
use crypto::PublicKeyWithHash;
use proptest::prelude::*;

impl OperationContent {
    /// Generation strategy for withdrawal operations.
    pub fn arb_withdrawal() -> BoxedStrategy<OperationContent> {
        (arb_kt1(), StringTicket::arb(), Entrypoint::arb())
            .prop_map(|(destination, ticket, entrypoint)| {
                OperationContent::withdrawal(destination, ticket, entrypoint)
            })
            .boxed()
    }

    /// Generation strategy for transfer operations.
    pub fn arb_transfer() -> BoxedStrategy<OperationContent> {
        (
            any::<[u8; HashType::ContractTz1Hash.size()]>(),
            StringTicket::arb(),
        )
            .prop_map(|(hash, ticket)| {
                let amount = ticket.amount_as().unwrap();
                let ticket_hash = ticket.hash().unwrap();
                OperationContent::transfer(
                    ContractTz1Hash::try_from_bytes(&hash).unwrap(),
                    ticket_hash,
                    amount,
                )
                .unwrap()
            })
            .boxed()
    }
}

impl Operation {
    /// Generation strategy for operations.
    pub fn arb_with_signer() -> BoxedStrategy<(Operation, SecretKeyEd25519)> {
        (
            any::<[u8; HashType::SeedEd25519.size()]>()
                .prop_map(|s| SeedEd25519::try_from_bytes(&s).unwrap()),
            i64::arbitrary().prop_map(|i| if i < 0 { -(i + 1) } else { i }),
            // Mix of withdrawals & transfers
            bool::arbitrary().prop_flat_map(|transfer| {
                if transfer {
                    OperationContent::arb_transfer()
                } else {
                    OperationContent::arb_withdrawal()
                }
            }),
            bool::arbitrary(),
        )
            .prop_map(|(seed, counter, contents, signer_as_address)| {
                let (pk, sk) = seed.keypair().unwrap();
                let signer = if signer_as_address {
                    Signer::Tz1(pk.pk_hash())
                } else {
                    Signer::PublicKey(pk)
                };

                let op = Operation {
                    signer,
                    counter,
                    contents,
                };
                (op, sk)
            })
            .boxed()
    }
}
