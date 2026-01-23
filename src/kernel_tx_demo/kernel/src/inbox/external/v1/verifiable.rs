// SPDX-FileCopyrightText: 2022-2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Parsing of operations, and verification that they are well-formed and signed.
use super::{
    Operation, OperationContent, OperationTransfer, OperationTransferCompressed, OperationWithdraw,
    TicketAmount, TicketIndex,
};
use crate::storage::get_or_set_ticket_id;
use crate::storage::Account;
use crate::storage::{account_path, AccountStorage, AccountStorageError};
use crate::transactions::withdrawal::Withdrawal;
use crypto::hash::ContractTz1Hash;
use crypto::CryptoError;
use crypto::PublicKeySignatureVerifier;
use nom::combinator::{consumed, map};
use nom::sequence::pair;
use num_bigint::{BigInt, TryFromBigIntError};
use tezos_crypto_rs::blake2b::Blake2bError;
use tezos_crypto_rs::hash::Ed25519Signature;
use tezos_data_encoding::nom::NomReader;
#[cfg(feature = "debug")]
use tezos_smart_rollup_debug::debug_msg;
use tezos_smart_rollup_encoding::michelson::ticket::TicketHashError;
use tezos_smart_rollup_host::path::OwnedPath;
use tezos_smart_rollup_host::runtime::Runtime;
use thiserror::Error;

/// Errors that may occur when verifying and executing transactions.
#[derive(Debug, Error)]
pub enum TransactionError {
    /// No account currently exists at address
    #[error("No account found at address {0}")]
    NoAccountOfAddress(ContractTz1Hash),
    /// No public key linked to address
    #[error("No public key linked to address {0}")]
    NoPublicKeyForAddress(ContractTz1Hash),
    /// The expected counter did not match the actual given.
    #[error("Account operation counter at {0}, transaction had {1}")]
    InvalidOperationCounter(i64, i64),
    /// Invalid ticket amount
    #[error("ticket amount out of range of u64 {0}")]
    InvalidAmount(TryFromBigIntError<BigInt>),
    /// Ticket amount cannot be zero
    #[error("Ticket amount must be greater than 0")]
    ZeroTicket,
    /// Could not identify ticket
    #[error("Unable to identify ticket {0}")]
    TicketIdentification(#[from] TicketHashError),
    /// Digest error
    #[error("Unable to digest operation: {0}")]
    Digest(#[from] Blake2bError),
    /// Unable to decompress public key
    #[error("Invalid signature or public key {0:?}")]
    InvalidSigOrPk(#[from] CryptoError),
    /// Incorrect signature
    #[error("Signature verification failed")]
    SignatureVerificationError,
    /// Cannot transfer from & to the same account.
    #[error("Invalid self-transfer at address {0}")]
    InvalidSelfTransfer(ContractTz1Hash),
    /// Error when using the account storage.
    #[error("Error when using durable account storage: {0}")]
    StorageError(AccountStorageError),
    /// Error when using the account storage API (getting and creating accounts).
    #[error("Error when using the transaction storage API {0}")]
    StorageAPIError(tezos_smart_rollup_storage::StorageError),
}

impl From<AccountStorageError> for TransactionError {
    fn from(error: AccountStorageError) -> Self {
        TransactionError::StorageError(error)
    }
}

impl From<tezos_smart_rollup_storage::StorageError> for TransactionError {
    fn from(error: tezos_smart_rollup_storage::StorageError) -> Self {
        TransactionError::StorageAPIError(error)
    }
}

impl From<TryFromBigIntError<BigInt>> for TransactionError {
    fn from(error: TryFromBigIntError<BigInt>) -> Self {
        TransactionError::InvalidAmount(error)
    }
}

/// Wrapper type for [Operation].
///
/// Contains the slice repressenting the binary encoding for the
/// operation - to be used with signature checking.
#[derive(Debug, PartialEq, Eq)]
pub struct VerifiableOperation<'a> {
    pub(crate) parsed: &'a [u8],
    pub(crate) operation: Operation,
    pub(crate) signature: Ed25519Signature,
}

impl<'a> VerifiableOperation<'a> {
    fn verify_sig(
        &self,
        host: &mut impl Runtime,
        account: &mut crate::storage::Account,
    ) -> Result<(), TransactionError> {
        let pk = match &self.operation.signer {
            crate::inbox::Signer::Tz1(address) => {
                if let Some(pk) = account.public_key(host)? {
                    pk
                } else {
                    return Err(TransactionError::NoPublicKeyForAddress(address.clone()));
                }
            }
            crate::inbox::Signer::PublicKey(pk) => {
                account.link_public_key(host, pk)?;
                pk.clone()
            }
        };

        pk.verify_signature(&self.signature, self.parsed)?;

        Ok(())
    }

    /// Execute the operation directly on durable storage. Assumes that a current
    /// transaction is in progress in durable storage and will operate on that. Also,
    /// assumes that the transaction will be rolled back, if an error is returned.
    pub fn execute<Host: Runtime>(
        self,
        host: &mut Host,
        account_storage: &mut AccountStorage,
    ) -> Result<Vec<Withdrawal>, TransactionError> {
        let signer_address = self.operation.signer.address();

        let signer_path: OwnedPath = account_path(&signer_address)?;

        let mut withdrawals = Vec::new();

        if let Some(mut signer_account) = account_storage.get(host, &signer_path)? {
            #[cfg(not(feature = "tx-kernel-no-sig-verif"))]
            self.verify_sig(host, &mut signer_account)?;

            signer_account.check_and_inc_counter(host, self.operation.counter)?;

            match self.operation.contents {
                OperationContent::Withdraw(OperationWithdraw {
                    destination,
                    entrypoint,
                    ticket,
                }) => {
                    let identity = ticket.hash()?;
                    let amount: u64 = ticket.amount_as()?;
                    if amount == 0 {
                        return Err(TransactionError::ZeroTicket);
                    }

                    let ticket_id = get_or_set_ticket_id(host, &identity)?;
                    signer_account.remove_ticket(host, ticket_id, amount)?;

                    #[cfg(feature = "debug")]
                    debug_msg!(host, "withdrawing {ticket:?} from {signer_address} to {destination}@{entrypoint:?}\n");

                    withdrawals.push(Withdrawal {
                        ticket,
                        destination,
                        entrypoint,
                        withdrawn_amount: amount,
                    })
                }

                OperationContent::Transfer(OperationTransfer { destination, .. })
                | OperationContent::CTransfer(OperationTransferCompressed {
                    destination, ..
                }) if destination == signer_address => {
                    return Err(TransactionError::InvalidSelfTransfer(destination));
                }

                OperationContent::Transfer(OperationTransfer {
                    destination,
                    ticket,
                    amount: TicketAmount { amount },
                }) => {
                    let ticket_id = get_or_set_ticket_id(host, &ticket)?;
                    handle_transfer(
                        host,
                        account_storage,
                        &mut signer_account,
                        destination,
                        ticket_id,
                        amount,
                    )?;
                }

                OperationContent::CTransfer(OperationTransferCompressed {
                    destination,
                    ticket: TicketIndex { index: ticket },
                    amount: TicketAmount { amount },
                }) => {
                    handle_transfer(
                        host,
                        account_storage,
                        &mut signer_account,
                        destination,
                        ticket,
                        amount,
                    )?;
                }
            }
        } else {
            return Err(TransactionError::NoAccountOfAddress(signer_address));
        }

        Ok(withdrawals)
    }

    /// Parse an operation, remembering the parsed slice.
    pub fn parse(input: &'a [u8]) -> tezos_data_encoding::nom::NomResult<'a, Self> {
        map(
            pair(consumed(Operation::nom_read), Ed25519Signature::nom_read),
            |((parsed, operation), signature)| Self {
                parsed,
                operation,
                signature,
            },
        )(input)
    }
}

fn handle_transfer(
    host: &mut impl Runtime,
    account_storage: &mut AccountStorage,
    signer_account: &mut Account,
    destination: ContractTz1Hash,
    ticket: u64,
    amount: u64,
) -> Result<(), TransactionError> {
    let dest_account_path: OwnedPath = account_path(&destination)?;

    let mut dest_account = account_storage.get_or_create(host, &dest_account_path)?;

    let _source_amount = signer_account.remove_ticket(host, ticket, amount)?;
    let _dest_amount = dest_account.add_ticket(host, ticket, amount)?;

    #[cfg(feature = "debug")]
    debug_msg!(host, "Transferring {amount} of {ticket} to {destination}\n");

    let _source_id = signer_account.get_or_set_id(host)?;

    let (_id, _amount) = if _source_id == 0 {
        (dest_account.get_or_set_id(host)? - 1, _dest_amount as u8)
    } else {
        (_source_id - 1, _source_amount as u8)
    };

    let [_id_0, _id_1] = _id.to_le_bytes();

    let _colour = match ticket {
        0 => b'R',
        1 => b'G',
        2 => b'B',
        a => {
            host.write_debug(&format!("Unknown ticket {a}"));
            return Ok(());
        }
    };

    #[cfg(not(feature = "debug"))]
    host.write_debug(unsafe { std::str::from_utf8_unchecked(&[_id_0, _id_1, _colour, _amount]) });

    Ok(())
}

/// The effects of an operation.
#[derive(Debug, PartialEq, Eq)]
pub struct VerifiedOperation {
    address: ContractTz1Hash,
    withdrawals: Vec<Withdrawal>,
}

#[cfg(test)]
mod test {
    use proptest::prelude::*;
    use tezos_data_encoding::enc::BinWriter;

    use crate::inbox::v1::Operation;

    use super::VerifiableOperation;
    use crypto::hash::HashType;

    proptest! {
        #[test]
        fn verifiable_operation_encode_decode(
            (operation, _key) in Operation::arb_with_signer(),
            sig in any::<[u8; HashType::Ed25519Signature.size()]>(),
            remaining_input in any::<Vec<u8>>(),
        ) {
            let mut encoded = Vec::new();
            operation.bin_write(&mut encoded).unwrap();

            encoded.extend_from_slice(sig.as_slice());
            encoded.extend_from_slice(remaining_input.as_slice());

            let (remaining, decoded) = VerifiableOperation::parse(encoded.as_slice()).unwrap();
            assert_eq!(remaining_input, remaining, "Incorrect remaining bytes");

            assert_eq!(operation, decoded.operation, "Operations do not match");
            assert_eq!(sig.as_slice(), decoded.signature.as_ref(), "Sigs do not match");
        }
    }
}
