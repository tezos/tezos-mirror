// SPDX-FileCopyrightText: 2023 PK Lab <contact@pklab.io>
//
// SPDX-License-Identifier: MIT

//! FA token deposit.
//!
//! Represents a ticket transfer from L1 to L2 that has:
//!     * Arbitrary ticketer (excluding whitelisted native token)
//!     * Standard ticket content (FA2.1 compatible)
//!     * Additional routing info parameter (raw bytes)
//!
//! It has several implicit constraints:
//!     * Total token supply must fit into U256
//!       (losses are possible otherwise)
//!     * Routing info must contain valid receiver address, user has access to
//!       (otherwise funds are forever lost)
//!
//! User can optionally specify an address of a valid existing proxy contract,
//! which is typically an ERC wrapper contract. Any runtime errors related
//! to that contract will be handled and user will still be able to withdraw
//! funds.
//!
//! Given the permissionless nature of the bridge (anyone can bridge any token),
//! these constraints can only be checked on the client side.
//!
//! A special deposit event is emitted upon the successful transfer
//! (regardless of the inner proxy contract call result) and can be used for
//! indexing.

use primitive_types::{H160, H256, U256};
use rlp::{Encodable, RlpDecodable, RlpEncodable};
use sha3::{Digest, Keccak256};
use tezos_data_encoding::enc::BinWriter;
use tezos_ethereum::Log;
use tezos_smart_rollup_encoding::michelson::{ticket::FA2_1Ticket, MichelsonBytes};

use crate::{
    abi::{ABI_H160_LEFT_PADDING, ABI_U32_LEFT_PADDING},
    utilities::{bigint_to_u256, keccak256_hash},
};

use super::error::FaBridgeError;

/// Keccak256 of deposit(address,uint256,uint256), first 4 bytes
/// This is function selector: https://docs.soliditylang.org/en/latest/abi-spec.html#function-selector
pub const FA_PROXY_DEPOSIT_METHOD_ID: &[u8; 4] = b"\x0e\xfe\x6a\x8b";

/// Keccak256 of Deposit(uint256,address,address,uint256,uint256,uint256)
/// This is main topic (non-anonymous event): https://docs.soliditylang.org/en/latest/abi-spec.html#events
pub const FA_DEPOSIT_EVENT_TOPIC: &[u8; 32] = b"\
    \x7e\xe7\xa1\xde\x9c\x18\xce\x69\x5c\x95\xb8\xb1\x9f\xbd\xf2\x6c\
    \xce\x35\x44\xe3\xca\x9e\x08\xc9\xf4\x87\x77\x67\x83\xd7\x59\x9f";

/// Overapproximation for the typical FA ticket payload (ticketer address and content)
const TICKET_PAYLOAD_SIZE_HINT: usize = 200;

/// Deposit structure parsed from the inbox message
#[derive(Debug, PartialEq, Clone, RlpEncodable, RlpDecodable)]
pub struct FaDeposit {
    /// Original ticket transfer amount
    pub amount: U256,
    /// Final deposit receiver address on L2
    pub receiver: H160,
    /// Optional proxy contract address on L2 (ERC wrapper)
    pub proxy: Option<H160>,
    /// Digest of the pair (ticketer address + ticket content)
    pub ticket_hash: H256,
    /// Inbox level containing the original deposit message
    pub inbox_level: u32,
    /// Inbox message id (can be used for tracking and as nonce)
    pub inbox_msg_id: u32,
}

impl FaDeposit {
    /// Tries to parse FA deposit given encoded parameters
    pub fn try_parse(
        ticket: FA2_1Ticket,
        routing_info: MichelsonBytes,
        inbox_level: u32,
        inbox_msg_id: u32,
    ) -> Result<Self, FaBridgeError> {
        let amount = bigint_to_u256(ticket.amount())?;
        let (receiver, proxy) = parse_l2_routing_info(routing_info)?;
        let ticket_hash = ticket_hash(&ticket)?;

        Ok(FaDeposit {
            amount,
            receiver,
            proxy,
            ticket_hash,
            inbox_level,
            inbox_msg_id,
        })
    }

    /// Returns calldata for the proxy (ERC wrapper) contract.
    ///
    /// Signature: deposit(address,uint256,uint256)
    pub fn calldata(&self) -> Vec<u8> {
        let mut call_data = Vec::with_capacity(100);
        call_data.extend_from_slice(FA_PROXY_DEPOSIT_METHOD_ID);

        call_data.extend_from_slice(&ABI_H160_LEFT_PADDING);
        call_data.extend_from_slice(&self.receiver.0);
        debug_assert!((call_data.len() - 4) % 32 == 0);

        call_data.extend_from_slice(&Into::<[u8; 32]>::into(self.amount));
        debug_assert!((call_data.len() - 4) % 32 == 0);

        call_data.extend_from_slice(self.ticket_hash.as_bytes());
        debug_assert!((call_data.len() - 4) % 32 == 0);

        call_data
    }

    /// Returns log structure for an implicit deposit event.
    ///
    /// This event is added to the outer transaction receipt,
    /// so that we can index successful deposits and update status.
    /// Ticket owner can be either proxy contract or receiver
    /// (if proxy is not specified or proxy call failed).
    ///
    /// Signature: Deposit(uint256,address,address,uint256,uint256,uint256)
    pub fn event_log(&self, ticket_owner: &H160) -> Log {
        let mut data = Vec::with_capacity(5 * 32);

        data.extend_from_slice(&ABI_H160_LEFT_PADDING);
        data.extend_from_slice(&ticket_owner.0);
        debug_assert!(data.len() % 32 == 0);

        data.extend_from_slice(&ABI_H160_LEFT_PADDING);
        data.extend_from_slice(&self.receiver.0);
        debug_assert!(data.len() % 32 == 0);

        data.extend_from_slice(&Into::<[u8; 32]>::into(self.amount));
        debug_assert!(data.len() % 32 == 0);

        data.extend_from_slice(&ABI_U32_LEFT_PADDING);
        data.extend_from_slice(&self.inbox_level.to_be_bytes());
        debug_assert!(data.len() % 32 == 0);

        data.extend_from_slice(&ABI_U32_LEFT_PADDING);
        data.extend_from_slice(&self.inbox_msg_id.to_be_bytes());
        debug_assert!(data.len() % 32 == 0);

        Log {
            // Emitted by the "system" contract
            address: H160::zero(),
            // Event ID (non-anonymous) and indexed fields
            topics: vec![H256(*FA_DEPOSIT_EVENT_TOPIC), self.ticket_hash],
            // Non-indexed fields
            data,
        }
    }

    /// Returns unique deposit digest that can be used as hash for the
    /// pseudo transaction.
    pub fn hash(&self, seed: &[u8]) -> H256 {
        let mut hasher = Keccak256::new();
        hasher.update(&self.rlp_bytes());
        hasher.update(seed);
        H256(hasher.finalize().into())
    }

    /// Formats FA deposit structure for logging purposes.
    pub fn display(&self) -> String {
        format!(
            "FA deposit {} of {} for {} via {:?}",
            self.amount, self.ticket_hash, self.receiver, self.proxy
        )
    }
}

/// Split routing info (raw bytes passed along with the ticket) into receiver and optional proxy addresses.
fn parse_l2_routing_info(
    routing_info: MichelsonBytes,
) -> Result<(H160, Option<H160>), FaBridgeError> {
    if routing_info.0.len() == 20 {
        Ok((H160::from_slice(&routing_info.0), None))
    } else if routing_info.0.len() == 40 {
        Ok((
            H160::from_slice(&routing_info.0[..20]),
            Some(H160::from_slice(&routing_info.0[20..])),
        ))
    } else {
        Err(FaBridgeError::InvalidRoutingInfo("invalid length"))
    }
}

/// Calculate unique ticket hash out of the ticket identifier (ticketer address and content).
///
/// Computed as Keccak256(ticketer || content) where
///  * ticketer: contract is in its forged form [ 0x01 | 20 bytes | 0x00 ]
///  * content: Micheline expression is in its forged form, legacy optimized mode
///
/// Solidity equivalent: uint256(keccak256(abi.encodePacked(ticketer, content)));
pub fn ticket_hash(ticket: &FA2_1Ticket) -> Result<H256, FaBridgeError> {
    let mut payload = Vec::with_capacity(TICKET_PAYLOAD_SIZE_HINT);
    ticket.creator().0.bin_write(&mut payload)?;
    ticket.contents().bin_write(&mut payload)?;
    Ok(keccak256_hash(&payload))
}

#[cfg(test)]
mod tests {
    use std::str::FromStr;

    use num_bigint::BigInt;
    use sha3::{Digest, Keccak256};
    use tezos_crypto_rs::hash::ContractKt1Hash;
    use tezos_smart_rollup_encoding::{
        contract::Contract,
        michelson::{MichelsonNat, MichelsonOption, MichelsonPair},
    };

    use super::*;

    fn create_fa_ticket(
        ticketer: &str,
        token_id: u64,
        metadata: &[u8],
        amount: BigInt,
    ) -> FA2_1Ticket {
        let creator =
            Contract::Originated(ContractKt1Hash::from_base58_check(ticketer).unwrap());
        let contents = MichelsonPair(
            MichelsonNat::new(BigInt::from(token_id).into()).unwrap(),
            MichelsonOption(Some(MichelsonBytes(metadata.to_vec()))),
        );
        FA2_1Ticket::new(creator, contents, amount).unwrap()
    }

    #[test]
    fn fa_deposit_parsing_success() {
        let ticket =
            create_fa_ticket("KT18amZmM5W7qDWVt2pH6uj7sCEd3kbzLrHT", 1, &[0u8], 2.into());
        let routing_info = MichelsonBytes([[1u8; 20], [0u8; 20]].concat().to_vec());
        let deposit =
            FaDeposit::try_parse(ticket, routing_info, 1, 0).expect("Failed to parse");

        pretty_assertions::assert_eq!(
            deposit,
            FaDeposit {
                amount: 2.into(),
                proxy: Some(H160([0u8; 20])),
                receiver: H160([1u8; 20]),
                inbox_level: 1,
                inbox_msg_id: 0,
                ticket_hash: H256::from_str(
                    "e0027297584c9e4162c872e072f1cc75b527023f9c0eda44ad4c732762b0b897"
                )
                .unwrap(),
            }
        )
    }

    #[test]
    fn fa_deposit_parsing_error_amount_overflow() {
        let ticket = create_fa_ticket(
            "KT1TxqZ8QtKvLu3V3JH7Gx58n7Co8pgtpQU5",
            1,
            &[0u8],
            BigInt::from_bytes_be(num_bigint::Sign::Plus, &[1u8; 64]),
        );
        let routing_info = MichelsonBytes([0u8; 40].to_vec());

        let res = FaDeposit::try_parse(ticket, routing_info, 1, 0);

        match res {
            Err(FaBridgeError::PrimitiveType(_)) => (),
            _ => panic!("Expected overflow error"),
        }
    }

    #[test]
    fn fa_deposit_parsing_error_invalid_routing() {
        let ticket =
            create_fa_ticket("KT1TxqZ8QtKvLu3V3JH7Gx58n7Co8pgtpQU5", 1, &[0u8], 1.into());
        let routing_info = MichelsonBytes([0u8; 10].to_vec());

        let res = FaDeposit::try_parse(ticket, routing_info, 1, 0);

        match res {
            Err(FaBridgeError::InvalidRoutingInfo(_)) => (),
            _ => panic!("Expected routing error"),
        }
    }

    #[test]
    fn fa_deposit_routing_does_not_contain_proxy() {
        let ticket =
            create_fa_ticket("KT1TxqZ8QtKvLu3V3JH7Gx58n7Co8pgtpQU5", 1, &[0u8], 1.into());
        let routing_info = MichelsonBytes([0u8; 20].to_vec());

        let res = FaDeposit::try_parse(ticket, routing_info, 1, 0).unwrap();
        assert_eq!(res.receiver, H160::zero());
        assert!(res.proxy.is_none());
    }

    #[test]
    fn fa_deposit_erc_calldata_consistent() {
        // Use this data to ensure consistency with the ERC wrapper contract
        let deposit = FaDeposit {
            amount: 1.into(),
            proxy: Some(H160([2u8; 20])),
            inbox_level: 3,
            inbox_msg_id: 0,
            receiver: H160([4u8; 20]),
            ticket_hash: H256::from_str(
                "12fb6647075cb9289e40af5560ce27a462ec2e49046b98298cdb41c9f128fb89",
            )
            .unwrap(),
        };

        assert_eq!(
            FA_PROXY_DEPOSIT_METHOD_ID.to_vec(),
            Keccak256::digest(b"deposit(address,uint256,uint256)").to_vec()[..4]
        );

        pretty_assertions::assert_eq!(
            hex::encode(deposit.calldata()),
            "0efe6a8b\
            0000000000000000000000000404040404040404040404040404040404040404\
            0000000000000000000000000000000000000000000000000000000000000001\
            12fb6647075cb9289e40af5560ce27a462ec2e49046b98298cdb41c9f128fb89"
        );
    }

    #[test]
    fn fa_deposit_event_log_consistent() {
        // Use this data to ensure consistency with the ERC wrapper contract
        let deposit = FaDeposit {
            amount: 1.into(),
            proxy: Some(H160([2u8; 20])),
            inbox_level: 3,
            inbox_msg_id: 43775,
            receiver: H160([4u8; 20]),
            ticket_hash: H256::from_str(
                "0000000000000000000000000000000000000000000000000000000000000000",
            )
            .unwrap(),
        };
        let event_log = deposit.event_log(&deposit.proxy.unwrap());

        pretty_assertions::assert_eq!(
            hex::encode(&event_log.data),
            "0000000000000000000000000202020202020202020202020202020202020202\
            0000000000000000000000000404040404040404040404040404040404040404\
            0000000000000000000000000000000000000000000000000000000000000001\
            0000000000000000000000000000000000000000000000000000000000000003\
            000000000000000000000000000000000000000000000000000000000000aaff"
        );

        assert_eq!(
            FA_DEPOSIT_EVENT_TOPIC.to_vec(),
            Keccak256::digest(
                b"Deposit(uint256,address,address,uint256,uint256,uint256)"
            )
            .to_vec()
        );
        assert!(event_log.address.is_zero());
    }

    #[test]
    fn ticket_payload_size_overapproximation() {
        let ticket = create_fa_ticket(
            "KT1TxqZ8QtKvLu3V3JH7Gx58n7Co8pgtpQU5",
            1000000,
            b"{\"contract_address\":\"KT1TxqZ8QtKvLu3V3JH7Gx58n7Co8pgtpQU5\",\"token_type\":\"FA2.0\",\"token_id\":\"1000000\",\"decimals\":\"12\",\"symbol\":\"USDQ\"}",
            BigInt::from_bytes_be(num_bigint::Sign::Plus, &[1u8; 64]),
        );
        let mut payload = Vec::new();
        ticket.creator().0.bin_write(&mut payload).unwrap();
        ticket.contents().bin_write(&mut payload).unwrap();
        assert!(payload.len() < TICKET_PAYLOAD_SIZE_HINT);
    }
}
