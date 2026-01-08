// SPDX-FileCopyrightText: 2023 PK Lab <contact@pklab.io>
// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
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

use alloy_primitives::bytes::BytesMut;
use alloy_sol_types::SolEvent;
use primitive_types::{H160, H256, U256};
use rlp::{Encodable, RlpDecodable, RlpEncodable, RlpStream};
use sha3::{Digest, Keccak256};
use tezos_data_encoding::enc::BinWriter;
use tezos_ethereum::Log;
use tezos_smart_rollup_encoding::michelson::{ticket::FA2_1Ticket, MichelsonBytes};

use crate::{
    precompiles::FA_BRIDGE_PRECOMPILE_ADDRESS,
    utilities::{
        alloy::{h160_to_alloy, u256_to_alloy},
        bigint_to_u256, keccak256_hash,
    },
};

use super::error::FaBridgeError;

/// Keccak256 of deposit(address,uint256,uint256), first 4 bytes
/// This is function selector: https://docs.soliditylang.org/en/latest/abi-spec.html#function-selector
pub const FA_PROXY_DEPOSIT_METHOD_ID: &[u8; 4] = b"\x0e\xfe\x6a\x8b";

pub const FA_CLAIM_METHOD_ID: &[u8; 4] = b"\x37\x96\x07\xf5";

/// Keccak256 of Deposit(uint256,address,address,uint256,uint256,uint256)
/// This is main topic (non-anonymous event): https://docs.soliditylang.org/en/latest/abi-spec.html#events
pub const FA_DEPOSIT_EVENT_TOPIC: &[u8; 32] = b"\
    \x7e\xe7\xa1\xde\x9c\x18\xce\x69\x5c\x95\xb8\xb1\x9f\xbd\xf2\x6c\
    \xce\x35\x44\xe3\xca\x9e\x08\xc9\xf4\x87\x77\x67\x83\xd7\x59\x9f";

/// Keccak256 of QueuedDeposit(uint256,address,uint256,uint256,uint256)
pub const FA_QUEUED_DEPOSIT_EVENT_TOPIC: &[u8; 32] =
    b"\x27\xa8\x8c\x03\x46\x49\x43\x4b\x9c\x0d\xd0\xbf\x36\xed\x46\x82\
      \x2c\xad\x64\x27\xda\xb6\x9d\x15\x87\x0d\xa9\x5c\x0e\x06\x9a\xcd";

/// Overapproximation for the typical FA ticket payload (ticketer address and content)
const TICKET_PAYLOAD_SIZE_HINT: usize = 200;

alloy_sol_types::sol! {
    event SolDepositProxyCallData (
        address receiver,
        uint256 amount,
        uint256 ticket_hash,
    );
}

alloy_sol_types::sol! {
    event SolDepositEvent (
        address ticket_owner,
        address receiver,
        uint256 amount,
        uint256 inbox_level,
        uint256 inbox_msg_id,
    );
}

alloy_sol_types::sol! {
    event SolQueuedDepositEvent (
        uint256 nonce,
        address receiver,
        uint256 amount,
        uint256 inbox_level,
        uint256 inbox_msg_id,
    );
}

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
    ) -> Result<(Self, Option<U256>), FaBridgeError> {
        let amount = bigint_to_u256(ticket.amount())?;
        let (receiver, proxy, chain_id) = parse_l2_routing_info(routing_info)?;
        let ticket_hash = ticket_hash(&ticket)?;

        Ok((
            FaDeposit {
                amount,
                receiver,
                proxy,
                ticket_hash,
                inbox_level,
                inbox_msg_id,
            },
            chain_id,
        ))
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
        let event_data = SolDepositEvent {
            ticket_owner: h160_to_alloy(ticket_owner),
            receiver: h160_to_alloy(&self.receiver),
            amount: u256_to_alloy(&self.amount).unwrap_or_default(),
            inbox_level: u256_to_alloy(&U256::from(self.inbox_level)).unwrap_or_default(),
            inbox_msg_id: u256_to_alloy(&U256::from(self.inbox_msg_id))
                .unwrap_or_default(),
        };

        let data = SolDepositEvent::encode_data(&event_data);

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
        hasher.update(self.rlp_bytes());
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

    pub fn to_fa_deposit_with_proxy(&self) -> Option<FaDepositWithProxy> {
        self.proxy.map(|proxy| FaDepositWithProxy {
            amount: self.amount,
            receiver: self.receiver,
            proxy,
            ticket_hash: self.ticket_hash,
            inbox_level: self.inbox_level,
            inbox_msg_id: self.inbox_msg_id,
        })
    }
}

/// Deposit structure decoded and encoded to the deposit queue
#[derive(Debug, PartialEq, Clone, RlpEncodable, RlpDecodable)]
pub struct FaDepositWithProxy {
    /// Original ticket transfer amount
    pub amount: U256,
    /// Final deposit receiver address on L2
    pub receiver: H160,
    /// Optional proxy contract address on L2 (ERC wrapper)
    pub proxy: H160,
    /// Digest of the pair (ticketer address + ticket content)
    pub ticket_hash: H256,
    /// Inbox level containing the original deposit message
    pub inbox_level: u32,
    /// Inbox message id (can be used for tracking and as nonce)
    pub inbox_msg_id: u32,
}

impl FaDepositWithProxy {
    pub fn to_fa_deposit(&self) -> FaDeposit {
        FaDeposit {
            amount: self.amount,
            receiver: self.receiver,
            proxy: Some(self.proxy),
            ticket_hash: self.ticket_hash,
            inbox_level: self.inbox_level,
            inbox_msg_id: self.inbox_msg_id,
        }
    }

    pub fn to_rlp_bytes(&self) -> BytesMut {
        let mut stream = RlpStream::new();
        stream.append(self);
        stream.out()
    }

    /// Returns calldata for the proxy (ERC wrapper) contract.
    ///
    /// Signature: deposit(address,uint256,uint256)
    pub fn calldata(&self) -> Vec<u8> {
        let mut call_data = Vec::with_capacity(100);
        call_data.extend_from_slice(FA_PROXY_DEPOSIT_METHOD_ID);

        let calldata_ = SolDepositProxyCallData {
            receiver: h160_to_alloy(&self.receiver),
            amount: u256_to_alloy(&self.amount).unwrap_or_default(),
            ticket_hash: u256_to_alloy(&U256::from_big_endian(
                self.ticket_hash.as_bytes(),
            ))
            .unwrap_or_default(),
        };

        let data = SolDepositProxyCallData::encode_data(&calldata_);
        call_data.extend_from_slice(&data);

        call_data
    }

    pub fn queued_log(&self, withdrawal_id: &U256) -> Log {
        let queued_data = SolQueuedDepositEvent {
            nonce: u256_to_alloy(withdrawal_id).unwrap_or_default(),
            receiver: h160_to_alloy(&self.receiver),
            amount: u256_to_alloy(&self.amount).unwrap_or_default(),
            inbox_level: u256_to_alloy(&U256::from(self.inbox_level)).unwrap_or_default(),
            inbox_msg_id: u256_to_alloy(&U256::from(self.inbox_msg_id))
                .unwrap_or_default(),
        };

        let data = queued_data.encode_data();

        let mut topics_proxy = [0u8; 32];
        topics_proxy[12..].copy_from_slice(self.proxy.as_bytes());

        Log {
            // Emitted by the fa bridge precompile contract
            address: FA_BRIDGE_PRECOMPILE_ADDRESS,
            // Event ID (non-anonymous) and indexed fields
            topics: vec![
                H256(*FA_QUEUED_DEPOSIT_EVENT_TOPIC),
                self.ticket_hash,
                H256(topics_proxy),
            ],
            // Non-indexed fields
            data,
        }
    }
}

const RECEIVER_LENGTH: usize = std::mem::size_of::<H160>();

const RECEIVER_AND_PROXY_LENGTH: usize = RECEIVER_LENGTH + std::mem::size_of::<H160>();

const RECEIVER_PROXY_AND_CHAIN_ID_LENGTH: usize =
    RECEIVER_AND_PROXY_LENGTH + std::mem::size_of::<U256>();

/// Split routing info (raw bytes passed along with the ticket) into receiver and optional proxy address and chain id.
fn parse_l2_routing_info(
    routing_info: MichelsonBytes,
) -> Result<(H160, Option<H160>, Option<U256>), FaBridgeError> {
    let routing_info_len = routing_info.0.len();
    if routing_info_len == RECEIVER_LENGTH {
        Ok((H160::from_slice(&routing_info.0), None, None))
    } else if routing_info_len == RECEIVER_AND_PROXY_LENGTH {
        Ok((
            H160::from_slice(&routing_info.0[..RECEIVER_LENGTH]),
            Some(H160::from_slice(&routing_info.0[RECEIVER_LENGTH..])),
            None,
        ))
    } else if routing_info_len == RECEIVER_PROXY_AND_CHAIN_ID_LENGTH {
        Ok((
            H160::from_slice(&routing_info.0[..RECEIVER_LENGTH]),
            Some(H160::from_slice(
                &routing_info.0[RECEIVER_LENGTH..RECEIVER_AND_PROXY_LENGTH],
            )),
            Some(U256::from_little_endian(
                &routing_info.0[RECEIVER_AND_PROXY_LENGTH..],
            )),
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

    use crate::fa_bridge::test_utils::create_fa_ticket;
    use num_bigint::BigInt;
    use sha3::{Digest, Keccak256};

    use super::*;

    #[test]
    fn fa_deposit_parsing_success_no_chain_id() {
        let ticket =
            create_fa_ticket("KT18amZmM5W7qDWVt2pH6uj7sCEd3kbzLrHT", 1, &[0u8], 2.into());
        let routing_info = MichelsonBytes([[1u8; 20], [0u8; 20]].concat().to_vec());
        let (deposit, chain_id) =
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
        );
        pretty_assertions::assert_eq!(chain_id, None)
    }

    #[test]
    fn fa_deposit_parsing_success() {
        let ticket =
            create_fa_ticket("KT18amZmM5W7qDWVt2pH6uj7sCEd3kbzLrHT", 1, &[0u8], 2.into());
        let mut routing_info = vec![];
        routing_info.extend([1u8; 20]);
        routing_info.extend([0u8; 20]);
        routing_info.extend([1u8]);
        routing_info.extend([0u8; 31]);
        let routing_info = MichelsonBytes(routing_info);
        let (deposit, chain_id) =
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
                    "e0027297584c9e4162c872e072f1cc75b527023f9c0eda44ad4c732762b0b897",
                )
                .unwrap(),
            }
        );
        pretty_assertions::assert_eq!(chain_id, Some(U256::one()))
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

        let (res, _chain_id) = FaDeposit::try_parse(ticket, routing_info, 1, 0).unwrap();
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
            hex::encode(deposit.to_fa_deposit_with_proxy().unwrap().calldata()),
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

        assert_eq!(
            FA_CLAIM_METHOD_ID.to_vec(),
            Keccak256::digest(b"claim(uint256)")[..4].to_vec()
        );

        assert_eq!(
            FA_QUEUED_DEPOSIT_EVENT_TOPIC.to_vec(),
            Keccak256::digest(b"QueuedDeposit(uint256,address,uint256,uint256,uint256)")
                .to_vec()
        );
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
