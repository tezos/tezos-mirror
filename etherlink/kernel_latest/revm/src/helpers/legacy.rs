// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use crate::error::EvmKernelError;
pub use evm_types::FaDepositWithProxy;
use num_bigint::BigInt;
use primitive_types::{H160, H256, U256 as PU256};
use revm::primitives::{alloy_primitives::Keccak256, Address, Log, B256, U256};
use rlp::{Encodable, RlpDecodable, RlpEncodable};
use tezos_data_encoding::enc::BinWriter;
use tezos_smart_rollup_encoding::michelson::{ticket::FA2_1Ticket, MichelsonBytes};

/// Deposit structure parsed from the inbox message
#[derive(Debug, PartialEq, Clone, RlpEncodable, RlpDecodable)]
pub struct FaDeposit {
    /// Original ticket transfer amount
    pub amount: PU256,
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

    /// Returns unique deposit digest that can be used as hash for the
    /// pseudo transaction.
    pub fn hash(&self, seed: &[u8]) -> H256 {
        let mut hasher = Keccak256::new();
        hasher.update(self.rlp_bytes());
        hasher.update(seed);
        H256(hasher.finalize().into())
    }

    /// Tries to parse FA deposit given encoded parameters
    pub fn try_parse(
        ticket: FA2_1Ticket,
        routing_info: MichelsonBytes,
        inbox_level: u32,
        inbox_msg_id: u32,
    ) -> Result<(Self, Option<PU256>), EvmKernelError> {
        let amount = bigint_to_u256(ticket.amount())
            .map_err(|_| EvmKernelError::TicketAmountOverflow)?;
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

    /// Formats FA deposit structure for logging purposes.
    pub fn display(&self) -> String {
        format!(
            "FA deposit {} of {} for {} via {:?}",
            self.amount, self.ticket_hash, self.receiver, self.proxy
        )
    }
}

const RECEIVER_LENGTH: usize = std::mem::size_of::<H160>();

const RECEIVER_AND_PROXY_LENGTH: usize = RECEIVER_LENGTH + std::mem::size_of::<H160>();

const RECEIVER_PROXY_AND_CHAIN_ID_LENGTH: usize =
    RECEIVER_AND_PROXY_LENGTH + std::mem::size_of::<U256>();

/// Split routing info (raw bytes passed along with the ticket) into receiver and optional proxy address and chain id.
fn parse_l2_routing_info(
    routing_info: MichelsonBytes,
) -> Result<(H160, Option<H160>, Option<PU256>), EvmKernelError> {
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
            Some(PU256::from_little_endian(
                &routing_info.0[RECEIVER_AND_PROXY_LENGTH..],
            )),
        ))
    } else {
        Err(EvmKernelError::InvalidRoutingInfoLength {
            actual: routing_info_len,
        })
    }
}

/// Overapproximation for the typical FA ticket payload (ticketer address and content)
const TICKET_PAYLOAD_SIZE_HINT: usize = 200;

/// Calculate unique ticket hash out of the ticket identifier (ticketer address and content).
///
/// Computed as Keccak256(ticketer || content) where
///  * ticketer: contract is in its forged form [ 0x01 | 20 bytes | 0x00 ]
///  * content: Micheline expression is in its forged form, legacy optimized mode
///
/// Solidity equivalent: uint256(keccak256(abi.encodePacked(ticketer, content)));
pub fn ticket_hash(ticket: &FA2_1Ticket) -> Result<H256, EvmKernelError> {
    let mut payload = Vec::with_capacity(TICKET_PAYLOAD_SIZE_HINT);
    ticket
        .creator()
        .0
        .bin_write(&mut payload)
        .map_err(|_| EvmKernelError::TicketCreatorEncoding)?;
    ticket
        .contents()
        .bin_write(&mut payload)
        .map_err(|_| EvmKernelError::TicketContentsEncoding)?;
    let mut hasher = Keccak256::new();
    hasher.update(&payload);
    Ok(H256(hasher.finalize().into()))
}

pub fn u256_to_alloy(value: &PU256) -> U256 {
    let mut bytes = [0u8; 32];
    value.to_little_endian(&mut bytes);
    U256::from_le_bytes::<32>(bytes)
}

pub fn h160_to_alloy(value: &H160) -> Address {
    Address::from_slice(&value.to_fixed_bytes())
}

pub fn alloy_to_u256(value: &U256) -> PU256 {
    PU256(value.into_limbs())
}

pub fn alloy_to_h160(value: &Address) -> H160 {
    H160::from_slice(value.as_slice())
}

pub fn alloy_to_log(value: &Log) -> ethereum::Log {
    ethereum::Log {
        address: H160(**value.address),
        topics: value
            .data
            .topics()
            .iter()
            .map(|topic| H256(**topic))
            .collect(),
        data: value.data.data.to_vec(),
    }
}

pub fn h256_to_alloy(value: &H256) -> B256 {
    B256::from_slice(&value.to_fixed_bytes())
}

pub fn alloy_to_h256(value: &B256) -> H256 {
    H256::from_slice(value.as_slice())
}

/// Convert a kernel-side access list (`Vec<tezos_ethereum::access_list::AccessListItem>`)
/// into the revm-native `AccessList`.
pub fn access_list_to_revm(
    access_list: Vec<tezos_ethereum::access_list::AccessListItem>,
) -> revm::context::transaction::AccessList {
    revm::context::transaction::AccessList::from(
        access_list
            .into_iter()
            .map(|item| revm::context::transaction::AccessListItem {
                address: Address::from_slice(&item.address.0),
                storage_keys: item
                    .storage_keys
                    .into_iter()
                    .map(|key| B256::from_slice(&key.0))
                    .collect(),
            })
            .collect::<Vec<revm::context::transaction::AccessListItem>>(),
    )
}

fn bigint_to_u256(value: &BigInt) -> Result<PU256, primitive_types::Error> {
    let (_, bytes) = value.to_bytes_le();
    if bytes.len() > 32 {
        return Err(primitive_types::Error::Overflow);
    }
    Ok(PU256::from_little_endian(&bytes))
}
