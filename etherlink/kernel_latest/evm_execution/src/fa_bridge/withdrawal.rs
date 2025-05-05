// SPDX-FileCopyrightText: 2023 PK Lab <contact@pklab.io>
// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

//! FA token withdrawal.
//!
//! Represents a ticket transfer from L2 to L1 where:
//!     * The content is of standard type (FA2.1 compatible)
//!     * Destination is either an implicit account,
//!       or a smart contract implementing standard "withdraw" method
//!
//! It has several implicit constraints:
//!     * Routing info must contain valid receiver address, user has access to
//!       (otherwise funds are forever lost)
//!     * If a smart contract address is specified as target, it must be
//!       correct and expose a standard "withdraw" method, otherwise funds
//!       will be lost.
//!
//! Unlike FA deposits, we cannot handle runtime errors on Tezos L1
//! (at least in the current implementation).
//!
//! It should also be noted that in order to complete the withdrawal on L1
//! one must obtain the outbox message proof and explicitly submit it to Tezos L1.
//!
//! A special withdrawal event is emitted upon successful withdrawal request,
//! which can be used both for indexing and for retrieving information necessary
//! to generate outbox message proof (outbox level + message id).

use alloy_primitives::FixedBytes;
use alloy_sol_types::SolEvent;
use enum_dispatch::enum_dispatch;
use num_bigint::BigInt;
use primitive_types::{H160, H256, U256};
use tezos_data_encoding::{enc::BinWriter, nom::NomReader, types::Zarith};
use tezos_ethereum::Log;
use tezos_smart_rollup_encoding::{
    contract::Contract,
    entrypoint::Entrypoint,
    michelson::{
        ticket::FA2_1Ticket, MichelsonBytes, MichelsonContract, MichelsonNat,
        MichelsonOption, MichelsonPair, MichelsonTimestamp,
    },
    outbox::{OutboxMessage, OutboxMessageTransaction},
};

use crate::{
    handler::Withdrawal,
    precompiles::FA_BRIDGE_PRECOMPILE_ADDRESS,
    utilities::alloy::{alloy_to_h160, alloy_to_u256, h160_to_alloy, u256_to_alloy},
    utilities::{keccak256_hash, u256_to_bigint},
};

use super::error::FaBridgeError;

/// Keccak256 of withdraw(address,uint256,uint256), first 4 bytes
pub const WITHDRAW_METHOD_ID: &[u8; 4] = b"\xb5\xc5\xf6\x72";

/// Keccak256 of Withdrawal(uint256,address,address,bytes22,bytes22,uint256,uint256)
pub const WITHDRAW_EVENT_TOPIC: &[u8; 32] = b"\
    \xab\x68\x45\x0c\x9e\x54\x6f\x60\x62\xa8\x61\xee\xbf\x8e\xc5\xbb\
    \xd4\x1b\x44\x25\xe2\x6b\x20\x19\x9c\x91\x22\x7c\x7f\x90\x38\xca";

/// Keccak256 of FastFaWithdrawal(address,address,bytes22,bytes22,uint256,uint256,uint256,bytes)
pub const FAST_FA_WITHDRAWAL_EVENT_TOPIC: [u8; 32] = [
    0x47, 0x7f, 0x54, 0x5a, 0x1f, 0x30, 0xcd, 0x1f, 0xb8, 0x9c, 0x51, 0xee, 0x4f, 0xa6,
    0x7e, 0x3a, 0x56, 0xa1, 0xe3, 0x7e, 0x3d, 0x74, 0x2b, 0x84, 0xf6, 0xce, 0x73, 0xad,
    0x6e, 0x33, 0x45, 0x1f,
];

/// L1 proxy contract entrypoint that will be invoked by the outbox message
/// execution.
pub const WITHDRAW_ENTRYPOINT: &str = "withdraw";

alloy_sol_types::sol! {
    event SolStandardWithdrawalInput (
        address ticket_owner,
        bytes   routing_info,
        uint256 amount,
        bytes22 ticketer,
        bytes   content,
    );
}

alloy_sol_types::sol! {
    event SolFastWithdrawalInput (
        address ticket_owner,
        bytes   routing_info,
        uint256 amount,
        bytes22 ticketer,
        bytes   content,
        string  fast_withdrawal_contract_address,
        bytes   payload,
    );
}

alloy_sol_types::sol! {
    event SolStandardWithdrawalProxyCallData (
        address sender,
        uint256 amount,
        uint256 ticket_hash,
    );
}

alloy_sol_types::sol! {
    event SolStandardWithdrawalEvent (
        address sender,
        address ticket_owner,
        bytes22 receiver,
        bytes22 proxy,
        uint256 amount,
        uint256 withdrawal_id,
    );
}

alloy_sol_types::sol! {
    event SolFastFAWithdrawalEvent (
        address sender,
        address ticket_owner,
        bytes22 receiver,
        bytes22 proxy,
        uint256 amount,
        uint256 withdrawal_id,
        uint256 timestamp,
        bytes   payload,
    );
}

#[derive(Debug, PartialEq)]
pub struct SystemFaDeposit {
    /// Original ticket transfer amount
    pub amount: U256,
    /// Final deposit receiver address on L2
    pub receiver: H160,
    /// Optional proxy contract address on L2 (ERC wrapper)
    pub proxy: Option<H160>,
    /// Digest of the pair (ticketer address + ticket content)
    pub ticket_hash: H256,
}

/// Withdrawal structure parsed from the precompile calldata
#[derive(Debug, PartialEq)]
pub struct FaWithdrawal {
    /// Account that invoked the precompile (assuming the sender)
    pub sender: H160,
    /// Contract (either implicit or originated) that will receive tokens or tickets
    pub receiver: Contract,
    /// Proxy contract on L1 that can handle ticket + receiver address (mandatory for now)
    pub proxy: Contract,
    /// Ticket transfer amount
    pub amount: U256,
    /// FA2.1 compatible ticket, constructed from the input
    pub ticket: FA2_1Ticket,
    /// Etherlink compatible ticket digest
    pub ticket_hash: H256,
    /// Actual ticket owner in global table (can be either sender or an ERC wrapper)
    pub ticket_owner: H160,
}

#[enum_dispatch]
pub trait FaWithdrawalMethods {
    fn calldata(&self) -> Vec<u8>;

    fn event_log(&self, withdrawal_id: U256) -> Log;

    fn into_outbox_message(self, withdrawal_id: U256) -> Withdrawal;

    fn display(&self) -> String;

    fn amount(&self) -> U256;
    fn ticket_owner(&self) -> H160;
    fn ticket_hash(&self) -> H256;
    fn sender(&self) -> H160;
}

impl FaWithdrawal {
    /// Tries to parse withdrawal structure from the precompile call data,
    /// method id excluded (first 4 bytes).
    ///
    /// withdraw(
    ///     address ticketOwner,
    ///     bytes memory routingInfo,
    ///     uint256 amount,
    ///     bytes22 ticketer,
    ///     bytes memory content
    /// )
    pub fn try_parse(input_data: &[u8], sender: H160) -> Result<Self, FaBridgeError> {
        let (ticket_owner, routing_info, amount, ticketer, content) =
            SolStandardWithdrawalInput::abi_decode_data(input_data, true).map_err(
                |_| FaBridgeError::AbiDecodeError("Failed to parse precompile call data"),
            )?;

        let ticket_owner = alloy_to_h160(&ticket_owner).unwrap_or_default();
        let amount = alloy_to_u256(&amount);
        let ticket_hash = ticket_hash_from_raw_parts(ticketer.as_slice(), &content);
        let (receiver, proxy) = parse_l1_routing_info(&routing_info)?;
        let ticketer: [u8; 22] = ticketer.as_slice().try_into().unwrap_or_default();
        let ticket = construct_ticket(ticketer, &content, amount)?;

        Ok(Self {
            sender,
            receiver,
            proxy,
            amount,
            ticket,
            ticket_hash,
            ticket_owner,
        })
    }
}

impl FaWithdrawalMethods for FaWithdrawal {
    /// Returns calldata for the proxy (ERC wrapper) contract.
    ///
    /// Signature: withdraw(address,uint256,uint256)
    /// TODO: https://gitlab.com/tezos/tezos/-/issues/7844
    /// Find a way to share the implementation of `calldata` with `FaFastWithdrawal`
    fn calldata(&self) -> Vec<u8> {
        let mut call_data = Vec::with_capacity(100);
        call_data.extend_from_slice(WITHDRAW_METHOD_ID);

        let calldata_ = SolStandardWithdrawalProxyCallData {
            sender: h160_to_alloy(&self.sender),
            amount: u256_to_alloy(&self.amount).unwrap_or_default(),
            ticket_hash: u256_to_alloy(&U256::from_big_endian(
                self.ticket_hash.as_bytes(),
            ))
            .unwrap_or_default(),
        };

        let data = SolStandardWithdrawalProxyCallData::encode_data(&calldata_);
        call_data.extend_from_slice(&data);

        call_data
    }

    /// Returns log structure for an implicit withdrawal event.
    /// This event is added to the outer transaction receipt,
    /// so that we can index successful withdrawal requests.
    ///
    /// It also contains unique withdrawal identifier.
    ///
    /// Signature: Withdrawal(uint256,address,address,bytes22,bytes22,uint256,uint256)
    fn event_log(&self, withdrawal_id: U256) -> Log {
        let mut receiver_bytes = vec![];
        let mut proxy_bytes = vec![];

        // It is safe to unwrap, underlying implementation never fails (always returns Ok(()))
        self.receiver.bin_write(&mut receiver_bytes).unwrap();
        let receiver_bytes: [u8; 22] = receiver_bytes.try_into().unwrap();

        // It is safe to unwrap, underlying implementation never fails (always returns Ok(()))
        self.proxy.bin_write(&mut proxy_bytes).unwrap();
        let proxy_bytes: [u8; 22] = proxy_bytes.try_into().unwrap();

        let event_data = SolStandardWithdrawalEvent {
            sender: h160_to_alloy(&self.sender),
            ticket_owner: h160_to_alloy(&self.ticket_owner),
            receiver: FixedBytes::<22>::from(&receiver_bytes),
            proxy: FixedBytes::<22>::from(&proxy_bytes),
            amount: u256_to_alloy(&self.amount).unwrap_or_default(),
            withdrawal_id: u256_to_alloy(&withdrawal_id).unwrap_or_default(),
        };

        let data = SolStandardWithdrawalEvent::encode_data(&event_data);

        Log {
            address: FA_BRIDGE_PRECOMPILE_ADDRESS,
            topics: vec![H256(*WITHDRAW_EVENT_TOPIC), self.ticket_hash],
            data,
        }
    }

    // Converts FA withdrawal to an outbox message with a predefined Michelson type
    fn into_outbox_message(self, _withdrawal_id: U256) -> Withdrawal {
        let message = OutboxMessageTransaction {
            // Destination is always proxy contract (until sr -> tz ticket transfers are enabled)
            destination: self.proxy,
            // Constant entrypoint name parsing won't fail
            entrypoint: Entrypoint::try_from(WITHDRAW_ENTRYPOINT.to_string()).unwrap(),
            // L1 proxy accepts ticket and the final receiver address
            parameters: MichelsonPair(MichelsonContract(self.receiver), self.ticket),
        };
        crate::handler::Withdrawal::Standard(OutboxMessage::AtomicTransactionBatch(
            vec![message].into(),
        ))
    }

    /// Formats FA withdrawal structure for logging purposes.
    fn display(&self) -> String {
        format!(
            "FA withdrawal {} of {} from {} via {:?}",
            self.amount, self.ticket_hash, self.sender, self.ticket_owner
        )
    }

    fn amount(&self) -> U256 {
        self.amount
    }
    fn ticket_owner(&self) -> H160 {
        self.ticket_owner
    }
    fn ticket_hash(&self) -> H256 {
        self.ticket_hash
    }
    fn sender(&self) -> H160 {
        self.sender
    }
}

/// Withdrawal structure parsed from the precompile calldata
#[derive(Debug, PartialEq)]
pub struct FaFastWithdrawal {
    /// Account that invoked the precompile (`l2_caller` or `caller` would be more
    /// accurate but keeping `sender` to remain consistent with the FaWithdrawal struct)
    pub sender: H160,
    /// Contract (either implicit or originated) that will receive tokens or tickets
    pub receiver: Contract,
    /// Proxy contract on L1 that can handle ticket + receiver address (mandatory for now)
    pub proxy: Contract,
    /// Ticket transfer amount
    pub amount: U256,
    /// FA2.1 compatible ticket, constructed from the input
    pub ticket: FA2_1Ticket,
    /// Etherlink compatible ticket digest
    pub ticket_hash: H256,
    /// Actual ticket owner in global table (can be either sender or an ERC wrapper)
    pub ticket_owner: H160,
    // Creation date of the withdrawal
    pub timestamp: U256,
    // Address of the contract on L1 receiving the withdrawal
    pub fast_withdrawal_contract_address: Contract,
    // Additional metadata
    pub payload: Vec<u8>,
}

impl FaFastWithdrawal {
    /// Tries to parse withdrawal structure from the precompile call data.
    ///
    /// fa_fast_withdraw(
    ///     address ticketOwner,
    ///     bytes memory routingInfo,
    ///     uint256 amount,
    ///     bytes22 ticketer,
    ///     bytes memory content,
    ///     string memory fast_withdrawal_contract,
    ///     bytes memory payload
    /// )
    pub fn try_parse(
        input_data: &[u8],
        timestamp: U256,
        sender: H160,
    ) -> Result<Self, FaBridgeError> {
        let (
            ticket_owner,
            routing_info,
            amount,
            ticketer,
            content,
            fast_withdrawal_contract_address,
            payload,
        ) = SolFastWithdrawalInput::abi_decode_data(input_data, true).map_err(|_| {
            FaBridgeError::AbiDecodeError("Failed to parse precompile call data")
        })?;

        let ticket_owner = alloy_to_h160(&ticket_owner).unwrap_or_default();
        let amount = alloy_to_u256(&amount);
        let ticket_hash = ticket_hash_from_raw_parts(ticketer.as_slice(), &content);
        let (receiver, proxy) = parse_l1_routing_info(&routing_info)?;
        let ticketer: [u8; 22] = ticketer.as_slice().try_into().unwrap_or_default();
        let ticket = construct_ticket(ticketer, &content, amount)?;

        let fast_withdrawal_contract_address =
            Contract::from_b58check(&fast_withdrawal_contract_address)
                .ok()
                .ok_or(FaBridgeError::AbiDecodeError(
                    "Failed to parse fast withdrawal contract address",
                ))?;

        Ok(Self {
            sender,
            receiver,
            proxy,
            amount,
            ticket,
            ticket_hash,
            ticket_owner,
            fast_withdrawal_contract_address,
            timestamp,
            payload: payload.to_vec(),
        })
    }
}

impl FaWithdrawalMethods for FaFastWithdrawal {
    /// Returns calldata for the proxy (ERC wrapper) contract.
    ///
    /// Signature: withdraw(address,uint256,uint256)
    fn calldata(&self) -> Vec<u8> {
        let mut call_data = Vec::with_capacity(100);
        call_data.extend_from_slice(WITHDRAW_METHOD_ID);

        let calldata_ = SolStandardWithdrawalProxyCallData {
            sender: h160_to_alloy(&self.sender),
            amount: u256_to_alloy(&self.amount).unwrap_or_default(),
            ticket_hash: u256_to_alloy(&U256::from_big_endian(
                self.ticket_hash.as_bytes(),
            ))
            .unwrap_or_default(),
        };

        let data = SolStandardWithdrawalProxyCallData::encode_data(&calldata_);
        call_data.extend_from_slice(&data);

        call_data
    }

    /// Returns log structure for an implicit withdrawal event.
    /// This event is added to the outer transaction receipt,
    /// so that we can index successful withdrawal requests.
    ///
    /// It also contains unique withdrawal identifier.
    ///
    /// Signature:  FastFaWithdrawal(address,bytes22,bytes22,uint256,uint256,uint256,bytes,address)
    fn event_log(&self, withdrawal_id: U256) -> Log {
        let mut receiver_bytes = vec![];
        let mut proxy_bytes = vec![];

        // It is safe to unwrap, underlying implementation never fails (always returns Ok(()))
        self.receiver.bin_write(&mut receiver_bytes).unwrap();
        let receiver_bytes: [u8; 22] = receiver_bytes.try_into().unwrap();

        // It is safe to unwrap, underlying implementation never fails (always returns Ok(()))
        self.proxy.bin_write(&mut proxy_bytes).unwrap();
        let proxy_bytes: [u8; 22] = proxy_bytes.try_into().unwrap();

        let event_data = SolFastFAWithdrawalEvent {
            // The sender is set to context.caller in the precompile
            sender: h160_to_alloy(&self.sender),
            ticket_owner: h160_to_alloy(&self.ticket_owner),
            receiver: FixedBytes::<22>::from(&receiver_bytes),
            proxy: FixedBytes::<22>::from(&proxy_bytes),
            amount: u256_to_alloy(&self.amount).unwrap_or_default(),
            withdrawal_id: u256_to_alloy(&withdrawal_id).unwrap_or_default(),
            timestamp: u256_to_alloy(&self.timestamp).unwrap_or_default(),
            payload: self.payload.to_vec().into(),
        };

        let data = SolFastFAWithdrawalEvent::encode_data(&event_data);

        Log {
            address: FA_BRIDGE_PRECOMPILE_ADDRESS,
            topics: vec![H256(FAST_FA_WITHDRAWAL_EVENT_TOPIC), self.ticket_hash],
            data,
        }
    }

    // Converts FA withdrawal to an outbox message with a predefined Michelson type
    fn into_outbox_message(self, withdrawal_id: U256) -> Withdrawal {
        // Constant entrypoint name parsing won't fail
        let entrypoint = Entrypoint::try_from(String::from("default")).unwrap();

        let payload = MichelsonBytes(self.payload.to_vec());

        let withdrawal_id =
            MichelsonNat::new(Zarith(u256_to_bigint(withdrawal_id))).unwrap();

        let timestamp = Zarith(u256_to_bigint(self.timestamp));
        let timestamp: MichelsonTimestamp = MichelsonTimestamp(timestamp);

        let caller = MichelsonBytes(self.sender.to_fixed_bytes().to_vec());

        // L1 proxy accepts ticket and the final receiver address
        let parameters = MichelsonPair(
            withdrawal_id,
            MichelsonPair(
                self.ticket,
                MichelsonPair(
                    timestamp,
                    MichelsonPair(
                        MichelsonContract(self.receiver),
                        MichelsonPair(payload, caller),
                    ),
                ),
            ),
        );

        // Destination is always the fast withdrawal manager proxy contract
        let destination = self.fast_withdrawal_contract_address;

        let message = OutboxMessageTransaction {
            destination,
            entrypoint,
            parameters,
        };

        crate::handler::Withdrawal::Fast(OutboxMessage::AtomicTransactionBatch(
            vec![message].into(),
        ))
    }

    /// Formats FA withdrawal structure for logging purposes.
    fn display(&self) -> String {
        format!(
            "FA fast withdrawal {} of {} from {} via {:?}",
            self.amount, self.ticket_hash, self.sender, self.ticket_owner
        )
    }

    fn amount(&self) -> U256 {
        self.amount
    }
    fn ticket_owner(&self) -> H160 {
        self.ticket_owner
    }
    fn ticket_hash(&self) -> H256 {
        self.ticket_hash
    }
    fn sender(&self) -> H160 {
        self.sender
    }
}

/// Split routing info (raw bytes passed along with the ticket) into receiver and proxy addresses.
fn parse_l1_routing_info(
    routing_info: &[u8],
) -> Result<(Contract, Contract), FaBridgeError> {
    let (rest, receiver) = Contract::nom_read(routing_info)
        .map_err(|_| FaBridgeError::InvalidRoutingInfo("receiver"))?;

    let (rest, proxy) = Contract::nom_read(rest)
        .map_err(|_| FaBridgeError::InvalidRoutingInfo("proxy"))?;

    if let Contract::Implicit(_) = proxy {
        return Err(FaBridgeError::InvalidRoutingInfo("implicit proxy"));
    }

    if !rest.is_empty() {
        return Err(FaBridgeError::InvalidRoutingInfo("trailing bytes"));
    }

    Ok((receiver, proxy))
}

/// Construct FA2.1 ticket given its parts in raw format
fn construct_ticket(
    ticketer: [u8; 22],
    content: &[u8],
    amount: U256,
) -> Result<FA2_1Ticket, FaBridgeError> {
    let (_, creator) = Contract::nom_read(&ticketer)
        .map_err(|_| FaBridgeError::TicketConstructError("creator"))?;
    let (_, contents) =
        MichelsonPair::<MichelsonNat, MichelsonOption<MichelsonBytes>>::nom_read(content)
            .map_err(|_| FaBridgeError::TicketConstructError("contents"))?;
    let amount =
        BigInt::from_bytes_be(num_bigint::Sign::Plus, &Into::<[u8; 32]>::into(amount));

    FA2_1Ticket::new(creator, contents, amount)
        .map_err(|_| FaBridgeError::TicketConstructError("amount"))
}

/// Calculate unique ticket hash out of the ticket identifier (ticketer address and content).
///
/// Computed as Keccak256(ticketer || content) where
///  * ticketer: contract is in its forged form [ 0x01 | 20 bytes | 0x00 ]
///  * content: Micheline expression is in its forged form, legacy optimized mode
///
/// Solidity equivalent: uint256(keccak256(abi.encodePacked(ticketer, content)));
fn ticket_hash_from_raw_parts(ticketer: &[u8], content: &[u8]) -> H256 {
    let mut bytes = Vec::with_capacity(ticketer.len() + content.len());
    bytes.extend_from_slice(ticketer);
    bytes.extend_from_slice(content);
    keccak256_hash(&bytes)
}

#[cfg(test)]
mod tests {
    use alloy_sol_types::{SolCall, SolEvent};
    use primitive_types::{H160, U256};
    use sha3::{Digest, Keccak256};
    use tezos_data_encoding::enc::BinWriter;
    use tezos_smart_rollup_encoding::contract::Contract;

    use crate::{
        fa_bridge::{
            deposit::ticket_hash,
            test_utils::{
                convert_h160, convert_log, convert_u256, dummy_ticket, kernel_wrapper,
                ticket_id, token_wrapper,
            },
            withdrawal::{FAST_FA_WITHDRAWAL_EVENT_TOPIC, WITHDRAW_EVENT_TOPIC},
            FaWithdrawalMethods,
        },
        utilities::bigint_to_u256,
    };

    use super::{ticket_hash_from_raw_parts, FaWithdrawal};

    fn dummy_fa_withdrawal() -> FaWithdrawal {
        let ticket = dummy_ticket();
        let sender = H160([1u8; 20]);
        let ticket_owner = H160([2u8; 20]);
        let ticket_hash = ticket_hash(&ticket).expect("Failed to calc ticket hash");
        let amount = bigint_to_u256(ticket.amount()).unwrap();
        FaWithdrawal {
            sender,
            receiver: Contract::from_b58check("tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU")
                .unwrap(),
            proxy: Contract::from_b58check("KT18amZmM5W7qDWVt2pH6uj7sCEd3kbzLrHT")
                .unwrap(),
            amount,
            ticket_hash,
            ticket,
            ticket_owner,
        }
    }

    #[test]
    fn withdrawal_event_signature() {
        assert_eq!(
            WITHDRAW_EVENT_TOPIC.to_vec(),
            Keccak256::digest(
                b"Withdrawal(uint256,address,address,bytes22,bytes22,uint256,uint256)"
            )
            .to_vec()
        );
    }

    #[test]
    fn fa_withdrawal_parsing_success() {
        let ticket_owner = H160([1u8; 20]);
        let receiver = [
            [0u8; 22].to_vec(),
            vec![0x01],
            [0u8; 20].to_vec(),
            vec![0x00],
        ]
        .concat();

        let ticket = dummy_ticket();
        let (ticketer, content) = ticket_id(&ticket);
        let ticket_hash = ticket_hash(&ticket).unwrap();

        let amount = bigint_to_u256(ticket.amount()).unwrap();

        let input = kernel_wrapper::withdrawCall::new((
            convert_h160(&ticket_owner),
            receiver.into(),
            convert_u256(&amount),
            ticketer.into(),
            content.into(),
        ))
        .abi_encode();
        let sender = H160::zero();

        let withdrawal = FaWithdrawal::try_parse(&input[4..], sender).unwrap();

        pretty_assertions::assert_eq!(
            withdrawal,
            FaWithdrawal {
                amount,
                sender,
                receiver: Contract::from_b58check("tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU")
                    .unwrap(),
                proxy: Contract::from_b58check("KT18amZmM5W7qDWVt2pH6uj7sCEd3kbzLrHT")
                    .unwrap(),
                ticket,
                ticket_hash,
                ticket_owner,
            }
        );
    }

    #[test]
    fn fa_withdrawal_verify_calldata_encoding() {
        let withdrawal = dummy_fa_withdrawal();

        let actual = withdrawal.calldata();

        let expected = token_wrapper::withdrawCall::new((
            convert_h160(&withdrawal.sender),
            convert_u256(&withdrawal.amount),
            alloy_primitives::U256::from_be_slice(&withdrawal.ticket_hash.0),
        ))
        .abi_encode();

        pretty_assertions::assert_eq!(expected, actual);
    }

    #[test]
    fn fa_withdrawal_verify_eventlog_encoding() {
        let withdrawal = dummy_fa_withdrawal();

        let log = withdrawal.event_log(U256::one());

        let withdrawal_event =
            kernel_wrapper::Withdrawal::decode_log_data(&convert_log(&log), true)
                .expect("Failed to parse Withdrawal event");

        let ticket_hash_topic =
            alloy_primitives::U256::from_be_slice(&withdrawal.ticket_hash.0);

        assert_eq!(withdrawal_event.topics().0 .0, *WITHDRAW_EVENT_TOPIC);
        assert_eq!(withdrawal_event.topics().1, ticket_hash_topic);

        assert_eq!(withdrawal_event.ticketHash, ticket_hash_topic);
        assert_eq!(withdrawal_event.sender, convert_h160(&withdrawal.sender));
        assert_eq!(
            withdrawal_event.ticketOwner,
            convert_h160(&withdrawal.ticket_owner)
        );
        assert_eq!(
            withdrawal_event.receiver,
            alloy_primitives::FixedBytes::<22>::repeat_byte(0)
        );
        assert_eq!(
            withdrawal_event.proxy,
            alloy_primitives::FixedBytes::<22>::from_slice(&[
                1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
            ])
        );
        assert_eq!(withdrawal_event.amount, convert_u256(&withdrawal.amount));
        assert_eq!(
            withdrawal_event.withdrawalId,
            alloy_primitives::U256::from(1)
        );
    }

    #[test]
    fn fa_withdrawal_verify_message_to_originated_contract_encoding() {
        let withdrawal = dummy_fa_withdrawal();

        let outbox_message = withdrawal.into_outbox_message(U256::default());

        let mut encoded_message = Vec::new();
        outbox_message.bin_write(&mut encoded_message).unwrap();

        // 0x00 byte prefix
        // forged array
        // [
        //      forged parameters — pair(receiver, ticket)
        //      forged destination — originated address
        //      forged entrypoint (array) — "withdraw"
        // ]
        // octez-codec decode 017-PtNairob.smart_rollup.outbox.message from <HEX>
        let expected = hex::decode(
            "\
            000000006607070a000000160000000000000000000000000000000000000000\
            000007070a000000160101010101010101010101010101010101010101010007\
            0707070000030600010100000000000000000000000000000000000000000000\
            0000087769746864726177",
        )
        .unwrap();

        pretty_assertions::assert_eq!(expected, encoded_message);
    }

    #[test]
    fn check_ticket_hash_equality() {
        let ticket = dummy_ticket();

        let mut ticketer = Vec::new();
        ticket.creator().0.bin_write(&mut ticketer).unwrap();
        assert_eq!(ticketer.len(), 22);

        let mut content = Vec::new();
        ticket.contents().bin_write(&mut content).unwrap();
        assert_eq!(content.len(), 6);

        let actual = ticket_hash_from_raw_parts(&ticketer, &content);
        let expected = ticket_hash(&ticket).unwrap();
        assert_eq!(expected, actual);
    }

    #[test]
    fn fast_fa_withdrawal_event_signature() {
        assert_eq!(
            FAST_FA_WITHDRAWAL_EVENT_TOPIC.to_vec(),
            Keccak256::digest(
                b"FastFaWithdrawal(address,address,bytes22,bytes22,uint256,uint256,uint256,bytes)"
            )
            .to_vec()
        );
    }
}
