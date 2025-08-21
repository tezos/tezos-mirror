// SPDX-FileCopyrightText: 2023 PK Lab <contact@pklab.io>
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

use num_bigint::BigInt;
use primitive_types::{H160, H256, U256};
use tezos_data_encoding::{enc::BinWriter, nom::NomReader};
use tezos_ethereum::Log;
use tezos_smart_rollup_encoding::{
    contract::Contract,
    entrypoint::Entrypoint,
    michelson::{
        ticket::FA2_1Ticket, MichelsonBytes, MichelsonContract, MichelsonNat,
        MichelsonOption, MichelsonPair,
    },
    outbox::{OutboxMessage, OutboxMessageTransaction},
};

use crate::{
    abi::{self, ABI_B22_RIGHT_PADDING, ABI_H160_LEFT_PADDING},
    handler::Withdrawal,
    precompiles::FA_BRIDGE_PRECOMPILE_ADDRESS,
    utilities::keccak256_hash,
};

use super::error::FaBridgeError;

/// Keccak256 of withdraw(address,uint256,uint256), first 4 bytes
pub const WITHDRAW_METHOD_ID: &[u8; 4] = b"\xb5\xc5\xf6\x72";

/// Keccak256 of Withdrawal(uint256,address,address,bytes22,bytes22,uint256,uint256)
pub const WITHDRAW_EVENT_TOPIC: &[u8; 32] = b"\
    \xab\x68\x45\x0c\x9e\x54\x6f\x60\x62\xa8\x61\xee\xbf\x8e\xc5\xbb\
    \xd4\x1b\x44\x25\xe2\x6b\x20\x19\x9c\x91\x22\x7c\x7f\x90\x38\xca";

/// L1 proxy contract entrypoint that will be invoked by the outbox message
/// execution.
pub const WITHDRAW_ENTRYPOINT: &str = "withdraw";

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
        let ticket_owner = abi::h160_parameter(input_data, 0)
            .ok_or(FaBridgeError::AbiDecodeError("ticket_owner"))?;
        let routing_info = abi::bytes_parameter(input_data, 1)
            .ok_or(FaBridgeError::AbiDecodeError("routing_info"))?;
        let amount = abi::u256_parameter(input_data, 2)
            .ok_or(FaBridgeError::AbiDecodeError("amount"))?;
        let ticketer: [u8; 22] = abi::fixed_bytes_parameter(input_data, 3)
            .ok_or(FaBridgeError::AbiDecodeError("ticketer"))?;
        let content = abi::bytes_parameter(input_data, 4)
            .ok_or(FaBridgeError::AbiDecodeError("content"))?;

        let ticket_hash = ticket_hash_from_raw_parts(&ticketer, content);
        let (receiver, proxy) = parse_l1_routing_info(routing_info)?;
        let ticket = construct_ticket(ticketer, content, amount)?;

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

    /// Returns calldata for the proxy (ERC wrapper) contract.
    ///
    /// Signature: withdraw(address,uint256,uint256)
    pub fn calldata(&self) -> Vec<u8> {
        let mut call_data = Vec::with_capacity(100);
        call_data.extend_from_slice(WITHDRAW_METHOD_ID);

        call_data.extend_from_slice(&ABI_H160_LEFT_PADDING);
        call_data.extend_from_slice(self.sender.as_bytes());
        debug_assert!((call_data.len() - 4) % 32 == 0);

        call_data.extend_from_slice(&Into::<[u8; 32]>::into(self.amount));
        debug_assert!((call_data.len() - 4) % 32 == 0);

        call_data.extend_from_slice(self.ticket_hash.as_bytes());
        debug_assert!((call_data.len() - 4) % 32 == 0);

        call_data
    }

    /// Returns log structure for an implicit withdrawal event.
    /// This event is added to the outer transaction receipt,
    /// so that we can index successful withdrawal requests.
    ///
    /// It also contains unique withdrawal identifier.
    ///
    /// Signature: Withdrawal(uint256,address,address,bytes22,bytes22,uint256,uint256)
    pub fn event_log(&self, withdrawal_id: U256) -> Log {
        let mut data = Vec::with_capacity(7 * 32);

        data.extend_from_slice(&ABI_H160_LEFT_PADDING);
        data.extend_from_slice(self.sender.as_bytes());
        debug_assert!(data.len() % 32 == 0);

        data.extend_from_slice(&ABI_H160_LEFT_PADDING);
        data.extend_from_slice(self.ticket_owner.as_bytes());
        debug_assert!(data.len() % 32 == 0);

        // It is safe to unwrap, underlying implementation never fails (always returns Ok(()))
        self.receiver.bin_write(&mut data).unwrap();
        data.extend_from_slice(&ABI_B22_RIGHT_PADDING);
        debug_assert!(data.len() % 32 == 0);

        self.proxy.bin_write(&mut data).unwrap();
        data.extend_from_slice(&ABI_B22_RIGHT_PADDING);
        debug_assert!(data.len() % 32 == 0);

        data.extend_from_slice(&Into::<[u8; 32]>::into(self.amount));
        debug_assert!(data.len() % 32 == 0);

        data.extend_from_slice(&Into::<[u8; 32]>::into(withdrawal_id));
        debug_assert!(data.len() % 32 == 0);

        Log {
            address: FA_BRIDGE_PRECOMPILE_ADDRESS,
            topics: vec![H256(*WITHDRAW_EVENT_TOPIC), self.ticket_hash],
            data,
        }
    }

    // Converts FA withdrawal to an outbox message with a predefined Michelson type
    pub fn into_outbox_message(self) -> Withdrawal {
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
    pub fn display(&self) -> String {
        format!(
            "FA withdrawal {} of {} from {} via {:?}",
            self.amount, self.ticket_hash, self.sender, self.ticket_owner
        )
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
    use tezos_data_encoding::enc::BinWriter;
    use tezos_smart_rollup_encoding::contract::Contract;

    use crate::{
        fa_bridge::{
            deposit::ticket_hash,
            test_utils::{
                convert_h160, convert_log, convert_u256, dummy_ticket, kernel_wrapper,
                ticket_id, token_wrapper,
            },
            withdrawal::WITHDRAW_EVENT_TOPIC,
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
                ticket_owner
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

        let outbox_message = withdrawal.into_outbox_message();

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
}
