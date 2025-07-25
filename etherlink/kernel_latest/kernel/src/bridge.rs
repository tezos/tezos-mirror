// SPDX-FileCopyrightText: 2022-2023 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Native token (TEZ) bridge primitives and helpers.

use std::borrow::Cow;

use alloy_sol_types::SolEvent;
use ethereum::Log;
use evm::{Config, ExitError};
use evm_execution::{
    account_storage::{account_path, AccountStorageError, EthereumAccountStorage},
    handler::{ExecutionOutcome, ExecutionResult},
    utilities::alloy::{h160_to_alloy, u256_to_alloy},
    EthereumError,
};
use primitive_types::{H160, H256, U256};
use rlp::{Decodable, DecoderError, Encodable, Rlp, RlpEncodable};
use sha3::{Digest, Keccak256};
use tezos_ethereum::{
    rlp_helpers::{decode_field, next},
    wei::eth_from_mutez,
};
use tezos_evm_logging::{log, Level::Info};
use tezos_evm_runtime::runtime::Runtime;
use tezos_smart_rollup::michelson::{ticket::FA2_1Ticket, MichelsonBytes};

use crate::tick_model;

/// Keccak256 of Deposit(uint256,address,uint256,uint256)
/// This is main topic (non-anonymous event): https://docs.soliditylang.org/en/latest/abi-spec.html#events
pub const DEPOSIT_EVENT_TOPIC: [u8; 32] = [
    211, 106, 47, 103, 208, 109, 40, 87, 134, 246, 26, 50, 176, 82, 185, 172, 230, 176,
    183, 171, 239, 81, 119, 181, 67, 88, 171, 220, 131, 160, 182, 155,
];

/// Native token bridge error
#[derive(Debug, thiserror::Error)]
pub enum BridgeError {
    #[error("Invalid deposit receiver address: {0:?}")]
    InvalidDepositReceiver(Vec<u8>),
}

alloy_sol_types::sol! {
    event SolBridgeDepositEvent (
        uint256 amount,
        address receiver,
        uint256 inbox_level,
        uint256 inbox_msg_id,
    );
}

/// Native token deposit
#[derive(Debug, PartialEq, Clone, RlpEncodable)]
pub struct Deposit {
    /// Deposit amount in wei
    pub amount: U256,
    /// Deposit receiver on L2
    pub receiver: H160,
    /// Inbox level containing the original deposit message
    pub inbox_level: u32,
    /// Inbox message id
    pub inbox_msg_id: u32,
}

impl Deposit {
    const RECEIVER_LENGTH: usize = std::mem::size_of::<H160>();

    const RECEIVER_AND_CHAIN_ID_LENGTH: usize =
        Self::RECEIVER_LENGTH + std::mem::size_of::<U256>();

    fn parse_receiver(
        input: MichelsonBytes,
    ) -> Result<(H160, Option<U256>), BridgeError> {
        let input_bytes = input.0;
        let input_length = input_bytes.len();
        if input_length == Self::RECEIVER_LENGTH {
            // Legacy format, input is exactly the receiver EVM address
            let receiver = H160::from_slice(&input_bytes);
            Ok((receiver, None))
        } else if input_length == Self::RECEIVER_AND_CHAIN_ID_LENGTH {
            // input is receiver followed by chain id
            let receiver = H160::from_slice(&input_bytes[..Self::RECEIVER_LENGTH]);
            let chain_id =
                U256::from_little_endian(&input_bytes[Self::RECEIVER_LENGTH..]);
            Ok((receiver, Some(chain_id)))
        } else {
            Err(BridgeError::InvalidDepositReceiver(input_bytes))
        }
    }

    /// Parses a deposit from a ticket transfer (internal inbox message).
    /// The "entrypoint" type is pair of ticket (FA2.1) and bytes (receiver address).
    #[cfg_attr(feature = "benchmark", inline(never))]
    pub fn try_parse(
        ticket: FA2_1Ticket,
        receiver: MichelsonBytes,
        inbox_level: u32,
        inbox_msg_id: u32,
    ) -> Result<(Self, Option<U256>), BridgeError> {
        // Amount
        let (_sign, amount_bytes) = ticket.amount().to_bytes_le();
        // We use the `U256::from_little_endian` as it takes arbitrary long
        // bytes. Afterward it's transform to `u64` to use `eth_from_mutez`, it's
        // obviously safe as we deposit CTEZ and the amount is limited by
        // the XTZ quantity.
        let amount_mutez: u64 = U256::from_little_endian(&amount_bytes).as_u64();
        let amount: U256 = eth_from_mutez(amount_mutez);

        // EVM address of the receiver and chain id both come from the
        // Michelson byte parameter.
        let (receiver, chain_id) = Self::parse_receiver(receiver)?;

        Ok((
            Self {
                amount,
                receiver,
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
    ///
    /// Signature: Deposit(uint256,address,uint256,uint256)
    pub fn event_log(&self) -> Log {
        let event_data = SolBridgeDepositEvent {
            receiver: h160_to_alloy(&self.receiver),
            amount: u256_to_alloy(&self.amount).unwrap_or_default(),
            inbox_level: u256_to_alloy(&U256::from(self.inbox_level)).unwrap_or_default(),
            inbox_msg_id: u256_to_alloy(&U256::from(self.inbox_msg_id))
                .unwrap_or_default(),
        };

        let data = SolBridgeDepositEvent::encode_data(&event_data);

        Log {
            // Emitted by the "system" contract
            address: H160::zero(),
            // Event ID (non-anonymous) and indexed fields
            topics: vec![H256(DEPOSIT_EVENT_TOPIC)],
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

    pub fn display(&self) -> String {
        format!("Deposit {} to {}", self.amount, self.receiver)
    }
}

impl Decodable for Deposit {
    /// Decode deposit from RLP bytes in a retro-compatible manner.
    /// If it is a legacy deposit it will have zero inbox level and message ID.
    ///
    /// NOTE that [Deposit::hash] would give the same results for "legacy" deposits,
    /// but since decoding is used only for items that are already in delayed inbox this is OK:
    /// the hash is calculated for items that are to be submitted to delayed inbox.
    fn decode(decoder: &Rlp) -> Result<Self, DecoderError> {
        if !decoder.is_list() {
            return Err(DecoderError::RlpExpectedToBeList);
        }
        match decoder.item_count()? {
            2 => {
                let mut it = decoder.iter();
                let amount: U256 = decode_field(&next(&mut it)?, "amount")?;
                let receiver: H160 = decode_field(&next(&mut it)?, "receiver")?;
                Ok(Self {
                    amount,
                    receiver,
                    inbox_level: 0,
                    inbox_msg_id: 0,
                })
            }
            4 => {
                let mut it = decoder.iter();
                let amount: U256 = decode_field(&next(&mut it)?, "amount")?;
                let receiver: H160 = decode_field(&next(&mut it)?, "receiver")?;
                let inbox_level: u32 = decode_field(&next(&mut it)?, "inbox_level")?;
                let inbox_msg_id: u32 = decode_field(&next(&mut it)?, "inbox_msg_id")?;
                Ok(Self {
                    amount,
                    receiver,
                    inbox_level,
                    inbox_msg_id,
                })
            }
            _ => Err(DecoderError::RlpIncorrectListLen),
        }
    }
}

pub fn execute_deposit<Host: Runtime>(
    host: &mut Host,
    evm_account_storage: &mut EthereumAccountStorage,
    deposit: &Deposit,
    config: &Config,
) -> Result<ExecutionOutcome, EthereumError> {
    // We should be able to obtain an account for arbitrary H160 address
    // otherwise it is a fatal error.
    let to_account_path =
        account_path(&deposit.receiver).map_err(AccountStorageError::from)?;
    let mut to_account = evm_account_storage.get_or_create(host, &to_account_path)?;

    let result = match to_account.balance_add(host, deposit.amount) {
        Ok(()) => ExecutionResult::TransferSucceeded,
        Err(err) => {
            log!(host, Info, "Deposit failed with {:?}", err);
            ExecutionResult::Error(ExitError::Other(Cow::from("Deposit failed")))
        }
    };

    let logs = if result.is_success() {
        vec![deposit.event_log()]
    } else {
        vec![]
    };

    // TODO: estimate how emitting an event influenced tick consumption
    let gas_used = config.gas_transaction_call;
    let estimated_ticks_used = tick_model::constants::TICKS_FOR_DEPOSIT;

    let execution_outcome = ExecutionOutcome {
        gas_used,
        logs,
        result,
        withdrawals: vec![],
        estimated_ticks_used,
    };
    Ok(execution_outcome)
}

#[cfg(test)]
mod tests {
    use alloy_sol_types::SolEvent;
    use evm_execution::fa_bridge::test_utils::create_fa_ticket;
    use evm_execution::{
        account_storage::init_account_storage, configuration::EVMVersion,
    };
    use primitive_types::{H160, U256};
    use rlp::Decodable;
    use tezos_evm_runtime::runtime::MockKernelHost;
    use tezos_smart_rollup_encoding::michelson::MichelsonBytes;

    use crate::bridge::DEPOSIT_EVENT_TOPIC;

    use super::{execute_deposit, Deposit};

    mod events {
        alloy_sol_types::sol! {
            event Deposit (
                uint256 amount,
                address receiver,
                uint256 inboxLevel,
                uint256 inboxMsgId
            );
        }
    }

    fn dummy_deposit() -> Deposit {
        Deposit {
            amount: 1.into(),
            receiver: H160([2u8; 20]),
            inbox_level: 3,
            inbox_msg_id: 4,
        }
    }

    #[test]
    fn deposit_event_topic() {
        assert_eq!(events::Deposit::SIGNATURE_HASH.0, DEPOSIT_EVENT_TOPIC);
    }

    #[test]
    fn deposit_parsing_no_chain_id() {
        let ticket =
            create_fa_ticket("KT18amZmM5W7qDWVt2pH6uj7sCEd3kbzLrHT", 0, &[0u8], 2.into());
        let receiver = MichelsonBytes(vec![1u8; 20]);
        let (deposit, chain_id) =
            Deposit::try_parse(ticket, receiver, 0, 0).expect("Failed to parse");
        pretty_assertions::assert_eq!(
            deposit,
            Deposit {
                amount: tezos_ethereum::wei::eth_from_mutez(2),
                receiver: H160([1u8; 20]),
                inbox_level: 0,
                inbox_msg_id: 0,
            }
        );
        pretty_assertions::assert_eq!(chain_id, None)
    }

    #[test]
    fn deposit_parsing_chain_id() {
        let ticket =
            create_fa_ticket("KT18amZmM5W7qDWVt2pH6uj7sCEd3kbzLrHT", 0, &[0u8], 2.into());
        let mut receiver_and_chain_id = vec![];
        receiver_and_chain_id.extend(vec![1u8; 20]);
        receiver_and_chain_id.extend(vec![1u8]);
        receiver_and_chain_id.extend(vec![0u8; 31]);
        let receiver_and_chain_id = MichelsonBytes(receiver_and_chain_id);
        let (deposit, chain_id) = Deposit::try_parse(ticket, receiver_and_chain_id, 0, 0)
            .expect("Failed to parse");
        pretty_assertions::assert_eq!(
            deposit,
            Deposit {
                amount: tezos_ethereum::wei::eth_from_mutez(2),
                receiver: H160([1u8; 20]),
                inbox_level: 0,
                inbox_msg_id: 0,
            }
        );
        pretty_assertions::assert_eq!(chain_id, Some(U256::one()))
    }

    #[test]
    fn deposit_decode_legacy() {
        let mut stream = rlp::RlpStream::new_list(2);
        stream.append(&U256::one()).append(&H160([1u8; 20]));
        let bytes = stream.out().to_vec();
        let decoder = rlp::Rlp::new(&bytes);
        let res = Deposit::decode(&decoder).unwrap();
        assert_eq!(
            res,
            Deposit {
                amount: U256::one(),
                receiver: H160([1u8; 20]),
                inbox_level: 0,
                inbox_msg_id: 0,
            }
        );
    }

    #[test]
    fn deposit_execution_outcome_contains_event() {
        let mut host = MockKernelHost::default();
        let mut evm_account_storage = init_account_storage().unwrap();

        let deposit = dummy_deposit();

        let outcome = execute_deposit(
            &mut host,
            &mut evm_account_storage,
            &deposit,
            &EVMVersion::current_test_config(),
        )
        .unwrap();
        assert!(outcome.is_success());
        assert_eq!(outcome.logs.len(), 1);

        let log_data = alloy_primitives::LogData::new_unchecked(
            outcome.logs[0].topics.iter().map(|x| x.0.into()).collect(),
            outcome.logs[0].data.clone().into(),
        );
        let event = events::Deposit::decode_log_data(&log_data).unwrap();
        assert_eq!(event.amount, alloy_primitives::U256::from(1));
        assert_eq!(
            event.receiver,
            alloy_primitives::Address::from_slice(&[2u8; 20])
        );
        assert_eq!(event.inboxLevel, alloy_primitives::U256::from(3));
        assert_eq!(event.inboxMsgId, alloy_primitives::U256::from(4));
    }

    #[test]
    fn deposit_execution_fails_due_to_balance_overflow() {
        let mut host = MockKernelHost::default();
        let mut evm_account_storage = init_account_storage().unwrap();

        let mut deposit = dummy_deposit();
        deposit.amount = U256::MAX;

        let outcome = execute_deposit(
            &mut host,
            &mut evm_account_storage,
            &deposit,
            &EVMVersion::current_test_config(),
        )
        .unwrap();
        assert!(outcome.is_success());

        let outcome = execute_deposit(
            &mut host,
            &mut evm_account_storage,
            &deposit,
            &EVMVersion::current_test_config(),
        )
        .unwrap();
        assert!(!outcome.is_success());
        assert!(outcome.logs.is_empty());
    }

    #[test]
    fn bridge_deposit_event_log_consistent() {
        let deposit = dummy_deposit();
        let log = deposit.event_log();
        let expected_log = hex::decode("0000000000000000000000000000000000000000000000000000000000000001\
                                        0000000000000000000000000202020202020202020202020202020202020202\
                                        0000000000000000000000000000000000000000000000000000000000000003\
                                        0000000000000000000000000000000000000000000000000000000000000004").unwrap();
        assert_eq!(expected_log, log.data)
    }
}
