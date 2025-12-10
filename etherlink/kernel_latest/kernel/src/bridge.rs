// SPDX-FileCopyrightText: 2022-2023 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2025-2026 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

//! Native token (TEZ) bridge primitives and helpers.

use std::fmt::Display;

use alloy_sol_types::SolEvent;
use primitive_types::{H160, H256, U256};
use revm::context::result::{Output, SuccessReason};
use revm::primitives::hardfork::SpecId;
use revm::primitives::{Address, Bytes, Log, LogData, B256};
use revm_etherlink::helpers::legacy::{alloy_to_h160, h160_to_alloy, u256_to_alloy};
use revm_etherlink::inspectors::TracerInput;
use revm_etherlink::precompiles::constants::FEED_DEPOSIT_ADDR;
use revm_etherlink::ExecutionOutcome;
use rlp::{Decodable, DecoderError, Encodable, Rlp, RlpEncodable};
use sha3::{Digest, Keccak256};
use tezos_data_encoding::enc::BinWriter;
use tezos_data_encoding::nom::NomReader;
use tezos_ethereum::block::BlockConstants;
use tezos_ethereum::rlp_helpers::{decode_field_u256_le, decode_option_explicit};
use tezos_ethereum::transaction::TRANSACTION_HASH_SIZE;
use tezos_ethereum::wei::mutez_from_wei;
use tezos_ethereum::{
    rlp_helpers::{decode_field, next},
    wei::eth_from_mutez,
};
use tezos_evm_logging::{log, Level::Error, Level::Info};
use tezos_evm_runtime::runtime::Runtime;
use tezos_execution::account_storage::{TezlinkAccount, TezosImplicitAccount};
use tezos_execution::context::Context;
use tezos_protocol::contract::Contract;
use tezos_smart_rollup::michelson::{ticket::FA2_1Ticket, MichelsonBytes};
use tezos_tezlink::operation::{Parameters, TransferContent};
use tezos_tezlink::operation_result::{
    ApplyOperationErrors, BalanceUpdate, ContentResult, TransferError, TransferSuccess,
    TransferTarget,
};
use tezos_tracing::trace_kernel;
use tezosx_interfaces::{Registry, RuntimeId};
use tezosx_tezos_runtime::TezosRuntime;

use crate::apply::{pure_xtz_deposit, ExecutionResult, TransactionResult};
use crate::chains::EvmLimits;
use crate::tick_model::constants::TICKS_FOR_DEPOSIT;

/// Keccak256 of Deposit(uint256,address,uint256,uint256)
/// This is main topic (non-anonymous event): https://docs.soliditylang.org/en/latest/abi-spec.html#events
pub const DEPOSIT_EVENT_TOPIC: [u8; 32] = [
    211, 106, 47, 103, 208, 109, 40, 87, 134, 246, 26, 50, 176, 82, 185, 172, 230, 176,
    183, 171, 239, 81, 119, 181, 67, 88, 171, 220, 131, 160, 182, 155,
];

// NB: The following value was obtain via:
// `revm::interpreter::gas::call_cost(spec_id, true, StateLoad::default())`
// which was available in { revm <= v29.0.0 }.
// The value was an approximation and still remains one.
// TODO: estimate how emitting an event influenced tick consumption
const DEPOSIT_EXECUTION_GAS_COST: u64 = 9100;

/// Native token bridge error
#[derive(Debug, thiserror::Error)]
pub enum BridgeError {
    #[error("Invalid deposit receiver address: {0:?}")]
    InvalidDepositReceiver(Vec<u8>),
    #[error("Invalid RLP bytes: {0}")]
    RlpError(DecoderError),
    #[error("Invalid amount for a deposit: {0:?}")]
    InvalidAmount(U256),
}

alloy_sol_types::sol! {
    event SolBridgeDepositEvent (
        uint256 amount,
        address receiver,
        uint256 inbox_level,
        uint256 inbox_msg_id,
    );
}

const TEZLINK_ADDRESS: u8 = 1;

#[derive(Debug, PartialEq, Clone)]
pub enum DepositReceiver {
    Ethereum(H160),
    Tezos(Contract),
}

impl DepositReceiver {
    pub fn to_h160(&self) -> Result<H160, BridgeError> {
        match self {
            Self::Ethereum(receiver) => Ok(*receiver),
            Self::Tezos(contract) => {
                // TODO https://linear.app/tezos/issue/L2-641
                // This function converts a Tezos like address to a H160
                // to represent its wallet in the Etherlink runtime. This should
                // be done outside of 'bridge.rs' and the conversion function
                // may be different (for example determine the H160 from the keccak)
                // of the Tezos contract bytes.
                let bytes = contract
                    .to_bytes()
                    .map_err(|_| BridgeError::InvalidDepositReceiver(vec![]))?;
                // Tezos addresses are 22-byte long, to canonically represent
                // a Tezos address as a H160, we drop the first and last bytes.
                if bytes.len() == 22 {
                    Ok(H160::from_slice(&bytes[1..21]))
                } else {
                    Err(BridgeError::InvalidDepositReceiver(bytes))
                }
            }
        }
    }

    pub fn to_contract(&self) -> Result<Contract, BridgeError> {
        match self {
            DepositReceiver::Ethereum(address) => {
                // A Tezos contract's length is 22 bytes
                // The first two bytes here represent:
                //     - 0u8: means that it's an implicit account
                //     - 0u8: means that it's a tz1
                // This code is temporary and should be replaced for the same
                // reason as in https://linear.app/tezos/issue/L2-641
                let mut contract = vec![0u8, 0u8];
                contract.extend_from_slice(address.as_bytes());
                match Contract::nom_read_exact(&contract) {
                    Ok(contract) => Ok(contract),
                    Err(_) => Err(BridgeError::InvalidDepositReceiver(contract)),
                }
            }
            DepositReceiver::Tezos(contract) => Ok(contract.clone()),
        }
    }
}

impl rlp::Encodable for DepositReceiver {
    fn rlp_append(&self, s: &mut rlp::RlpStream) {
        match self {
            DepositReceiver::Ethereum(addr) => addr.rlp_append(s),
            DepositReceiver::Tezos(contract) => {
                s.begin_list(2);
                s.append(&TEZLINK_ADDRESS);
                let mut out = vec![];
                // BinWriter returns a Result<()> but can't properly
                // propagate it outside of rlp_append
                let _ = contract.bin_write(&mut out);
                s.append(&out);
            }
        }
    }
}

impl rlp::Decodable for DepositReceiver {
    fn decode(decoder: &Rlp) -> Result<Self, DecoderError> {
        if decoder.is_list() {
            if decoder.item_count()? != 2 {
                return Err(DecoderError::RlpIncorrectListLen);
            }
            let tag: u8 = decoder.at(0)?.as_val()?;
            let receiver = decoder.at(1)?;
            match tag {
                TEZLINK_ADDRESS => {
                    let receiver = receiver.data()?;
                    let contract = Contract::nom_read_exact(receiver).map_err(|_| {
                        DecoderError::Custom(
                            "Can't decode Tezos receiver using NomReader",
                        )
                    })?;
                    Ok(Self::Tezos(contract))
                }
                _ => Err(DecoderError::Custom("Unknown receiver tag.")),
            }
        } else {
            let receiver: H160 = decode_field(decoder, "receiver")?;
            Ok(Self::Ethereum(receiver))
        }
    }
}

impl Display for DepositReceiver {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Ethereum(address) => write!(f, "{address}"),
            Self::Tezos(contract) => write!(f, "{}", contract.to_b58check()),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct DepositInfo {
    pub receiver: DepositReceiver,
    pub chain_id: Option<U256>,
}

impl DepositInfo {
    fn decode(bytes: &[u8]) -> Result<Self, DecoderError> {
        let version = bytes[0];
        let decoder = Rlp::new(&bytes[1..]);
        if version == 1u8 {
            if !decoder.is_list() {
                return Err(DecoderError::RlpExpectedToBeList);
            }
            if decoder.item_count()? != 2 {
                return Err(DecoderError::RlpIncorrectListLen);
            }
            let mut it: rlp::RlpIterator<'_, '_> = decoder.iter();
            let receiver: DepositReceiver = decode_field(&next(&mut it)?, "receiver")?;
            let chain_id: Option<U256> = decode_option_explicit(
                &next(&mut it)?,
                "chain_id",
                decode_field_u256_le,
            )?;
            Ok(Self { receiver, chain_id })
        } else {
            Err(DecoderError::Custom(
                "Unexpected version for Deposit informations",
            ))
        }
    }
}
/// Native token deposit
#[derive(Debug, PartialEq, Clone, RlpEncodable)]
pub struct Deposit {
    /// Deposit amount in wei
    pub amount: U256,
    /// Deposit receiver on L2
    pub receiver: DepositReceiver,
    /// Inbox level containing the original deposit message
    pub inbox_level: u32,
    /// Inbox message id
    pub inbox_msg_id: u32,
}

impl Deposit {
    const RECEIVER_LENGTH: usize = std::mem::size_of::<Address>();

    const RECEIVER_AND_CHAIN_ID_LENGTH: usize =
        Self::RECEIVER_LENGTH + std::mem::size_of::<U256>();

    fn parse_deposit_info(input: MichelsonBytes) -> Result<DepositInfo, BridgeError> {
        let input_bytes = input.0;
        let input_length = input_bytes.len();
        if input_length == Self::RECEIVER_LENGTH {
            // Legacy format, input is exactly the receiver EVM address
            let receiver = H160::from_slice(&input_bytes);
            Ok(DepositInfo {
                receiver: DepositReceiver::Ethereum(receiver),
                chain_id: None,
            })
        } else if input_length == Self::RECEIVER_AND_CHAIN_ID_LENGTH {
            // input is receiver followed by chain id
            let receiver = H160::from_slice(&input_bytes[..Self::RECEIVER_LENGTH]);
            let chain_id =
                U256::from_little_endian(&input_bytes[Self::RECEIVER_LENGTH..]);
            Ok(DepositInfo {
                receiver: DepositReceiver::Ethereum(receiver),
                chain_id: Some(chain_id),
            })
        } else {
            // From now on, bytes are versionned rlp
            DepositInfo::decode(&input_bytes).map_err(BridgeError::RlpError)
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
        let info = Self::parse_deposit_info(receiver)?;

        Ok((
            Self {
                amount,
                receiver: info.receiver,
                inbox_level,
                inbox_msg_id,
            },
            info.chain_id,
        ))
    }

    /// Returns log structure for an implicit deposit event.
    ///
    /// This event is added to the outer transaction receipt,
    /// so that we can index successful deposits and update status.
    ///
    /// Signature: Deposit(uint256,address,uint256,uint256)
    pub fn event_log(&self) -> Result<Log, BridgeError> {
        let event_data = SolBridgeDepositEvent {
            receiver: h160_to_alloy(&self.receiver.to_h160()?),
            amount: u256_to_alloy(&self.amount),
            inbox_level: u256_to_alloy(&U256::from(self.inbox_level)),
            inbox_msg_id: u256_to_alloy(&U256::from(self.inbox_msg_id)),
        };

        let data = SolBridgeDepositEvent::encode_data(&event_data);

        Ok(Log {
            // Emitted by the "system" contract
            address: Address::ZERO,
            // (Event ID (non-anonymous) and indexed fields, Non-indexed fields)
            data: LogData::new_unchecked(
                vec![B256::from_slice(&DEPOSIT_EVENT_TOPIC)],
                data.into(),
            ),
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
                    receiver: DepositReceiver::Ethereum(receiver),
                    inbox_level: 0,
                    inbox_msg_id: 0,
                })
            }
            4 => {
                let mut it = decoder.iter();
                let amount: U256 = decode_field(&next(&mut it)?, "amount")?;
                let receiver_decoder = next(&mut it)?;
                let receiver: DepositReceiver =
                    decode_field(&receiver_decoder, "receiver")?;
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

pub struct DepositResult<Outcome> {
    pub outcome: Outcome,
    #[allow(dead_code)]
    pub runtime: RuntimeId,
}

#[allow(clippy::too_many_arguments)]
#[trace_kernel]
pub fn apply_tezosx_xtz_deposit<Host: Runtime>(
    host: &mut Host,
    registry: &impl Registry,
    deposit: &Deposit,
    block_constants: &BlockConstants,
    transaction_hash: [u8; TRANSACTION_HASH_SIZE],
    tracer_input: Option<TracerInput>,
    spec_id: &SpecId,
    limits: &EvmLimits,
) -> Result<ExecutionResult<TransactionResult>, crate::Error> {
    match &deposit.receiver {
        DepositReceiver::Ethereum(_) => {
            let execution_outcome = pure_xtz_deposit(
                host,
                registry,
                deposit,
                block_constants,
                transaction_hash,
                limits.maximum_gas_limit,
                spec_id,
                tracer_input,
            )?;

            let transaction_result = TransactionResult {
                // A specific address is allocated for queue call
                // System address can only be used as caller for simulations
                caller: alloy_to_h160(&FEED_DEPOSIT_ADDR),
                execution_outcome,
                estimated_ticks_used: TICKS_FOR_DEPOSIT,
                runtime: RuntimeId::Ethereum,
            };

            Ok(ExecutionResult::Valid(transaction_result))
        }
        DepositReceiver::Tezos(Contract::Implicit(pkh)) => {
            let amount = mutez_from_wei(deposit.amount)
                .map_err(|_| crate::Error::InvalidConversion)?;

            match TezosRuntime::add_balance(host, pkh, U256::from(amount))
                .map_err(|e| revm_etherlink::Error::Custom(e.to_string()))
            {
                Ok(()) => {
                    let execution_outcome = ExecutionOutcome {
                        result: revm::context::result::ExecutionResult::Success {
                            reason: SuccessReason::Return,
                            gas_used: DEPOSIT_EXECUTION_GAS_COST,
                            gas_refunded: 0,
                            logs: vec![deposit
                                .event_log()
                                .map_err(|_| crate::Error::InvalidConversion)?],
                            output: Output::Call(Bytes::from_static(&[1u8])),
                        },
                        withdrawals: vec![],
                    };

                    let transaction_result = TransactionResult {
                        caller: H160::zero(),
                        execution_outcome,
                        estimated_ticks_used: TICKS_FOR_DEPOSIT,
                        runtime: RuntimeId::Tezos,
                    };

                    Ok(ExecutionResult::Valid(transaction_result))
                }
                Err(err) => {
                    log!(host, Info, "Deposit failed because of {err}");
                    let execution_outcome = ExecutionOutcome {
                        result: revm::context::result::ExecutionResult::Revert {
                            gas_used: DEPOSIT_EXECUTION_GAS_COST,
                            output: Bytes::from_static(&[0u8]),
                        },
                        withdrawals: vec![],
                    };

                    let transaction_result = TransactionResult {
                        caller: H160::zero(),
                        execution_outcome,
                        estimated_ticks_used: TICKS_FOR_DEPOSIT,
                        runtime: RuntimeId::Tezos,
                    };

                    Ok(ExecutionResult::Valid(transaction_result))
                }
            }
        }
        DepositReceiver::Tezos(Contract::Originated(kt1)) => Err(
            crate::Error::BridgeError(format!("Invalid deposit receiver: {kt1}")),
        ),
    }
}

pub const TEZLINK_DEPOSITOR: [u8; 22] = [0u8; 22];

fn tezlink_deposit<Host: Runtime, C: Context>(
    host: &mut Host,
    context: &C,
    amount: u64,
    receiver: Contract,
) -> Result<TransferSuccess, TransferError> {
    let to_account = context
        .implicit_from_contract(&receiver)
        .map_err(|_| TransferError::FailedToFetchDestinationAccount)?;

    let _was_allocated = to_account
        .allocate(host)
        .map_err(|_| TransferError::FailedToAllocateDestination)?;

    match to_account.add_balance(host, amount) {
        Ok(()) => {
            let mut success = TransferSuccess::default();
            let depositor = Contract::nom_read_exact(&TEZLINK_DEPOSITOR)
                .map_err(|e| TransferError::DepositError(format!("{e:?}")))?;
            let balance_updates = BalanceUpdate::transfer(depositor, receiver, amount);
            success.balance_updates = balance_updates;
            Ok(success)
        }
        Err(e) => {
            log!(host, Error, "Deposit failed because of {e}");
            Err(TransferError::DepositError(e.to_string()))
        }
    }
}

type TezlinkOutcome = (ContentResult<TransferContent>, TransferContent);

pub fn execute_tezlink_deposit<Host: Runtime, C: Context>(
    host: &mut Host,
    context: &C,
    deposit: &Deposit,
) -> Result<DepositResult<TezlinkOutcome>, BridgeError> {
    // We should be able to obtain an account for arbitrary H160 address
    // otherwise it is a fatal error.
    let receiver = deposit.receiver.to_contract()?;

    let amount = mutez_from_wei(deposit.amount)
        .map_err(|_| BridgeError::InvalidAmount(deposit.amount))?;

    let content = TransferContent {
        amount: amount.into(),
        destination: receiver.clone(),
        parameters: Parameters::default(),
    };

    let result = match tezlink_deposit(host, context, amount, receiver) {
        Ok(success) => ContentResult::Applied(TransferTarget::ToContrat(success)),
        Err(err) => ContentResult::Failed(ApplyOperationErrors {
            errors: vec![err.into()],
        }),
    };

    let result = DepositResult {
        outcome: (result, content),
        runtime: RuntimeId::Tezos,
    };

    Ok(result)
}

#[cfg(test)]
mod tests {
    use crate::registry_impl::RegistryImpl;
    use alloy_sol_types::SolEvent;
    use num_bigint::BigInt;
    use primitive_types::{H160, U256};
    use revm::primitives::hardfork::SpecId;
    use revm_etherlink::precompiles::initializer::init_precompile_bytecodes;
    use rlp::{Decodable, RlpStream};
    use tezos_crypto_rs::hash::ContractKt1Hash;
    use tezos_ethereum::{
        block::{BlockConstants, BlockFees},
        rlp_helpers::{append_option_explicit, append_u256_le},
        transaction::TRANSACTION_HASH_SIZE,
    };
    use tezos_evm_runtime::runtime::MockKernelHost;
    use tezos_protocol::contract::Contract;
    use tezos_smart_rollup::michelson::{
        ticket::FA2_1Ticket, MichelsonNat, MichelsonOption, MichelsonPair,
    };
    use tezos_smart_rollup_encoding::michelson::MichelsonBytes;

    use crate::{
        apply::{ExecutionResult, TransactionResult},
        bridge::{DepositInfo, DepositReceiver, DEPOSIT_EVENT_TOPIC},
        chains::EvmLimits,
    };

    use super::{apply_tezosx_xtz_deposit, Deposit};

    mod xtz_events {
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
            amount: U256::from(1u64),
            receiver: DepositReceiver::Ethereum(H160::from([2u8; 20])),
            inbox_level: 3,
            inbox_msg_id: 4,
        }
    }

    pub fn create_fa_ticket(
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
    fn deposit_event_topic() {
        assert_eq!(xtz_events::Deposit::SIGNATURE_HASH.0, DEPOSIT_EVENT_TOPIC);
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
                receiver: DepositReceiver::Ethereum(H160([1u8; 20])),
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
                receiver: DepositReceiver::Ethereum(H160::from([1u8; 20])),
                inbox_level: 0,
                inbox_msg_id: 0,
            }
        );
        pretty_assertions::assert_eq!(chain_id, Some(U256::one()))
    }

    #[test]
    fn ensure_deposit_legacy_and_deposit_have_no_collision() {
        let receiver = H160::from_slice(&[1u8; 20]);
        let chain_id: U256 = 15544u64.into();
        let mut chain_id_encoded = [0u8; 32];
        chain_id.to_little_endian(&mut chain_id_encoded);

        // Receiver representation should be 20 bytes long
        let legacy_receiver = MichelsonBytes(receiver.as_bytes().to_vec());

        assert_eq!(legacy_receiver.0.len(), Deposit::RECEIVER_LENGTH);

        // Receiver and chain_id old representation should be 52 bytes long
        let mut legacy_receiver_and_chain_id = MichelsonBytes(vec![]);
        legacy_receiver_and_chain_id
            .0
            .extend_from_slice(receiver.as_bytes());
        legacy_receiver_and_chain_id
            .0
            .extend_from_slice(&chain_id_encoded);

        assert_eq!(
            legacy_receiver_and_chain_id.0.len(),
            Deposit::RECEIVER_AND_CHAIN_ID_LENGTH
        );

        // DepositInfo with no chain_id representation should be 24 bytes long
        const RLP_DEPOSIT_NO_CHAIN_ID: usize = 24;
        let deposit_info = DepositInfo {
            receiver: DepositReceiver::Ethereum(receiver),
            chain_id: None,
        };
        let mut stream = RlpStream::new();
        stream.append(&1u8);
        stream.begin_list(2);
        stream.append(&deposit_info.receiver);
        stream.append(&deposit_info.chain_id);
        let deposit_info_no_chain_id = MichelsonBytes(stream.as_raw().to_vec());

        assert_eq!(deposit_info_no_chain_id.0.len(), RLP_DEPOSIT_NO_CHAIN_ID);

        // DepositInfo with no chain_id representation should be 56 bytes long
        const RLP_DEPOSIT_CHAIN_ID: usize = 56;
        let deposit_info = DepositInfo {
            receiver: DepositReceiver::Ethereum(receiver),
            chain_id: Some(chain_id),
        };
        let mut stream = RlpStream::new();
        stream.append(&1u8);
        stream.begin_list(2);
        stream.append(&deposit_info.receiver);
        append_option_explicit(&mut stream, &deposit_info.chain_id, append_u256_le);

        let deposit_info_chain_id = MichelsonBytes(stream.as_raw().to_vec());

        assert_eq!(deposit_info_chain_id.0.len(), RLP_DEPOSIT_CHAIN_ID);

        let ticket =
            create_fa_ticket("KT18amZmM5W7qDWVt2pH6uj7sCEd3kbzLrHT", 0, &[0u8], 2.into());
        let result_legacy_receiver =
            Deposit::try_parse(ticket, legacy_receiver, 0, 0).expect("Failed to parse");

        let ticket =
            create_fa_ticket("KT18amZmM5W7qDWVt2pH6uj7sCEd3kbzLrHT", 0, &[0u8], 2.into());
        let result_legacy_receiver_chain_id =
            Deposit::try_parse(ticket, legacy_receiver_and_chain_id, 0, 0)
                .expect("Failed to parse");

        let ticket =
            create_fa_ticket("KT18amZmM5W7qDWVt2pH6uj7sCEd3kbzLrHT", 0, &[0u8], 2.into());
        let result_deposit_info_no_chain_id =
            Deposit::try_parse(ticket, deposit_info_no_chain_id, 0, 0)
                .expect("Failed to parse");

        let ticket =
            create_fa_ticket("KT18amZmM5W7qDWVt2pH6uj7sCEd3kbzLrHT", 0, &[0u8], 2.into());
        let result_deposit_info_chain_id =
            Deposit::try_parse(ticket, deposit_info_chain_id, 0, 0)
                .expect("Failed to parse");

        pretty_assertions::assert_eq!(
            result_legacy_receiver,
            result_deposit_info_no_chain_id,
        );

        pretty_assertions::assert_eq!(
            result_legacy_receiver_chain_id,
            result_deposit_info_chain_id,
        );
    }

    #[test]
    fn deposit_decode_legacy() {
        let mut stream = rlp::RlpStream::new_list(2);
        stream
            .append(&primitive_types::U256::one())
            .append(&primitive_types::H160([1u8; 20]));
        let bytes = stream.out().to_vec();
        let decoder = rlp::Rlp::new(&bytes);
        let res = Deposit::decode(&decoder).unwrap();
        assert_eq!(
            res,
            Deposit {
                amount: U256::one(),
                receiver: DepositReceiver::Ethereum(H160::from([1u8; 20])),
                inbox_level: 0,
                inbox_msg_id: 0,
            }
        );
    }

    #[test]
    fn deposit_execution_outcome_contains_event() {
        let mut host = MockKernelHost::default();
        let registry = &RegistryImpl::new();
        init_precompile_bytecodes(&mut host).unwrap();

        let deposit = dummy_deposit();

        let limits = EvmLimits::default();
        let execution_result = apply_tezosx_xtz_deposit(
            &mut host,
            registry,
            &deposit,
            &BlockConstants::first_block(
                U256::zero(),
                U256::zero(),
                BlockFees::new(
                    limits.minimum_base_fee_per_gas,
                    U256::zero(),
                    U256::zero(),
                ),
                limits.maximum_gas_limit,
                H160::zero(),
            ),
            [0; TRANSACTION_HASH_SIZE],
            None,
            &SpecId::default(),
            &limits,
        )
        .unwrap();
        let outcome = if let ExecutionResult::Valid(TransactionResult {
            execution_outcome,
            ..
        }) = execution_result
        {
            execution_outcome
        } else {
            panic!("Deposit is invalid")
        };
        let logs = outcome.result.logs();
        assert!(outcome.result.is_success());
        assert_eq!(logs.len(), 1);

        let event = xtz_events::Deposit::decode_log_data(&logs[0].data).unwrap();
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
        let registry = &RegistryImpl::new();
        init_precompile_bytecodes(&mut host).unwrap();

        let mut deposit = dummy_deposit();
        deposit.amount = U256::MAX;

        let limits = EvmLimits::default();
        let execution_result = apply_tezosx_xtz_deposit(
            &mut host,
            registry,
            &deposit,
            &BlockConstants::first_block(
                U256::zero(),
                U256::zero(),
                BlockFees::new(
                    limits.minimum_base_fee_per_gas,
                    U256::zero(),
                    U256::zero(),
                ),
                limits.maximum_gas_limit,
                H160::zero(),
            ),
            [0; TRANSACTION_HASH_SIZE],
            None,
            &SpecId::default(),
            &limits,
        )
        .unwrap();
        let outcome = if let ExecutionResult::Valid(TransactionResult {
            execution_outcome,
            ..
        }) = execution_result
        {
            execution_outcome
        } else {
            panic!("Deposit is invalid")
        };
        assert!(outcome.result.is_success());

        let execution_result = apply_tezosx_xtz_deposit(
            &mut host,
            registry,
            &deposit,
            &BlockConstants::first_block(
                U256::zero(),
                U256::zero(),
                BlockFees::new(
                    limits.minimum_base_fee_per_gas,
                    U256::zero(),
                    U256::zero(),
                ),
                limits.maximum_gas_limit,
                H160::zero(),
            ),
            [0; TRANSACTION_HASH_SIZE],
            None,
            &SpecId::default(),
            &limits,
        )
        .unwrap();
        let outcome = if let ExecutionResult::Valid(TransactionResult {
            execution_outcome,
            ..
        }) = execution_result
        {
            execution_outcome
        } else {
            panic!("Deposit is invalid")
        };
        assert!(!outcome.result.is_success());
        assert!(outcome.result.logs().is_empty());
    }

    #[test]
    fn bridge_deposit_event_log_consistent() {
        let deposit = dummy_deposit();
        let log = deposit.event_log();
        let expected_log = hex::decode("0000000000000000000000000000000000000000000000000000000000000001\
                                        0000000000000000000000000202020202020202020202020202020202020202\
                                        0000000000000000000000000000000000000000000000000000000000000003\
                                        0000000000000000000000000000000000000000000000000000000000000004").unwrap();
        assert_eq!(expected_log, log.unwrap().data.data)
    }
}
