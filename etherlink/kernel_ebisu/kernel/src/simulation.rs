// SPDX-FileCopyrightText: 2022-2024 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2024-2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

// Module containing most Simulation related code, in one place, to be deleted
// when the proxy node simulates directly

use std::borrow::Cow;

use crate::apply::revm_run_transaction;
use crate::block_storage;
use crate::chains::{ChainFamily, ETHERLINK_SAFE_STORAGE_ROOT_PATH};
use crate::fees::simulation_add_gas_for_fees;
use crate::l2block::L2Block;
use crate::storage::{
    read_last_info_per_level_timestamp, read_or_set_maximum_gas_per_transaction,
    read_sequencer_pool_address, read_tracer_input,
};
use crate::tick_model::constants::MAXIMUM_GAS_LIMIT;
use crate::{error::Error, storage};

use crate::{parsable, parsing, retrieve_chain_id};

use evm::Config;
use evm_execution::handler::{
    ExecutionOutcome, ExecutionResult as ExecutionOutcomeResult,
};
use evm_execution::trace::TracerInput;
use evm_execution::EthereumError;
use primitive_types::{H160, U256};
use revm_etherlink::u256_to_alloy;
use rlp::{Decodable, DecoderError, Encodable, Rlp};
use tezos_ethereum::access_list::empty_access_list;
use tezos_ethereum::block::{BlockConstants, BlockFees};
use tezos_ethereum::rlp_helpers::{
    append_option_u64_le, check_list, decode_field, decode_option, decode_option_u64_le,
    decode_timestamp, next, VersionedEncoding,
};
use tezos_ethereum::transaction::TransactionObject;
use tezos_evm_logging::{log, Level::*};
use tezos_evm_runtime::runtime::Runtime;
use tezos_smart_rollup::types::Timestamp;

// SIMULATION/SIMPLE/RLP_ENCODED_SIMULATION
pub const SIMULATION_SIMPLE_TAG: u8 = 1;
// SIMULATION/CREATE/NUM_CHUNKS 2B
pub const SIMULATION_CREATE_TAG: u8 = 2;
// SIMULATION/CHUNK/NUM 2B/CHUNK
pub const SIMULATION_CHUNK_TAG: u8 = 3;
/// Tag indicating simulation is an evaluation.
pub const EVALUATION_TAG: u8 = 0x00;
/// Tag indicating simulation is a validation.
///
/// This tag can no longer be used, if you plan to add another tag,
/// do not reuse this one to avoid mistakes.
const _VALIDATION_TAG: u8 = 0x01;

/// Version of the encoding in use.
pub const SIMULATION_ENCODING_VERSION: u8 = 0x01;

pub const OK_TAG: u8 = 0x1;
pub const ERR_TAG: u8 = 0x2;

const OUT_OF_TICKS_MSG: &str = "The transaction would exhaust all the ticks it
    is allocated. Try reducing its gas consumption or splitting the call in
    multiple steps, if possible.";
// Redefined Result as we cannot implement Decodable and Encodable traits on Result
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum SimulationResult<T, E> {
    Ok(T),
    Err(E),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ExecutionResult {
    value: Option<Vec<u8>>,
    gas_used: Option<u64>,
}

type CallResult = SimulationResult<ExecutionResult, Vec<u8>>;

#[derive(Debug, PartialEq, Clone)]
pub struct ValidationResult {
    transaction_object: TransactionObject,
}

impl<T: Encodable, E: Encodable> Encodable for SimulationResult<T, E> {
    fn rlp_append(&self, stream: &mut rlp::RlpStream) {
        stream.begin_list(2);
        match self {
            Self::Ok(value) => {
                stream.append(&OK_TAG);
                stream.append(value)
            }
            Self::Err(e) => {
                stream.append(&ERR_TAG);
                stream.append(e)
            }
        };
    }
}

impl<T: Decodable, E: Decodable> Decodable for SimulationResult<T, E> {
    fn decode(decoder: &Rlp<'_>) -> Result<Self, DecoderError> {
        check_list(decoder, 2)?;

        let mut it = decoder.iter();
        match decode_field(&next(&mut it)?, "tag")? {
            OK_TAG => Ok(Self::Ok(decode_field(&next(&mut it)?, "ok")?)),
            ERR_TAG => Ok(Self::Err(decode_field(&next(&mut it)?, "error")?)),
            _ => Err(DecoderError::Custom("Invalid execution tag")),
        }
    }
}

impl Encodable for ExecutionResult {
    fn rlp_append(&self, stream: &mut rlp::RlpStream) {
        stream.begin_list(2);
        stream.append(&self.value);
        append_option_u64_le(&self.gas_used, stream);
    }
}

impl Decodable for ExecutionResult {
    fn decode(decoder: &Rlp<'_>) -> Result<Self, DecoderError> {
        check_list(decoder, 2)?;

        let mut it = decoder.iter();
        let value = decode_field(&next(&mut it)?, "value")?;
        let gas_used = decode_option_u64_le(&next(&mut it)?, "gas_used")?;
        Ok(ExecutionResult { value, gas_used })
    }
}

impl Encodable for ValidationResult {
    fn rlp_append(&self, stream: &mut rlp::RlpStream) {
        self.transaction_object.rlp_append(stream);
    }
}

impl Decodable for ValidationResult {
    fn decode(decoder: &Rlp) -> Result<Self, DecoderError> {
        Ok(ValidationResult {
            transaction_object: TransactionObject::decode(decoder)?,
        })
    }
}

/// Container for eth_call data, used in messages sent by the rollup node
/// simulation.
///
/// They are transmitted in RLP encoded form, in messages of the form\
/// `\parsing::SIMULATION_TAG \SIMULATION_SIMPLE_TAG \<rlp encoded Evaluation>`\
/// or in chunks if they are bigger than what the inbox can receive, with a
/// first message giving the number of chunks\
/// `\parsing::SIMULATION_TAG \SIMULATION_CREATE_TAG \XXXX`
/// where `XXXX` is 2 bytes containing the number of chunks, followed by the
/// chunks:\
/// `\parsing::SIMULATION_TAG \SIMULATION_CHUNK_TAG \XXXX \<bytes>`\
/// where `XXXX` is the number of the chunk over 2 bytes, and the rest is a
/// chunk of the rlp encoded evaluation.
///
/// Ethereum doc: https://ethereum.org/en/developers/docs/apis/json-rpc/#eth_call
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Evaluation {
    /// (optional) The address the transaction is sent from.\
    /// Encoding: 20 bytes or empty (0x80)
    pub from: Option<H160>,
    /// The address the transaction is directed to.
    /// Some indexer seem to expect it to be optionnal\
    /// Encoding: 20 bytes
    pub to: Option<H160>,
    /// (optional) Integer of the gas provided for the transaction execution.
    /// eth_call consumes zero gas, but this parameter may be needed by some
    /// executions.\
    /// Encoding: little endian
    pub gas: Option<u64>,
    /// (optional) Integer of the gasPrice used for each paid gas\
    /// Encoding: little endian
    pub gas_price: Option<u64>,
    /// (optional) Integer of the value sent with this transaction (in Wei)\
    /// Encoding: little endian
    pub value: Option<U256>,
    /// (optional) Hash of the method signature and encoded parameters.
    pub data: Vec<u8>,
    /// The gas returned by the simualtion include the DA fees if this parameter
    /// is set to true.
    ///
    /// Important: latest versions of the node no longer use with_da_fees. All
    /// simulation set with_da_fees to false. The node now adds the da fees
    /// itself.
    /// The field is not removed from the kernel simulation for retro-compatibility
    /// reason, and in case we want to revert and switch back to this implementation.
    pub with_da_fees: bool,
    /// The timestamp used during simulation. It is marked as optional as
    /// the support has been added in `StorageVersion::V13`.
    pub timestamp: Option<Timestamp>,
}

impl<T> From<EthereumError> for SimulationResult<T, String> {
    fn from(err: EthereumError) -> Self {
        let msg = format!("The transaction failed: {err:?}.");
        Self::Err(msg)
    }
}

impl From<Result<Option<ExecutionOutcome>, EthereumError>>
    for SimulationResult<CallResult, String>
{
    fn from(result: Result<Option<ExecutionOutcome>, EthereumError>) -> Self {
        match result {
            Ok(Some(ExecutionOutcome {
                gas_used, result, ..
            })) if result.is_success() => {
                Self::Ok(SimulationResult::Ok(ExecutionResult {
                    value: result.output().map(|x| x.to_vec()),
                    gas_used: Some(gas_used),
                }))
            }
            Ok(Some(
                outcome @ ExecutionOutcome {
                    result: ExecutionOutcomeResult::CallReverted(_),
                    ..
                },
            )) => Self::Ok(SimulationResult::Err(
                outcome.output().unwrap_or_default().to_vec(),
            )),
            Ok(Some(ExecutionOutcome {
                result: ExecutionOutcomeResult::OutOfTicks,
                ..
            })) => Self::Err(String::from(OUT_OF_TICKS_MSG)),
            Ok(Some(ExecutionOutcome { result, .. })) => {
                let msg = format!("The transaction failed: {result:?}.");
                Self::Err(msg)
            }
            Ok(None) => Self::Err(String::from(
                "No outcome was produced when the transaction was ran",
            )),
            Err(err) => err.into(),
        }
    }
}

impl Evaluation {
    fn rlp_decode_v0(decoder: &Rlp<'_>) -> Result<Self, DecoderError> {
        // the proxynode works preferably with little endian
        let u64_from_le = |v: Vec<u8>| u64::from_le_bytes(parsable!(v.try_into().ok()));
        let u256_from_le = |v: Vec<u8>| U256::from_little_endian(&v);
        if decoder.is_list() {
            if Ok(6) == decoder.item_count() {
                let mut it = decoder.iter();
                let from: Option<H160> = decode_option(&next(&mut it)?, "from")?;
                let to: Option<H160> = decode_option(&next(&mut it)?, "to")?;
                let gas: Option<u64> =
                    decode_option(&next(&mut it)?, "gas")?.map(u64_from_le);
                let gas_price: Option<u64> =
                    decode_option(&next(&mut it)?, "gas_price")?.map(u64_from_le);
                let value: Option<U256> =
                    decode_option(&next(&mut it)?, "value")?.map(u256_from_le);
                let data: Vec<u8> = decode_field(&next(&mut it)?, "data")?;
                Ok(Self {
                    from,
                    to,
                    gas,
                    gas_price,
                    value,
                    data,
                    with_da_fees: true,
                    timestamp: None,
                })
            } else {
                Err(DecoderError::RlpIncorrectListLen)
            }
        } else {
            Err(DecoderError::RlpExpectedToBeList)
        }
    }

    fn rlp_decode_v1(decoder: &Rlp<'_>) -> Result<Self, DecoderError> {
        // the proxynode works preferably with little endian
        let u64_from_le = |v: Vec<u8>| u64::from_le_bytes(parsable!(v.try_into().ok()));
        let u256_from_le = |v: Vec<u8>| U256::from_little_endian(&v);
        if decoder.is_list() {
            if Ok(7) == decoder.item_count() {
                let mut it = decoder.iter();
                let from: Option<H160> = decode_option(&next(&mut it)?, "from")?;
                let to: Option<H160> = decode_option(&next(&mut it)?, "to")?;
                let gas: Option<u64> =
                    decode_option(&next(&mut it)?, "gas")?.map(u64_from_le);
                let gas_price: Option<u64> =
                    decode_option(&next(&mut it)?, "gas_price")?.map(u64_from_le);
                let value: Option<U256> =
                    decode_option(&next(&mut it)?, "value")?.map(u256_from_le);
                let data: Vec<u8> = decode_field(&next(&mut it)?, "data")?;
                let with_da_fees: bool = decode_field(&next(&mut it)?, "with_da_fees")?;
                Ok(Self {
                    from,
                    to,
                    gas,
                    gas_price,
                    value,
                    data,
                    with_da_fees,
                    timestamp: None,
                })
            } else {
                Err(DecoderError::RlpIncorrectListLen)
            }
        } else {
            Err(DecoderError::RlpExpectedToBeList)
        }
    }

    fn rlp_decode_v2(decoder: &Rlp<'_>) -> Result<Self, DecoderError> {
        // the proxynode works preferably with little endian
        let u64_from_le = |v: Vec<u8>| u64::from_le_bytes(parsable!(v.try_into().ok()));
        let u256_from_le = |v: Vec<u8>| U256::from_little_endian(&v);
        if decoder.is_list() {
            if Ok(8) == decoder.item_count() {
                let mut it = decoder.iter();
                let from: Option<H160> = decode_option(&next(&mut it)?, "from")?;
                let to: Option<H160> = decode_option(&next(&mut it)?, "to")?;
                let gas: Option<u64> =
                    decode_option(&next(&mut it)?, "gas")?.map(u64_from_le);
                let gas_price: Option<u64> =
                    decode_option(&next(&mut it)?, "gas_price")?.map(u64_from_le);
                let value: Option<U256> =
                    decode_option(&next(&mut it)?, "value")?.map(u256_from_le);
                let data: Vec<u8> = decode_field(&next(&mut it)?, "data")?;
                let with_da_fees: bool = decode_field(&next(&mut it)?, "with_da_fees")?;
                let timestamp: Timestamp = decode_timestamp(&next(&mut it)?)?;
                Ok(Self {
                    from,
                    to,
                    gas,
                    gas_price,
                    value,
                    data,
                    with_da_fees,
                    timestamp: Some(timestamp),
                })
            } else {
                Err(DecoderError::RlpIncorrectListLen)
            }
        } else {
            Err(DecoderError::RlpExpectedToBeList)
        }
    }

    pub fn from_bytes(bytes: &[u8]) -> Result<Evaluation, DecoderError> {
        let first = *bytes.first().ok_or(DecoderError::Custom("Empty bytes"))?;
        match first {
            0x01 => {
                let decoder = Rlp::new(&bytes[1..]);
                Self::rlp_decode_v1(&decoder)
            }
            0x02 => {
                let decoder = Rlp::new(&bytes[1..]);
                Self::rlp_decode_v2(&decoder)
            }
            _ => {
                let decoder = Rlp::new(bytes);
                Self::rlp_decode_v0(&decoder)
            }
        }
    }

    /// Execute the simulation
    pub fn run<Host: Runtime>(
        &self,
        host: &mut Host,
        tracer_input: Option<TracerInput>,
        evm_configuration: &Config,
    ) -> Result<SimulationResult<CallResult, String>, Error> {
        let chain_id = retrieve_chain_id(host)?;
        let minimum_base_fee_per_gas = crate::retrieve_minimum_base_fee_per_gas(host);
        let da_fee = crate::retrieve_da_fee(host)?;
        let coinbase = read_sequencer_pool_address(host).unwrap_or_default();

        let constants = match block_storage::read_current(
            host,
            &ETHERLINK_SAFE_STORAGE_ROOT_PATH,
            &ChainFamily::Evm,
        ) {
            Ok(L2Block::Etherlink(block)) => {
                // Timestamp is taken from the simulation caller if provided.
                // If the timestamp is missing, because of an older evm-node,
                // default to last block timestamp.
                let timestamp = self
                    .timestamp
                    .map(|timestamp| U256::from(timestamp.as_u64()))
                    .unwrap_or_else(|| U256::from(block.timestamp.as_u64()));

                let block_fees = BlockFees::new(
                    minimum_base_fee_per_gas,
                    block.base_fee_per_gas,
                    da_fee,
                );
                BlockConstants {
                    number: block.number + 1,
                    coinbase,
                    timestamp,
                    gas_limit: crate::block::GAS_LIMIT,
                    block_fees,
                    chain_id,
                    prevrandao: None,
                }
            }
            Ok(L2Block::Tezlink(_block)) => {
                log!(
                    host,
                    Fatal,
                    "Read a tezlink block when expecting an etherlink block"
                );
                return Err(Error::Simulation(EthereumError::WrappedError(Cow::from(
                    "Should not have found a Tezlink block",
                ))));
            }
            Err(_) => {
                // Timestamp is taken from the simulation caller if provided.
                // If the timestamp is missing, because of an older evm-node,
                // default to current timestamp.
                let timestamp = self
                    .timestamp
                    .map(|timestamp| U256::from(timestamp.as_u64()))
                    .unwrap_or_else(|| {
                        U256::from(
                            read_last_info_per_level_timestamp(host)
                                .unwrap_or(Timestamp::from(0))
                                .as_u64(),
                        )
                    });

                let base_fee_per_gas = minimum_base_fee_per_gas;
                let block_fees =
                    BlockFees::new(minimum_base_fee_per_gas, base_fee_per_gas, da_fee);
                BlockConstants::first_block(
                    timestamp,
                    chain_id,
                    block_fees,
                    crate::block::GAS_LIMIT,
                    coinbase,
                )
            }
        };

        let gas_price = if let Some(gas_price) = self.gas_price {
            U256::from(gas_price)
        } else {
            constants.block_fees.base_fee_per_gas()
        };

        let from = self.from.unwrap_or(H160::zero());

        let mut simulation_caller =
            revm_etherlink::storage::world_state_handler::StorageAccount::from_address(
                &revm::primitives::Address::from_slice(&from.0),
            )?;
        let max_gas_limit =
            read_or_set_maximum_gas_per_transaction(host).unwrap_or(MAXIMUM_GAS_LIMIT);
        let gas = self
            .gas
            .map_or(max_gas_limit, |gas| u64::min(gas, max_gas_limit));
        let max_gas_to_pay = constants.base_fee_per_gas() * gas;

        // If the simulation is performed with the zero address and has a non
        // null value, the simulation will fail with out of funds.
        // This can be problematic as some tools doesn't provide the `from`
        // field but provide a non null `value`.
        //
        // We solve the issue by giving funds before the simulation to the
        // zero address if necessary.
        if from.is_zero() {
            if let Some(value) = self.value {
                let mut info = simulation_caller.info(host)?;
                info.balance = u256_to_alloy(&value.saturating_add(max_gas_to_pay));
                simulation_caller.set_info_without_code(host, info)?;
            }
        }

        let mut info = simulation_caller.info(host)?;
        info.balance = info.balance.saturating_add(u256_to_alloy(&max_gas_to_pay));
        simulation_caller.set_info_without_code(host, info)?;

        match revm_run_transaction(
            host,
            &constants,
            from,
            self.to,
            self.value.unwrap_or_default(),
            gas,
            self.data.clone(),
            gas_price,
            // TODO: Replace this by the decoded access lists if any.
            empty_access_list(),
            None,
            evm_configuration,
            tracer_input,
            true,
        ) {
            Ok(Some(outcome)) if !self.with_da_fees => {
                let result: SimulationResult<CallResult, String> =
                    Result::Ok(Some(outcome)).into();

                Ok(result)
            }
            Ok(Some(outcome)) => {
                let outcome = simulation_add_gas_for_fees(
                    outcome,
                    &constants.block_fees,
                    &self.data,
                )
                .map_err(Error::Simulation)?;

                let result: SimulationResult<CallResult, String> =
                    Result::Ok(Some(outcome)).into();

                Ok(result)
            }
            Ok(None) => {
                let result: SimulationResult<CallResult, String> =
                    Result::Ok(None).into();

                Ok(result)
            }
            Err(err) => {
                let result: SimulationResult<CallResult, String> =
                    Result::Err(EthereumError::WrappedError(Cow::Owned(err.to_string())))
                        .into();

                Ok(result)
            }
        }
    }
}

#[derive(Debug, PartialEq)]
enum Message {
    Evaluation(Evaluation),
}

impl TryFrom<&[u8]> for Message {
    type Error = DecoderError;

    fn try_from(bytes: &[u8]) -> Result<Self, Self::Error> {
        let Some(&tag) = bytes.first() else {
            return Err(DecoderError::Custom("Empty simulation message"));
        };
        let Some(bytes) = bytes.get(1..) else {
            return Err(DecoderError::Custom("Empty simulation message"));
        };

        match tag {
            EVALUATION_TAG => Evaluation::from_bytes(bytes).map(Message::Evaluation),
            _ => Err(DecoderError::Custom("Unknown message to simulate")),
        }
    }
}

#[derive(Default, Debug, PartialEq)]
enum Input {
    #[default]
    Unparsable,
    Simple(Box<Message>),
    NewChunked(u16),
    Chunk {
        i: u16,
        data: Vec<u8>,
    },
}

impl Input {
    fn parse_new_chunk_simulation(bytes: &[u8]) -> Self {
        let num_chunks = u16::from_le_bytes(parsable!(bytes.try_into().ok()));
        Self::NewChunked(num_chunks)
    }

    fn parse_simulation_chunk(bytes: &[u8]) -> Self {
        let (num, remaining) = parsable!(parsing::split_at(bytes, 2));
        let i = u16::from_le_bytes(num.try_into().unwrap());
        Self::Chunk {
            i,
            data: remaining.to_vec(),
        }
    }
    fn parse_simple_simulation(bytes: &[u8]) -> Self {
        let message = parsable!(bytes.try_into().ok());
        Input::Simple(Box::new(message))
    }

    // Internal custom message structure :
    // SIMULATION_TAG 1B / MESSAGE_TAG 1B / DATA
    fn parse(input: &[u8]) -> Self {
        if input.len() <= 3 {
            return Self::Unparsable;
        }
        let internal = parsable!(input.first());
        let message = parsable!(input.get(1));
        let data = parsable!(input.get(2..));
        if *internal != parsing::SIMULATION_TAG {
            return Self::Unparsable;
        }
        match *message {
            SIMULATION_SIMPLE_TAG => Self::parse_simple_simulation(data),
            SIMULATION_CREATE_TAG => Self::parse_new_chunk_simulation(data),
            SIMULATION_CHUNK_TAG => Self::parse_simulation_chunk(data),
            _ => Self::Unparsable,
        }
    }
}

fn read_chunks<Host: Runtime>(
    host: &mut Host,
    num_chunks: u16,
) -> Result<Message, Error> {
    let mut buffer: Vec<u8> = Vec::new();
    for n in 0..num_chunks {
        match read_input(host)? {
            Input::Chunk { i, data } => {
                if i != n {
                    return Err(Error::InvalidConversion);
                } else {
                    buffer.extend(&data);
                }
            }
            _ => return Err(Error::InvalidConversion),
        }
    }
    Ok(buffer.as_slice().try_into()?)
}

fn read_input<Host: Runtime>(host: &mut Host) -> Result<Input, Error> {
    match host.read_input()? {
        Some(input) => Ok(Input::parse(input.as_ref())),
        None => Ok(Input::Unparsable),
    }
}

fn parse_inbox<Host: Runtime>(host: &mut Host) -> Result<Message, Error> {
    // we just received simulation tag
    // next message is either a simulation or the nb of chunks needed
    match read_input(host)? {
        Input::Simple(s) => Ok(*s),
        Input::NewChunked(num_chunks) => {
            // loop to find the chunks
            read_chunks(host, num_chunks)
        }
        _ => Err(Error::InvalidConversion),
    }
}

impl<T: Encodable + Decodable> VersionedEncoding for SimulationResult<T, String> {
    const VERSION: u8 = SIMULATION_ENCODING_VERSION;
    fn unversionned_encode(&self) -> bytes::BytesMut {
        self.rlp_bytes()
    }
    fn unversionned_decode(decoder: &Rlp) -> Result<Self, DecoderError> {
        Self::decode(decoder)
    }
}

pub fn start_simulation_mode<Host: Runtime>(
    host: &mut Host,
    evm_configuration: &Config,
) -> Result<(), anyhow::Error> {
    log!(host, Debug, "Starting simulation mode ");
    let simulation = parse_inbox(host)?;
    match simulation {
        Message::Evaluation(simulation) => {
            let tracer_input = read_tracer_input(host)?;
            let outcome = simulation.run(host, tracer_input, evm_configuration)?;
            storage::store_simulation_result(host, outcome)
        }
    }
}

#[cfg(test)]
mod tests {

    use evm_execution::{
        account_storage, configuration::EVMVersion, precompiles::PrecompileBTreeMap,
    };
    use primitive_types::H256;
    use tezos_ethereum::{block::BlockConstants, tx_signature::TxSignature};
    use tezos_evm_runtime::runtime::MockKernelHost;

    use crate::{retrieve_block_fees, retrieve_chain_id};

    use super::*;

    fn address_of_str(s: &str) -> Option<H160> {
        let data = &hex::decode(s).unwrap();
        Some(H160::from_slice(data))
    }

    #[test]
    fn test_decode_empty() {
        let input_string =
            hex::decode("da8094353535353535353535353535353535353535353580808080")
                .unwrap();
        let address = address_of_str("3535353535353535353535353535353535353535");
        let expected = Evaluation {
            from: None,
            to: address,
            gas: None,
            gas_price: None,
            value: None,
            data: vec![],
            with_da_fees: true,
            timestamp: None,
        };

        let evaluation = Evaluation::from_bytes(&input_string);

        assert!(evaluation.is_ok(), "Simulation input should be decodable");
        assert_eq!(
            expected,
            evaluation.unwrap(),
            "The decoded result is not the one expected"
        );
    }

    #[test]
    fn test_decode_non_empty() {
        let input_string =
            hex::decode("f84894242424242424242424242424242424242424242494353535353535353535353535353535353535353588672b00000000000088ce56000000000000883582000000000000821616").unwrap();
        let to = address_of_str("3535353535353535353535353535353535353535");
        let from = address_of_str("2424242424242424242424242424242424242424");
        let data = hex::decode("1616").unwrap();
        let expected = Evaluation {
            from,
            to,
            gas: Some(11111),
            gas_price: Some(22222),
            value: Some(U256::from(33333)),
            data,
            with_da_fees: true,
            timestamp: None,
        };

        let evaluation = Evaluation::from_bytes(&input_string);

        assert!(evaluation.is_ok(), "Simulation input should be decodable");
        assert_eq!(
            expected,
            evaluation.unwrap(),
            "The decoded result is not the one expected"
        );
    }

    // The compiled initialization code for the Ethereum demo contract given
    // as an example in kernel_latest/solidity_examples/storage.sol
    const STORAGE_CONTRACT_INITIALIZATION: &str = "608060405234801561001057600080fd5b5061017f806100206000396000f3fe608060405234801561001057600080fd5b50600436106100415760003560e01c80634e70b1dc1461004657806360fe47b1146100645780636d4ce63c14610080575b600080fd5b61004e61009e565b60405161005b91906100d0565b60405180910390f35b61007e6004803603810190610079919061011c565b6100a4565b005b6100886100ae565b60405161009591906100d0565b60405180910390f35b60005481565b8060008190555050565b60008054905090565b6000819050919050565b6100ca816100b7565b82525050565b60006020820190506100e560008301846100c1565b92915050565b600080fd5b6100f9816100b7565b811461010457600080fd5b50565b600081359050610116816100f0565b92915050565b600060208284031215610132576101316100eb565b5b600061014084828501610107565b9150509291505056fea2646970667358221220ec57e49a647342208a1f5c9b1f2049bf1a27f02e19940819f38929bf67670a5964736f6c63430008120033";
    // call: num (direct access to state variable)
    const STORAGE_CONTRACT_CALL_NUM: &str = "4e70b1dc";
    // call: get (public view)
    const STORAGE_CONTRACT_CALL_GET: &str = "6d4ce63c";

    fn create_contract<Host>(host: &mut Host) -> H160
    where
        Host: Runtime,
    {
        let timestamp =
            read_last_info_per_level_timestamp(host).unwrap_or(Timestamp::from(0));
        let timestamp = U256::from(timestamp.as_u64());
        let chain_id = retrieve_chain_id(host);
        assert!(chain_id.is_ok(), "chain_id should be defined");
        let block_fees = retrieve_block_fees(host);
        assert!(chain_id.is_ok(), "chain_id should be defined");
        assert!(block_fees.is_ok(), "block_fees should be defined");
        let block = BlockConstants::first_block(
            timestamp,
            chain_id.unwrap(),
            block_fees.unwrap(),
            crate::block::GAS_LIMIT,
            H160::zero(),
        );
        let mut evm_account_storage = account_storage::init_account_storage().unwrap();

        let callee = None;
        let caller = H160::from_low_u64_be(117);
        let transaction_value = U256::from(0);
        let call_data: Vec<u8> = hex::decode(STORAGE_CONTRACT_INITIALIZATION).unwrap();

        // gas limit was estimated using Remix on Shanghai network (256,842)
        // plus a safety margin for gas accounting discrepancies
        let gas_limit = 300_000;
        let gas_price = U256::from(21000);
        // create contract
        let outcome = evm_execution::run_transaction(
            host,
            &block,
            &mut evm_account_storage,
            &PrecompileBTreeMap::new(),
            &EVMVersion::current_test_config(),
            callee,
            caller,
            call_data,
            Some(gas_limit),
            gas_price,
            transaction_value,
            false,
            None,
            empty_access_list(),
        );
        assert!(outcome.is_ok(), "contract should have been created");
        let outcome = outcome.unwrap();
        assert!(
            outcome.is_some(),
            "execution should have produced some outcome"
        );
        outcome.unwrap().new_address().unwrap()
    }

    #[test]
    fn simulation_result() {
        // setup
        let mut host = MockKernelHost::default();
        let new_address = create_contract(&mut host);

        // run evaluation num
        let evaluation = Evaluation {
            from: None,
            gas_price: None,
            to: Some(new_address),
            data: hex::decode(STORAGE_CONTRACT_CALL_NUM).unwrap(),
            gas: Some(100000),
            value: None,
            with_da_fees: false,
            timestamp: None,
        };
        let outcome = evaluation.run(&mut host, None, &EVMVersion::current_test_config());

        assert!(outcome.is_ok(), "evaluation should have succeeded");
        let outcome = outcome.unwrap();

        if let SimulationResult::Ok(SimulationResult::Ok(ExecutionResult {
            value,
            gas_used: _,
        })) = outcome
        {
            assert_eq!(Some(vec![0u8; 32]), value, "simulation result should be 0");
        } else {
            panic!("evaluation should have reached outcome");
        }

        // run simulation get
        let evaluation = Evaluation {
            from: None,
            gas_price: None,
            to: Some(new_address),
            data: hex::decode(STORAGE_CONTRACT_CALL_GET).unwrap(),
            gas: Some(111111),
            value: None,
            with_da_fees: false,
            timestamp: None,
        };
        let outcome = evaluation.run(&mut host, None, &EVMVersion::current_test_config());

        assert!(outcome.is_ok(), "simulation should have succeeded");
        let outcome = outcome.unwrap();
        if let SimulationResult::Ok(SimulationResult::Ok(ExecutionResult {
            value,
            gas_used: _,
        })) = outcome
        {
            assert_eq!(Some(vec![0u8; 32]), value, "evaluation result should be 0");
        } else {
            panic!("evaluation should have reached outcome");
        }
    }

    #[test]
    fn evaluation_result_no_gas() {
        // setup
        let mut host = MockKernelHost::default();
        let new_address = create_contract(&mut host);

        // run evaluation num
        let evaluation = Evaluation {
            from: None,
            gas_price: None,
            to: Some(new_address),
            data: hex::decode(STORAGE_CONTRACT_CALL_NUM).unwrap(),
            gas: None,
            value: None,
            with_da_fees: false,
            timestamp: None,
        };
        let outcome = evaluation.run(&mut host, None, &EVMVersion::current_test_config());

        assert!(outcome.is_ok(), "evaluation should have succeeded");
        let outcome = outcome.unwrap();
        if let SimulationResult::Ok(SimulationResult::Ok(ExecutionResult {
            value,
            gas_used: _,
        })) = outcome
        {
            assert_eq!(Some(vec![0u8; 32]), value, "evaluation result should be 0");
        } else {
            panic!("evaluation should have reached outcome");
        }
    }

    #[ignore]
    #[test]
    fn parse_simulation() {
        let to = address_of_str("3535353535353535353535353535353535353535");
        let from = address_of_str("2424242424242424242424242424242424242424");
        let data = hex::decode("1616").unwrap();
        let expected = Evaluation {
            from,
            to,
            gas: Some(11111),
            gas_price: Some(22222),
            value: Some(U256::from(33333)),
            data,
            with_da_fees: false,
            timestamp: None,
        };

        let mut encoded =
            hex::decode("f84894242424242424242424242424242424242424242494353535353535353535353535353535353535353588672b00000000000088ce56000000000000883582000000000000821616").unwrap();
        let mut input = vec![
            parsing::SIMULATION_TAG,
            SIMULATION_SIMPLE_TAG,
            EVALUATION_TAG,
        ];
        input.append(&mut encoded);

        let parsed = Input::parse(&input);

        assert_eq!(
            Input::Simple(Box::new(Message::Evaluation(expected))),
            parsed,
            "should have been parsed as complete simulation"
        );
    }

    #[ignore]
    #[test]
    fn parse_simulation2() {
        // setup
        let mut host = MockKernelHost::default();
        let new_address = create_contract(&mut host);

        let to = Some(new_address);
        let data = hex::decode(STORAGE_CONTRACT_CALL_GET).unwrap();
        let gas = Some(11111);
        let expected = Evaluation {
            from: None,
            to,
            gas,
            gas_price: None,
            value: None,
            data,
            with_da_fees: false,
            timestamp: None,
        };

        let encoded = hex::decode(
            "ff0100e68094907823e0a92f94355968feb2cbf0fbb594fe321488672b0000000000008080846d4ce63c",
        )
        .unwrap();

        let parsed = Input::parse(&encoded);
        assert_eq!(
            Input::Simple(Box::new(Message::Evaluation(expected))),
            parsed,
            "should have been parsed as complete simulation"
        );
    }

    #[test]
    fn parse_num_chunks() {
        let num: u16 = 42;
        let mut input = vec![parsing::SIMULATION_TAG, SIMULATION_CREATE_TAG];
        input.extend(num.to_le_bytes());

        let parsed = Input::parse(&input);

        assert_eq!(
            Input::NewChunked(42),
            parsed,
            "should have parsed start of chunked simulation"
        );
    }

    #[test]
    fn parse_chunk() {
        let i: u16 = 20;
        let mut input = vec![parsing::SIMULATION_TAG, SIMULATION_CHUNK_TAG];
        input.extend(i.to_le_bytes());
        input.extend(hex::decode("aaaaaa").unwrap());

        let expected = Input::Chunk {
            i: 20,
            data: vec![170u8; 3],
        };

        let parsed = Input::parse(&input);

        assert_eq!(expected, parsed, "should have parsed a chunk");
    }

    pub fn check_roundtrip<R: Decodable + Encodable + core::fmt::Debug + PartialEq>(
        v: R,
    ) {
        let bytes = v.rlp_bytes();
        let decoder = Rlp::new(&bytes);
        println!("{bytes:?}");
        let decoded = R::decode(&decoder).expect("Value should be decodable");
        assert_eq!(v, decoded, "Roundtrip failed on {v:?}")
    }

    #[test]
    fn test_simulation_result_encoding_roundtrip() {
        let valid: SimulationResult<ValidationResult, String> =
            SimulationResult::Ok(ValidationResult {
                transaction_object: TransactionObject {
                    block_number: U256::from(532532),
                    from: address_of_str("3535353535353535353535353535353535353535")
                        .unwrap(),
                    gas_used: U256::from(32523),
                    gas_price: U256::from(100432432),
                    hash: [5; 32],
                    input: vec![],
                    nonce: 8888,
                    to: address_of_str("3635353535353535353535353535353535353536"),
                    index: 15u32,
                    value: U256::from(0),
                    signature: Some(
                        TxSignature::new(
                            U256::from(1337),
                            H256::from_low_u64_be(1),
                            H256::from_low_u64_be(2),
                        )
                        .unwrap(),
                    ),
                },
            });
        let call: SimulationResult<CallResult, String> =
            SimulationResult::Ok(SimulationResult::Ok(ExecutionResult {
                value: Some(vec![0, 1, 2, 3]),
                gas_used: Some(123),
            }));
        let revert: SimulationResult<CallResult, String> =
            SimulationResult::Ok(SimulationResult::Err(vec![3, 2, 1, 0]));
        let error: SimulationResult<CallResult, String> =
            SimulationResult::Err(String::from("Un festival de GADTs"));

        check_roundtrip(valid);
        check_roundtrip(call);
        check_roundtrip(revert);
        check_roundtrip(error)
    }
}
