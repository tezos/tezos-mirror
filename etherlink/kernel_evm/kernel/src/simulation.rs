// SPDX-FileCopyrightText: 2022-2024 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

// Module containing most Simulation related code, in one place, to be deleted
// when the proxy node simulates directly

use crate::{error::Error, error::StorageError, storage};

use crate::{
    current_timestamp, parsable, parsing, retrieve_block_fees, retrieve_chain_id,
    tick_model, CONFIG,
};

use evm_execution::run_transaction;
use evm_execution::{account_storage, handler::ExecutionOutcome, precompiles};
use primitive_types::{H160, U256};
use rlp::{Decodable, DecoderError, Rlp};
use tezos_ethereum::block::BlockConstants;
use tezos_ethereum::rlp_helpers::{decode_field, decode_option, next};
use tezos_ethereum::tx_common::EthereumTransactionCommon;
use tezos_evm_logging::{log, Level::*};
use tezos_smart_rollup_host::runtime::Runtime;

// SIMULATION/SIMPLE/RLP_ENCODED_SIMULATION
pub const SIMULATION_SIMPLE_TAG: u8 = 1;
// SIMULATION/CREATE/NUM_CHUNKS 2B
pub const SIMULATION_CREATE_TAG: u8 = 2;
// SIMULATION/CHUNK/NUM 2B/CHUNK
pub const SIMULATION_CHUNK_TAG: u8 = 3;
/// Tag indicating simulation is an evaluation.
pub const EVALUATION_TAG: u8 = 0x00;
/// Tag indicating simulation is a validation.
pub const VALIDATION_TAG: u8 = 0x01;

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
}

impl Evaluation {
    /// Unserialize bytes as RLP encoded data.
    pub fn from_rlp_bytes(bytes: &[u8]) -> Result<Evaluation, DecoderError> {
        let decoder = Rlp::new(bytes);
        Evaluation::decode(&decoder)
    }

    /// Execute the simulation
    pub fn run<Host: Runtime>(
        &self,
        host: &mut Host,
    ) -> Result<Option<ExecutionOutcome>, Error> {
        let chain_id = retrieve_chain_id(host)?;
        let block_fees = retrieve_block_fees(host)?;

        let current_constants = match storage::read_current_block(host) {
            Ok(block) => block.constants(chain_id, block_fees),
            Err(_) => {
                let timestamp = current_timestamp(host);
                let timestamp = U256::from(timestamp.as_u64());
                BlockConstants::first_block(timestamp, chain_id, block_fees)
            }
        };

        let mut evm_account_storage = account_storage::init_account_storage()
            .map_err(|_| Error::Storage(StorageError::AccountInitialisation))?;
        let precompiles = precompiles::precompile_set::<Host>();
        let default_caller = H160::zero();
        let tx_data_size = self.data.len() as u64;
        let allocated_ticks =
            tick_model::estimate_remaining_ticks_for_transaction_execution(
                0,
                tx_data_size,
            );

        let gas_price = if let Some(gas_price) = self.gas_price {
            U256::from(gas_price)
        } else {
            block_fees.base_fee_per_gas()
        };

        let outcome = run_transaction(
            host,
            &current_constants,
            &mut evm_account_storage,
            &precompiles,
            CONFIG,
            self.to,
            self.from.unwrap_or(default_caller),
            self.data.clone(),
            self.gas.or(Some(u64::MAX)),
            gas_price,
            self.value,
            false,
            allocated_ticks,
        )
        .map_err(Error::Simulation)?;
        Ok(outcome)
    }
}

impl Decodable for Evaluation {
    fn decode(decoder: &Rlp<'_>) -> Result<Self, DecoderError> {
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
                })
            } else {
                Err(DecoderError::RlpIncorrectListLen)
            }
        } else {
            Err(DecoderError::RlpExpectedToBeList)
        }
    }
}

impl TryFrom<&[u8]> for Evaluation {
    type Error = DecoderError;

    fn try_from(bytes: &[u8]) -> Result<Self, Self::Error> {
        Self::from_rlp_bytes(bytes)
    }
}

#[derive(Debug, PartialEq)]
struct TxValidation {
    transaction: EthereumTransactionCommon,
}

#[derive(Debug, PartialEq)]
enum TxValidationOutcome {
    Valid(H160),
    NonceTooLow,
    NotCorrectSignature,
    InvalidChainId,
    MaxGasFeeTooLow,
    OutOfTicks,
}

impl TxValidation {
    // Run the transaction and checks if it fails with "OutOfTicks". It might
    // fail for other reasons, but in that case it is still correct.
    pub fn would_exhaust_ticks<Host: Runtime>(
        host: &mut Host,
        transaction: &EthereumTransactionCommon,
        caller: &H160,
    ) -> Result<bool, anyhow::Error> {
        let chain_id = retrieve_chain_id(host)?;
        let block_fees = retrieve_block_fees(host)?;

        let current_constants = match storage::read_current_block(host) {
            Ok(block) => block.constants(chain_id, block_fees),
            Err(_) => {
                let timestamp = current_timestamp(host);
                let timestamp = U256::from(timestamp.as_u64());
                BlockConstants::first_block(timestamp, chain_id, block_fees)
            }
        };

        let mut evm_account_storage = account_storage::init_account_storage()
            .map_err(|_| Error::Storage(StorageError::AccountInitialisation))?;
        let precompiles = precompiles::precompile_set::<Host>();
        let tx_data_size = transaction.data.len() as u64;
        let allocated_ticks =
            tick_model::estimate_remaining_ticks_for_transaction_execution(
                0,
                tx_data_size,
            );

        let gas_price = if let Ok(gas_price) =
            transaction.effective_gas_price(block_fees.base_fee_per_gas())
        {
            gas_price
        } else {
            block_fees.base_fee_per_gas()
        };

        match run_transaction(
            host,
            &current_constants,
            &mut evm_account_storage,
            &precompiles,
            CONFIG,
            transaction.to,
            *caller,
            transaction.data.clone(),
            Some(transaction.execution_gas_limit()), // gas could be omitted
            gas_price,
            Some(transaction.value),
            false,
            allocated_ticks,
        ) {
            Err(evm_execution::EthereumError::OutOfTicks) => Ok(true),
            _ => Ok(false),
        }
    }

    /// Execute the simulation
    pub fn run<Host: Runtime>(
        &self,
        host: &mut Host,
    ) -> Result<TxValidationOutcome, anyhow::Error> {
        let tx = &self.transaction;
        let evm_account_storage = account_storage::init_account_storage()?;
        // Get the caller
        let Ok(caller) = tx.caller() else {return Ok(TxValidationOutcome::NotCorrectSignature)};
        // Get the caller account
        let caller_account_path = evm_execution::account_storage::account_path(&caller)?;
        let caller_account = evm_account_storage.get(host, &caller_account_path)?;
        // Get the nonce of the caller
        let caller_nonce = match &caller_account {
            Some(account) => account.nonce(host)?,
            None => U256::zero(),
        };
        let block_fees = retrieve_block_fees(host)?;
        // Get the chain_id
        let chain_id = storage::read_chain_id(host)?;
        // Check if nonce is too low
        if tx.nonce < caller_nonce {
            return Ok(TxValidationOutcome::NonceTooLow);
        }
        // Check if the chain id is correct
        if tx.chain_id.is_some() && tx.chain_id != Some(chain_id) {
            return Ok(TxValidationOutcome::InvalidChainId);
        }
        // Check if running the transaction (assuming it is valid) would run out
        // of ticks.
        if let Ok(true) = Self::would_exhaust_ticks(host, tx, &caller) {
            return Ok(TxValidationOutcome::OutOfTicks);
        }
        // Check if the gas price is high enough
        if tx.max_fee_per_gas < block_fees.base_fee_per_gas()
            || tx.max_fee_per_gas < tx.max_priority_fee_per_gas
        {
            return Ok(TxValidationOutcome::MaxGasFeeTooLow);
        }
        // TODO: #6498
        // check PRE_PAY condition
        Ok(TxValidationOutcome::Valid(caller))
    }
}

impl TryFrom<&[u8]> for TxValidation {
    type Error = DecoderError;

    fn try_from(bytes: &[u8]) -> Result<Self, Self::Error> {
        let transaction = EthereumTransactionCommon::from_bytes(bytes)?;
        Ok(Self { transaction })
    }
}

#[derive(Debug, PartialEq)]
enum Message {
    Evaluation(Evaluation),
    TxValidation(Box<TxValidation>),
}

impl TryFrom<&[u8]> for Message {
    type Error = DecoderError;

    fn try_from(bytes: &[u8]) -> Result<Self, Self::Error> {
        let Some(&tag) = bytes.first() else {return Err(DecoderError::Custom("Empty simulation message"))};
        let Some(bytes) = bytes.get(1..) else {return Err(DecoderError::Custom("Empty simulation message"))};

        match tag {
            EVALUATION_TAG => Evaluation::try_from(bytes).map(Message::Evaluation),
            VALIDATION_TAG => TxValidation::try_from(bytes)
                .map(|tx| Message::TxValidation(Box::new(tx))),
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

fn store_simulation_outcome<Host: Runtime>(
    host: &mut Host,
    outcome: Option<ExecutionOutcome>,
) -> Result<(), anyhow::Error> {
    log!(host, Debug, "outcome={:?} ", outcome);
    match outcome {
        Some(outcome) => {
            storage::store_simulation_status(host, outcome.is_success)?;
            storage::store_evaluation_gas(host, outcome.gas_used)?;
            storage::store_simulation_result(host, outcome.result)
        }
        None => Ok(()),
    }
}

fn store_tx_validation_outcome<Host: Runtime>(
    host: &mut Host,
    outcome: TxValidationOutcome,
) -> Result<(), anyhow::Error> {
    match outcome {
        TxValidationOutcome::Valid(caller) => {
            storage::store_simulation_status(host, true)?;
            storage::store_simulation_result(host, Some(caller.to_fixed_bytes().to_vec()))
        }
        TxValidationOutcome::NonceTooLow => {
            storage::store_simulation_status(host, false)?;
            storage::store_simulation_result(host, Some(b"Nonce too low.".to_vec()))
        }
        TxValidationOutcome::NotCorrectSignature => {
            storage::store_simulation_status(host, false)?;
            storage::store_simulation_result(host, Some(b"Incorrect signature.".to_vec()))
        }
        TxValidationOutcome::InvalidChainId => {
            storage::store_simulation_status(host, false)?;
            storage::store_simulation_result(host, Some(b"Invalid chain id.".to_vec()))
        }
        TxValidationOutcome::MaxGasFeeTooLow => {
            storage::store_simulation_status(host, false)?;
            storage::store_simulation_result(host, Some(b"Max gas fee too low.".to_vec()))
        }
        TxValidationOutcome::OutOfTicks => {
            storage::store_simulation_status(host, false)?;
            storage::store_simulation_result(
                host,
                Some(
                    b"The transaction would exhaust all the ticks it is allocated. \
                      Try reducing its gas consumption or splitting the call in \
                      multiple steps, if possible."
                        .to_vec(),
                ),
            )
        }
    }
}

pub fn start_simulation_mode<Host: Runtime>(
    host: &mut Host,
) -> Result<(), anyhow::Error> {
    log!(host, Debug, "Starting simulation mode ");
    let simulation = parse_inbox(host)?;
    match simulation {
        Message::Evaluation(simulation) => {
            let outcome = simulation.run(host)?;
            store_simulation_outcome(host, outcome)
        }
        Message::TxValidation(tx_validation) => {
            let outcome = tx_validation.run(host)?;
            store_tx_validation_outcome(host, outcome)
        }
    }
}

#[cfg(test)]
mod tests {

    use primitive_types::H256;
    use tezos_ethereum::{
        block::BlockConstants, transaction::TransactionType, tx_signature::TxSignature,
    };
    use tezos_smart_rollup_mock::MockHost;

    use crate::{current_timestamp, retrieve_block_fees, retrieve_chain_id};

    use super::*;

    impl Evaluation {
        /// Unserialize an hex string as RLP encoded data.
        pub fn from_rlp(e: String) -> Result<Evaluation, DecoderError> {
            let tx = hex::decode(e)
                .or(Err(DecoderError::Custom("Couldn't parse hex value")))?;
            Self::from_rlp_bytes(&tx)
        }
    }

    fn address_of_str(s: &str) -> Option<H160> {
        let data = &hex::decode(s).unwrap();
        Some(H160::from_slice(data))
    }

    #[test]
    fn test_decode_empty() {
        let input_string =
            "da8094353535353535353535353535353535353535353580808080".to_string();
        let address = address_of_str("3535353535353535353535353535353535353535");
        let expected = Evaluation {
            from: None,
            to: address,
            gas: None,
            gas_price: None,
            value: None,
            data: vec![],
        };

        let evaluation = Evaluation::from_rlp(input_string);

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
            "f84894242424242424242424242424242424242424242494353535353535353535353535353535353535353588672b00000000000088ce56000000000000883582000000000000821616".to_string();
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
        };

        let evaluation = Evaluation::from_rlp(input_string);

        assert!(evaluation.is_ok(), "Simulation input should be decodable");
        assert_eq!(
            expected,
            evaluation.unwrap(),
            "The decoded result is not the one expected"
        );
    }

    // The compiled initialization code for the Ethereum demo contract given
    // as an example in kernel_evm/solidity_examples/storage.sol
    const STORAGE_CONTRACT_INITIALIZATION: &str = "608060405234801561001057600080fd5b5061017f806100206000396000f3fe608060405234801561001057600080fd5b50600436106100415760003560e01c80634e70b1dc1461004657806360fe47b1146100645780636d4ce63c14610080575b600080fd5b61004e61009e565b60405161005b91906100d0565b60405180910390f35b61007e6004803603810190610079919061011c565b6100a4565b005b6100886100ae565b60405161009591906100d0565b60405180910390f35b60005481565b8060008190555050565b60008054905090565b6000819050919050565b6100ca816100b7565b82525050565b60006020820190506100e560008301846100c1565b92915050565b600080fd5b6100f9816100b7565b811461010457600080fd5b50565b600081359050610116816100f0565b92915050565b600060208284031215610132576101316100eb565b5b600061014084828501610107565b9150509291505056fea2646970667358221220ec57e49a647342208a1f5c9b1f2049bf1a27f02e19940819f38929bf67670a5964736f6c63430008120033";
    // call: num (direct access to state variable)
    const STORAGE_CONTRACT_CALL_NUM: &str = "4e70b1dc";
    // call: get (public view)
    const STORAGE_CONTRACT_CALL_GET: &str = "6d4ce63c";

    const DUMMY_ALLOCATED_TICKS: u64 = 1_000_000_000;

    fn create_contract<Host>(host: &mut Host) -> H160
    where
        Host: Runtime,
    {
        let timestamp = current_timestamp(host);
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
        );
        let precompiles = precompiles::precompile_set::<Host>();
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
            &precompiles,
            CONFIG,
            callee,
            caller,
            call_data,
            Some(gas_limit),
            gas_price,
            Some(transaction_value),
            false,
            DUMMY_ALLOCATED_TICKS,
        );
        assert!(outcome.is_ok(), "contract should have been created");
        let outcome = outcome.unwrap();
        assert!(
            outcome.is_some(),
            "execution should have produced some outcome"
        );
        outcome.unwrap().new_address.unwrap()
    }

    #[test]
    fn simulation_result() {
        // setup
        let mut host = MockHost::default();
        let new_address = create_contract(&mut host);

        // run evaluation num
        let evaluation = Evaluation {
            from: None,
            gas_price: None,
            to: Some(new_address),
            data: hex::decode(STORAGE_CONTRACT_CALL_NUM).unwrap(),
            gas: Some(100000),
            value: None,
        };
        let outcome = evaluation.run(&mut host);

        assert!(outcome.is_ok(), "evaluation should have succeeded");
        let outcome = outcome.unwrap();
        assert!(
            outcome.is_some(),
            "simulation should have produced some outcome"
        );
        let outcome = outcome.unwrap();
        assert_eq!(
            Some(vec![0u8; 32]),
            outcome.result,
            "simulation result should be 0"
        );

        // run simulation get
        let evaluation = Evaluation {
            from: None,
            gas_price: None,
            to: Some(new_address),
            data: hex::decode(STORAGE_CONTRACT_CALL_GET).unwrap(),
            gas: Some(111111),
            value: None,
        };
        let outcome = evaluation.run(&mut host);

        assert!(outcome.is_ok(), "simulation should have succeeded");
        let outcome = outcome.unwrap();
        assert!(
            outcome.is_some(),
            "simulation should have produced some outcome"
        );
        let outcome = outcome.unwrap();
        assert_eq!(
            Some(vec![0u8; 32]),
            outcome.result,
            "evaluation result should be 0"
        );
    }

    #[test]
    fn evaluation_result_no_gas() {
        // setup
        let mut host = MockHost::default();
        let new_address = create_contract(&mut host);

        // run evaluation num
        let evaluation = Evaluation {
            from: None,
            gas_price: None,
            to: Some(new_address),
            data: hex::decode(STORAGE_CONTRACT_CALL_NUM).unwrap(),
            gas: None,
            value: None,
        };
        let outcome = evaluation.run(&mut host);

        assert!(outcome.is_ok(), "evaluation should have succeeded");
        let outcome = outcome.unwrap();
        assert!(
            outcome.is_some(),
            "simulation should have produced some outcome"
        );
        let outcome = outcome.unwrap();
        assert_eq!(
            Some(vec![0u8; 32]),
            outcome.result,
            "evaluation result should be 0"
        );
    }

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

    #[test]
    fn parse_simulation2() {
        // setup
        let mut host = MockHost::default();
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

    #[test]
    fn parse_tx_validation() {
        let expected = {
            let v = 2710.into();

            let r = H256::from_slice(
                &hex::decode(
                    "0c4604516693aafd2e74a993c280455fcad144a414f5aa580d96f3c51d4428e5",
                )
                .unwrap(),
            );

            let s = H256::from_slice(
                &hex::decode(
                    "630fb7fc1af4c1c1a82cabb4ef9d12f8fc2e54a047eb3e3bdffc9d23cd07a94e",
                )
                .unwrap(),
            );

            let signature = TxSignature::new(v, r, s).unwrap();

            EthereumTransactionCommon::new(
                TransactionType::Legacy,
                Some(1337.into()),
                0.into(),
                U256::default(),
                U256::default(),
                2000000,
                Some(H160::default()),
                U256::default(),
                vec![],
                Vec::default(),
                Some(signature),
            )
        };

        let hex = "f8628080831e84809400000000000000000000000000000000000000008080820a96a00c4604516693aafd2e74a993c280455fcad144a414f5aa580d96f3c51d4428e5a0630fb7fc1af4c1c1a82cabb4ef9d12f8fc2e54a047eb3e3bdffc9d23cd07a94e";
        let data = hex::decode(hex).unwrap();
        let tx = EthereumTransactionCommon::from_bytes(&data).unwrap();

        assert_eq!(tx, expected);

        let mut encoded = hex::decode(hex).unwrap();
        let mut input = vec![
            parsing::SIMULATION_TAG,
            SIMULATION_SIMPLE_TAG,
            VALIDATION_TAG,
        ];
        input.append(&mut encoded);

        let parsed = Input::parse(&input);

        assert_eq!(
            Input::Simple(Box::new(Message::TxValidation(Box::new(TxValidation {
                transaction: expected
            })))),
            parsed,
            "should have been parsed as complete tx validation"
        );
    }

    fn address_from_str(s: &str) -> Option<H160> {
        let data = &hex::decode(s).unwrap();
        Some(H160::from_slice(data))
    }

    #[test]
    fn test_tx_validation_gas_price() {
        let mut host = MockHost::default();

        let transaction = EthereumTransactionCommon::new(
            TransactionType::Eip1559,
            Some(U256::from(1)),
            U256::from(0),
            U256::zero(),
            U256::from(1),
            21000,
            Some(H160::zero()),
            U256::zero(),
            vec![],
            vec![],
            None,
        );
        let signed = transaction
            .sign_transaction(
                "e922354a3e5902b5ac474f3ff08a79cff43533826b8f451ae2190b65a9d26158"
                    .to_string(),
            )
            .unwrap();
        let simulation = TxValidation {
            transaction: signed,
        };
        storage::store_chain_id(&mut host, U256::from(1))
            .expect("should be able to store a chain id");
        let evm_account_storage = account_storage::init_account_storage().unwrap();
        let _account = evm_account_storage
            .get_or_create(
                &host,
                &account_storage::account_path(
                    &address_from_str("f95abdf6ede4c3703e0e9453771fbee8592d31e9")
                        .unwrap(),
                )
                .unwrap(),
            )
            .unwrap();
        let result = simulation.run(&mut host);
        assert!(result.is_ok());
        assert_eq!(TxValidationOutcome::MaxGasFeeTooLow, result.unwrap());
    }
}
