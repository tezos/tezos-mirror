// SPDX-FileCopyrightText: 2022-2023 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

// Module containing most Simulation related code, in one place, to be deleted
// when the proxy node simulates directly

use crate::{error::Error, error::StorageError, storage};

use crate::{parsable, parsing, CONFIG};

use evm_execution::{account_storage, handler::ExecutionOutcome, precompiles};
use primitive_types::{H160, U256};
use rlp::{Decodable, DecoderError, Rlp};
use tezos_ethereum::rlp_helpers::{decode_field, decode_option, next};
use tezos_smart_rollup_debug::{debug_msg, Runtime};

// SIMULATION/SIMPLE/RLP_ENCODED_SIMULATION
pub const SIMULATION_SIMPLE_TAG: u8 = 1;
// SIMULATION/CREATE/NUM_CHUNKS 2B
pub const SIMULATION_CREATE_TAG: u8 = 2;
// SIMULATION/CHUNK/NUM 2B/CHUNK
pub const SIMULATION_CHUNK_TAG: u8 = 3;
/// Maximum gas used by the simulation. Bounded to limit DOS on the rollup node
/// Is used as default value if no gas is set.
pub const MAX_SIMULATION_GAS: u64 = 1_000_000_000u64;

/// Container for eth_call data, used in messages sent by the rollup node
/// simulation.
///
/// They are transmitted in RLP encoded form, in messages of the form\
/// `\parsing::SIMULATION_TAG \SIMULATION_SIMPLE_TAG \<rlp encoded Simulation>`\
/// or in chunks if they are bigger than what the inbox can receive, with a
/// first message giving the number of chunks\
/// `\parsing::SIMULATION_TAG \SIMULATION_CREATE_TAG \XXXX`
/// where `XXXX` is 2 bytes containing the number of chunks, followed by the
/// chunks:\
/// `\parsing::SIMULATION_TAG \SIMULATION_CHUNK_TAG \XXXX \<bytes>`\
/// where `XXXX` is the number of the chunk over 2 bytes, and the rest is a
/// chunk of the rlp encoded simulation.
///
/// Ethereum doc: https://ethereum.org/en/developers/docs/apis/json-rpc/#eth_call
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Simulation {
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

impl Simulation {
    /// Unserialize bytes as RLP encoded data.
    pub fn from_rlp_bytes(bytes: &[u8]) -> Result<Simulation, DecoderError> {
        let decoder = Rlp::new(bytes);
        Simulation::decode(&decoder)
    }

    /// Execute the simulation
    pub fn run<Host: Runtime>(&self, host: &mut Host) -> Result<ExecutionOutcome, Error> {
        let current_block = storage::read_current_block(host)?;
        let mut evm_account_storage = account_storage::init_account_storage()
            .map_err(|_| Error::Storage(StorageError::AccountInitialisation))?;
        let precompiles = precompiles::precompile_set::<Host>();
        let default_caller = H160::zero();
        let outcome = evm_execution::run_transaction(
            host,
            &current_block.constants(),
            &mut evm_account_storage,
            &precompiles,
            CONFIG,
            self.to,
            self.from.unwrap_or(default_caller),
            self.data.clone(),
            self.gas
                .map(|gas| u64::max(gas, MAX_SIMULATION_GAS))
                .or(Some(MAX_SIMULATION_GAS)), // gas could be omitted
            self.value,
        )
        .map_err(Error::Simulation)?;
        Ok(outcome)
    }
}

impl Decodable for Simulation {
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

impl TryFrom<&[u8]> for Simulation {
    type Error = DecoderError;

    fn try_from(bytes: &[u8]) -> Result<Self, Self::Error> {
        Self::from_rlp_bytes(bytes)
    }
}

#[derive(Default, Debug, PartialEq)]
enum Input {
    #[default]
    Unparsable,
    SimpleSimulation(Simulation),
    NewChunkedSimulation(u16),
    SimulationChunk {
        i: u16,
        data: Vec<u8>,
    },
}

impl Input {
    fn parse_new_chunk_simulation(bytes: &[u8]) -> Self {
        let num_chunks = u16::from_le_bytes(parsable!(bytes.try_into().ok()));
        Self::NewChunkedSimulation(num_chunks)
    }

    fn parse_simulation_chunk(bytes: &[u8]) -> Self {
        let (num, remaining) = parsable!(parsing::split_at(bytes, 2));
        let i = u16::from_le_bytes(num.try_into().unwrap());
        Self::SimulationChunk {
            i,
            data: remaining.to_vec(),
        }
    }
    fn parse_simple_simulation(bytes: &[u8]) -> Self {
        Input::SimpleSimulation(parsable!(bytes.try_into().ok()))
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
) -> Result<Simulation, Error> {
    let mut buffer: Vec<u8> = Vec::new();
    for n in 0..num_chunks {
        match read_input(host)? {
            Input::SimulationChunk { i, data } => {
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

fn parse_inbox<Host: Runtime>(host: &mut Host) -> Result<Simulation, Error> {
    // we just received simulation tag
    // next message is either a simulation or the nb of chunks needed
    match read_input(host)? {
        Input::SimpleSimulation(s) => Ok(s),
        Input::NewChunkedSimulation(num_chunks) => {
            // loop to find the chunks
            read_chunks(host, num_chunks)
        }
        _ => Err(Error::InvalidConversion),
    }
}

pub fn start_simulation_mode<Host: Runtime>(host: &mut Host) -> Result<(), Error> {
    debug_msg!(host, "Starting simulation mode ");
    let simulation = parse_inbox(host)?;
    let outcome = simulation.run(host)?;
    debug_msg!(host, "outcome={:?} ", outcome);
    storage::store_simulation_status(host, outcome.is_success)?;
    storage::store_simulation_result(host, outcome.result)
}

#[cfg(test)]
mod tests {

    use tezos_ethereum::block::BlockConstants;
    use tezos_smart_rollup_mock::MockHost;

    use crate::genesis::init_block;

    use super::*;

    impl Simulation {
        /// Unserialize an hex string as RLP encoded data.
        pub fn from_rlp(e: String) -> Result<Simulation, DecoderError> {
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
        let expected = Simulation {
            from: None,
            to: address,
            gas: None,
            gas_price: None,
            value: None,
            data: vec![],
        };

        let simulation = Simulation::from_rlp(input_string);

        assert!(simulation.is_ok(), "Simulation input should be decodable");
        assert_eq!(
            expected,
            simulation.unwrap(),
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
        let expected = Simulation {
            from,
            to,
            gas: Some(11111),
            gas_price: Some(22222),
            value: Some(U256::from(33333)),
            data,
        };

        let simulation = Simulation::from_rlp(input_string);

        assert!(simulation.is_ok(), "Simulation input should be decodable");
        assert_eq!(
            expected,
            simulation.unwrap(),
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

    fn create_contract<Host>(host: &mut Host) -> H160
    where
        Host: Runtime,
    {
        // setup
        assert!(init_block(host).is_ok());

        let block = BlockConstants::first_block();
        let precompiles = precompiles::precompile_set::<Host>();
        let mut evm_account_storage = account_storage::init_account_storage().unwrap();

        let callee = None;
        let caller = H160::from_low_u64_be(117);
        let transaction_value = U256::from(0);
        let call_data: Vec<u8> = hex::decode(STORAGE_CONTRACT_INITIALIZATION).unwrap();

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
            Some(10000),
            Some(transaction_value),
        );
        assert!(outcome.is_ok(), "contract should have been created");
        outcome.unwrap().new_address.unwrap()
    }

    #[test]
    fn simulation_result() {
        // setup
        let mut host = MockHost::default();
        let new_address = create_contract(&mut host);

        // run simulation num
        let simulation = Simulation {
            from: None,
            gas_price: None,
            to: Some(new_address),
            data: hex::decode(STORAGE_CONTRACT_CALL_NUM).unwrap(),
            gas: Some(10000),
            value: None,
        };
        let outcome = simulation.run(&mut host);

        assert!(outcome.is_ok(), "simulation should have succeeded");
        let outcome = outcome.unwrap();
        assert_eq!(
            Some(vec![0u8; 32]),
            outcome.result,
            "simulation result should be 0"
        );

        // run simulation get
        let simulation = Simulation {
            from: None,
            gas_price: None,
            to: Some(new_address),
            data: hex::decode(STORAGE_CONTRACT_CALL_GET).unwrap(),
            gas: Some(10000),
            value: None,
        };
        let outcome = simulation.run(&mut host);

        assert!(outcome.is_ok(), "simulation should have succeeded");
        let outcome = outcome.unwrap();
        assert_eq!(
            Some(vec![0u8; 32]),
            outcome.result,
            "simulation result should be 0"
        );
    }

    #[test]
    fn simulation_result_no_gas() {
        // setup
        let mut host = MockHost::default();
        let new_address = create_contract(&mut host);

        // run simulation num
        let simulation = Simulation {
            from: None,
            gas_price: None,
            to: Some(new_address),
            data: hex::decode(STORAGE_CONTRACT_CALL_NUM).unwrap(),
            gas: None,
            value: None,
        };
        let outcome = simulation.run(&mut host);

        assert!(outcome.is_ok(), "simulation should have succeeded");
        let outcome = outcome.unwrap();
        assert_eq!(
            Some(vec![0u8; 32]),
            outcome.result,
            "simulation result should be 0"
        );
    }

    #[test]
    fn parse_simulation() {
        let to = address_of_str("3535353535353535353535353535353535353535");
        let from = address_of_str("2424242424242424242424242424242424242424");
        let data = hex::decode("1616").unwrap();
        let expected = Simulation {
            from,
            to,
            gas: Some(11111),
            gas_price: Some(22222),
            value: Some(U256::from(33333)),
            data,
        };

        let mut encoded =
            hex::decode("f84894242424242424242424242424242424242424242494353535353535353535353535353535353535353588672b00000000000088ce56000000000000883582000000000000821616").unwrap();
        let mut input = vec![parsing::SIMULATION_TAG, SIMULATION_SIMPLE_TAG];
        input.append(&mut encoded);

        let parsed = Input::parse(&input);

        assert_eq!(
            Input::SimpleSimulation(expected),
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
        let expected = Simulation {
            from: None,
            to,
            gas,
            gas_price: None,
            value: None,
            data,
        };

        let encoded = hex::decode(
            "ff01e68094907823e0a92f94355968feb2cbf0fbb594fe321488672b0000000000008080846d4ce63c",
        )
        .unwrap();

        let parsed = Input::parse(&encoded);
        assert_eq!(
            Input::SimpleSimulation(expected),
            parsed,
            "should have been parsed as complete simulation"
        );

        if let Input::SimpleSimulation(s) = parsed {
            let res = s.run(&mut host).expect("simulation should run");
            assert!(res.is_success, "simulation should have succeeded");
        } else {
            panic!("Parsing failed")
        }
    }

    #[test]
    fn parse_num_chunks() {
        let num: u16 = 42;
        let mut input = vec![parsing::SIMULATION_TAG, SIMULATION_CREATE_TAG];
        input.extend(num.to_le_bytes());

        let parsed = Input::parse(&input);

        assert_eq!(
            Input::NewChunkedSimulation(42),
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

        let expected = Input::SimulationChunk {
            i: 20,
            data: vec![170u8; 3],
        };

        let parsed = Input::parse(&input);

        assert_eq!(expected, parsed, "should have parsed a chunk");
    }
}
