// SPDX-FileCopyrightText: 2022-2024 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023-2025 Functori <contact@functori.com>
// SPDX-FileCopyrightText: 2023-2024 PK Lab <contact@pklab.io>
//
// SPDX-License-Identifier: MIT

//! Handle details of EVM runtime
//!
//! The interface between SputnikVM and the kernel. This includes interface
//! to storage, account balances, block constants, _and transaction state_.

use crate::access_record::AccessRecord;
use crate::account_storage::{
    account_path, AccountStorageError, EthereumAccount, EthereumAccountStorage,
    StorageValue,
};
use crate::precompiles::reentrancy_guard::ReentrancyGuard;
use crate::precompiles::{FA_BRIDGE_PRECOMPILE_ADDRESS, WITHDRAWAL_ADDRESS};
use crate::storage::blocks::{get_block_hash, BLOCKS_STORED};
use crate::storage::tracer;
use crate::tick_model_opcodes;
use crate::trace::{
    CallTrace, CallTracerConfig, CallTracerInput, StorageMapItem, StructLog,
    StructLoggerInput, TracerInput,
};
use crate::transaction::TransactionContext;
use crate::transaction_layer_data::{CallContext, TransactionLayerData};
use crate::utilities::create_address_legacy;
use crate::EthereumError;
use crate::PrecompileSet;
use crate::TracerInput::{CallTracer, StructLogger};
use alloc::borrow::Cow;
use alloc::rc::Rc;
use core::convert::Infallible;
use evm::executor::stack::Log;
use evm::gasometer::{GasCost, Gasometer, MemoryCost};
use evm::{
    CallScheme, Capture, Config, Context, CreateScheme, ExitError, ExitFatal, ExitReason,
    ExitRevert, ExitSucceed, Handler, Opcode, Resolve, Stack, Transfer,
};
use primitive_types::{H160, H256, U256};
use sha3::{Digest, Keccak256};
use std::cmp::min;
use std::collections::{BTreeSet, HashMap};
use std::fmt::Debug;
use tezos_data_encoding::enc::{BinResult, BinWriter};
use tezos_ethereum::block::BlockConstants;
use tezos_evm_logging::{log, Level::*};
use tezos_evm_runtime::runtime::Runtime;
use tezos_smart_rollup_encoding::michelson::ticket::FA2_1Ticket;
use tezos_smart_rollup_encoding::michelson::{
    MichelsonBytes, MichelsonContract, MichelsonNat, MichelsonPair, MichelsonTimestamp,
};
use tezos_smart_rollup_encoding::outbox::OutboxMessage;
use tezos_smart_rollup_storage::StorageError;
use tezos_storage::helpers::bytes_hash;

/// Withdrawal interface of the ticketer contract
pub type RouterInterface = MichelsonPair<MichelsonContract, FA2_1Ticket>;

/// Interface of the default entrypoint of the fast withdrawal contract.
///
/// The parameters corresponds to (from left to right w.r.t. `MichelsonPair`):
/// * withdrawal_id
/// * ticket
/// * timestamp
/// * withdrawer's address
/// * generic payload
/// * l2 caller's address
pub type FastWithdrawalInterface = MichelsonPair<
    MichelsonNat,
    MichelsonPair<
        FA2_1Ticket,
        MichelsonPair<
            MichelsonTimestamp,
            MichelsonPair<
                MichelsonContract,
                MichelsonPair<MichelsonBytes, MichelsonBytes>,
            >,
        >,
    >,
>;

/// Outbox messages that implements the different withdrawal interfaces,
/// ready to be encoded and posted.
#[derive(Debug, PartialEq, Eq)]
pub enum Withdrawal {
    Standard(OutboxMessage<RouterInterface>),
    Fast(OutboxMessage<FastWithdrawalInterface>),
}

impl BinWriter for Withdrawal {
    fn bin_write(&self, output: &mut Vec<u8>) -> BinResult {
        match self {
            Withdrawal::Standard(outbox_message_full) => {
                outbox_message_full.bin_write(output)
            }
            Withdrawal::Fast(outbox_message_full) => {
                outbox_message_full.bin_write(output)
            }
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum ExecutionResult {
    TransferSucceeded,
    ContractDeployed(H160, Vec<u8>),
    CallSucceeded(ExitSucceed, Vec<u8>),
    CallReverted(Vec<u8>),
    Error(ExitError),
    FatalError(ExitFatal),
    OutOfTicks,
}

impl ExecutionResult {
    pub const fn is_success(&self) -> bool {
        matches!(
            self,
            ExecutionResult::TransferSucceeded
                | ExecutionResult::ContractDeployed(_, _)
                | ExecutionResult::CallSucceeded(_, _)
        )
    }
    pub fn output(&self) -> Option<&[u8]> {
        match self {
            ExecutionResult::ContractDeployed(_, output)
            | ExecutionResult::CallSucceeded(_, output)
            | ExecutionResult::CallReverted(output) => Some(output.as_slice()),
            _ => None,
        }
    }
    pub const fn new_address(&self) -> Option<H160> {
        match self {
            ExecutionResult::ContractDeployed(new_address, _) => Some(*new_address),
            _ => None,
        }
    }
}

/// Outcome of making the [EvmHandler] run an Ethereum transaction
///
/// Be it contract -call, -create or simple transfer, the handler will update the world
/// state in durable storage _and_ produce a summary of the outcome that will be needed
/// for creating a transaction receipt.
#[derive(Debug, Eq, PartialEq)]
pub struct ExecutionOutcome {
    /// How much gas was used for processing an entire transaction.
    pub gas_used: u64,
    /// Logs generated by the transaction.
    pub logs: Vec<Log>,
    /// Result of the execution
    pub result: ExecutionResult,
    /// Withdrawals generated by the transaction. This field will be empty if the
    /// transaction fails (or if the transaction doesn't produce any withdrawals).
    pub withdrawals: Vec<Withdrawal>,
    /// Number of estimated ticks used at the end of the contract call
    pub estimated_ticks_used: u64,
}

impl ExecutionOutcome {
    pub const fn is_success(&self) -> bool {
        self.result.is_success()
    }
    pub fn output(&self) -> Option<&[u8]> {
        self.result.output()
    }
    pub const fn new_address(&self) -> Option<H160> {
        self.result.new_address()
    }
}

/// The result of calling a contract as expected by the SputnikVM EVM implementation.
/// First part of the tuple tells Sputnik how the execution went (success or failure
/// and in what way). Second part tells Sputnik the return data if any.
type CallOutcome = (ExitReason, Vec<u8>);

// Will be used to check precondition before executing a call or a create
pub enum Precondition {
    PassPrecondition,
    PreconditionErr(ExitReason),
    EthereumErr(EthereumError),
}

/// The result of creating a contract as expected by the SputnikVM EVM implementation.
/// First part of the triple is the execution outcome - same as for normal contract
/// execution. Second part is the address of the newly created contract, if one was
/// created. Last part is the return value, which is required by Sputnik, but it is
/// always an empty vector when this type is used for create outcome.
///
/// Beware that this type is sometimes used as outcome of a _call_. This is simply to
/// be able to use the `end_xxx_transaction` functions for both contract -create and
/// -call. In this case, the last element of the triple can be non-empty, and the
/// address will be `None`.
pub(crate) type CreateOutcome = (ExitReason, Option<H160>, Vec<u8>);

/// Wrap ethereum errors in the SputnikVM errors
///
/// This function wraps critical errors that indicate something is wrong
/// with the kernel or rollup node into errors that can be passed on to
/// SputnikVM execution. This is needed if an error occurs in a callback
/// called by SputnikVM.
fn ethereum_error_to_exit_reason(exit_reason: &EthereumError) -> ExitReason {
    match exit_reason {
        EthereumError::EthereumAccountError(AccountStorageError::NonceOverflow) => {
            ExitReason::Error(ExitError::MaxNonce)
        }
        _ => ExitReason::Fatal(ExitFatal::Other(Cow::from(format!("{:?}", exit_reason)))),
    }
}

fn ethereum_error_to_execution_result(error: &EthereumError) -> ExecutionResult {
    match error {
        EthereumError::EthereumAccountError(AccountStorageError::NonceOverflow) => {
            ExecutionResult::Error(ExitError::MaxNonce)
        }
        _ => ExecutionResult::FatalError(ExitFatal::Other(Cow::from(format!(
            "{:?}",
            error
        )))),
    }
}

pub enum TransferExitReason {
    Returned,
    OutOfFund,
}

#[cfg(feature = "benchmark-opcodes")]
mod benchmarks {

    use super::*;

    /// These values encodes the result of an evaluation step of the virtual
    /// machine. They can be used to filter some data that can be seen as non
    /// conclusive or irrelevant for the ticks model, or simply for data
    /// analysis.
    const STEP_CONTINUE: u8 = 0;
    const SUCCEED_STOP: u8 = 1;
    const SUCCEED_RETURN: u8 = 2;
    const SUCCEED_SUICIDE: u8 = 3;
    const EXIT_ERROR: u8 = 4;
    const EXIT_REVERT: u8 = 5;
    const EXIT_FATAL: u8 = 6;
    const TRAP: u8 = 7;

    #[inline(always)]
    fn step_exit_reason<T>(capture: &Result<(), Capture<ExitReason, T>>) -> u8 {
        match capture {
            Ok(()) => STEP_CONTINUE,
            Err(Capture::Exit(ExitReason::Succeed(ExitSucceed::Stopped))) => SUCCEED_STOP,
            Err(Capture::Exit(ExitReason::Succeed(ExitSucceed::Returned))) => {
                SUCCEED_RETURN
            }
            Err(Capture::Exit(ExitReason::Succeed(ExitSucceed::Suicided))) => {
                SUCCEED_SUICIDE
            }
            Err(Capture::Exit(ExitReason::Error(_))) => EXIT_ERROR,
            Err(Capture::Exit(ExitReason::Revert(_))) => EXIT_REVERT,
            Err(Capture::Exit(ExitReason::Fatal(_))) => EXIT_FATAL,
            Err(Capture::Trap(_)) => TRAP,
        }
    }

    // About the two `static mut` below and their usage
    //
    // Low key optimisation to avoid the formatting: we know that the data are
    // always 1 byte for the opcode, 8 bytes pour the gas (u64), 1 byte for the
    // step exit reason. The messages are preallocated and updated with the
    // correct values each time they are called. It avoid using the formatting.
    // The overhead of formatting is significative.

    // The start section for the opcodes expects a single byte which is the
    // current opcode.
    static mut START_OPCODE_SECTION_MSG: [u8; 35] =
        *b"__wasm_debugger__::start_section(\0)";

    // The start section for the precompiles expects the address of the
    // contract (20 bytes) and the size of the data (4 bytes).
    static mut START_PRECOMPILE_SECTION_MSG: [u8; 58] =
        *b"__wasm_debugger__::start_section(\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0)";

    #[inline(always)]
    pub fn start_opcode_section<Host: Runtime>(host: &mut Host, opcode: &Opcode) {
        unsafe {
            START_OPCODE_SECTION_MSG[33] = opcode.as_u8();
            host.write_debug(core::str::from_utf8_unchecked(&START_OPCODE_SECTION_MSG));
        }
    }

    #[inline(always)]
    pub fn start_precompile_section<Host: Runtime>(
        host: &mut Host,
        address: H160,
        input: &Vec<u8>,
    ) {
        unsafe {
            START_PRECOMPILE_SECTION_MSG[33..53].copy_from_slice(address.as_bytes());
            START_PRECOMPILE_SECTION_MSG[53..57]
                .copy_from_slice(&input.len().to_be_bytes());
            host.write_debug(core::str::from_utf8_unchecked(
                &START_PRECOMPILE_SECTION_MSG,
            ));
        }
    }

    // The value of the ending sections are:
    // - 8 bytes for the gas, in little endian
    // - 1 byte that describes the continuation of the evaluation: it either
    //   continues to the next opcode (`STEP_CONTINUE`) or stops for a given
    //   reason, this reason being encoded in a byte. These values are described
    //   at the beginning of the `benchmarks` module.
    static mut END_OPCODE_SECTION_MSG: [u8; 41] =
        *b"__wasm_debugger__::end_section(\0\0\0\0\0\0\0\0\0)";

    static mut END_PRECOMPILE_SECTION_MSG: [u8; 32] =
        *b"__wasm_debugger__::end_section()";

    #[inline(always)]
    pub fn end_opcode_section<Host: Runtime, T>(
        host: &mut Host,
        gas: u64,
        step_result: &Result<(), Capture<ExitReason, T>>,
    ) {
        unsafe {
            END_OPCODE_SECTION_MSG[31..39].copy_from_slice(&gas.to_le_bytes());
            END_OPCODE_SECTION_MSG[39] = step_exit_reason(step_result);
            host.write_debug(core::str::from_utf8_unchecked(&END_OPCODE_SECTION_MSG));
        }
    }

    #[inline(always)]
    pub fn end_precompile_section<Host: Runtime>(host: &mut Host) {
        unsafe {
            host.write_debug(core::str::from_utf8_unchecked(&END_PRECOMPILE_SECTION_MSG));
        }
    }
}

#[derive(Eq, Hash, PartialEq, Clone, Copy, Debug)]
pub struct StorageKey {
    pub address: H160,
    pub index: H256,
}

#[derive(Clone, Copy)]
pub enum CacheStorageValue {
    Read(StorageValue),
    Write(H256),
}

impl CacheStorageValue {
    pub fn h256(&self) -> H256 {
        match self {
            CacheStorageValue::Read(storage_value) => storage_value.h256(),
            CacheStorageValue::Write(storage_value) => *storage_value,
        }
    }
}

/// The layer cache is associating a storage slot which is an
/// address and an index (StorageKey) to a value (CacheStorageValue).
pub type LayerCache = HashMap<StorageKey, CacheStorageValue>;

/// A transient layer associates at each address and index (StorageKey)
/// its value (H256).
pub type TransientLayer = HashMap<StorageKey, H256>;

/// The storage cache is associating at each layer (usize) its
/// own cache (LayerCache). For each slot that is modified or
/// read during a call it will be added to the cache in its own
/// layer.
// NB: The cache is implicitly bounded in memory thanks to the
// gas limit, because at most we can do 300_000 different SSTORE
// which costs 100 gas (300_000 × 100 = 30M gas).
// In memory it means we take at most:
// 300_000 × 32B = 9_600_000B = 9.6MB
pub type StorageCache = HashMap<usize, LayerCache>;

/// The transient storage is a temporary data storage area within the
/// EVM. It is associating at each layer (usize) its own storage map.
/// For each slot that is modified it will be added to the associated
/// layer and map. If the value did not exist, by default we return the
/// default value as specified by EIP-1153.
pub type TransientStorage = HashMap<usize, TransientLayer>;

#[derive(Default)]
struct Contract {
    code: Vec<u8>,
    hash: H256,
}

/// The contract cache is used each time a contract was called
/// and is considered "hot". Each heated contract will cost 100 for
/// a CALL instead of 2_600 for a cold contract.
// NB: The cache is implicitely bounded in memory thanks to the
// gas limit. At most we can do 11_538 different cold CALLs which
// costs 2_600 gas. (11_538 × 2_600 = 30M gas).
// In memory it means we take at most:
// 11_538 × 24_576B = 283_557_888B = 284MB.
// 24_576 is MAX_CODE_SIZE as per EIP-170.
// See: https://eips.ethereum.org/EIPS/eip-170.
type ContractCache = HashMap<H160, Contract>;

/// The implementation of the SputnikVM [Handler] trait
pub struct EvmHandler<'a, Host: Runtime> {
    /// The host
    pub host: &'a mut Host,
    /// The ethereum accounts storage
    evm_account_storage: &'a mut EthereumAccountStorage,
    /// The original caller initiating the toplevel transaction
    origin: H160,
    /// The constants for the current block
    pub block: &'a BlockConstants,
    /// The precompiled functions
    precompiles: &'a dyn PrecompileSet<Host>,
    /// The configuration, eg, London or Frontier for execution
    config: &'a Config,
    /// The contexts associated with transaction(s) currently in
    /// progress
    transaction_data: Vec<TransactionLayerData<'a>>,
    /// Estimated ticks spent for the execution of the current transaction,
    /// according to the ticks per gas per opcode model
    pub estimated_ticks_used: u64,
    /// The effective gas price of the current transaction
    effective_gas_price: U256,
    /// Tracer configuration for debugging.
    tracer: Option<TracerInput>,
    /// Storage cache during a given execution.
    /// NB: See `StorageCache`'s documentation for more information.
    storage_cache: StorageCache,
    /// The original storage cache has its own cache because its unrelated
    /// to the VM, its used by an internal mechanism of Sputnik to quickly
    /// access storage slots before any transaction happens.
    /// See: `fn original_storage`.
    original_storage_cache: LayerCache,
    /// Code contract cache during a given execution.
    /// NB: See `ContractCache`'s documentation for more information.
    contract_cache: ContractCache,
    /// Transient storage as specified by EIP-1153.
    pub transient_storage: TransientStorage,
    /// All the freshly created contracts.
    /// It will help identify created contracts within the same transaction
    /// accross all the execution layers to help comply with EIP-6780.
    pub created_contracts: BTreeSet<H160>,
    /// Reentrancy guard prevents circular calls to impure precompiles
    reentrancy_guard: ReentrancyGuard,
}

impl<'a, Host: Runtime> EvmHandler<'a, Host> {
    /// Create a new handler to suit a new, initial EVM call context
    #[allow(clippy::too_many_arguments)]
    pub fn new(
        host: &'a mut Host,
        evm_account_storage: &'a mut EthereumAccountStorage,
        origin: H160,
        block: &'a BlockConstants,
        config: &'a Config,
        precompiles: &'a dyn PrecompileSet<Host>,
        effective_gas_price: U256,
        tracer: Option<TracerInput>,
    ) -> Self {
        Self {
            host,
            evm_account_storage,
            origin,
            block,
            config,
            precompiles,
            transaction_data: vec![],
            estimated_ticks_used: 0,
            effective_gas_price,
            tracer,
            storage_cache: HashMap::with_capacity(10),
            original_storage_cache: HashMap::with_capacity(10),
            transient_storage: HashMap::with_capacity(10),
            contract_cache: HashMap::with_capacity(10),
            created_contracts: BTreeSet::new(),
            reentrancy_guard: ReentrancyGuard::new(vec![
                WITHDRAWAL_ADDRESS,
                FA_BRIDGE_PRECOMPILE_ADDRESS,
            ]),
        }
    }

    /// Get the total amount of gas used for the duration of the current
    /// transaction.
    pub fn gas_used(&self) -> u64 {
        self.transaction_data
            .last()
            .map(|layer| {
                layer
                    .gasometer
                    .as_ref()
                    .map(|g| g.total_used_gas())
                    .unwrap_or_default()
            })
            .unwrap_or_default()
    }

    // Gas used at the end of the transaction as per EIP-3529.
    // See: https://eips.ethereum.org/EIPS/eip-3529.
    pub fn gas_used_after_refund(&self) -> u64 {
        let compute_gas = |g: &Gasometer| {
            let total_used_gas = g.total_used_gas();
            let max_refund_quotient = self.config.max_refund_quotient;
            let refunded_gas = g.refunded_gas() as u64;
            total_used_gas - min(total_used_gas / max_refund_quotient, refunded_gas)
        };

        self.transaction_data
            .last()
            .map(|layer| {
                layer
                    .gasometer
                    .as_ref()
                    .map(|g| compute_gas(g))
                    .unwrap_or_default()
            })
            .unwrap_or_default()
    }

    /// Get the amount of gas still available for the current transaction.
    pub fn gas_remaining(&self) -> u64 {
        self.transaction_data
            .last()
            .map(|layer| {
                layer
                    .gasometer
                    .as_ref()
                    .map(|g| g.gas())
                    .unwrap_or_default()
            })
            .unwrap_or_default()
    }

    /// Get the amount of gas refunded for the current transaction.
    pub fn gas_refunded(&self) -> i64 {
        self.transaction_data
            .last()
            .map(|layer| {
                layer
                    .gasometer
                    .as_ref()
                    .map(|g| g.refunded_gas())
                    .unwrap_or_default()
            })
            .unwrap_or_default()
    }

    /// Record the cost of a static-cost opcode
    pub fn record_cost(&mut self, cost: u64) -> Result<(), ExitError> {
        let Some(layer) = self.transaction_data.last_mut() else {
            return Err(ExitError::Other(Cow::from(
                "Recording cost, but there is no transaction in progress",
            )));
        };

        layer
            .gasometer
            .as_mut()
            .map(|gasometer| gasometer.record_cost(cost))
            .unwrap_or(Ok(()))
    }

    /// Record code deposit. Pay per byte for a CREATE operation
    pub fn record_deposit(&mut self, len: usize) -> Result<(), ExitError> {
        let Some(layer) = self.transaction_data.last_mut() else {
            return Err(ExitError::Other(Cow::from(
                "Recording cost, but there is no transaction in progress",
            )));
        };

        layer
            .gasometer
            .as_mut()
            .map(|gasometer| gasometer.record_deposit(len))
            .unwrap_or(Ok(()))
    }

    /// Record the cost of a dynamic-cost opcode
    fn record_dynamic_cost(
        &mut self,
        cost: GasCost,
        memory_cost: Option<MemoryCost>,
    ) -> Result<(), ExitError> {
        let Some(layer) = self.transaction_data.last_mut() else {
            return Err(ExitError::Other(Cow::from(
                "Recording cost, but there is no transaction in progress",
            )));
        };

        layer
            .gasometer
            .as_mut()
            .map(|gasometer| gasometer.record_dynamic_cost(cost, memory_cost))
            .unwrap_or(Ok(()))
    }

    /// Record the stipend of a contract call. This differs from a storage
    /// operation refund in that the refunded gas can be used again by the
    /// same transaction. Function name reflects the SputnikVM name used to
    /// implement this functionality.
    fn record_stipend(&mut self, stipend: u64) -> Result<(), EthereumError> {
        let Some(layer) = self.transaction_data.last_mut() else {
            return Err(EthereumError::InconsistentTransactionStack(
                self.transaction_data.len(),
                false,
                false,
            ));
        };

        layer
            .gasometer
            .as_mut()
            .map(|gasometer| gasometer.record_stipend(stipend))
            .unwrap_or(Ok(()))
            .map_err(|_| {
                EthereumError::InconsistentState(Cow::from(
                    "Recording a stipend returned an error",
                ))
            })
    }

    /// Record the refund of a contract call.
    fn record_refund(&mut self, refund: i64) -> Result<(), EthereumError> {
        let Some(layer) = self.transaction_data.last_mut() else {
            return Err(EthereumError::InconsistentTransactionStack(
                self.transaction_data.len(),
                false,
                false,
            ));
        };

        layer
            .gasometer
            .as_mut()
            .map(|gasometer| gasometer.record_refund(refund))
            .unwrap_or(Ok(()))
            .map_err(|_| {
                EthereumError::InconsistentState(Cow::from(
                    "Recording a refund returned an error",
                ))
            })
    }

    fn get_word_size(&self, init_code: &[u8]) -> u64 {
        // ceil(len(init_code) / 32)
        (init_code.len() as u64).div_ceil(32)
    }

    fn record_init_code_cost(&mut self, init_code: &[u8]) -> Result<(), ExitError> {
        // As per EIP-3860:
        // > We define init_code_cost to equal INITCODE_WORD_COST * get_word_size(init_code).
        // where INITCODE_WORD_COST is 2.
        let init_code_cost = 2 * self.get_word_size(init_code);
        self.record_cost(init_code_cost)
    }

    /// Mark a location in durable storage as _hot_ for the purpose of calculating
    /// cost of SSTORE and SLOAD. Return type chosen for compatibility with the
    /// SputnikVM functions that need to call this function.
    fn mark_storage_as_hot(
        &mut self,
        address: H160,
        index: H256,
    ) -> Result<(), ExitError> {
        match self.transaction_data.last_mut() {
            Some(layer) => {
                layer.accessed_storage_keys.insert_storage(address, index);
                Ok(())
            }
            None => Err(ExitError::Other(Cow::from(
                "Invalid transaction data stack for mark_storage_as_hot",
            ))),
        }
    }

    /// Mark an address as _hot_ for the purpose of calculating
    /// cost of *CALL, BALANCE, EXT* and SELFDESTRUCT. Return type chosen for compatibility with the
    /// SputnikVM functions that need to call this function.
    fn mark_address_as_hot(&mut self, address: H160) -> Result<(), ExitError> {
        match self.transaction_data.last_mut() {
            Some(layer) => {
                layer.accessed_storage_keys.insert_address(address);
                Ok(())
            }
            None => Err(ExitError::Other(Cow::from(
                "Invalid transaction data stack for mark_address_as_hot",
            ))),
        }
    }

    /// Check if some location in durable storage is hot
    fn is_storage_hot(&self, address: H160, index: H256) -> Result<bool, ExitError> {
        let Some(layer) = self.transaction_data.last() else {
            return Err(ExitError::Other(Cow::from(
                "Invalid transaction data stack for is_storage_hot",
            )));
        };

        Ok(layer.accessed_storage_keys.contains_storage(address, index))
    }

    /// Check if address is hot
    fn is_address_hot(&self, address: H160) -> Result<bool, ExitError> {
        let Some(layer) = self.transaction_data.last() else {
            return Err(ExitError::Other(Cow::from(
                "Invalid transaction data stack for is_address_hot",
            )));
        };

        Ok(layer.accessed_storage_keys.contains_address(address))
    }

    /// Check if an address has either a nonzero nonce, or a nonzero code length, i.e., if the address exists.
    fn is_colliding(&mut self, address: H160) -> Result<bool, EthereumError> {
        let Some(account) = self.get_account(address)? else {
            return Ok(false);
        };

        let has_code = account
            .code_size(self.borrow_host())
            .map(|s| s != U256::zero())?;
        let non_zero_nonce = account.nonce(self.borrow_host()).map(|s| s != 0)?;

        Ok(has_code || non_zero_nonce)
    }

    /// Returns true if there is a static transaction in progress, otherwise
    /// return false.
    fn is_static(&self) -> bool {
        self.transaction_data
            .last()
            .map(|data| data.call_context.is_static)
            .unwrap_or(false)
    }

    /// Returns true if [address] was created during the on-going transaction accross
    /// all the layers.
    fn was_created(&self, address: &H160) -> bool {
        self.created_contracts.contains(address)
    }

    /// Record the base fee part of the transaction cost. We need the SputnikVM
    /// error code in case this goes wrong, so that's what we return.
    fn record_base_gas_cost(
        &mut self,
        is_create: bool,
        data: &[u8],
    ) -> Result<(), ExitError> {
        let base_cost = if is_create {
            self.config.gas_transaction_create
        } else {
            self.config.gas_transaction_call
        };

        let data_cost: u64 = data
            .iter()
            .map(|datum| {
                if *datum == 0_u8 {
                    self.config.gas_transaction_zero_data
                } else {
                    self.config.gas_transaction_non_zero_data
                }
            })
            .sum();

        self.record_cost(base_cost + data_cost)
    }

    /// Add withdrawals to the current transaction layer
    pub fn add_withdrawals(
        &mut self,
        withdrawals: &mut Vec<Withdrawal>,
    ) -> Result<(), EthereumError> {
        match self.transaction_data.last_mut() {
            Some(layer) => {
                layer.withdrawals.try_reserve_exact(withdrawals.len())?;
                layer.withdrawals.append(withdrawals);

                Ok(())
            }
            None => Err(EthereumError::InconsistentTransactionStack(0, false, false)),
        }
    }

    /// Add log to the current transaction layer
    pub(crate) fn add_log(&mut self, log: Log) -> Result<(), ExitError> {
        if let Some(top_data) = self.transaction_data.last_mut() {
            top_data.logs.push(log);
            Ok(())
        } else {
            Err(ExitError::Other(Cow::from("No transaction data for log")))
        }
    }

    /// Have the caller account pay for gas. Returns `Ok(true)` if the payment
    /// went through; returns `Ok(false)` if `caller` doesn't have the funds.
    /// Return `Err(...)` in case something is at fault with durable storage or
    /// runtime.
    pub fn pre_pay_transactions(
        &mut self,
        caller: H160,
        gas_limit: Option<u64>,
        effective_gas_price: U256,
    ) -> Result<bool, EthereumError> {
        let Some(gas_limit) = gas_limit else {
            return Ok(true);
        };

        let amount = U256::from(gas_limit)
            .checked_mul(effective_gas_price)
            .ok_or(EthereumError::GasPaymentOverflow)?;

        log!(
            self.host,
            Debug,
            "{caller:?} pays {amount:?} for transaction"
        );

        self.get_or_create_account(caller)?
            .balance_remove(self.host, amount)
            .map_err(EthereumError::from)
    }

    /// Repay unused gas
    pub fn repay_gas(
        &mut self,
        caller: H160,
        unused_gas: Option<u64>,
        effective_gas_price: U256,
    ) -> Result<(), EthereumError> {
        let Some(unused_gas) = unused_gas else {
            return Ok(());
        };

        let amount = U256::from(unused_gas)
            .checked_mul(effective_gas_price)
            .ok_or(EthereumError::GasPaymentOverflow)?;

        log!(
            self.host,
            Debug,
            "{caller:?} refunded {amount:?} for transaction"
        );

        self.get_or_create_account(caller)?
            .balance_add(self.host, amount)
            .map_err(EthereumError::from)
    }

    /// Account for the estimated ticks spent during the execution of the given opcode
    pub fn account_for_ticks(&mut self, opcode: &Opcode, gas: u64) {
        self.estimated_ticks_used += tick_model_opcodes::ticks(opcode, gas)
    }

    /// Clear the entire storage located at [address].
    pub fn clear_storage(&mut self, address: H160) -> Result<(), EthereumError> {
        if let Some(account) = self.get_account(address)? {
            account.clear_storage(self.host)?
        };
        Ok(())
    }

    /// Prepare trace info if needed
    fn prepare_trace(
        &mut self,
        runtime: &evm::Runtime,
        opcode: &Option<Opcode>,
    ) -> Option<StructLog> {
        if let (
            Some(StructLogger(StructLoggerInput {
                transaction_hash: _,
                config,
            })),
            Some(opcode),
        ) = (&self.tracer, opcode)
        {
            let opcode = opcode.as_u8();
            let pc: u64 = runtime
                .machine()
                .trace_position()
                .ok()?
                .try_into()
                .unwrap_or_default();
            let gas = self.gas_remaining();
            let depth: u16 = self.transaction_data.len().try_into().unwrap_or_default();
            let stack = (!config.disable_stack)
                .then(|| runtime.machine().stack().data().to_owned());
            let return_data = config
                .enable_return_data
                .then(|| runtime.machine().return_value());
            let memory = config
                .enable_memory
                .then(|| runtime.machine().memory().data()[..].to_vec());
            let storage = (!config.disable_storage).then(|| {
                let mut flat_storage = vec![];
                self.storage_cache
                    .clone()
                    .into_iter()
                    .flat_map(|(_, layer_cache)| layer_cache)
                    .for_each(|(StorageKey { address, index }, value)| {
                        flat_storage.push(StorageMapItem {
                            address,
                            index,
                            value: value.h256(),
                        });
                    });
                flat_storage
            });

            Some(StructLog::prepare(
                pc,
                opcode,
                gas,
                depth,
                stack,
                return_data,
                memory,
                storage,
            ))
        } else {
            None
        }
    }

    fn complete_and_store_trace(
        &mut self,
        trace: Option<StructLog>,
        gas_cost: u64,
        step_result: &Result<(), Capture<ExitReason, Resolve<EvmHandler<'_, Host>>>>,
    ) -> Result<(), EthereumError> {
        if let (
            Some(StructLogger(StructLoggerInput {
                transaction_hash,
                config: _,
            })),
            Some(struct_log),
        ) = (self.tracer, trace)
        {
            // TODO: https://gitlab.com/tezos/tezos/-/issues/7437
            // For error, find the appropriate value to return for tracing.
            // The following value is kind of a placeholder.
            let error = if let Err(Capture::Exit(reason)) = &step_result {
                match &reason {
                    ExitReason::Error(exit) => {
                        Some(format!("{:?}", exit).as_bytes().to_vec())
                    }
                    ExitReason::Fatal(exit) => {
                        Some(format!("{:?}", exit).as_bytes().to_vec())
                    }
                    _ => None,
                }
            } else {
                None
            };
            tracer::store_struct_log(
                self.host,
                struct_log.finish(gas_cost, error),
                &transaction_hash,
            )?;
        }
        Ok(())
    }

    /// Execute a SputnikVM run with this handler
    ///
    // DO NOT RENAME: function name is used during benchmark
    // Never inlined when the kernel is compiled for benchmarks, to ensure the
    // function is visible in the profiling results.
    #[cfg_attr(feature = "benchmark", inline(never))]
    fn execute(
        &mut self,
        runtime: &mut evm::Runtime,
    ) -> Result<ExitReason, EthereumError> {
        loop {
            // This decomposition allows both benchmarking the ticks per gas
            // consumption of opcode and implement the tick model at the opcode
            // level. At the end of each step if the kernel takes more than the
            // allocated ticks the transaction is marked as failed.
            let opcode = runtime.machine().inspect().map(|p| p.0);
            let trace = self.prepare_trace(runtime, &opcode);

            #[cfg(feature = "benchmark-opcodes")]
            if let Some(opcode) = opcode {
                benchmarks::start_opcode_section(self.host, &opcode);
            }

            // For now, these variables capturing the gas one will be marked
            // unused without benchmarking, but they will be used during the
            // tick accounting.
            #[cfg_attr(not(feature = "benchmark-opcodes"), allow(unused_variables))]
            let gas_before = self.gas_used();

            let step_result = runtime.step(self);

            #[cfg_attr(not(feature = "benchmark-opcodes"), allow(unused_variables))]
            let gas_after = self.gas_used();

            let gas_cost = gas_after - gas_before;

            if let Some(opcode) = opcode {
                self.account_for_ticks(&opcode, gas_cost);
                #[cfg(feature = "benchmark-opcodes")]
                benchmarks::end_opcode_section(self.host, gas_cost, &step_result);
            };

            self.complete_and_store_trace(trace, gas_cost, &step_result)?;

            match step_result {
                Ok(()) => (),
                Err(Capture::Exit(reason)) => {
                    return Ok(reason);
                }
                Err(Capture::Trap(_)) => {
                    return Err(EthereumError::InternalTrapError);
                }
            }
        }
    }

    fn create_address(
        &mut self,
        scheme: CreateScheme,
    ) -> Result<H160, AccountStorageError> {
        match scheme {
            CreateScheme::Create2 {
                caller,
                code_hash,
                salt,
            } => {
                let mut hasher = Keccak256::new();
                hasher.update([0xff]);
                hasher.update(caller);
                hasher.update(salt);
                hasher.update(code_hash);
                Ok(H256::from_slice(hasher.finalize().as_slice()).into())
            }
            CreateScheme::Legacy { caller } => {
                let nonce = self.get_nonce(caller)?;
                Ok(create_address_legacy(&caller, &nonce))
            }
            CreateScheme::Fixed(address) => Ok(address),
        }
    }

    /// Execute a transfer between two accounts
    ///
    /// In case the transfer succeeds, the function returns
    /// `Ok(ExitReason::Succeed(ExitSucceed::Returned))`. In case the
    /// transaction fails, but execution doesn't encounter non-contract or
    /// -account errors, it returns `Ok(ExitReason::Error(err))`, where `err`
    /// indicates what went wrong (insufficient balance, etc.). In case of
    /// critical errors in the rollup node or kernel, an `Err(err)` is returned,
    /// where `err` indicates what went wrong, eg, a storage error.
    fn execute_transfer(
        &mut self,
        from: H160,
        to: H160,
        value: U256,
    ) -> Result<TransferExitReason, EthereumError> {
        log!(
            self.host,
            Debug,
            "Executing a transfer from {} to {} of {}",
            from,
            to,
            value
        );

        if value == U256::zero() {
            // Nothing to transfer so succeeds by default
            Ok(TransferExitReason::Returned)
        } else if let Some(mut from_account) = self.get_account(from)? {
            let mut to_account = self.get_or_create_account(to)?;

            if from_account.balance_remove(self.host, value)? {
                to_account
                    .balance_add(self.host, value)
                    .map_err(EthereumError::from)?;
                Ok(TransferExitReason::Returned)
            } else {
                log!(
                    self.host,
                    Debug,
                    "Failed transfer due to insufficient funds, value: {:?}, from: {:?}, to: {:?}",
                    value,
                    from_account,
                    to
                );

                Ok(TransferExitReason::OutOfFund)
            }
        } else {
            log!(self.host, Debug, "'from' account {:?} is empty", from);
            // Accounts of zero balance by default, so this must be
            // an underflow.
            Ok(TransferExitReason::OutOfFund)
        }
    }

    // Stack depth is the number of internal call that happened in the EVM
    // It's just the number of transaction minus the initial one
    // NB: Different from `stack_depth` in `src/kernel_sdk/storage/src/storage.rs`
    fn stack_depth(&self) -> usize {
        let number_of_tx_layer = self.evm_account_storage.stack_depth();
        number_of_tx_layer.checked_sub(1).unwrap_or_default()
    }

    fn has_enough_fund(&self, from: H160, value: &U256) -> Result<bool, EthereumError> {
        if value.is_zero() {
            Ok(true)
        } else if let Some(from_account) = self.get_account(from)? {
            let balance = from_account
                .balance(self.host)
                .map_err(EthereumError::from)?;
            let enough_balance = &balance >= value;
            if !enough_balance {
                log!(
                    self.host,
                    Debug,
                    "Insufficient funds, balance of {:?} is {:?} but needs at least {:?}",
                    from,
                    balance,
                    value
                );
            }
            Ok(enough_balance)
        } else {
            log!(self.host, Debug, "'from' account {:?} is empty", from);
            // Accounts of zero balance by default, so this must be
            // an underflow.
            Ok(false)
        }
    }

    fn end_create(
        &mut self,
        runtime: evm::Runtime,
        creation_result: Result<ExitReason, EthereumError>,
        address: H160,
    ) -> Result<CreateOutcome, EthereumError> {
        match creation_result {
            Ok(sub_context_result @ ExitReason::Succeed(ExitSucceed::Suicided)) => {
                Ok((sub_context_result, Some(address), vec![]))
            }
            Ok(sub_context_result @ ExitReason::Succeed(_)) => {
                let code_out = runtime.machine().return_value();

                if code_out.first() == Some(&0xef) {
                    // EIP-3541: see https://github.com/ethereum/EIPs/blob/master/EIPS/eip-3541.md
                    return Ok((
                        ExitReason::Error(ExitError::InvalidCode(Opcode(0xef))),
                        None,
                        vec![],
                    ));
                }

                // We check that the maximum allowed code size as specified by EIP-170 can not
                // be reached.
                if let Some(create_contract_limit) = self.config.create_contract_limit {
                    if code_out.len() > create_contract_limit {
                        // EIP-170: see https://github.com/ethereum/EIPs/blob/master/EIPS/eip-170.md
                        return Ok((
                            ExitReason::Error(ExitError::CreateContractLimit),
                            None,
                            vec![],
                        ));
                    }
                }

                if let Err(err) = self.record_deposit(code_out.len()) {
                    return Ok((ExitReason::Error(err), None, vec![]));
                }

                self.set_contract_code(address, &code_out)?;

                Ok((sub_context_result, Some(address), code_out))
            }
            Ok(sub_context_result @ ExitReason::Revert(_)) => {
                // EIP-140: https://github.com/ethereum/EIPs/blob/master/EIPS/eip-140.md
                // In case of a REVERT in a context of a CREATE, CREATE2, the error message
                // is available in the returndata buffer
                Ok((sub_context_result, None, runtime.machine().return_value()))
            }
            // Since the creation fails, return address 0 (`None` in our case) (https://www.evm.codes/#f0?fork=shanghai)
            Ok(create_err @ ExitReason::Error(_)) => Ok((create_err, None, vec![])),
            Ok(create_err @ ExitReason::Fatal(_)) => Ok((create_err, None, vec![])),
            Err(err) => Err(err),
        }
    }

    pub fn can_begin_inter_transaction_call_stack(&self) -> bool {
        self.stack_depth() < self.config.call_stack_limit
    }

    // Sub function to determine if the `caller` can create a new internal transaction.
    // According to the Ethereum yellow paper (p.37) for `CALL`, `CREATE`, ... instructions that
    // creates a new substate, it must always check that there are no `OutOfFund` or `CallTooDeep`
    fn can_begin_inter_transaction(&self, caller: H160, value: &U256) -> Precondition {
        // This check SHOULD be called outside of `begin_inter` and `end_inter`, this way
        // we can reproduce the exact same check on the stack from the Ethereum yellow paper (p.37).
        match (
            self.has_enough_fund(caller, value),
            self.can_begin_inter_transaction_call_stack(),
        ) {
            (Ok(true), true) => Precondition::PassPrecondition,
            (Ok(false), _) => {
                Precondition::PreconditionErr(ExitReason::Error(ExitError::OutOfFund))
            }
            (Ok(_), false) => {
                Precondition::PreconditionErr(ExitReason::Error(ExitError::CallTooDeep))
            }
            (Err(err), _) => Precondition::EthereumErr(err),
        }
    }

    // Sub function to handle the collision part, we pinpoint two ways of colliding:
    //   - the contract already exists
    //   - it's been marked as `deleted` within the same transaction
    fn contract_will_collide(&mut self, address: H160) -> Precondition {
        if self.deleted(address) {
            // The contract has been deleted, so the address is empty.
            // We are trying to re-create the same contract that was deleted at
            // the same transaction level: this is not allowed.
            // TODO/NB: https://gitlab.com/tezos/tezos/-/issues/6783
            // This behaviour is appropriate to <=Shanghai configuration.
            // In the upcoming Cancun fork, the semantic of this behaviour will change.
            Precondition::PreconditionErr(ExitReason::Error(ExitError::CreateCollision))
        } else {
            match self.is_colliding(address) {
                Ok(false) => Precondition::PassPrecondition,
                Ok(true) => {
                    log!(
                        self.host,
                        Debug,
                        "Failed to create contract at {:?}. Address is non-empty",
                        address
                    );
                    Precondition::PreconditionErr(ExitReason::Error(
                        ExitError::CreateCollision,
                    ))
                }
                Err(collide_err) => Precondition::EthereumErr(collide_err),
            }
        }
    }

    /// Create a contract
    ///
    /// Performs the actual contract creation for both transactions initiated
    /// by external accounts and contract creation initiated through contract
    /// execution.
    ///
    /// In the specific case where this function is called via the CREATE opcode,
    /// it needs to bump the nonce. If it's a transaction initated by external
    /// accounts, the nonce must be bumped by the caller.
    fn execute_create(
        &mut self,
        caller: H160,
        value: U256,
        initial_code: Vec<u8>,
        address: H160,
    ) -> Result<CreateOutcome, EthereumError> {
        log!(self.host, Debug, "Executing a contract create");

        let context = Context {
            address,
            caller,
            apparent_value: value,
        };

        let mut runtime = evm::Runtime::new(
            Rc::new(initial_code),
            Rc::new(Vec::new()),
            context,
            self.config.stack_limit,
            self.config.memory_limit,
        );

        // Execute if there is no collision
        let creation_result = match self.contract_will_collide(address) {
            Precondition::PassPrecondition => {
                match self.execute_transfer(caller, address, value) {
                    Ok(TransferExitReason::Returned) => {
                        match self.increment_nonce(address) {
                            Ok(()) => {
                                self.clear_storage(address)?;
                                self.execute(&mut runtime)
                            }
                            Err(eth_err) => Err(eth_err),
                        }
                    }
                    Ok(TransferExitReason::OutOfFund) => {
                        Ok(ExitReason::Error(ExitError::OutOfFund))
                    }
                    Err(err) => Err(err),
                }
            }
            Precondition::PreconditionErr(collision) => Ok(collision),
            Precondition::EthereumErr(err) => Err(err),
        };

        self.end_create(runtime, creation_result, address)
    }

    /// Call a contract
    ///
    /// Perform the actual contract execution - works both for executing an
    /// Ethereum transaction as initiated by an external account or as aresult
    /// of any of the -CALL instructions.
    ///
    /// The outcome is encoded as a SputnikVM _Create_ outcome for easy transaction
    /// handling. The new address "field" in the triple is always `None`.
    #[allow(clippy::too_many_arguments)]
    pub(crate) fn execute_call(
        &mut self,
        address: H160,
        transfer: Option<Transfer>,
        input: Vec<u8>,
        transaction_context: TransactionContext,
    ) -> Result<CreateOutcome, EthereumError> {
        let stack_depth = self.stack_depth();
        log!(
            self.host,
            Debug,
            "Executing contract call on contract {} at depth: {}",
            address,
            stack_depth
        );

        if let Some(ref transfer) = transfer {
            match self.execute_transfer(
                transfer.source,
                transfer.target,
                transfer.value,
            )? {
                TransferExitReason::OutOfFund => {
                    return Ok((ExitReason::Error(ExitError::OutOfFund), None, vec![]))
                }
                TransferExitReason::Returned => (), // Otherwise result is ok and we do nothing and continue
            }
        }
        #[cfg(feature = "benchmark-opcodes")]
        benchmarks::start_precompile_section(self.host, address, &input);

        if let Err(reason) = self.reentrancy_guard.begin_precompile_call(&address) {
            return Ok((ExitReason::Revert(ExitRevert::Reverted), None, reason));
        }

        let precompile_execution_result = self.precompiles.execute(
            self,
            address,
            &input,
            &transaction_context.context,
            self.is_static(),
            transfer,
        );

        self.reentrancy_guard.end_precompile_call();

        #[cfg(feature = "benchmark-opcodes")]
        benchmarks::end_precompile_section(self.host);

        if let Some(precompile_result) = precompile_execution_result {
            match precompile_result {
                Ok(mut outcome) => {
                    self.add_withdrawals(&mut outcome.withdrawals)?;
                    self.estimated_ticks_used += outcome.estimated_ticks;
                    Ok((outcome.exit_status, None, outcome.output))
                }
                Err(err) => Err(err),
            }
        } else {
            let code = self.code(address);

            let mut runtime = evm::Runtime::new(
                Rc::new(code),
                Rc::new(input),
                transaction_context.context,
                self.config.stack_limit,
                self.config.memory_limit,
            );

            let result = self.execute(&mut runtime)?;

            Ok((result, None, runtime.machine().return_value()))
        }
    }

    /// Perform a contract call transaction
    pub fn call_contract(
        &mut self,
        caller: H160,
        callee: H160,
        value: Option<U256>,
        input: Vec<u8>,
        gas_limit: Option<u64>,
        is_static: bool,
    ) -> Result<ExecutionOutcome, EthereumError> {
        self.increment_nonce(caller)?;
        self.begin_initial_transaction(
            CallContext {
                is_static,
                is_creation: false,
            },
            gas_limit,
        )?;

        if self.mark_address_as_hot(caller).is_err() {
            return Err(EthereumError::InconsistentState(Cow::from(
                "Failed to mark caller address as hot",
            )));
        }

        if self.mark_address_as_hot(callee).is_err() {
            return Err(EthereumError::InconsistentState(Cow::from(
                "Failed to mark callee address as hot",
            )));
        }

        if let Err(err) = self.record_base_gas_cost(false, &input) {
            return self.end_initial_transaction(Ok((
                ExitReason::Error(err),
                None,
                vec![],
            )));
        }

        let result = self.execute_call(
            callee,
            value.map(|value| Transfer {
                source: caller,
                target: callee,
                value,
            }),
            input,
            TransactionContext::new(caller, callee, value.unwrap_or_default()),
        );

        self.end_initial_transaction(result)
    }

    /// Perform a create-contract transaction
    pub fn create_contract(
        &mut self,
        caller: H160,
        value: Option<U256>,
        input: Vec<u8>,
        gas_limit: Option<u64>,
    ) -> Result<ExecutionOutcome, EthereumError> {
        let default_create_scheme = CreateScheme::Legacy { caller };
        let address = self.create_address(default_create_scheme)?;
        self.increment_nonce(caller)?;

        self.begin_initial_transaction(
            CallContext {
                is_static: false,
                is_creation: true,
            },
            gas_limit,
        )?;

        if self.mark_address_as_hot(caller).is_err() {
            return Err(EthereumError::InconsistentState(Cow::from(
                "Failed to mark caller address as hot",
            )));
        }

        if let Err(err) = self.record_base_gas_cost(true, &input) {
            return self.end_initial_transaction(Ok((
                ExitReason::Error(err),
                None,
                vec![],
            )));
        }

        if self.mark_address_as_hot(address).is_err() {
            return Err(EthereumError::InconsistentState(Cow::from(
                "Failed to mark callee address as hot",
            )));
        }

        // We check that the maximum allowed init code size as specified by EIP-3860
        // can not be reached.
        if let Some(max_initcode_size) = self.config.max_initcode_size {
            if input.len() > max_initcode_size {
                return self.end_initial_transaction(Ok((
                    ExitReason::Error(ExitError::CreateContractLimit),
                    None,
                    vec![],
                )));
            }
        }

        if let Err(err) = self.record_init_code_cost(&input) {
            log!(self.host, Debug, "{:?}: Cannot record init code cost.", err);

            return self.end_initial_transaction(Ok((
                ExitReason::Error(ExitError::OutOfGas),
                None,
                vec![],
            )));
        }

        let result =
            self.execute_create(caller, value.unwrap_or_default(), input, address);

        self.end_initial_transaction(result)
    }

    pub(crate) fn get_or_create_account(
        &self,
        address: H160,
    ) -> Result<EthereumAccount, EthereumError> {
        self.evm_account_storage
            .get_or_create(
                self.host,
                &account_path(&address).map_err(AccountStorageError::from)?,
            )
            .map_err(EthereumError::from)
    }

    pub(crate) fn get_account(
        &self,
        address: H160,
    ) -> Result<Option<EthereumAccount>, StorageError> {
        // Note: if we get an error we cannot report this to SputnikVM as the return types
        // for functions that use _this_ function don't support errors. Rather than do
        // error handling in all those functions (and those we'll write in the future), we
        // do the error handling here.
        if let Ok(path) = account_path(&address) {
            self.evm_account_storage.get(self.host, &path)
        } else {
            log!(
                self.host,
                Debug,
                "Failed to get account path for EVM handler get_account"
            );
            Ok(None)
        }
    }

    fn get_original_account(
        &self,
        address: H160,
    ) -> Result<Option<EthereumAccount>, StorageError> {
        // Note, there is no way to recover from an error when creating the
        // account path. At this point we are being called from SputnikVM and
        // it does not allow for this to fail, so we just return None.
        if let Ok(path) = account_path(&address) {
            self.evm_account_storage.get_original(self.host, &path)
        } else {
            log!(
                self.host,
                Debug,
                "Failed to get account path for EVM handler get_original_account"
            );
            Ok(None)
        }
    }

    pub fn increment_nonce(&mut self, address: H160) -> Result<(), EthereumError> {
        match account_path(&address) {
            Ok(path) => {
                let mut account =
                    self.evm_account_storage.get_or_create(self.host, &path)?;
                account
                    .increment_nonce(self.host)
                    .map_err(EthereumError::from)
            }
            Err(err) => {
                log!(
                    self.host,
                    Debug,
                    "Failed to increment nonce for account {:?}",
                    address
                );
                Err(EthereumError::from(AccountStorageError::from(err)))
            }
        }
    }

    fn set_contract_code(
        &mut self,
        address: H160,
        code: &[u8],
    ) -> Result<(), EthereumError> {
        self.update_contract_cache(address, code);
        self.get_or_create_account(address)?
            .set_code(self.host, code)
            .map_err(EthereumError::from)
    }

    fn get_nonce(&self, address: H160) -> Result<u64, AccountStorageError> {
        self.get_account(address)?
            .map(|account| account.nonce(self.host))
            .unwrap_or(Ok(0))
    }

    fn reset_balance(&mut self, address: H160) -> Result<(), AccountStorageError> {
        match self.get_account(address)? {
            Some(mut account) => account.set_balance(self.host, U256::zero()),
            None => Err(AccountStorageError::StorageError(
                StorageError::InvalidAccountsPath,
            )),
        }
    }

    /// Completely delete an account including nonce, code, and data. This is for
    /// contract selfdestruct completion, ie, when contract selfdestructs takes final
    /// effect.
    fn delete_contract(&mut self, address: H160) -> Result<(), EthereumError> {
        log!(self.host, Debug, "Deleting contract at {:?}", address);

        self.evm_account_storage
            .delete(
                self.host,
                &account_path(&address).map_err(AccountStorageError::from)?,
            )
            .map_err(EthereumError::from)
    }

    /// Borrow a reference to the host - needed for eg precompiled contracts
    pub fn borrow_host(&mut self) -> &'_ mut Host {
        self.host
    }

    /// Begin the first transaction layer
    ///
    /// This requires that no other transaction is in progress. If there is a
    /// transaction in progress, then the function returns an error to report
    /// this.
    pub(crate) fn begin_initial_transaction(
        &mut self,
        call_context: CallContext,
        gas_limit: Option<u64>,
    ) -> Result<(), EthereumError> {
        let number_of_tx_layer = self.evm_account_storage.stack_depth();
        log!(self.host, Debug, "Begin initial transaction");

        if number_of_tx_layer > 0 {
            log!(
                self.host,
                Debug,
                "Initial transaction when there is already {} transaction",
                number_of_tx_layer
            );

            return Err(EthereumError::InconsistentTransactionStack(
                number_of_tx_layer,
                true,
                true,
            ));
        }

        self.transaction_data.push(TransactionLayerData::new(
            self.is_static() || call_context.is_static,
            call_context.is_creation,
            gas_limit,
            self.config,
            AccessRecord::default(),
        ));

        self.evm_account_storage
            .begin_transaction(self.host)
            .map_err(EthereumError::from)
    }

    /// Final commit of initial transaction
    ///
    /// This requires that only one transaction is in progress. Since we should
    /// never end in a state with a transaction in progress after we are done
    /// executing, such state is the sort of thing that may cause panic.
    fn commit_initial_transaction(
        &mut self,
        result: ExecutionResult,
    ) -> Result<ExecutionOutcome, EthereumError> {
        let number_of_tx_layer = self.evm_account_storage.stack_depth();
        log!(self.host, Debug, "Committing initial transaction");

        if number_of_tx_layer != 1 {
            log!(
                self.host,
                Debug,
                "Committing initial transaction, but there are {:?} transactions",
                number_of_tx_layer
            );

            return Err(EthereumError::InconsistentTransactionStack(
                number_of_tx_layer,
                true,
                false,
            ));
        }

        if number_of_tx_layer != self.transaction_data.len() {
            return Err(EthereumError::InconsistentTransactionData(
                number_of_tx_layer,
                self.transaction_data.len(),
            ));
        }

        let gas_used = self.gas_used_after_refund();

        if let Some(last_layer) = self.transaction_data.pop() {
            for address in last_layer.deleted_contracts.iter() {
                if self.delete_contract(*address).is_err() {
                    log!(
                        self.host,
                        Debug,
                        "Failed to remove deleted address {:?}",
                        address
                    );
                }
            }

            commit_storage_cache(self, number_of_tx_layer);

            self.evm_account_storage
                .commit_transaction(self.host)
                .map_err(EthereumError::from)?;

            if let ExecutionResult::ContractDeployed(new_address, _) = result {
                self.created_contracts.insert(new_address);
            }

            // We flush the storage's cache into the durable storage
            // by actually writing in it.
            self.flush_storage_cache()?;
            self.clear_internal_caches();

            Ok(ExecutionOutcome {
                gas_used,
                logs: last_layer.logs,
                result,
                withdrawals: last_layer.withdrawals,
                estimated_ticks_used: self.estimated_ticks_used,
            })
        } else {
            Err(EthereumError::InconsistentState(Cow::from(
                "The transaction data stack is empty when committing the initial transaction",
            )))
        }
    }

    /// Rollback of initial transaction
    ///
    /// This requires that only one transaction is in progress. Since we should
    /// never end in a state with a transaction in progress after we are done
    /// executing, such state is the sort of thing that may cause panic.
    fn rollback_initial_transaction(
        &mut self,
        result: ExecutionResult,
    ) -> Result<ExecutionOutcome, EthereumError> {
        let number_of_tx_layer = self.evm_account_storage.stack_depth();
        log!(self.host, Debug, "Rolling back the initial transaction");

        if number_of_tx_layer != 1 {
            log!(
                self.host,
                Debug,
                "Rolling back initial transaction, but there are {:?} in progress",
                number_of_tx_layer
            );

            return Err(EthereumError::InconsistentTransactionStack(
                number_of_tx_layer,
                true,
                false,
            ));
        }

        if number_of_tx_layer != self.transaction_data.len() {
            return Err(EthereumError::InconsistentTransactionData(
                number_of_tx_layer,
                self.transaction_data.len(),
            ));
        }

        let gas_used = self.gas_used_after_refund();

        self.evm_account_storage
            .rollback_transaction(self.host)
            .map_err(EthereumError::from)?;

        let _ = self.transaction_data.pop();
        self.clear_internal_caches();

        Ok(ExecutionOutcome {
            gas_used,
            logs: vec![],
            result,
            withdrawals: vec![],
            estimated_ticks_used: self.estimated_ticks_used,
        })
    }

    fn flush_storage_cache(&mut self) -> Result<(), EthereumError> {
        let storage_cache_size = self.storage_cache.len();
        if storage_cache_size > 1 {
            log!(
                self.host,
                Fatal,
                "The storage cache size is {storage_cache_size} when \
                 flushing. It is inconsistent with the transaction stack. \
                 The EVM is most likely broken."
            );
            return Err(EthereumError::InconsistentTransactionStack(
                storage_cache_size,
                false,
                false,
            ));
        }

        if let Some(cache) = self.storage_cache.get(&0) {
            for (StorageKey { address, index }, value) in cache.iter() {
                if let CacheStorageValue::Write(value) = value {
                    let mut account = self.get_or_create_account(*address)?;
                    account.set_storage(self.host, index, value)?;
                }
            }
        }
        Ok(())
    }

    /// End the initial transaction with either a commit or a rollback. The
    /// outcome depends on the execution result given.
    pub(crate) fn end_initial_transaction(
        &mut self,
        execution_result: Result<CreateOutcome, EthereumError>,
    ) -> Result<ExecutionOutcome, EthereumError> {
        match execution_result {
            Ok((ExitReason::Succeed(r), new_address, result)) => {
                log!(
                    self.host,
                    Debug,
                    "The initial transaction ended with success: {:?}",
                    r
                );

                self.commit_initial_transaction(if let Some(new_address) = new_address {
                    ExecutionResult::ContractDeployed(new_address, result)
                } else {
                    ExecutionResult::CallSucceeded(r, result)
                })
            }
            Ok((ExitReason::Revert(ExitRevert::Reverted), _, result)) => {
                self.rollback_initial_transaction(ExecutionResult::CallReverted(result))
            }
            Ok((ExitReason::Error(error), _, _)) => {
                log!(
                    self.host,
                    Debug,
                    "The initial transaction ended with an error: {:?}",
                    error
                );

                self.rollback_initial_transaction(ExecutionResult::Error(error))
            }
            Ok((ExitReason::Fatal(ExitFatal::Other(cow_str)), _, _)) => {
                self.rollback_initial_transaction(ExecutionResult::FatalError(
                    ExitFatal::Other(cow_str.clone()),
                ))?;
                Err(EthereumError::WrappedError(cow_str))
            }
            Ok((ExitReason::Fatal(fatal_error), _, _)) => {
                log!(
                    self.host,
                    Debug,
                    "The initial transaction ended with a fatal error: {:?}",
                    fatal_error
                );

                self.rollback_initial_transaction(ExecutionResult::FatalError(
                    fatal_error,
                ))
            }
            Err(EthereumError::OutOfTicks) => {
                log!(
                    self.host,
                    Debug,
                    "The initial transaction exhausted the allocated ticks."
                );

                self.rollback_initial_transaction(ExecutionResult::OutOfTicks)
            }
            Err(err) => {
                log!(
                    self.host,
                    Debug,
                    "The initial transaction ended with an Ethereum error: {:?}",
                    err
                );

                self.rollback_initial_transaction(ethereum_error_to_execution_result(
                    &err,
                ))?;
                Err(err)
            }
        }
    }

    /// Begin an intermediate transaction
    pub fn begin_inter_transaction(
        &mut self,
        call_context: CallContext,
        gas_limit: Option<u64>,
    ) -> Result<(), EthereumError> {
        let number_of_tx_layer = self.evm_account_storage.stack_depth();
        log!(
            self.host,
            Debug,
            "Begin transaction from {} at transaction depth: {}",
            self.origin(),
            self.stack_depth()
        );

        if number_of_tx_layer == 0 {
            return Err(EthereumError::InconsistentTransactionStack(0, false, true));
        }

        let Some(current_top) = self.transaction_data.last() else {
            return Err(EthereumError::InconsistentTransactionStack(0, false, true));
        };

        let accessed_storage_keys = current_top.accessed_storage_keys.clone();

        self.transaction_data.push(TransactionLayerData::new(
            self.is_static() || call_context.is_static,
            call_context.is_creation,
            gas_limit,
            self.config,
            accessed_storage_keys,
        ));

        self.evm_account_storage
            .begin_transaction(self.host)
            .map_err(EthereumError::from)
    }

    /// Commit an intermediate transaction
    fn commit_inter_transaction(&mut self) -> Result<(), EthereumError> {
        let number_of_tx_layer = self.evm_account_storage.stack_depth();

        if number_of_tx_layer < 2 {
            return Err(EthereumError::InconsistentTransactionStack(
                number_of_tx_layer,
                false,
                false,
            ));
        }

        log!(
            self.host,
            Debug,
            "Commit transaction at transaction depth: {}",
            self.stack_depth()
        );

        let gas_remaining = self.gas_remaining();
        let gas_refunded = self.gas_refunded();

        commit_storage_cache(self, number_of_tx_layer);
        commit_transient_storage(self, number_of_tx_layer);

        self.evm_account_storage
            .commit_transaction(self.host)
            .map_err(EthereumError::from)?;

        if let Some(mut committed_data) = self.transaction_data.pop() {
            if let Some(top_layer) = self.transaction_data.last_mut() {
                top_layer
                    .logs
                    .try_reserve_exact(committed_data.logs.len())?;
                top_layer.logs.append(&mut committed_data.logs);

                top_layer
                    .withdrawals
                    .try_reserve_exact(committed_data.withdrawals.len())?;
                top_layer
                    .withdrawals
                    .append(&mut committed_data.withdrawals);

                top_layer
                    .deleted_contracts
                    .extend(committed_data.deleted_contracts);
                top_layer.accessed_storage_keys = committed_data.accessed_storage_keys;

                self.record_stipend(gas_remaining)?;
                self.record_refund(gas_refunded)?;

                Ok(())
            } else {
                Err(EthereumError::InconsistentState(Cow::from(
                    "The transaction data stack is empty",
                )))
            }
        } else {
            Err(EthereumError::InconsistentState(Cow::from(
                "The transaction data stack is empty at commit",
            )))
        }
    }

    /// Rollback an intermediate transaction
    fn rollback_inter_transaction(
        &mut self,
        refund_gas: bool,
    ) -> Result<(), EthereumError> {
        let number_of_tx_layer = self.evm_account_storage.stack_depth();

        if number_of_tx_layer < 2 {
            return Err(EthereumError::InconsistentTransactionStack(
                number_of_tx_layer,
                false,
                false,
            ));
        }

        log!(
            self.host,
            Debug,
            "Rollback transaction at transaction depth: {}",
            self.stack_depth()
        );

        if refund_gas {
            let gas_remaining = self.gas_remaining();
            let _ = self.transaction_data.pop();
            self.record_stipend(gas_remaining)?;
        } else {
            let _ = self.transaction_data.pop();
        }

        self.storage_cache.remove(&number_of_tx_layer);
        self.transient_storage.remove(&number_of_tx_layer);

        self.evm_account_storage
            .rollback_transaction(self.host)
            .map_err(EthereumError::from)
    }

    fn rollback_inter_transaction_side_effect<T>(
        handler: &mut EvmHandler<'_, Host>,
        execution_result: CreateOutcome,
        refund_gas: bool,
    ) -> Capture<CreateOutcome, T> {
        if let Err(err) = handler.rollback_inter_transaction(refund_gas) {
            log!(
                handler.host,
                Debug,
                "Rolling back reverted transaction caused an error: {:?}",
                err
            );

            Capture::Exit((ethereum_error_to_exit_reason(&err), None, vec![]))
        } else {
            Capture::Exit(execution_result)
        }
    }

    /// End a transaction based on an execution result from a call to
    /// [execute]. This can be either a rollback or a commit depending
    /// on whether the execution was successful or not.
    ///
    /// This function applies _only_ to intermediate transactions. Calling
    /// it with only the initial transaction in progress is an error.
    pub fn end_inter_transaction<T>(
        &mut self,
        execution_result: Result<CreateOutcome, EthereumError>,
    ) -> Capture<CreateOutcome, T> {
        match execution_result {
            Ok((ref exit_reason, new_address, _)) => match exit_reason {
                ExitReason::Succeed(_) => {
                    log!(
                        self.host,
                        Debug,
                        "Intermediate transaction ended with: {:?}",
                        exit_reason
                    );

                    if let Some(new_address) = new_address {
                        self.created_contracts.insert(new_address);
                    }

                    if let Err(err) = self.commit_inter_transaction() {
                        log!(
                            self.host,
                            Debug,
                            "Committing intermediate transaction caused an error: {:?}",
                            err
                        );

                        Capture::Exit((ethereum_error_to_exit_reason(&err), None, vec![]))
                    } else {
                        Capture::Exit(execution_result.unwrap()) // safe unwrap
                    }
                }
                ExitReason::Revert(_) => {
                    log!(
                        self.host,
                        Debug,
                        "Intermediate transaction reverted with: {:?}",
                        exit_reason
                    );

                    Self::rollback_inter_transaction_side_effect(
                        self,
                        execution_result.unwrap(), // safe unwrap
                        true,
                    )
                }
                ExitReason::Error(_) => {
                    log!(
                        self.host,
                        Debug,
                        "Intermediate transaction produced the following error: {:?}",
                        exit_reason
                    );

                    Self::rollback_inter_transaction_side_effect(
                        self,
                        execution_result.unwrap(), // safe unwrap
                        false,
                    )
                }
                ExitReason::Fatal(ExitFatal::CallErrorAsFatal(
                    ExitError::CreateContractLimit,
                )) => {
                    // For more context for why we need this specific case and behaviour
                    // look out for [MAX_INIT_CODE_SIZE_RETURN_HACK] in this file.

                    let create_contract_limit =
                        ExitReason::Error(ExitError::CreateContractLimit);
                    let execution_result = execution_result.unwrap(); // safe unwrap

                    log!(
                        self.host,
                        Debug,
                        "Intermediate transaction produced the following error: {:?}",
                        create_contract_limit
                    );

                    Self::rollback_inter_transaction_side_effect(
                        self,
                        (
                            create_contract_limit,
                            execution_result.1,
                            execution_result.2,
                        ),
                        false,
                    )
                }
                ExitReason::Fatal(_) => {
                    log!(
                        self.host,
                        Debug,
                        "Intermediate transaction produced the following fatal error: {:?}",
                        exit_reason
                    );

                    Self::rollback_inter_transaction_side_effect(
                        self,
                        execution_result.unwrap(), // safe unwrap
                        false,
                    )
                }
            },
            Err(EthereumError::PrecompileFailed(failure_reason)) => {
                // We need this case, otherwise the failure will be considered as fatal
                // when it shouldn't.

                log!(
                    self.host,
                    Debug,
                    "Intermediate precompiled call ended with failure: {:?}",
                    failure_reason
                );

                if let Err(err) = self.rollback_inter_transaction(false) {
                    log!(
                        self.host,
                        Debug,
                        "Rolling back reverted transaction caused an error: {:?}",
                        err
                    );
                }

                Capture::Exit((
                    ExitReason::Error(ExitError::Other(Cow::Owned(format!(
                        "{:?}",
                        EthereumError::PrecompileFailed(failure_reason)
                    )))),
                    None,
                    vec![],
                ))
            }
            Err(err) => {
                log!(
                    self.host,
                    Debug,
                    "Intermediate transaction ended in error: {:?}",
                    err
                );

                if let Err(err) = self.rollback_inter_transaction(false) {
                    log!(
                        self.host,
                        Debug,
                        "Rolling back reverted transaction caused an error: {:?}",
                        err
                    );
                }

                Capture::Exit((ethereum_error_to_exit_reason(&err), None, vec![]))
            }
        }
    }

    pub fn nested_call_gas_limit(&mut self, target_gas: Option<u64>) -> Option<u64> {
        // Part of EIP-150: https://github.com/ethereum/EIPs/blob/master/EIPS/eip-150.md
        let gas_remaining = self.gas_remaining();
        let max_gas_limit = if self.config.call_l64_after_gas {
            gas_remaining - gas_remaining / 64
        } else {
            gas_remaining
        };
        if let Some(gas) = target_gas {
            Some(min(gas, max_gas_limit))
        } else {
            Some(max_gas_limit)
        }
    }

    fn output_for_inter_create(&self, reason: &ExitReason, output: Vec<u8>) -> Vec<u8> {
        match reason {
            ExitReason::Error(_) | ExitReason::Revert(_) | ExitReason::Fatal(_) => output,
            ExitReason::Succeed(_) => vec![],
        }
    }

    /// Test helper used to demonstrate that reentrancy guard is actually the exit reason.
    #[cfg(test)]
    pub(crate) fn disable_reentrancy_guard(&mut self) {
        self.reentrancy_guard.disable();
    }

    fn update_contract_cache(&mut self, address: H160, code: &[u8]) -> H256 {
        let hash = bytes_hash(code);
        let code = code.to_vec();
        self.contract_cache.insert(address, Contract { code, hash });
        hash
    }

    fn get_contract(&mut self, address: H160) -> Contract {
        if let Some(contract) = self.contract_cache.get(&address) {
            Contract {
                code: contract.code.to_vec(),
                hash: contract.hash,
            }
        } else {
            let code = self
                .get_account(address)
                .ok()
                .flatten()
                .and_then(|a| a.code(self.host).ok());

            match code {
                Some(code) => {
                    let hash = self.update_contract_cache(address, &code);
                    Contract {
                        code: code.to_vec(),
                        hash,
                    }
                }
                None => Contract::default(),
            }
        }
    }

    fn clear_internal_caches(&mut self) {
        self.storage_cache.clear();
        self.transient_storage.clear();
        self.original_storage_cache.clear();
        self.contract_cache.clear();
    }
}

fn update_cache<Host: Runtime>(
    handler: &mut EvmHandler<'_, Host>,
    address: H160,
    index: H256,
    value: CacheStorageValue,
    layer: usize,
) {
    if let Some(cache) = handler.storage_cache.get_mut(&layer) {
        cache.insert(StorageKey { address, index }, value);
    } else {
        let mut cache = HashMap::new();
        cache.insert(StorageKey { address, index }, value);
        handler.storage_cache.insert(layer, cache);
    }
}

fn find_storage_key<Host: Runtime>(
    handler: &mut EvmHandler<'_, Host>,
    address: H160,
    index: H256,
    current_layer: usize,
) -> Option<H256> {
    for layer in (0..=current_layer).rev() {
        if let Some(cache) = handler.storage_cache.get(&layer) {
            if let Some(value) = cache.get(&StorageKey { address, index }) {
                return Some(value.h256());
            }
        }
    }
    None
}

/// Committing the storage cache means that the current layer changes
/// are propagated to the previous one and the current layer is popped.
fn commit_storage_cache<Host: Runtime>(
    handler: &mut EvmHandler<'_, Host>,
    current_layer: usize,
) {
    let commit_layer = current_layer - 1;
    if let Some(cache) = handler.storage_cache.remove(&current_layer) {
        if let Some(prev_layer_cache) = handler.storage_cache.get_mut(&commit_layer) {
            prev_layer_cache.extend(cache);
        } else {
            handler.storage_cache.insert(commit_layer, cache);
        }
    }
}

fn commit_transient_storage<Host: Runtime>(
    handler: &mut EvmHandler<'_, Host>,
    current_layer: usize,
) {
    let commit_layer = current_layer - 1;
    if let Some(t_storage) = handler.transient_storage.remove(&current_layer) {
        if let Some(prev_t_storage) = handler.transient_storage.get_mut(&commit_layer) {
            prev_t_storage.extend(t_storage);
        } else {
            handler.transient_storage.insert(commit_layer, t_storage);
        }
    }
}

fn cached_storage_access<Host: Runtime>(
    handler: &mut EvmHandler<'_, Host>,
    address: H160,
    index: H256,
    layer: usize,
) -> H256 {
    if let Some(value) = find_storage_key(handler, address, index, layer) {
        value
    } else {
        let value = handler
            .get_account(address)
            .ok()
            .flatten()
            .and_then(|a| a.read_storage(handler.host, &index).ok());

        // This condition will help avoiding unecessary write access
        // in the durable storage at the end of the transaction.
        if let Some(value) = value {
            update_cache(
                handler,
                address,
                index,
                CacheStorageValue::Read(value),
                layer,
            );
        }

        value.map(|x| x.h256()).unwrap_or_default()
    }
}

#[allow(clippy::too_many_arguments)]
pub fn trace_call<Host: Runtime>(
    handler: &mut EvmHandler<Host>,
    call_scheme: CallScheme,
    caller: H160,
    value: U256,
    gas_used: u64,
    input: Vec<u8>,
    context_address: H160,
    code_address: H160,
    target_gas: Option<u64>,
    output: &[u8],
    reason: &ExitReason,
) {
    if let Some(CallTracer(CallTracerInput {
        transaction_hash,
        config:
            CallTracerConfig {
                with_logs,
                only_top_call: false,
            },
    })) = handler.tracer
    {
        let (type_, from) = match call_scheme {
            CallScheme::Call => ("CALL", caller),
            CallScheme::StaticCall => ("STATICCALL", caller),
            CallScheme::DelegateCall => {
                // FIXME: #7738 this only point to parent call
                // address if it was not a DELEGATECALL or
                // CALLCODE itself
                ("DELEGATECALL", context_address)
            }
            CallScheme::CallCode => {
                // FIXME: #7738 this only point to parent call
                // address if it was not a DELEGATECALL or
                // CALLCODE itself
                ("CALLCODE", context_address)
            }
        };
        let mut call_trace = CallTrace::new_minimal_trace(
            type_.into(),
            from,
            value,
            gas_used,
            input,
            // We need to make the distinction between the initial call (depth 0)
            // and the other subcalls
            (handler.stack_depth() + 1).try_into().unwrap_or_default(),
        );

        // for the trace we want the contract address to always be the "to"
        // field, not necessarily the address used in the transition context
        // which may be something else (eg DELEGATECALL)
        call_trace.add_to(Some(code_address));
        call_trace.add_gas(target_gas);
        call_trace.add_output(Some(output.to_owned()));

        // TODO: https://gitlab.com/tezos/tezos/-/issues/7437
        // For errors and revert reasons, find the appropriate values
        // to return for tracing. The following values are kind of placeholders.
        match reason {
            ExitReason::Succeed(_) => (),
            ExitReason::Error(e) => call_trace.add_error(Some(format!("{:?}", e).into())),
            ExitReason::Revert(r) => {
                call_trace.add_error(Some(format!("{:?}", r).into()))
            }
            ExitReason::Fatal(f) => call_trace.add_error(Some(format!("{:?}", f).into())),
        };

        if with_logs {
            call_trace.add_logs(
                handler
                    .transaction_data
                    .last()
                    .map(|tx_layer| tx_layer.logs.clone()),
            )
        }
        let _ = tracer::store_call_trace(handler.host, call_trace, &transaction_hash);
    }
}

/// SELFDESTRUCT implementation prior to EIP-6780.
/// See https://eips.ethereum.org/EIPS/eip-6780.
fn mark_delete_legacy<Host: Runtime>(
    handler: &mut EvmHandler<'_, Host>,
    address: H160,
    target: H160,
) -> Result<(), ExitError> {
    let new_deletion = match handler.transaction_data.last_mut() {
        Some(top_layer) => Ok(top_layer.deleted_contracts.insert(address)),
        None => Err(ExitError::Other(Cow::from(
            "No transaction data for delete",
        ))),
    }?;
    if new_deletion && address == target {
        handler.reset_balance(address).map_err(|_| {
            ExitError::Other(Cow::from("Could not reset balance when deleting contract"))
        })
    } else if new_deletion {
        let balance = handler.balance(address);

        handler
            .execute_transfer(address, target, balance)
            .map_err(|_| {
                ExitError::Other(Cow::from(
                    "Could not execute transfer on contract delete",
                ))
            })?;
        Ok(())
    } else {
        log!(handler.host, Debug, "Contract already marked to delete");
        Ok(())
    }
}

/// SELFDESTRUCT implementation after EIP-6780.
/// See https://eips.ethereum.org/EIPS/eip-6780.
fn mark_delete_eip6780<Host: Runtime>(
    handler: &mut EvmHandler<'_, Host>,
    address: H160,
    target: H160,
) -> Result<(), ExitError> {
    if address != target {
        let balance = handler.balance(address);

        handler
            .execute_transfer(address, target, balance)
            .map_err(|_| {
                ExitError::Other(Cow::from(
                    "Could not execute transfer on contract delete",
                ))
            })?;
    }

    match handler.transaction_data.last_mut() {
        Some(top_layer) => {
            if top_layer.call_context.is_creation {
                top_layer.deleted_contracts.insert(address);
                handler.reset_balance(address).map_err(|_| {
                    ExitError::Other(Cow::from(
                        "Could not reset balance when deleting contract",
                    ))
                })?;
            }
            Ok(())
        }
        None => Err(ExitError::Other(Cow::from(
            "No transaction data for delete",
        ))),
    }
}

#[allow(unused_variables)]
impl<Host: Runtime> Handler for EvmHandler<'_, Host> {
    type CreateInterrupt = Infallible;
    type CreateFeedback = Infallible;
    type CallInterrupt = Infallible;
    type CallFeedback = Infallible;

    fn balance(&self, address: H160) -> U256 {
        self.get_account(address)
            .ok()
            .flatten()
            .and_then(|a| a.balance(self.host).ok())
            .unwrap_or_default()
    }

    fn code_size(&mut self, address: H160) -> U256 {
        let code = self.get_contract(address).code;
        U256::from(code.len())
    }

    // Hash of the chosen account's code, the empty hash (CODE_HASH_DEFAULT) if the account has no code,
    // or 0 if the account does not exist or has been destroyed.
    fn code_hash(&mut self, address: H160) -> H256 {
        if !self.exists(address) {
            return H256::zero();
        }

        self.get_contract(address).hash
    }

    fn code(&mut self, address: H160) -> Vec<u8> {
        self.get_contract(address).code
    }

    fn storage(&mut self, address: H160, index: H256) -> H256 {
        let layer = self.evm_account_storage.stack_depth();
        cached_storage_access(self, address, index, layer)
    }

    fn transient_storage(&self, address: H160, index: H256) -> H256 {
        let layer = self.evm_account_storage.stack_depth();
        for layer in (0..=layer).rev() {
            if let Some(t_storage) = self.transient_storage.get(&layer) {
                if let Some(value) = t_storage.get(&StorageKey { address, index }) {
                    return *value;
                }
            }
        }
        H256::zero()
    }

    fn original_storage(&mut self, address: H160, index: H256) -> H256 {
        let key = StorageKey { address, index };
        if let Some(value) = self.original_storage_cache.get(&key) {
            value.h256()
        } else {
            let value = self
                .get_original_account(address)
                .ok()
                .flatten()
                .and_then(|a| a.get_storage(self.host, &index).ok())
                .unwrap_or_default();

            self.original_storage_cache
                .insert(key, CacheStorageValue::Read(StorageValue::Hit(value)));

            value
        }
    }

    fn gas_left(&self) -> U256 {
        self.gas_remaining().into()
    }

    fn gas_price(&self) -> U256 {
        self.effective_gas_price
    }

    fn origin(&self) -> H160 {
        self.origin
    }

    fn block_hash(&self, number: U256) -> H256 {
        // return 0 when block number not in valid range
        // Ref. https://www.evm.codes/#40?fork=shanghai (opcode 0x40)

        match self.block.number.checked_sub(number) {
            Some(block_diff)
                if block_diff <= U256::from(BLOCKS_STORED)
                    && block_diff != U256::zero() =>
            {
                get_block_hash(self.host, number).unwrap_or_default()
            }
            _ => H256::zero(),
        }
    }

    fn block_number(&self) -> U256 {
        self.block.number
    }

    fn block_coinbase(&self) -> H160 {
        self.block.coinbase
    }

    fn block_timestamp(&self) -> U256 {
        self.block.timestamp
    }

    fn block_difficulty(&self) -> U256 {
        // There's no difficulty in the blocks
        // A default value is returned here
        U256::zero()
    }

    fn block_gas_limit(&self) -> U256 {
        self.block.gas_limit.into()
    }

    fn block_base_fee_per_gas(&self) -> U256 {
        self.block.base_fee_per_gas()
    }

    fn blob_hash(&self, _index: H256) -> H256 {
        // Etherlink doesn't support blob as defined by
        // EIP-4844 (Proto-Danksharding).
        // As such, the following value will always return
        // zero.
        H256::zero()
    }

    fn block_blob_base_fee(&self) -> U256 {
        self.block.blob_base_fee()
    }

    fn block_randomness(&self) -> Option<H256> {
        self.block.prevrandao // Always None
    }

    fn chain_id(&self) -> U256 {
        self.block.chain_id
    }

    fn exists(&mut self, address: H160) -> bool {
        self.code_size(address) > U256::zero()
            || self.get_nonce(address).unwrap_or_default() > 0
            || self.balance(address) > U256::zero()
    }

    fn deleted(&self, address: H160) -> bool {
        for data in &self.transaction_data {
            if data.deleted_contracts.contains(&address) {
                return true;
            }
        }

        false
    }

    fn is_cold(&mut self, address: H160, index: Option<H256>) -> Result<bool, ExitError> {
        // EIP-3651
        if self.config.warm_coinbase_address && address == self.block_coinbase() {
            return Ok(false);
        }

        match index {
            Some(index) => {
                let is_cold = self.is_storage_hot(address, index).map(|x| !x);
                if let Ok(true) = is_cold {
                    self.mark_storage_as_hot(address, index)?;
                }
                is_cold
            }
            None => {
                if self.precompiles.is_precompile(address) {
                    Ok(false)
                } else {
                    let is_cold = self.is_address_hot(address).map(|x| !x);
                    if let Ok(true) = is_cold {
                        self.mark_address_as_hot(address)?;
                    }
                    is_cold
                }
            }
        }
    }

    fn set_storage(
        &mut self,
        address: H160,
        index: H256,
        value: H256,
    ) -> Result<(), ExitError> {
        let layer = self.evm_account_storage.stack_depth();
        update_cache(self, address, index, CacheStorageValue::Write(value), layer);
        Ok(())
    }

    fn set_transient_storage(
        &mut self,
        address: H160,
        index: H256,
        value: H256,
    ) -> Result<(), ExitError> {
        if self.is_static() {
            return Err(ExitError::Other(Cow::from(
                "TSTORE cannot be executed inside a static call",
            )));
        }

        let layer = self.evm_account_storage.stack_depth();
        if let Some(t_storage) = self.transient_storage.get_mut(&layer) {
            t_storage.insert(StorageKey { address, index }, value);
        } else {
            let mut t_storage = HashMap::new();
            t_storage.insert(StorageKey { address, index }, value);
            self.transient_storage.insert(layer, t_storage);
        }
        Ok(())
    }

    fn log(
        &mut self,
        address: H160,
        topics: Vec<H256>,
        data: Vec<u8>,
    ) -> Result<(), ExitError> {
        self.add_log(Log {
            address,
            topics,
            data,
        })
    }

    fn mark_delete(&mut self, address: H160, target: H160) -> Result<(), ExitError> {
        // To comply with EIP-6780, if the opcode is considered not deprecated or if
        // the address where the SELFDESTRUCT is called was just created, we keep
        // the legacy behavior, otherwise we use the "deprecated" behavior.
        if !self.config.selfdestruct_deprecated || self.was_created(&address) {
            mark_delete_legacy(self, address, target)
        } else {
            mark_delete_eip6780(self, address, target)
        }
    }

    fn create(
        &mut self,
        caller: H160,
        scheme: CreateScheme,
        value: U256,
        init_code: Vec<u8>,
        target_gas: Option<u64>,
    ) -> Capture<CreateOutcome, Self::CreateInterrupt> {
        // We check that the maximum allowed init code size as specified by EIP-3860
        // can not be reached.
        if let Some(max_initcode_size) = self.config.max_initcode_size {
            if init_code.len() > max_initcode_size {
                // [MAX_INIT_CODE_SIZE_RETURN_HACK]
                // The normal behavior stated by https://www.evm.codes/#f0?fork=shanghai
                // would be to return a simple error.
                // « Error cases: [..]
                //  * size is greater than the chain's maximum initcode size (since Shanghai fork) »
                //
                // Unfortunately there is a bug in [evm-runtime-0.39.0] where the `finish_create`
                // function will always consider error/revert/succed as a "Ok(()) => Control::Continue"
                // flow which makes it that we can not rollback anything as it should in this case.
                // The hack-ish way to be able to capture that error and rollback as it should is
                // to consider this error as fatal and then catch it in `end_inter_transaction`,
                // rollback what needs to be and then transform the outputed fatal error to a simple
                // `ExitReason::Error(ExitError::CreateContractLimit)`.

                return Capture::Exit((
                    ExitReason::Fatal(ExitFatal::CallErrorAsFatal(
                        ExitError::CreateContractLimit,
                    )),
                    None,
                    vec![],
                ));
            }
        }

        if let Err(err) = self.record_init_code_cost(&init_code) {
            log!(self.host, Debug, "{:?}: Cannot record init code cost.", err);

            return Capture::Exit((ExitReason::Error(ExitError::OutOfGas), None, vec![]));
        }

        match self.can_begin_inter_transaction(caller, &value) {
            Precondition::PassPrecondition => {
                // The contract address is created before the increment of the nonce
                // to generate a correct address when the scheme is `Legacy`.
                let contract_address = match self.create_address(scheme) {
                    Ok(address) => address,
                    Err(err) => {
                        return Capture::Exit((
                            ethereum_error_to_exit_reason(&err.into()),
                            None,
                            vec![],
                        ));
                    }
                };

                // This `mark_address_as_hot` must be before the `begin_inter_transaction`
                // so the address will still be hot even if the creation fails
                if self.mark_address_as_hot(contract_address).is_err() {
                    let err = EthereumError::InconsistentState(Cow::from(
                        "Failed to mark callee address as hot",
                    ));
                    return Capture::Exit((
                        ethereum_error_to_exit_reason(&err),
                        None,
                        vec![],
                    ));
                }

                // The nonce of the caller is incremented before the internal tx
                // Even if the internal transaction rollback the nonce will not
                if let Err(err) = self.increment_nonce(caller) {
                    log!(
                        self.host,
                        Debug,
                        "Failed to increment nonce of {:?}",
                        caller
                    );

                    return Capture::Exit((
                        ethereum_error_to_exit_reason(&err),
                        None,
                        vec![],
                    ));
                }

                let gas_limit = self.nested_call_gas_limit(target_gas);

                if let Err(err) = self.record_cost(gas_limit.unwrap_or_default()) {
                    log!(
                        self.host,
                        Debug,
                        "Not enough gas for create. Required at least: {:?}",
                        gas_limit
                    );

                    return Capture::Exit((
                        ExitReason::Error(ExitError::OutOfGas),
                        None,
                        vec![],
                    ));
                }

                match self.begin_inter_transaction(
                    CallContext {
                        is_static: false,
                        is_creation: true,
                    },
                    gas_limit,
                ) {
                    Ok(()) => {
                        let gas_before = self.gas_used();
                        let result = self.execute_create(
                            caller,
                            value,
                            init_code.clone(),
                            contract_address,
                        );
                        let gas_after = self.gas_used();

                        match self.end_inter_transaction(result) {
                            Capture::Exit((reason, address, output)) => {
                                // TRACING
                                if let Some(CallTracer(CallTracerInput {
                                    transaction_hash,
                                    config:
                                        CallTracerConfig {
                                            with_logs,
                                            only_top_call: false,
                                        },
                                })) = self.tracer
                                {
                                    let mut call_trace = CallTrace::new_minimal_trace(
                                        if let CreateScheme::Create2 { .. } = scheme {
                                            "CREATE2"
                                        } else {
                                            "CREATE"
                                        }
                                        .into(),
                                        caller,
                                        value,
                                        gas_after - gas_before,
                                        init_code,
                                        // We need to make the distinction between the initial call (depth 0)
                                        // and the other subcalls
                                        (self.stack_depth() + 1)
                                            .try_into()
                                            .unwrap_or_default(),
                                    );

                                    call_trace.add_to(address);
                                    call_trace.add_output(Some(output.to_owned()));
                                    call_trace.add_gas(target_gas);

                                    // TODO: https://gitlab.com/tezos/tezos/-/issues/7437
                                    // For errors and revert reasons, find the appropriate values
                                    // to return for tracing. The following values are kind of placeholders.
                                    match &reason {
                                        ExitReason::Error(e) => call_trace
                                            .add_error(Some(format!("{:?}", e).into())),
                                        ExitReason::Revert(r) => call_trace
                                            .add_error(Some(format!("{:?}", r).into())),
                                        ExitReason::Fatal(f) => call_trace
                                            .add_error(Some(format!("{:?}", f).into())),
                                        ExitReason::Succeed(_) => (),
                                    };

                                    if with_logs {
                                        call_trace.add_logs(
                                            self.transaction_data
                                                .last()
                                                .map(|tx_layer| tx_layer.logs.clone()),
                                        )
                                    }

                                    let _ = tracer::store_call_trace(
                                        self.host,
                                        call_trace,
                                        &transaction_hash,
                                    );
                                }
                                let output =
                                    self.output_for_inter_create(&reason, output);
                                Capture::Exit((reason, address, output))
                            }
                            Capture::Trap(x) => Capture::Trap(x),
                        }
                    }
                    Err(err) => {
                        log!(
                            self.host,
                            Debug,
                            "Intermediate transaction failed, reason: {:?}",
                            err
                        );

                        Capture::Exit((ethereum_error_to_exit_reason(&err), None, vec![]))
                    }
                }
            }
            Precondition::PreconditionErr(exit_reason) => {
                Capture::Exit((exit_reason, None, vec![]))
            }
            Precondition::EthereumErr(err) => {
                Capture::Exit((ethereum_error_to_exit_reason(&err), None, vec![]))
            }
        }
    }

    fn call(
        &mut self,
        code_address: H160,
        transfer: Option<Transfer>,
        input: Vec<u8>,
        target_gas: Option<u64>,
        call_scheme: CallScheme,
        context: Context,
    ) -> Capture<CallOutcome, Self::CallInterrupt> {
        let transaction_context = TransactionContext::from_context(context);
        let caller = transaction_context.context.caller;

        // Retrieve value from `Transfer` struct to check if caller has enough balance
        let value = match transfer {
            None => U256::zero(),
            Some(Transfer { value, .. }) => value,
        };

        match self.can_begin_inter_transaction(caller, &value) {
            Precondition::PassPrecondition => {
                let mut gas_limit = self.nested_call_gas_limit(target_gas);

                if let Err(err) = self.record_cost(gas_limit.unwrap_or_default()) {
                    log!(
                        self.host,
                        Debug,
                        "Not enough gas for call. Required at least: {:?}",
                        gas_limit
                    );

                    return Capture::Exit((
                        ExitReason::Error(ExitError::OutOfGas),
                        vec![],
                    ));
                }

                // For call with transfer value > 0, a stipend is added to the gaslimit.
                // see yellowpaper, appendix H, opcode CALL (0xf1) and CALLCODE (Oxf2)
                // Note that for other CALL* opcodes sputnik will not add a transfer at all.
                if value > U256::zero() {
                    gas_limit =
                        gas_limit.map(|v| v.saturating_add(self.config.call_stipend));
                }

                if let Err(err) = self.begin_inter_transaction(
                    CallContext {
                        is_static: call_scheme == CallScheme::StaticCall,
                        is_creation: false,
                    },
                    gas_limit,
                ) {
                    return Capture::Exit((ethereum_error_to_exit_reason(&err), vec![]));
                }

                let gas_before = self.gas_used();
                let context_address = transaction_context.context.address;
                let result = self.execute_call(
                    code_address,
                    transfer,
                    input.clone(),
                    transaction_context.clone(),
                );
                let gas_after = self.gas_used();

                match self.end_inter_transaction(result) {
                    Capture::Exit((reason, _, output)) => {
                        log!(self.host, Debug, "Call ended with reason: {:?}", reason);

                        // TRACING
                        trace_call(
                            self,
                            call_scheme,
                            caller,
                            value,
                            gas_after - gas_before,
                            input,
                            context_address,
                            code_address,
                            target_gas,
                            &output,
                            &reason,
                        );

                        Capture::Exit((reason, output))
                    }
                    Capture::Trap(x) => Capture::Trap(x),
                }
            }
            Precondition::PreconditionErr(exit_reason) => {
                if value > U256::zero() {
                    if let Err(err) = self.record_stipend(self.config.call_stipend) {
                        return Capture::Exit((
                            ethereum_error_to_exit_reason(&err),
                            vec![],
                        ));
                    }
                }
                Capture::Exit((exit_reason, vec![]))
            }
            Precondition::EthereumErr(err) => {
                Capture::Exit((ethereum_error_to_exit_reason(&err), vec![]))
            }
        }
    }

    fn pre_validate(
        &mut self,
        context: &Context,
        opcode: Opcode,
        stack: &Stack,
    ) -> Result<(), ExitError> {
        if let Some(cost) = evm::gasometer::static_opcode_cost(opcode) {
            self.record_cost(cost)
        } else {
            let (cost, _target, memory_cost) = evm::gasometer::dynamic_opcode_cost(
                context.address,
                opcode,
                stack,
                self.is_static(),
                self.config,
                self,
            )?;

            self.record_dynamic_cost(cost, memory_cost)
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::account_storage::{init_account_storage, CODE_HASH_DEFAULT};
    use crate::configuration::EVMVersion;
    use crate::precompiles;
    use evm::Config;
    use pretty_assertions::assert_eq;
    use primitive_types::{H160, H256};
    use std::cmp::Ordering;
    use std::str::FromStr;
    use std::vec;
    use tezos_ethereum::block::BlockFees;
    use tezos_evm_runtime::runtime::MockKernelHost;

    fn set_code(
        handler: &mut EvmHandler<'_, MockKernelHost>,
        address: &H160,
        code: Vec<u8>,
    ) {
        handler.update_contract_cache(*address, &code);
        let mut account = handler.get_or_create_account(*address).unwrap();
        account.delete_code(handler.borrow_host()).unwrap(); //first clean code if it exists.
        account.set_code(handler.borrow_host(), &code).unwrap();
    }

    fn set_nonce(
        handler: &mut EvmHandler<'_, MockKernelHost>,
        address: &H160,
        nonce: u64,
    ) {
        let mut account = handler.get_or_create_account(*address).unwrap();
        account.set_nonce(handler.borrow_host(), nonce).unwrap()
    }

    fn get_balance(handler: &mut EvmHandler<'_, MockKernelHost>, address: &H160) -> U256 {
        let account = handler.get_or_create_account(*address).unwrap();
        account.balance(handler.borrow_host()).unwrap()
    }

    fn set_balance(
        handler: &mut EvmHandler<'_, MockKernelHost>,
        address: &H160,
        new_balance: U256,
    ) {
        let mut account = handler.get_or_create_account(*address).unwrap();
        let old_balance = account.balance(handler.borrow_host()).unwrap();
        match old_balance.cmp(&new_balance) {
            Ordering::Greater => {
                // we require that fund removal goes fine
                assert!(
                    account
                        .balance_remove(handler.borrow_host(), old_balance - new_balance)
                        .unwrap(),
                    "Could not set balance of account"
                )
            }
            Ordering::Less => account
                .balance_add(handler.borrow_host(), new_balance - old_balance)
                .unwrap(),
            Ordering::Equal => (),
        }
    }

    fn get_durable_slot(
        handler: &mut EvmHandler<'_, MockKernelHost>,
        address: &H160,
        index: &H256,
    ) -> H256 {
        let layer = handler.evm_account_storage.stack_depth();
        cached_storage_access(handler, *address, *index, layer)
    }

    fn dummy_first_block() -> BlockConstants {
        let block_fees = BlockFees::new(
            U256::one(),
            U256::from(12345),
            U256::from(2_000_000_000_000u64),
        );
        BlockConstants::first_block(
            U256::zero(),
            U256::one(),
            block_fees,
            u64::MAX,
            H160::zero(),
        )
    }

    #[test]
    fn legacy_create_to_correct_address() {
        let mut mock_runtime = MockKernelHost::default();
        let block = dummy_first_block();
        let precompiles = precompiles::precompile_set::<MockKernelHost>(false);
        let mut evm_account_storage = init_account_storage().unwrap();
        let config = EVMVersion::current_test_config();

        let gas_price = U256::from(21000);

        // This is a randomly generated address. It has been used for testing legacy address
        // generation with zero nonce using Ethereum. To replicate (with new address):
        // - generate a fresh Ethereum account (on Rinkeby or other test net)
        // - make sure it has eth (transfer from faucet)
        // - check nonce is zero (or bump nonce accordingly below)
        // - create a new contract. Any contract will do.
        // - check address of new contract - it is `expected_result` below.
        let caller: H160 =
            H160::from_str("9bbfed6889322e016e0a02ee459d306fc19545d8").unwrap();

        let mut handler = EvmHandler::new(
            &mut mock_runtime,
            &mut evm_account_storage,
            caller,
            &block,
            &config,
            &precompiles,
            gas_price,
            None,
        );

        let result = handler
            .create_address(CreateScheme::Legacy { caller })
            .unwrap_or_default();

        let expected_result: H160 =
            H160::from_str("43a61f3f4c73ea0d444c5c1c1a8544067a86219b").unwrap();

        assert_eq!(result, expected_result);
    }

    #[test]
    fn create2_to_correct_address() {
        let mut mock_runtime = MockKernelHost::default();
        let block = dummy_first_block();
        let precompiles = precompiles::precompile_set::<MockKernelHost>(false);
        let mut evm_account_storage = init_account_storage().unwrap();
        let config = EVMVersion::current_test_config();

        let caller: H160 =
            H160::from_str("9bbfed6889322e016e0a02ee459d306fc19545d8").unwrap();

        let gas_price = U256::from(21000);

        let mut handler = EvmHandler::new(
            &mut mock_runtime,
            &mut evm_account_storage,
            caller,
            &block,
            &config,
            &precompiles,
            gas_price,
            None,
        );

        let code_hash: H256 = CODE_HASH_DEFAULT;
        let salt: H256 = H256::zero();

        let result = handler
            .create_address(CreateScheme::Create2 {
                caller,
                code_hash,
                salt,
            })
            .unwrap_or_default();

        let expected_result: H160 =
            H160::from_str("0687a12da0ffa0a64a28c9512512b8ae8870b7ea").unwrap();

        assert_eq!(result, expected_result);
    }

    #[test]
    fn create2_to_correct_address_nonzero_salt() {
        let mut mock_runtime = MockKernelHost::default();
        let block = dummy_first_block();
        let precompiles = precompiles::precompile_set::<MockKernelHost>(false);
        let mut evm_account_storage = init_account_storage().unwrap();
        let config = EVMVersion::current_test_config();

        let gas_price = U256::from(21000);

        let caller: H160 =
            H160::from_str("9bbfed6889322e016e0a02ee459d306fc19545d8").unwrap();

        let mut handler = EvmHandler::new(
            &mut mock_runtime,
            &mut evm_account_storage,
            caller,
            &block,
            &config,
            &precompiles,
            gas_price,
            None,
        );

        let code_hash: H256 = CODE_HASH_DEFAULT;
        let salt: H256 = H256::from_str(
            "0000000000000000000000000000000000000000000000000000000000000001",
        )
        .unwrap();

        let result = handler
            .create_address(CreateScheme::Create2 {
                caller,
                code_hash,
                salt,
            })
            .unwrap_or_default();

        let expected_result: H160 =
            H160::from_str("dbd0b036a125995a83d0ab020656a8355abac612").unwrap();

        assert_eq!(result, expected_result);
    }

    #[test]
    fn origin_instruction_returns_origin_address() {
        let mut mock_runtime = MockKernelHost::default();
        let block = dummy_first_block();
        let precompiles = precompiles::precompile_set::<MockKernelHost>(false);
        let mut evm_account_storage = init_account_storage().unwrap();
        let config = EVMVersion::current_test_config();

        let caller = H160::from_low_u64_be(28349_u64);

        // We use an origin distinct from caller for testing purposes
        let origin = H160::from_low_u64_be(117_u64);

        let gas_price = U256::from(21000);

        let mut handler = EvmHandler::new(
            &mut mock_runtime,
            &mut evm_account_storage,
            origin,
            &block,
            &config,
            &precompiles,
            gas_price,
            None,
        );

        let address = H160::from_low_u64_be(213_u64);
        let input = vec![0_u8];
        let transaction_context = TransactionContext::new(caller, address, U256::zero());
        let transfer: Option<Transfer> = None;
        let code: Vec<u8> = vec![
            Opcode::ORIGIN.as_u8(), // Push the 32(!) byte origin on to stack (this is "the value")
            Opcode::PUSH1.as_u8(), // Push a zero valued word onto stack (this is "the address")
            0_u8,
            Opcode::MSTORE.as_u8(), // Store "the value" at "the address"
            Opcode::PUSH1.as_u8(),  // Push value 2 onto stack - this is "number of bytes"
            32_u8,
            Opcode::PUSH1.as_u8(), // Push value 0 onto stack - this is "the address" again
            0_u8,
            Opcode::RETURN.as_u8(), // Return "number of bytes" at "the address" in the RETURNBUFFER
        ];

        set_code(&mut handler, &address, code);

        handler
            .begin_initial_transaction(
                CallContext {
                    is_static: false,
                    is_creation: false,
                },
                None,
            )
            .unwrap();

        let result = handler.execute_call(address, transfer, input, transaction_context);

        match result {
            Ok(result) => {
                let expected_result = (
                    ExitReason::Succeed(ExitSucceed::Returned),
                    None,
                    H256::from(origin).0.to_vec(),
                );
                assert_eq!(result, expected_result);
                assert_eq!(handler.gas_used(), 0);
            }
            Err(err) => {
                panic!("Expected Ok, but got {:?}", err);
            }
        }
    }

    #[test]
    fn contract_call_produces_correct_output() {
        let mut mock_runtime = MockKernelHost::default();
        let block = dummy_first_block();
        let precompiles = precompiles::precompile_set::<MockKernelHost>(false);
        let mut evm_account_storage = init_account_storage().unwrap();
        let config = EVMVersion::current_test_config();

        let caller = H160::from_low_u64_be(28349_u64);

        let gas_price = U256::from(21000);

        let mut handler = EvmHandler::new(
            &mut mock_runtime,
            &mut evm_account_storage,
            caller,
            &block,
            &config,
            &precompiles,
            gas_price,
            None,
        );

        let address = H160::from_low_u64_be(213_u64);
        let input = vec![0_u8];
        let transaction_context = TransactionContext::new(caller, address, U256::zero());
        let transfer: Option<Transfer> = None;
        let code: Vec<u8> = vec![
            Opcode::PUSH32.as_u8(), // Push a 32 byte word onto stack (this is "the value")
            0xFF_u8,
            1,
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            Opcode::PUSH1.as_u8(), // Push a zero valued word onto stack (this is "the address")
            0_u8,
            Opcode::MSTORE.as_u8(), // Store "the value" at "the address"
            Opcode::PUSH1.as_u8(),  // Push value 2 onto stack - this is "number of bytes"
            2_u8,
            Opcode::PUSH1.as_u8(), // Push value 0 onto stack - this is "the address" again
            0_u8,
            Opcode::RETURN.as_u8(), // Return "number of bytes" at "the address" in the RETURNBUFFER
        ];

        set_code(&mut handler, &address, code);

        handler
            .begin_initial_transaction(
                CallContext {
                    is_static: false,
                    is_creation: false,
                },
                None,
            )
            .unwrap();

        let result = handler.execute_call(address, transfer, input, transaction_context);

        match result {
            Ok(result) => {
                let expected_result = (
                    ExitReason::Succeed(ExitSucceed::Returned),
                    None,
                    vec![0xFF_u8, 0x01_u8],
                );
                assert_eq!(result, expected_result);
                assert_eq!(handler.gas_used(), 0);
            }
            Err(err) => {
                panic!("Expected Ok, but got {:?}", err);
            }
        }
    }

    #[test]
    fn contract_call_fails_beyond_max_stack_depth() {
        let mut mock_runtime = MockKernelHost::default();
        let block = dummy_first_block();
        let precompiles = precompiles::precompile_set::<MockKernelHost>(false);
        let mut evm_account_storage = init_account_storage().unwrap();
        let config = EVMVersion::current_test_config();

        let caller = H160::from_low_u64_be(2340);

        let gas_price = U256::from(21000);

        let mut handler = EvmHandler::new(
            &mut mock_runtime,
            &mut evm_account_storage,
            caller,
            &block,
            &config,
            &precompiles,
            gas_price,
            None,
        );

        let input_value = U256::from(1025_u32); // transaction depth for contract below is callarg - 1
        let mut input = [0_u8; 32];
        input_value.to_big_endian(&mut input);

        let address = H160::from_low_u64_be(12389);
        let transaction_context = TransactionContext::new(caller, address, U256::zero());
        let transfer: Option<Transfer> = None;
        let code: Vec<u8> = vec![
            // get input data, subtract one and prepare as argument to nested call
            Opcode::PUSH1.as_u8(),
            1,
            Opcode::PUSH1.as_u8(),
            0, // call data offset
            Opcode::CALLDATALOAD.as_u8(),
            Opcode::SUB.as_u8(),
            // check if result is zero - if so, skip to return
            Opcode::DUP1.as_u8(),
            Opcode::ISZERO.as_u8(),
            Opcode::PUSH1.as_u8(),
            28_u8, // to JPMDEST
            Opcode::JUMPI.as_u8(),
            // store result in memory to use as call argument
            Opcode::PUSH1.as_u8(),
            0,
            Opcode::MSTORE.as_u8(),
            // set call parameters
            Opcode::PUSH1.as_u8(),
            0, // return size
            Opcode::PUSH1.as_u8(),
            0, // return offset
            Opcode::PUSH1.as_u8(),
            32_u8, // arg size
            Opcode::PUSH1.as_u8(),
            0, // arg offset
            Opcode::PUSH1.as_u8(),
            0,                       // value
            Opcode::ADDRESS.as_u8(), // address
            Opcode::PUSH1.as_u8(),
            0,                    // gas
            Opcode::CALL.as_u8(), // call self
            // when we get here we are done
            Opcode::JUMPDEST.as_u8(),
            Opcode::PUSH1.as_u8(),
            0, // return data size
            Opcode::PUSH1.as_u8(),
            0, // return data offset
            Opcode::RETURN.as_u8(),
        ];

        set_code(&mut handler, &address, code);

        handler
            .begin_initial_transaction(
                CallContext {
                    is_static: false,
                    is_creation: false,
                },
                None,
            )
            .unwrap();

        let result =
            handler.execute_call(address, transfer, input.to_vec(), transaction_context);

        match result {
            Ok(result) => {
                let expected_result =
                    (ExitReason::Succeed(ExitSucceed::Returned), None, vec![]);
                assert_eq!(result, expected_result);
                assert_eq!(handler.gas_used(), 0);
            }
            Err(err) => {
                panic!("Expected Ok, but got {:?}", err);
            }
        }
    }

    #[test]
    fn contract_can_use_durable_storage() {
        let mut mock_runtime = MockKernelHost::default();
        let block = dummy_first_block();
        let precompiles = precompiles::precompile_set::<MockKernelHost>(false);
        let mut evm_account_storage = init_account_storage().unwrap();
        let config = EVMVersion::current_test_config();

        let caller = H160::from_low_u64_be(444);

        let gas_price = U256::from(21000);

        let mut handler = EvmHandler::new(
            &mut mock_runtime,
            &mut evm_account_storage,
            caller,
            &block,
            &config,
            &precompiles,
            gas_price,
            None,
        );

        let address = H160::from_low_u64_be(312);
        let input: Vec<u8> = vec![0_u8];
        let transaction_context = TransactionContext::new(caller, address, U256::zero());
        let transfer: Option<Transfer> = None;
        let code: Vec<u8> = vec![
            Opcode::PUSH1.as_u8(),
            46_u8,
            Opcode::PUSH1.as_u8(),
            0_u8,
            Opcode::SSTORE.as_u8(),
            Opcode::PUSH1.as_u8(),
            0_u8,
            Opcode::SLOAD.as_u8(),
            Opcode::PUSH1.as_u8(),
            1_u8,
            Opcode::SLOAD.as_u8(),
            Opcode::PUSH1.as_u8(),
            0,
            Opcode::RETURN.as_u8(),
        ];

        set_code(&mut handler, &address, code);

        handler
            .begin_initial_transaction(
                CallContext {
                    is_static: false,
                    is_creation: false,
                },
                None,
            )
            .unwrap();

        let result = handler.execute_call(address, transfer, input, transaction_context);

        match result {
            Ok(result) => {
                let expected_result =
                    (ExitReason::Succeed(ExitSucceed::Returned), None, vec![]);
                assert_eq!(result, expected_result);
                let expected_in_storage = H256::from_str(
                    "000000000000000000000000000000000000000000000000000000000000002e",
                )
                .unwrap();
                assert_eq!(
                    get_durable_slot(&mut handler, &address, &H256::zero()),
                    expected_in_storage
                );
                assert_eq!(handler.gas_used(), 0);
            }
            Err(err) => {
                panic!("Expected Ok, but got {:?}", err);
            }
        }
    }

    #[test]
    fn contract_create_can_use_durable_storage() {
        let mut mock_runtime = MockKernelHost::default();
        let block = dummy_first_block();
        let precompiles = precompiles::precompile_set::<MockKernelHost>(false);
        let mut evm_account_storage = init_account_storage().unwrap();
        let config = EVMVersion::current_test_config();

        let caller = H160::from_low_u64_be(117);

        let gas_price = U256::from(21000);

        let mut handler = EvmHandler::new(
            &mut mock_runtime,
            &mut evm_account_storage,
            caller,
            &block,
            &config,
            &precompiles,
            gas_price,
            None,
        );

        let value = U256::zero();
        let create_scheme = CreateScheme::Legacy { caller };
        let init_code: Vec<u8> = hex::decode("608060405234801561001057600080fd5b50602a600081905550610150806100286000396000f3fe608060405234801561001057600080fd5b50600436106100365760003560e01c80632e64cec11461003b5780636057361d14610059575b600080fd5b610043610075565b60405161005091906100a1565b60405180910390f35b610073600480360381019061006e91906100ed565b61007e565b005b60008054905090565b8060008190555050565b6000819050919050565b61009b81610088565b82525050565b60006020820190506100b66000830184610092565b92915050565b600080fd5b6100ca81610088565b81146100d557600080fd5b50565b6000813590506100e7816100c1565b92915050565b600060208284031215610103576101026100bc565b5b6000610111848285016100d8565b9150509291505056fea26469706673582212204d6c1853cec27824f5dbf8bcd0994714258d22fc0e0dc8a2460d87c70e3e57a564736f6c63430008120033").unwrap();

        let expected_address = handler.create_address(create_scheme).unwrap_or_default();

        handler
            .begin_initial_transaction(
                CallContext {
                    is_static: false,
                    is_creation: true,
                },
                None,
            )
            .unwrap();

        let result = handler.execute_create(caller, value, init_code, expected_address);

        match result {
            Ok(result) => {
                let expected_result = (
                    ExitReason::Succeed(ExitSucceed::Returned),
                    Some(expected_address),
                    hex::decode("608060405234801561001057600080fd5b50600436106100365760003560e01c80632e64cec11461003b5780636057361d14610059575b600080fd5b610043610075565b60405161005091906100a1565b60405180910390f35b610073600480360381019061006e91906100ed565b61007e565b005b60008054905090565b8060008190555050565b6000819050919050565b61009b81610088565b82525050565b60006020820190506100b66000830184610092565b92915050565b600080fd5b6100ca81610088565b81146100d557600080fd5b50565b6000813590506100e7816100c1565b92915050565b600060208284031215610103576101026100bc565b5b6000610111848285016100d8565b9150509291505056fea26469706673582212204d6c1853cec27824f5dbf8bcd0994714258d22fc0e0dc8a2460d87c70e3e57a564736f6c63430008120033").unwrap(),
                );
                assert_eq!(result, expected_result);
                assert_eq!(
                    get_durable_slot(&mut handler, &expected_address, &H256::zero()),
                    H256::from_str(
                        "000000000000000000000000000000000000000000000000000000000000002a"
                    )
                    .unwrap()
                );
                assert_eq!(handler.gas_used(), 0);
            }
            Err(err) => {
                panic!("Expected Ok, but got {:?}", err);
            }
        }
    }

    #[test]
    fn contract_create_has_return_when_revert() {
        let mut mock_runtime = MockKernelHost::default();
        let block = dummy_first_block();
        let precompiles = precompiles::precompile_set::<MockKernelHost>(false);
        let mut evm_account_storage = init_account_storage().unwrap();
        let config = EVMVersion::current_test_config();

        let caller = H160::from_low_u64_be(117);

        let gas_price = U256::from(21000);

        let mut handler = EvmHandler::new(
            &mut mock_runtime,
            &mut evm_account_storage,
            caller,
            &block,
            &config,
            &precompiles,
            gas_price,
            None,
        );

        let value = U256::zero();
        let create_scheme = CreateScheme::Legacy { caller };

        // The code of the contract revert with 0x18 (equivalent to 24)
        let initial_code: Vec<u8> = vec![
            Opcode::PUSH1.as_u8(),
            0x18,
            Opcode::PUSH1.as_u8(),
            0x00,
            Opcode::MSTORE.as_u8(),
            Opcode::PUSH1.as_u8(),
            0x20,
            Opcode::PUSH1.as_u8(),
            0x00,
            Opcode::REVERT.as_u8(),
        ];

        let contract_address = handler.create_address(create_scheme).unwrap_or_default();
        handler
            .begin_initial_transaction(
                CallContext {
                    is_static: false,
                    is_creation: true,
                },
                None,
            )
            .unwrap();

        let result =
            handler.execute_create(caller, value, initial_code, contract_address);

        match result {
            Ok(result) => {
                // Expecting to revert with 0x18 in the return vector
                let expected_result = (
                    ExitReason::Revert(ExitRevert::Reverted),
                    None,
                    vec![
                        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                        0, 0, 0, 0, 0, 0, 0, 0, 0, 24,
                    ],
                );
                assert_eq!(result, expected_result);
            }
            Err(err) => {
                panic!("Expected Ok, but got {:?}", err);
            }
        }
    }

    #[test]
    fn contract_call_does_transfer() {
        let mut mock_runtime = MockKernelHost::default();
        let block = dummy_first_block();
        let precompiles = precompiles::precompile_set::<MockKernelHost>(false);
        let mut evm_account_storage = init_account_storage().unwrap();
        let config = EVMVersion::current_test_config();

        let caller = H160::from_low_u64_be(118);

        let gas_price = U256::from(21000);

        let mut handler = EvmHandler::new(
            &mut mock_runtime,
            &mut evm_account_storage,
            caller,
            &block,
            &config,
            &precompiles,
            gas_price,
            None,
        );

        let address = H160::from_low_u64_be(117);
        let input = vec![0_u8];
        let transaction_context =
            TransactionContext::new(caller, address, U256::from(50_u32));
        let transfer: Option<Transfer> = Some(Transfer {
            source: caller,
            target: address,
            value: U256::from(100_u32),
        });
        let code: Vec<u8> = vec![
            Opcode::PUSH1.as_u8(),
            0u8,
            Opcode::PUSH1.as_u8(),
            0u8,
            Opcode::RETURN.as_u8(),
        ];

        set_code(&mut handler, &address, code);
        set_balance(&mut handler, &caller, U256::from(101_u32));

        handler
            .begin_initial_transaction(
                CallContext {
                    is_static: false,
                    is_creation: false,
                },
                None,
            )
            .unwrap();

        let result = handler.execute_call(address, transfer, input, transaction_context);

        match result {
            Ok(result) => {
                let expected_result =
                    (ExitReason::Succeed(ExitSucceed::Returned), None, vec![]);
                assert_eq!(result, expected_result);
                assert_eq!(get_balance(&mut handler, &address), U256::from(100_u32));
                assert_eq!(get_balance(&mut handler, &caller), U256::from(1_u32));
                assert_eq!(handler.gas_used(), 0);
            }
            Err(err) => {
                panic!("Expected Ok, but got {:?}", err);
            }
        }
    }

    #[test]
    fn contract_call_fails_when_insufficient_funds_for_transfer() {
        let mut mock_runtime = MockKernelHost::default();
        let block = dummy_first_block();
        let precompiles = precompiles::precompile_set::<MockKernelHost>(false);
        let mut evm_account_storage = init_account_storage().unwrap();
        let config = EVMVersion::current_test_config();

        let caller = H160::from_low_u64_be(523_u64);

        let gas_price = U256::from(21000);

        let mut handler = EvmHandler::new(
            &mut mock_runtime,
            &mut evm_account_storage,
            caller,
            &block,
            &config,
            &precompiles,
            gas_price,
            None,
        );

        let address = H160::from_low_u64_be(210_u64);
        let input = vec![0_u8];
        let transaction_context = TransactionContext::new(caller, address, U256::zero());
        let transfer: Option<Transfer> = Some(Transfer {
            source: caller,
            target: address,
            value: U256::from(100_u32),
        });
        let code: Vec<u8> = vec![
            Opcode::PUSH1.as_u8(),
            0u8,
            Opcode::PUSH1.as_u8(),
            0u8,
            Opcode::RETURN.as_u8(),
        ];

        set_code(&mut handler, &address, code);
        set_balance(&mut handler, &caller, U256::from(99_u32));

        handler
            .begin_initial_transaction(
                CallContext {
                    is_static: false,
                    is_creation: false,
                },
                None,
            )
            .unwrap();

        let result = handler.execute_call(address, transfer, input, transaction_context);

        match result {
            Ok(result) => {
                let expected_result =
                    (ExitReason::Error(ExitError::OutOfFund), None, vec![]);
                assert_eq!(result, expected_result);
                assert_eq!(get_balance(&mut handler, &caller), U256::from(99_u32));
                assert_eq!(get_balance(&mut handler, &address), U256::zero());
                assert_eq!(handler.gas_used(), 0);
            }
            Err(err) => {
                panic!("Unexpected error: {:?}", err);
            }
        }
    }

    #[test]
    fn revert_can_return_a_value() {
        let mut mock_runtime = MockKernelHost::default();
        let block = dummy_first_block();
        let precompiles = precompiles::precompile_set::<MockKernelHost>(false);
        let mut evm_account_storage = init_account_storage().unwrap();
        let config = EVMVersion::current_test_config();

        let caller = H160::from_low_u64_be(523_u64);

        let gas_price = U256::from(21000);

        let mut handler = EvmHandler::new(
            &mut mock_runtime,
            &mut evm_account_storage,
            caller,
            &block,
            &config,
            &precompiles,
            gas_price,
            None,
        );

        let address = H160::from_low_u64_be(210_u64);
        let input = vec![0_u8];
        let transaction_context = TransactionContext::new(caller, address, U256::zero());
        let transfer: Option<Transfer> = None;

        let code: Vec<u8> = vec![
            Opcode::PUSH8.as_u8(), // push value of return data
            0,
            1,
            2,
            3,
            4,
            5,
            6,
            7,
            Opcode::PUSH1.as_u8(), // push address of return data
            0,
            Opcode::MSTORE.as_u8(), // store return data in memory
            Opcode::PUSH1.as_u8(),  // push size of return data
            8,
            Opcode::PUSH1.as_u8(), // push offset in memory of return data
            24,
            Opcode::REVERT.as_u8(),
        ];

        set_code(&mut handler, &address, code);
        set_balance(&mut handler, &caller, U256::from(99_u32));

        handler
            .begin_initial_transaction(
                CallContext {
                    is_static: false,
                    is_creation: false,
                },
                None,
            )
            .unwrap();

        let result = handler.execute_call(address, transfer, input, transaction_context);

        match result {
            Ok(result) => {
                let expected_result = (
                    ExitReason::Revert(ExitRevert::Reverted),
                    None,
                    vec![0, 1, 2, 3, 4, 5, 6, 7],
                );
                assert_eq!(expected_result, result);
            }
            Err(err) => {
                panic!("Unexpected error: {:?}", err);
            }
        }
    }

    #[test]
    fn return_hash_of_zero_for_unavailable_block() {
        let mut mock_runtime = MockKernelHost::default();
        let block = dummy_first_block();
        let precompiles = precompiles::precompile_set::<MockKernelHost>(false);
        let mut evm_account_storage = init_account_storage().unwrap();
        let config = EVMVersion::current_test_config();

        let caller = H160::from_low_u64_be(523_u64);

        let gas_price = U256::from(21000);

        let handler = EvmHandler::new(
            &mut mock_runtime,
            &mut evm_account_storage,
            caller,
            &block,
            &config,
            &precompiles,
            gas_price,
            None,
        );

        let hash_of_unavailable_block = handler.block_hash(U256::zero());
        assert_eq!(H256::zero(), hash_of_unavailable_block)
    }

    #[test]
    fn store_after_offset_1024() {
        let mut mock_runtime = MockKernelHost::default();
        let block = dummy_first_block();
        let precompiles = precompiles::precompile_set::<MockKernelHost>(false);
        let mut evm_account_storage = init_account_storage().unwrap();
        let config = EVMVersion::current_test_config();

        let caller = H160::from_low_u64_be(523_u64);

        let gas_price = U256::from(21000);

        let mut handler = EvmHandler::new(
            &mut mock_runtime,
            &mut evm_account_storage,
            caller,
            &block,
            &config,
            &precompiles,
            gas_price,
            None,
        );

        let address = H160::from_low_u64_be(210_u64);
        let input = vec![0_u8];
        let transaction_context = TransactionContext::new(caller, address, U256::zero());
        let transfer: Option<Transfer> = None;

        let code: Vec<u8> = vec![
            Opcode::PUSH1.as_u8(), // push value 0xff
            0xff,
            Opcode::PUSH2.as_u8(), // push offset 0x401 == 1025
            0x04,
            0x01,
            Opcode::MSTORE8.as_u8(), // Store 0xff at offset 1025 in memory
        ];

        set_code(&mut handler, &address, code);
        set_balance(&mut handler, &caller, U256::from(99_u32));

        handler
            .begin_initial_transaction(
                CallContext {
                    is_static: false,
                    is_creation: false,
                },
                None,
            )
            .unwrap();

        let result = handler.execute_call(address, transfer, input, transaction_context);

        assert_eq!(
            Ok((ExitReason::Succeed(ExitSucceed::Stopped), None, vec![])),
            result,
            "Writing at offset 1025 in the memory doesn't work"
        )
    }

    #[test]
    fn dont_crash_on_blockhash_instruction() {
        let mut mock_runtime = MockKernelHost::default();
        let block = dummy_first_block();
        let precompiles = precompiles::precompile_set::<MockKernelHost>(false);
        let mut evm_account_storage = init_account_storage().unwrap();
        let config = EVMVersion::current_test_config();

        let caller = H160::from_low_u64_be(523_u64);

        let gas_price = U256::from(21000);

        let mut handler = EvmHandler::new(
            &mut mock_runtime,
            &mut evm_account_storage,
            caller,
            &block,
            &config,
            &precompiles,
            gas_price,
            None,
        );

        let address = H160::from_low_u64_be(210_u64);
        let input = vec![0_u8];
        let transaction_context = TransactionContext::new(caller, address, U256::zero());
        let transfer: Option<Transfer> = None;

        let code: Vec<u8> = vec![
            Opcode::PUSH1.as_u8(), // push value 0x1
            0x1,
            Opcode::PUSH1.as_u8(), // push value 0x0
            0x0,
            Opcode::MSTORE.as_u8(), // a 1 at location 0
            Opcode::PUSH4.as_u8(),  // push value 0xffffffff
            0xff,
            0xff,
            0xff,
            0xff,
            Opcode::BLOCKHASH.as_u8(),
            Opcode::PUSH1.as_u8(), // push value 0x0
            0x0,
            Opcode::MSTORE.as_u8(), // store blockhash at location 0x0
            Opcode::PUSH1.as_u8(),  // push 32
            32,
            Opcode::PUSH1.as_u8(), // push 0x0
            0x0,
            Opcode::RETURN.as_u8(),
        ];

        set_code(&mut handler, &address, code);
        set_balance(&mut handler, &caller, U256::from(99_u32));

        handler
            .begin_initial_transaction(
                CallContext {
                    is_static: false,
                    is_creation: false,
                },
                None,
            )
            .unwrap();

        let result = handler.execute_call(address, transfer, input, transaction_context);

        assert_eq!(
            Ok((
                ExitReason::Succeed(ExitSucceed::Returned),
                None,
                vec![0; 32],
            )),
            result,
        )
    }

    #[test]
    fn prevent_collision_create2_selfdestruct() {
        let mut mock_runtime = MockKernelHost::default();
        let block = dummy_first_block();
        let precompiles = precompiles::precompile_set::<MockKernelHost>(false);
        let mut evm_account_storage = init_account_storage().unwrap();

        let config = EVMVersion::current_test_config();

        let caller_address: [u8; 20] =
            hex::decode("a94f5374fce5edbc8e2a8697c15331677e6ebf0b")
                .unwrap()
                .try_into()
                .unwrap();
        let caller = H160::from(caller_address);
        let target_address: [u8; 20] =
            hex::decode("ec2c6832d00680ece8ff9254f81fdab0a5a2ac50")
                .unwrap()
                .try_into()
                .unwrap();
        let target_address = H160::from(target_address);

        let transaction_context =
            TransactionContext::new(caller, target_address, U256::zero());

        let gas_price = U256::from(21000);

        // { (CALL 50000 0xec2c6832d00680ece8ff9254f81fdab0a5a2ac50 0 0 0 0 0) (MSTORE 0 0x6460016001556000526005601bf3) (CREATE2 0 18 14 0) }
        let input = hex::decode("6000600060006000600073e2b35478fdd26477cc576dd906e6277761246a3c61c350f1506000600060006000f500").unwrap();

        let mut handler = EvmHandler::new(
            &mut mock_runtime,
            &mut evm_account_storage,
            caller,
            &block,
            &config,
            &precompiles,
            gas_price,
            None,
        );

        // { (SELFDESTRUCT 0x10) }
        let code = hex::decode("6010ff").unwrap();

        set_code(&mut handler, &target_address, code);
        set_balance(&mut handler, &caller, U256::from(1000000000000000000u64));

        handler
            .begin_initial_transaction(
                CallContext {
                    is_static: false,
                    is_creation: false,
                },
                None,
            )
            .unwrap();

        let result =
            handler.execute_call(target_address, None, input, transaction_context);

        // This assertion will change with the upcoming Dencun config/fork
        // See: https://eips.ethereum.org/EIPS/eip-6780
        assert_eq!(
            Ok((ExitReason::Succeed(ExitSucceed::Suicided), None, vec![],)),
            result,
        );

        let code = handler.code(target_address);
        let balance = handler.balance(target_address);

        assert_eq!(code, vec![96, 16, 255]);
        assert_eq!(balance, U256::zero());
    }

    #[test]
    fn create_contract_with_insufficient_funds() {
        //Init
        let mut mock_runtime = MockKernelHost::default();
        let block = dummy_first_block();
        let precompiles = precompiles::precompile_set::<MockKernelHost>(false);
        let mut evm_account_storage = init_account_storage().unwrap();
        let config = EVMVersion::current_test_config();

        let caller = H160::from_str("a94f5374fce5edbc8e2a8697c15331677e6ebf0b").unwrap();

        let mut handler = EvmHandler::new(
            &mut mock_runtime,
            &mut evm_account_storage,
            caller,
            &block,
            &config,
            &precompiles,
            U256::one(),
            None,
        );

        set_balance(&mut handler, &caller, U256::from(10000));

        let scheme = CreateScheme::Legacy { caller };
        let code = hex::decode("600c60005566602060406000f060205260076039f3").unwrap();

        let contract_address = handler.create_address(scheme).unwrap_or_default();

        handler
            .begin_initial_transaction(
                CallContext {
                    is_static: false,
                    is_creation: true,
                },
                Some(10000),
            )
            .unwrap();

        let result =
            handler.execute_create(caller, U256::from(100000), code, contract_address);

        assert_eq!(
            result.unwrap(),
            (ExitReason::Error(ExitError::OutOfFund), None, vec![])
        );
    }

    #[test]
    fn inter_call_with_non_zero_transfer_value_gets_call_stipend() {
        let mut mock_runtime = MockKernelHost::default();
        let block = dummy_first_block();
        let precompiles = precompiles::precompile_set::<MockKernelHost>(false);
        let mut evm_account_storage = init_account_storage().unwrap();
        let config = EVMVersion::current_test_config();

        let caller = H160::from_low_u64_be(523_u64);

        let gas_price = U256::from(21000);

        let mut handler = EvmHandler::new(
            &mut mock_runtime,
            &mut evm_account_storage,
            caller,
            &block,
            &config,
            &precompiles,
            gas_price,
            None,
        );

        let address_1 = H160::from_low_u64_be(210_u64);
        let address_2 = H160::from_low_u64_be(211_u64);
        let input = vec![0_u8];
        let transaction_context =
            TransactionContext::new(caller, address_1, U256::zero());
        let transfer: Option<Transfer> = None;

        let code_1: Vec<u8> = vec![
            Opcode::PUSH1.as_u8(),
            0, // return size
            Opcode::PUSH1.as_u8(),
            0, // return offset
            Opcode::PUSH1.as_u8(),
            0, // arg size
            Opcode::PUSH1.as_u8(),
            0, // arg offset
            Opcode::PUSH1.as_u8(),
            100, // non-zero value
            Opcode::PUSH1.as_u8(),
            211, // address
            Opcode::PUSH1.as_u8(),
            0,                    // gas
            Opcode::CALL.as_u8(), // call should suceed and return 1
            Opcode::PUSH1.as_u8(),
            0,
            Opcode::MSTORE.as_u8(), // store 1 to Memory[0:32]
            Opcode::PUSH1.as_u8(),
            1,
            Opcode::PUSH1.as_u8(),
            31,
            Opcode::RETURN.as_u8(), // return byte that contains the 1
        ];

        let code_2: Vec<u8> = vec![Opcode::TIMESTAMP.as_u8()];

        set_code(&mut handler, &address_1, code_1);
        set_code(&mut handler, &address_2, code_2);

        set_balance(&mut handler, &caller, U256::from(1000_u32));
        set_balance(&mut handler, &address_1, U256::from(1000_u32));

        handler
            .begin_initial_transaction(
                CallContext {
                    is_static: false,
                    is_creation: false,
                },
                None,
            )
            .unwrap();

        let result =
            handler.execute_call(address_1, transfer, input, transaction_context);

        assert_eq!(
            Ok((ExitReason::Succeed(ExitSucceed::Returned), None, vec![1],)),
            result,
        )
    }

    #[test]
    fn code_hash_of_zero_for_non_existing_address() {
        let mut mock_runtime = MockKernelHost::default();
        let block = dummy_first_block();
        let precompiles = precompiles::precompile_set::<MockKernelHost>(false);
        let mut evm_account_storage = init_account_storage().unwrap();
        let config = EVMVersion::current_test_config();

        let caller = H160::from_low_u64_be(523_u64);

        let gas_price = U256::from(21000);

        let mut handler = EvmHandler::new(
            &mut mock_runtime,
            &mut evm_account_storage,
            caller,
            &block,
            &config,
            &precompiles,
            gas_price,
            None,
        );

        let hash = handler.code_hash(H160::from_low_u64_le(1));

        assert_eq!(H256::zero(), hash)
    }

    #[test]
    fn create_contract_with_selfdestruct_init_code() {
        let mut mock_runtime = MockKernelHost::default();
        let block = dummy_first_block();
        let precompiles = precompiles::precompile_set::<MockKernelHost>(false);
        let mut evm_account_storage = init_account_storage().unwrap();
        let config = EVMVersion::current_test_config();

        let caller = H160::from_str("a94f5374fce5edbc8e2a8697c15331677e6ebf0b").unwrap();
        let withdrawal_contract =
            H160::from_str("2adc25665018aa1fe0e6bc666dac8fc2697ff9ba").unwrap();

        let mut handler = EvmHandler::new(
            &mut mock_runtime,
            &mut evm_account_storage,
            caller,
            &block,
            &config,
            &precompiles,
            U256::one(),
            None,
        );

        set_balance(&mut handler, &caller, U256::from(1000000000));

        let code = hex::decode("732adc25665018aa1fe0e6bc666dac8fc2697ff9baff00").unwrap(); // transfer balance to 0x2adc25665018aa1fe0e6bc666dac8fc2697ff9ba and selfdestruct

        let result = handler
            .create_contract(caller, Some(U256::one()), code, None)
            .unwrap();

        let suicided_contract = result.new_address().unwrap();

        assert!(matches!(
            result.result,
            ExecutionResult::ContractDeployed(_, _)
        ));
        assert_eq!(get_balance(&mut handler, &withdrawal_contract), U256::one());
        assert_eq!(get_balance(&mut handler, &caller), U256::from(999999999));
        assert!(!handler.exists(suicided_contract));
    }

    #[test]
    fn contract_that_selfdestruct_not_deleted_within_same_transaction() {
        let mut mock_runtime = MockKernelHost::default();
        let block = dummy_first_block();
        let precompiles = precompiles::precompile_set::<MockKernelHost>(false);
        let mut evm_account_storage = init_account_storage().unwrap();
        let config = EVMVersion::current_test_config();

        let caller = H160::from_low_u64_be(523_u64);

        let gas_price = U256::from(21000);

        let mut handler = EvmHandler::new(
            &mut mock_runtime,
            &mut evm_account_storage,
            caller,
            &block,
            &config,
            &precompiles,
            gas_price,
            None,
        );

        let address_1 = H160::from_low_u64_be(210_u64);
        let address_2 = H160::from_low_u64_be(211_u64);
        let input = vec![0_u8];
        let transaction_context =
            TransactionContext::new(caller, address_1, U256::zero());
        let transfer: Option<Transfer> = None;

        let code_1: Vec<u8> = vec![
            Opcode::PUSH1.as_u8(),
            0, // return size
            Opcode::PUSH1.as_u8(),
            0, // return offset
            Opcode::PUSH1.as_u8(),
            0, // arg size
            Opcode::PUSH1.as_u8(),
            0, // arg offset
            Opcode::PUSH1.as_u8(),
            0, // value
            Opcode::PUSH1.as_u8(),
            211, // address
            Opcode::PUSH2.as_u8(),
            100,
            0,                    // gas
            Opcode::CALL.as_u8(), // call should cause address 2 to selfdestruct
            Opcode::POP.as_u8(),  // pop return value off the stack
            Opcode::PUSH1.as_u8(),
            211, // address
            Opcode::EXTCODESIZE.as_u8(),
            Opcode::PUSH1.as_u8(),
            0,
            Opcode::MSTORE.as_u8(), // store 1 to Memory[0:32]
            Opcode::PUSH1.as_u8(),
            1,
            Opcode::PUSH1.as_u8(),
            31,
            Opcode::RETURN.as_u8(), // return codesize of the contract that selfdestruct
        ];

        let code_2: Vec<u8> = vec![Opcode::PUSH1.as_u8(), 0, Opcode::SUICIDE.as_u8()];

        set_code(&mut handler, &address_1, code_1);
        set_code(&mut handler, &address_2, code_2);

        set_balance(&mut handler, &caller, U256::from(1000_u32));
        set_balance(&mut handler, &address_1, U256::from(1000_u32));

        handler
            .begin_initial_transaction(
                CallContext {
                    is_static: false,
                    is_creation: false,
                },
                Some(1000000),
            )
            .unwrap();

        let result =
            handler.execute_call(address_1, transfer, input, transaction_context);

        assert_eq!(
            Ok((ExitReason::Succeed(ExitSucceed::Returned), None, vec![3],)),
            result,
        )
    }

    #[test]
    fn contract_that_selfdestruct_can_be_called_again_in_same_transaction() {
        let mut mock_runtime = MockKernelHost::default();
        let block = dummy_first_block();
        let precompiles = precompiles::precompile_set::<MockKernelHost>(false);
        let mut evm_account_storage = init_account_storage().unwrap();
        let config = EVMVersion::current_test_config();

        let caller = H160::from_low_u64_be(523_u64);

        let gas_price = U256::from(21000);

        let mut handler = EvmHandler::new(
            &mut mock_runtime,
            &mut evm_account_storage,
            caller,
            &block,
            &config,
            &precompiles,
            gas_price,
            None,
        );

        let address_1 = H160::from_low_u64_be(210_u64);
        let address_2 = H160::from_low_u64_be(211_u64);
        let input = vec![0_u8];
        let transaction_context =
            TransactionContext::new(caller, address_1, U256::zero());
        let transfer: Option<Transfer> = None;

        let code_1: Vec<u8> = vec![
            Opcode::PUSH1.as_u8(),
            0, // return size
            Opcode::PUSH1.as_u8(),
            0, // return offset
            Opcode::PUSH1.as_u8(),
            0, // arg size
            Opcode::PUSH1.as_u8(),
            0, // arg offset
            Opcode::PUSH1.as_u8(),
            0, // value
            Opcode::PUSH1.as_u8(),
            211, // address
            Opcode::PUSH2.as_u8(),
            100,
            0,                    // gas
            Opcode::CALL.as_u8(), // call should cause address 2 to selfdestruct
            Opcode::POP.as_u8(),  // pop return value off the stack
            Opcode::PUSH1.as_u8(),
            0, // return size
            Opcode::PUSH1.as_u8(),
            0, // return offset
            Opcode::PUSH1.as_u8(),
            0, // arg size
            Opcode::PUSH1.as_u8(),
            0, // arg offset
            Opcode::PUSH1.as_u8(),
            0, // value
            Opcode::PUSH1.as_u8(),
            211, // address
            Opcode::PUSH2.as_u8(),
            100,
            0,                    // gas
            Opcode::CALL.as_u8(), // call should cause address 2 to selfdestruct
            Opcode::PUSH1.as_u8(),
            0,
            Opcode::MSTORE.as_u8(), // store result to Memory[0:1]
            Opcode::PUSH1.as_u8(),
            1,
            Opcode::PUSH1.as_u8(),
            31,
            Opcode::RETURN.as_u8(), // return result of second call
        ];

        let code_2: Vec<u8> = vec![Opcode::PUSH1.as_u8(), 0, Opcode::SUICIDE.as_u8()];

        set_code(&mut handler, &address_1, code_1);
        set_code(&mut handler, &address_2, code_2);

        set_balance(&mut handler, &caller, U256::from(1000_u32));
        set_balance(&mut handler, &address_1, U256::from(1000_u32));

        handler
            .begin_initial_transaction(
                CallContext {
                    is_static: false,
                    is_creation: false,
                },
                Some(1000000),
            )
            .unwrap();

        let result =
            handler.execute_call(address_1, transfer, input, transaction_context);

        // The transaction should consume more than twice the cost of a selfdestruct
        assert!(handler.gas_used() > 10000);

        // The second call succeeded
        assert_eq!(
            Ok((ExitReason::Succeed(ExitSucceed::Returned), None, vec![1],)),
            result,
        )
    }

    #[test]
    fn contract_selfdestruct_itself_has_no_balance_left() {
        let mut mock_runtime = MockKernelHost::default();
        let block = dummy_first_block();
        let precompiles = precompiles::precompile_set::<MockKernelHost>(false);
        let mut evm_account_storage = init_account_storage().unwrap();
        // This test is specifically testing a Shanghai behaviour:
        let config = Config::shanghai();

        let caller = H160::from_str("095e7baea6a6c7c4c2dfeb977efac326af552d87").unwrap();

        let gas_price = U256::from(21000);

        let mut handler = EvmHandler::new(
            &mut mock_runtime,
            &mut evm_account_storage,
            caller,
            &block,
            &config,
            &precompiles,
            gas_price,
            None,
        );

        let target_destruct =
            H160::from_str("a94f5374fce5edbc8e2a8697c15331677e6ebf0b").unwrap();

        let contract = H160::from_low_u64_be(523_u64);

        // Contract selfdestruct itself and then tries to selfdestruct to target_destruct.
        // The second selfdestruct is ignored and the first remove the balance of contract
        let code = hex::decode("60003560085730ff5b600080808080305af15073a94f5374fce5edbc8e2a8697c15331677e6ebf0bff").unwrap();

        set_code(&mut handler, &contract, code);

        set_balance(&mut handler, &contract, U256::from(100000_u32));

        let result = handler.call_contract(
            caller,
            contract,
            None,
            vec![0xff],
            Some(100_000),
            false,
        );

        match result {
            Ok(exec_out) if exec_out.is_success() => {
                assert_eq!(
                    get_balance(&mut handler, &contract),
                    U256::zero(),
                    "Contract balance is not 0"
                );

                assert_eq!(
                    get_balance(&mut handler, &target_destruct),
                    U256::zero(),
                    "target_destruct balance is not 0"
                );
            }
            _ => panic!("Execution failed"),
        }
    }

    // According EIP-2929, the created address should still be hot even if the creation fails
    #[test]
    fn address_still_marked_as_hot_after_creation_fails() {
        let mut mock_runtime = MockKernelHost::default();
        let block = dummy_first_block();
        let precompiles = precompiles::precompile_set::<MockKernelHost>(false);
        let mut evm_account_storage = init_account_storage().unwrap();
        let config = EVMVersion::current_test_config();

        let gas_price = U256::from(21000);

        let caller = H160::from_str("a94f5374fce5edbc8e2a8697c15331677e6ebf0b").unwrap();

        let mut handler = EvmHandler::new(
            &mut mock_runtime,
            &mut evm_account_storage,
            caller,
            &block,
            &config,
            &precompiles,
            gas_price,
            None,
        );

        let contrac_addr =
            H160::from_str("095e7baea6a6c7c4c2dfeb977efac326af552d87").unwrap();
        let expected_address = handler
            .create_address(CreateScheme::Legacy {
                caller: contrac_addr,
            })
            .unwrap_or_default();

        // Tries to CREATE a contract (that will revert)
        let contract_code = vec![
            Opcode::PUSH5.as_u8(),
            0x60,
            0x00,
            0x60,
            0x00,
            0xfd,
            Opcode::PUSH1.as_u8(),
            0x00,
            Opcode::MSTORE.as_u8(),
            Opcode::PUSH1.as_u8(),
            0x05,
            Opcode::PUSH1.as_u8(),
            0x1b,
            Opcode::PUSH1.as_u8(),
            0x00,
            Opcode::CREATE.as_u8(),
        ];
        set_code(&mut handler, &contrac_addr, contract_code);
        let input = vec![0_u8];
        let transaction_context =
            TransactionContext::new(caller, contrac_addr, U256::zero());
        let transfer: Option<Transfer> = None;

        handler
            .begin_initial_transaction(
                CallContext {
                    is_static: false,
                    is_creation: false,
                },
                Some(1000000),
            )
            .unwrap();

        let _ = handler.execute_call(contrac_addr, transfer, input, transaction_context);

        let exist = handler.is_colliding(expected_address).unwrap();

        assert!(!exist, "Expected address should not exist");

        // After the `execute_call` expected address should be marked as hot
        let is_hot = handler.is_address_hot(expected_address).unwrap();

        assert!(is_hot, "Expected address is cold where it should be hot");
    }

    #[test]
    fn precompile_failure_are_not_fatal() {
        let mut host = MockKernelHost::default();
        let block = dummy_first_block();
        let precompiles = precompiles::precompile_set::<MockKernelHost>(false);
        let mut evm_account_storage = init_account_storage().unwrap();
        let config = EVMVersion::current_test_config();

        let caller = H160::from_low_u64_be(523_u64);

        let mut handler = EvmHandler::new(
            &mut host,
            &mut evm_account_storage,
            caller,
            &block,
            &config,
            &precompiles,
            U256::from(21000),
            None,
        );

        handler
            .begin_initial_transaction(
                CallContext {
                    is_static: false,
                    is_creation: false,
                },
                Some(150000),
            )
            .unwrap();

        handler
            .begin_inter_transaction(
                CallContext {
                    is_static: false,
                    is_creation: false,
                },
                Some(150000),
            )
            .unwrap();

        let ecmul = H160::from_low_u64_be(7u64);

        // ecmul -> point not on curve fail
        let failing_input = hex::decode(
            "\
            1111111111111111111111111111111111111111111111111111111111111111\
            1111111111111111111111111111111111111111111111111111111111111111\
            0f00000000000000000000000000000000000000000000000000000000000000",
        )
        .unwrap();

        let transaction_context = TransactionContext::new(caller, ecmul, U256::zero());

        let result =
            handler.execute_call(ecmul, None, failing_input, transaction_context);

        let inter_result: Capture<CreateOutcome, H160> =
            handler.end_inter_transaction(result);

        // Internal result is not a Fatal case anymore.
        match inter_result {
            Capture::Exit((exit_reason, _, _)) => match exit_reason {
                ExitReason::Error(_) => (),
                e => panic!("The exit reason should be an error but got {:?}.", e),
            },
            Capture::Trap(_) => panic!("The internal result shouldn't be a trap case."),
        }
    }

    #[test]
    fn inner_create_costs_gas() {
        let mut mock_runtime = MockKernelHost::default();
        let block = dummy_first_block();
        let precompiles = precompiles::precompile_set::<MockKernelHost>(false);
        let mut evm_account_storage = init_account_storage().unwrap();
        let config = EVMVersion::current_test_config();

        let caller = H160::from_low_u64_be(523_u64);

        let gas_price = U256::from(21000);

        let mut handler = EvmHandler::new(
            &mut mock_runtime,
            &mut evm_account_storage,
            caller,
            &block,
            &config,
            &precompiles,
            gas_price,
            None,
        );

        let address = H160::from_low_u64_be(210_u64);

        // Create an account with 0 wei and 4 FF as code
        // PUSH13 0x63FFFFFFFF6000526004601CF3
        // PUSH1 0
        // MSTORE
        // PUSH1 13
        // PUSH1 19
        // PUSH1 0
        // CREATE
        // STOP

        let code: Vec<u8> = vec![
            Opcode::PUSH13.as_u8(), // 3 gas
            0x63,                   // 3 gas
            0xff,
            0xff,
            0xff,
            0xff,
            0x60, // 3 gas
            0x00,
            0x52, // 6 gas
            0x60, // 3 gas
            0x04,
            0x60, // 3 gas
            0x1c,
            0xf3,
            Opcode::PUSH1.as_u8(), // 3 gas
            0,
            Opcode::MSTORE.as_u8(), // 6 gas
            Opcode::PUSH1.as_u8(),  // 3 gas
            13,
            Opcode::PUSH1.as_u8(), // 3 gas
            19,
            Opcode::PUSH1.as_u8(), // 3 gas
            0,
            Opcode::CREATE.as_u8(), // 32000 gas + 800 gas (code_deposit_cost) + 2 gas (init_code_cost)
            Opcode::STOP.as_u8(),
        ];

        set_code(&mut handler, &address, code);
        set_balance(&mut handler, &caller, U256::from(99_u32));

        let result =
            handler.call_contract(caller, address, None, vec![], Some(1000000), false);

        assert_eq!(
            Ok(ExecutionOutcome {
                gas_used: 53841,
                logs: vec![],
                result: ExecutionResult::CallSucceeded(ExitSucceed::Stopped, vec![]),
                withdrawals: vec![],
                estimated_ticks_used: 40549406
            }),
            result,
        )
    }

    #[test]
    fn exceed_max_create_init_code_size_fail_with_error() {
        let mut mock_runtime = MockKernelHost::default();
        let block = dummy_first_block();
        let precompiles = precompiles::precompile_set::<MockKernelHost>(false);
        let mut evm_account_storage = init_account_storage().unwrap();
        let config = EVMVersion::current_test_config();

        let caller = H160::from_low_u64_be(523_u64);

        let gas_price = U256::from(21000);

        let mut handler = EvmHandler::new(
            &mut mock_runtime,
            &mut evm_account_storage,
            caller,
            &block,
            &config,
            &precompiles,
            gas_price,
            None,
        );

        let initial_code = [1; 49153]; // MAX_INIT_CODE_SIZE + 1

        handler
            .begin_initial_transaction(
                CallContext {
                    is_static: false,
                    is_creation: true,
                },
                Some(150000),
            )
            .unwrap();

        let capture = handler.create(
            caller,
            CreateScheme::Legacy { caller },
            U256::zero(),
            initial_code.to_vec(),
            None,
        );

        match capture {
            Capture::Exit((
                ExitReason::Fatal(ExitFatal::CallErrorAsFatal(
                    ExitError::CreateContractLimit,
                )),
                ..,
            )) => (),
            e => panic!(
                "Create doesn't fail with error CreateContractLimit but with {:?}",
                e
            ),
        }
    }

    #[test]
    fn create_fails_with_max_nonce() {
        let mut mock_runtime = MockKernelHost::default();
        let block = dummy_first_block();
        let precompiles = precompiles::precompile_set::<MockKernelHost>(false);
        let mut evm_account_storage = init_account_storage().unwrap();
        let config = EVMVersion::current_test_config();

        let caller = H160::from_str("a94f5374fce5edbc8e2a8697c15331677e6ebf0b").unwrap();

        let mut handler = EvmHandler::new(
            &mut mock_runtime,
            &mut evm_account_storage,
            caller,
            &block,
            &config,
            &precompiles,
            U256::one(),
            None,
        );

        let _ = handler.begin_initial_transaction(
            CallContext {
                is_static: false,
                is_creation: true,
            },
            None,
        );

        set_balance(&mut handler, &caller, U256::from(1000000000));
        set_nonce(&mut handler, &caller, u64::MAX);

        let init_code = hex::decode("60006000fd").unwrap(); // Just revert

        let capture = handler.create(
            caller,
            CreateScheme::Legacy { caller },
            U256::zero(),
            init_code,
            None,
        );

        match capture {
            Capture::Exit((ExitReason::Error(ExitError::MaxNonce), ..)) => (),
            _ => panic!("Create doesn't fail with Error MaxNonce"),
        }
    }

    #[test]
    fn record_call_stipend_when_balance_not_enough_for_inner_call() {
        let mut mock_runtime = MockKernelHost::default();
        let block = dummy_first_block();
        let precompiles = precompiles::precompile_set::<MockKernelHost>(false);
        let mut evm_account_storage = init_account_storage().unwrap();
        let config = EVMVersion::current_test_config();

        let caller = H160::from_low_u64_be(523_u64);

        let mut handler = EvmHandler::new(
            &mut mock_runtime,
            &mut evm_account_storage,
            caller,
            &block,
            &config,
            &precompiles,
            U256::from(21000),
            None,
        );

        let address1 = H160::from_low_u64_be(210_u64);
        let address2 = H160::from_low_u64_be(211_u64);

        let code1: Vec<u8> = vec![
            Opcode::PUSH1.as_u8(),
            0, // return size
            Opcode::PUSH1.as_u8(),
            0, // return offset
            Opcode::PUSH1.as_u8(),
            0, // arg size
            Opcode::PUSH1.as_u8(),
            0, // arg offset
            Opcode::PUSH1.as_u8(),
            100, // value
            Opcode::PUSH1.as_u8(),
            211, // address
            Opcode::PUSH1.as_u8(),
            0,                    // gas
            Opcode::CALL.as_u8(), // Call should fail due to insufficient balance, leaving a zero on the stack
            Opcode::PUSH1.as_u8(),
            0, // memory index for mstore
            Opcode::MSTORE.as_u8(),
            Opcode::PUSH1.as_u8(),
            1, //  size of return
            Opcode::PUSH1.as_u8(),
            31, // memory index for return
            Opcode::RETURN.as_u8(),
        ];

        let code2: Vec<u8> = vec![Opcode::STOP.as_u8()];

        set_code(&mut handler, &address1, code1);
        set_code(&mut handler, &address2, code2);
        set_balance(&mut handler, &caller, U256::from(100_u32));

        let result = handler
            .call_contract(
                caller,
                address1,
                Some(U256::from(99_u32)),
                vec![],
                Some(1000000),
                false,
            )
            .unwrap();

        // Gas cost: 21000(BASE) + 10 * 3(PUSH1) + 3(MSTORE) + 3(Memory expansion) + 100(Call) + 9000(Positive value cost) - 2300(Call Stipend)
        assert_eq!(result.gas_used, 30336);

        assert_eq!(
            result.result,
            ExecutionResult::CallSucceeded(ExitSucceed::Returned, vec![0])
        );
    }

    // eip-161
    #[test]
    fn eip161_gas_consumption_rules_for_suicide() {
        let mut mock_runtime = MockKernelHost::default();
        let block = dummy_first_block();
        let precompiles = precompiles::precompile_set::<MockKernelHost>(false);
        let mut evm_account_storage = init_account_storage().unwrap();
        // This test is specifically testing a Shanghai behaviour:
        let config = Config::shanghai();

        let caller = H160::from_low_u64_be(111_u64);

        let gas_price = U256::from(21000);

        let mut handler = EvmHandler::new(
            &mut mock_runtime,
            &mut evm_account_storage,
            caller,
            &block,
            &config,
            &precompiles,
            gas_price,
            None,
        );

        // SUICIDE would charge 25,000 gas when the destination is non-existent,
        // now the charge SHALL only be levied if the operation transfers more than zero value
        // and the destination account is dead.

        let cases = [
            (100_u32, 100_u8, 53603_u64), // transfer > 0 && non-existent destination
            (100, 100, 28603),            // transfer > 0 && touched destination
            (0, 101, 28603),              // transfer == 0 && non-existent destination
            (0, 101, 28603),              // transfer == 0 && touched destination
        ];

        for (balance, destination, expected_gas) in cases {
            let address = H160::from_low_u64_be(110_u64);
            set_balance(&mut handler, &address, U256::from(balance));
            set_code(
                &mut handler,
                &address,
                vec![
                    Opcode::PUSH1.as_u8(), // 3 gas
                    destination,
                    Opcode::SUICIDE.as_u8(), // 5000(BASE) + (25000 gas if transfer > 0 && destination is dead)
                ],
            );

            let result = handler
                .call_contract(
                    caller,
                    address,
                    Some(U256::zero()),
                    vec![],
                    Some(1000000),
                    false,
                )
                .unwrap();

            assert_eq!(result.gas_used, expected_gas);

            assert_eq!(
                result.result,
                ExecutionResult::CallSucceeded(ExitSucceed::Suicided, vec![])
            );

            // At the end of the transaction, any account touched by the execution of that transaction
            // which is now empty SHALL instead become non-existent (i.e. deleted).
            assert!(!handler.exists(address));
        }
    }

    #[test]
    fn eip161_gas_consumption_rules_for_call() {
        let mut mock_runtime = MockKernelHost::default();
        let block = dummy_first_block();
        let precompiles = precompiles::precompile_set::<MockKernelHost>(false);
        let mut evm_account_storage = init_account_storage().unwrap();
        let config = EVMVersion::current_test_config();

        let caller = H160::from_low_u64_be(111_u64);

        let gas_price = U256::from(21000);

        let mut handler = EvmHandler::new(
            &mut mock_runtime,
            &mut evm_account_storage,
            caller,
            &block,
            &config,
            &precompiles,
            gas_price,
            None,
        );

        // CALL would charge 25,000 gas when the destination is non-existent,
        // now the charge SHALL only be levied if the operation transfers more than zero value
        // and the destination account is dead.

        let address = H160::from_low_u64_be(110_u64);

        let cases = [
            (100_u32, 10_u8, 100_u8, 55321_u64), // transfer > 0 && non-existent destination
            (100, 10, 100, 30321),               // transfer > 0 && touched destination
            (100, 0, 101, 23621), // transfer == 0 && non-existent destination
            (100, 0, 101, 23621), // transger == 0 && touched destination
            (0, 10, 102, 55321), // unsufficient balance && transfer > 0 && non-existent destination
            (0, 10, 100, 30321), // unsufficient balance && transfer > 0 && touched destination
        ];

        for (balance, value, destination, expected_gas) in cases {
            set_balance(&mut handler, &address, U256::from(balance));
            set_code(
                &mut handler,
                &address,
                vec![
                    Opcode::PUSH1.as_u8(), // 3 gas
                    0,                     // return size
                    Opcode::PUSH1.as_u8(), // 3 gas
                    0,                     // return offset
                    Opcode::PUSH1.as_u8(), // 3 gas
                    0,                     // arg size
                    Opcode::PUSH1.as_u8(), // 3 gas
                    0,                     // arg offset
                    Opcode::PUSH1.as_u8(), // 3 gas
                    value,                 // non-zero value
                    Opcode::PUSH1.as_u8(), // 3 gas
                    destination,           // address
                    Opcode::PUSH1.as_u8(), // 3 gas
                    0,
                    // 100(BASE) + 9000(non-zero value cost)
                    // + (25000 gas if transfer > 0 && destination is dead) - 2300(call stipend)
                    Opcode::CALL.as_u8(),
                ],
            );

            let result = handler
                .call_contract(
                    caller,
                    address,
                    Some(U256::zero()),
                    vec![],
                    Some(1000000),
                    false,
                )
                .unwrap();

            assert_eq!(result.gas_used, expected_gas);

            assert_eq!(
                result.result,
                ExecutionResult::CallSucceeded(ExitSucceed::Stopped, vec![])
            );
        }
    }

    #[test]
    fn eip3529_check_gas_refund_is_done_at_the_end() {
        let mut mock_runtime = MockKernelHost::default();
        let block = dummy_first_block();
        let precompiles = precompiles::precompile_set::<MockKernelHost>(false);
        let mut evm_account_storage = init_account_storage().unwrap();
        let gas_price = U256::from(21000);
        let config = EVMVersion::current_test_config();
        let caller = H160::from_low_u64_be(666_u64);

        let mut handler = EvmHandler::new(
            &mut mock_runtime,
            &mut evm_account_storage,
            caller,
            &block,
            &config,
            &precompiles,
            gas_price,
            None,
        );

        set_balance(&mut handler, &caller, U256::from(100_000_u32));

        let address = H160::from_low_u64_be(999_u64);
        let index = H256::zero();
        let value = H256::from_low_u64_be(251195_u64); // non-zero value

        // {Small code sample to get a refund by restoring a storage slot}
        // PUSH1 0x00  <| Value to store (0)
        // PUSH1 0x00  <| Storage slot index (0)
        // SSTORE      <| Store 0 at slot 0
        // STOP
        let code = hex::decode("600060005500").unwrap();

        let mut account = handler.get_or_create_account(address).unwrap();
        account.set_storage(handler.host, &index, &value).unwrap();
        account.set_code(handler.host, &code).unwrap();

        let gas_limit = Some(100000);

        let call_context = CallContext {
            is_static: false,
            is_creation: false,
        };

        let transaction_context = TransactionContext::new(caller, address, U256::zero());

        handler
            .begin_initial_transaction(call_context, gas_limit)
            .unwrap();

        let execution_result =
            handler.execute_call(address, None, vec![], transaction_context);

        // At this point no refund is done

        let gas_used = handler.gas_used();
        assert_eq!(gas_used, 5006);

        let end_result = handler.end_initial_transaction(execution_result);

        // At this point refund is done as we ended the initial transaction

        match end_result {
            Ok(ExecutionOutcome { gas_used, .. }) => assert_eq!(gas_used, 4005),
            Err(_) => panic!("The transaction should have succeeded"),
        }
    }
}
