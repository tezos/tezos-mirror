// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
// SPDX-FileCopyrightText: 2023, 2025 Functori <contact@functori.com>
// SPDX-FileCopyrightText: 2022-2024 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2023-2024 PK Lab <contact@pklab.io>
//
// SPDX-License-Identifier: MIT

use ethereum::Log;
use evm::Config;
use evm_execution::account_storage::{EthereumAccount, EthereumAccountStorage};
use evm_execution::fa_bridge::deposit::FaDeposit;
use evm_execution::fa_bridge::queue_fa_deposit;
use evm_execution::handler::{
    ExecutionOutcome, FastWithdrawalInterface, RouterInterface,
};
use evm_execution::precompiles::{
    self, PrecompileBTreeMap, FA_BRIDGE_PRECOMPILE_ADDRESS,
};
use evm_execution::run_transaction;
use evm_execution::storage::tracer;
use evm_execution::trace::TracerInput::CallTracer;
use evm_execution::trace::{
    get_tracer_configuration, CallTrace, CallTracerConfig, CallTracerInput, TracerInput,
};
use primitive_types::{H160, H256, U256};
use tezos_ethereum::block::BlockConstants;
use tezos_ethereum::transaction::{TransactionHash, TransactionType};
use tezos_ethereum::tx_common::EthereumTransactionCommon;
use tezos_ethereum::tx_signature::TxSignature;
use tezos_evm_logging::{log, Level::*};
use tezos_evm_runtime::runtime::Runtime;
use tezos_smart_rollup::outbox::{OutboxMessage, OutboxQueue};
use tezos_smart_rollup_host::path::{Path, RefPath};

use crate::bridge::{execute_deposit, Deposit};
use crate::chains::EvmLimits;
use crate::error::Error;
use crate::fees::{tx_execution_gas_limit, FeeUpdates};
use crate::transaction::{Transaction, TransactionContent};

// This implementation of `Transaction` is used to share the logic of
// transaction receipt and transaction object making. The functions
// `make_receipt_info` and `make_object_info` use these functions to build
// the associated infos.
impl Transaction {
    fn to(&self) -> Option<H160> {
        match &self.content {
            TransactionContent::Deposit(Deposit { receiver, .. }) => Some(*receiver),
            TransactionContent::FaDeposit(FaDeposit { .. }) => {
                Some(FA_BRIDGE_PRECOMPILE_ADDRESS)
            }
            TransactionContent::Ethereum(transaction)
            | TransactionContent::EthereumDelayed(transaction) => transaction.to,
        }
    }

    fn data(&self) -> Vec<u8> {
        match &self.content {
            TransactionContent::Deposit(_) | TransactionContent::FaDeposit(_) => vec![],
            TransactionContent::Ethereum(transaction)
            | TransactionContent::EthereumDelayed(transaction) => {
                transaction.data.clone()
            }
        }
    }

    fn value(&self) -> U256 {
        match &self.content {
            TransactionContent::Deposit(Deposit { amount, .. }) => *amount,
            &TransactionContent::FaDeposit(_) => U256::zero(),
            TransactionContent::Ethereum(transaction)
            | TransactionContent::EthereumDelayed(transaction) => transaction.value,
        }
    }

    fn nonce(&self) -> u64 {
        match &self.content {
            TransactionContent::Deposit(_) | TransactionContent::FaDeposit(_) => 0,
            TransactionContent::Ethereum(transaction)
            | TransactionContent::EthereumDelayed(transaction) => transaction.nonce,
        }
    }

    fn signature(&self) -> Option<TxSignature> {
        match &self.content {
            TransactionContent::Deposit(_) | TransactionContent::FaDeposit(_) => None,
            TransactionContent::Ethereum(transaction)
            | TransactionContent::EthereumDelayed(transaction) => {
                transaction.signature.clone()
            }
        }
    }
}

pub struct TransactionReceiptInfo {
    pub tx_hash: TransactionHash,
    pub index: u32,
    pub execution_outcome: Option<ExecutionOutcome>,
    pub caller: H160,
    pub to: Option<H160>,
    pub effective_gas_price: U256,
    pub type_: TransactionType,
    pub overall_gas_used: U256,
}

/// Details about the original transaction.
///
/// See <https://ethereum.org/en/developers/docs/apis/json-rpc/#eth_gettransactionbyhash>
/// for more details.
#[derive(Debug)]
pub struct TransactionObjectInfo {
    pub from: H160,
    /// Gas provided by the sender
    pub gas: U256,
    /// Gas price provided by the sender
    pub gas_price: U256,
    pub hash: TransactionHash,
    pub input: Vec<u8>,
    pub nonce: u64,
    pub to: Option<H160>,
    pub index: u32,
    pub value: U256,
    pub signature: Option<TxSignature>,
}

#[inline(always)]
#[allow(clippy::too_many_arguments)]
fn make_receipt_info(
    tx_hash: TransactionHash,
    index: u32,
    execution_outcome: Option<ExecutionOutcome>,
    caller: H160,
    to: Option<H160>,
    effective_gas_price: U256,
    type_: TransactionType,
    overall_gas_used: U256,
) -> TransactionReceiptInfo {
    TransactionReceiptInfo {
        tx_hash,
        index,
        execution_outcome,
        caller,
        to,
        effective_gas_price,
        type_,
        overall_gas_used,
    }
}

#[inline(always)]
fn make_object_info(
    transaction: &Transaction,
    from: H160,
    index: u32,
    fee_updates: &FeeUpdates,
) -> Result<TransactionObjectInfo, anyhow::Error> {
    let (gas, gas_price) = match &transaction.content {
        TransactionContent::Ethereum(e) | TransactionContent::EthereumDelayed(e) => {
            (e.gas_limit_with_fees().into(), e.max_fee_per_gas)
        }
        TransactionContent::Deposit(_) | TransactionContent::FaDeposit(_) => {
            (fee_updates.overall_gas_used, fee_updates.overall_gas_price)
        }
    };

    Ok(TransactionObjectInfo {
        from,
        gas,
        gas_price,
        hash: transaction.tx_hash,
        input: transaction.data(),
        nonce: transaction.nonce(),
        to: transaction.to(),
        index,
        value: transaction.value(),
        signature: transaction.signature(),
    })
}

fn account<Host: Runtime>(
    host: &mut Host,
    caller: H160,
    evm_account_storage: &mut EthereumAccountStorage,
) -> Result<Option<EthereumAccount>, Error> {
    let caller_account_path = evm_execution::account_storage::account_path(&caller)?;
    Ok(evm_account_storage.get(host, &caller_account_path)?)
}

#[derive(Debug, PartialEq)]
pub enum Validity {
    Valid(H160, u64),
    InvalidChainId,
    InvalidSignature,
    InvalidNonce,
    InvalidPrePay,
    InvalidCode,
    InvalidMaxBaseFee,
    InvalidNotEnoughGasForFees,
}

// TODO: https://gitlab.com/tezos/tezos/-/issues/6812
//       arguably, effective_gas_price should be set on EthereumTransactionCommon
//       directly - initialised when constructed.
fn is_valid_ethereum_transaction_common<Host: Runtime>(
    host: &mut Host,
    evm_account_storage: &mut EthereumAccountStorage,
    transaction: &EthereumTransactionCommon,
    block_constant: &BlockConstants,
    effective_gas_price: U256,
    is_delayed: bool,
    limits: &EvmLimits,
) -> Result<Validity, Error> {
    // Chain id is correct.
    if transaction.chain_id.is_some()
        && Some(block_constant.chain_id) != transaction.chain_id
    {
        log!(host, Benchmarking, "Transaction status: ERROR_CHAINID");
        return Ok(Validity::InvalidChainId);
    }

    // ensure that the user was willing to at least pay the base fee
    if transaction.max_fee_per_gas < block_constant.base_fee_per_gas() {
        log!(host, Benchmarking, "Transaction status: ERROR_MAX_BASE_FEE");
        return Ok(Validity::InvalidMaxBaseFee);
    }

    // The transaction signature is valid.
    let caller = match transaction.caller() {
        Ok(caller) => caller,
        Err(_err) => {
            log!(host, Benchmarking, "Transaction status: ERROR_SIGNATURE.");
            // Transaction with undefined caller are ignored, i.e. the caller
            // could not be derived from the signature.
            return Ok(Validity::InvalidSignature);
        }
    };

    let account = account(host, caller, evm_account_storage)?;

    let (nonce, balance, code_exists): (u64, U256, bool) = match account {
        None => (0, U256::zero(), false),
        Some(account) => (
            account.nonce(host)?,
            account.balance(host)?,
            account.code_exists(host)?,
        ),
    };

    // The transaction nonce is valid.
    if nonce != transaction.nonce {
        log!(host, Benchmarking, "Transaction status: ERROR_NONCE.");
        return Ok(Validity::InvalidNonce);
    };

    // The sender account balance contains at least the cost.
    let total_gas_limit = U256::from(transaction.gas_limit_with_fees());
    let cost = total_gas_limit.saturating_mul(effective_gas_price);
    // The sender can afford the max gas fee he set, see EIP-1559
    let max_fee = total_gas_limit.saturating_mul(transaction.max_fee_per_gas);

    if balance < cost || balance < max_fee {
        log!(host, Benchmarking, "Transaction status: ERROR_PRE_PAY.");
        return Ok(Validity::InvalidPrePay);
    }

    // The sender does not have code, see EIP3607.
    if code_exists {
        log!(host, Benchmarking, "Transaction status: ERROR_CODE.");
        return Ok(Validity::InvalidCode);
    }

    // check that enough gas is provided to cover fees
    let Ok(gas_limit) =
        tx_execution_gas_limit(transaction, &block_constant.block_fees, is_delayed)
    else {
        log!(host, Benchmarking, "Transaction status: ERROR_GAS_FEE.");
        return Ok(Validity::InvalidNotEnoughGasForFees);
    };
    let capped_gas_limit = u64::min(gas_limit, limits.maximum_gas_limit);
    Ok(Validity::Valid(caller, capped_gas_limit))
}

pub struct TransactionResult {
    caller: H160,
    execution_outcome: Option<ExecutionOutcome>,
    gas_used: U256,
    estimated_ticks_used: u64,
}

/// Technically incorrect: it is possible to do a call without sending any data,
/// however it's done for benchmarking only, and benchmarking doesn't include
/// such a scenario
fn log_transaction_type<Host: Runtime>(host: &Host, to: Option<H160>, data: &[u8]) {
    if to.is_none() {
        log!(host, Benchmarking, "Transaction type: CREATE");
    } else if data.is_empty() {
        log!(host, Benchmarking, "Transaction type: TRANSFER");
    } else {
        log!(host, Benchmarking, "Transaction type: CALL");
    }
}

#[allow(clippy::too_many_arguments)]
fn apply_ethereum_transaction_common<Host: Runtime>(
    host: &mut Host,
    block_constants: &BlockConstants,
    precompiles: &PrecompileBTreeMap<Host>,
    evm_account_storage: &mut EthereumAccountStorage,
    transaction: &EthereumTransactionCommon,
    is_delayed: bool,
    tracer_input: Option<TracerInput>,
    evm_configuration: &Config,
    limits: &EvmLimits,
) -> Result<ExecutionResult<TransactionResult>, anyhow::Error> {
    let effective_gas_price = block_constants.base_fee_per_gas();
    let (caller, gas_limit) = match is_valid_ethereum_transaction_common(
        host,
        evm_account_storage,
        transaction,
        block_constants,
        effective_gas_price,
        is_delayed,
        limits,
    )? {
        Validity::Valid(caller, gas_limit) => (caller, gas_limit),
        _reason => {
            log!(host, Benchmarking, "Transaction type: INVALID");
            return Ok(ExecutionResult::Invalid);
        }
    };

    let to = transaction.to;
    let call_data = transaction.data.clone();
    log_transaction_type(host, to, &call_data);
    let value = transaction.value;
    let execution_outcome = match run_transaction(
        host,
        block_constants,
        evm_account_storage,
        precompiles,
        evm_configuration,
        to,
        caller,
        call_data,
        Some(gas_limit),
        effective_gas_price,
        value,
        true,
        tracer_input,
    ) {
        Ok(outcome) => outcome,
        Err(err) => {
            return Err(Error::InvalidRunTransaction(err).into());
        }
    };

    let (gas_used, estimated_ticks_used) = match &execution_outcome {
        Some(execution_outcome) => {
            log!(
                host,
                Benchmarking,
                "Transaction status: OK_{}.",
                execution_outcome.is_success()
            );
            (
                execution_outcome.gas_used.into(),
                execution_outcome.estimated_ticks_used,
            )
        }
        None => {
            log!(host, Benchmarking, "Transaction status: OK_UNKNOWN.");
            (U256::zero(), 0)
        }
    };

    let transaction_result = TransactionResult {
        caller,
        execution_outcome,
        gas_used,
        estimated_ticks_used,
    };

    Ok(ExecutionResult::Valid(transaction_result))
}

fn trace_deposit<Host: Runtime>(
    host: &mut Host,
    amount: U256,
    receiver: Option<H160>,
    gas_used: u64,
    logs: &[Log],
    tracer_input: Option<TracerInput>,
) {
    if let Some(CallTracer(CallTracerInput {
        transaction_hash,
        config: CallTracerConfig { with_logs, .. },
    })) = tracer_input
    {
        let mut call_trace = CallTrace::new_minimal_trace(
            "CALL".into(),
            H160::zero(),
            amount,
            gas_used,
            vec![],
            0,
        );

        call_trace.add_to(receiver);

        if with_logs {
            call_trace.add_logs(Some(logs.to_owned()))
        }

        let _ = tracer::store_call_trace(host, call_trace, &transaction_hash);
    }
}

fn apply_deposit<Host: Runtime>(
    host: &mut Host,
    evm_account_storage: &mut EthereumAccountStorage,
    deposit: &Deposit,
    transaction: &Transaction,
    tracer_input: Option<TracerInput>,
    evm_configuration: &Config,
) -> Result<ExecutionResult<TransactionResult>, Error> {
    let execution_outcome =
        execute_deposit(host, evm_account_storage, deposit, evm_configuration)
            .map_err(Error::InvalidRunTransaction)?;

    trace_deposit(
        host,
        transaction.value(),
        transaction.to(),
        execution_outcome.gas_used,
        &execution_outcome.logs,
        tracer_input,
    );

    Ok(ExecutionResult::Valid(TransactionResult {
        caller: H160::zero(),
        gas_used: execution_outcome.gas_used.into(),
        estimated_ticks_used: execution_outcome.estimated_ticks_used,
        execution_outcome: Some(execution_outcome),
    }))
}

#[allow(clippy::too_many_arguments)]
fn apply_fa_deposit<Host: Runtime>(
    host: &mut Host,
    evm_account_storage: &mut EthereumAccountStorage,
    fa_deposit: &FaDeposit,
    block_constants: &BlockConstants,
    transaction: &Transaction,
    tracer_input: Option<TracerInput>,
    evm_configuration: &Config,
) -> Result<ExecutionResult<TransactionResult>, Error> {
    let caller = H160::zero();
    // Prevent inner calls to XTZ/FA withdrawal precompiles
    let precompiles = precompiles::precompile_set_with_revert_withdrawals(true);
    let (outcome, _) = queue_fa_deposit(
        host,
        block_constants,
        evm_account_storage,
        &precompiles,
        evm_configuration,
        caller,
        fa_deposit,
        tracer_input,
    )
    .map_err(Error::InvalidRunTransaction)?;

    log!(
        host,
        Benchmarking,
        "Transaction status: OK_{}.",
        outcome.is_success()
    );

    trace_deposit(
        host,
        transaction.value(),
        transaction.to(),
        outcome.gas_used,
        &outcome.logs,
        tracer_input,
    );

    Ok(ExecutionResult::Valid(TransactionResult {
        caller,
        gas_used: outcome.gas_used.into(),
        estimated_ticks_used: outcome.estimated_ticks_used,
        execution_outcome: Some(outcome),
    }))
}

pub const WITHDRAWAL_OUTBOX_QUEUE: RefPath =
    RefPath::assert_from(b"/evm/world_state/__outbox_queue");

pub struct ExecutionInfo {
    pub receipt_info: TransactionReceiptInfo,
    pub object_info: TransactionObjectInfo,
    pub estimated_ticks_used: u64,
    pub execution_gas_used: U256,
}

pub enum ExecutionResult<T> {
    Valid(T),
    Invalid,
}

impl<T> From<Option<T>> for ExecutionResult<T> {
    fn from(opt: Option<T>) -> ExecutionResult<T> {
        match opt {
            Some(v) => ExecutionResult::Valid(v),
            None => ExecutionResult::Invalid,
        }
    }
}

#[allow(clippy::too_many_arguments)]
pub fn handle_transaction_result<Host: Runtime>(
    host: &mut Host,
    outbox_queue: &OutboxQueue<'_, impl Path>,
    block_constants: &BlockConstants,
    transaction: &Transaction,
    index: u32,
    evm_account_storage: &mut EthereumAccountStorage,
    transaction_result: TransactionResult,
    pay_fees: bool,
    sequencer_pool_address: Option<H160>,
) -> Result<ExecutionInfo, anyhow::Error> {
    let TransactionResult {
        caller,
        mut execution_outcome,
        gas_used,
        estimated_ticks_used: ticks_used,
    } = transaction_result;

    let to = transaction.to();

    let fee_updates = transaction
        .content
        .fee_updates(&block_constants.block_fees, gas_used);

    if let Some(outcome) = &mut execution_outcome {
        log!(host, Debug, "Transaction executed, outcome: {:?}", outcome);
        log!(host, Benchmarking, "gas_used: {:?}", outcome.gas_used);
        log!(host, Benchmarking, "reason: {:?}", outcome.result);
        for message in outcome.withdrawals.drain(..) {
            match message {
                evm_execution::handler::Withdrawal::Standard(message) => {
                    let outbox_message: OutboxMessage<RouterInterface> = message;
                    let len = outbox_queue.queue_message(host, outbox_message)?;
                    log!(host, Debug, "Length of the outbox queue: {}", len);
                }
                evm_execution::handler::Withdrawal::Fast(message) => {
                    let outbox_message: OutboxMessage<FastWithdrawalInterface> = message;
                    let len = outbox_queue.queue_message(host, outbox_message)?;
                    log!(host, Debug, "Length of the outbox queue: {}", len);
                }
            }
        }
    }

    if pay_fees {
        fee_updates.apply(host, evm_account_storage, caller, sequencer_pool_address)?;
    }

    let object_info = make_object_info(transaction, caller, index, &fee_updates)?;

    let receipt_info = make_receipt_info(
        transaction.tx_hash,
        index,
        execution_outcome,
        caller,
        to,
        fee_updates.overall_gas_price,
        transaction.type_(),
        fee_updates.overall_gas_used,
    );

    Ok(ExecutionInfo {
        receipt_info,
        object_info,
        estimated_ticks_used: ticks_used,
        execution_gas_used: gas_used,
    })
}

#[allow(clippy::too_many_arguments)]
pub fn apply_transaction<Host: Runtime>(
    host: &mut Host,
    outbox_queue: &OutboxQueue<'_, impl Path>,
    block_constants: &BlockConstants,
    precompiles: &PrecompileBTreeMap<Host>,
    transaction: &Transaction,
    index: u32,
    evm_account_storage: &mut EthereumAccountStorage,
    sequencer_pool_address: Option<H160>,
    tracer_input: Option<TracerInput>,
    evm_configuration: &Config,
    limits: &EvmLimits,
) -> Result<ExecutionResult<ExecutionInfo>, anyhow::Error> {
    let tracer_input = get_tracer_configuration(H256(transaction.tx_hash), tracer_input);
    let apply_result = match &transaction.content {
        TransactionContent::Ethereum(tx) => apply_ethereum_transaction_common(
            host,
            block_constants,
            precompiles,
            evm_account_storage,
            tx,
            false,
            tracer_input,
            evm_configuration,
            limits,
        )?,
        TransactionContent::EthereumDelayed(tx) => apply_ethereum_transaction_common(
            host,
            block_constants,
            precompiles,
            evm_account_storage,
            tx,
            true,
            tracer_input,
            evm_configuration,
            limits,
        )?,
        TransactionContent::Deposit(deposit) => {
            log!(host, Benchmarking, "Transaction type: DEPOSIT");
            apply_deposit(
                host,
                evm_account_storage,
                deposit,
                transaction,
                tracer_input,
                evm_configuration,
            )?
        }
        TransactionContent::FaDeposit(fa_deposit) => {
            log!(host, Benchmarking, "Transaction type: FA_DEPOSIT");
            apply_fa_deposit(
                host,
                evm_account_storage,
                fa_deposit,
                block_constants,
                transaction,
                tracer_input,
                evm_configuration,
            )?
        }
    };

    match apply_result {
        ExecutionResult::Valid(tx_result) => {
            let execution_result = handle_transaction_result(
                host,
                outbox_queue,
                block_constants,
                transaction,
                index,
                evm_account_storage,
                tx_result,
                true,
                sequencer_pool_address,
            )?;
            Ok(ExecutionResult::Valid(execution_result))
        }
        ExecutionResult::Invalid => Ok(ExecutionResult::Invalid),
    }
}

#[cfg(test)]
mod tests {

    use crate::{apply::Validity, chains::EvmLimits, fees::gas_for_fees};
    use evm_execution::account_storage::{account_path, EthereumAccountStorage};
    use primitive_types::{H160, U256};
    use tezos_ethereum::{
        block::{BlockConstants, BlockFees},
        transaction::TransactionType,
        tx_common::EthereumTransactionCommon,
    };
    use tezos_evm_runtime::runtime::MockKernelHost;
    use tezos_smart_rollup_encoding::timestamp::Timestamp;

    use super::is_valid_ethereum_transaction_common;

    const CHAIN_ID: u32 = 1337;

    fn mock_block_constants() -> BlockConstants {
        let block_fees = BlockFees::new(
            U256::from(12345),
            U256::from(12345),
            U256::from(2_000_000_000_000u64),
        );
        BlockConstants::first_block(
            U256::from(Timestamp::from(0).as_u64()),
            CHAIN_ID.into(),
            block_fees,
            crate::block::GAS_LIMIT,
            H160::zero(),
        )
    }

    fn address_from_str(s: &str) -> H160 {
        let data = &hex::decode(s).unwrap();
        H160::from_slice(data)
    }

    fn set_balance(
        host: &mut MockKernelHost,
        evm_account_storage: &mut EthereumAccountStorage,
        address: &H160,
        balance: U256,
    ) {
        let mut account = evm_account_storage
            .get_or_create(host, &account_path(address).unwrap())
            .unwrap();
        let current_balance = account.balance(host).unwrap();
        if current_balance > balance {
            account
                .balance_remove(host, current_balance - balance)
                .unwrap();
        } else {
            account
                .balance_add(host, balance - current_balance)
                .unwrap();
        }
    }

    fn resign(transaction: EthereumTransactionCommon) -> EthereumTransactionCommon {
        // corresponding caller's address is 0xaf1276cbb260bb13deddb4209ae99ae6e497f446
        let private_key =
            "dcdff53b4f013dbcdc717f89fe3bf4d8b10512aae282b48e01d7530470382701";
        transaction
            .sign_transaction(private_key.to_string())
            .expect("Should have been able to sign")
    }

    fn valid_tx(gas_limit: u64) -> EthereumTransactionCommon {
        let transaction = EthereumTransactionCommon::new(
            TransactionType::Eip1559,
            Some(CHAIN_ID.into()),
            0,
            U256::zero(),
            U256::from(21000),
            gas_limit,
            Some(H160::zero()),
            U256::zero(),
            vec![],
            vec![],
            None,
        );
        // sign tx
        resign(transaction)
    }

    fn gas_for_fees_no_data(block_constants: &BlockConstants) -> u64 {
        gas_for_fees(
            block_constants.block_fees.da_fee_per_byte(),
            block_constants.block_fees.minimum_base_fee_per_gas(),
            vec![].as_slice(),
            vec![].as_slice(),
        )
        .expect("should have been able to calculate fees")
    }

    #[test]
    fn test_tx_is_valid() {
        let mut host = MockKernelHost::default();
        let mut evm_account_storage =
            evm_execution::account_storage::init_account_storage().unwrap();
        let block_constants = mock_block_constants();
        // setup
        let address = address_from_str("af1276cbb260bb13deddb4209ae99ae6e497f446");
        let gas_price = U256::from(21000);
        let fee_gas = gas_for_fees_no_data(&block_constants);
        let balance = U256::from(fee_gas + 21000) * gas_price;
        let gas_limit = 21000 + fee_gas;
        let transaction = valid_tx(gas_limit);
        // fund account
        set_balance(&mut host, &mut evm_account_storage, &address, balance);

        // act
        let res = is_valid_ethereum_transaction_common(
            &mut host,
            &mut evm_account_storage,
            &transaction,
            &block_constants,
            gas_price,
            false,
            &EvmLimits::default(),
        );
        assert_eq!(
            Validity::Valid(address, 21000),
            res.expect("Verification should not have raise an error"),
            "Transaction should have been rejected"
        );
    }

    #[test]
    fn test_tx_is_invalid_cannot_prepay() {
        let mut host = MockKernelHost::default();
        let mut evm_account_storage =
            evm_execution::account_storage::init_account_storage().unwrap();
        let block_constants = mock_block_constants();

        // setup
        let address = address_from_str("af1276cbb260bb13deddb4209ae99ae6e497f446");
        let gas_price = U256::from(21000);
        let fee_gas = gas_for_fees_no_data(&block_constants);
        // account doesnt have enough funds for execution
        let balance = U256::from(fee_gas) * gas_price;
        let gas_limit = 21000 + fee_gas;
        let transaction = valid_tx(gas_limit);
        // fund account
        set_balance(&mut host, &mut evm_account_storage, &address, balance);

        // act
        let res = is_valid_ethereum_transaction_common(
            &mut host,
            &mut evm_account_storage,
            &transaction,
            &block_constants,
            gas_price,
            false,
            &EvmLimits::default(),
        );
        assert_eq!(
            Validity::InvalidPrePay,
            res.expect("Verification should not have raise an error"),
            "Transaction should have been rejected"
        );
    }

    #[test]
    fn test_tx_is_invalid_signature() {
        let mut host = MockKernelHost::default();
        let mut evm_account_storage =
            evm_execution::account_storage::init_account_storage().unwrap();
        let block_constants = mock_block_constants();

        // setup
        let address = address_from_str("af1276cbb260bb13deddb4209ae99ae6e497f446");
        let gas_price = U256::from(21000);
        let fee_gas = gas_for_fees_no_data(&block_constants);
        let balance = U256::from(fee_gas + 21000) * gas_price;
        let gas_limit = 21000 + fee_gas;
        let mut transaction = valid_tx(gas_limit);
        transaction.signature = None;
        // fund account
        set_balance(&mut host, &mut evm_account_storage, &address, balance);

        // act
        let res = is_valid_ethereum_transaction_common(
            &mut host,
            &mut evm_account_storage,
            &transaction,
            &block_constants,
            gas_price,
            false,
            &EvmLimits::default(),
        );
        assert_eq!(
            Validity::InvalidSignature,
            res.expect("Verification should not have raise an error"),
            "Transaction should have been rejected"
        );
    }

    #[test]
    fn test_tx_is_invalid_wrong_nonce() {
        let mut host = MockKernelHost::default();
        let mut evm_account_storage =
            evm_execution::account_storage::init_account_storage().unwrap();
        let block_constants = mock_block_constants();

        // setup
        let address = address_from_str("af1276cbb260bb13deddb4209ae99ae6e497f446");
        let gas_price = U256::from(21000);
        let fee_gas = gas_for_fees_no_data(&block_constants);
        let balance = U256::from(fee_gas + 21000) * gas_price;
        let gas_limit = 21000 + fee_gas;
        let mut transaction = valid_tx(gas_limit);
        transaction.nonce = 42;
        transaction = resign(transaction);

        // fund account
        set_balance(&mut host, &mut evm_account_storage, &address, balance);

        // act
        let res = is_valid_ethereum_transaction_common(
            &mut host,
            &mut evm_account_storage,
            &transaction,
            &block_constants,
            gas_price,
            false,
            &EvmLimits::default(),
        );
        assert_eq!(
            Validity::InvalidNonce,
            res.expect("Verification should not have raise an error"),
            "Transaction should have been rejected"
        );
    }

    #[test]
    fn test_tx_is_invalid_wrong_chain_id() {
        let mut host = MockKernelHost::default();
        let mut evm_account_storage =
            evm_execution::account_storage::init_account_storage().unwrap();
        let block_constants = mock_block_constants();

        // setup
        let address = address_from_str("af1276cbb260bb13deddb4209ae99ae6e497f446");
        let gas_price = U256::from(21000);
        let balance = U256::from(21000) * gas_price;
        let mut transaction = valid_tx(1);
        transaction.chain_id = Some(U256::from(42));
        transaction = resign(transaction);

        // fund account
        set_balance(&mut host, &mut evm_account_storage, &address, balance);

        // act
        let res = is_valid_ethereum_transaction_common(
            &mut host,
            &mut evm_account_storage,
            &transaction,
            &block_constants,
            gas_price,
            false,
            &EvmLimits::default(),
        );
        assert_eq!(
            Validity::InvalidChainId,
            res.expect("Verification should not have raise an error"),
            "Transaction should have been rejected"
        );
    }

    #[test]
    fn test_tx_is_invalid_max_fee_less_than_base_fee() {
        let mut host = MockKernelHost::default();
        let mut evm_account_storage =
            evm_execution::account_storage::init_account_storage().unwrap();
        let block_constants = mock_block_constants();

        // setup
        let gas_price = U256::from(21000);
        let max_gas_price = U256::one();
        // account doesnt have enough funds for execution
        let fee_gas = gas_for_fees_no_data(&block_constants);
        let gas_limit = 21000 + fee_gas;
        let mut transaction = valid_tx(gas_limit);
        // set a max base fee too low
        transaction.max_fee_per_gas = max_gas_price;
        transaction = resign(transaction);

        // act
        let res = is_valid_ethereum_transaction_common(
            &mut host,
            &mut evm_account_storage,
            &transaction,
            &block_constants,
            gas_price,
            false,
            &EvmLimits::default(),
        );
        assert_eq!(
            Validity::InvalidMaxBaseFee,
            res.expect("Verification should not have raise an error"),
            "Transaction should have been rejected"
        );
    }

    #[test]
    fn test_tx_invalid_not_enough_gas_for_fee() {
        let mut host = MockKernelHost::default();
        let mut evm_account_storage =
            evm_execution::account_storage::init_account_storage().unwrap();
        let block_constants = mock_block_constants();

        // setup
        let address = address_from_str("af1276cbb260bb13deddb4209ae99ae6e497f446");
        let gas_price = U256::from(21000);
        let balance = U256::from(21000) * gas_price;
        // fund account
        set_balance(&mut host, &mut evm_account_storage, &address, balance);

        let gas_limit = 21000; // gas limit is not enough to cover fees
        let mut transaction = valid_tx(gas_limit);
        transaction.data = vec![1u8];
        transaction = resign(transaction);

        // act
        let res = is_valid_ethereum_transaction_common(
            &mut host,
            &mut evm_account_storage,
            &transaction,
            &block_constants,
            gas_price,
            false,
            &EvmLimits::default(),
        );
        assert_eq!(
            Validity::InvalidNotEnoughGasForFees,
            res.expect("Verification should not have raise an error"),
            "Transaction should have been rejected"
        );

        let res = is_valid_ethereum_transaction_common(
            &mut host,
            &mut evm_account_storage,
            &transaction,
            &block_constants,
            gas_price,
            true,
            &EvmLimits::default(),
        );
        assert!(
            matches!(
                res.expect("Verification should not have raise an error"),
                Validity::Valid(_, _)
            ),
            "Transaction should have been accepted through delayed inbox"
        );
    }
}
