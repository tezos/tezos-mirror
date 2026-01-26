// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
// SPDX-FileCopyrightText: 2023, 2025-2026 Functori <contact@functori.com>
// SPDX-FileCopyrightText: 2022-2024 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2023-2024 PK Lab <contact@pklab.io>
//
// SPDX-License-Identifier: MIT

use alloy_sol_types::{sol, SolCall};
use primitive_types::{H160, U256};
use revm::primitives::hardfork::SpecId;
use revm::primitives::{Address, Bytes, B256};
use revm_etherlink::helpers::legacy::{alloy_to_h160, FaDeposit, FaDepositWithProxy};
use revm_etherlink::inspectors::call_tracer::CallTracerInput;
use revm_etherlink::inspectors::struct_logger::StructLoggerInput;
use revm_etherlink::inspectors::{get_tracer_configuration, TracerInput};
use revm_etherlink::precompiles::constants::{
    FA_BRIDGE_SOL_ADDR, FA_DEPOSIT_EXECUTION_COST, FEED_DEPOSIT_ADDR,
    XTZ_BRIDGE_SOL_ADDR, XTZ_DEPOSIT_EXECUTION_COST,
};
use revm_etherlink::precompiles::send_outbox_message::{
    FastWithdrawalInterface, RouterInterface, Withdrawal,
};
use revm_etherlink::storage::world_state_handler::StorageAccount;
use revm_etherlink::GasData;
use revm_etherlink::{
    helpers::legacy::{h160_to_alloy, u256_to_alloy},
    ExecutionOutcome,
};
use tezos_ethereum::access_list::{AccessList, AccessListItem};
use tezos_ethereum::block::{BlockConstants, BlockFees};
use tezos_ethereum::transaction::{
    TransactionHash, TransactionType, TRANSACTION_HASH_SIZE,
};
use tezos_ethereum::tx_common::{
    signed_authorization, AuthorizationList, EthereumTransactionCommon,
};
use tezos_ethereum::tx_signature::TxSignature;
use tezos_evm_logging::{log, tracing::instrument, Level::*};
use tezos_evm_runtime::runtime::Runtime;
use tezos_smart_rollup::outbox::{OutboxMessage, OutboxQueue};
use tezos_smart_rollup_host::path::{Path, RefPath};
use tezos_tracing::trace_kernel;

use crate::bridge::{execute_etherlink_deposit, Deposit};
use crate::chains::EvmLimits;
use crate::error::Error;
use crate::fees::{tx_execution_gas_limit, FeeUpdates};
use crate::transaction::{Transaction, TransactionContent};

pub struct TransactionReceiptInfo {
    pub tx_hash: TransactionHash,
    pub index: u32,
    pub execution_outcome: ExecutionOutcome,
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
    execution_outcome: ExecutionOutcome,
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
        to: transaction.to()?,
        index,
        value: transaction.value(),
        signature: transaction.signature(),
    })
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
#[instrument(skip_all)]
pub fn is_valid_ethereum_transaction_common<Host: Runtime>(
    host: &mut Host,
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

    let account = StorageAccount::from_address(&h160_to_alloy(&caller))?;
    let info = account.info(host)?;

    // The transaction nonce is valid.
    if info.nonce != transaction.nonce {
        log!(host, Benchmarking, "Transaction status: ERROR_NONCE.");
        return Ok(Validity::InvalidNonce);
    };

    // The sender account balance contains at least the cost.
    let total_gas_limit = U256::from(transaction.gas_limit_with_fees());
    let cost = total_gas_limit.saturating_mul(effective_gas_price);
    // The sender can afford the max gas fee he set, see EIP-1559
    let max_fee = total_gas_limit.saturating_mul(transaction.max_fee_per_gas);

    if info.balance < u256_to_alloy(&cost) || info.balance < u256_to_alloy(&max_fee) {
        log!(host, Benchmarking, "Transaction status: ERROR_PRE_PAY.");
        return Ok(Validity::InvalidPrePay);
    }

    if let Some(code) = revm_etherlink::storage::code::CodeStorage::new(&info.code_hash)?
        .get_code(host)?
    {
        // The sender does not have code (EIP-3607) or isn't an EIP-7702 authorized account.
        if !code.is_empty()
            && !code.original_byte_slice().starts_with(&[0xef, 0x01, 0x00])
        {
            log!(host, Benchmarking, "Transaction status: ERROR_CODE.");
            return Ok(Validity::InvalidCode);
        }
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
    pub caller: H160,
    pub execution_outcome: ExecutionOutcome,
    pub gas_used: U256,
    pub estimated_ticks_used: u64,
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

#[trace_kernel]
#[allow(clippy::too_many_arguments)]
#[instrument(skip_all)]
pub fn revm_run_transaction<Host: Runtime>(
    host: &mut Host,
    block_constants: &BlockConstants,
    transaction_hash: Option<[u8; TRANSACTION_HASH_SIZE]>,
    caller: H160,
    to: Option<H160>,
    value: U256,
    call_data: Vec<u8>,
    gas_limit: u64,
    effective_gas_price: U256,
    maximum_gas_per_transaction: u64,
    access_list: AccessList,
    authorization_list: Option<AuthorizationList>,
    spec_id: &SpecId,
    tracer_input: Option<TracerInput>,
    is_simulation: bool,
) -> Result<ExecutionOutcome, anyhow::Error> {
    // Disclaimer:
    // The following code is over-complicated because we maintain
    // two sets of primitives inside the kernel's codebase.
    // There's a lot of dummy conversions that are
    // needed to make the translation from our type to the
    // ones from REVM (and the other way round).
    //
    // NB:
    // None of the revm primitives are imported globally on purpose to
    // avoid disturbing the workflow of other engineers. This part of the
    // code is extremely self-contained on purpose.
    //
    // TODO: Simplify all the base structures to avoid these translations
    // once we fully make the switch to REVM.
    let mut bytes = vec![0u8; 32];
    value.to_little_endian(&mut bytes);
    let effective_gas_price = u128::from_le_bytes(if effective_gas_price.bits() < 128 {
        effective_gas_price.low_u128().to_le_bytes()
    } else {
        return Err(Error::InvalidRunTransaction(revm_etherlink::Error::Custom(
            "Given amount does not fit in a u128".to_string(),
        ))
        .into());
    });
    let gas_data =
        GasData::new(gas_limit, effective_gas_price, maximum_gas_per_transaction);
    revm_etherlink::run_transaction(
        host,
        *spec_id,
        block_constants,
        transaction_hash,
        Address::from_slice(&caller.0),
        to.map(|to| Address::from_slice(&to.0)),
        Bytes::from(call_data),
        gas_data,
        revm::primitives::U256::from_le_slice(&bytes),
        revm::context::transaction::AccessList::from(
            access_list
                .into_iter()
                .map(
                    |AccessListItem {
                         address,
                         storage_keys,
                     }| {
                        revm::context::transaction::AccessListItem {
                            address: Address::from_slice(&address.0),
                            storage_keys: storage_keys
                                .into_iter()
                                .map(|key| B256::from_slice(&key.0))
                                .collect(),
                        }
                    },
                )
                .collect::<Vec<revm::context::transaction::AccessListItem>>(),
        ),
        authorization_list.map(signed_authorization),
        tracer_input.map(|tracer_input| match tracer_input {
            TracerInput::CallTracer(CallTracerInput {
                transaction_hash,
                config,
            }) => revm_etherlink::inspectors::TracerInput::CallTracer(
                revm_etherlink::inspectors::call_tracer::CallTracerInput {
                    config: revm_etherlink::inspectors::call_tracer::CallTracerConfig {
                        only_top_call: config.only_top_call,
                        with_logs: config.with_logs,
                    },
                    transaction_hash: transaction_hash.map(|hash| B256::from(hash.0)),
                },
            ),
            TracerInput::StructLogger(StructLoggerInput {
                transaction_hash,
                config,
            }) => revm_etherlink::inspectors::TracerInput::StructLogger(
                revm_etherlink::inspectors::struct_logger::StructLoggerInput {
                    config:
                        revm_etherlink::inspectors::struct_logger::StructLoggerConfig {
                            enable_memory: config.enable_memory,
                            enable_return_data: config.enable_return_data,
                            disable_stack: config.disable_stack,
                            disable_storage: config.disable_storage,
                        },
                    transaction_hash: transaction_hash.map(|hash| B256::from(hash.0)),
                },
            ),
            TracerInput::NoOp => revm_etherlink::inspectors::TracerInput::NoOp,
        }),
        is_simulation,
    )
    .map_err(|err| {
        Error::InvalidRunTransaction(revm_etherlink::Error::Custom(format!(
            "REVM error {err:?}"
        )))
        .into()
    })
}

#[allow(clippy::too_many_arguments)]
#[instrument(skip_all)]
fn apply_ethereum_transaction_common<Host: Runtime>(
    host: &mut Host,
    block_constants: &BlockConstants,
    transaction: &EthereumTransactionCommon,
    transaction_hash: [u8; TRANSACTION_HASH_SIZE],
    is_delayed: bool,
    tracer_input: Option<TracerInput>,
    spec_id: &SpecId,
    limits: &EvmLimits,
) -> Result<ExecutionResult<TransactionResult>, anyhow::Error> {
    let effective_gas_price = block_constants.base_fee_per_gas();
    let (caller, gas_limit) = match is_valid_ethereum_transaction_common(
        host,
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
    let execution_outcome = match revm_run_transaction(
        host,
        block_constants,
        Some(transaction_hash),
        caller,
        to,
        value,
        call_data,
        gas_limit,
        effective_gas_price,
        limits.maximum_gas_limit,
        transaction.access_list.clone(),
        transaction.authorization_list.clone(),
        spec_id,
        tracer_input,
        false,
    ) {
        Ok(outcome) => outcome,
        Err(err) => {
            return Err(Error::InvalidRunTransaction(revm_etherlink::Error::Custom(
                err.to_string(),
            ))
            .into());
        }
    };

    let gas_used = execution_outcome.result.gas_used().into();

    let transaction_result = TransactionResult {
        caller,
        execution_outcome,
        gas_used,
        estimated_ticks_used: 0,
    };

    Ok(ExecutionResult::Valid(transaction_result))
}

sol! {
    struct SolXTZDeposit {
        address receiver;
        uint256 inbox_level;
        uint256 inbox_msg_id;
    }

    function handle_xtz_deposit(SolXTZDeposit memory deposit) external;
}

impl From<&Deposit> for SolXTZDeposit {
    fn from(deposit: &Deposit) -> Self {
        SolXTZDeposit {
            receiver: h160_to_alloy(&deposit.receiver.to_h160().unwrap_or_default()),
            inbox_level: u256_to_alloy(&U256::from(deposit.inbox_level)),
            inbox_msg_id: u256_to_alloy(&U256::from(deposit.inbox_msg_id)),
        }
    }
}

pub fn pure_xtz_deposit<Host: Runtime>(
    host: &mut Host,
    deposit: &Deposit,
    block_constants: &BlockConstants,
    transaction_hash: [u8; TRANSACTION_HASH_SIZE],
    maximum_gas_limit: u64,
    spec_id: &SpecId,
    tracer_input: Option<TracerInput>,
) -> Result<ExecutionOutcome, Error> {
    // Fees are set to zero, this is an internal call to the XTZ bridge
    // solidity contract.
    // It isn't required for anyone to pay for the execution cost.
    let block_constants = BlockConstants {
        block_fees: BlockFees::new(U256::zero(), U256::zero(), U256::zero()),
        ..*block_constants
    };

    let caller = alloy_to_h160(&FEED_DEPOSIT_ADDR);
    let mut caller_account = StorageAccount::from_address(&FEED_DEPOSIT_ADDR)?;
    let to = Some(alloy_to_h160(&XTZ_BRIDGE_SOL_ADDR));
    let gas_limit = XTZ_DEPOSIT_EXECUTION_COST;
    let value = deposit.amount;
    // We prefund the feeder address for the xtz deposit.
    caller_account.add_balance(host, u256_to_alloy(&value))?;
    let call_data = handle_xtz_depositCall {
        deposit: SolXTZDeposit::from(deposit),
    }
    .abi_encode();
    let effective_gas_price = block_constants.base_fee_per_gas();
    match revm_run_transaction(
        host,
        &block_constants,
        Some(transaction_hash),
        caller,
        to,
        value,
        call_data,
        gas_limit,
        effective_gas_price,
        maximum_gas_limit,
        Vec::new(),
        None,
        spec_id,
        tracer_input,
        false,
    ) {
        Ok(execution_outcome) => Ok(execution_outcome),
        Err(err) => {
            // Something went wrong, we remove the added balance for the xtz deposit.
            caller_account.sub_balance(host, u256_to_alloy(&value))?;
            Err(Error::InvalidRunTransaction(revm_etherlink::Error::Custom(
                err.to_string(),
            )))
        }
    }
}

sol! {
    struct SolFaDepositWithProxy {
        uint256 amount;
        address receiver;
        address proxy;
        uint256 ticket_hash;
        uint256 inbox_level;
        uint256 inbox_msg_id;
    }

    struct SolFaDepositWithoutProxy {
        uint256 amount;
        address receiver;
        uint256 ticket_hash;
        uint256 inbox_level;
        uint256 inbox_msg_id;
    }

    function queue(SolFaDepositWithProxy memory deposit) external;

    function execute_without_proxy(SolFaDepositWithoutProxy memory deposit);
}

impl From<&FaDepositWithProxy> for SolFaDepositWithProxy {
    fn from(deposit: &FaDepositWithProxy) -> Self {
        SolFaDepositWithProxy {
            amount: u256_to_alloy(&deposit.amount),
            receiver: h160_to_alloy(&deposit.receiver),
            proxy: h160_to_alloy(&deposit.proxy),
            inbox_level: u256_to_alloy(&U256::from(deposit.inbox_level)),
            inbox_msg_id: u256_to_alloy(&U256::from(deposit.inbox_msg_id)),
            ticket_hash: u256_to_alloy(&U256::from_big_endian(
                deposit.ticket_hash.as_bytes(),
            )),
        }
    }
}

impl From<&FaDeposit> for SolFaDepositWithoutProxy {
    fn from(deposit: &FaDeposit) -> Self {
        SolFaDepositWithoutProxy {
            amount: u256_to_alloy(&deposit.amount),
            receiver: h160_to_alloy(&deposit.receiver),
            inbox_level: u256_to_alloy(&U256::from(deposit.inbox_level)),
            inbox_msg_id: u256_to_alloy(&U256::from(deposit.inbox_msg_id)),
            ticket_hash: u256_to_alloy(&U256::from_big_endian(
                deposit.ticket_hash.as_bytes(),
            )),
        }
    }
}

#[trace_kernel]
pub fn pure_fa_deposit<Host: Runtime>(
    host: &mut Host,
    fa_deposit: &FaDeposit,
    block_constants: &BlockConstants,
    transaction_hash: [u8; TRANSACTION_HASH_SIZE],
    maximum_gas_limit: u64,
    spec_id: &SpecId,
    tracer_input: Option<TracerInput>,
) -> Result<ExecutionOutcome, Error> {
    // Fees are set to zero, this is an internal call from the system address to the FA bridge solidity contract.
    // We do not require the system address to pay for the execution cost.
    let block_constants = BlockConstants {
        block_fees: BlockFees::new(U256::zero(), U256::zero(), U256::zero()),
        ..*block_constants
    };

    // A specific address is allocated for queue call
    // System address can only be used as caller for simulations
    let caller = alloy_to_h160(&FEED_DEPOSIT_ADDR);
    let to = Some(alloy_to_h160(&FA_BRIDGE_SOL_ADDR));
    let value = U256::zero();
    let gas_limit = FA_DEPOSIT_EXECUTION_COST;
    let call_data = match fa_deposit.to_fa_deposit_with_proxy() {
        Some(deposit) => queueCall {
            deposit: SolFaDepositWithProxy::from(&deposit),
        }
        .abi_encode(),
        None => execute_without_proxyCall {
            deposit: SolFaDepositWithoutProxy::from(fa_deposit),
        }
        .abi_encode(),
    };
    let effective_gas_price = block_constants.base_fee_per_gas();
    match revm_run_transaction(
        host,
        &block_constants,
        Some(transaction_hash),
        caller,
        to,
        value,
        call_data,
        gas_limit,
        effective_gas_price,
        maximum_gas_limit,
        Vec::new(),
        None,
        spec_id,
        tracer_input,
        false,
    ) {
        Ok(outcome) => Ok(outcome),
        Err(err) => Err(Error::InvalidRunTransaction(revm_etherlink::Error::Custom(
            err.to_string(),
        ))),
    }
}

fn apply_fa_deposit<Host: Runtime>(
    host: &mut Host,
    fa_deposit: &FaDeposit,
    block_constants: &BlockConstants,
    transaction_hash: [u8; TRANSACTION_HASH_SIZE],
    tracer_input: Option<TracerInput>,
    spec_id: &SpecId,
    limits: &EvmLimits,
) -> Result<ExecutionResult<TransactionResult>, Error> {
    let execution_outcome = pure_fa_deposit(
        host,
        fa_deposit,
        block_constants,
        transaction_hash,
        limits.maximum_gas_limit,
        spec_id,
        tracer_input,
    )?;

    let gas_used = execution_outcome.result.gas_used().into();

    let transaction_result = TransactionResult {
        // A specific address is allocated for queue call
        // System address can only be used as caller for simulations
        caller: alloy_to_h160(&FEED_DEPOSIT_ADDR),
        execution_outcome,
        gas_used,
        estimated_ticks_used: 0,
    };

    Ok(ExecutionResult::Valid(transaction_result))
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
#[instrument(skip_all)]
pub fn handle_transaction_result<Host: Runtime>(
    host: &mut Host,
    outbox_queue: &OutboxQueue<'_, impl Path>,
    block_constants: &BlockConstants,
    transaction: &Transaction,
    index: u32,
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

    let to = transaction.to()?;

    let fee_updates = transaction
        .content
        .fee_updates(&block_constants.block_fees, gas_used);

    log!(
        host,
        Debug,
        "Transaction executed, outcome: {:?}",
        execution_outcome
    );
    log!(
        host,
        Benchmarking,
        "gas_used: {:?}",
        execution_outcome.result.gas_used()
    );
    log!(host, Benchmarking, "reason: {:?}", execution_outcome.result);
    for message in execution_outcome.withdrawals.drain(..) {
        match message {
            Withdrawal::Standard(message) => {
                let outbox_message: OutboxMessage<RouterInterface> = message;
                let len = outbox_queue.queue_message(host, outbox_message)?;
                log!(host, Debug, "Length of the outbox queue: {}", len);
            }
            Withdrawal::Fast(message) => {
                let outbox_message: OutboxMessage<FastWithdrawalInterface> = message;
                let len = outbox_queue.queue_message(host, outbox_message)?;
                log!(host, Debug, "Length of the outbox queue: {}", len);
            }
        }
    }

    if pay_fees {
        fee_updates.apply(host, caller, sequencer_pool_address)?;
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
#[instrument(skip_all)]
pub fn apply_transaction<Host: Runtime>(
    host: &mut Host,
    outbox_queue: &OutboxQueue<'_, impl Path>,
    block_constants: &BlockConstants,
    transaction: &Transaction,
    index: u32,
    sequencer_pool_address: Option<H160>,
    tracer_input: Option<TracerInput>,
    spec_id: &SpecId,
    limits: &EvmLimits,
) -> Result<ExecutionResult<ExecutionInfo>, anyhow::Error> {
    let tracer_input = get_tracer_configuration(
        revm::primitives::B256::from_slice(&transaction.tx_hash),
        tracer_input,
    );
    let apply_result = match &transaction.content {
        TransactionContent::Ethereum(tx) => apply_ethereum_transaction_common(
            host,
            block_constants,
            tx,
            transaction.tx_hash,
            false,
            tracer_input,
            spec_id,
            limits,
        )?,
        TransactionContent::EthereumDelayed(tx) => apply_ethereum_transaction_common(
            host,
            block_constants,
            tx,
            transaction.tx_hash,
            true,
            tracer_input,
            spec_id,
            limits,
        )?,
        TransactionContent::Deposit(deposit) => {
            log!(host, Benchmarking, "Transaction type: DEPOSIT");
            execute_etherlink_deposit(
                host,
                deposit,
                block_constants,
                transaction.tx_hash,
                tracer_input,
                spec_id,
                limits,
            )?
        }
        TransactionContent::FaDeposit(fa_deposit) => {
            log!(host, Benchmarking, "Transaction type: FA_DEPOSIT");
            apply_fa_deposit(
                host,
                fa_deposit,
                block_constants,
                transaction.tx_hash,
                tracer_input,
                spec_id,
                limits,
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

    use crate::{
        apply::{is_valid_ethereum_transaction_common, Validity},
        chains::EvmLimits,
        fees::gas_for_fees,
    };
    use primitive_types::{H160, U256};
    use revm_etherlink::{
        helpers::legacy::{h160_to_alloy, u256_to_alloy},
        storage::world_state_handler::StorageAccount,
    };
    use tezos_ethereum::{
        block::{BlockConstants, BlockFees},
        transaction::TransactionType,
        tx_common::EthereumTransactionCommon,
    };
    use tezos_evm_runtime::runtime::MockKernelHost;
    use tezos_smart_rollup_encoding::timestamp::Timestamp;

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

    fn set_balance(host: &mut MockKernelHost, address: &H160, balance: U256) {
        let mut account = StorageAccount::from_address(&h160_to_alloy(address)).unwrap();
        let mut info = account.info(host).unwrap_or_default();
        info.balance = u256_to_alloy(&balance);
        account.set_info(host, info).unwrap();
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
        let block_constants = mock_block_constants();
        // setup
        let address = address_from_str("af1276cbb260bb13deddb4209ae99ae6e497f446");
        let gas_price = U256::from(21000);
        let fee_gas = gas_for_fees_no_data(&block_constants);
        let balance = U256::from(fee_gas + 21000) * gas_price;
        let gas_limit = 21000 + fee_gas;
        let transaction = valid_tx(gas_limit);
        // fund account
        set_balance(&mut host, &address, balance);

        // act
        let res = is_valid_ethereum_transaction_common(
            &mut host,
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
        set_balance(&mut host, &address, balance);

        // act
        let res = is_valid_ethereum_transaction_common(
            &mut host,
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
        set_balance(&mut host, &address, balance);

        // act
        let res = is_valid_ethereum_transaction_common(
            &mut host,
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
        set_balance(&mut host, &address, balance);

        // act
        let res = is_valid_ethereum_transaction_common(
            &mut host,
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
        let block_constants = mock_block_constants();

        // setup
        let address = address_from_str("af1276cbb260bb13deddb4209ae99ae6e497f446");
        let gas_price = U256::from(21000);
        let balance = U256::from(21000) * gas_price;
        let mut transaction = valid_tx(1);
        transaction.chain_id = Some(U256::from(42));
        transaction = resign(transaction);

        // fund account
        set_balance(&mut host, &address, balance);

        // act
        let res = is_valid_ethereum_transaction_common(
            &mut host,
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
        let block_constants = mock_block_constants();

        // setup
        let address = address_from_str("af1276cbb260bb13deddb4209ae99ae6e497f446");
        let gas_price = U256::from(21000);
        let balance = U256::from(21000) * gas_price;
        // fund account
        set_balance(&mut host, &address, balance);

        let gas_limit = 21000; // gas limit is not enough to cover fees
        let mut transaction = valid_tx(gas_limit);
        transaction.data = vec![1u8];
        transaction = resign(transaction);

        // act
        let res = is_valid_ethereum_transaction_common(
            &mut host,
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
