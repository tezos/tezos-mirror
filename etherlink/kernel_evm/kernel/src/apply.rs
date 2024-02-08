// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
// SPDX-FileCopyrightText: 2023 Functori <contact@functori.com>
// SPDX-FileCopyrightText: 2022-2024 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use alloc::borrow::Cow;
use evm::{ExitError, ExitReason, ExitSucceed};
use evm_execution::account_storage::{
    account_path, EthereumAccount, EthereumAccountStorage,
};
use evm_execution::handler::{ExecutionOutcome, ExtendedExitReason};
use evm_execution::precompiles::PrecompileBTreeMap;
use evm_execution::run_transaction;
use primitive_types::{H160, U256};
use tezos_data_encoding::enc::BinWriter;
use tezos_ethereum::block::BlockConstants;
use tezos_ethereum::transaction::TransactionHash;
use tezos_ethereum::tx_common::EthereumTransactionCommon;
use tezos_ethereum::tx_signature::TxSignature;
use tezos_ethereum::withdrawal::Withdrawal;
use tezos_evm_logging::{log, Level::*};
use tezos_smart_rollup_core::MAX_OUTPUT_SIZE;
use tezos_smart_rollup_encoding::contract::Contract;
use tezos_smart_rollup_encoding::entrypoint::Entrypoint;
use tezos_smart_rollup_encoding::michelson::ticket::{FA2_1Ticket, Ticket};
use tezos_smart_rollup_encoding::michelson::{
    MichelsonContract, MichelsonOption, MichelsonPair,
};
use tezos_smart_rollup_encoding::outbox::OutboxMessage;
use tezos_smart_rollup_encoding::outbox::OutboxMessageTransaction;
use tezos_smart_rollup_host::runtime::Runtime;

use crate::error::Error;
use crate::fees::{tx_execution_gas_limit, FeeUpdates};
use crate::inbox::{Deposit, Transaction, TransactionContent};
use crate::indexable_storage::IndexableStorage;
use crate::storage::{index_account, read_ticketer};
use crate::{tick_model, CONFIG};

// This implementation of `Transaction` is used to share the logic of
// transaction receipt and transaction object making. The functions
// `make_receipt_info` and `make_object_info` use these functions to build
// the associated infos.
impl Transaction {
    fn to(&self) -> Option<H160> {
        match &self.content {
            TransactionContent::Deposit(Deposit { receiver, .. }) => Some(*receiver),
            TransactionContent::Ethereum(transaction) => transaction.to,
        }
    }

    fn data(&self) -> Vec<u8> {
        match &self.content {
            TransactionContent::Deposit(_) => vec![],
            TransactionContent::Ethereum(transaction) => transaction.data.clone(),
        }
    }

    fn value(&self) -> U256 {
        match &self.content {
            TransactionContent::Deposit(Deposit { amount, .. }) => *amount,
            TransactionContent::Ethereum(transaction) => transaction.value,
        }
    }

    fn nonce(&self) -> U256 {
        match &self.content {
            TransactionContent::Deposit(_) => U256::zero(),
            TransactionContent::Ethereum(transaction) => transaction.nonce,
        }
    }

    fn signature(&self) -> Option<TxSignature> {
        match &self.content {
            TransactionContent::Deposit(_) => None,
            TransactionContent::Ethereum(transaction) => transaction.signature.clone(),
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
}

#[derive(Debug)]
pub struct TransactionObjectInfo {
    pub from: H160,
    pub gas_used: U256,
    pub gas_price: U256,
    pub hash: TransactionHash,
    pub input: Vec<u8>,
    pub nonce: U256,
    pub to: Option<H160>,
    pub index: u32,
    pub value: U256,
    pub signature: Option<TxSignature>,
}

#[inline(always)]
fn make_receipt_info(
    tx_hash: TransactionHash,
    index: u32,
    execution_outcome: Option<ExecutionOutcome>,
    caller: H160,
    to: Option<H160>,
    effective_gas_price: U256,
) -> TransactionReceiptInfo {
    TransactionReceiptInfo {
        tx_hash,
        index,
        execution_outcome,
        caller,
        to,
        effective_gas_price,
    }
}

#[inline(always)]
fn make_object_info(
    transaction: &Transaction,
    from: H160,
    index: u32,
    fee_updates: &FeeUpdates,
) -> Result<TransactionObjectInfo, anyhow::Error> {
    Ok(TransactionObjectInfo {
        from,
        gas_used: fee_updates.overall_gas_used,
        gas_price: fee_updates.overall_gas_price,
        hash: transaction.tx_hash,
        input: transaction.data(),
        nonce: transaction.nonce(),
        to: transaction.to(),
        index,
        value: transaction.value(),
        signature: transaction.signature(),
    })
}

// From a receipt, indexes the caller, recipient and the new address if needs
// be.
fn index_new_accounts<Host: Runtime>(
    host: &mut Host,
    accounts_index: &mut IndexableStorage,
    receipt: &TransactionReceiptInfo,
) -> Result<(), Error> {
    index_account(host, &receipt.caller, accounts_index)?;
    if let Some(to) = receipt.to {
        index_account(host, &to, accounts_index)?
    };
    match receipt
        .execution_outcome
        .as_ref()
        .and_then(|o| o.new_address)
    {
        Some(to) => index_account(host, &to, accounts_index),
        None => Ok(()),
    }
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
    Valid(H160),
    InvalidChainId,
    InvalidSignature,
    InvalidNonce,
    InvalidPrePay,
    InvalidCode,
    InvalidMaxBaseFee,
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
) -> Result<Validity, Error> {
    // Chain id is correct.
    if transaction.chain_id.is_some()
        && Some(block_constant.chain_id) != transaction.chain_id
    {
        log!(host, Debug, "Transaction status: ERROR_CHAINID");
        return Ok(Validity::InvalidChainId);
    }

    // ensure that the user was willing to at least pay the base fee
    if transaction.max_fee_per_gas < block_constant.base_fee_per_gas() {
        log!(host, Debug, "Transaction status: ERROR_MAX_BASE_FEE");
        return Ok(Validity::InvalidMaxBaseFee);
    }

    // The transaction signature is valid.
    let caller = match transaction.caller() {
        Ok(caller) => caller,
        Err(_err) => {
            log!(host, Debug, "Transaction status: ERROR_SIGNATURE.");
            // Transaction with undefined caller are ignored, i.e. the caller
            // could not be derived from the signature.
            return Ok(Validity::InvalidSignature);
        }
    };

    let account = account(host, caller, evm_account_storage)?;

    let (nonce, balance, code_exists): (U256, U256, bool) = match account {
        None => (U256::zero(), U256::zero(), false),
        Some(account) => (
            account.nonce(host)?,
            account.balance(host)?,
            account.code_exists(host)?,
        ),
    };

    // The transaction nonce is valid.
    if nonce != transaction.nonce {
        log!(host, Debug, "Transaction status: ERROR_NONCE.");
        return Ok(Validity::InvalidNonce);
    };

    // The sender account balance contains at least the cost.
    let total_gas_limit = U256::from(transaction.gas_limit_with_fees());
    let cost = total_gas_limit.saturating_mul(effective_gas_price);
    // The sender can afford the max gas fee he set, see EIP-1559
    let max_fee = total_gas_limit.saturating_mul(transaction.max_fee_per_gas);

    if balance < cost || balance < max_fee {
        log!(host, Debug, "Transaction status: ERROR_PRE_PAY.");
        return Ok(Validity::InvalidPrePay);
    }

    // The sender does not have code, see EIP3607.
    if code_exists {
        log!(host, Debug, "Transaction status: ERROR_CODE.");
        return Ok(Validity::InvalidCode);
    }

    Ok(Validity::Valid(caller))
}

pub struct TransactionResult {
    caller: H160,
    execution_outcome: Option<ExecutionOutcome>,
    gas_used: U256,
    estimated_ticks_used: u64,
}

fn apply_ethereum_transaction_common<Host: Runtime>(
    host: &mut Host,
    block_constants: &BlockConstants,
    precompiles: &PrecompileBTreeMap<Host>,
    evm_account_storage: &mut EthereumAccountStorage,
    transaction: &EthereumTransactionCommon,
    allocated_ticks: u64,
    retriable: bool,
) -> Result<ExecutionResult<TransactionResult>, anyhow::Error> {
    let effective_gas_price = block_constants.base_fee_per_gas();
    let caller = match is_valid_ethereum_transaction_common(
        host,
        evm_account_storage,
        transaction,
        block_constants,
        effective_gas_price,
    )? {
        Validity::Valid(caller) => caller,
        _reason => return Ok(ExecutionResult::Invalid),
    };

    let to = transaction.to;
    let call_data = transaction.data.clone();
    let gas_limit = tx_execution_gas_limit(transaction, &block_constants.block_fees)?;
    let value = transaction.value;
    let execution_outcome = match run_transaction(
        host,
        block_constants,
        evm_account_storage,
        precompiles,
        CONFIG,
        to,
        caller,
        call_data,
        Some(gas_limit),
        effective_gas_price,
        Some(value),
        true,
        allocated_ticks,
        retriable,
    ) {
        Ok(outcome) => outcome,
        Err(err) => {
            // TODO: https://gitlab.com/tezos/tezos/-/issues/5665
            // Because the proposal's state is unclear, and we do not have a sequencer
            // if an error that leads to a durable storage corruption is caught, we
            // invalidate the entire proposal.
            return Err(Error::InvalidRunTransaction(err).into());
        }
    };

    let (gas_used, estimated_ticks_used, out_of_ticks) = match &execution_outcome {
        Some(execution_outcome) => {
            log!(
                host,
                Debug,
                "Transaction status: OK_{}.",
                execution_outcome.is_success
            );
            (
                execution_outcome.gas_used.into(),
                execution_outcome.estimated_ticks_used,
                execution_outcome.reason == ExtendedExitReason::OutOfTicks,
            )
        }
        None => {
            log!(host, Debug, "Transaction status: OK_UNKNOWN.");
            (U256::zero(), 0, false)
        }
    };

    let transaction_result = TransactionResult {
        caller,
        execution_outcome,
        gas_used,
        estimated_ticks_used,
    };

    if out_of_ticks && retriable {
        Ok(ExecutionResult::Retriable(transaction_result))
    } else {
        Ok(ExecutionResult::Valid(transaction_result))
    }
}

fn apply_deposit<Host: Runtime>(
    host: &mut Host,
    evm_account_storage: &mut EthereumAccountStorage,
    deposit: &Deposit,
) -> Result<ExecutionResult<TransactionResult>, Error> {
    let Deposit { amount, receiver } = deposit;

    let mut do_deposit = |()| -> Option<()> {
        let mut to_account = evm_account_storage
            .get_or_create(host, &account_path(receiver).ok()?)
            .ok()?;
        to_account.balance_add(host, *amount).ok()
    };

    let is_success = do_deposit(()).is_some();

    let reason = if is_success {
        ExitReason::Succeed(ExitSucceed::Returned)
    } else {
        ExitReason::Error(ExitError::Other(Cow::from("Deposit failed")))
    };

    let gas_used = CONFIG.gas_transaction_call;

    // TODO: https://gitlab.com/tezos/tezos/-/issues/6551
    let estimated_ticks_used = tick_model::constants::TICKS_FOR_DEPOSIT;

    let execution_outcome = ExecutionOutcome {
        gas_used,
        is_success,
        reason: reason.into(),
        new_address: None,
        logs: vec![],
        result: None,
        withdrawals: vec![],
        estimated_ticks_used,
    };

    let caller = H160::zero();

    Ok(ExecutionResult::Valid(TransactionResult {
        caller,
        execution_outcome: Some(execution_outcome),
        gas_used: gas_used.into(),
        estimated_ticks_used,
    }))
}

fn post_withdrawals<Host: Runtime>(
    host: &mut Host,
    withdrawals: &Vec<Withdrawal>,
) -> Result<(), Error> {
    if withdrawals.is_empty() {
        return Ok(());
    };

    let destination = match read_ticketer(host) {
        Some(x) => Contract::Originated(x),
        None => return Err(Error::InvalidParsing),
    };
    let entrypoint = Entrypoint::try_from(String::from("burn"))?;

    for withdrawal in withdrawals {
        // Wei is 10^18, whereas mutez is 10^6.
        let amount: U256 =
            U256::checked_div(withdrawal.amount, U256::from(10).pow(U256::from(12)))
                // If we reach the unwrap_or it will fail at the next step because
                // we cannot create a ticket with no amount. But by construction
                // it should not happen, we do not divide by 0.
                .unwrap_or(U256::zero());

        let amount = if amount < U256::from(u64::max_value()) {
            amount.as_u64()
        } else {
            // Users can withdraw only mutez, converted to ETH, thus the
            // maximum value of `amount` is `Int64.max_int` which fit
            // in a u64.
            return Err(Error::InvalidConversion);
        };

        let ticket: FA2_1Ticket = Ticket::new(
            destination.clone(),
            MichelsonPair(0.into(), MichelsonOption(None)),
            amount,
        )?;
        let parameters = MichelsonPair::<MichelsonContract, FA2_1Ticket>(
            MichelsonContract(withdrawal.target.clone()),
            ticket,
        );

        let withdrawal = OutboxMessageTransaction {
            parameters,
            entrypoint: entrypoint.clone(),
            destination: destination.clone(),
        };
        let outbox_message =
            OutboxMessage::AtomicTransactionBatch(vec![withdrawal].into());

        let mut encoded = Vec::with_capacity(MAX_OUTPUT_SIZE);

        outbox_message.bin_write(&mut encoded)?;

        host.write_output(&encoded)?;
    }

    Ok(())
}

pub struct ExecutionInfo {
    pub receipt_info: TransactionReceiptInfo,
    pub object_info: TransactionObjectInfo,
    pub estimated_ticks_used: u64,
}

pub enum ExecutionResult<T> {
    Valid(T),
    Invalid,
    Retriable(T),
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
    block_constants: &BlockConstants,
    transaction: &Transaction,
    index: u32,
    evm_account_storage: &mut EthereumAccountStorage,
    accounts_index: &mut IndexableStorage,
    transaction_result: TransactionResult,
    pay_fees: bool,
) -> Result<ExecutionInfo, anyhow::Error> {
    let TransactionResult {
        caller,
        mut execution_outcome,
        gas_used,
        estimated_ticks_used: ticks_used,
    } = transaction_result;

    let to = transaction.to();

    let fee_updates = match &transaction.content {
        TransactionContent::Deposit(_) => FeeUpdates::for_deposit(gas_used),
        TransactionContent::Ethereum(tx) => {
            FeeUpdates::for_tx(tx, &block_constants.block_fees, gas_used)
        }
    };

    if let Some(outcome) = &mut execution_outcome {
        log!(host, Debug, "Transaction executed, outcome: {:?}", outcome);
        fee_updates.modify_outcome(outcome);
        post_withdrawals(host, &outcome.withdrawals)?
    }

    if pay_fees {
        fee_updates.apply(host, evm_account_storage, caller)?;
    }

    let object_info = make_object_info(transaction, caller, index, &fee_updates)?;

    let receipt_info = make_receipt_info(
        transaction.tx_hash,
        index,
        execution_outcome,
        caller,
        to,
        object_info.gas_price,
    );

    index_new_accounts(host, accounts_index, &receipt_info)?;
    Ok(ExecutionInfo {
        receipt_info,
        object_info,
        estimated_ticks_used: ticks_used,
    })
}

#[allow(clippy::too_many_arguments)]
pub fn apply_transaction<Host: Runtime>(
    host: &mut Host,
    block_constants: &BlockConstants,
    precompiles: &PrecompileBTreeMap<Host>,
    transaction: &Transaction,
    index: u32,
    evm_account_storage: &mut EthereumAccountStorage,
    accounts_index: &mut IndexableStorage,
    allocated_ticks: u64,
    retriable: bool,
) -> Result<ExecutionResult<ExecutionInfo>, anyhow::Error> {
    let apply_result = match &transaction.content {
        TransactionContent::Ethereum(tx) => apply_ethereum_transaction_common(
            host,
            block_constants,
            precompiles,
            evm_account_storage,
            tx,
            allocated_ticks,
            retriable,
        )?,
        TransactionContent::Deposit(deposit) => {
            apply_deposit(host, evm_account_storage, deposit)?
        }
    };

    match apply_result {
        ExecutionResult::Valid(tx_result) => {
            let execution_result = handle_transaction_result(
                host,
                block_constants,
                transaction,
                index,
                evm_account_storage,
                accounts_index,
                tx_result,
                true,
            )?;
            Ok(ExecutionResult::Valid(execution_result))
        }
        // Note that both branch must be differentiated as the fees won't be
        // collected yet if the transaction is retriable.
        ExecutionResult::Retriable(tx_result) => {
            let execution_result = handle_transaction_result(
                host,
                block_constants,
                transaction,
                index,
                evm_account_storage,
                accounts_index,
                tx_result,
                false,
            )?;
            Ok(ExecutionResult::Retriable(execution_result))
        }
        ExecutionResult::Invalid => Ok(ExecutionResult::Invalid),
    }
}

#[cfg(test)]
mod tests {
    use crate::apply::Validity;
    use evm_execution::account_storage::{account_path, EthereumAccountStorage};
    use primitive_types::{H160, U256};
    use tezos_ethereum::{
        block::{BlockConstants, BlockFees},
        transaction::TransactionType,
        tx_common::EthereumTransactionCommon,
    };
    use tezos_smart_rollup_encoding::timestamp::Timestamp;
    use tezos_smart_rollup_mock::MockHost;

    use super::is_valid_ethereum_transaction_common;

    const CHAIN_ID: u32 = 1337;

    fn mock_block_constants() -> BlockConstants {
        let block_fees = BlockFees::new(
            U256::from(12345),
            U256::from(1_000_000),
            U256::from(2_000_000_000_000u64),
        );
        BlockConstants::first_block(
            U256::from(Timestamp::from(0).as_u64()),
            CHAIN_ID.into(),
            block_fees,
        )
    }

    fn address_from_str(s: &str) -> H160 {
        let data = &hex::decode(s).unwrap();
        H160::from_slice(data)
    }

    fn set_balance(
        host: &mut MockHost,
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
            U256::from(0),
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

    #[test]
    fn test_tx_is_valid() {
        let mut host = MockHost::default();
        let mut evm_account_storage =
            evm_execution::account_storage::init_account_storage().unwrap();
        let block_constants = mock_block_constants();

        // setup
        let address = address_from_str("af1276cbb260bb13deddb4209ae99ae6e497f446");
        let gas_price = U256::from(21000);
        let fee_gas = block_constants.block_fees.gas_for_fees(gas_price);
        let balance = (fee_gas + 21000) * gas_price;
        let gas_limit = 21000 + fee_gas.as_u64();
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
        );
        assert_eq!(
            Validity::Valid(address),
            res.expect("Verification should not have raise an error"),
            "Transaction should have been rejected"
        );
    }

    #[test]
    fn test_tx_is_invalid_cannot_prepay() {
        let mut host = MockHost::default();
        let mut evm_account_storage =
            evm_execution::account_storage::init_account_storage().unwrap();
        let block_constants = mock_block_constants();

        // setup
        let address = address_from_str("af1276cbb260bb13deddb4209ae99ae6e497f446");
        let gas_price = U256::from(21000);
        let fee_gas = block_constants.block_fees.gas_for_fees(gas_price);
        // account doesnt have enough funds for execution
        let balance = fee_gas * gas_price;
        let gas_limit = 21000 + fee_gas.as_u64();
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
        );
        assert_eq!(
            Validity::InvalidPrePay,
            res.expect("Verification should not have raise an error"),
            "Transaction should have been rejected"
        );
    }

    #[test]
    fn test_tx_is_invalid_signature() {
        let mut host = MockHost::default();
        let mut evm_account_storage =
            evm_execution::account_storage::init_account_storage().unwrap();
        let block_constants = mock_block_constants();

        // setup
        let address = address_from_str("af1276cbb260bb13deddb4209ae99ae6e497f446");
        let gas_price = U256::from(21000);
        let fee_gas = block_constants.block_fees.gas_for_fees(gas_price);
        let balance = (fee_gas + 21000) * gas_price;
        let gas_limit = 21000 + fee_gas.as_u64();
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
        );
        assert_eq!(
            Validity::InvalidSignature,
            res.expect("Verification should not have raise an error"),
            "Transaction should have been rejected"
        );
    }

    #[test]
    fn test_tx_is_invalid_wrong_nonce() {
        let mut host = MockHost::default();
        let mut evm_account_storage =
            evm_execution::account_storage::init_account_storage().unwrap();
        let block_constants = mock_block_constants();

        // setup
        let address = address_from_str("af1276cbb260bb13deddb4209ae99ae6e497f446");
        let gas_price = U256::from(21000);
        let fee_gas = block_constants.block_fees.gas_for_fees(gas_price);
        let balance = (fee_gas + 21000) * gas_price;
        let gas_limit = 21000 + fee_gas.as_u64();
        let mut transaction = valid_tx(gas_limit);
        transaction.nonce = U256::from(42);
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
        );
        assert_eq!(
            Validity::InvalidNonce,
            res.expect("Verification should not have raise an error"),
            "Transaction should have been rejected"
        );
    }

    #[test]
    fn test_tx_is_invalid_wrong_chain_id() {
        let mut host = MockHost::default();
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
        );
        assert_eq!(
            Validity::InvalidChainId,
            res.expect("Verification should not have raise an error"),
            "Transaction should have been rejected"
        );
    }

    #[test]
    fn test_tx_is_invalid_max_fee_less_than_base_fee() {
        let mut host = MockHost::default();
        let mut evm_account_storage =
            evm_execution::account_storage::init_account_storage().unwrap();
        let block_constants = mock_block_constants();

        // setup
        let gas_price = U256::from(21000);
        let max_gas_price = U256::one();
        let fee_gas = block_constants.block_fees.gas_for_fees(max_gas_price);
        // account doesnt have enough funds for execution
        let gas_limit = 21000 + fee_gas.as_u64();
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
        );
        assert_eq!(
            Validity::InvalidMaxBaseFee,
            res.expect("Verification should not have raise an error"),
            "Transaction should have been rejected"
        );
    }
}
