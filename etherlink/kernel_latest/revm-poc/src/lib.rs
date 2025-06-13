// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

mod block_storage;
mod code_storage;
mod database;
mod precompile_provider;
mod storage_helpers;
mod withdrawal;
mod world_state_handler;

#[cfg(test)]
mod test {
    use primitive_types::{H160 as PH160, U256 as PU256};
    use revm::{
        context::{
            result::ExecutionResult, transaction::AccessList, BlockEnv, CfgEnv,
            ContextTr, LocalContext, TxEnv,
        },
        context_interface::block::BlobExcessGasAndPrice,
        inspector::inspectors::GasInspector,
        primitives::{
            hardfork::SpecId, hex::FromHex, Address, Bytes, FixedBytes, TxKind, B256,
            U256,
        },
        state::{AccountInfo, Bytecode},
        Context, Database, ExecuteCommitEvm, ExecuteEvm, Journal, MainBuilder,
    };
    use tezos_ethereum::block::{BlockConstants, BlockFees};
    use tezos_evm_runtime::runtime::MockKernelHost;
    use utilities::{block_env, etherlink_vm_db, evm, tx_env};

    use crate::{database::EtherlinkVMDB, precompile_provider::EtherlinkPrecompiles};
    use crate::{
        withdrawal::WITHDRAWAL_EVENT_TOPIC, world_state_handler::new_world_state_handler,
    };

    const ETHERLINK_CHAIN_ID: u64 = 42793;
    const DEFAULT_SPEC_ID: SpecId = SpecId::PRAGUE;

    mod utilities {
        use revm::{
            context::Evm,
            handler::{instructions::EthInstructions, EthPrecompiles},
            interpreter::interpreter::EthInterpreter,
        };

        use super::*;

        pub(crate) fn block_env(number: Option<U256>, gas_limit: u64) -> BlockEnv {
            BlockEnv {
                number: number.unwrap_or(U256::from(1)),
                beneficiary: Address::ZERO,
                timestamp: U256::from(0),
                gas_limit,
                basefee: 0,
                difficulty: U256::ZERO,
                prevrandao: Some(FixedBytes::new([0; 32])),
                blob_excess_gas_and_price: Some(BlobExcessGasAndPrice::new(0, 1)),
            }
        }

        pub(crate) fn tx_env(
            caller: Address,
            destination: Option<Address>,
            gas_limit: u64,
            gas_price: u128,
            value: U256,
            data: Bytes,
            nonce: Option<u64>,
        ) -> TxEnv {
            let kind = match destination {
                Some(address) => TxKind::Call(address),
                None => TxKind::Create,
            };

            TxEnv {
                tx_type: 2,
                caller,
                gas_limit,
                gas_price,
                kind,
                value,
                data,
                nonce: nonce.unwrap_or_default(),
                chain_id: Some(ETHERLINK_CHAIN_ID),
                access_list: AccessList(vec![]),
                gas_priority_fee: None,
                blob_hashes: vec![],
                max_fee_per_blob_gas: 10,
                authorization_list: vec![],
            }
        }

        pub(crate) fn block_constants(block_env: &BlockEnv) -> BlockConstants {
            BlockConstants::first_block(
                PU256::from_little_endian(block_env.number.as_le_slice()),
                PU256::from(1),
                BlockFees::new(
                    PU256::from(1),
                    PU256::from(block_env.basefee),
                    PU256::from(1),
                ),
                block_env.gas_limit,
                PH160::from(block_env.beneficiary.into_array()),
            )
        }

        pub(crate) fn etherlink_vm_db<'a>(
            block_env: &BlockEnv,
        ) -> EtherlinkVMDB<'a, MockKernelHost> {
            let host = Box::leak(Box::new(MockKernelHost::default()));
            let world_state_handler = Box::leak(Box::new(new_world_state_handler()));
            let block_constants = Box::leak(Box::new(block_constants(block_env)));

            EtherlinkVMDB::new(host, block_constants, world_state_handler)
        }

        type EvmContext<'a> = Evm<
            Context<&'a BlockEnv, &'a TxEnv, CfgEnv, EtherlinkVMDB<'a, MockKernelHost>>,
            (),
            EthInstructions<
                EthInterpreter,
                Context<
                    &'a BlockEnv,
                    &'a TxEnv,
                    CfgEnv,
                    EtherlinkVMDB<'a, MockKernelHost>,
                >,
            >,
            EthPrecompiles,
        >;

        pub(crate) fn evm<'a>(
            db: EtherlinkVMDB<'a, MockKernelHost>,
            block: &'a BlockEnv,
            tx: &'a TxEnv,
        ) -> EvmContext<'a> {
            let cfg = CfgEnv::new()
                .with_chain_id(ETHERLINK_CHAIN_ID)
                .with_spec(DEFAULT_SPEC_ID);

            let context: Context<
                BlockEnv,
                TxEnv,
                CfgEnv,
                EtherlinkVMDB<'a, MockKernelHost>,
                Journal<EtherlinkVMDB<'a, MockKernelHost>>,
                (),
                LocalContext,
            > = Context::new(db, DEFAULT_SPEC_ID);

            context
                .with_block(block)
                .with_tx(tx)
                .with_cfg(cfg)
                .build_mainnet()
        }
    }

    #[test]
    fn test_withdrawal_precompile_contract() {
        let caller =
            Address::from_hex("1111111111111111111111111111111111111111").unwrap();
        let destination =
            Address::from_hex("0xff00000000000000000000000000000000000001").unwrap();
        let value = U256::from(5);

        let block = block_env(None, 1_000_000);
        let tx = tx_env(
            caller,
            Some(destination),
            block.gas_limit,
            0,
            value,
            Bytes::from_hex("cda4fee200112233").unwrap(),
            None,
        );

        let mut db = etherlink_vm_db(&block);

        let account_info = AccountInfo {
            balance: U256::MAX,
            nonce: 0,
            code_hash: Default::default(),
            code: None,
        };

        db.insert_account_info(caller, account_info);

        let evm = evm(db, &block, &tx);
        let tracer = GasInspector::default();
        let mut host = MockKernelHost::default();
        let precompiles = EtherlinkPrecompiles::new(&mut host);
        let mut evm = evm.with_inspector(tracer).with_precompiles(precompiles);
        let execution_result: ExecutionResult = evm.transact(&tx).unwrap();

        let expected_topics = vec![B256::new(WITHDRAWAL_EVENT_TOPIC)];
        assert_eq!(
            execution_result.logs().first().unwrap().topics(),
            expected_topics
        )
    }

    #[test]
    fn test_simple_transfer() {
        let caller =
            Address::from_hex("1111111111111111111111111111111111111111").unwrap();
        let destination =
            Address::from_hex("2222222222222222222222222222222222222222").unwrap();
        let value = U256::from(5);

        let block = block_env(None, 1_000_000);
        let tx = tx_env(
            caller,
            Some(destination),
            block.gas_limit,
            0,
            value,
            Bytes::new(),
            None,
        );

        let mut db = etherlink_vm_db(&block);

        let account_info = AccountInfo {
            balance: U256::MAX,
            nonce: 0,
            code_hash: Default::default(),
            code: None,
        };

        db.insert_account_info(caller, account_info);

        let mut evm = evm(db, &block, &tx);

        let account_caller_pre_tx = evm.db_mut().basic(caller).unwrap().unwrap();
        let account_destination_pre_tx =
            evm.db_mut().basic(destination).unwrap().unwrap();

        // Check balances before executing the transfer
        assert_eq!(account_caller_pre_tx.balance, U256::MAX);
        assert_eq!(account_destination_pre_tx.balance, U256::ZERO);

        let execution_result = evm.transact_commit(&tx).unwrap();

        // Check the outcome of the transaction
        match execution_result {
            ExecutionResult::Success { .. } => (),
            ExecutionResult::Revert { .. } | ExecutionResult::Halt { .. } => {
                panic!("Simple transfer should have succeeded")
            }
        }

        // Check balances after executing the transfer
        let account_caller_post_tx = evm.db_mut().basic(caller).unwrap().unwrap();
        let account_destination_post_tx =
            evm.db_mut().basic(destination).unwrap().unwrap();

        assert_eq!(
            account_caller_post_tx.balance,
            U256::MAX.checked_sub(value).unwrap()
        );
        assert_eq!(account_destination_post_tx.balance, value);
    }

    #[test]
    fn test_contract_call_sload_sstore() {
        let caller =
            Address::from_hex("1111111111111111111111111111111111111111").unwrap();
        let contract =
            Address::from_hex("2222222222222222222222222222222222222222").unwrap();

        let block = block_env(None, 10_000_000);
        let tx = tx_env(
            caller,
            Some(contract),
            block.gas_limit,
            10,
            U256::from(0),
            Bytes::new(),
            None,
        );

        let mut db = etherlink_vm_db(&block);

        let caller_info = AccountInfo {
            balance: U256::MAX,
            nonce: 0,
            code_hash: Default::default(),
            code: None,
        };

        let contract_info = AccountInfo {
            balance: U256::ZERO,
            nonce: 0,
            // Code hash will be automatically computed and inserted when
            // inserting the account info into the db.
            code_hash: Default::default(),
            // PUSH1 0x42      # Value to store
            // PUSH1 0x01      # Storage slot index
            // SSTORE          # Store the value in storage
            // PUSH1 0x01      # Load from the same storage slot
            // SLOAD           # Retrieve the value
            code: Some(Bytecode::new_raw(
                Bytes::from_hex("6042600155600154").unwrap(),
            )),
        };

        db.insert_account_info(caller, caller_info);
        db.insert_account_info(contract, contract_info);

        let mut evm = evm(db, &block, &tx);

        let execution_result = evm.transact_commit(&tx).unwrap();

        // Check the outcome of the transaction
        match execution_result {
            ExecutionResult::Success { gas_used, .. } => {
                assert!(gas_used > 0);
            }
            ExecutionResult::Revert { .. } | ExecutionResult::Halt { .. } => {
                panic!("Simple transfer should have succeeded")
            }
        }

        // Check that the storage slot at 0x01 was updated with 0x42
        let storage_slot_value = evm.db().storage_slot(contract, U256::from(1));
        assert_eq!(storage_slot_value, U256::from(66));
    }
}
