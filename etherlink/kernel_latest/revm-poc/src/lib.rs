// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

mod database;
mod precompile_provider;
mod storage_helpers;
mod world_state_handler;

#[cfg(test)]
mod test {
    use primitive_types::{H160 as PH160, U256 as PU256};
    use revm::{
        context::{transaction::AccessList, BlockEnv, CfgEnv, TxEnv},
        context_interface::block::BlobExcessGasAndPrice,
        database::{CacheDB, EmptyDB},
        inspector::inspectors::GasInspector,
        primitives::{hardfork::SpecId, Address, Bytes, FixedBytes, TxKind, U256},
        state::AccountInfo,
        Context, ExecuteEvm, MainBuilder, MainContext,
    };
    use tezos_ethereum::block::{BlockConstants, BlockFees};
    use tezos_evm_runtime::runtime::MockKernelHost;

    use crate::database::EtherlinkVMDB;
    use crate::precompile_provider;
    use crate::world_state_handler::new_world_state_handler;

    #[test]
    fn test_revm_usage() {
        let block = BlockEnv {
            number: U256::from(11),
            beneficiary: Address::ZERO,
            timestamp: U256::from(12),
            gas_limit: 1_000_000,
            basefee: 10,
            difficulty: U256::ZERO,
            prevrandao: Some(FixedBytes::new([0; 32])),
            blob_excess_gas_and_price: Some(BlobExcessGasAndPrice::new(0, 1)),
        };

        let tx = TxEnv {
            tx_type: 2,
            caller: Address::ZERO,
            gas_limit: block.gas_limit,
            gas_price: 45,
            kind: TxKind::Call(Address::ZERO),
            value: U256::ZERO,
            data: Bytes::new(),
            nonce: 0,
            chain_id: Some(1),
            access_list: AccessList(vec![]),
            gas_priority_fee: None,
            blob_hashes: vec![],
            max_fee_per_blob_gas: 45,
            authorization_list: vec![],
        };

        let mut db = CacheDB::new(EmptyDB::default());

        let account_info = AccountInfo {
            balance: U256::MAX,
            nonce: 0,
            code_hash: Default::default(),
            code: None,
        };

        db.insert_account_info(tx.caller, account_info);

        let mut cfg = CfgEnv::default();
        cfg.spec = SpecId::PRAGUE;

        let mut evm = Context::mainnet()
            .with_block(&block)
            .with_tx(&tx)
            .with_db(&mut db)
            .with_cfg(cfg)
            .build_mainnet();

        let out = evm.transact(&tx);

        println!("evm.transact:\n {:?}", out);

        let tracer = GasInspector::default();
        let precompiles = precompile_provider::Dummy {};
        let mut evm = evm.with_inspector(tracer).with_precompiles(precompiles);
        let _ = evm.transact(&tx);

        println!("evm.transact with inspector:\n{:?}", evm.inspector);
    }

    #[test]
    #[should_panic] // as the implementation contains dummy unimplented!()
    fn db_test_case() {
        let block = BlockEnv {
            number: U256::from(11),
            beneficiary: Address::ZERO,
            timestamp: U256::from(12),
            gas_limit: 1_000_000,
            basefee: 10,
            difficulty: U256::ZERO,
            prevrandao: Some(FixedBytes::new([0; 32])),
            blob_excess_gas_and_price: Some(BlobExcessGasAndPrice::new(0, 1)),
        };

        let tx = TxEnv {
            tx_type: 2,
            caller: Address::ZERO,
            gas_limit: block.gas_limit,
            gas_price: 45,
            kind: TxKind::Call(Address::ZERO),
            value: U256::ZERO,
            data: Bytes::new(),
            nonce: 0,
            chain_id: Some(1),
            access_list: AccessList(vec![]),
            gas_priority_fee: None,
            blob_hashes: vec![],
            max_fee_per_blob_gas: 45,
            authorization_list: vec![],
        };

        let mut host = MockKernelHost::default();
        let block_constants = BlockConstants::first_block(
            PU256::from(1),
            PU256::from(1),
            BlockFees::new(PU256::from(1), PU256::from(1), PU256::from(1)),
            1_000_000,
            PH160::from([0; 20]),
        );
        let mut world_state_handler = new_world_state_handler();
        let mut db =
            EtherlinkVMDB::new(&mut host, &block_constants, &mut world_state_handler);

        let mut cfg = CfgEnv::default();
        cfg.spec = SpecId::PRAGUE;

        let mut evm = Context::mainnet()
            .with_block(&block)
            .with_tx(&tx)
            .with_db(&mut db)
            .with_cfg(cfg)
            .build_mainnet();

        let out = evm.transact(&tx);

        println!("evm.transact:\n {:?}", out);

        let tracer = GasInspector::default();
        let mut evm = evm.with_inspector(tracer);
        let _ = evm.transact(&tx);

        println!("evm.transact with inspector:\n{:?}", evm.inspector);
    }
}
