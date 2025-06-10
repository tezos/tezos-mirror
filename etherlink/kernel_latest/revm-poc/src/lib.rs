// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

#[cfg(test)]
mod test {
    use revm::{
        context::{transaction::AccessList, BlockEnv, CfgEnv, TxEnv},
        context_interface::block::BlobExcessGasAndPrice,
        database::{CacheDB, EmptyDB},
        inspector::inspectors::GasInspector,
        primitives::{hardfork::SpecId, Address, Bytes, FixedBytes, TxKind, U256},
        state::AccountInfo,
        Context, ExecuteEvm, MainBuilder, MainContext,
    };

    #[test]
    fn test_revm_usage() {
        let block = BlockEnv {
            number: 11,
            beneficiary: Address::ZERO,
            timestamp: 12,
            gas_limit: 1_000_000,
            basefee: 10,
            difficulty: U256::ZERO,
            prevrandao: Some(FixedBytes::new([0; 32])),
            blob_excess_gas_and_price: Some(BlobExcessGasAndPrice::new(0, true)),
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
        let mut evm = evm.with_inspector(tracer);
        let _ = evm.transact(&tx);

        println!("evm.transact with inspector:\n{:?}", evm.inspector);
    }
}
