// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use crate::address::OriginationNonce;
use crate::context::Context;
use crate::get_contract_entrypoint;
use mir::{
    ast::{AddressHash, PublicKeyHash},
    context::CtxTrait,
};
use num_bigint::BigUint;
use tezos_crypto_rs::hash::ChainId;
use tezos_evm_runtime::runtime::Runtime;
use tezos_smart_rollup::types::Timestamp;
use tezos_tezlink::enc_wrappers::BlockNumber;

pub struct Ctx<'a, Host, Context, Gas, OpCounter, OrigNonce> {
    pub host: Host,
    pub context: Context,
    pub sender: AddressHash,
    pub amount: i64,
    pub self_address: AddressHash,
    pub balance: i64,
    pub level: BlockNumber,
    pub now: Timestamp,
    pub chain_id: ChainId,

    pub source: PublicKeyHash,
    pub gas: Gas,
    pub operation_counter: OpCounter,
    pub origination_nonce: OrigNonce,

    pub lazy_storage: mir::ast::big_map::InMemoryLazyStorage<'a>,
}

impl<'a, Host: Runtime> CtxTrait<'a>
    for Ctx<'a, &mut Host, &Context, &mut mir::gas::Gas, &mut u128, &mut OriginationNonce>
{
    type BigMapStorage = mir::ast::big_map::InMemoryLazyStorage<'a>;

    fn sender(&self) -> AddressHash {
        self.sender.clone()
    }

    fn source(&self) -> PublicKeyHash {
        self.source.clone()
    }

    fn amount(&self) -> i64 {
        self.amount
    }

    fn self_address(&self) -> AddressHash {
        self.self_address.clone()
    }

    fn balance(&self) -> i64 {
        self.balance
    }

    fn gas(&mut self) -> &mut mir::gas::Gas {
        self.gas
    }

    fn level(&self) -> BigUint {
        self.level.block_number.into()
    }

    fn min_block_time(&self) -> BigUint {
        1u32.into()
    }

    fn chain_id(&self) -> mir::ast::ChainId {
        self.chain_id.clone()
    }

    fn voting_power(&self, _: &PublicKeyHash) -> BigUint {
        0u32.into()
    }

    fn now(&self) -> num_bigint::BigInt {
        i64::from(self.now).into()
    }

    fn total_voting_power(&self) -> BigUint {
        1u32.into()
    }

    fn operation_group_hash(&self) -> [u8; 32] {
        self.origination_nonce.operation.0 .0
    }

    fn big_map_storage(&mut self) -> &mut mir::ast::big_map::InMemoryLazyStorage<'a> {
        &mut self.lazy_storage
    }

    fn origination_counter(&mut self) -> u32 {
        let c: &mut u32 = &mut self.origination_nonce.index;
        *c += 1;
        *c
    }

    fn operation_counter(&mut self) -> u128 {
        let c: &mut u128 = self.operation_counter;
        *c += 1;
        *c
    }

    fn lookup_contract(
        &self,
        address: &AddressHash,
    ) -> Option<std::collections::HashMap<mir::ast::Entrypoint, mir::ast::Type>> {
        get_contract_entrypoint(self.host, self.context, address)
    }
}
