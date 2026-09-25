pub struct RegistryImpl {
    ethereum: tezosx_ethereum_runtime::EthereumRuntime,
    tezos: tezosx_tezos_runtime::TezosRuntime,
}

impl Default for RegistryImpl {
    fn default() -> Self {
        Self::new(U256::from(1337), ChainId::from([1, 3, 3, 7]))
    }
}

impl RegistryImpl {
    pub fn new(eth_chain_id: U256, tez_chain_id: ChainId) -> Self {
        Self {
            ethereum: EthereumRuntime::new(eth_chain_id),
            tezos: TezosRuntime::new(tez_chain_id),
        }
    }
}

use primitive_types::U256;
use tezos_crypto_rs::hash::ChainId;
use tezos_evm_runtime::runtime_keyspaces::RuntimeKeyspaces;
use tezos_smart_rollup_host::storage::StorageV1;
use tezos_smart_rollup_keyspace::{KeySpace, KeySpaceLoader};
use tezosx_ethereum_runtime::EthereumRuntime;
use tezosx_interfaces::{AliasResolution, Registry, RuntimeInterface};
use tezosx_journal::TezosXJournal;
use tezosx_tezos_runtime::TezosRuntime;

impl Registry for RegistryImpl {
    type Journal = TezosXJournal;

    fn ensure_alias<Host>(
        &self,
        rk: &mut RuntimeKeyspaces<'_, Host, Host::KeySpace>,
        journal: &mut TezosXJournal,
        alias_info: tezosx_interfaces::AliasInfo,
        native_public_key: Option<&[u8]>,
        target_runtime: tezosx_interfaces::RuntimeId,
        context: tezosx_interfaces::CrossRuntimeContext,
        gas_remaining: tezosx_interfaces::Gas,
    ) -> Result<
        (String, tezosx_interfaces::AliasResolution),
        tezosx_interfaces::TezosXRuntimeError,
    >
    where
        Host: StorageV1 + KeySpaceLoader,
    {
        // The alias lives in `target_runtime`, so it is that runtime's
        // derivation that names it. `alias_info.runtime` is the *source*
        // runtime of the native address — dispatching on it would derive
        // the alias in the wrong address format.
        let alias = self.compute_alias(&tezosx_interfaces::AliasInfo {
            runtime: target_runtime,
            native_address: alias_info.native_address.clone(),
        })?;
        let result = if !self.alias_exists(rk, journal, target_runtime, &alias)? {
            match target_runtime {
                tezosx_interfaces::RuntimeId::Tezos => self.tezos.create_alias(
                    self,
                    rk,
                    journal,
                    &alias,
                    alias_info,
                    native_public_key,
                    context,
                    gas_remaining,
                ),
                tezosx_interfaces::RuntimeId::Ethereum => self.ethereum.create_alias(
                    self,
                    rk,
                    journal,
                    &alias,
                    alias_info,
                    native_public_key,
                    context,
                    gas_remaining,
                ),
            }
        } else {
            Ok(AliasResolution::build(gas_remaining))
        };
        result.map(|resolution| (alias, resolution))
    }

    fn alias_exists<Host>(
        &self,
        rk: &mut RuntimeKeyspaces<'_, Host, Host::KeySpace>,
        journal: &mut Self::Journal,
        target_runtime: tezosx_interfaces::RuntimeId,
        alias: &str,
    ) -> Result<bool, tezosx_interfaces::TezosXRuntimeError>
    where
        Host: StorageV1 + KeySpaceLoader,
    {
        match target_runtime {
            tezosx_interfaces::RuntimeId::Tezos => {
                self.tezos.alias_exists(rk, journal, alias)
            }
            tezosx_interfaces::RuntimeId::Ethereum => {
                self.ethereum.alias_exists(rk, journal, alias)
            }
        }
    }

    fn compute_alias(
        &self,
        alias_info: &tezosx_interfaces::AliasInfo,
    ) -> Result<String, tezosx_interfaces::TezosXRuntimeError> {
        match alias_info.runtime {
            tezosx_interfaces::RuntimeId::Tezos => self
                .tezos
                .compute_alias(alias_info.native_address.as_bytes()),
            tezosx_interfaces::RuntimeId::Ethereum => self
                .ethereum
                .compute_alias(alias_info.native_address.as_bytes()),
        }
    }

    fn address_from_string(
        &self,
        address_str: &str,
        runtime_id: tezosx_interfaces::RuntimeId,
    ) -> Result<Vec<u8>, tezosx_interfaces::TezosXRuntimeError> {
        match runtime_id {
            tezosx_interfaces::RuntimeId::Tezos => {
                self.tezos.address_from_string(address_str)
            }
            tezosx_interfaces::RuntimeId::Ethereum => {
                self.ethereum.address_from_string(address_str)
            }
        }
    }

    fn read_origin<Host, KS>(
        &self,
        rk: &RuntimeKeyspaces<'_, Host, KS>,
        addr_runtime: tezosx_interfaces::RuntimeId,
        addr: &str,
        budget: tezosx_interfaces::Gas,
    ) -> Result<
        (tezosx_interfaces::Classification, tezosx_interfaces::Gas),
        tezosx_interfaces::TezosXRuntimeError,
    >
    where
        Host: StorageV1,
        KS: KeySpace,
    {
        match addr_runtime {
            tezosx_interfaces::RuntimeId::Tezos => {
                self.tezos.read_origin(rk, addr, budget)
            }
            tezosx_interfaces::RuntimeId::Ethereum => {
                self.ethereum.read_origin(rk, addr, budget)
            }
        }
    }

    fn serve<Host, KS>(
        &self,
        rk: &mut RuntimeKeyspaces<'_, Host, KS>,
        journal: &mut TezosXJournal,
        request: http::Request<Vec<u8>>,
    ) -> http::Response<Vec<u8>>
    where
        Host: StorageV1 + KeySpaceLoader<KeySpace = KS>,
        KS: KeySpace,
    {
        journal.record_request(&request);
        let response = match request.uri().host() {
            Some(h) if h == self.tezos.host() => {
                self.tezos.serve(self, rk, journal, request)
            }
            Some(h) if h == self.ethereum.host() => {
                self.ethereum.serve(self, rk, journal, request)
            }
            unknown => http::Response::builder()
                .status(http::StatusCode::NOT_FOUND)
                .body(
                    format!("No runtime handles host: {}", unknown.unwrap_or("(none)"))
                        .into_bytes(),
                )
                .unwrap(),
        };
        journal.record_response(&response);
        response
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use alloy_primitives::{hex::FromHex, Address, Bytes};
    use pretty_assertions::assert_eq;
    use revm_etherlink::helpers::storage::bytes_hash;
    use revm_etherlink::storage::world_state_handler::{
        AccountInfo, AccountOrigin, StorageAccount,
    };
    use tezos_crypto_rs::hash::ContractKt1Hash;
    use tezos_evm_runtime::runtime::MockKernelHost;
    use tezos_evm_runtime::runtime_keyspaces::RuntimeKeyspaces;
    use tezos_execution::{context, NULL_PKH};
    use tezos_smart_rollup::types::PublicKeyHash;
    use tezosx_interfaces::{Classification, Gas, Origin, RuntimeId, ALIAS_LOOKUP_COST};
    use tezosx_journal::TezosXJournal;

    #[test]
    fn test_serve_unknown_host_returns_404() {
        let registry = RegistryImpl::default();
        let mut host = MockKernelHost::default();
        let mut rk = RuntimeKeyspaces::init(&mut host).unwrap();

        let request = http::Request::builder()
            .uri("http://unknown/some/path")
            .body(vec![])
            .unwrap();

        let mut journal = TezosXJournal::mock(RuntimeId::Ethereum);
        let response = registry.serve(&mut rk, &mut journal, request);
        assert_eq!(response.status(), http::StatusCode::NOT_FOUND);
        let body = String::from_utf8(response.into_body()).unwrap();
        assert!(body.contains("unknown"));
    }

    #[test]
    fn test_serve_no_host_returns_404() {
        let registry = RegistryImpl::default();
        let mut host = MockKernelHost::default();
        let mut rk = RuntimeKeyspaces::init(&mut host).unwrap();

        let request = http::Request::builder()
            .uri("/some/path")
            .body(vec![])
            .unwrap();

        let mut journal = TezosXJournal::mock(RuntimeId::Ethereum);
        let response = registry.serve(&mut rk, &mut journal, request);
        assert_eq!(response.status(), http::StatusCode::NOT_FOUND);
        let body = String::from_utf8(response.into_body()).unwrap();
        assert!(body.contains("(none)"));
    }

    // ── Registry::read_origin dispatch tests ─────────────────────────────

    #[test]
    fn read_origin_dispatches_to_ethereum_runtime() {
        let mut host = MockKernelHost::default();
        let mut rk = RuntimeKeyspaces::init(&mut host).unwrap();
        let registry = RegistryImpl::default();

        let addr =
            Address::from_hex("0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa").unwrap();
        let addr_str = format!("0x{}", alloy_primitives::hex::encode(addr.0 .0));

        let mut account = StorageAccount::from_address(&addr).unwrap();
        account
            .set_info(
                rk.eth_accounts_mut(),
                AccountInfo {
                    origin: AccountOrigin::Native,
                    ..AccountInfo::default()
                },
            )
            .unwrap();

        let budget = Gas::new(100_000, RuntimeId::Ethereum);
        let (class, consumed) = registry
            .read_origin(&rk, RuntimeId::Ethereum, &addr_str, budget)
            .unwrap();
        assert_eq!(class, Classification::Native);
        assert_eq!(consumed, ALIAS_LOOKUP_COST); // recorded origin → no back-stop charge
    }

    #[test]
    fn read_origin_dispatches_to_tezos_runtime() {
        let mut host = MockKernelHost::default();
        let rk = RuntimeKeyspaces::init(&mut host).unwrap();
        let registry = RegistryImpl::default();

        // An implicit tz1 is Native by construction — no seeding needed.
        let budget = Gas::new(1_000_000, RuntimeId::Tezos);
        let (class, consumed) = registry
            .read_origin(
                &rk,
                RuntimeId::Tezos,
                "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx",
                budget,
            )
            .unwrap();
        assert_eq!(class, Classification::Native);
        // Tezos charges the same lookup, expressed in milligas.
        assert_eq!(consumed, ALIAS_LOOKUP_COST);
    }

    #[test]
    fn read_origin_ethereum_unknown_address_fires_backstop() {
        let mut host = MockKernelHost::default();
        let mut rk = RuntimeKeyspaces::init(&mut host).unwrap();
        let registry = RegistryImpl::default();

        let addr =
            Address::from_hex("0xbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb").unwrap();
        let addr_str = format!("0x{}", alloy_primitives::hex::encode(addr.0 .0));

        // Unclassified account with non-empty bytecode — exercises the
        // code-presence back-stop.
        let bytecode_raw = Bytes::from_static(&[0x60, 0x00]);
        let code_hash = bytes_hash(&bytecode_raw);
        let mut account = StorageAccount::from_address(&addr).unwrap();
        account
            .set_info(
                rk.eth_accounts_mut(),
                AccountInfo {
                    code_hash,
                    ..AccountInfo::default()
                },
            )
            .unwrap();

        let budget = Gas::new(100_000, RuntimeId::Ethereum);
        let (class, consumed) = registry
            .read_origin(&rk, RuntimeId::Ethereum, &addr_str, budget)
            .unwrap();
        assert_eq!(class, Classification::Native);
        assert_eq!(consumed, ALIAS_LOOKUP_COST);
    }

    #[test]
    fn read_origin_tezos_implicit_address_native_no_backstop_charge() {
        let mut host = MockKernelHost::default();
        let rk = RuntimeKeyspaces::init(&mut host).unwrap();
        let registry = RegistryImpl::default();

        let budget = Gas::new(1_000_000, RuntimeId::Tezos);
        // An implicit tz1 with no durable state: it is Tezos-native by
        // construction (a public-key hash can never be a cross-runtime
        // alias), so it classifies Native without a durable read. Unlike the
        // Ethereum path, a Tezos lookup never fires the code back-stop, so
        // only the flat lookup milligas is charged.
        let (class, consumed) = registry
            .read_origin(
                &rk,
                RuntimeId::Tezos,
                "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx",
                budget,
            )
            .unwrap();
        assert_eq!(class, Classification::Native);
        assert_eq!(consumed, ALIAS_LOOKUP_COST);
    }

    #[test]
    fn ensure_alias_is_idempotent_no_op() {
        // First branch of the function. A second call with the same
        // input must return the same address with the gas budget
        // unchanged. The first call deploys; the second is just a
        // read of the classification path.
        let registry = RegistryImpl::default();
        let mut host = MockKernelHost::default();
        let mut rk = RuntimeKeyspaces::init(&mut host).unwrap();
        let mut journal = TezosXJournal::mock(RuntimeId::Ethereum);
        let native_address = "0x3333333333333333333333333333333333333333";
        let alias_info = tezosx_interfaces::AliasInfo {
            native_address: native_address.to_string(),
            runtime: RuntimeId::Ethereum,
        };
        let first = registry
            .ensure_alias(
                &mut rk,
                &mut journal,
                alias_info.clone(),
                None,
                RuntimeId::Ethereum,
                tezosx_interfaces::CrossRuntimeContext {
                    block_number: U256::from(0),
                    timestamp: U256::from(0),
                },
                tezosx_interfaces::Gas::new(100_000, RuntimeId::Ethereum),
            )
            .unwrap();

        // The first call materialized the alias: the classification is
        // staged in the EVM journal (it flushes to durable storage at
        // commit), and it points back at the native address.
        let alias = Address::from_hex(&first.0).unwrap();
        assert_eq!(
            journal.evm.layered_state.pending_alias_origin(&alias),
            Some(Origin::Alias(alias_info.clone()))
        );

        let second = registry
            .ensure_alias(
                &mut rk,
                &mut journal,
                alias_info,
                None,
                RuntimeId::Ethereum,
                tezosx_interfaces::CrossRuntimeContext {
                    block_number: U256::from(0),
                    timestamp: U256::from(0),
                },
                tezosx_interfaces::Gas::new(100_000, RuntimeId::Ethereum),
            )
            .unwrap();
        assert_eq!(first.0, second.0);
        assert_eq!(
            second.1.gas_remaining,
            tezosx_interfaces::Gas::new(100_000, RuntimeId::Ethereum)
        );
    }

    #[test]
    fn ensure_alias_is_idempotent_no_op_michelson() {
        // Companion to `ensure_alias_is_idempotent_no_op`, driven from the
        // Michelson runtime: the alias of an EVM address is a KT1 whose
        // classification `create_alias` writes durably, so the second call
        // only reads it back and leaves the gas budget unchanged.
        let registry = RegistryImpl::default();
        let mut host = MockKernelHost::default();
        let mut rk = RuntimeKeyspaces::init(&mut host).unwrap();
        // Originating the forwarder snapshots the Michelson world state, so
        // seed the subtree the way migration does in production.
        let null_pkh = PublicKeyHash::from_b58check(NULL_PKH).unwrap();
        context::implicit_from_public_key_hash(&null_pkh)
            .unwrap()
            .allocate(rk.host_mut())
            .unwrap();

        let mut journal = TezosXJournal::mock(RuntimeId::Ethereum);
        let native_address = "0x3333333333333333333333333333333333333333";
        let alias_info = tezosx_interfaces::AliasInfo {
            native_address: native_address.to_string(),
            runtime: RuntimeId::Ethereum,
        };
        let budget = Gas::new(5_000_000, RuntimeId::Tezos);
        let first = registry
            .ensure_alias(
                &mut rk,
                &mut journal,
                alias_info.clone(),
                None,
                RuntimeId::Tezos,
                tezosx_interfaces::CrossRuntimeContext {
                    block_number: U256::from(0),
                    timestamp: U256::from(0),
                },
                budget,
            )
            .unwrap();

        // The first call materialized the alias: the KT1 carries the
        // classification record pointing back at the EVM address.
        let kt1 = ContractKt1Hash::from_base58_check(&first.0).unwrap();
        let account = context::originated_from_kt1(&kt1).unwrap();
        assert_eq!(
            account.origin(rk.host()).unwrap(),
            Some(Origin::Alias(alias_info.clone()))
        );

        let second = registry
            .ensure_alias(
                &mut rk,
                &mut journal,
                alias_info,
                None,
                RuntimeId::Tezos,
                tezosx_interfaces::CrossRuntimeContext {
                    block_number: U256::from(0),
                    timestamp: U256::from(0),
                },
                budget,
            )
            .unwrap();
        assert_eq!(first.0, second.0);
        assert_eq!(second.1.gas_remaining, budget);
    }
}
