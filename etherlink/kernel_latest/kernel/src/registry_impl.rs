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
use tezos_smart_rollup_host::storage::StorageV1;
use tezosx_ethereum_runtime::EthereumRuntime;
use tezosx_interfaces::{Registry, RuntimeInterface};
use tezosx_journal::TezosXJournal;
use tezosx_tezos_runtime::TezosRuntime;

impl Registry for RegistryImpl {
    fn ensure_alias<Host>(
        &self,
        host: &mut Host,
        journal: &mut TezosXJournal,
        alias_info: tezosx_interfaces::AliasInfo,
        native_public_key: Option<&[u8]>,
        target_runtime: tezosx_interfaces::RuntimeId,
        context: tezosx_interfaces::CrossRuntimeContext,
        gas_remaining: u64,
    ) -> Result<
        (String, tezosx_interfaces::AliasResolution),
        tezosx_interfaces::TezosXRuntimeError,
    >
    where
        Host: StorageV1,
    {
        match target_runtime {
            tezosx_interfaces::RuntimeId::Tezos => self.tezos.ensure_alias(
                self,
                host,
                journal,
                alias_info,
                native_public_key,
                context,
                gas_remaining,
            ),
            tezosx_interfaces::RuntimeId::Ethereum => self.ethereum.ensure_alias(
                self,
                host,
                journal,
                alias_info,
                native_public_key,
                context,
                gas_remaining,
            ),
        }
    }

    fn compute_alias(
        &self,
        alias_info: tezosx_interfaces::AliasInfo,
    ) -> Result<String, tezosx_interfaces::TezosXRuntimeError> {
        match alias_info.runtime {
            tezosx_interfaces::RuntimeId::Tezos => {
                self.tezos.compute_alias(&alias_info.native_address)
            }
            tezosx_interfaces::RuntimeId::Ethereum => {
                self.ethereum.compute_alias(&alias_info.native_address)
            }
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

    fn read_origin<Host>(
        &self,
        host: &Host,
        addr_runtime: tezosx_interfaces::RuntimeId,
        addr: &str,
        budget: u64,
    ) -> Result<
        (tezosx_interfaces::Classification, u64),
        tezosx_interfaces::TezosXRuntimeError,
    >
    where
        Host: StorageV1,
    {
        match addr_runtime {
            tezosx_interfaces::RuntimeId::Tezos => {
                self.tezos.read_origin(host, addr, budget)
            }
            tezosx_interfaces::RuntimeId::Ethereum => {
                self.ethereum.read_origin(host, addr, budget)
            }
        }
    }

    fn serve<Host>(
        &self,
        host: &mut Host,
        journal: &mut TezosXJournal,
        request: http::Request<Vec<u8>>,
    ) -> http::Response<Vec<u8>>
    where
        Host: StorageV1,
    {
        journal.record_request(&request);
        let response = match request.uri().host() {
            Some(h) if h == self.tezos.host() => {
                self.tezos.serve(self, host, journal, request)
            }
            Some(h) if h == self.ethereum.host() => {
                self.ethereum.serve(self, host, journal, request)
            }
            unknown => http::Response::builder()
                .status(http::StatusCode::NOT_FOUND)
                .body(
                    format!("No runtime handles host: {}", unknown.unwrap_or("(none)"))
                        .into_bytes(),
                )
                .unwrap(),
        };
        journal.record_response(response.clone());
        response
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use alloy_primitives::{hex::FromHex, Address, Bytes};
    use revm_etherlink::helpers::storage::bytes_hash;
    use revm_etherlink::storage::world_state_handler::{
        AccountInfo, AccountOrigin, StorageAccount,
    };
    use tezos_crypto_rs::public_key_hash::PublicKeyHash;
    use tezos_evm_runtime::runtime::MockKernelHost;
    use tezosx_interfaces::{
        Classification, Origin, RuntimeId, ALIAS_LOOKUP_COST, ALIAS_LOOKUP_MILLIGAS,
    };
    use tezosx_journal::TezosXJournal;
    use tezosx_tezos_runtime::account::set_origin_for_implicit;

    #[test]
    fn test_serve_unknown_host_returns_404() {
        let registry = RegistryImpl::default();
        let mut host = MockKernelHost::default();

        let request = http::Request::builder()
            .uri("http://unknown/some/path")
            .body(vec![])
            .unwrap();

        let mut journal = TezosXJournal::mock(RuntimeId::Ethereum);
        let response = registry.serve(&mut host, &mut journal, request);
        assert_eq!(response.status(), http::StatusCode::NOT_FOUND);
        let body = String::from_utf8(response.into_body()).unwrap();
        assert!(body.contains("unknown"));
    }

    #[test]
    fn test_serve_no_host_returns_404() {
        let registry = RegistryImpl::default();
        let mut host = MockKernelHost::default();

        let request = http::Request::builder()
            .uri("/some/path")
            .body(vec![])
            .unwrap();

        let mut journal = TezosXJournal::mock(RuntimeId::Ethereum);
        let response = registry.serve(&mut host, &mut journal, request);
        assert_eq!(response.status(), http::StatusCode::NOT_FOUND);
        let body = String::from_utf8(response.into_body()).unwrap();
        assert!(body.contains("(none)"));
    }

    // ── Registry::read_origin dispatch tests ─────────────────────────────

    #[test]
    fn read_origin_dispatches_to_ethereum_runtime() {
        let mut host = MockKernelHost::default();
        let registry = RegistryImpl::default();

        let addr =
            Address::from_hex("0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa").unwrap();
        let addr_str = format!("0x{}", alloy_primitives::hex::encode(addr.0 .0));

        let mut account = StorageAccount::from_address(&addr).unwrap();
        account
            .set_info(
                &mut host,
                AccountInfo {
                    origin: AccountOrigin::Native,
                    ..AccountInfo::default()
                },
            )
            .unwrap();

        let budget = 100_000;
        let (class, consumed) = registry
            .read_origin(&host, RuntimeId::Ethereum, &addr_str, budget)
            .unwrap();
        assert_eq!(class, Classification::Native);
        assert_eq!(consumed, ALIAS_LOOKUP_COST); // recorded origin → no back-stop charge
    }

    #[test]
    fn read_origin_dispatches_to_tezos_runtime() {
        let mut host = MockKernelHost::default();
        let registry = RegistryImpl::default();

        let pkh =
            PublicKeyHash::from_b58check("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx").unwrap();
        set_origin_for_implicit(&mut host, &pkh, &Origin::Native).unwrap();

        let budget = 1_000_000;
        let (class, consumed) = registry
            .read_origin(
                &host,
                RuntimeId::Tezos,
                "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx",
                budget,
            )
            .unwrap();
        assert_eq!(class, Classification::Native);
        assert_eq!(consumed, ALIAS_LOOKUP_MILLIGAS); // Tezos: charges milligas
    }

    #[test]
    fn read_origin_ethereum_unknown_address_fires_backstop() {
        let mut host = MockKernelHost::default();
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
                &mut host,
                AccountInfo {
                    code_hash,
                    ..AccountInfo::default()
                },
            )
            .unwrap();

        let budget = 100_000;
        let (class, consumed) = registry
            .read_origin(&host, RuntimeId::Ethereum, &addr_str, budget)
            .unwrap();
        assert_eq!(class, Classification::Native);
        assert_eq!(consumed, ALIAS_LOOKUP_COST);
    }

    #[test]
    fn read_origin_tezos_unknown_address_no_backstop_charge() {
        let host = MockKernelHost::default();
        let registry = RegistryImpl::default();

        let budget = 1_000_000;
        let (class, consumed) = registry
            .read_origin(
                &host,
                RuntimeId::Tezos,
                "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx",
                budget,
            )
            .unwrap();
        assert_eq!(class, Classification::Unknown);
        assert_eq!(consumed, ALIAS_LOOKUP_MILLIGAS); // Tezos: charges milligas even on miss
    }
}
