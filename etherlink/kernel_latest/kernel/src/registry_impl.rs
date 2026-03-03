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
use tezos_evm_runtime::runtime::Runtime;
use tezosx_ethereum_runtime::EthereumRuntime;
use tezosx_interfaces::{CrossCallResult, Registry, RuntimeInterface};
use tezosx_tezos_runtime::TezosRuntime;

impl Registry for RegistryImpl {
    fn bridge<Host: Runtime>(
        &self,
        host: &mut Host,
        destination_runtime: tezosx_interfaces::RuntimeId,
        destination_address: &[u8],
        source_address: &[u8],
        amount: U256,
        data: &[u8],
        context: tezosx_interfaces::CrossRuntimeContext,
    ) -> Result<CrossCallResult, tezosx_interfaces::TezosXRuntimeError> {
        match destination_runtime {
            tezosx_interfaces::RuntimeId::Tezos => self.tezos.call(
                self,
                host,
                source_address,
                destination_address,
                amount,
                data,
                context,
            ),
            tezosx_interfaces::RuntimeId::Ethereum => self.ethereum.call(
                self,
                host,
                source_address,
                destination_address,
                amount,
                data,
                context,
            ),
        }
    }

    fn generate_alias<Host: Runtime>(
        &self,
        host: &mut Host,
        native_address: &[u8],
        runtime_id: tezosx_interfaces::RuntimeId,
        context: tezosx_interfaces::CrossRuntimeContext,
    ) -> Result<Vec<u8>, tezosx_interfaces::TezosXRuntimeError> {
        match runtime_id {
            tezosx_interfaces::RuntimeId::Tezos => {
                self.tezos
                    .generate_alias(self, host, native_address, context)
            }
            tezosx_interfaces::RuntimeId::Ethereum => {
                self.ethereum
                    .generate_alias(self, host, native_address, context)
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

    fn serve<Host: Runtime>(
        &self,
        host: &mut Host,
        request: http::Request<Vec<u8>>,
    ) -> Result<http::Response<Vec<u8>>, tezosx_interfaces::TezosXRuntimeError> {
        match request.uri().host() {
            Some(h) if h == self.tezos.host() => self.tezos.serve(self, host, request),
            Some(h) if h == self.ethereum.host() => {
                self.ethereum.serve(self, host, request)
            }
            unknown => Ok(http::Response::builder()
                .status(http::StatusCode::NOT_FOUND)
                .body(
                    format!("No runtime handles host: {}", unknown.unwrap_or("(none)"))
                        .into_bytes(),
                )
                .unwrap()),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tezos_evm_runtime::runtime::MockKernelHost;

    #[test]
    fn test_serve_unknown_host_returns_404() {
        let registry = RegistryImpl::default();
        let mut host = MockKernelHost::default();

        let request = http::Request::builder()
            .uri("http://unknown/some/path")
            .body(vec![])
            .unwrap();

        let response = registry.serve(&mut host, request).unwrap();
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

        let response = registry.serve(&mut host, request).unwrap();
        assert_eq!(response.status(), http::StatusCode::NOT_FOUND);
        let body = String::from_utf8(response.into_body()).unwrap();
        assert!(body.contains("(none)"));
    }
}
