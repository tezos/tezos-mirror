pub struct RegistryImpl {
    ethereum: tezosx_ethereum_runtime::EthereumRuntime,
    tezos: tezosx_tezos_runtime::TezosRuntime,
}

impl Default for RegistryImpl {
    fn default() -> Self {
        Self::new(U256::from(1337))
    }
}

impl RegistryImpl {
    pub fn new(eth_chain_id: U256) -> Self {
        Self {
            ethereum: EthereumRuntime::new(eth_chain_id),
            tezos: TezosRuntime {},
        }
    }
}

use primitive_types::U256;
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
    ) -> Result<CrossCallResult, tezosx_interfaces::TezosXRuntimeError> {
        match destination_runtime {
            tezosx_interfaces::RuntimeId::Tezos => self.tezos.call(
                self,
                host,
                source_address,
                destination_address,
                amount,
                data,
            ),
            tezosx_interfaces::RuntimeId::Ethereum => self.ethereum.call(
                self,
                host,
                source_address,
                destination_address,
                amount,
                data,
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
}
