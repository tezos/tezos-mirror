use revm::{
    context::{Cfg, ContextTr, LocalContextTr},
    handler::{EthPrecompiles, PrecompileProvider},
    interpreter::{CallInput, InputsImpl, InterpreterResult},
    primitives::{hex::FromHex, Address, HashSet},
};
use tezos_evm_runtime::runtime::Runtime;

use crate::{database::AccountDatabase, withdrawal::withdrawal_precompile};

pub struct EtherlinkPrecompiles<'a, Host: Runtime> {
    host: &'a mut Host,
    customs: HashSet<Address>,
    builtins: EthPrecompiles,
}

impl<'a, Host: Runtime> EtherlinkPrecompiles<'a, Host> {
    #[allow(dead_code)]
    pub fn new(host: &'a mut Host) -> Self {
        Self {
            host,
            customs: HashSet::from([
                // Withdrawals
                Address::from_hex("0xff00000000000000000000000000000000000001").unwrap(),
                // FA bridge
                Address::from_hex("0xff00000000000000000000000000000000000002").unwrap(),
            ]),
            builtins: EthPrecompiles::default(),
        }
    }

    fn warm_addresses(&self) -> Box<impl Iterator<Item = Address>> {
        Box::new(self.builtins.warm_addresses().chain(self.customs.clone()))
    }

    fn contains(&self, address: &Address) -> bool {
        self.customs.contains(address) || self.builtins.contains(address)
    }

    fn run_custom_precompile<CTX>(
        &mut self,
        context: &mut CTX,
        address: &Address,
        inputs: &InputsImpl,
        is_static: bool,
        _gas_limit: u64,
    ) -> Result<Option<InterpreterResult>, String>
    where
        CTX: ContextTr,
        CTX::Db: AccountDatabase,
    {
        // NIT: can probably do this more efficiently by keeping an immutable
        // reference on the slice but next mutable call makes it nontrivial
        let input_bytes = match &inputs.input {
            CallInput::SharedBuffer(range) => {
                if let Some(slice) =
                    context.local().shared_memory_buffer_slice(range.clone())
                {
                    slice.to_vec()
                } else {
                    vec![]
                }
            }
            CallInput::Bytes(bytes) => bytes.to_vec(),
        };

        if address
            == &Address::from_hex("0xff00000000000000000000000000000000000001").unwrap()
        {
            let result = withdrawal_precompile(
                self.host,
                &input_bytes,
                context,
                is_static,
                inputs,
                address,
            )?;
            Ok(Some(result))
        } else {
            Ok(None)
        }
    }
}

impl<Host: Runtime, CTX> PrecompileProvider<CTX> for EtherlinkPrecompiles<'_, Host>
where
    CTX: ContextTr,
    CTX::Db: AccountDatabase,
{
    type Output = InterpreterResult;

    fn set_spec(&mut self, spec: <CTX::Cfg as Cfg>::Spec) -> bool {
        <EthPrecompiles as PrecompileProvider<CTX>>::set_spec(&mut self.builtins, spec)
    }

    fn run(
        &mut self,
        context: &mut CTX,
        address: &Address,
        inputs: &InputsImpl,
        is_static: bool,
        gas_limit: u64,
    ) -> Result<Option<Self::Output>, String> {
        if let Some(custom_result) =
            self.run_custom_precompile(context, address, inputs, is_static, gas_limit)?
        {
            return Ok(Some(custom_result));
        }

        self.builtins
            .run(context, address, inputs, is_static, gas_limit)
    }

    fn warm_addresses(&self) -> Box<impl Iterator<Item = Address>> {
        self.warm_addresses()
    }

    fn contains(&self, address: &Address) -> bool {
        self.contains(address)
    }
}
