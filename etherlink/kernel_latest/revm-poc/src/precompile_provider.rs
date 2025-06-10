use revm::{
    context::{Cfg, ContextTr},
    handler::{EthPrecompiles, PrecompileProvider},
    interpreter::{CallInput, Gas, InputsImpl, InstructionResult, InterpreterResult},
    primitives::{hex::FromHex, Address, Bytes, HashSet},
};

pub struct EtherlinkPrecompiles {
    customs: HashSet<Address>,
    builtins: EthPrecompiles,
}

impl EtherlinkPrecompiles {
    #[allow(dead_code)]
    pub fn new() -> Self {
        Self {
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

    fn run_custom_precompile(
        &self,
        _context: &mut impl ContextTr,
        _address: &Address,
        inputs: &InputsImpl,
        gas_limit: u64,
    ) -> Result<Option<InterpreterResult>, String> {
        let _input_bytes = match &inputs.input {
            CallInput::SharedBuffer(_range) => &[],
            CallInput::Bytes(bytes) => bytes.iter().as_slice(),
        };

        // TODO: precompile logic here

        let result = InterpreterResult {
            result: InstructionResult::Return,
            gas: Gas::new(gas_limit),
            output: Bytes::new(),
        };

        Ok(Some(result))
    }
}

impl<CTX: ContextTr> PrecompileProvider<CTX> for EtherlinkPrecompiles {
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
            self.run_custom_precompile(context, address, inputs, gas_limit)?
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
