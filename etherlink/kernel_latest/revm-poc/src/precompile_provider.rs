use revm::{
    context::{Cfg, ContextTr},
    handler::PrecompileProvider,
    interpreter::{InputsImpl, InterpreterResult},
    primitives::Address,
};

pub struct Dummy {}

impl<CTX> PrecompileProvider<CTX> for Dummy
where
    CTX: ContextTr,
{
    type Output = InterpreterResult;

    fn set_spec(&mut self, _spec: <CTX::Cfg as Cfg>::Spec) -> bool {
        unimplemented!()
    }

    fn run(
        &mut self,
        _context: &mut CTX,
        _address: &Address,
        _inputs: &InputsImpl,
        _is_static: bool,
        _gas_limit: u64,
    ) -> Result<Option<Self::Output>, String> {
        unimplemented!()
    }

    fn warm_addresses(&self) -> Box<impl Iterator<Item = Address>> {
        Box::new(vec![].into_iter())
    }

    fn contains(&self, _address: &Address) -> bool {
        unimplemented!()
    }
}
