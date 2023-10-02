#[derive(Debug, Default)]
pub struct Ctx {
    pub gas: crate::gas::Gas,
    pub amount: i64,
}
