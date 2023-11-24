use crate::gas::Gas;

#[derive(Debug)]
pub struct Ctx {
    pub gas: Gas,
    pub amount: i64,
    pub chain_id: tezos_crypto_rs::hash::ChainId,
}

impl Default for Ctx {
    fn default() -> Self {
        Ctx {
            gas: Gas::default(),
            amount: 0,
            // the default chain id is NetXynUjJNZm7wi, which is also the default chain id of octez-client in mockup mode
            chain_id: tezos_crypto_rs::hash::ChainId(vec![0xf3, 0xd4, 0x85, 0x54]),
        }
    }
}
