use tezos_smart_rollup::prelude::*;

pub fn main(host: impl Runtime) {
    let msg = "Hello World\n";
    debug_msg!(host, "{}", msg);
}
