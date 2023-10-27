use tezos_smart_rollup_core::SmartRollupCore;

pub fn main(host: impl SmartRollupCore) {
    let msg = "Hello World\n";
    unsafe {
        host.write_debug(msg.as_ptr(), msg.len());
    }
}
