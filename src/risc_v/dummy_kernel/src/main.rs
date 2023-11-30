use tezos_smart_rollup::{kernel_entry, prelude::*};

pub fn entry(host: &mut impl Runtime) {
    let msg = "Hello World\n";
    debug_msg!(host, "{}", msg);
    use tezos_crypto_rs::blake2b::digest_256;
    let hash = digest_256(msg.as_bytes()).unwrap();
    debug_msg!(host, "{:02X?}\n", hash);
    panic!("Abort");
}

kernel_entry!(entry);
