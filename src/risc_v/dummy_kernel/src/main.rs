use tezos_smart_rollup::{
    inbox::InboxMessage, kernel_entry, michelson::MichelsonUnit, prelude::*,
    types::SmartRollupAddress,
};

pub fn entry(host: &mut impl Runtime) {
    let msg = "Hello World\n";
    debug_msg!(host, "{}", msg);

    let meta = host.reveal_metadata();
    debug_msg!(host, "I am {}\n", SmartRollupAddress::new(meta.address()));
    debug_msg!(host, "{:#?}\n", meta);

    use tezos_crypto_rs::blake2b::digest_256;
    let hash = digest_256(msg.as_bytes()).unwrap();
    debug_msg!(host, "{:02X?}\n", hash);

    while let Some(msg) = host.read_input().expect("Want message") {
        let (_, msg) = InboxMessage::<MichelsonUnit>::parse(msg.as_ref())
            .expect("Failed to parse inbox message");
        debug_msg!(host, "{:#?}\n", msg);
    }

    panic!("Abort");
}

kernel_entry!(entry);
