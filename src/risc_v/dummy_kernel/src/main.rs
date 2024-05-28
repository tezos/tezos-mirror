mod sbi_crypto;

use tezos_crypto_rs::blake2b::digest_256;
use tezos_smart_rollup::{
    inbox::InboxMessage, kernel_entry, michelson::MichelsonUnit, prelude::*,
    storage::path::OwnedPath, types::SmartRollupAddress,
};

pub fn entry(host: &mut impl Runtime) {
    let msg = "Hello World\n";
    debug_msg!(host, "{}", msg);

    let meta = host.reveal_metadata();
    debug_msg!(host, "I am {}\n", SmartRollupAddress::new(meta.address()));
    debug_msg!(host, "{:#?}\n", meta);

    let hash = digest_256(msg.as_bytes()).unwrap();
    debug_msg!(host, "{:02X?}\n", hash);

    unsafe {
        assert_eq!(hash, sbi_crypto::blake2b_hash256(msg.as_bytes()));
    }

    while let Some(msg) = host.read_input().expect("Want message") {
        let (_, msg) = InboxMessage::<MichelsonUnit>::parse(msg.as_ref())
            .expect("Failed to parse inbox message");
        debug_msg!(host, "{:#?}\n", msg);
    }

    let path: OwnedPath = "/hello".as_bytes().to_vec().try_into().unwrap();
    let () = host
        .store_write(&path, msg.as_bytes(), 0)
        .expect("Could not write to storage");
    let read_msg = host
        .store_read(&path, 0, msg.len())
        .expect("Could not read from storage");
    assert_eq!(read_msg.as_slice(), msg.as_bytes());

    unsafe {
        let public_key: [u8; 32] = [
            171, 32, 104, 249, 65, 125, 118, 36, 210, 237, 61, 116, 43, 133, 16, 15, 177, 4, 114,
            245, 84, 7, 13, 184, 49, 110, 76, 46, 147, 20, 137, 69,
        ];
        let secret_key: [u8; 32] = [
            147, 228, 82, 23, 29, 245, 139, 43, 80, 32, 225, 196, 0, 239, 143, 245, 138, 203, 227,
            208, 158, 63, 121, 41, 8, 220, 224, 224, 33, 132, 133, 237,
        ];

        let data = "Hello World".as_bytes();

        let sig = sbi_crypto::ed25519_sign(&secret_key, data);
        debug_msg!(host, "Signature is {sig:?}\n");

        assert!(sbi_crypto::ed25519_verify(&public_key, &sig, data));
    }

    debug_msg!(host, "Done\n");

    // Drain the inbox, making the sandbox stop.
    while host.read_input().map(|msg| msg.is_some()).unwrap_or(true) {}
}

kernel_entry!(entry);
