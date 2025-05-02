mod sbi_crypto;

#[cfg(target_os = "linux")]
mod linux_only;

use tezos_crypto_rs::blake2b::digest_256;
use tezos_smart_rollup::entrypoint;
use tezos_smart_rollup::inbox::InboxMessage;
use tezos_smart_rollup::inbox::InternalInboxMessage;
use tezos_smart_rollup::michelson::MichelsonUnit;
use tezos_smart_rollup::prelude::*;
use tezos_smart_rollup::storage::path::OwnedPath;
use tezos_smart_rollup::types::SmartRollupAddress;
use tezos_smart_rollup_constants::core::PREIMAGE_HASH_SIZE;
use tezos_smart_rollup_constants::riscv::SBI_FIRMWARE_TEZOS;
use tezos_smart_rollup_constants::riscv::SBI_TEZOS_REVEAL;
use tezos_smart_rollup_constants::riscv::SbiError;

#[entrypoint::main]
pub fn entry(host: &mut impl Runtime) {
    #[cfg(target_os = "linux")]
    linux_only::dummy();

    let msg = "Hello World\n";
    debug_msg!(host, "{}", msg);

    let meta = host.reveal_metadata();
    debug_msg!(host, "I am {}\n", SmartRollupAddress::new(meta.address()));
    debug_msg!(host, "{:#?}\n", meta);

    let hash = digest_256(msg.as_bytes());
    debug_msg!(host, "{:02X?}\n", hash);

    unsafe {
        assert_eq!(hash, sbi_crypto::blake2b_hash256(msg.as_bytes()));
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

    host.write_debug("Reveal metadata...\n");
    let result = host.reveal_metadata();
    host.write_debug("Reveal metadata succeeded, result: \n");
    host.write_debug(&format!("Rollup address: {:?}\n", result));

    host.write_debug("Reveal preimage...\n");

    let hash: [u8; PREIMAGE_HASH_SIZE] = [
        // tag byte of preimage hash
        vec![0u8],
        digest_256(hex::decode("cafebabe").unwrap().as_slice()),
    ]
    .concat()
    .try_into()
    .unwrap();
    let mut buffer = [0u8; 4096];
    let result_size = host.reveal_preimage(&hash, &mut buffer[..]).unwrap();
    host.write_debug(&format!(
        "Preimage: {:?}\n",
        hex::encode(&buffer[..result_size])
    ));

    host.write_debug("Invalid reveal request...\n");

    let payload = [255u8];
    let mut buffer = [0u8; 4096];

    // An ecall is made via inline assembly to trigger the invalid reveal request.
    let result: isize;
    unsafe {
        core::arch::asm!(
            "ecall",
            in("a0") &payload,
            in("a1") std::mem::size_of_val(&payload),
            in("a2") &mut buffer,
            in("a3") std::mem::size_of_val(&buffer),
            in("a6") SBI_TEZOS_REVEAL,
            in("a7") SBI_FIRMWARE_TEZOS,
            lateout("a0") result,
        );
    }

    assert!(result == SbiError::InvalidParam as isize);

    debug_msg!(host, "Reveals Done\n");

    while let Some(msg) = host.read_input().expect("Want message") {
        let (_, msg) = InboxMessage::<MichelsonUnit>::parse(msg.as_ref())
            .expect("Failed to parse inbox message");
        match msg {
            // When running a rollup node with this kernel through Tezt tests
            // `InfoPerlevel` messages are not identical for each run. This can
            // result in slight changes to the number of ticks used to
            // format them, which causes regression tests to fail.
            InboxMessage::Internal(InternalInboxMessage::InfoPerLevel(_)) => {
                debug_msg!(host, "Internal(InfoPerLevel)\n")
            }
            msg => debug_msg!(host, "{:#?}\n", msg),
        }
    }
}
