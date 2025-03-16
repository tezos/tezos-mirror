// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use tezos::{keys::*, Error};
use tezos_crypto_rs::base58;

macro_rules! test_b58check {
    ($name:ident, $ty:ty, $($b58:expr),+ $(,)?) => {
        #[test]
        fn $name() {
            $(
                let value = <$ty>::from_b58check($b58).expect(&format!(
                    "Converting {} into {} should succeed",
                    $b58,
                    stringify!($ty)
                ));
                let b58 = value.to_b58check();
                assert_eq!(b58, $b58, "Value must not have changed");
            )+
        }
    };
}

// All keys were generated using `octez-client`

test_b58check!(
    tz1_decode_encode,
    ContractTz1Hash,
    // sk: edsk3x51ok91omWQd5ZBzBuMV3pjCyFCjxh4fyoTRJ4QYuk59sjZch
    "tz1XnHxtAwqiJzwNicuU1A1haG2RW5sxJ2VJ"
);

test_b58check!(
    tz2_decode_encode,
    ContractTz2Hash,
    // sk: spsk1ppL4ohtyZeighKZehzfGr2p6dL51kwQqEV2N1sNT7rx9cg5jG
    "tz2GNQB7rXjNXBX6msePzQ2nBWYUUGutYy5p"
);

test_b58check!(
    tz3_decode_encode,
    ContractTz3Hash,
    // sk: p2sk432A1xe56akKM7n8vk4g7tSyTTWodJgcwwH9mSmA6jjrApUB3F
    "tz3co4CsEMspEpBC7TstUupntak7zTixZyhH"
);

test_b58check!(
    tz4_decode_encode,
    ContractTz4Hash,
    // sk: BLsk27Wo9tVS8XbNDfCD7X9FnQBwZ4o2gQCZxXkJRmdFYoUTvuTu1S
    "tz4Quq6VcCeJVmCknjzTX5kcrhUzcMruoavF"
);

test_b58check!(
    pkh_decode_encode,
    PublicKeyHash,
    // sk: edsk4BYt5bFFa7rHeoZSxgbTUJFYJjSynS9tFgFBzjQpRTL4Gku2x2
    "tz1Pta3ShQiC5uZRjfivFUmYQYd4Lai1vJrU",
    // sk: spsk236JVFEqgJtsg4EbVeMjcppsumD1CdwNiBnP1YWeDhHubWRh1Z
    "tz2QwAWxWWyUyPsNyaQsXM6PpbggDA5Tc3im",
    // sk: p2sk3ne2WApaPmU9QiBK7BKgaSi18YB2J7Sv81YMykyFwWaYYyAoHz
    "tz3dvgoUnnXoAP9MDqAwiZwRtvTFHcuDMxTR",
    // sk: BLsk1wk9JVZKhzsdHExkMR2kAPeZrdtpLaACTMtwxzvavje3n22msQ
    "tz4KAEXbRNNbgK2UqYrAsHXPmwFponv1mnXC",
);

test_b58check!(
    edpk_decode_encode,
    PublicKeyEd25519,
    // sk: edsk3x51ok91omWQd5ZBzBuMV3pjCyFCjxh4fyoTRJ4QYuk59sjZch
    "edpkuD7aFMY3wQ2TVfVC9BJ6SdLVnZgnt67mTUuvkn8wo9v6Bqq48G"
);

test_b58check!(
    sppk_decode_encode,
    PublicKeySecp256k1,
    // sk: spsk1ppL4ohtyZeighKZehzfGr2p6dL51kwQqEV2N1sNT7rx9cg5jG
    "sppk7cW8fKpjrM2713enEhvk6e5xhctLdhVCbE2YzMLCmNBHhphH4zE"
);

test_b58check!(
    p2pk_decode_encode,
    PublicKeyP256,
    // sk: p2sk432A1xe56akKM7n8vk4g7tSyTTWodJgcwwH9mSmA6jjrApUB3F
    "p2pk65g1jMTQnMmLCRELobvVR8WAx7A89qyLVHSRNzTZ7mLDgx28fGD"
);

test_b58check!(
    blpk_decode_encode,
    PublicKeyBls,
    // sk: BLsk27Wo9tVS8XbNDfCD7X9FnQBwZ4o2gQCZxXkJRmdFYoUTvuTu1S
    "BLpk1xwPVMQMstf4Nt41TGYeFNeck9RQJ7pHGqWpJQYgsQqGSPnhfoMMmU6fTzr1BD3mPLyzCoQB"
);

test_b58check!(
    pk_decode_encode,
    PublicKey,
    // sk: edsk4BYt5bFFa7rHeoZSxgbTUJFYJjSynS9tFgFBzjQpRTL4Gku2x2
    "edpkuWbLgKBsLVDE5dn678Y4ogZUjBQtCdh4HebhfJ9CaTR2o6WpFp",
    // sk: spsk236JVFEqgJtsg4EbVeMjcppsumD1CdwNiBnP1YWeDhHubWRh1Z
    "sppk7ZMef32rXEYsxrAE7jjLVqrcbHPRA3ikSSrywzREro52PPKU42B",
    // sk: p2sk3ne2WApaPmU9QiBK7BKgaSi18YB2J7Sv81YMykyFwWaYYyAoHz
    "p2pk674NNbXHqaDT5HcDuLJrR3csgjdLNrq4yMUXD1zMrx4uTcwEPzA",
    // sk: BLsk1wk9JVZKhzsdHExkMR2kAPeZrdtpLaACTMtwxzvavje3n22msQ
    "BLpk1uJ3JLHwYWUrpk6rPmZ4jAjCzbQ3xjkW7HgumJMcT8WRiwTbYdzU9dF4mKK1hP4DaYvvCfZB",
);

#[test]
fn decode_invalid_checksum() {
    assert!(
        matches!(
            ContractTz1Hash::from_b58check("tz1XnHxtAwqiJzwNicuU1A1haG2RW5sxJ2Vj"),
            Err(Error::Base58(base58::FromBase58CheckError::InvalidChecksum))
        ),
        "Converting hash with invalid checksum must fail with a Base58(InvalidChecksum) error"
    );
}

#[test]
fn decode_invalid_prefix() {
    assert!(
        matches!(
            ContractTz1Hash::from_b58check("tz2GNQB7rXjNXBX6msePzQ2nBWYUUGutYy5p"),
            Err(Error::Base58(
                base58::FromBase58CheckError::IncorrectBase58Prefix
            ))
        ),
        "Converting hash with invalid prefix must fail with a Base58(IncorrectBase58Prefix) error"
    );
}

#[test]
fn decode_invalid_len() {
    assert!(
        matches!(
            ContractTz1Hash::from_b58check("JTi3L79QVzucqwub3PCXae8unJTjfXbYwFx4UdB"),
            Err(Error::Base58(
                base58::FromBase58CheckError::MismatchedLength {
                    expected: 23,
                    actual: 25
                }
            ))
        ),
        "Converting hash with invalid length must fail with a correct Base58(MismatchedLength) error"
    );
}

#[test]
fn decode_invalid_pkh() {
    assert!(
        matches!(
            PublicKeyHash::from_b58check(
                "edpkuWbLgKBsLVDE5dn678Y4ogZUjBQtCdh4HebhfJ9CaTR2o6WpFp"
            ),
            Err(Error::Base58(base58::FromBase58CheckError::InvalidBase58))
        ),
        "Converting a hash that is not a public key into a public key must fail with a Base58(InvalidBase58) error"
    );
}
