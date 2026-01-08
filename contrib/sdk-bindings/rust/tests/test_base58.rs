// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use tezos::{hash::*, Error};
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
    block_hash_decode_encode,
    BlockHash,
    "BKiHLREqU3JkXfzEDYAkmmfX48gBDtYhMrpA98s7Aq4SzbUAB6M",
    "BMa4rETz3gBbgRfTNp1swZL9GbUSSCf2riufwBDav26xEDsEmGe",
    "BLJWtjDMGwa31xTPsyeTBSCuxzUPBMcXehfXJ4v2aShHm3VYWvm"
);

test_b58check!(
    kt1_decode_encode,
    ContractKt1Hash,
    "KT18amZmM5W7qDWVt2pH6uj7sCEd3kbzLrHT",
    "KT1BzqPeYDt8xWmgjkGJupRtdxMsZS37Xm7U",
    "KT1Bt81PD1FwEw8cou8z391vdWAKtLaEnC8z"
);

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
    unknown_sig_decode_encode,
    UnknownSignature,
    // sk: edsk4QUBg4kqJD5u5mvkwWe6qnmimoL3sAy8v2vDWEnWPbJeEcMMZp
    // msg: "a"
    "sigWrzQCbre6B7VLP4kGntoQGrEBLLvc8cFPySNiDj5m2cTd4DfJG2feBLhgyjtHcTiLenwrActDgp9y6pxp3MS5m5sqVCY2",
    // sk: spsk36wmmgfs88ffW7ujgw9wm511zhZUkM4GnsYxhHMD9ZDy1AsKRa
    // msg: "a"
    "sigaeDzBgzkdvQRhYV2YUFpzYGE19bRe62YdZnnodZMDSXD9P97jBro2v8W2o8a1wRxfJEWJpLEbv2W1TM8jKqBdFCCuKcDp",
    // sk: p2sk3EC67qL8iPQv6o2j14bgqKR6JaPBahXHpZFypVCGkbeMvv5H5G
    // msg: "a"
    "sigTCLMigPjVA1ogWQP16sp6DpcAb3Wf4wkxUWDBHUa8wuixro5AVQLkWCGtBH91iYdcDLwPqW2yFs2XppXfBdbCSdbD3v9f",
);

test_b58check!(
    edsig_decode_encode,
    Ed25519Signature,
    // sk: edsk3x51ok91omWQd5ZBzBuMV3pjCyFCjxh4fyoTRJ4QYuk59sjZch
    // msg: "a"
    "edsigtditBCndqgbQf4qrDzUoLjhq79VfxfRzsBp7EctpgPP1Qn3EPuTVjxSt1qyxvyEW9S3HCd4XyehT2tAcG2ATPDaYyjZaLJ"
);

test_b58check!(
    spsig_decode_encode,
    Secp256k1Signature,
    // sk: spsk1ppL4ohtyZeighKZehzfGr2p6dL51kwQqEV2N1sNT7rx9cg5jG
    // msg: "a"
    "spsig1Dm7vPsHHTNdeYrTYtyBjEgwmdcMpCRBEHctmE63qQ4Cbt78sNxLYFG2qQq7ouQwqAqH2hGXr7fi2fAJh2WLUtvDFbSH2e"
);

test_b58check!(
    p2sig_decode_encode,
    P256Signature,
    // sk: p2sk432A1xe56akKM7n8vk4g7tSyTTWodJgcwwH9mSmA6jjrApUB3F
    // msg: "a"
    "p2sigqUk7jhcXFUQqLkRhb7o3oTyp7hbttCenbcrsg4qmmh4HjM4fHfXExj7raBTFoaJJKt3oR3sUjy4V163rkQLx9pAYERW7r"
);

test_b58check!(
    blsig_decode_encode,
    BlsSignature,
    // sk: BLsk27Wo9tVS8XbNDfCD7X9FnQBwZ4o2gQCZxXkJRmdFYoUTvuTu1S
    // msg: "a"
    "BLsigB5MLy2Godk2FSEfWyi1A8XsvgMmZq6siYN7fdTiu5kRapXtrd7Ebit97qinPmF9j5zpPGGoe7Du6JB2Ybpo9839z4E6CSQRNCR3FRqAfWYdoMLz5cTYjmYT1MczxXahrnV8JtZzNF"
);

test_b58check!(
    contract_decode_encode,
    Contract,
    "tz1SUWNMC3hUdBRzzrbTbiuGPH1KFVifTQw7",
    "tz2PbzLDYrPAZS38BteBY7gqtnZfsTqHF2xu",
    "tz3hqqamVC1G22LACFoMgcJeFKZgoGMFSfSn",
    "tz4Uzyxg26DJyM4pc1V2pUvLpdsR5jdyzYsZ",
    "KT1S5cQmS4wXjG7JubRUCWzH3DaU7S2XfeFT",
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
    sig_decode_encode,
    Signature,
    // sk: edsk4BYt5bFFa7rHeoZSxgbTUJFYJjSynS9tFgFBzjQpRTL4Gku2x2
    // msg: "a"
    "edsigtgNwQE9rwEWL8Jr7AL414yUN1ZvVK6dZrCf5fuXUxn9q8Te2SVbzFcw5VdgKipscVoYAcvFyAnJDB7MRQRswPGKY4DY73h",
    // sk: spsk236JVFEqgJtsg4EbVeMjcppsumD1CdwNiBnP1YWeDhHubWRh1Z
    // msg: "a"
    "spsig1bXDvP4WArPkBc7HeUkLWhk2KXjD28JZHMyVXDkFMiX7o7tjAy8x9caU2MSYJUAp5mhAa9aX3KcSxbsWnnTM965AFsVELH",
    // sk: p2sk3ne2WApaPmU9QiBK7BKgaSi18YB2J7Sv81YMykyFwWaYYyAoHz
    // msg: "a"
    "p2sigVyZHbirqggsswTg872LQZGJhvmeJ4USDMTs61Baqd6UNbwrSwx7W55Z5j6AK3Nw9BV4qcPhenW1mFYsX5etkXpTS9ek75",
    // sk: BLsk1wk9JVZKhzsdHExkMR2kAPeZrdtpLaACTMtwxzvavje3n22msQ
    // msg: "a"
    "BLsigB898bZ9fssKyvZfxCvpmNWZuSSEer2tDgb7GB55KEoYevyUqE7oh9mprT9G4eRFdjKeybHXaWoN2kQGpBJNBssawpCmRuivPoLytj19wUtceFbgDDy2d1XATvt4mE5sHHPGpmP773",
    // sk: edsk4abGREee5mNXXBQ6mm14YMbRpinMHSi7iKJCyou39agACSfnxf
    // msg: "a"
    "sighMHEn46gJGa5DBq3ag7MXEEbgGEntEuKqWVJV6C2JYmLZ2ZzfeM85LwCA4Juyn2VouibrFr2WsgEWMbNZ1sZpXx2yiVn1",
    // sk: spsk36X1Kac5Ez6JZ7dVnfjku1VJMNpe8yJuuNAKMmxx1PKtMBoFrw
    // msg: "a"
    "sigcWjAnrCe5LuNJFZrAt5avZ1vebmjXTugHZUAYsUqCtGmbDd17BqiMnp44hXqdt9wPikbHZfnC7veNRt4nAVZsEt8DRPZd",
    // sk: p2sk35P3PjBAGA7TLjzK9jkSerSj1n9sP2ttLtaQ3HR6Uv71smpFzB
    // msg: "a"
    "sigw87gSwNrAt8DJUG8kFCgiy8PB3rfrRYrHYtcBNhDeiBnrhk4B9MwaJDSb6BXDttvTeTMJkkSK6DP4u44Q336qtb7Fgrif",
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
