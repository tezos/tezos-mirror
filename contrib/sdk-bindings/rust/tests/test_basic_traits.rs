// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use paste::paste;
use tezos::hash::*;

// All keys were generated using `octez-client`

macro_rules! generate_tests {
    ($type_name:ident, $raw_h1:expr, $raw_h2:expr) => {
        paste! {
            #[test]
            fn [<test_ $type_name:lower _equality>]() {
                let h = <$type_name>::from_b58check($raw_h1).unwrap();
                let h1 = <$type_name>::from_b58check($raw_h1).unwrap();
                let h2 = <$type_name>::from_b58check($raw_h2).unwrap();

                assert_eq!(h, h1);
                assert_ne!(h, h2);
            }

            #[test]
            fn [<test_ $type_name:lower _display>]() {
                let h = <$type_name>::from_b58check($raw_h1).unwrap();
                assert_eq!(format!("{}", h), $raw_h1);
            }

            #[test]
            fn [<test_ $type_name:lower _debug>]() {
                let h = <$type_name>::from_b58check($raw_h1).unwrap();
                assert!(format!("{:?}", h).contains($raw_h1));
            }
        }
    };
}

generate_tests!(
    BlockHash,
    "BLzGG2VsshDHBxmgWpMbUQKX8LWEMLLG3fEEE8NaGpEmh4NSjsZ",
    "BL4d8vHu4UQQ9D7ZSF4adpA9HcFm1oG8QfrnDpg1UmXXHiTUHkH"
);

generate_tests!(
    ContractKt1Hash,
    "KT1G4D3W9cf86dzAmZBN9nwUn7sYh4DYMRb4",
    "KT1WPztCUj2wXnsXvgG6C3RnFe9pv2MY9Pv2"
);

generate_tests!(
    ContractTz1Hash,
    // sk: edsk2stcDLHYC5N5AFowvKTvdQ86zuqQCn7QsFBoFJUgYyHL4xSaYB
    "tz1XFq85mnnXhyhzpNEpxFvrkcuNtFBsSsVu",
    // sk: edsk2qWwwqoVa2XCiXeaihYVg6BfLcicR1TwC6vf63dCDYPh3qjR2g
    "tz1SUWNMC3hUdBRzzrbTbiuGPH1KFVifTQw7"
);

generate_tests!(
    ContractTz2Hash,
    // sk: spsk2YJesPtHH4swmdVdJpGXU1NLnpKiq2nicQFEtR5Eyb6i8Lju4z
    "tz2RcdU4n2PvJHUNYkS8FPuvcnFmBqEccxb4",
    // sk: spsk1nDmRj6hETy89DfJzHnmyFicx853ShpiLHLJAbg2Qu9gYdx35n
    "tz2PbzLDYrPAZS38BteBY7gqtnZfsTqHF2xu"
);

generate_tests!(
    ContractTz3Hash,
    // sk: p2sk37fQnziQeeWmZFuTDpf5Kn42BncafWsJ1wZ29yNUzNppV4eN8n
    "tz3S6P2LccJNrejt27KvJRb3BcuS5vGghkP8",
    // sk: p2sk3heCRmbfiArx8so4SBevK8t7mPGRqBN8eAYTzZJPWnu6LadRbM
    "tz3hqqamVC1G22LACFoMgcJeFKZgoGMFSfSn"
);

generate_tests!(
    ContractTz4Hash,
    // sk: BLsk2rrqeqLp7ujt4NSCG8W6xDMFDBm6QHoTabeSQ4HjPDewaX4F6k
    "tz4F2fxv7sKQx9wyoRMteoJwZEZnV9WFU2wL",
    // sk: BLsk2SdiXbRuYrWkfkSDbN1tCBGjGV7tTHxjVrokaiJsv17rDd8scd
    "tz4Uzyxg26DJyM4pc1V2pUvLpdsR5jdyzYsZ"
);

generate_tests!(
    PublicKeyEd25519,
    // sk: edsk4BYt5bFFa7rHeoZSxgbTUJFYJjSynS9tFgFBzjQpRTL4Gku2x2
    "edpkuWbLgKBsLVDE5dn678Y4ogZUjBQtCdh4HebhfJ9CaTR2o6WpFp",
    // sk: edsk4QUBg4kqJD5u5mvkwWe6qnmimoL3sAy8v2vDWEnWPbJeEcMMZp
    "edpktvpansLmKrvHCS1aWkFHS6gJdSd5haH1Z74MJFAAeNDSuSgHBH"
);

generate_tests!(
    PublicKeySecp256k1,
    // sk: spsk236JVFEqgJtsg4EbVeMjcppsumD1CdwNiBnP1YWeDhHubWRh1Z
    "sppk7ZMef32rXEYsxrAE7jjLVqrcbHPRA3ikSSrywzREro52PPKU42B",
    // sk: spsk36wmmgfs88ffW7ujgw9wm511zhZUkM4GnsYxhHMD9ZDy1AsKRa
    "sppk7bo7kcRyjajZaAqEfqdtCNx3wgizhJPFqaEuisncbDFMgn6v4iP"
);

generate_tests!(
    PublicKeyP256,
    // sk: p2sk3ne2WApaPmU9QiBK7BKgaSi18YB2J7Sv81YMykyFwWaYYyAoHz
    "p2pk674NNbXHqaDT5HcDuLJrR3csgjdLNrq4yMUXD1zMrx4uTcwEPzA",
    // sk: p2sk3EC67qL8iPQv6o2j14bgqKR6JaPBahXHpZFypVCGkbeMvv5H5G
    "p2pk66iANdYfcK2wd3rvvPACnHSHjN3PDFSjCkTmGY54rayykNs9Mdd"
);

generate_tests!(
    PublicKeyBls,
    // sk: BLsk1wk9JVZKhzsdHExkMR2kAPeZrdtpLaACTMtwxzvavje3n22msQ
    "BLpk1uJ3JLHwYWUrpk6rPmZ4jAjCzbQ3xjkW7HgumJMcT8WRiwTbYdzU9dF4mKK1hP4DaYvvCfZB",
    // sk: BLsk2CJPzPttewLrtdN5cTg6bXfMM2goVvMRKh2b9MPGZiZMcnKhwC
    "BLpk1zYRUvTi2pEQDhDNqpHCAR4Mocz3izibPgQCNJTBP8wA8cyGMMCadqbsU6Gj2WDqNQmKQhM8"
);

generate_tests!(
    UnknownSignature,
    // sk: spsk36wmmgfs88ffW7ujgw9wm511zhZUkM4GnsYxhHMD9ZDy1AsKRa
    // msg: "a"
    "sigaeDzBgzkdvQRhYV2YUFpzYGE19bRe62YdZnnodZMDSXD9P97jBro2v8W2o8a1wRxfJEWJpLEbv2W1TM8jKqBdFCCuKcDp",
    // sk: spsk2YJesPtHH4swmdVdJpGXU1NLnpKiq2nicQFEtR5Eyb6i8Lju4z
    // msg: "a"
    "sigmstyCA7dwXvY7UXo8mkUG47WPCybhngBYvHokpnReuZdKZwPoXK8aKp4sEZgAhxeJsnNyJ68Q4zdJyJuSw25JTjxgtVpA"
);

generate_tests!(
    Ed25519Signature,
    // sk: edsk2stcDLHYC5N5AFowvKTvdQ86zuqQCn7QsFBoFJUgYyHL4xSaYB
    // msg: "a"
    "edsigtmnB915emZPLVrk7oyuRtZXrYhc2ychu5e7kHHSd7LUmiCReV5C16HCNXxt6hnnWxSKQmHFvuNZzUm5K1BKWBM2vejS4T1",
    // sk: edsk2qWwwqoVa2XCiXeaihYVg6BfLcicR1TwC6vf63dCDYPh3qjR2g
    // msg: "a"
    "edsigtxuEw21vZaZYaamHwkdCb2XXVAG45hmaHE6QjEMGdNst3WJ1UYFK5R1HY7UtBJ7ZuU1ud52LPE7YPWPYSNbRDBArr39oAv"
);

generate_tests!(
    Secp256k1Signature,
    // sk: spsk2YJesPtHH4swmdVdJpGXU1NLnpKiq2nicQFEtR5Eyb6i8Lju4z
    // msg: "a"
    "spsig1N6Qd7PqszxEba1k2E4S3Zf75FLtcr6MQ4UXNJLpEQxkwXJ97hVzBMkxWs2CWF73Q729xBvuCD7ZsuxtG8VADob8g9HWxY",
    // sk: spsk1nDmRj6hETy89DfJzHnmyFicx853ShpiLHLJAbg2Qu9gYdx35n
    // msg: "a"
    "spsig1SRGvxX8B746NBT1gS5AtUWYRwZx7SFv8LWUFYDegmCiW7b4dt3ZXU1MmokYHDQEa2JiEUpnKEteaPx7fRMsyeQ5NizEC7"
);

generate_tests!(
    P256Signature,
    // sk: p2sk37fQnziQeeWmZFuTDpf5Kn42BncafWsJ1wZ29yNUzNppV4eN8n
    // msg: "a"
    "p2sigqusPxJeh3NDzdCPdfk6UecDjL99MowfGGtV9i1PibkZ8XX6geDRsegSkKtqwuc93uuHS42vbpGF7wxbzExuVDZNXjp9Xh",
    // sk: p2sk3heCRmbfiArx8so4SBevK8t7mPGRqBN8eAYTzZJPWnu6LadRbM
    // msg: "a"
    "p2sigak11zwxUbQi33u35Q7A3vpvrZz4kyaDmnkXC3ZpzoGzske9rdNhnAw6wKAvp9jLGPoLPstXVtUXxKi1V12mnp8EQaw4db"
);

generate_tests!(
    BlsSignature,
    // sk: BLsk2rrqeqLp7ujt4NSCG8W6xDMFDBm6QHoTabeSQ4HjPDewaX4F6k
    // msg: "a"
    "BLsigBRM1iKtzTgi3iThgPALG9GamDf3ecXFB1ZkEGNanPV9PbxKsuxtHgtMT1puDao9ns7UvJSds3zAma9ogpRqrh9GuL8dM2vEdRJtGGyLm3WA11f3DJqMqAcXs3mmuaNFsiPAH5WWGL",
    // sk: BLsk2SdiXbRuYrWkfkSDbN1tCBGjGV7tTHxjVrokaiJsv17rDd8scd
    // msg: "a"
    "BLsigBjCT4gTkW7e3yaWrum16tApsZNxw12NQxjpJTTQJyjVt4yJeNjnqxNrzKzrLQ2VqkcJr8djGUeDS3fmgoDn4kukS5zEMGPxGaduG5nrpMd5Vzkray9D9nzpcakWrscVf11KzkH4A6"
);

generate_tests!(
    Contract,
    // sk: edsk4BYt5bFFa7rHeoZSxgbTUJFYJjSynS9tFgFBzjQpRTL4Gku2x2
    "tz1Pta3ShQiC5uZRjfivFUmYQYd4Lai1vJrU",
    "KT1DvqNXfdigfmDeh2zyF9Q4mTFruyUr1rmv"
);

generate_tests!(
    PublicKey,
    // sk: spsk1ppL4ohtyZeighKZehzfGr2p6dL51kwQqEV2N1sNT7rx9cg5jG
    "sppk7cW8fKpjrM2713enEhvk6e5xhctLdhVCbE2YzMLCmNBHhphH4zE",
    // sk: BLsk27Wo9tVS8XbNDfCD7X9FnQBwZ4o2gQCZxXkJRmdFYoUTvuTu1S
    "BLpk1xwPVMQMstf4Nt41TGYeFNeck9RQJ7pHGqWpJQYgsQqGSPnhfoMMmU6fTzr1BD3mPLyzCoQB"
);

generate_tests!(
    PublicKeyHash,
    // sk: edsk3x51ok91omWQd5ZBzBuMV3pjCyFCjxh4fyoTRJ4QYuk59sjZch
    "tz1XnHxtAwqiJzwNicuU1A1haG2RW5sxJ2VJ",
    // sk: p2sk432A1xe56akKM7n8vk4g7tSyTTWodJgcwwH9mSmA6jjrApUB3F
    "tz3co4CsEMspEpBC7TstUupntak7zTixZyhH"
);

generate_tests!(
    Signature,
    // sk: edsk3x51ok91omWQd5ZBzBuMV3pjCyFCjxh4fyoTRJ4QYuk59sjZch
    // msg: "a"
    "edsigtzr9dzEN2jY59fMGEoFb1X99KPg5JaJzDsDTLraJgxHKQQEp6XMcXYEAAjV1oJno67WQCf1z3KxhYE8jWUmLLfz113HM19",
    // sk: p2sk432A1xe56akKM7n8vk4g7tSyTTWodJgcwwH9mSmA6jjrApUB3F
    // msg: "a"
    "p2siggqoZAEkNVcW6Xebq1rzPwn3qj4sEFuiePhbinCFpMNmpDXVKBSETJHJKS8yNtZYT27HGBkcUNLG3AqHFb3QMCfiQ957s1"
);
