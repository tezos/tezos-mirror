// SPDX-FileCopyrightText: 2022-2023 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! rlp encoding for Ethereum compatibility
//!
//! We need to sign and write Ethereum specific values such
//! as addresses and values.
use crate::address::EthereumAddress;
use crate::basic::{GasLimit, GasPrice, Wei, H256, U256};
use crate::signatures::EthereumEIP1559Transaction;
//TODO: make decoding typed so to be a bit more general
use primitive_types::U256 as PTU256;

fn rlp_decode_step(mut input: String) -> (String, String) {
    let mut rest = input.split_off(2);
    let len = usize::from_str_radix(&input, 16).unwrap();
    match len {
        0 => ("80".to_string(), rest),
        1..=127 => (input, rest),
        128 => ("00".to_string(), rest),
        129..=183 => {
            let f = rest.split_off(2 * (len - 128));
            (rest, f)
        }
        184..=191 => {
            let mut f1 = rest.split_off(2 * (len - 183));
            let len1 = usize::from_str_radix(&rest, 16).unwrap();
            let f2 = f1.split_off(2 * (len1));
            (f1, f2)
        }
        _ => ("".to_string(), rest),
    }
}
fn rlp_decode(mut vec: Vec<String>, input: String) -> Vec<String> {
    if input.is_empty() {
        vec
    } else {
        let (r1, r2) = rlp_decode_step(input);
        vec.push(r1);

        rlp_decode(vec, r2)
    }
}

/// decoding a EIP-1559c transaction
pub fn rlp_decode_eip_1559(mut input: String) -> EthereumEIP1559Transaction {
    let mut r = input.split_off(2);
    if input != "02" {
        panic!("not an EIP-1559 hex")
    } else {
        let mut rest = r.split_off(2);
        let len = usize::from_str_radix(&r, 16).unwrap();
        match len {
            247.. => {
                let f = rest.split_off(2 * (len - 247));

                let ee = rlp_decode(vec![], f);
                match &ee[..] {
                    [chain_id, nonce, max_priority_fee_per_gas, max_fee_per_gas, gas_limit, to, value, data, _access_list, v, r, s] =>
                    {
                        let chain_id = u8::from_str_radix(chain_id, 16).unwrap();

                        let nonce =
                            U256::from(PTU256::from_str_radix(nonce, 16).unwrap());
                        let max_priority_fee_per_gas = U256::from(
                            PTU256::from_str_radix(max_priority_fee_per_gas, 16).unwrap(),
                        );
                        let max_fee_per_gas = U256::from(
                            PTU256::from_str_radix(max_fee_per_gas, 16).unwrap(),
                        );

                        let gas_limit =
                            U256::from(PTU256::from_str_radix(gas_limit, 16).unwrap());

                        let to: [u8; 20] = if to == "00" {
                            [0; 20]
                        } else {
                            hex::decode(to).unwrap().try_into().unwrap()
                        };
                        let value =
                            U256::from(PTU256::from_str_radix(value, 16).unwrap());
                        let data: Vec<u8> = if data == "00" {
                            vec![]
                        } else {
                            hex::decode(data).unwrap()
                        };
                        let v = u8::from_str_radix(v, 16).unwrap();
                        let r: [u8; 32] = hex::decode(r).unwrap().try_into().unwrap();
                        let s: [u8; 32] = hex::decode(s).unwrap().try_into().unwrap();
                        EthereumEIP1559Transaction {
                            chain_id,
                            nonce,
                            max_priority_fee_per_gas: GasPrice {
                                value: max_priority_fee_per_gas,
                            },
                            max_fee_per_gas: GasPrice {
                                value: max_fee_per_gas,
                            },
                            gas_limit: GasLimit { value: gas_limit },
                            to: EthereumAddress::from(to),
                            value: Wei { value },
                            data,
                            v,
                            r: H256::from(r),
                            s: H256::from(s),
                        }
                    }
                    _ => {
                        panic!("wrong array")
                    }
                }
            }
            _ => panic!("not a correct hex"),
        }
    }
}

fn hex(a: u64) -> String {
    format!("{:x}", a)
}
fn rlp_encode_str(str: String) -> String {
    let str = if str.len() % 2 == 1 {
        format!("0{}", str)
    } else {
        str
    };
    let s: u64 = str.len().try_into().unwrap();
    let b = if str.is_empty() || str == *"00".to_string() {
        "80".to_string()
    } else if str == "[]" {
        "c0".to_string()
    } else if s <= 2 {
        str
    } else if hex(s).len() < 56 {
        if s % 2 == 1 {
            format!("{:x}0{}", s / 2 + 129, str)
        } else {
            format!("{:x}{}", s / 2 + 128, str)
        }
    } else {
        format!("{:x}{}{}", hex(s).len() + 183, hex(s), str)
    };
    b
}

/// small util
pub fn to_string(a: U256) -> String {
    let ptu: PTU256 = a.into();
    format!("{:x}", ptu)
}

/// encode a list
pub fn rlp_encode_list(a: &[String]) -> String {
    let r = a.iter().fold("".to_string(), |acc, x| {
        let b = rlp_encode_str(x.to_string());
        format!("{}{}", acc, b)
    });
    let size = format!("{:x}", (r.len() + 1) / 2);
    if r.len() / 2 < 56 {
        format!("{:x}{}", r.len() / 2 + 192, r)
    } else {
        format!("{:x}{}{}", (size.len()) / 2 + 247, size, r)
    }
}

/// rlp encoding for a classical transaction
pub fn rlp_encode_eip1559(e: EthereumEIP1559Transaction) -> String {
    let data = if e.data.is_empty() {
        "".to_string()
    } else {
        hex::encode(&e.data[..])
    };
    let st = rlp_encode_list(&[
        format!("{:x}", e.chain_id),
        to_string(e.nonce),
        to_string(e.max_priority_fee_per_gas.value),
        to_string(e.max_fee_per_gas.value),
        to_string(e.gas_limit.value),
        e.to.into(),
        to_string(e.value.value),
        data,
        "[]".to_string(),
        format!("{:x}", e.v),
        e.r.into(),
        e.s.into(),
    ]);
    format!("02{}", st)
}

/// rlp encoding of the message to be signed
pub fn rlp_encode_eip1559_for_sign(e: EthereumEIP1559Transaction) -> String {
    let data = if e.data.is_empty() {
        "".to_string()
    } else {
        hex::encode(&e.data[..])
    };
    let st = rlp_encode_list(&[
        format!("{:x}", e.chain_id),
        to_string(e.nonce),
        to_string(e.max_priority_fee_per_gas.value),
        to_string(e.max_fee_per_gas.value),
        to_string(e.gas_limit.value),
        e.to.into(),
        to_string(e.value.value),
        data,
        "[]".to_string(),
    ]);
    format!("02{}", st)
}
#[test]
fn test_rlp_decode_encode() {
    let n ="02f87205018459682f008459682f3682520894d0a2dbb5e6f757fd2066a7664f413caac504bc9588016345785d8a000080c001a02bade97eabf34bf84b5e02e342a69eb2ed755fc7c7a7fdbec1bb878b5da2a074a0600035bcf379bc8f0d30d7f85772b81a56acdc5594ce6cdca5169eca9570d169".to_string();
    let e = rlp_decode_eip_1559(n.clone());
    let str1 = rlp_encode_eip1559(e);
    assert_eq!(str1, n)
}
