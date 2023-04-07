// SPDX-FileCopyrightText: 2022-2023 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

//! Signature functions for Ethereum compatibility
//!
//! We need to sign and write Ethereum specific values such
//! as addresses and values.

use crate::address::EthereumAddress;
use crate::basic::{GasLimit, GasPrice, Wei, H256, U256};
use libsecp256k1::{
    curve::Scalar, recover, sign, verify, Message, PublicKey, RecoveryId, SecretKey,
    Signature,
};
use primitive_types::H256 as PTH256;
use rlp::{Decodable, DecoderError, Encodable, Rlp, RlpIterator, RlpStream};
use sha3::{Digest, Keccak256};

/// produces address from a secret key
pub fn string_to_sk_and_address(s: String) -> (SecretKey, EthereumAddress) {
    let data: [u8; 32] = hex::decode(s).unwrap().try_into().unwrap();
    let sk = SecretKey::parse(&data).unwrap();
    let pk = PublicKey::from_secret_key(&sk);
    let serialised = &pk.serialize()[1..];
    let kec = Keccak256::digest(serialised);
    let value: [u8; 20] = kec.as_slice()[12..].try_into().unwrap();
    (sk, EthereumAddress::from(value))
}

/// the type of a transaction
#[derive(Debug, PartialEq, Eq, Clone)]

pub enum EthereumTransactionType {
    /// transfer
    EthereumTransfer,
    ///create
    EthereumCreate,
    /// call
    EthereumCall,
}

/// Data common to all Ethereum transaction types
#[derive(Debug, PartialEq, Eq, Clone)]

pub struct EthereumTransactionCommon {
    /// the id of the chain
    /// see `<https://chainlist.org/>` for values
    pub chain_id: U256,
    /// A scalar value equal to the number of transactions sent by the sender
    pub nonce: U256,
    /// A scalar value equal to the number of
    /// Wei to be paid per unit of gas for all computation
    /// costs incurred as a result of the execution of this
    /// transaction
    pub gas_price: GasPrice,
    /// A scalar value equal to the maximum
    /// amount of gas that should be used in executing
    /// this transaction. This is paid up-front, before any
    /// computation is done and may not be increased
    /// later
    pub gas_limit: GasLimit,
    /// The 160-bit address of the message call’s recipient
    /// or, for a contract creation transaction
    pub to: EthereumAddress,
    /// A scalar value equal to the number of Wei to
    /// be transferred to the message call’s recipient or,
    /// in the case of contract creation, as an endowment
    /// to the newly created account
    pub value: Wei,
    /// the transaction data. In principle this can be large
    pub data: Vec<u8>,
    /// Signature x-axis part of point on elliptic curve. See yellow paper, appendix F
    pub r: H256,
    /// Signature, See yellow paper appendix F
    pub s: H256,
    /// the parity (recovery id) of the signature See yellow paper appendix F
    /// used to recompute chain_id is applicable
    /// See encoding details in <https://github.com/ethereum/EIPs/blob/master/EIPS/eip-155.md>
    pub v: U256,
}

impl EthereumTransactionCommon {
    /// Extracts the Keccak encoding of a message from an EthereumTransactionCommon
    pub fn message(&self) -> Message {
        let to_sign = EthereumTransactionCommon {
            v: self.chain_id,
            r: H256::zero(),
            s: H256::zero(),
            ..self.clone()
        };
        let bytes = to_sign.rlp_bytes();
        let hash: [u8; 32] = Keccak256::digest(bytes).into();
        Message::parse(&hash)
    }

    /// Extracts the signature from an EthereumTransactionCommon
    pub fn signature(&self) -> (Signature, RecoveryId) {
        // copy r to Scalar
        let r: PTH256 = self.r.into();
        let r1: [u8; 32] = r.into();
        let mut r = Scalar([0; 8]);
        let _ = r.set_b32(&r1);
        // copy s to Scalar
        let s: PTH256 = self.s.into();
        let s1: [u8; 32] = s.into();
        let mut s = Scalar([0; 8]);
        let _ = s.set_b32(&s1);
        // recompute parity from v and chain_id
        let ri = self.v - (self.chain_id * U256::from(2) + U256::from(35));
        if let Ok(ri) = RecoveryId::parse(ri.into()) {
            (Signature { r, s }, ri)
        } else {
            panic!(
                "could not recompute parity from v={}, chain_id={}",
                self.v, self.chain_id
            )
        }
    }
    /// Find the caller address from r and s of the common data
    /// for an Ethereum transaction, ie, what address is associated
    /// with the signature of the message.
    /// TODO <https://gitlab.com/tezos/tezos/-/milestones/115>
    pub fn caller(&self) -> EthereumAddress {
        let mes = self.message();
        let (sig, ri) = self.signature();
        let pk = recover(&mes, &sig, &ri).expect("Recover public key");
        let serialised = &pk.serialize()[1..];
        let kec = Keccak256::digest(serialised);
        let value: [u8; 20] = kec.as_slice()[12..]
            .try_into()
            .expect("Hash is 32 bytes, so [12..] should be 20 bytes long");
        EthereumAddress::from(value)
    }

    ///produce a signed EthereumTransactionCommon. If the initial one was signed
    ///  you should get the same thing.
    pub fn sign_transaction(&self, string_sk: String) -> Self {
        let sk = SecretKey::parse_slice(&hex::decode(string_sk).unwrap()).unwrap();
        let mes = self.message();
        let (sig, ri) = sign(&mes, &sk);
        let Signature { r, s } = sig;
        let (r, s) = (
            H256::from(PTH256::from(r.b32())),
            H256::from(PTH256::from(s.b32())),
        );

        let parity: u8 = ri.into();
        let v = if self.chain_id == U256::zero() {
            (27 + parity).into()
        } else {
            U256::from(parity) + U256::from(2) * self.chain_id + U256::from(35)
        };
        EthereumTransactionCommon {
            v,
            r,
            s,
            ..self.clone()
        }
    }

    /// verifies the signature
    pub fn verify_signature(self) -> bool {
        let mes = self.message();
        let (sig, ri) = self.signature();
        let pk = recover(&mes, &sig, &ri).unwrap();
        verify(&mes, &sig, &pk)
    }

    /// Unserialize an hex string as a RLP encoded legacy transaction.
    pub fn from_rlp(e: String) -> Result<EthereumTransactionCommon, DecoderError> {
        let tx =
            hex::decode(e).or(Err(DecoderError::Custom("Couldn't parse hex value")))?;
        let decoder = Rlp::new(&tx);
        EthereumTransactionCommon::decode(&decoder)
    }
}

impl From<String> for EthereumTransactionCommon {
    /// Decode a transaction in hex format. Unsafe, to be used only in tests : panics when fails
    fn from(e: String) -> Self {
        EthereumTransactionCommon::from_rlp(e).unwrap()
    }
}

/// Decoder helpers

fn next<'a, 'v>(decoder: &mut RlpIterator<'a, 'v>) -> Result<Rlp<'a>, DecoderError> {
    decoder.next().ok_or(DecoderError::RlpIncorrectListLen)
}

fn decode_field<T: Decodable>(
    decoder: &Rlp<'_>,
    field_name: &'static str,
) -> Result<T, DecoderError> {
    let custom_err = |_: DecoderError| (DecoderError::Custom(field_name));
    decoder.as_val().map_err(custom_err)
}

impl Decodable for EthereumTransactionCommon {
    fn decode(decoder: &Rlp<'_>) -> Result<EthereumTransactionCommon, DecoderError> {
        if decoder.is_list() && decoder.item_count() == Ok(9) {
            let mut it = decoder.iter();
            let nonce: U256 = decode_field(&next(&mut it)?, "nonce")?;
            let gas_price: GasPrice = decode_field(&next(&mut it)?, "gas_price")?;
            let gas_limit: GasLimit = decode_field(&next(&mut it)?, "gas_limit")?;
            let to: EthereumAddress = decode_field(&next(&mut it)?, "to")?;
            let value: Wei = decode_field(&next(&mut it)?, "value")?;
            let data: Vec<u8> = decode_field(&next(&mut it)?, "data")?;
            let v: U256 = decode_field(&next(&mut it)?, "v")?;
            let r: H256 = decode_field(&next(&mut it)?, "r")?;
            let s: H256 = decode_field(&next(&mut it)?, "s")?;
            // in a rlp encoded unsigned eip-155 transaction, v is used to store the chainid
            // in a rlp encoded signed eip-155 transaction, v is {0,1} + CHAIN_ID * 2 + 35
            let chain_id: U256 = if v > U256::from(35) {
                (v - U256::from(35)) / U256::from(2)
            } else {
                v
            };
            Ok(Self {
                chain_id,
                nonce,
                gas_price,
                gas_limit,
                to,
                value,
                data,
                v,
                r,
                s,
            })
        } else {
            Err(DecoderError::RlpExpectedToBeList)
        }
    }
}

impl Encodable for EthereumTransactionCommon {
    fn rlp_append(&self, stream: &mut RlpStream) {
        stream.begin_list(9);
        stream.append(&self.nonce);
        stream.append_internal(&self.gas_price);
        stream.append_internal(&self.gas_limit);
        stream.append_internal(&self.to);
        stream.append_internal(&self.value);
        if self.data.is_empty() {
            // no data == null, not empty vec
            stream.append_empty_data();
        } else {
            stream.append_iter(self.data.iter().cloned());
        }
        stream.append(&self.v);
        stream.append_internal(&self.r);
        stream.append_internal(&self.s);
        assert!(stream.is_finished());
    }
}

// cargo test ethereum::signatures::test --features testing
#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_caller_classic() {
        let (_sk, address_from_sk) = string_to_sk_and_address(
            "4646464646464646464646464646464646464646464646464646464646464646"
                .to_string(),
        );
        let encoded =
        "f86c098504a817c800825208943535353535353535353535353535353535353535880de0b6b3a76400008025a028ef61340bd939bc2195fe537567866003e1a15d3c71ff63e1590620aa636276a067cbe9d8997f761aecb703304b3800ccf555c9f3dc64214b297fb1966a3b6d83".to_string();
        let transaction = EthereumTransactionCommon::from_rlp(encoded).unwrap();
        let address = transaction.caller();
        let expected_address_string: [u8; 20] =
            hex::decode("9d8A62f656a8d1615C1294fd71e9CFb3E4855A4F")
                .unwrap()
                .try_into()
                .unwrap();
        let expected_address = EthereumAddress::from(expected_address_string);
        assert_eq!(expected_address, address);
        assert_eq!(expected_address, address_from_sk)
    }

    #[test]
    fn test_decoding_eip_155_example_unsigned() {
        // https://github.com/ethereum/EIPs/blob/master/EIPS/eip-155.md
        // Consider a transaction with nonce = 9, gasprice = 20 * 10**9, startgas = 21000, to = 0x3535353535353535353535353535353535353535, value = 10**18, data='' (empty)
        // signing data 0xec098504a817c800825208943535353535353535353535353535353535353535880de0b6b3a764000080018080
        // private key : 0x4646464646464646464646464646464646464646464646464646464646464646
        // corresponding address 0x9d8a62f656a8d1615c1294fd71e9cfb3e4855a4f
        // signed tx : 0xf86c098504a817c800825208943535353535353535353535353535353535353535880de0b6b3a76400008025a028ef61340bd939bc2195fe537567866003e1a15d3c71ff63e1590620aa636276a067cbe9d8997f761aecb703304b3800ccf555c9f3dc64214b297fb1966a3b6d83
        let nonce = U256::from(9);
        let gas_price = GasPrice::new(U256::from(20000000000u64));
        let gas_limit = GasLimit::new(U256::from(21000));
        let to =
            EthereumAddress::from("3535353535353535353535353535353535353535".to_string());
        assert_ne!(
            to,
            EthereumAddress::from_u64_be(0),
            "making sure the expected address is correct"
        );
        let value = Wei::new(U256::from(1000000000000000000u64));
        let data: Vec<u8> = vec![];
        let chain_id = U256::one();
        let v = chain_id;
        let expected_transaction = EthereumTransactionCommon {
            chain_id,
            nonce,
            gas_price,
            gas_limit,
            to,
            value,
            data,
            v,
            r: H256::zero(),
            s: H256::zero(),
        };

        // setup
        let signing_data = "ec098504a817c800825208943535353535353535353535353535353535353535880de0b6b3a764000080018080";
        let tx = hex::decode(signing_data).unwrap();
        let decoder = Rlp::new(&tx);

        let decoded = EthereumTransactionCommon::decode(&decoder);
        assert!(decoded.is_ok(), "testing the decoding went ok");

        let decoded_transaction = decoded.unwrap();
        assert_eq!(nonce, decoded_transaction.nonce, "testing nonce");
        assert_eq!(
            gas_price, decoded_transaction.gas_price,
            "testing gas price"
        );
        assert_eq!(
            gas_limit, decoded_transaction.gas_limit,
            "testing gas limit"
        );
        assert_eq!(to, decoded_transaction.to, "testing addresse");
        assert_eq!(value, decoded_transaction.value, "testing value");
        assert_eq!(Vec::<u8>::new(), decoded_transaction.data, "testing data");
        assert_eq!(U256::one(), decoded_transaction.v, "testing v");
        assert_eq!(H256::zero(), decoded_transaction.r, "testing r");
        assert_eq!(H256::zero(), decoded_transaction.s, "testing s");
        assert_eq!(expected_transaction, decoded_transaction)
    }

    #[test]
    fn test_encoding_eip155_unsigned() {
        // https://github.com/ethereum/EIPs/blob/master/EIPS/eip-155.md
        // Consider a transaction with nonce = 9, gasprice = 20 * 10**9, startgas = 21000, to = 0x3535353535353535353535353535353535353535, value = 10**18, data='' (empty)
        // signing data 0xec098504a817c800825208943535353535353535353535353535353535353535880de0b6b3a764000080018080
        // private key : 0x4646464646464646464646464646464646464646464646464646464646464646
        // corresponding address 0x9d8a62f656a8d1615c1294fd71e9cfb3e4855a4f
        // signed tx : 0xf86c098504a817c800825208943535353535353535353535353535353535353535880de0b6b3a76400008025a028ef61340bd939bc2195fe537567866003e1a15d3c71ff63e1590620aa636276a067cbe9d8997f761aecb703304b3800ccf555c9f3dc64214b297fb1966a3b6d83
        let nonce = U256::from(9);
        let gas_price = GasPrice::new(U256::from(20000000000u64));
        let gas_limit = GasLimit::new(U256::from(21000));
        let to =
            EthereumAddress::from("3535353535353535353535353535353535353535".to_string());
        let value = Wei::new(U256::from(1000000000000000000u64));
        let data: Vec<u8> = vec![];
        let chain_id = U256::one();
        let v = chain_id;
        let expected_transaction = EthereumTransactionCommon {
            chain_id,
            nonce,
            gas_price,
            gas_limit,
            to,
            value,
            data,
            v,
            r: H256::zero(),
            s: H256::zero(),
        };

        // setup
        let signing_data = "ec098504a817c800825208943535353535353535353535353535353535353535880de0b6b3a764000080018080";
        let tx = hex::decode(signing_data).unwrap();
        let encoded = expected_transaction.rlp_bytes();

        assert_eq!(signing_data, hex::encode(&encoded));
        assert_eq!(tx, &encoded[..]);
        assert_eq!(tx, encoded.to_vec());
    }

    #[test]
    fn test_encoding_create() {
        // transaction "without to field"
        // private key : 0x4646464646464646464646464646464646464646464646464646464646464646
        // corresponding address 0x9d8a62f656a8d1615c1294fd71e9cfb3e4855a4f
        // signed tx : 0xf8572e8506c50218ba8304312280843b9aca0082ffff26a0e9637495be4c216a833ef390b1f6798917c8a102ab165c5085cced7ca1f2eb3aa057854e7044a8fee7bccb6a2c32c4229dd9cbacad74350789e0ce75bf40b6f713
        let nonce = U256::from(46);
        let gas_price = GasPrice::new(U256::from(29075052730u64));
        let gas_limit = GasLimit::new(U256::from(274722));
        let to = EthereumAddress::from("".to_string());
        let value = Wei::new(U256::from(1000000000u64));
        let data: Vec<u8> = hex::decode("ffff").unwrap();
        let chain_id = U256::one();
        let v = U256::from(38);
        let r = H256::from_string_unsafe(
            "e9637495be4c216a833ef390b1f6798917c8a102ab165c5085cced7ca1f2eb3a",
        );
        let s = H256::from_string_unsafe(
            "57854e7044a8fee7bccb6a2c32c4229dd9cbacad74350789e0ce75bf40b6f713",
        );
        let expected_transaction = EthereumTransactionCommon {
            chain_id,
            nonce,
            gas_price,
            gas_limit,
            to,
            value,
            data,
            v,
            r,
            s,
        };

        // setup
        let signed_tx = "f8572e8506c50218ba8304312280843b9aca0082ffff26a0e9637495be4c216a833ef390b1f6798917c8a102ab165c5085cced7ca1f2eb3aa057854e7044a8fee7bccb6a2c32c4229dd9cbacad74350789e0ce75bf40b6f713";

        let encoded = expected_transaction.rlp_bytes();

        assert_eq!(signed_tx, hex::encode(&encoded));
    }
    #[test]
    fn test_encoding_eip155_signed() {
        // https://github.com/ethereum/EIPs/blob/master/EIPS/eip-155.md
        // Consider a transaction with nonce = 9, gasprice = 20 * 10**9, startgas = 21000, to = 0x3535353535353535353535353535353535353535, value = 10**18, data='' (empty)
        // signing data 0xec098504a817c800825208943535353535353535353535353535353535353535880de0b6b3a764000080018080
        // private key : 0x4646464646464646464646464646464646464646464646464646464646464646
        // corresponding address 0x9d8a62f656a8d1615c1294fd71e9cfb3e4855a4f
        // signed tx : 0xf86c098504a817c800825208943535353535353535353535353535353535353535880de0b6b3a76400008025a028ef61340bd939bc2195fe537567866003e1a15d3c71ff63e1590620aa636276a067cbe9d8997f761aecb703304b3800ccf555c9f3dc64214b297fb1966a3b6d83
        let nonce = U256::from(9);
        let gas_price = GasPrice::new(U256::from(20000000000u64));
        let gas_limit = GasLimit::new(U256::from(21000));
        let to =
            EthereumAddress::from("3535353535353535353535353535353535353535".to_string());
        let value = Wei::new(U256::from(1000000000000000000u64));
        let data: Vec<u8> = vec![];
        let chain_id = U256::one();
        let v = U256::from(37);
        let r = H256::from_string_unsafe(
            "28EF61340BD939BC2195FE537567866003E1A15D3C71FF63E1590620AA636276",
        );
        let s = H256::from_string_unsafe(
            "67CBE9D8997F761AECB703304B3800CCF555C9F3DC64214B297FB1966A3B6D83",
        );
        let expected_transaction = EthereumTransactionCommon {
            chain_id,
            nonce,
            gas_price,
            gas_limit,
            to,
            value,
            data,
            v,
            r,
            s,
        };

        // setup
        let signed_tx = "f86c098504a817c800825208943535353535353535353535353535353535353535880de0b6b3a76400008025a028ef61340bd939bc2195fe537567866003e1a15d3c71ff63e1590620aa636276a067cbe9d8997f761aecb703304b3800ccf555c9f3dc64214b297fb1966a3b6d83";

        let encoded = expected_transaction.rlp_bytes();

        assert_eq!(signed_tx, hex::encode(&encoded));
    }
    #[test]
    fn test_decoding_arbitrary_signed() {
        // arbitrary transaction with data
        //setup
        let nonce = U256::from(0);
        let gas_price = GasPrice::new(U256::from(40000000000u64));
        let gas_limit = GasLimit::new(U256::from(21000));
        let to =
            EthereumAddress::from("423163e58aabec5daa3dd1130b759d24bef0f6ea".to_string());
        let value = Wei::new(U256::from(5000000000000000u64));
        let data: Vec<u8> = hex::decode("deace8f5000000000000000000000000000000000000000000000000000000000000a4b100000000000000000000000041bca408a6b4029b42883aeb2c25087cab76cb58000000000000000000000000000000000000000000000000002386f26fc10000000000000000000000000000000000000000000000000000002357a49c7d75f600000000000000000000000000000000000000000000000000000000640b5549000000000000000000000000710bda329b2a6224e4b44833de30f38e7f81d5640000000000000000000000000000000000000000000000000000000000000000").unwrap();
        let v = U256::from(37);
        let r = H256::from_string_unsafe(
            "25dd6c973368c45ddfc17f5148e3f468a2e3f2c51920cbe9556a64942b0ab2eb",
        );
        let s = H256::from_string_unsafe(
            "31da07ce40c24b0a01f46fb2abc028b5ccd70dbd1cb330725323edc49a2a9558",
        );
        let expected_transaction = EthereumTransactionCommon {
            chain_id: U256::one(),
            nonce,
            gas_price,
            gas_limit,
            to,
            value,
            data,
            v,
            r,
            s,
        };

        // act
        let signed_data = "f90150808509502f900082520894423163e58aabec5daa3dd1130b759d24bef0f6ea8711c37937e08000b8e4deace8f5000000000000000000000000000000000000000000000000000000000000a4b100000000000000000000000041bca408a6b4029b42883aeb2c25087cab76cb58000000000000000000000000000000000000000000000000002386f26fc10000000000000000000000000000000000000000000000000000002357a49c7d75f600000000000000000000000000000000000000000000000000000000640b5549000000000000000000000000710bda329b2a6224e4b44833de30f38e7f81d564000000000000000000000000000000000000000000000000000000000000000025a025dd6c973368c45ddfc17f5148e3f468a2e3f2c51920cbe9556a64942b0ab2eba031da07ce40c24b0a01f46fb2abc028b5ccd70dbd1cb330725323edc49a2a9558";
        let tx = hex::decode(signed_data).unwrap();
        let decoder = Rlp::new(&tx);

        let decoded = EthereumTransactionCommon::decode(&decoder);

        // assert
        assert_eq!(Ok(expected_transaction), decoded)
    }
    #[test]
    fn test_decoding_eip_155_example_signed() {
        // https://github.com/ethereum/EIPs/blob/master/EIPS/eip-155.md
        // Consider a transaction with nonce = 9, gasprice = 20 * 10**9, startgas = 21000, to = 0x3535353535353535353535353535353535353535, value = 10**18, data='' (empty)
        // signing data 0xec098504a817c800825208943535353535353535353535353535353535353535880de0b6b3a764000080018080
        // private key : 0x4646464646464646464646464646464646464646464646464646464646464646
        // corresponding address 0x9d8a62f656a8d1615c1294fd71e9cfb3e4855a4f
        // signed tx : 0xf86c098504a817c800825208943535353535353535353535353535353535353535880de0b6b3a76400008025a028ef61340bd939bc2195fe537567866003e1a15d3c71ff63e1590620aa636276a067cbe9d8997f761aecb703304b3800ccf555c9f3dc64214b297fb1966a3b6d83
        let nonce = U256::from(9);
        let gas_price = GasPrice::new(U256::from(20000000000u64));
        let gas_limit = GasLimit::new(U256::from(21000));
        let to =
            EthereumAddress::from("3535353535353535353535353535353535353535".to_string());
        assert_ne!(
            to,
            EthereumAddress::from_u64_be(0),
            "making sure the expected address is correct"
        );
        let value = Wei::new(U256::from(1000000000000000000u64));
        let data: Vec<u8> = vec![];
        let v = U256::from(37);
        let r = H256::from_string_unsafe(
            "28EF61340BD939BC2195FE537567866003E1A15D3C71FF63E1590620AA636276",
        );
        let s = H256::from_string_unsafe(
            "67CBE9D8997F761AECB703304B3800CCF555C9F3DC64214B297FB1966A3B6D83",
        );
        let expected_transaction = EthereumTransactionCommon {
            chain_id: U256::one(),
            nonce,
            gas_price,
            gas_limit,
            to,
            value,
            data,
            v,
            r,
            s,
        };

        // setup
        let signed_data = "f86c098504a817c800825208943535353535353535353535353535353535353535880de0b6b3a76400008025a028ef61340bd939bc2195fe537567866003e1a15d3c71ff63e1590620aa636276a067cbe9d8997f761aecb703304b3800ccf555c9f3dc64214b297fb1966a3b6d83";
        let tx = hex::decode(signed_data).unwrap();
        let decoder = Rlp::new(&tx);

        let decoded = EthereumTransactionCommon::decode(&decoder);
        assert!(decoded.is_ok(), "testing the decoding went ok");

        let decoded_transaction = decoded.unwrap();
        assert_eq!(nonce, decoded_transaction.nonce, "testing nonce");
        assert_eq!(
            gas_price, decoded_transaction.gas_price,
            "testing gas price"
        );
        assert_eq!(
            gas_limit, decoded_transaction.gas_limit,
            "testing gas limit"
        );
        assert_eq!(to, decoded_transaction.to, "testing addresse");
        assert_eq!(value, decoded_transaction.value, "testing value");
        assert_eq!(Vec::<u8>::new(), decoded_transaction.data, "testing data");
        assert_eq!(U256::from(37), decoded_transaction.v, "testing v");
        assert_eq!(r, decoded_transaction.r, "testing r");
        assert_eq!(s, decoded_transaction.s, "testing s");

        assert_eq!(expected_transaction, decoded_transaction)
    }

    #[test]
    fn test_decoding_uniswap_call_signed() {
        // inspired by 0xf598016f51e0544187088ddd50fd37818fd268a0363a17281576425f3ee334cb
        // private key dcdff53b4f013dbcdc717f89fe3bf4d8b10512aae282b48e01d7530470382701
        // corresponding address 0xaf1276cbb260bb13deddb4209ae99ae6e497f446
        // to 0xef1c6e67703c7bd7107eed8303fbe6ec2554bf6b
        // data: 0x3593564c000000000000000000000000000000000000000000000000000000000000006000000000000000000000000000000000000000000000000000000000000000a00000000000000000000000000000000000000000000000000000000064023c1700000000000000000000000000000000000000000000000000000000000000030b090c00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000003000000000000000000000000000000000000000000000000000000000000006000000000000000000000000000000000000000000000000000000000000000c000000000000000000000000000000000000000000000000000000000000001e0000000000000000000000000000000000000000000000000000000000000004000000000000000000000000000000000000000000000000000000000000000020000000000000000000000000000000000000000000000000a8db2d41b89b009000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000002ab0c205a56c1e000000000000000000000000000000000000000000000000000000a8db2d41b89b00900000000000000000000000000000000000000000000000000000000000000a000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000002000000000000000000000000c02aaa39b223fe8d0a0e5c4f27ead9083c756cc20000000000000000000000009eb6299e4bb6669e42cb295a254c8492f67ae2c6000000000000000000000000000000000000000000000000000000000000004000000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000000
        // tx: 0xf903732e8506c50218ba8304312294ef1c6e67703c7bd7107eed8303fbe6ec2554bf6b880a8db2d41b89b009b903043593564c000000000000000000000000000000000000000000000000000000000000006000000000000000000000000000000000000000000000000000000000000000a00000000000000000000000000000000000000000000000000000000064023c1700000000000000000000000000000000000000000000000000000000000000030b090c00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000003000000000000000000000000000000000000000000000000000000000000006000000000000000000000000000000000000000000000000000000000000000c000000000000000000000000000000000000000000000000000000000000001e0000000000000000000000000000000000000000000000000000000000000004000000000000000000000000000000000000000000000000000000000000000020000000000000000000000000000000000000000000000000a8db2d41b89b009000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000002ab0c205a56c1e000000000000000000000000000000000000000000000000000000a8db2d41b89b00900000000000000000000000000000000000000000000000000000000000000a000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000002000000000000000000000000c02aaa39b223fe8d0a0e5c4f27ead9083c756cc20000000000000000000000009eb6299e4bb6669e42cb295a254c8492f67ae2c600000000000000000000000000000000000000000000000000000000000000400000000000000000000000000000000000000000000000000000000000000001000000000000000000000000000000000000000000000000000000000000000025a0c78be9ab81c622c08f7098eefc250935365fb794dfd94aec0fea16c32adec45aa05721614264d8490c6866f110c1594151bbcc4fac43758adae644db6bc3314d06

        //setup
        let nonce = U256::from(46);
        let gas_price = GasPrice::new(U256::from(29075052730u64));
        let gas_limit = GasLimit::new(U256::from(274722));
        let to =
            EthereumAddress::from("ef1c6e67703c7bd7107eed8303fbe6ec2554bf6b".to_string());
        let value = Wei::new(U256::from(760460536160301065u64)); // /!\ > 2^53 -1
        let data: Vec<u8> = hex::decode("3593564c000000000000000000000000000000000000000000000000000000000000006000000000000000000000000000000000000000000000000000000000000000a00000000000000000000000000000000000000000000000000000000064023c1700000000000000000000000000000000000000000000000000000000000000030b090c00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000003000000000000000000000000000000000000000000000000000000000000006000000000000000000000000000000000000000000000000000000000000000c000000000000000000000000000000000000000000000000000000000000001e0000000000000000000000000000000000000000000000000000000000000004000000000000000000000000000000000000000000000000000000000000000020000000000000000000000000000000000000000000000000a8db2d41b89b009000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000002ab0c205a56c1e000000000000000000000000000000000000000000000000000000a8db2d41b89b00900000000000000000000000000000000000000000000000000000000000000a000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000002000000000000000000000000c02aaa39b223fe8d0a0e5c4f27ead9083c756cc20000000000000000000000009eb6299e4bb6669e42cb295a254c8492f67ae2c6000000000000000000000000000000000000000000000000000000000000004000000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000000").unwrap();
        let v = U256::from(37);
        let r = H256::from_string_unsafe(
            "c78be9ab81c622c08f7098eefc250935365fb794dfd94aec0fea16c32adec45a",
        );
        let s = H256::from_string_unsafe(
            "5721614264d8490c6866f110c1594151bbcc4fac43758adae644db6bc3314d06",
        );
        let expected_transaction = EthereumTransactionCommon {
            chain_id: U256::one(),
            nonce,
            gas_price,
            gas_limit,
            to,
            value,
            data,
            v,
            r,
            s,
        };

        // act
        let signed_data = "f903732e8506c50218ba8304312294ef1c6e67703c7bd7107eed8303fbe6ec2554bf6b880a8db2d41b89b009b903043593564c000000000000000000000000000000000000000000000000000000000000006000000000000000000000000000000000000000000000000000000000000000a00000000000000000000000000000000000000000000000000000000064023c1700000000000000000000000000000000000000000000000000000000000000030b090c00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000003000000000000000000000000000000000000000000000000000000000000006000000000000000000000000000000000000000000000000000000000000000c000000000000000000000000000000000000000000000000000000000000001e0000000000000000000000000000000000000000000000000000000000000004000000000000000000000000000000000000000000000000000000000000000020000000000000000000000000000000000000000000000000a8db2d41b89b009000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000002ab0c205a56c1e000000000000000000000000000000000000000000000000000000a8db2d41b89b00900000000000000000000000000000000000000000000000000000000000000a000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000002000000000000000000000000c02aaa39b223fe8d0a0e5c4f27ead9083c756cc20000000000000000000000009eb6299e4bb6669e42cb295a254c8492f67ae2c600000000000000000000000000000000000000000000000000000000000000400000000000000000000000000000000000000000000000000000000000000001000000000000000000000000000000000000000000000000000000000000000025a0c78be9ab81c622c08f7098eefc250935365fb794dfd94aec0fea16c32adec45aa05721614264d8490c6866f110c1594151bbcc4fac43758adae644db6bc3314d06";
        let tx = hex::decode(signed_data).unwrap();
        let decoder = Rlp::new(&tx);

        let decoded = EthereumTransactionCommon::decode(&decoder);

        // assert
        assert_eq!(Ok(expected_transaction), decoded);
    }

    #[test]
    fn test_encoding_uniswap_call_signed() {
        // inspired by 0xf598016f51e0544187088ddd50fd37818fd268a0363a17281576425f3ee334cb
        // private key dcdff53b4f013dbcdc717f89fe3bf4d8b10512aae282b48e01d7530470382701
        // corresponding address 0xaf1276cbb260bb13deddb4209ae99ae6e497f446
        // to 0xef1c6e67703c7bd7107eed8303fbe6ec2554bf6b
        // data: 0x3593564c000000000000000000000000000000000000000000000000000000000000006000000000000000000000000000000000000000000000000000000000000000a00000000000000000000000000000000000000000000000000000000064023c1700000000000000000000000000000000000000000000000000000000000000030b090c00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000003000000000000000000000000000000000000000000000000000000000000006000000000000000000000000000000000000000000000000000000000000000c000000000000000000000000000000000000000000000000000000000000001e0000000000000000000000000000000000000000000000000000000000000004000000000000000000000000000000000000000000000000000000000000000020000000000000000000000000000000000000000000000000a8db2d41b89b009000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000002ab0c205a56c1e000000000000000000000000000000000000000000000000000000a8db2d41b89b00900000000000000000000000000000000000000000000000000000000000000a000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000002000000000000000000000000c02aaa39b223fe8d0a0e5c4f27ead9083c756cc20000000000000000000000009eb6299e4bb6669e42cb295a254c8492f67ae2c6000000000000000000000000000000000000000000000000000000000000004000000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000000
        // tx: 0xf903732e8506c50218ba8304312294ef1c6e67703c7bd7107eed8303fbe6ec2554bf6b880a8db2d41b89b009b903043593564c000000000000000000000000000000000000000000000000000000000000006000000000000000000000000000000000000000000000000000000000000000a00000000000000000000000000000000000000000000000000000000064023c1700000000000000000000000000000000000000000000000000000000000000030b090c00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000003000000000000000000000000000000000000000000000000000000000000006000000000000000000000000000000000000000000000000000000000000000c000000000000000000000000000000000000000000000000000000000000001e0000000000000000000000000000000000000000000000000000000000000004000000000000000000000000000000000000000000000000000000000000000020000000000000000000000000000000000000000000000000a8db2d41b89b009000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000002ab0c205a56c1e000000000000000000000000000000000000000000000000000000a8db2d41b89b00900000000000000000000000000000000000000000000000000000000000000a000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000002000000000000000000000000c02aaa39b223fe8d0a0e5c4f27ead9083c756cc20000000000000000000000009eb6299e4bb6669e42cb295a254c8492f67ae2c600000000000000000000000000000000000000000000000000000000000000400000000000000000000000000000000000000000000000000000000000000001000000000000000000000000000000000000000000000000000000000000000025a0c78be9ab81c622c08f7098eefc250935365fb794dfd94aec0fea16c32adec45aa05721614264d8490c6866f110c1594151bbcc4fac43758adae644db6bc3314d06

        //setup
        let nonce = U256::from(46);
        let gas_price = GasPrice::new(U256::from(29075052730u64));
        let gas_limit = GasLimit::new(U256::from(274722));
        let to =
            EthereumAddress::from("ef1c6e67703c7bd7107eed8303fbe6ec2554bf6b".to_string());
        let value = Wei::new(U256::from(760460536160301065u64)); // /!\ > 2^53 -1
        let data: Vec<u8> = hex::decode("3593564c000000000000000000000000000000000000000000000000000000000000006000000000000000000000000000000000000000000000000000000000000000a00000000000000000000000000000000000000000000000000000000064023c1700000000000000000000000000000000000000000000000000000000000000030b090c00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000003000000000000000000000000000000000000000000000000000000000000006000000000000000000000000000000000000000000000000000000000000000c000000000000000000000000000000000000000000000000000000000000001e0000000000000000000000000000000000000000000000000000000000000004000000000000000000000000000000000000000000000000000000000000000020000000000000000000000000000000000000000000000000a8db2d41b89b009000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000002ab0c205a56c1e000000000000000000000000000000000000000000000000000000a8db2d41b89b00900000000000000000000000000000000000000000000000000000000000000a000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000002000000000000000000000000c02aaa39b223fe8d0a0e5c4f27ead9083c756cc20000000000000000000000009eb6299e4bb6669e42cb295a254c8492f67ae2c6000000000000000000000000000000000000000000000000000000000000004000000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000000").unwrap();
        let v = U256::from(37);
        let r = H256::from_string_unsafe(
            "c78be9ab81c622c08f7098eefc250935365fb794dfd94aec0fea16c32adec45a",
        );
        let s = H256::from_string_unsafe(
            "5721614264d8490c6866f110c1594151bbcc4fac43758adae644db6bc3314d06",
        );
        let expected_transaction = EthereumTransactionCommon {
            chain_id: U256::one(),
            nonce,
            gas_price,
            gas_limit,
            to,
            value,
            data,
            v,
            r,
            s,
        };

        // act
        let signed_data = "f903732e8506c50218ba8304312294ef1c6e67703c7bd7107eed8303fbe6ec2554bf6b880a8db2d41b89b009b903043593564c000000000000000000000000000000000000000000000000000000000000006000000000000000000000000000000000000000000000000000000000000000a00000000000000000000000000000000000000000000000000000000064023c1700000000000000000000000000000000000000000000000000000000000000030b090c00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000003000000000000000000000000000000000000000000000000000000000000006000000000000000000000000000000000000000000000000000000000000000c000000000000000000000000000000000000000000000000000000000000001e0000000000000000000000000000000000000000000000000000000000000004000000000000000000000000000000000000000000000000000000000000000020000000000000000000000000000000000000000000000000a8db2d41b89b009000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000002ab0c205a56c1e000000000000000000000000000000000000000000000000000000a8db2d41b89b00900000000000000000000000000000000000000000000000000000000000000a000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000002000000000000000000000000c02aaa39b223fe8d0a0e5c4f27ead9083c756cc20000000000000000000000009eb6299e4bb6669e42cb295a254c8492f67ae2c600000000000000000000000000000000000000000000000000000000000000400000000000000000000000000000000000000000000000000000000000000001000000000000000000000000000000000000000000000000000000000000000025a0c78be9ab81c622c08f7098eefc250935365fb794dfd94aec0fea16c32adec45aa05721614264d8490c6866f110c1594151bbcc4fac43758adae644db6bc3314d06";

        let encoded = expected_transaction.rlp_bytes();

        assert_eq!(signed_data, hex::encode(&encoded));
    }

    #[test]
    fn test_decoding_ethereum_js() {
        // private key 0xcb9db6b5878db2fa20586e23b7f7b51c22a7c6ed0530daafc2615b116f170cd3
        // from 0xd9e5c94a12f78a96640757ac97ba0c257e8aa262
        // "nonce": 1,
        // "gasPrice": 30000000000,
        // "gasLimit": "0x100000",
        // "to": "0x4e1b2c985d729ae6e05ef7974013eeb48f394449",
        // "value": 1000000000,
        // "data": "",
        // "chainId": 1,
        // v: 38
        // r: bb03310570362eef497a09dd6e4ef42f56374965cfb09cc4e055a22a2eeac7ad
        // s: 6053c1bd83abb30c109801844709202208736d598649afe2a53f024b61b3383f
        // tx: 0xf869018506fc23ac0083100000944e1b2c985d729ae6e05ef7974013eeb48f394449843b9aca008026a0bb03310570362eef497a09dd6e4ef42f56374965cfb09cc4e055a22a2eeac7ada06053c1bd83abb30c109801844709202208736d598649afe2a53f024b61b3383f

        let expected_transaction = EthereumTransactionCommon {
            chain_id: U256::one(),
            nonce: U256::from(1),
            gas_price: GasPrice::new(U256::from(30000000000u64)),
            gas_limit: GasLimit::new(U256::from(1048576)),
            to: EthereumAddress::from(
                "4e1b2c985d729ae6e05ef7974013eeb48f394449".to_string(),
            ),
            value: Wei::new(U256::from(1000000000u64)),
            data: vec![],
            v: U256::from(38),
            r: H256::from_string_unsafe(
                "bb03310570362eef497a09dd6e4ef42f56374965cfb09cc4e055a22a2eeac7ad",
            ),
            s: H256::from_string_unsafe(
                "6053c1bd83abb30c109801844709202208736d598649afe2a53f024b61b3383f",
            ),
        };

        // act
        let signed_data = "f869018506fc23ac0083100000944e1b2c985d729ae6e05ef7974013eeb48f394449843b9aca008026a0bb03310570362eef497a09dd6e4ef42f56374965cfb09cc4e055a22a2eeac7ada06053c1bd83abb30c109801844709202208736d598649afe2a53f024b61b3383f";
        let tx = hex::decode(signed_data).unwrap();
        let decoder = Rlp::new(&tx);

        let decoded = EthereumTransactionCommon::decode(&decoder);

        assert_eq!(Ok(expected_transaction), decoded);
    }

    #[test]
    fn test_caller_ethereum_js() {
        // private key 0xcb9db6b5878db2fa20586e23b7f7b51c22a7c6ed0530daafc2615b116f170cd3
        // from 0xd9e5c94a12f78a96640757ac97ba0c257e8aa262
        // "nonce": 1,
        // "gasPrice": 30000000000,
        // "gasLimit": "0x100000",
        // "to": "0x4e1b2c985d729ae6e05ef7974013eeb48f394449",
        // "value": 1000000000,
        // "data": "",
        // "chainId": 1,
        // r: bb03310570362eef497a09dd6e4ef42f56374965cfb09cc4e055a22a2eeac7ad
        // s: 6053c1bd83abb30c109801844709202208736d598649afe2a53f024b61b3383f
        // tx: 0xf869018506fc23ac0083100000944e1b2c985d729ae6e05ef7974013eeb48f394449843b9aca008026a0bb03310570362eef497a09dd6e4ef42f56374965cfb09cc4e055a22a2eeac7ada06053c1bd83abb30c109801844709202208736d598649afe2a53f024b61b3383f

        let transaction = EthereumTransactionCommon {
            chain_id: U256::one(),
            nonce: U256::from(1),
            gas_price: GasPrice::new(U256::from(30000000000u64)),
            gas_limit: GasLimit::new(U256::from(1048576)),
            to: EthereumAddress::from(
                "4e1b2c985d729ae6e05ef7974013eeb48f394449".to_string(),
            ),
            value: Wei::new(U256::from(1000000000u64)),
            data: vec![],
            v: U256::from(38),
            r: H256::from_string_unsafe(
                "bb03310570362eef497a09dd6e4ef42f56374965cfb09cc4e055a22a2eeac7ad",
            ),
            s: H256::from_string_unsafe(
                "6053c1bd83abb30c109801844709202208736d598649afe2a53f024b61b3383f",
            ),
        };

        // assert
        assert_eq!(
            EthereumAddress::from("d9e5c94a12f78a96640757ac97ba0c257e8aa262".to_string()),
            transaction.caller(),
            "test field from"
        )
    }
    #[test]
    fn test_signature_ethereum_js() {
        // private key 0xcb9db6b5878db2fa20586e23b7f7b51c22a7c6ed0530daafc2615b116f170cd3
        // from 0xd9e5c94a12f78a96640757ac97ba0c257e8aa262
        // "nonce": 1,
        // "gasPrice": 30000000000,
        // "gasLimit": "0x100000",
        // "to": "0x4e1b2c985d729ae6e05ef7974013eeb48f394449",
        // "value": 1000000000,
        // "data": "",
        // "chainId": 1,
        // r: bb03310570362eef497a09dd6e4ef42f56374965cfb09cc4e055a22a2eeac7ad
        // s: 6053c1bd83abb30c109801844709202208736d598649afe2a53f024b61b3383f
        // tx: 0xf869018506fc23ac0083100000944e1b2c985d729ae6e05ef7974013eeb48f394449843b9aca008026a0bb03310570362eef497a09dd6e4ef42f56374965cfb09cc4e055a22a2eeac7ada06053c1bd83abb30c109801844709202208736d598649afe2a53f024b61b3383f

        let transaction = EthereumTransactionCommon {
            chain_id: U256::one(),
            nonce: U256::from(1),
            gas_price: GasPrice::new(U256::from(30000000000u64)),
            gas_limit: GasLimit::new(U256::from(1048576)),
            to: EthereumAddress::from(
                "4e1b2c985d729ae6e05ef7974013eeb48f394449".to_string(),
            ),
            value: Wei::new(U256::from(1000000000u64)),
            data: vec![],
            v: U256::one(),
            r: H256::zero(),
            s: H256::zero(),
        };

        let signed = transaction.sign_transaction(
            "cb9db6b5878db2fa20586e23b7f7b51c22a7c6ed0530daafc2615b116f170cd3"
                .to_string(),
        );

        let v = U256::from(38);
        let r = H256::from_string_unsafe(
            "bb03310570362eef497a09dd6e4ef42f56374965cfb09cc4e055a22a2eeac7ad",
        );
        let s = H256::from_string_unsafe(
            "6053c1bd83abb30c109801844709202208736d598649afe2a53f024b61b3383f",
        );

        assert_eq!(v, signed.v, "checking v");
        assert_eq!(r, signed.r, "checking r");
        assert_eq!(s, signed.s, "checking s");
    }

    #[test]
    fn test_caller_classic_with_chain_id() {
        let sk = "9bfc9fbe6296c8fef8eb8d6ce2ed5f772a011898c6cabe32d35e7c3e419efb1b"
            .to_string();
        let (_sk, address) = string_to_sk_and_address(sk.clone());
        // Check that the derived address is the expected one.
        let expected_address_string: [u8; 20] =
            hex::decode(b"6471A723296395CF1Dcc568941AFFd7A390f94CE")
                .unwrap()
                .try_into()
                .unwrap();
        let expected_address = EthereumAddress::from(expected_address_string);
        assert_eq!(expected_address, address);

        // Check that the derived sender address is the expected one.
        let encoded = "f86d80843b9aca00825208940b52d4d3be5d18a7ab5e4476a2f5382bbf2b38d888016345785d8a000080820a95a0d9ef1298c18c88604e3f08e14907a17dfa81b1dc6b37948abe189d8db5cb8a43a06fc7040a71d71d3cb74bd05ead7046b10668ad255da60391c017eea31555f156".to_string();
        let transaction = EthereumTransactionCommon::from_rlp(encoded).unwrap();
        let address = transaction.caller();
        assert_eq!(expected_address, address);

        // Check that signing the signed transaction returns the same transaction.
        let signed_transaction = transaction.sign_transaction(sk);
        assert_eq!(transaction, signed_transaction)
    }

    #[test]
    fn test_call_eip155_example() {
        // example directly lifted from eip155 description
        // signing data 0xec098504a817c800825208943535353535353535353535353535353535353535880de0b6b3a764000080018080
        // private key : 0x4646464646464646464646464646464646464646464646464646464646464646
        // corresponding address 0x9d8a62f656a8d1615c1294fd71e9cfb3e4855a4f
        // signed tx : 0xf86c098504a817c800825208943535353535353535353535353535353535353535880de0b6b3a76400008025a028ef61340bd939bc2195fe537567866003e1a15d3c71ff63e1590620aa636276a067cbe9d8997f761aecb703304b3800ccf555c9f3dc64214b297fb1966a3b6d83        let nonce = U256::from(9);

        let transaction = EthereumTransactionCommon {
            chain_id: U256::one(),
            nonce: U256::from(9),
            gas_price: GasPrice::new(U256::from(20000000000u64)),
            gas_limit: GasLimit::new(U256::from(21000)),
            to: EthereumAddress::from(
                "3535353535353535353535353535353535353535".to_string(),
            ),
            value: Wei::new(U256::from(1000000000000000000u64)),
            data: vec![],
            v: U256::from(37),
            r: H256::from_string_unsafe(
                "28EF61340BD939BC2195FE537567866003E1A15D3C71FF63E1590620AA636276",
            ),
            s: H256::from_string_unsafe(
                "67CBE9D8997F761AECB703304B3800CCF555C9F3DC64214B297FB1966A3B6D83",
            ),
        };

        assert_eq!(
            EthereumAddress::from("9d8a62f656a8d1615c1294fd71e9cfb3e4855a4f".to_string()),
            transaction.caller()
        )
    }

    #[test]
    fn test_caller_uniswap_inspired() {
        // inspired by 0xf598016f51e0544187088ddd50fd37818fd268a0363a17281576425f3ee334cb

        let data: Vec<u8> = hex::decode("3593564c000000000000000000000000000000000000000000000000000000000000006000000000000000000000000000000000000000000000000000000000000000a00000000000000000000000000000000000000000000000000000000064023c1700000000000000000000000000000000000000000000000000000000000000030b090c00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000003000000000000000000000000000000000000000000000000000000000000006000000000000000000000000000000000000000000000000000000000000000c000000000000000000000000000000000000000000000000000000000000001e0000000000000000000000000000000000000000000000000000000000000004000000000000000000000000000000000000000000000000000000000000000020000000000000000000000000000000000000000000000000a8db2d41b89b009000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000002ab0c205a56c1e000000000000000000000000000000000000000000000000000000a8db2d41b89b00900000000000000000000000000000000000000000000000000000000000000a000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000002000000000000000000000000c02aaa39b223fe8d0a0e5c4f27ead9083c756cc20000000000000000000000009eb6299e4bb6669e42cb295a254c8492f67ae2c6000000000000000000000000000000000000000000000000000000000000004000000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000000").unwrap();

        let transaction = EthereumTransactionCommon {
            chain_id: U256::one(),
            nonce: U256::from(46),
            gas_price: GasPrice::new(U256::from(29075052730u64)),
            gas_limit: GasLimit::new(U256::from(274722)),
            to: EthereumAddress::from(
                "ef1c6e67703c7bd7107eed8303fbe6ec2554bf6b".to_string(),
            ),
            value: Wei::new(U256::from(760460536160301065u64)),
            data,
            v: U256::from(37),
            r: H256::from_string_unsafe(
                "c78be9ab81c622c08f7098eefc250935365fb794dfd94aec0fea16c32adec45a",
            ),
            s: H256::from_string_unsafe(
                "5721614264d8490c6866f110c1594151bbcc4fac43758adae644db6bc3314d06",
            ),
        };
        assert_eq!(
            EthereumAddress::from("af1276cbb260bb13deddb4209ae99ae6e497f446".to_string()),
            transaction.caller(),
            "checking caller"
        )
    }

    #[test]
    fn test_message_uniswap_inspired() {
        // inspired by 0xf598016f51e0544187088ddd50fd37818fd268a0363a17281576425f3ee334cb

        let data: Vec<u8> = hex::decode("3593564c000000000000000000000000000000000000000000000000000000000000006000000000000000000000000000000000000000000000000000000000000000a00000000000000000000000000000000000000000000000000000000064023c1700000000000000000000000000000000000000000000000000000000000000030b090c00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000003000000000000000000000000000000000000000000000000000000000000006000000000000000000000000000000000000000000000000000000000000000c000000000000000000000000000000000000000000000000000000000000001e0000000000000000000000000000000000000000000000000000000000000004000000000000000000000000000000000000000000000000000000000000000020000000000000000000000000000000000000000000000000a8db2d41b89b009000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000002ab0c205a56c1e000000000000000000000000000000000000000000000000000000a8db2d41b89b00900000000000000000000000000000000000000000000000000000000000000a000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000002000000000000000000000000c02aaa39b223fe8d0a0e5c4f27ead9083c756cc20000000000000000000000009eb6299e4bb6669e42cb295a254c8492f67ae2c6000000000000000000000000000000000000000000000000000000000000004000000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000000").unwrap();

        let transaction = EthereumTransactionCommon {
            chain_id: U256::one(),
            nonce: U256::from(46),
            gas_price: GasPrice::new(U256::from(29075052730u64)),
            gas_limit: GasLimit::new(U256::from(274722)),
            to: EthereumAddress::from(
                "ef1c6e67703c7bd7107eed8303fbe6ec2554bf6b".to_string(),
            ),
            value: Wei::new(U256::from(760460536160301065u64)),
            data,
            v: U256::one(),
            r: H256::zero(),
            s: H256::zero(),
        };

        assert_eq!(
            Message::parse_slice(
                &hex::decode(
                    "f1099d98570e86be48efa3ba9d3df6531d0069b1f9d7590329ba3791d97a37f1"
                )
                .unwrap()
            )
            .unwrap(),
            transaction.message(),
            "checking message hash"
        );
    }

    #[test]
    fn test_signature_uniswap_inspired() {
        // inspired by 0xf598016f51e0544187088ddd50fd37818fd268a0363a17281576425f3ee334cb
        // private key dcdff53b4f013dbcdc717f89fe3bf4d8b10512aae282b48e01d7530470382701
        let data: Vec<u8> = hex::decode("3593564c000000000000000000000000000000000000000000000000000000000000006000000000000000000000000000000000000000000000000000000000000000a00000000000000000000000000000000000000000000000000000000064023c1700000000000000000000000000000000000000000000000000000000000000030b090c00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000003000000000000000000000000000000000000000000000000000000000000006000000000000000000000000000000000000000000000000000000000000000c000000000000000000000000000000000000000000000000000000000000001e0000000000000000000000000000000000000000000000000000000000000004000000000000000000000000000000000000000000000000000000000000000020000000000000000000000000000000000000000000000000a8db2d41b89b009000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000002ab0c205a56c1e000000000000000000000000000000000000000000000000000000a8db2d41b89b00900000000000000000000000000000000000000000000000000000000000000a000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000002000000000000000000000000c02aaa39b223fe8d0a0e5c4f27ead9083c756cc20000000000000000000000009eb6299e4bb6669e42cb295a254c8492f67ae2c6000000000000000000000000000000000000000000000000000000000000004000000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000000").unwrap();

        let transaction = EthereumTransactionCommon {
            chain_id: U256::one(),
            nonce: U256::from(46),
            gas_price: GasPrice::new(U256::from(29075052730u64)),
            gas_limit: GasLimit::new(U256::from(274722)),
            to: EthereumAddress::from(
                "ef1c6e67703c7bd7107eed8303fbe6ec2554bf6b".to_string(),
            ),
            value: Wei::new(U256::from(760460536160301065u64)),
            data,
            v: U256::zero(),
            r: H256::zero(),
            s: H256::zero(),
        };

        // act
        let signed = transaction.sign_transaction(
            "dcdff53b4f013dbcdc717f89fe3bf4d8b10512aae282b48e01d7530470382701"
                .to_string(),
        );

        let v = U256::from(37);
        let r = H256::from_string_unsafe(
            "c78be9ab81c622c08f7098eefc250935365fb794dfd94aec0fea16c32adec45a",
        );
        let s = H256::from_string_unsafe(
            "5721614264d8490c6866f110c1594151bbcc4fac43758adae644db6bc3314d06",
        );

        assert_eq!(v, signed.v, "checking v");
        assert_eq!(r, signed.r, "checking r");
        assert_eq!(s, signed.s, "checking s");
    }

    #[test]
    fn test_signature_eip155_example() {
        // example directly lifted from eip155 description
        // signing data 0xec098504a817c800825208943535353535353535353535353535353535353535880de0b6b3a764000080018080
        // private key : 0x4646464646464646464646464646464646464646464646464646464646464646
        // corresponding address 0x9d8a62f656a8d1615c1294fd71e9cfb3e4855a4f
        // signed tx : 0xf86c098504a817c800825208943535353535353535353535353535353535353535880de0b6b3a76400008025a028ef61340bd939bc2195fe537567866003e1a15d3c71ff63e1590620aa636276a067cbe9d8997f761aecb703304b3800ccf555c9f3dc64214b297fb1966a3b6d83        let nonce = U256::from(9);

        let transaction = EthereumTransactionCommon {
            chain_id: U256::one(),
            nonce: U256::from(9),
            gas_price: GasPrice::new(U256::from(20000000000u64)),
            gas_limit: GasLimit::new(U256::from(21000)),
            to: EthereumAddress::from(
                "3535353535353535353535353535353535353535".to_string(),
            ),
            value: Wei::new(U256::from(1000000000000000000u64)),
            data: vec![],
            v: U256::one(),
            r: H256::zero(),
            s: H256::zero(),
        };
        let v = U256::from(37);
        let r = H256::from_string_unsafe(
            "28EF61340BD939BC2195FE537567866003E1A15D3C71FF63E1590620AA636276",
        );
        let s = H256::from_string_unsafe(
            "67CBE9D8997F761AECB703304B3800CCF555C9F3DC64214B297FB1966A3B6D83",
        );

        let signed = transaction.sign_transaction(
            "4646464646464646464646464646464646464646464646464646464646464646"
                .to_string(),
        );

        assert_eq!(v, signed.v, "checking v");
        assert_eq!(r, signed.r, "checking r");
        assert_eq!(s, signed.s, "checking s");
    }

    #[test]
    fn test_rlp_decode_encode() {
        let strings =
    ["f86c0a8502540be400825208944bbeeb066ed09b7aed07bf39eee0460dfa261520880de0b6b3a7640000801ca0f3ae52c1ef3300f44df0bcfd1341c232ed6134672b16e35699ae3f5fe2493379a023d23d2955a239dd6f61c4e8b2678d174356ff424eac53da53e17706c43ef871".to_string(),
    "f86a8302ae2a7b82f618948e998a00253cb1747679ac25e69a8d870b52d8898802c68af0bb140000802da0cd2d976eb691dc16a397462c828975f0b836e1b448ecb8f00d9765cf5032cecca066247d13fc2b65fd70a2931b5897fff4b3079e9587e69ac8a0036c99eb5ea927".to_string()];

        strings.iter().fold((), |_, str| {
            let e = EthereumTransactionCommon::from_rlp(str.clone()).unwrap();
            let encoded = e.rlp_bytes();
            assert_eq!(hex::encode(&encoded), *str);
        });
    }
}
