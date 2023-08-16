// SPDX-FileCopyrightText: 2022-2023 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

//! Signature functions for Ethereum compatibility
//!
//! We need to sign and write Ethereum specific values such
//! as addresses and values.

use std::array::TryFromSliceError;

use libsecp256k1::{recover, Message, RecoveryId, Signature};
use primitive_types::{H160, H256, U256};
use rlp::{Decodable, DecoderError, Encodable, Rlp, RlpStream};
use sha3::{Digest, Keccak256};
use thiserror::Error;

use crate::{
    rlp_helpers::{
        append_h256, append_option, append_vec, decode_field, decode_field_h256,
        decode_option, next,
    },
    tx_signature::{TxSigError, TxSignature},
};

#[derive(Error, Debug, PartialEq)]
pub enum SigError {
    #[error("Error decoding RLP encoded byte array: {0}")]
    DecoderError(#[from] DecoderError),

    #[error("Error extracting a slice")]
    SlicingError,

    #[error("Signature error: {0}")]
    TxSigError(TxSigError),

    #[error("Transaction doesn't have a signature")]
    UnsignedTransactionError,
}

impl From<TryFromSliceError> for SigError {
    fn from(_: TryFromSliceError) -> Self {
        Self::SlicingError
    }
}

impl From<TxSigError> for SigError {
    fn from(e: TxSigError) -> Self {
        SigError::TxSigError(e)
    }
}

/// Data common for all kind of Ethereum transactions
/// (transfers, contract creation and contract invocation).
/// All transaction versions (Legacy, EIP-2930 and EIP-1559)
/// are parsed to this common type.
/// This type is common for both signed and unsigned transactions as well.
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
    pub gas_price: U256,
    /// A scalar value equal to the maximum
    /// amount of gas that should be used in executing
    /// this transaction. This is paid up-front, before any
    /// computation is done and may not be increased
    /// later
    pub gas_limit: u64,
    /// The 160-bit address of the message call’s recipient
    /// or, for a contract creation transaction
    pub to: Option<H160>,
    /// A scalar value equal to the number of Wei to
    /// be transferred to the message call’s recipient or,
    /// in the case of contract creation, as an endowment
    /// to the newly created account
    pub value: U256,
    /// the transaction data. In principle this can be large
    pub data: Vec<u8>,
    /// If transaction is unsigned then this field is None
    /// See encoding details in <https://github.com/ethereum/EIPs/blob/master/EIPS/eip-155.md>
    pub signature: Option<TxSignature>,
}

impl EthereumTransactionCommon {
    /// Extracts the Keccak encoding of a message from an EthereumTransactionCommon
    fn message(&self) -> Message {
        let to_sign = EthereumTransactionCommon {
            signature: None,
            ..self.clone()
        };

        let bytes = to_sign.to_bytes();
        let hash: [u8; 32] = Keccak256::digest(bytes).into();
        Message::parse(&hash)
    }

    /// Extracts the signature from an EthereumTransactionCommon
    pub fn signature(&self) -> Result<(Signature, RecoveryId), SigError> {
        let tx_signature = self
            .signature
            .as_ref()
            .ok_or(SigError::UnsignedTransactionError)?;
        tx_signature
            .signature(self.chain_id)
            .map_err(SigError::TxSigError)
    }

    /// Find the caller address from r and s of the common data
    /// for an Ethereum transaction, ie, what address is associated
    /// with the signature of the message.
    // TODO <https://gitlab.com/tezos/tezos/-/milestones/115>
    pub fn caller(&self) -> Result<H160, SigError> {
        let mes = self.message();
        let (sig, ri) = self.signature()?;
        let pk = recover(&mes, &sig, &ri).map_err(TxSigError::ECDSAError)?;
        let serialised = &pk.serialize()[1..];
        let kec = Keccak256::digest(serialised);
        let value: [u8; 20] = kec.as_slice()[12..].try_into()?;

        Ok(value.into())
    }

    ///produce a signed EthereumTransactionCommon. If the initial one was signed
    ///  you should get the same thing.
    pub fn sign_transaction(&self, string_sk: String) -> Result<Self, SigError> {
        let mes = self.message();
        let signature = TxSignature::sign_legacy(&mes, string_sk, self.chain_id)?;

        Ok(EthereumTransactionCommon {
            signature: Some(signature),
            ..self.clone()
        })
    }

    // Unserialize Ethereum tx of arbitrary version from raw bytes.
    // This is a separate method of the tx type
    // but not rlp::Decodable instance because after legacy
    // version a tx encoding, strictly speaking, is not RLP list anymore,
    // rather opaque sequence of bytes.
    pub fn from_bytes(bytes: &[u8]) -> Result<EthereumTransactionCommon, DecoderError> {
        let decoder = Rlp::new(bytes);
        EthereumTransactionCommon::decode(&decoder)
    }

    /// Unserialize an hex string as a RLP encoded legacy transaction.
    pub fn from_hex(e: String) -> Result<EthereumTransactionCommon, DecoderError> {
        let tx =
            hex::decode(e).or(Err(DecoderError::Custom("Couldn't parse hex value")))?;
        Self::from_bytes(&tx)
    }

    // Serialize Ethereum tx of arbitrary version to raw bytes.
    // This is a separate method of the tx type
    // but not rlp::Encodable instance because after legacy
    // version a tx encoding, strictly speaking, is not RLP list anymore,
    // rather opaque sequence of bytes.
    pub fn to_bytes(self) -> Vec<u8> {
        self.rlp_bytes().into()
    }
}

impl From<String> for EthereumTransactionCommon {
    /// Decode a transaction in hex format. Unsafe, to be used only in tests : panics when fails
    fn from(e: String) -> Self {
        EthereumTransactionCommon::from_hex(e).unwrap()
    }
}

impl TryFrom<&[u8]> for EthereumTransactionCommon {
    type Error = DecoderError;

    fn try_from(bytes: &[u8]) -> Result<Self, Self::Error> {
        Self::from_bytes(bytes)
    }
}

impl Decodable for EthereumTransactionCommon {
    fn decode(decoder: &Rlp<'_>) -> Result<EthereumTransactionCommon, DecoderError> {
        if decoder.is_list() {
            if Ok(9) == decoder.item_count() {
                let mut it = decoder.iter();
                let nonce: U256 = decode_field(&next(&mut it)?, "nonce")?;
                let gas_price: U256 = decode_field(&next(&mut it)?, "gas_price")?;
                let gas_limit: u64 = decode_field(&next(&mut it)?, "gas_limit")?;
                let to: Option<H160> = decode_option(&next(&mut it)?, "to")?;
                let value: U256 = decode_field(&next(&mut it)?, "value")?;
                let data: Vec<u8> = decode_field(&next(&mut it)?, "data")?;
                let v: U256 = decode_field(&next(&mut it)?, "v")?;
                let r: H256 = decode_field_h256(&next(&mut it)?, "r")?;
                let s: H256 = decode_field_h256(&next(&mut it)?, "s")?;

                let is_unsigned = r == H256::zero() && s == H256::zero();
                let chain_id = if is_unsigned {
                    // in a rlp encoded unsigned eip-155 transaction, v is used to store the chainid
                    Ok(v)
                } else {
                    // It's a **signed eip-155 transaction** with 9 fields,
                    // it means v has to be {0,1} + CHAIN_ID * 2 + 35 according to
                    // https://eips.ethereum.org/EIPS/eip-155
                    // v > 36 (not v > 35) because we support only chain_id which is strictly greater than 0
                    if v > U256::from(36) {
                        Ok((v - 35) / 2)
                    } else {
                        Err(DecoderError::Custom(
                            "v has to be greater than 36 for a signed EIP-155 transaction",
                        ))
                    }
                }?;

                let signature = if is_unsigned {
                    None
                } else {
                    Some(
                        TxSignature::new(v, r, s)
                            .map_err(|_| DecoderError::Custom("Invalid signature"))?,
                    )
                };

                Ok(Self {
                    chain_id,
                    nonce,
                    gas_price,
                    gas_limit,
                    to,
                    value,
                    data,
                    signature,
                })
            } else {
                Err(DecoderError::RlpIncorrectListLen)
            }
        } else {
            Err(DecoderError::RlpExpectedToBeList)
        }
    }
}

impl Encodable for EthereumTransactionCommon {
    fn rlp_append(&self, stream: &mut RlpStream) {
        stream.begin_list(9);
        stream.append(&self.nonce);
        stream.append(&self.gas_price);
        stream.append(&self.gas_limit);
        append_option(stream, self.to);
        stream.append(&self.value);
        append_vec(stream, self.data.clone());
        match self.signature {
            None => {
                stream.append(&self.chain_id);
                append_h256(stream, H256::zero());
                append_h256(stream, H256::zero());
            }
            Some(ref sig) => Encodable::rlp_append(sig, stream),
        }
    }
}

#[allow(clippy::from_over_into)]
impl Into<Vec<u8>> for EthereumTransactionCommon {
    fn into(self) -> Vec<u8> {
        self.to_bytes()
    }
}

// Produces address from a secret key
// Used in tests only
pub fn string_to_sk_and_address_unsafe(
    s: String,
) -> (libsecp256k1::SecretKey, primitive_types::H160) {
    use libsecp256k1::PublicKey;
    use libsecp256k1::SecretKey;

    let mut data: [u8; 32] = [0u8; 32];
    hex::decode_to_slice(s, &mut data).unwrap();
    let sk = SecretKey::parse(&data).unwrap();
    let pk = PublicKey::from_secret_key(&sk);
    let serialised = &pk.serialize()[1..];
    let kec = Keccak256::digest(serialised);
    let mut value: [u8; 20] = [0u8; 20];
    value.copy_from_slice(&kec[12..]);
    (sk, value.into())
}

// cargo test ethereum::signatures::test --features testing
#[cfg(test)]
mod test {

    use std::ops::Neg;

    use libsecp256k1::curve::Scalar;

    use crate::rlp_helpers::decode_h256;

    use crate::tx_signature::TxSignature;

    use super::*;
    fn address_from_str(s: &str) -> Option<H160> {
        let data = &hex::decode(s).unwrap();
        Some(H160::from_slice(data))
    }

    // utility function to just build a standard correct transaction
    // extracted from example in EIP 155 standard
    // https://github.com/ethereum/EIPs/blob/master/EIPS/eip-155.md
    // signing data 0xec098504a817c800825208943535353535353535353535353535353535353535880de0b6b3a764000080018080
    // private key : 0x4646464646464646464646464646464646464646464646464646464646464646
    // corresponding address 0x9d8a62f656a8d1615c1294fd71e9cfb3e4855a4f
    // signed tx : 0xf86c098504a817c800825208943535353535353535353535353535353535353535880de0b6b3a76400008025a028ef61340bd939bc2195fe537567866003e1a15d3c71ff63e1590620aa636276a067cbe9d8997f761aecb703304b3800ccf555c9f3dc64214b297fb1966a3b6d83
    fn basic_eip155_transaction() -> EthereumTransactionCommon {
        EthereumTransactionCommon {
            chain_id: U256::one(),
            nonce: U256::from(9),
            gas_price: U256::from(20000000000u64),
            gas_limit: 21000u64,
            to: address_from_str("3535353535353535353535353535353535353535"),
            value: U256::from(1000000000000000000u64),
            data: vec![],
            signature: Some(TxSignature::new_unsafe(
                37,
                string_to_h256_unsafe(
                    "28EF61340BD939BC2195FE537567866003E1A15D3C71FF63E1590620AA636276",
                ),
                string_to_h256_unsafe(
                    "67CBE9D8997F761AECB703304B3800CCF555C9F3DC64214B297FB1966A3B6D83",
                ),
            )),
        }
    }

    fn basic_eip155_transaction_unsigned() -> EthereumTransactionCommon {
        EthereumTransactionCommon {
            signature: None,
            ..basic_eip155_transaction()
        }
    }

    fn h256_to_string(e: H256) -> String {
        format!("{:x}", e)
    }

    /// used in test to decode a string and get the size of the decoded input,
    /// before determining the H256 value
    fn decode(str: &str) -> (Result<H256, DecoderError>, usize) {
        let hash = hex::decode(str).unwrap();
        let decoder = Rlp::new(&hash);
        let decoded = decode_h256(&decoder);
        assert!(decoded.is_ok(), "hash should be decoded ok");
        let length = decoder.data().unwrap().len();
        (decoded, length)
    }

    #[test]
    fn test_decode_h256_l0() {
        // rlp encoding of empty is the byte 80
        let (decoded, length) = decode("80");
        assert_eq!(0, length);
        assert_eq!(
            H256::zero(),
            decoded.unwrap(),
            "empty hash should be decoded as 0x0...0"
        );
    }

    #[test]
    fn test_decode_h256_l32() {
        // rlp encoding of hex string of 32 bytes
        let (decoded, length) =
            decode("a03232323232323232323232323232323232323232323232323232323232323232");
        assert_eq!(32, length);
        assert_eq!(
            "3232323232323232323232323232323232323232323232323232323232323232",
            h256_to_string(decoded.unwrap()),
            "32 hash should be decoded as 0x32...32"
        );
    }

    #[test]
    fn test_decode_h256_l31() {
        // rlp encoding of hex string of 31 bytes
        let (decoded, length) =
            decode("9f31313131313131313131313131313131313131313131313131313131313131");
        assert_eq!(31, length);
        assert_eq!(
            "0031313131313131313131313131313131313131313131313131313131313131",
            h256_to_string(decoded.unwrap()),
            "31 hash should be decoded as 0x0031..31"
        );
    }

    #[test]
    fn test_caller_classic() {
        // setup
        let (_sk, address_from_sk) = string_to_sk_and_address_unsafe(
            "4646464646464646464646464646464646464646464646464646464646464646"
                .to_string(),
        );
        let encoded =
        "f86c098504a817c800825208943535353535353535353535353535353535353535880de0b6b3a76400008025a028ef61340bd939bc2195fe537567866003e1a15d3c71ff63e1590620aa636276a067cbe9d8997f761aecb703304b3800ccf555c9f3dc64214b297fb1966a3b6d83".to_string();

        let expected_address =
            address_from_str("9d8A62f656a8d1615C1294fd71e9CFb3E4855A4F").unwrap();

        // act
        let transaction = EthereumTransactionCommon::from_hex(encoded).unwrap();
        let address = transaction.caller().unwrap();

        // assert
        assert_eq!(expected_address, address);
        assert_eq!(expected_address, address_from_sk)
    }

    #[test]
    fn test_decoding_eip_155_example_unsigned() {
        // setup
        let expected_transaction = basic_eip155_transaction_unsigned();
        let signing_data = "ec098504a817c800825208943535353535353535353535353535353535353535880de0b6b3a764000080018080";

        // act
        let tx = hex::decode(signing_data).unwrap();
        let decoded = EthereumTransactionCommon::from_bytes(&tx);
        assert!(decoded.is_ok(), "testing the decoding went ok");

        // assert
        let decoded_transaction = decoded.unwrap();
        assert_eq!(expected_transaction, decoded_transaction)
    }

    #[test]
    fn test_decoding_leading0_signature() {
        // decoding of a transaction where r or s had some leading 0, which where deleted
        let signed_tx = "f888018506fc23ac00831000009412f142944da31ab85458787aaecaf5e34128619d80a40b7d796e0000000000000000000000000000000000000000000000000000000000000000269f75b1bc94b868a5a047470eae6008602e414d1471c2bbd14b37ffe56b1a85c9a001d9d58bb23af2090742aab9824c916fdc021a91f3e8d36571a5fc55547bc596";

        // act
        let tx = hex::decode(signed_tx).unwrap();
        let decoded = EthereumTransactionCommon::from_bytes(&tx);

        // assert
        assert!(decoded.is_ok(), "testing the decoding went ok");
    }

    #[test]
    fn test_encoding_eip155_unsigned() {
        // setup
        let expected_transaction = basic_eip155_transaction_unsigned();
        let signing_data = "ec098504a817c800825208943535353535353535353535353535353535353535880de0b6b3a764000080018080";

        // act
        let encoded = expected_transaction.to_bytes();

        // assert
        assert_eq!(signing_data, hex::encode(encoded));
    }

    pub fn string_to_h256_unsafe(s: &str) -> H256 {
        let mut v: [u8; 32] = [0; 32];
        hex::decode_to_slice(s, &mut v).expect("Could not parse to 256 hex value.");
        H256::from(v)
    }

    fn basic_create() -> EthereumTransactionCommon {
        // transaction "without to field"
        // private key : 0x4646464646464646464646464646464646464646464646464646464646464646
        // corresponding address 0x9d8a62f656a8d1615c1294fd71e9cfb3e4855a4f
        // signed tx : 0xf8572e8506c50218ba8304312280843b9aca0082ffff26a0e9637495be4c216a833ef390b1f6798917c8a102ab165c5085cced7ca1f2eb3aa057854e7044a8fee7bccb6a2c32c4229dd9cbacad74350789e0ce75bf40b6f713
        let nonce = U256::from(46);
        let gas_price = U256::from(29075052730u64);
        let gas_limit = 274722u64;
        let to = None;
        let value = U256::from(1000000000u64);
        let data: Vec<u8> = hex::decode("ffff").unwrap();
        let chain_id = U256::one();
        let r = string_to_h256_unsafe(
            "e9637495be4c216a833ef390b1f6798917c8a102ab165c5085cced7ca1f2eb3a",
        );
        let s = string_to_h256_unsafe(
            "57854e7044a8fee7bccb6a2c32c4229dd9cbacad74350789e0ce75bf40b6f713",
        );
        EthereumTransactionCommon {
            chain_id,
            nonce,
            gas_price,
            gas_limit,
            to,
            value,
            data,
            signature: Some(TxSignature::new_unsafe(38, r, s)),
        }
    }

    #[test]
    fn test_encoding_create() {
        // setup
        let transaction = basic_create();
        let expected_encoded = "f8572e8506c50218ba8304312280843b9aca0082ffff26a0e9637495be4c216a833ef390b1f6798917c8a102ab165c5085cced7ca1f2eb3aa057854e7044a8fee7bccb6a2c32c4229dd9cbacad74350789e0ce75bf40b6f713";

        // act
        let encoded = transaction.to_bytes();

        // assert
        assert_eq!(expected_encoded, hex::encode(encoded));
    }

    #[test]
    fn test_decoding_create() {
        // setup
        let expected_transaction = basic_create();
        let signed_tx = "f8572e8506c50218ba8304312280843b9aca0082ffff26a0e9637495be4c216a833ef390b1f6798917c8a102ab165c5085cced7ca1f2eb3aa057854e7044a8fee7bccb6a2c32c4229dd9cbacad74350789e0ce75bf40b6f713";

        // act
        let tx = hex::decode(signed_tx).unwrap();
        let decoded = EthereumTransactionCommon::from_bytes(&tx);

        // assert
        assert!(decoded.is_ok());
        assert_eq!(expected_transaction, decoded.unwrap());
    }

    #[test]
    fn test_encoding_eip155_signed() {
        // setup
        let expected_transaction = basic_eip155_transaction();
        let signed_tx = "f86c098504a817c800825208943535353535353535353535353535353535353535880de0b6b3a76400008025a028ef61340bd939bc2195fe537567866003e1a15d3c71ff63e1590620aa636276a067cbe9d8997f761aecb703304b3800ccf555c9f3dc64214b297fb1966a3b6d83";

        // act
        let encoded = expected_transaction.to_bytes();

        // assert
        assert_eq!(signed_tx, hex::encode(encoded));
    }

    #[test]
    fn test_decoding_arbitrary_signed() {
        // arbitrary transaction with data
        //setup
        let nonce = U256::from(0);
        let gas_price = U256::from(40000000000u64);
        let gas_limit = 21000u64;
        let to = address_from_str("423163e58aabec5daa3dd1130b759d24bef0f6ea");
        let value = U256::from(5000000000000000u64);
        let data: Vec<u8> = hex::decode("deace8f5000000000000000000000000000000000000000000000000000000000000a4b100000000000000000000000041bca408a6b4029b42883aeb2c25087cab76cb58000000000000000000000000000000000000000000000000002386f26fc10000000000000000000000000000000000000000000000000000002357a49c7d75f600000000000000000000000000000000000000000000000000000000640b5549000000000000000000000000710bda329b2a6224e4b44833de30f38e7f81d5640000000000000000000000000000000000000000000000000000000000000000").unwrap();
        let r = string_to_h256_unsafe(
            "25dd6c973368c45ddfc17f5148e3f468a2e3f2c51920cbe9556a64942b0ab2eb",
        );
        let s = string_to_h256_unsafe(
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
            signature: Some(TxSignature::new_unsafe(37, r, s)),
        };
        let signed_data = "f90150808509502f900082520894423163e58aabec5daa3dd1130b759d24bef0f6ea8711c37937e08000b8e4deace8f5000000000000000000000000000000000000000000000000000000000000a4b100000000000000000000000041bca408a6b4029b42883aeb2c25087cab76cb58000000000000000000000000000000000000000000000000002386f26fc10000000000000000000000000000000000000000000000000000002357a49c7d75f600000000000000000000000000000000000000000000000000000000640b5549000000000000000000000000710bda329b2a6224e4b44833de30f38e7f81d564000000000000000000000000000000000000000000000000000000000000000025a025dd6c973368c45ddfc17f5148e3f468a2e3f2c51920cbe9556a64942b0ab2eba031da07ce40c24b0a01f46fb2abc028b5ccd70dbd1cb330725323edc49a2a9558";

        // act
        let tx = hex::decode(signed_data).unwrap();
        let decoded = EthereumTransactionCommon::from_bytes(&tx);

        // assert
        assert_eq!(Ok(expected_transaction), decoded)
    }

    #[test]
    fn test_decoding_eip_155_example_signed() {
        // setup
        let expected_transaction = basic_eip155_transaction();
        let signed_data = "f86c098504a817c800825208943535353535353535353535353535353535353535880de0b6b3a76400008025a028ef61340bd939bc2195fe537567866003e1a15d3c71ff63e1590620aa636276a067cbe9d8997f761aecb703304b3800ccf555c9f3dc64214b297fb1966a3b6d83";

        // act
        let tx = hex::decode(signed_data).unwrap();
        let decoded = EthereumTransactionCommon::from_bytes(&tx);

        // assert
        assert!(decoded.is_ok(), "testing the decoding went ok");
        let decoded_transaction = decoded.unwrap();
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
        let gas_price = U256::from(29075052730u64);
        let gas_limit = 274722u64;
        let to = address_from_str("ef1c6e67703c7bd7107eed8303fbe6ec2554bf6b");
        let value = U256::from(760460536160301065u64); // /!\ > 2^53 -1
        let data: Vec<u8> = hex::decode("3593564c000000000000000000000000000000000000000000000000000000000000006000000000000000000000000000000000000000000000000000000000000000a00000000000000000000000000000000000000000000000000000000064023c1700000000000000000000000000000000000000000000000000000000000000030b090c00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000003000000000000000000000000000000000000000000000000000000000000006000000000000000000000000000000000000000000000000000000000000000c000000000000000000000000000000000000000000000000000000000000001e0000000000000000000000000000000000000000000000000000000000000004000000000000000000000000000000000000000000000000000000000000000020000000000000000000000000000000000000000000000000a8db2d41b89b009000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000002ab0c205a56c1e000000000000000000000000000000000000000000000000000000a8db2d41b89b00900000000000000000000000000000000000000000000000000000000000000a000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000002000000000000000000000000c02aaa39b223fe8d0a0e5c4f27ead9083c756cc20000000000000000000000009eb6299e4bb6669e42cb295a254c8492f67ae2c6000000000000000000000000000000000000000000000000000000000000004000000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000000").unwrap();
        let r = string_to_h256_unsafe(
            "c78be9ab81c622c08f7098eefc250935365fb794dfd94aec0fea16c32adec45a",
        );
        let s = string_to_h256_unsafe(
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
            signature: Some(TxSignature::new_unsafe(37, r, s)),
        };

        // act
        let signed_data = "f903732e8506c50218ba8304312294ef1c6e67703c7bd7107eed8303fbe6ec2554bf6b880a8db2d41b89b009b903043593564c000000000000000000000000000000000000000000000000000000000000006000000000000000000000000000000000000000000000000000000000000000a00000000000000000000000000000000000000000000000000000000064023c1700000000000000000000000000000000000000000000000000000000000000030b090c00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000003000000000000000000000000000000000000000000000000000000000000006000000000000000000000000000000000000000000000000000000000000000c000000000000000000000000000000000000000000000000000000000000001e0000000000000000000000000000000000000000000000000000000000000004000000000000000000000000000000000000000000000000000000000000000020000000000000000000000000000000000000000000000000a8db2d41b89b009000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000002ab0c205a56c1e000000000000000000000000000000000000000000000000000000a8db2d41b89b00900000000000000000000000000000000000000000000000000000000000000a000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000002000000000000000000000000c02aaa39b223fe8d0a0e5c4f27ead9083c756cc20000000000000000000000009eb6299e4bb6669e42cb295a254c8492f67ae2c600000000000000000000000000000000000000000000000000000000000000400000000000000000000000000000000000000000000000000000000000000001000000000000000000000000000000000000000000000000000000000000000025a0c78be9ab81c622c08f7098eefc250935365fb794dfd94aec0fea16c32adec45aa05721614264d8490c6866f110c1594151bbcc4fac43758adae644db6bc3314d06";
        let tx = hex::decode(signed_data).unwrap();
        let decoded = EthereumTransactionCommon::from_bytes(&tx);

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
        let gas_price = U256::from(29075052730u64);
        let gas_limit = 274722u64;
        let to = address_from_str("ef1c6e67703c7bd7107eed8303fbe6ec2554bf6b");
        let value = U256::from(760460536160301065u64); // /!\ > 2^53 -1
        let data: Vec<u8> = hex::decode("3593564c000000000000000000000000000000000000000000000000000000000000006000000000000000000000000000000000000000000000000000000000000000a00000000000000000000000000000000000000000000000000000000064023c1700000000000000000000000000000000000000000000000000000000000000030b090c00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000003000000000000000000000000000000000000000000000000000000000000006000000000000000000000000000000000000000000000000000000000000000c000000000000000000000000000000000000000000000000000000000000001e0000000000000000000000000000000000000000000000000000000000000004000000000000000000000000000000000000000000000000000000000000000020000000000000000000000000000000000000000000000000a8db2d41b89b009000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000002ab0c205a56c1e000000000000000000000000000000000000000000000000000000a8db2d41b89b00900000000000000000000000000000000000000000000000000000000000000a000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000002000000000000000000000000c02aaa39b223fe8d0a0e5c4f27ead9083c756cc20000000000000000000000009eb6299e4bb6669e42cb295a254c8492f67ae2c6000000000000000000000000000000000000000000000000000000000000004000000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000000").unwrap();
        let r = string_to_h256_unsafe(
            "c78be9ab81c622c08f7098eefc250935365fb794dfd94aec0fea16c32adec45a",
        );
        let s = string_to_h256_unsafe(
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
            signature: Some(TxSignature::new_unsafe(37, r, s)),
        };
        let signed_data = "f903732e8506c50218ba8304312294ef1c6e67703c7bd7107eed8303fbe6ec2554bf6b880a8db2d41b89b009b903043593564c000000000000000000000000000000000000000000000000000000000000006000000000000000000000000000000000000000000000000000000000000000a00000000000000000000000000000000000000000000000000000000064023c1700000000000000000000000000000000000000000000000000000000000000030b090c00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000003000000000000000000000000000000000000000000000000000000000000006000000000000000000000000000000000000000000000000000000000000000c000000000000000000000000000000000000000000000000000000000000001e0000000000000000000000000000000000000000000000000000000000000004000000000000000000000000000000000000000000000000000000000000000020000000000000000000000000000000000000000000000000a8db2d41b89b009000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000002ab0c205a56c1e000000000000000000000000000000000000000000000000000000a8db2d41b89b00900000000000000000000000000000000000000000000000000000000000000a000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000002000000000000000000000000c02aaa39b223fe8d0a0e5c4f27ead9083c756cc20000000000000000000000009eb6299e4bb6669e42cb295a254c8492f67ae2c600000000000000000000000000000000000000000000000000000000000000400000000000000000000000000000000000000000000000000000000000000001000000000000000000000000000000000000000000000000000000000000000025a0c78be9ab81c622c08f7098eefc250935365fb794dfd94aec0fea16c32adec45aa05721614264d8490c6866f110c1594151bbcc4fac43758adae644db6bc3314d06";

        // act
        let encoded = expected_transaction.to_bytes();

        // assert
        assert_eq!(signed_data, hex::encode(encoded));
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
            gas_price: U256::from(30000000000u64),
            gas_limit: 1048576u64,
            to: address_from_str("4e1b2c985d729ae6e05ef7974013eeb48f394449"),
            value: U256::from(1000000000u64),
            data: vec![],
            signature: Some(TxSignature::new_unsafe(
                38,
                string_to_h256_unsafe(
                    "bb03310570362eef497a09dd6e4ef42f56374965cfb09cc4e055a22a2eeac7ad",
                ),
                string_to_h256_unsafe(
                    "6053c1bd83abb30c109801844709202208736d598649afe2a53f024b61b3383f",
                ),
            )),
        };
        let signed_data = "f869018506fc23ac0083100000944e1b2c985d729ae6e05ef7974013eeb48f394449843b9aca008026a0bb03310570362eef497a09dd6e4ef42f56374965cfb09cc4e055a22a2eeac7ada06053c1bd83abb30c109801844709202208736d598649afe2a53f024b61b3383f";

        // act
        let tx = hex::decode(signed_data).unwrap();
        let decoded = EthereumTransactionCommon::from_bytes(&tx);

        // assert
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
            gas_price: U256::from(30000000000u64),
            gas_limit: 1048576u64,
            to: address_from_str("4e1b2c985d729ae6e05ef7974013eeb48f394449"),
            value: U256::from(1000000000u64),
            data: vec![],
            signature: Some(TxSignature::new_unsafe(
                38,
                string_to_h256_unsafe(
                    "bb03310570362eef497a09dd6e4ef42f56374965cfb09cc4e055a22a2eeac7ad",
                ),
                string_to_h256_unsafe(
                    "6053c1bd83abb30c109801844709202208736d598649afe2a53f024b61b3383f",
                ),
            )),
        };

        // assert
        assert_eq!(
            Ok(address_from_str("d9e5c94a12f78a96640757ac97ba0c257e8aa262").unwrap()),
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

        // setup
        let transaction = EthereumTransactionCommon {
            chain_id: U256::one(),
            nonce: U256::from(1),
            gas_price: U256::from(30000000000u64),
            gas_limit: 1048576u64,
            to: address_from_str("4e1b2c985d729ae6e05ef7974013eeb48f394449"),
            value: U256::from(1000000000u64),
            data: vec![],
            signature: None,
        };

        // act
        let signature = transaction
            .sign_transaction(
                "cb9db6b5878db2fa20586e23b7f7b51c22a7c6ed0530daafc2615b116f170cd3"
                    .to_string(),
            )
            .unwrap()
            .signature
            .unwrap();

        // assert
        let r = string_to_h256_unsafe(
            "bb03310570362eef497a09dd6e4ef42f56374965cfb09cc4e055a22a2eeac7ad",
        );
        let s = string_to_h256_unsafe(
            "6053c1bd83abb30c109801844709202208736d598649afe2a53f024b61b3383f",
        );

        assert_eq!(U256::from(38), signature.v(), "checking v");
        assert_eq!(&r, signature.r(), "checking r");
        assert_eq!(&s, signature.s(), "checking s");
    }

    #[test]
    fn test_caller_classic_with_chain_id() {
        let sk = "9bfc9fbe6296c8fef8eb8d6ce2ed5f772a011898c6cabe32d35e7c3e419efb1b"
            .to_string();
        let (_sk, address) = string_to_sk_and_address_unsafe(sk.clone());
        // Check that the derived address is the expected one.
        let expected_address =
            address_from_str("6471A723296395CF1Dcc568941AFFd7A390f94CE").unwrap();
        assert_eq!(expected_address, address);

        // Check that the derived sender address is the expected one.
        let encoded = "f86d80843b9aca00825208940b52d4d3be5d18a7ab5e4476a2f5382bbf2b38d888016345785d8a000080820a95a0d9ef1298c18c88604e3f08e14907a17dfa81b1dc6b37948abe189d8db5cb8a43a06fc7040a71d71d3cb74bd05ead7046b10668ad255da60391c017eea31555f156".to_string();
        let transaction = EthereumTransactionCommon::from_hex(encoded).unwrap();
        let address = transaction.caller().unwrap();
        assert_eq!(expected_address, address);

        // Check that signing the signed transaction returns the same transaction.
        let signed_transaction = transaction.sign_transaction(sk);
        assert_eq!(transaction, signed_transaction.unwrap())
    }

    #[test]
    fn test_caller_eip155_example() {
        let transaction = basic_eip155_transaction();
        assert_eq!(
            address_from_str("9d8a62f656a8d1615c1294fd71e9cfb3e4855a4f").unwrap(),
            transaction.caller().unwrap()
        )
    }

    #[test]
    fn test_caller_eip155_example_fail_eip2() {
        // this test checks that EIP2 part (2) is implemented
        // https://eips.ethereum.org/EIPS/eip-2
        // ie, All transaction signatures whose s-value is greater
        // than secp256k1n/2 are now considered invalid

        let transaction = basic_eip155_transaction();
        let signature = transaction.signature.unwrap();
        // flip s
        let s: &H256 = signature.s();
        let s1: [u8; 32] = (*s).into();
        let mut scalar = Scalar([0; 8]);
        let _ = scalar.set_b32(&s1);
        let flipped_scalar = scalar.neg();
        let flipped_s = H256::from_slice(&flipped_scalar.b32());

        // flip v
        let flipped_v = if signature.v() == U256::from(37) {
            38
        } else {
            37
        };

        let flipped_transaction = EthereumTransactionCommon {
            signature: Some(TxSignature::new_unsafe(
                flipped_v,
                *signature.r(),
                flipped_s,
            )),
            ..transaction
        };

        // as v and s are flipped, the signature is a correct ECDSA signature
        // and the caller should be the same, if EIP2 is not implemented
        // but with EIP2 s should be too big, and the transaction should be rejected
        assert_eq!(
            Err(SigError::TxSigError(TxSigError::ECDSAError(
                libsecp256k1::Error::InvalidSignature
            ))),
            flipped_transaction.caller()
        )
    }

    #[test]
    fn test_caller_uniswap_inspired() {
        // inspired by 0xf598016f51e0544187088ddd50fd37818fd268a0363a17281576425f3ee334cb

        // setup
        let data: Vec<u8> = hex::decode("3593564c000000000000000000000000000000000000000000000000000000000000006000000000000000000000000000000000000000000000000000000000000000a00000000000000000000000000000000000000000000000000000000064023c1700000000000000000000000000000000000000000000000000000000000000030b090c00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000003000000000000000000000000000000000000000000000000000000000000006000000000000000000000000000000000000000000000000000000000000000c000000000000000000000000000000000000000000000000000000000000001e0000000000000000000000000000000000000000000000000000000000000004000000000000000000000000000000000000000000000000000000000000000020000000000000000000000000000000000000000000000000a8db2d41b89b009000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000002ab0c205a56c1e000000000000000000000000000000000000000000000000000000a8db2d41b89b00900000000000000000000000000000000000000000000000000000000000000a000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000002000000000000000000000000c02aaa39b223fe8d0a0e5c4f27ead9083c756cc20000000000000000000000009eb6299e4bb6669e42cb295a254c8492f67ae2c6000000000000000000000000000000000000000000000000000000000000004000000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000000").unwrap();

        let transaction = EthereumTransactionCommon {
            chain_id: U256::one(),
            nonce: U256::from(46),
            gas_price: U256::from(29075052730u64),
            gas_limit: 274722u64,
            to: address_from_str("ef1c6e67703c7bd7107eed8303fbe6ec2554bf6b"),
            value: U256::from(760460536160301065u64),
            data,
            signature: Some(TxSignature::new_unsafe(
                37,
                string_to_h256_unsafe(
                    "c78be9ab81c622c08f7098eefc250935365fb794dfd94aec0fea16c32adec45a",
                ),
                string_to_h256_unsafe(
                    "5721614264d8490c6866f110c1594151bbcc4fac43758adae644db6bc3314d06",
                ),
            )),
        };

        // check
        assert_eq!(
            Ok(address_from_str("af1276cbb260bb13deddb4209ae99ae6e497f446").unwrap()),
            transaction.caller(),
            "checking caller"
        )
    }

    #[test]
    fn test_message_uniswap_inspired() {
        // inspired by 0xf598016f51e0544187088ddd50fd37818fd268a0363a17281576425f3ee334cb

        // setup
        let data: Vec<u8> = hex::decode("3593564c000000000000000000000000000000000000000000000000000000000000006000000000000000000000000000000000000000000000000000000000000000a00000000000000000000000000000000000000000000000000000000064023c1700000000000000000000000000000000000000000000000000000000000000030b090c00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000003000000000000000000000000000000000000000000000000000000000000006000000000000000000000000000000000000000000000000000000000000000c000000000000000000000000000000000000000000000000000000000000001e0000000000000000000000000000000000000000000000000000000000000004000000000000000000000000000000000000000000000000000000000000000020000000000000000000000000000000000000000000000000a8db2d41b89b009000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000002ab0c205a56c1e000000000000000000000000000000000000000000000000000000a8db2d41b89b00900000000000000000000000000000000000000000000000000000000000000a000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000002000000000000000000000000c02aaa39b223fe8d0a0e5c4f27ead9083c756cc20000000000000000000000009eb6299e4bb6669e42cb295a254c8492f67ae2c6000000000000000000000000000000000000000000000000000000000000004000000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000000").unwrap();

        let transaction = EthereumTransactionCommon {
            chain_id: U256::one(),
            nonce: U256::from(46),
            gas_price: U256::from(29075052730u64),
            gas_limit: 274722u64,
            to: address_from_str("ef1c6e67703c7bd7107eed8303fbe6ec2554bf6b"),
            value: U256::from(760460536160301065u64),
            data,
            signature: None,
        };

        // check
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
            gas_price: U256::from(29075052730u64),
            gas_limit: 274722u64,
            to: address_from_str("ef1c6e67703c7bd7107eed8303fbe6ec2554bf6b"),
            value: U256::from(760460536160301065u64),
            data,
            signature: None,
        };

        // act
        let signature = transaction
            .sign_transaction(
                "dcdff53b4f013dbcdc717f89fe3bf4d8b10512aae282b48e01d7530470382701"
                    .to_string(),
            )
            .unwrap()
            .signature
            .unwrap();

        // assert
        let v = U256::from(37);
        let r = string_to_h256_unsafe(
            "c78be9ab81c622c08f7098eefc250935365fb794dfd94aec0fea16c32adec45a",
        );
        let s = string_to_h256_unsafe(
            "5721614264d8490c6866f110c1594151bbcc4fac43758adae644db6bc3314d06",
        );

        assert_eq!(v, signature.v(), "checking v");
        assert_eq!(&r, signature.r(), "checking r");
        assert_eq!(&s, signature.s(), "checking s");
    }

    #[test]
    fn test_signature_eip155_example() {
        // example directly lifted from eip155 description
        // signing data 0xec098504a817c800825208943535353535353535353535353535353535353535880de0b6b3a764000080018080
        // private key : 0x4646464646464646464646464646464646464646464646464646464646464646
        // corresponding address 0x9d8a62f656a8d1615c1294fd71e9cfb3e4855a4f
        // signed tx : 0xf86c098504a817c800825208943535353535353535353535353535353535353535880de0b6b3a76400008025a028ef61340bd939bc2195fe537567866003e1a15d3c71ff63e1590620aa636276a067cbe9d8997f761aecb703304b3800ccf555c9f3dc64214b297fb1966a3b6d83        let nonce = U256::from(9);

        // setup
        let transaction = basic_eip155_transaction_unsigned();
        let expected_signed = basic_eip155_transaction();

        // act
        let signed = transaction.sign_transaction(
            "4646464646464646464646464646464646464646464646464646464646464646"
                .to_string(),
        );

        // assert
        assert_eq!(Ok(expected_signed), signed, "checking signed transaction")
    }

    #[test]
    fn test_rlp_decode_fails_without_chain_id() {
        // This transaction is signed but its v doesn't equal to CHAIN_ID * 2 + 35 + {0, 1}
        // but equal to 27/28 as in "old" (before https://eips.ethereum.org/EIPS/eip-155)
        // six fields encoding
        let malformed_tx = "f86c0a8502540be400825208944bbeeb066ed09b7aed07bf39eee0460dfa261520880de0b6b3a7640000801ca0f3ae52c1ef3300f44df0bcfd1341c232ed6134672b16e35699ae3f5fe2493379a023d23d2955a239dd6f61c4e8b2678d174356ff424eac53da53e17706c43ef871".to_string();
        let e = EthereumTransactionCommon::from_hex(malformed_tx);
        assert!(e.is_err());
    }

    #[test]
    fn test_rlp_decode_encode_with_valid_chain_id() {
        let wellformed_tx =
    "f86a8302ae2a7b82f618948e998a00253cb1747679ac25e69a8d870b52d8898802c68af0bb140000802da0cd2d976eb691dc16a397462c828975f0b836e1b448ecb8f00d9765cf5032cecca066247d13fc2b65fd70a2931b5897fff4b3079e9587e69ac8a0036c99eb5ea927".to_string();
        let e = EthereumTransactionCommon::from_hex(wellformed_tx.clone()).unwrap();
        let encoded = e.to_bytes();
        assert_eq!(hex::encode(encoded), wellformed_tx);
    }

    #[test]
    fn test_decoding_not_eip_155_fails_gracefully() {
        // decoding of a transaction that is not eip 155, ie v = 28 / 27
        // initial transaction:
        // {
        //     "nonce": "0x0",
        //     "gasPrice": "0x10000000000",
        //     "gasLimit": "0x25000",
        //     "value": "0x0",
        //     "data": "0x608060405234801561001057600080fd5b50602a600081905550610150806100286000396000f3fe608060405234801561001057600080fd5b50600436106100365760003560e01c80632e64cec11461003b5780636057361d14610059575b600080fd5b610043610075565b60405161005091906100a1565b60405180910390f35b610073600480360381019061006e91906100ed565b61007e565b005b60008054905090565b8060008190555050565b6000819050919050565b61009b81610088565b82525050565b60006020820190506100b66000830184610092565b92915050565b600080fd5b6100ca81610088565b81146100d557600080fd5b50565b6000813590506100e7816100c1565b92915050565b600060208284031215610103576101026100bc565b5b6000610111848285016100d8565b9150509291505056fea26469706673582212204d6c1853cec27824f5dbf8bcd0994714258d22fc0e0dc8a2460d87c70e3e57a564736f6c63430008120033",
        //     "chainId": 0
        // }
        // private key: 0xe75f4c63daecfbb5be03f65940257f5b15e440e6cf26faa126ce68741d5d0f78
        // caller address: 0x3dbeca6e9a6f0677e3c7b5946fc8adbb1b071e0a

        // setup
        let signed_tx = "f901cc8086010000000000830250008080b90178608060405234801561001057600080fd5b50602a600081905550610150806100286000396000f3fe608060405234801561001057600080fd5b50600436106100365760003560e01c80632e64cec11461003b5780636057361d14610059575b600080fd5b610043610075565b60405161005091906100a1565b60405180910390f35b610073600480360381019061006e91906100ed565b61007e565b005b60008054905090565b8060008190555050565b6000819050919050565b61009b81610088565b82525050565b60006020820190506100b66000830184610092565b92915050565b600080fd5b6100ca81610088565b81146100d557600080fd5b50565b6000813590506100e7816100c1565b92915050565b600060208284031215610103576101026100bc565b5b6000610111848285016100d8565b9150509291505056fea26469706673582212204d6c1853cec27824f5dbf8bcd0994714258d22fc0e0dc8a2460d87c70e3e57a564736f6c634300081200331ca06d851632958801b6919ba534b4b1feb1bdfaabd0d42890bce200a11ac735d58da0219b058d7169d7a4839c5cdd555b0820b545797365287a81ba409419912de7b1";
        // act
        let tx = hex::decode(signed_tx).unwrap();
        let decoded = EthereumTransactionCommon::from_bytes(&tx);

        // sanity check
        assert_eq!(
            decoded.err(),
            Some(DecoderError::Custom(
                "v has to be greater than 36 for a signed EIP-155 transaction",
            ))
        );
    }

    #[test]
    fn test_signature_unsigned_fails_gracefully() {
        let transaction = basic_eip155_transaction_unsigned();

        // check signature fails gracefully
        assert!(
            transaction.signature().is_err(),
            "testing signature for unsigned fails"
        );
    }

    #[test]
    fn test_impossible_create_invalid_sig() {
        let basic = basic_eip155_transaction();
        let signature = basic.signature.unwrap();
        assert!(TxSignature::new(U256::from(38), H256::zero(), *signature.s()).is_err());
        assert!(TxSignature::new(U256::from(38), *signature.r(), H256::zero()).is_err());
    }

    #[test]
    fn test_signature_invalid_parity_fails_gracefully() {
        let basic = basic_eip155_transaction();
        let signature = basic.signature.unwrap();
        // most data is not relevant here, the point is to test failure mode of signature verification
        let transaction = EthereumTransactionCommon {
            signature: Some(TxSignature::new_unsafe(
                150,
                signature.r().to_owned(),
                signature.s().to_owned(),
            )),
            chain_id: U256::one(),
            ..basic
        };

        // check signature fails gracefully
        assert!(
            transaction.signature().is_err(),
            "testing signature checking fails gracefully"
        );
    }

    #[test]
    fn test_signature_invalid_chain_id_fails_gracefully() {
        // most data is not relevant here, the point is to test failure mode of signature verification
        let transaction = EthereumTransactionCommon {
            chain_id: U256::max_value(), // chain_id will overflow parity computation
            ..basic_eip155_transaction()
        };

        // check signature fails gracefully
        assert!(
            transaction.signature().is_err(),
            "testing signature checking fails gracefully"
        );
    }

    #[test]
    fn test_sign_invalid_chain_id_fails_gracefully() {
        // most data is not relevant here, the point is to test failure mode of signature verification
        let transaction = EthereumTransactionCommon {
            chain_id: U256::max_value(),
            ..basic_eip155_transaction_unsigned()
        };

        // check signature fails gracefully
        assert!(
            transaction
                .sign_transaction(
                    "4646464646464646464646464646464646464646464646464646464646464646"
                        .to_string()
                )
                .is_err(),
            "testing signature fails gracefully"
        );
    }
}
