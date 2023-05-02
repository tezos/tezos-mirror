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

use crate::address::EthereumAddress;
use hex::FromHexError;
use libsecp256k1::{
    curve::Scalar, recover, sign, verify, Message, PublicKey, RecoveryId, SecretKey,
    Signature,
};
use primitive_types::{H256, U256};
use rlp::{Decodable, DecoderError, Encodable, Rlp, RlpIterator, RlpStream};
use sha3::{Digest, Keccak256};
use thiserror::Error;

#[derive(Error, Debug, PartialEq)]
pub enum ParityError {
    #[error("Couldn't reconstruct V from chain_id: {0}")]
    ChainId(U256),

    #[error("Couldn't reconstruct parity from V: {0}")]
    V(U256),
}
#[derive(Error, Debug, PartialEq)]
pub enum TransactionError {
    #[error("Error reading a hex string: {0}")]
    HexError(#[from] FromHexError),

    #[error("Error decoding RLP encoded byte array: {0}")]
    DecoderError(#[from] DecoderError),

    #[error("Error extracting a slice")]
    SlicingError,

    #[error("Error manipulating ECDSA key: {0}")]
    ECDSAError(libsecp256k1::Error),

    #[error("Error recomputing parity of signature: {0}")]
    Parity(ParityError),
}

impl From<libsecp256k1::Error> for TransactionError {
    fn from(e: libsecp256k1::Error) -> Self {
        Self::ECDSAError(e)
    }
}

impl From<TryFromSliceError> for TransactionError {
    fn from(_: TryFromSliceError) -> Self {
        Self::SlicingError
    }
}
/// produces address from a secret key
pub fn string_to_sk_and_address(
    s: String,
) -> Result<(SecretKey, EthereumAddress), TransactionError> {
    let mut data: [u8; 32] = [0u8; 32];
    hex::decode_to_slice(s, &mut data)?;
    let sk = SecretKey::parse(&data)?;
    let pk = PublicKey::from_secret_key(&sk);
    let serialised = &pk.serialize()[1..];
    let kec = Keccak256::digest(serialised);
    let mut value: [u8; 20] = [0u8; 20];
    value.copy_from_slice(&kec[12..]);
    Ok((sk, EthereumAddress::from(value)))
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
    pub gas_price: U256,
    /// A scalar value equal to the maximum
    /// amount of gas that should be used in executing
    /// this transaction. This is paid up-front, before any
    /// computation is done and may not be increased
    /// later
    pub gas_limit: U256,
    /// The 160-bit address of the message call’s recipient
    /// or, for a contract creation transaction
    pub to: EthereumAddress,
    /// A scalar value equal to the number of Wei to
    /// be transferred to the message call’s recipient or,
    /// in the case of contract creation, as an endowment
    /// to the newly created account
    pub value: U256,
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

    /// recompute parity from v and chain_id
    fn compute_parity(&self) -> Option<U256> {
        let chain_id_encoding = self
            .chain_id
            .checked_mul(U256::from(2))?
            .checked_add(U256::from(35))?;
        self.v.checked_sub(chain_id_encoding)
    }

    /// Extracts the signature from an EthereumTransactionCommon
    pub fn signature(&self) -> Result<(Signature, RecoveryId), TransactionError> {
        // copy r to Scalar
        let r: H256 = self.r;
        let r1: [u8; 32] = r.into();
        let mut r = Scalar([0; 8]);
        let _ = r.set_b32(&r1);
        // copy s to Scalar
        let s: H256 = self.s;
        let s1: [u8; 32] = s.into();
        let mut s = Scalar([0; 8]);
        let _ = s.set_b32(&s1);
        // recompute parity from v and chain_id
        let ri_val = self
            .compute_parity()
            .ok_or(TransactionError::Parity(ParityError::V(self.v)))?;
        let ri = RecoveryId::parse(ri_val.byte(0))?;
        Ok((Signature { r, s }, ri))
    }
    /// Find the caller address from r and s of the common data
    /// for an Ethereum transaction, ie, what address is associated
    /// with the signature of the message.
    /// TODO <https://gitlab.com/tezos/tezos/-/milestones/115>
    pub fn caller(&self) -> Result<EthereumAddress, TransactionError> {
        let mes = self.message();
        let (sig, ri) = self.signature()?;
        let pk = recover(&mes, &sig, &ri)?;
        let serialised = &pk.serialize()[1..];
        let kec = Keccak256::digest(serialised);
        let value: [u8; 20] = kec.as_slice()[12..].try_into()?;

        Ok(EthereumAddress::from(value))
    }

    /// compute v from parity and chain_id
    fn compute_v(&self, parity: u8) -> Option<U256> {
        if self.chain_id == U256::zero() {
            // parity is 0 or 1
            Some((27 + parity).into())
        } else {
            let chain_id_encoding = self
                .chain_id
                .checked_mul(U256::from(2))?
                .checked_add(U256::from(35))?;
            U256::from(parity).checked_add(chain_id_encoding)
        }
    }

    ///produce a signed EthereumTransactionCommon. If the initial one was signed
    ///  you should get the same thing.
    pub fn sign_transaction(&self, string_sk: String) -> Result<Self, TransactionError> {
        let hex: &[u8] = &hex::decode(string_sk)?;
        let sk = SecretKey::parse_slice(hex)?;
        let mes = self.message();
        let (sig, ri) = sign(&mes, &sk);
        let Signature { r, s } = sig;
        let (r, s) = (H256::from(r.b32()), H256::from(s.b32()));

        let parity: u8 = ri.into();
        let v = self.compute_v(parity).ok_or(TransactionError::Parity(
            ParityError::ChainId(self.chain_id),
        ))?;
        Ok(EthereumTransactionCommon {
            v,
            r,
            s,
            ..self.clone()
        })
    }

    /// verifies the signature
    pub fn verify_signature(self) -> Result<bool, TransactionError> {
        let mes = self.message();
        let (sig, ri) = self.signature()?;
        let pk = recover(&mes, &sig, &ri)?;
        Ok(verify(&mes, &sig, &pk))
    }

    /// Unserialize bytes as a RLP encoded legacy transaction.
    pub fn from_rlp_bytes(
        bytes: &[u8],
    ) -> Result<EthereumTransactionCommon, DecoderError> {
        let decoder = Rlp::new(bytes);
        EthereumTransactionCommon::decode(&decoder)
    }

    /// Unserialize an hex string as a RLP encoded legacy transaction.
    pub fn from_rlp(e: String) -> Result<EthereumTransactionCommon, DecoderError> {
        let tx =
            hex::decode(e).or(Err(DecoderError::Custom("Couldn't parse hex value")))?;
        Self::from_rlp_bytes(&tx)
    }

    fn append_internal_h256(h256: &H256, s: &mut rlp::RlpStream) {
        if &H256::zero() != h256 {
            s.append(h256);
        } else {
            // we could make the distinction between 0 and null
            // but we don't, null is encoded as 0
            // which is not such a big deal as H256 is used for hashed values
            s.append_empty_data();
        }
    }
}

impl From<String> for EthereumTransactionCommon {
    /// Decode a transaction in hex format. Unsafe, to be used only in tests : panics when fails
    fn from(e: String) -> Self {
        EthereumTransactionCommon::from_rlp(e).unwrap()
    }
}

impl TryFrom<&[u8]> for EthereumTransactionCommon {
    type Error = DecoderError;

    fn try_from(bytes: &[u8]) -> Result<Self, Self::Error> {
        Self::from_rlp_bytes(bytes)
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

fn decode_h256(decoder: &Rlp<'_>) -> Result<H256, DecoderError> {
    let length = decoder.data()?.len();
    if length == 32 {
        Ok(H256::from_slice(decoder.data()?))
    } else if length < 32 && length > 0 {
        // there were missing 0 that encoding deleted
        let missing = 32 - length;
        let mut full = [0u8; 32];
        full[missing..].copy_from_slice(decoder.data()?);
        Ok(H256::from(full))
    } else if decoder.data()?.is_empty() {
        // considering the case empty allows to decode unsigned transactions
        Ok(H256::zero())
    } else {
        Err(DecoderError::RlpInvalidLength)
    }
}

fn decode_field_h256(
    decoder: &Rlp<'_>,
    field_name: &'static str,
) -> Result<H256, DecoderError> {
    let custom_err = |_: DecoderError| (DecoderError::Custom(field_name));
    decode_h256(decoder).map_err(custom_err)
}

impl Decodable for EthereumTransactionCommon {
    fn decode(decoder: &Rlp<'_>) -> Result<EthereumTransactionCommon, DecoderError> {
        if decoder.is_list() && decoder.item_count() == Ok(9) {
            let mut it = decoder.iter();
            let nonce: U256 = decode_field(&next(&mut it)?, "nonce")?;
            let gas_price: U256 = decode_field(&next(&mut it)?, "gas_price")?;
            let gas_limit: U256 = decode_field(&next(&mut it)?, "gas_limit")?;
            let to: EthereumAddress = decode_field(&next(&mut it)?, "to")?;
            let value: U256 = decode_field(&next(&mut it)?, "value")?;
            let data: Vec<u8> = decode_field(&next(&mut it)?, "data")?;
            let v: U256 = decode_field(&next(&mut it)?, "v")?;
            let r: H256 = decode_field_h256(&next(&mut it)?, "r")?;
            let s: H256 = decode_field_h256(&next(&mut it)?, "s")?;
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
        stream.append(&self.gas_price);
        stream.append(&self.gas_limit);
        stream.append_internal(&self.to);
        stream.append(&self.value);
        if self.data.is_empty() {
            // no data == null, not empty vec
            stream.append_empty_data();
        } else {
            stream.append_iter(self.data.iter().cloned());
        }
        stream.append(&self.v);
        Self::append_internal_h256(&self.r, stream);
        Self::append_internal_h256(&self.s, stream);
        assert!(stream.is_finished());
    }
}

#[allow(clippy::from_over_into)]
impl Into<Vec<u8>> for EthereumTransactionCommon {
    fn into(self) -> Vec<u8> {
        self.rlp_bytes().to_vec()
    }
}

// cargo test ethereum::signatures::test --features testing
#[cfg(test)]
mod test {
    use super::*;

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
            gas_limit: U256::from(21000),
            to: EthereumAddress::from(
                "3535353535353535353535353535353535353535".to_string(),
            ),
            value: U256::from(1000000000000000000u64),
            data: vec![],
            v: U256::from(37),
            r: string_to_h256_unsafe(
                "28EF61340BD939BC2195FE537567866003E1A15D3C71FF63E1590620AA636276",
            ),
            s: string_to_h256_unsafe(
                "67CBE9D8997F761AECB703304B3800CCF555C9F3DC64214B297FB1966A3B6D83",
            ),
        }
    }

    fn basic_eip155_transaction_unsigned() -> EthereumTransactionCommon {
        EthereumTransactionCommon {
            v: U256::one(),
            r: H256::zero(),
            s: H256::zero(),
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
        let (_sk, address_from_sk) = string_to_sk_and_address(
            "4646464646464646464646464646464646464646464646464646464646464646"
                .to_string(),
        )
        .unwrap();
        let encoded =
        "f86c098504a817c800825208943535353535353535353535353535353535353535880de0b6b3a76400008025a028ef61340bd939bc2195fe537567866003e1a15d3c71ff63e1590620aa636276a067cbe9d8997f761aecb703304b3800ccf555c9f3dc64214b297fb1966a3b6d83".to_string();

        let expected_address_string: [u8; 20] =
            hex::decode("9d8A62f656a8d1615C1294fd71e9CFb3E4855A4F")
                .unwrap()
                .try_into()
                .unwrap();
        let expected_address = EthereumAddress::from(expected_address_string);

        // act
        let transaction = EthereumTransactionCommon::from_rlp(encoded).unwrap();
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
        let decoder = Rlp::new(&tx);
        let decoded = EthereumTransactionCommon::decode(&decoder);
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
        let decoder = Rlp::new(&tx);
        let decoded = EthereumTransactionCommon::decode(&decoder);

        // assert
        assert!(decoded.is_ok(), "testing the decoding went ok");
    }

    #[test]
    fn test_encoding_eip155_unsigned() {
        // setup
        let expected_transaction = basic_eip155_transaction_unsigned();
        let signing_data = "ec098504a817c800825208943535353535353535353535353535353535353535880de0b6b3a764000080018080";

        // act
        let encoded = expected_transaction.rlp_bytes();

        // assert
        assert_eq!(signing_data, hex::encode(&encoded));
    }

    pub fn string_to_h256_unsafe(s: &str) -> H256 {
        let mut v: [u8; 32] = [0; 32];
        hex::decode_to_slice(s, &mut v).expect("Could not parse to 256 hex value.");
        H256::from(v)
    }

    #[test]
    fn test_encoding_create() {
        // setup

        // transaction "without to field"
        // private key : 0x4646464646464646464646464646464646464646464646464646464646464646
        // corresponding address 0x9d8a62f656a8d1615c1294fd71e9cfb3e4855a4f
        // signed tx : 0xf8572e8506c50218ba8304312280843b9aca0082ffff26a0e9637495be4c216a833ef390b1f6798917c8a102ab165c5085cced7ca1f2eb3aa057854e7044a8fee7bccb6a2c32c4229dd9cbacad74350789e0ce75bf40b6f713
        let nonce = U256::from(46);
        let gas_price = U256::from(29075052730u64);
        let gas_limit = U256::from(274722);
        let to = EthereumAddress::from("".to_string());
        let value = U256::from(1000000000u64);
        let data: Vec<u8> = hex::decode("ffff").unwrap();
        let chain_id = U256::one();
        let v = U256::from(38);
        let r = string_to_h256_unsafe(
            "e9637495be4c216a833ef390b1f6798917c8a102ab165c5085cced7ca1f2eb3a",
        );
        let s = string_to_h256_unsafe(
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

        let signed_tx = "f8572e8506c50218ba8304312280843b9aca0082ffff26a0e9637495be4c216a833ef390b1f6798917c8a102ab165c5085cced7ca1f2eb3aa057854e7044a8fee7bccb6a2c32c4229dd9cbacad74350789e0ce75bf40b6f713";

        // act
        let encoded = expected_transaction.rlp_bytes();

        // assert
        assert_eq!(signed_tx, hex::encode(&encoded));
    }

    #[test]
    fn test_encoding_eip155_signed() {
        // setup
        let expected_transaction = basic_eip155_transaction();
        let signed_tx = "f86c098504a817c800825208943535353535353535353535353535353535353535880de0b6b3a76400008025a028ef61340bd939bc2195fe537567866003e1a15d3c71ff63e1590620aa636276a067cbe9d8997f761aecb703304b3800ccf555c9f3dc64214b297fb1966a3b6d83";

        // act
        let encoded = expected_transaction.rlp_bytes();

        // assert
        assert_eq!(signed_tx, hex::encode(&encoded));
    }

    #[test]
    fn test_decoding_arbitrary_signed() {
        // arbitrary transaction with data
        //setup
        let nonce = U256::from(0);
        let gas_price = U256::from(40000000000u64);
        let gas_limit = U256::from(21000);
        let to =
            EthereumAddress::from("423163e58aabec5daa3dd1130b759d24bef0f6ea".to_string());
        let value = U256::from(5000000000000000u64);
        let data: Vec<u8> = hex::decode("deace8f5000000000000000000000000000000000000000000000000000000000000a4b100000000000000000000000041bca408a6b4029b42883aeb2c25087cab76cb58000000000000000000000000000000000000000000000000002386f26fc10000000000000000000000000000000000000000000000000000002357a49c7d75f600000000000000000000000000000000000000000000000000000000640b5549000000000000000000000000710bda329b2a6224e4b44833de30f38e7f81d5640000000000000000000000000000000000000000000000000000000000000000").unwrap();
        let v = U256::from(37);
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
            v,
            r,
            s,
        };
        let signed_data = "f90150808509502f900082520894423163e58aabec5daa3dd1130b759d24bef0f6ea8711c37937e08000b8e4deace8f5000000000000000000000000000000000000000000000000000000000000a4b100000000000000000000000041bca408a6b4029b42883aeb2c25087cab76cb58000000000000000000000000000000000000000000000000002386f26fc10000000000000000000000000000000000000000000000000000002357a49c7d75f600000000000000000000000000000000000000000000000000000000640b5549000000000000000000000000710bda329b2a6224e4b44833de30f38e7f81d564000000000000000000000000000000000000000000000000000000000000000025a025dd6c973368c45ddfc17f5148e3f468a2e3f2c51920cbe9556a64942b0ab2eba031da07ce40c24b0a01f46fb2abc028b5ccd70dbd1cb330725323edc49a2a9558";

        // act
        let tx = hex::decode(signed_data).unwrap();
        let decoder = Rlp::new(&tx);
        let decoded = EthereumTransactionCommon::decode(&decoder);

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
        let decoder = Rlp::new(&tx);
        let decoded = EthereumTransactionCommon::decode(&decoder);

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
        let gas_limit = U256::from(274722);
        let to =
            EthereumAddress::from("ef1c6e67703c7bd7107eed8303fbe6ec2554bf6b".to_string());
        let value = U256::from(760460536160301065u64); // /!\ > 2^53 -1
        let data: Vec<u8> = hex::decode("3593564c000000000000000000000000000000000000000000000000000000000000006000000000000000000000000000000000000000000000000000000000000000a00000000000000000000000000000000000000000000000000000000064023c1700000000000000000000000000000000000000000000000000000000000000030b090c00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000003000000000000000000000000000000000000000000000000000000000000006000000000000000000000000000000000000000000000000000000000000000c000000000000000000000000000000000000000000000000000000000000001e0000000000000000000000000000000000000000000000000000000000000004000000000000000000000000000000000000000000000000000000000000000020000000000000000000000000000000000000000000000000a8db2d41b89b009000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000002ab0c205a56c1e000000000000000000000000000000000000000000000000000000a8db2d41b89b00900000000000000000000000000000000000000000000000000000000000000a000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000002000000000000000000000000c02aaa39b223fe8d0a0e5c4f27ead9083c756cc20000000000000000000000009eb6299e4bb6669e42cb295a254c8492f67ae2c6000000000000000000000000000000000000000000000000000000000000004000000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000000").unwrap();
        let v = U256::from(37);
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
        let gas_price = U256::from(29075052730u64);
        let gas_limit = U256::from(274722);
        let to =
            EthereumAddress::from("ef1c6e67703c7bd7107eed8303fbe6ec2554bf6b".to_string());
        let value = U256::from(760460536160301065u64); // /!\ > 2^53 -1
        let data: Vec<u8> = hex::decode("3593564c000000000000000000000000000000000000000000000000000000000000006000000000000000000000000000000000000000000000000000000000000000a00000000000000000000000000000000000000000000000000000000064023c1700000000000000000000000000000000000000000000000000000000000000030b090c00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000003000000000000000000000000000000000000000000000000000000000000006000000000000000000000000000000000000000000000000000000000000000c000000000000000000000000000000000000000000000000000000000000001e0000000000000000000000000000000000000000000000000000000000000004000000000000000000000000000000000000000000000000000000000000000020000000000000000000000000000000000000000000000000a8db2d41b89b009000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000002ab0c205a56c1e000000000000000000000000000000000000000000000000000000a8db2d41b89b00900000000000000000000000000000000000000000000000000000000000000a000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000002000000000000000000000000c02aaa39b223fe8d0a0e5c4f27ead9083c756cc20000000000000000000000009eb6299e4bb6669e42cb295a254c8492f67ae2c6000000000000000000000000000000000000000000000000000000000000004000000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000000").unwrap();
        let v = U256::from(37);
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
            v,
            r,
            s,
        };
        let signed_data = "f903732e8506c50218ba8304312294ef1c6e67703c7bd7107eed8303fbe6ec2554bf6b880a8db2d41b89b009b903043593564c000000000000000000000000000000000000000000000000000000000000006000000000000000000000000000000000000000000000000000000000000000a00000000000000000000000000000000000000000000000000000000064023c1700000000000000000000000000000000000000000000000000000000000000030b090c00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000003000000000000000000000000000000000000000000000000000000000000006000000000000000000000000000000000000000000000000000000000000000c000000000000000000000000000000000000000000000000000000000000001e0000000000000000000000000000000000000000000000000000000000000004000000000000000000000000000000000000000000000000000000000000000020000000000000000000000000000000000000000000000000a8db2d41b89b009000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000002ab0c205a56c1e000000000000000000000000000000000000000000000000000000a8db2d41b89b00900000000000000000000000000000000000000000000000000000000000000a000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000002000000000000000000000000c02aaa39b223fe8d0a0e5c4f27ead9083c756cc20000000000000000000000009eb6299e4bb6669e42cb295a254c8492f67ae2c600000000000000000000000000000000000000000000000000000000000000400000000000000000000000000000000000000000000000000000000000000001000000000000000000000000000000000000000000000000000000000000000025a0c78be9ab81c622c08f7098eefc250935365fb794dfd94aec0fea16c32adec45aa05721614264d8490c6866f110c1594151bbcc4fac43758adae644db6bc3314d06";

        // act
        let encoded = expected_transaction.rlp_bytes();

        // assert
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
            gas_price: U256::from(30000000000u64),
            gas_limit: U256::from(1048576),
            to: EthereumAddress::from(
                "4e1b2c985d729ae6e05ef7974013eeb48f394449".to_string(),
            ),
            value: U256::from(1000000000u64),
            data: vec![],
            v: U256::from(38),
            r: string_to_h256_unsafe(
                "bb03310570362eef497a09dd6e4ef42f56374965cfb09cc4e055a22a2eeac7ad",
            ),
            s: string_to_h256_unsafe(
                "6053c1bd83abb30c109801844709202208736d598649afe2a53f024b61b3383f",
            ),
        };
        let signed_data = "f869018506fc23ac0083100000944e1b2c985d729ae6e05ef7974013eeb48f394449843b9aca008026a0bb03310570362eef497a09dd6e4ef42f56374965cfb09cc4e055a22a2eeac7ada06053c1bd83abb30c109801844709202208736d598649afe2a53f024b61b3383f";

        // act
        let tx = hex::decode(signed_data).unwrap();
        let decoder = Rlp::new(&tx);
        let decoded = EthereumTransactionCommon::decode(&decoder);

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
            gas_limit: U256::from(1048576),
            to: EthereumAddress::from(
                "4e1b2c985d729ae6e05ef7974013eeb48f394449".to_string(),
            ),
            value: U256::from(1000000000u64),
            data: vec![],
            v: U256::from(38),
            r: string_to_h256_unsafe(
                "bb03310570362eef497a09dd6e4ef42f56374965cfb09cc4e055a22a2eeac7ad",
            ),
            s: string_to_h256_unsafe(
                "6053c1bd83abb30c109801844709202208736d598649afe2a53f024b61b3383f",
            ),
        };

        // assert
        assert_eq!(
            Ok(EthereumAddress::from(
                "d9e5c94a12f78a96640757ac97ba0c257e8aa262".to_string()
            )),
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
            gas_limit: U256::from(1048576),
            to: EthereumAddress::from(
                "4e1b2c985d729ae6e05ef7974013eeb48f394449".to_string(),
            ),
            value: U256::from(1000000000u64),
            data: vec![],
            v: U256::one(),
            r: H256::zero(),
            s: H256::zero(),
        };

        // act
        let signed = transaction
            .sign_transaction(
                "cb9db6b5878db2fa20586e23b7f7b51c22a7c6ed0530daafc2615b116f170cd3"
                    .to_string(),
            )
            .unwrap();

        // assert
        let v = U256::from(38);
        let r = string_to_h256_unsafe(
            "bb03310570362eef497a09dd6e4ef42f56374965cfb09cc4e055a22a2eeac7ad",
        );
        let s = string_to_h256_unsafe(
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
        let (_sk, address) = string_to_sk_and_address(sk.clone()).unwrap();
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
            EthereumAddress::from("9d8a62f656a8d1615c1294fd71e9cfb3e4855a4f".to_string()),
            transaction.caller().unwrap()
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
            gas_limit: U256::from(274722),
            to: EthereumAddress::from(
                "ef1c6e67703c7bd7107eed8303fbe6ec2554bf6b".to_string(),
            ),
            value: U256::from(760460536160301065u64),
            data,
            v: U256::from(37),
            r: string_to_h256_unsafe(
                "c78be9ab81c622c08f7098eefc250935365fb794dfd94aec0fea16c32adec45a",
            ),
            s: string_to_h256_unsafe(
                "5721614264d8490c6866f110c1594151bbcc4fac43758adae644db6bc3314d06",
            ),
        };

        // check
        assert_eq!(
            Ok(EthereumAddress::from(
                "af1276cbb260bb13deddb4209ae99ae6e497f446".to_string()
            )),
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
            gas_limit: U256::from(274722),
            to: EthereumAddress::from(
                "ef1c6e67703c7bd7107eed8303fbe6ec2554bf6b".to_string(),
            ),
            value: U256::from(760460536160301065u64),
            data,
            v: U256::one(),
            r: H256::zero(),
            s: H256::zero(),
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
            gas_limit: U256::from(274722),
            to: EthereumAddress::from(
                "ef1c6e67703c7bd7107eed8303fbe6ec2554bf6b".to_string(),
            ),
            value: U256::from(760460536160301065u64),
            data,
            v: U256::zero(),
            r: H256::zero(),
            s: H256::zero(),
        };

        // act
        let signed = transaction
            .sign_transaction(
                "dcdff53b4f013dbcdc717f89fe3bf4d8b10512aae282b48e01d7530470382701"
                    .to_string(),
            )
            .unwrap();

        // assert
        let v = U256::from(37);
        let r = string_to_h256_unsafe(
            "c78be9ab81c622c08f7098eefc250935365fb794dfd94aec0fea16c32adec45a",
        );
        let s = string_to_h256_unsafe(
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
        let r = string_to_h256_unsafe(
            "6d851632958801b6919ba534b4b1feb1bdfaabd0d42890bce200a11ac735d58d",
        );
        let s = string_to_h256_unsafe(
            "219b058d7169d7a4839c5cdd555b0820b545797365287a81ba409419912de7b1",
        );

        // act
        let tx = hex::decode(signed_tx).unwrap();
        let decoder = Rlp::new(&tx);
        let decoded = EthereumTransactionCommon::decode(&decoder);

        // sanity check
        assert!(decoded.is_ok(), "testing the decoding went ok");
        let decoded_transaction = decoded.unwrap();
        assert_eq!(U256::from(28), decoded_transaction.v, "testing v");
        assert_eq!(r, decoded_transaction.r, "testing r");
        assert_eq!(s, decoded_transaction.s, "testing s");

        // check signature fails gracefully
        assert!(
            decoded_transaction.signature().is_err(),
            "testing signature"
        );
        assert!(decoded_transaction.caller().is_err(), "testing caller");
    }

    #[test]
    fn test_signature_unsigned_fails_gracefully() {
        // most data is not relevant here, the point is to test failure mode of signature verification
        let transaction = EthereumTransactionCommon {
            v: U256::one(), // parity is not consistent with a signed transaction
            ..basic_eip155_transaction()
        };

        // check signature fails gracefully
        assert!(transaction.signature().is_err(), "testing invalid parity");
    }

    #[test]
    fn test_signature_invalid_signature_fails_gracefully() {
        // most data is not relevant here, the point is to test failure mode of signature verification
        let transaction = EthereumTransactionCommon {
            v: U256::from(38), // parity is consistent with a signed transaction
            r: H256::zero(),   // signature value is wrong
            ..basic_eip155_transaction()
        };

        // sanity check
        assert!(
            transaction.signature().is_ok(),
            "testing signature is well formed"
        );

        // check caller fails gracefully
        assert!(
            transaction.caller().is_err(),
            "testing caller fails, because signature is useless"
        );
    }

    #[test]
    fn test_signature_invalid_parity_fails_gracefully() {
        // most data is not relevant here, the point is to test failure mode of signature verification
        let transaction = EthereumTransactionCommon {
            v: U256::from(150), // parity is not consistent with chain_id
            chain_id: U256::one(),
            ..basic_eip155_transaction()
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
