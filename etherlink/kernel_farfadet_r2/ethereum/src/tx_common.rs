// SPDX-FileCopyrightText: 2022-2024 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

//! Signature functions for Ethereum compatibility
//!
//! We need to sign and write Ethereum specific values such
//! as addresses and values.

use std::array::TryFromSliceError;

use libsecp256k1::{recover, Message, RecoveryId, Signature};
use primitive_types::{H160, H256, U256};
use rlp::{Decodable, DecoderError, Encodable, Rlp, RlpIterator, RlpStream};
use sha3::{Digest, Keccak256};
use thiserror::Error;

use crate::{
    access_list::AccessList,
    rlp_helpers::{
        append_compressed_h256, append_option, append_vec, decode_compressed_h256,
        decode_field, decode_list, decode_option, next,
    },
    transaction::TransactionType,
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

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct WrappedSignedAuthorization {
    inner: revm::context::transaction::SignedAuthorization,
}

impl Encodable for WrappedSignedAuthorization {
    fn rlp_append(&self, stream: &mut RlpStream) {
        stream.begin_list(7);
        stream.append(&U256(self.inner.inner().chain_id().into_limbs()));
        let address_h160 = std::convert::TryInto::<[u8; 20]>::try_into(
            self.inner.inner().address().0.as_slice(),
        )
        // Can never fail `Address` is just a wrapper on `FixedBytes<N:20>`
        // and `H160` expects a `[u8; 20]`.
        .unwrap();
        stream.append(&H160(address_h160));
        stream.append(&self.inner.inner().nonce());
        stream.append(&self.inner.y_parity());
        stream.append(&U256(self.inner.r().into_limbs()));
        stream.append(&U256(self.inner.s().into_limbs()));
    }
}

fn revm_u256(value: &U256) -> Option<revm::primitives::U256> {
    let mut bytes = vec![0; 32];
    value.to_little_endian(&mut bytes);
    Some(revm::primitives::U256::from_le_bytes::<32>(
        bytes.try_into().ok()?,
    ))
}

impl Decodable for WrappedSignedAuthorization {
    fn decode(rlp: &Rlp) -> Result<Self, DecoderError> {
        if !rlp.is_list() {
            Err(DecoderError::RlpExpectedToBeList)
        } else {
            let mut it = rlp.iter();
            let chain_id: U256 = decode_field(&next(&mut it)?, "chain_id")?;
            let address: H160 = decode_field(&next(&mut it)?, "address")?;
            let nonce: u64 = decode_field(&next(&mut it)?, "nonce")?;
            let y_parity: u8 = decode_field(&next(&mut it)?, "y_parity")?;
            let r: U256 = decode_field(&next(&mut it)?, "r")?;
            let s: U256 = decode_field(&next(&mut it)?, "s")?;
            if it.next().is_some() {
                return Err(DecoderError::RlpIncorrectListLen);
            }
            Ok(Self {
                inner: revm::context::transaction::SignedAuthorization::new_unchecked(
                    revm::context::transaction::Authorization {
                        chain_id: revm_u256(&chain_id).unwrap_or_default(),
                        address: revm::primitives::Address::from(
                            address.as_fixed_bytes(),
                        ),
                        nonce,
                    },
                    y_parity,
                    revm_u256(&r).unwrap_or_default(),
                    revm_u256(&s).unwrap_or_default(),
                ),
            })
        }
    }
}

pub type AuthorizationList = Vec<WrappedSignedAuthorization>;

#[inline]
pub fn signed_authorization(
    authorization_list: AuthorizationList,
) -> Vec<revm::context::transaction::SignedAuthorization> {
    authorization_list
        .into_iter()
        .map(|s_a| s_a.inner)
        .collect()
}

/// Data common for all kind of Ethereum transactions
/// (transfers, contract creation and contract invocation).
/// All transaction versions (Legacy, EIP-2930 and EIP-1559)
/// are parsed to this common type.
/// This type is common for both signed and unsigned transactions as well.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct EthereumTransactionCommon {
    pub type_: TransactionType,
    /// the id of the chain
    /// see `<https://chainlist.org/>` for values
    /// None if the signature doesn't contain the chain id (pre EIP-155).
    pub chain_id: Option<U256>,
    /// A scalar value equal to the number of transactions sent by the sender
    pub nonce: u64,

    /// Normally, this would be a fee paid per gas in addition to base fee per gas.
    /// This would incentivise miners to include the transaction.
    /// More details see here https://eips.ethereum.org/EIPS/eip-1559#abstract
    ///
    /// We choose to ignore this, however, as we actually do not implement eip-1559
    /// mechanism exactly. The sequencer is compensated via the data availability fee.
    ///
    /// We keep this field purely for compatibility with existing ethereum tooling.
    max_priority_fee_per_gas: U256,
    /// Maximum amount of fee to be paid per gas.
    /// Thus, as a transaction might be included in the block
    /// with higher base_fee_per_gas then one
    /// at the moment of the creation of tx,
    /// this value is protection for user that
    /// they won't pay for a tx more than they wanted to pay.
    /// Given this cap, effective priority fee will be equal to
    /// min(max_priority_fee_per_gas, max_fee_per_gas - base_fee_per_gas).
    /// More details see here https://eips.ethereum.org/EIPS/eip-1559#abstract
    pub max_fee_per_gas: U256,

    /// The maximum amount of gas that the user is willing to pay.
    ///
    /// *NB* this is inclusive of any additional fees that are paid, prior to execution:
    /// - data availability fee
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
    /// Access list specifies a list of addresses and storage keys,
    /// which are going to be accessed during transaction execution.
    /// For more information see https://eips.ethereum.org/EIPS/eip-2930
    pub access_list: AccessList,
    /// Authorization list specifies a list of signed authorization tuples,
    /// enabling externally owned accounts (EOAs) to delegate execution context
    /// to designated smart contract code.
    /// For more information see https://eips.ethereum.org/EIPS/eip-7702
    /// Will alwasy be None for non EIP-7702 transactions.
    pub authorization_list: Option<AuthorizationList>,
    /// If transaction is unsigned then this field is None
    /// See encoding details in <https://github.com/ethereum/EIPs/blob/master/EIPS/eip-155.md>
    pub signature: Option<TxSignature>,
}

impl EthereumTransactionCommon {
    #[allow(clippy::too_many_arguments)]
    pub fn new(
        type_: TransactionType,
        chain_id: Option<U256>,
        nonce: u64,
        max_priority_fee_per_gas: U256,
        max_fee_per_gas: U256,
        gas_limit: u64,
        to: Option<H160>,
        value: U256,
        data: Vec<u8>,
        access_list: AccessList,
        authorization_list: Option<AuthorizationList>,
        signature: Option<TxSignature>,
    ) -> Self {
        Self {
            type_,
            chain_id,
            nonce,
            max_priority_fee_per_gas,
            max_fee_per_gas,
            gas_limit,
            to,
            value,
            data,
            access_list,
            authorization_list,
            signature,
        }
    }

    // This decoding function encapsulates logic of decoding (v, r, s).
    // There might be 3 possible cases:
    // - unsigned NON-legacy: (0, 0, 0)
    // - signed NON-legacy: (v, r, s), r > 0 && s > 0
    // - unsigned legacy: none of coordinates present
    fn rlp_decode_vrs(
        it: &mut RlpIterator,
    ) -> Result<Option<(U256, H256, H256)>, DecoderError> {
        let v = it.next();
        let r = it.next();
        let s = it.next();
        match (v, r, s) {
            // It might be either signed NON-legacy tx case or usigned legacy with all zeros
            (Some(v), Some(r), Some(s)) => {
                let v: U256 = decode_field(&v, "v")?;
                let r: H256 = decode_compressed_h256(&r)?;
                let s: H256 = decode_compressed_h256(&s)?;
                Ok(Some((v, r, s)))
            }
            // It might be unsigned NON-legacy tx case
            (None, None, None) => Ok(None),
            // Malformed signature: none of the cases is applicable
            _ => Err(DecoderError::Custom(
                "Invalid transaction encoding: neither signed nor unsigned tx",
            )),
        }
    }

    // RLP decoding of a Legacy transaction.
    fn rlp_decode_legacy_tx(decoder: &Rlp) -> Result<Self, DecoderError> {
        // If we don't have 9 elements, then list has incorrect length
        if decoder.item_count() != Ok(9) {
            return Err(DecoderError::RlpIncorrectListLen);
        }

        let mut it = decoder.iter();
        let nonce: u64 = decode_field(&next(&mut it)?, "nonce")?;
        let gas_price: U256 = decode_field(&next(&mut it)?, "gas_price")?;
        let gas_limit: u64 = decode_field(&next(&mut it)?, "gas_limit")?;
        let to: Option<H160> = decode_option(&next(&mut it)?, "to")?;
        let value: U256 = decode_field(&next(&mut it)?, "value")?;
        let data: Vec<u8> = decode_field(&next(&mut it)?, "data")?;

        // Decode v, r, s of signatures
        let vrs = Self::rlp_decode_vrs(&mut it)?;
        // In case of legacy tx: both signed and unsigned tx has 9 elements,
        // hence all v, r, and s should be presented
        let (v, r, s) =
                vrs.ok_or(
                    DecoderError::Custom("Invalid legacy tx encoding: legacy tx has to have 9 elements in the RLP list"))?;
        let is_unsigned = r == H256::zero() && s == H256::zero();

        // Derive chain_id from v of the signature
        let chain_id = if is_unsigned {
            // in a rlp encoded unsigned eip-155 transaction, v is used to store the chainid
            Ok(Some(v))
        } else {
            // in a rlp encoded signed eip-155 transaction, v is {0,1} + CHAIN_ID * 2 + 35
            // v > 36 is because we support only chain_id which is strictly greater than 0
            if v > U256::from(36) {
                Ok(Some((v - 35) / 2))
            // signatures pre EIP-155 don't encode the chain id in the parity of
            // the signature, as such `v` is either 27 or 28.
            } else if v == U256::from(27) || v == U256::from(28) {
                Ok(None)
            } else {
                Err(DecoderError::Custom(
                    "v has to be greater than 36 for a signed EIP-155 transaction",
                ))
            }
        }?;

        // Set signature
        let signature = if is_unsigned {
            None
        } else {
            Some(
                TxSignature::new(v, r, s)
                    .map_err(|_| DecoderError::Custom("Invalid signature"))?,
            )
        };
        Ok(EthereumTransactionCommon {
            type_: TransactionType::Legacy,
            chain_id,
            nonce,
            max_fee_per_gas: gas_price,
            max_priority_fee_per_gas: gas_price,
            gas_limit,
            to,
            value,
            data,
            access_list: vec![],
            authorization_list: None,
            signature,
        })
    }

    // RLP decoding of an EIP-2930 transaction.
    fn rlp_decode_eip2930_tx(decoder: &Rlp) -> Result<Self, DecoderError> {
        // It's either:
        // - 8 fields fields for an unsigned tx
        // - 11 fields for a signed tx
        if decoder.item_count() != Ok(8) && decoder.item_count() != Ok(11) {
            return Err(DecoderError::RlpIncorrectListLen);
        }

        let mut it = decoder.iter();
        let chain_id: U256 = decode_field(&next(&mut it)?, "chain_id")?;
        let nonce: u64 = decode_field(&next(&mut it)?, "nonce")?;
        let gas_price: U256 = decode_field(&next(&mut it)?, "gas_price")?;
        let gas_limit: u64 = decode_field(&next(&mut it)?, "gas_limit")?;
        let to: Option<H160> = decode_option(&next(&mut it)?, "to")?;
        let value: U256 = decode_field(&next(&mut it)?, "value")?;
        let data: Vec<u8> = decode_field(&next(&mut it)?, "data")?;
        let access_list = decode_list(&next(&mut it)?, "access_list")?;
        // Decode a signature if it exists
        let vrs = Self::rlp_decode_vrs(&mut it)?;
        let signature = match vrs {
            Some((v, r, s)) => TxSignature::new(v, r, s)
                .map(Option::Some)
                .map_err(|_| DecoderError::Custom("Invalid signature")),
            None => Ok(None),
        }?;

        Ok(EthereumTransactionCommon {
            type_: TransactionType::Eip2930,
            chain_id: Some(chain_id),
            nonce,
            max_fee_per_gas: gas_price,
            max_priority_fee_per_gas: gas_price,
            gas_limit,
            to,
            value,
            data,
            access_list,
            authorization_list: None,
            signature,
        })
    }

    // RLP decoding of an EIP-1559 transaction.
    fn rlp_decode_eip1559_tx(decoder: &Rlp) -> Result<Self, DecoderError> {
        // It's either:
        // - 9 fields fields for an unsigned tx
        // - 12 fields for a signed tx
        if decoder.item_count() != Ok(9) && decoder.item_count() != Ok(12) {
            return Err(DecoderError::RlpIncorrectListLen);
        }

        let mut it = decoder.iter();
        let chain_id: U256 = decode_field(&next(&mut it)?, "chain_id")?;
        let nonce: u64 = decode_field(&next(&mut it)?, "nonce")?;
        let max_priority_fee_per_gas =
            decode_field(&next(&mut it)?, "max_priority_fee_per_gas")?;
        let max_fee_per_gas = decode_field(&next(&mut it)?, "max_fee_per_gas")?;
        let gas_limit: u64 = decode_field(&next(&mut it)?, "gas_limit")?;
        let to: Option<H160> = decode_option(&next(&mut it)?, "to")?;
        let value: U256 = decode_field(&next(&mut it)?, "value")?;
        let data: Vec<u8> = decode_field(&next(&mut it)?, "data")?;
        let access_list: AccessList = decode_list(&next(&mut it)?, "access_list")?;

        let vrs = Self::rlp_decode_vrs(&mut it)?;
        let signature = match vrs {
            Some((v, r, s)) => TxSignature::new(v, r, s)
                .map(Option::Some)
                .map_err(|_| DecoderError::Custom("Invalid signature")),
            None => Ok(None),
        }?;
        Ok(EthereumTransactionCommon {
            type_: TransactionType::Eip1559,
            chain_id: Some(chain_id),
            nonce,
            max_priority_fee_per_gas,
            max_fee_per_gas,
            gas_limit,
            to,
            value,
            data,
            access_list,
            authorization_list: None,
            signature,
        })
    }

    // RLP decoding of an EIP-7702 transaction.
    fn rlp_decode_eip7702_tx(decoder: &Rlp) -> Result<Self, DecoderError> {
        // It's either:
        // - 10 fields fields for an unsigned tx
        // - 13 fields for a signed tx
        if decoder.item_count() != Ok(10) && decoder.item_count() != Ok(13) {
            return Err(DecoderError::RlpIncorrectListLen);
        }

        let mut it = decoder.iter();
        let chain_id: U256 = decode_field(&next(&mut it)?, "chain_id")?;
        let nonce: u64 = decode_field(&next(&mut it)?, "nonce")?;
        let max_priority_fee_per_gas =
            decode_field(&next(&mut it)?, "max_priority_fee_per_gas")?;
        let max_fee_per_gas = decode_field(&next(&mut it)?, "max_fee_per_gas")?;
        let gas_limit: u64 = decode_field(&next(&mut it)?, "gas_limit")?;
        let to: Option<H160> = decode_option(&next(&mut it)?, "to")?;
        let value: U256 = decode_field(&next(&mut it)?, "value")?;
        let data: Vec<u8> = decode_field(&next(&mut it)?, "data")?;
        let access_list: AccessList = decode_list(&next(&mut it)?, "access_list")?;
        let authorization_list: AuthorizationList =
            decode_list(&next(&mut it)?, "authorization_list")?;

        let vrs = Self::rlp_decode_vrs(&mut it)?;
        let signature = match vrs {
            Some((v, r, s)) => TxSignature::new(v, r, s)
                .map(Option::Some)
                .map_err(|_| DecoderError::Custom("Invalid signature")),
            None => Ok(None),
        }?;
        Ok(EthereumTransactionCommon {
            type_: TransactionType::Eip7702,
            chain_id: Some(chain_id),
            nonce,
            max_priority_fee_per_gas,
            max_fee_per_gas,
            gas_limit,
            to,
            value,
            data,
            access_list,
            authorization_list: Some(authorization_list),
            signature,
        })
    }

    fn from_rlp_any(
        decoder: &Rlp,
        tx_version: TransactionType,
    ) -> Result<Self, DecoderError> {
        if !decoder.is_list() {
            return Err(DecoderError::RlpExpectedToBeList);
        }
        let tx = match tx_version {
            TransactionType::Legacy => Self::rlp_decode_legacy_tx(decoder),
            TransactionType::Eip2930 => Self::rlp_decode_eip2930_tx(decoder),
            TransactionType::Eip1559 => Self::rlp_decode_eip1559_tx(decoder),
            TransactionType::Eip7702 => Self::rlp_decode_eip7702_tx(decoder),
        }?;
        Ok(tx)
    }

    /// Encodes a transaction as before EIP-155, i.e. without the chain_id
    fn rlp_encode_legacy_pre_eip155(&self, stream: &mut RlpStream) {
        if self.signature.is_some() {
            // If there is a signature, there will be 9 fields
            stream.begin_list(9);
        } else {
            // Otherwise, there won't be signature
            stream.begin_list(6);
        }

        stream.append(&self.nonce);
        // self.max_fee_per_gas has to be equal to gas_price
        stream.append(&self.max_fee_per_gas);
        stream.append(&self.gas_limit);
        append_option(stream, &self.to);
        stream.append(&self.value);
        append_vec(stream, &self.data);
        if let Some(sig) = &self.signature {
            sig.rlp_append(stream)
        }
    }

    fn rlp_encode_legacy_post_eip155(&self, chain_id: U256, stream: &mut RlpStream) {
        stream.begin_list(9);
        stream.append(&self.nonce);
        // self.max_fee_per_gas has to be equal to gas_price
        stream.append(&self.max_fee_per_gas);
        stream.append(&self.gas_limit);
        append_option(stream, &self.to);
        stream.append(&self.value);
        append_vec(stream, &self.data);
        match &self.signature {
            None => {
                // In case of unsigned legacy tx we have to append chain_id as v component of (v, r, s)
                stream.append(&chain_id);
                append_compressed_h256(stream, H256::zero());
                append_compressed_h256(stream, H256::zero());
            }
            Some(sig) => sig.rlp_append(stream),
        }
    }
    fn rlp_encode_legacy_tx(&self, stream: &mut RlpStream) {
        match self.chain_id {
            Some(chain_id) => self.rlp_encode_legacy_post_eip155(chain_id, stream),
            None => self.rlp_encode_legacy_pre_eip155(stream),
        }
    }

    fn rlp_encode_eip2930_tx(&self, stream: &mut RlpStream) {
        if self.signature.is_some() {
            // If there is a signature, there will be 11 fields
            stream.begin_list(11);
        } else {
            // Otherwise, there won't be signature
            stream.begin_list(8);
        }
        // In that case the chain id is mandatory, as such this unwrapping is safe
        stream.append(&self.chain_id.unwrap());
        stream.append(&self.nonce);
        // self.max_fee_per_gas has to be equal to gas_price
        stream.append(&self.max_fee_per_gas);
        stream.append(&self.gas_limit);
        append_option(stream, &self.to);
        stream.append(&self.value);
        append_vec(stream, &self.data);
        stream.append_list(&self.access_list);

        // If tx is NOT legacy and unsigned: DON'T append anything like (0, 0, 0)
        if let Some(sig) = &self.signature {
            sig.rlp_append(stream)
        }
    }

    fn rlp_encode_eip1559_tx(&self, stream: &mut RlpStream) {
        if self.signature.is_some() {
            // If there is a signature, there will be 12 fields
            stream.begin_list(12);
        } else {
            // Otherwise, there won't be signature
            stream.begin_list(9);
        }

        // In that case the chain id is mandatory, as such this unwrapping is safe
        stream.append(&self.chain_id.unwrap());
        stream.append(&self.nonce);
        stream.append(&self.max_priority_fee_per_gas);
        stream.append(&self.max_fee_per_gas);
        stream.append(&self.gas_limit);
        append_option(stream, &self.to);
        stream.append(&self.value);
        append_vec(stream, &self.data);
        stream.append_list(&self.access_list);

        // If tx is NOT legacy and unsigned: DON'T append anything like (0, 0, 0)
        if let Some(sig) = &self.signature {
            sig.rlp_append(stream)
        }
    }

    fn rlp_encode_eip7702_tx(&self, stream: &mut RlpStream) {
        if self.signature.is_some() {
            // If there is a signature, there will be 13 fields
            stream.begin_list(13);
        } else {
            // Otherwise, there won't be signature
            stream.begin_list(10);
        }

        // In that case the chain id is mandatory, as such this unwrapping is safe
        stream.append(&self.chain_id.unwrap());
        stream.append(&self.nonce);
        stream.append(&self.max_priority_fee_per_gas);
        stream.append(&self.max_fee_per_gas);
        stream.append(&self.gas_limit);
        append_option(stream, &self.to);
        stream.append(&self.value);
        append_vec(stream, &self.data);
        stream.append_list(&self.access_list);
        stream.append_list(self.authorization_list.as_deref().unwrap_or_default());

        // If tx is NOT legacy and unsigned: DON'T append anything like (0, 0, 0)
        if let Some(sig) = &self.signature {
            sig.rlp_append(stream)
        }
    }

    fn to_rlp_any(self: &EthereumTransactionCommon, stream: &mut RlpStream) {
        match &self.type_ {
            TransactionType::Legacy => self.rlp_encode_legacy_tx(stream),
            TransactionType::Eip2930 => self.rlp_encode_eip2930_tx(stream),
            TransactionType::Eip1559 => self.rlp_encode_eip1559_tx(stream),
            TransactionType::Eip7702 => self.rlp_encode_eip7702_tx(stream),
        }
    }

    /// Extracts the Keccak encoding of a message from an EthereumTransactionCommon
    #[cfg_attr(feature = "benchmark", inline(never))]
    fn message(&self) -> Message {
        let to_sign = EthereumTransactionCommon {
            signature: None,
            ..self.clone()
        };

        let hash: [u8; 32] = Keccak256::digest(to_sign.to_bytes()).into();
        Message::parse(&hash)
    }

    /// Extracts the signature from an EthereumTransactionCommon
    pub fn signature(&self) -> Result<(Signature, RecoveryId), SigError> {
        let tx_signature = self
            .signature
            .as_ref()
            .ok_or(SigError::UnsignedTransactionError)?;
        match self.type_ {
            TransactionType::Legacy => tx_signature
                .signature_legacy(self.chain_id)
                .map_err(SigError::TxSigError),
            _ => tx_signature.signature().map_err(SigError::TxSigError),
        }
    }

    /// Find the caller address from r and s of the common data
    /// for an Ethereum transaction, ie, what address is associated
    /// with the signature of the message.
    // TODO <https://gitlab.com/tezos/tezos/-/milestones/115>
    // DO NOT RENAME: function name is used during benchmark
    // Never inlined when the kernel is compiled for benchmarks, to ensure the
    // function is visible in the profiling results.
    #[cfg_attr(feature = "benchmark", inline(never))]
    pub fn caller(&self) -> Result<H160, SigError> {
        let mes = self.message();
        let (sig, ri) = self.signature()?;
        let pk = recover(&mes, &sig, &ri).map_err(TxSigError::ECDSAError)?;
        let serialised = &pk.serialize()[1..];
        let kec = Keccak256::digest(serialised);
        let value: [u8; 20] = kec.as_slice()[12..].try_into()?;

        Ok(value.into())
    }

    /// Produce a signed EthereumTransactionCommon. If the initial one was signed
    ///  you should get the same thing.
    pub fn sign_transaction(&self, string_sk: String) -> Result<Self, SigError> {
        let mes = self.message();
        let signature = match self.type_ {
            TransactionType::Legacy => {
                TxSignature::sign_legacy(&mes, string_sk, self.chain_id)?
            }
            _ => TxSignature::sign(&mes, string_sk)?,
        };

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
        let first = *bytes.first().ok_or(DecoderError::Custom("Empty bytes"))?;
        if first == 0x01 {
            let decoder = Rlp::new(&bytes[1..]);
            Self::from_rlp_any(&decoder, TransactionType::Eip2930)
        } else if first == 0x02 {
            let decoder = Rlp::new(&bytes[1..]);
            Self::from_rlp_any(&decoder, TransactionType::Eip1559)
        } else if first == 0x04 {
            let decoder = Rlp::new(&bytes[1..]);
            Self::from_rlp_any(&decoder, TransactionType::Eip7702)
        } else {
            let decoder = Rlp::new(bytes);
            Self::from_rlp_any(&decoder, TransactionType::Legacy)
        }
    }

    /// Unserialize an hex string
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
    pub fn to_bytes(&self) -> Vec<u8> {
        let mut stream = RlpStream::new();
        self.to_rlp_any(&mut stream);
        let mut rlp_enc = stream.out().to_vec();
        match self.type_ {
            TransactionType::Legacy => rlp_enc,
            TransactionType::Eip2930
            | TransactionType::Eip1559
            | TransactionType::Eip7702 => {
                let tag = From::from(self.type_);
                rlp_enc.insert(0, tag);
                rlp_enc
            }
        }
    }

    /// Returns the total gas limit for this transaction, including execution and fees.
    #[inline(always)]
    pub const fn gas_limit_with_fees(&self) -> u64 {
        self.gas_limit
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

impl Encodable for EthereumTransactionCommon {
    fn rlp_append(&self, stream: &mut rlp::RlpStream) {
        let eth_bytes = self.to_bytes();
        stream.encoder().encode_value(&eth_bytes)
    }
}

impl Decodable for EthereumTransactionCommon {
    fn decode(decoder: &rlp::Rlp) -> Result<Self, DecoderError> {
        let bytes: Vec<u8> = decoder.as_val()?;
        EthereumTransactionCommon::from_bytes(&bytes)
    }
}

// cargo test ethereum::signatures::test --features testing
#[cfg(test)]
mod test {

    use std::{ops::Neg, str::FromStr};

    use libsecp256k1::curve::Scalar;

    use crate::access_list::AccessListItem;

    use crate::tx_signature::{h256_to_scalar, TxSignature};

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
            type_: TransactionType::Legacy,
            chain_id: Some(U256::one()),
            nonce: 9,
            max_fee_per_gas: U256::from(20000000000u64),
            max_priority_fee_per_gas: U256::from(20000000000u64),
            gas_limit: 21000u64,
            to: address_from_str("3535353535353535353535353535353535353535"),
            value: U256::from(1000000000000000000u64),
            data: vec![],
            access_list: vec![],
            authorization_list: None,
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

    fn eip2930_tx() -> EthereumTransactionCommon {
        EthereumTransactionCommon {
            type_: TransactionType::Eip2930,
            chain_id: Some(U256::from(1900)),
            nonce: 34,
            max_fee_per_gas: U256::from(1000000000u64),
            max_priority_fee_per_gas: U256::from(1000000000u64),
            gas_limit: 100000,
            to: address_from_str("09616C3d61b3331fc4109a9E41a8BDB7d9776609"),
            value: U256::from(0x5af3107a4000_u64),
            data: hex::decode("616263646566").unwrap(),
            access_list: vec![AccessListItem {
                address: address_from_str("0000000000000000000000000000000000000001")
                    .unwrap(),
                storage_keys: vec![H256::from_str(
                    "0100000000000000000000000000000000000000000000000000000000000000",
                )
                .unwrap()],
            }],
            authorization_list: None,
            signature: Some(TxSignature::new_unsafe(
                0,
                string_to_h256_unsafe(
                    "ea38506c4afe4bb402e030877fbe1011fa1da47aabcf215db8da8fee5d3af086",
                ),
                string_to_h256_unsafe(
                    "51e9af653b8eb98e74e894a766cf88904dbdb10b0bc1fbd12f18f661fa2797a4",
                ),
            )),
        }
    }

    fn eip1559_tx() -> EthereumTransactionCommon {
        EthereumTransactionCommon {
            type_: TransactionType::Eip1559,
            chain_id: Some( U256::one()),
            nonce: 4142,
            max_priority_fee_per_gas: U256::from(1000000000),
            max_fee_per_gas: U256::from(28750000000_i64),
            gas_limit: 37262,
            to: address_from_str("9f8f72aa9304c8b593d555f12ef6589cc3a579a2"),
            value: U256::from(0),
            data: hex::decode("a9059cbb000000000000000000000000a9d1e08c7793af67e9d92fe308d5697fb81d3e43000000000000000000000000000000000000000000000000f020482e89b73c14").unwrap(),
            access_list: vec![],
            authorization_list: None,
            signature: Some(TxSignature::new_unsafe(
                0,
                string_to_h256_unsafe(
                    "f876cca0898a15f2eaf17ee8a0ac33ec7933b17db612bd5aaf5925774da84fad",
                ),
                string_to_h256_unsafe(
                    "5c7310fa69b650d583fa5a66fbcfd720e96d9eacadc18632595cb6423e2f613f",
                ),
            )),
        }
    }

    fn h256_to_string(e: H256) -> String {
        format!("{e:x}")
    }

    /// used in test to decode a string and get the size of the decoded input,
    /// before determining the H256 value
    fn decode_compressed_h256_helper(str: &str) -> (Result<H256, DecoderError>, usize) {
        let hash = hex::decode(str).unwrap();
        let decoder = Rlp::new(&hash);
        let decoded = decode_compressed_h256(&decoder);
        assert!(decoded.is_ok(), "hash should be decoded ok");
        let length = decoder.data().unwrap().len();
        (decoded, length)
    }

    #[test]
    fn test_decode_compressed_h256_l0() {
        // rlp encoding of empty is the byte 80
        let (decoded, length) = decode_compressed_h256_helper("80");
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
        let (decoded, length) = decode_compressed_h256_helper(
            "a03232323232323232323232323232323232323232323232323232323232323232",
        );
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
        let (decoded, length) = decode_compressed_h256_helper(
            "9f31313131313131313131313131313131313131313131313131313131313131",
        );
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
        let nonce = 46;
        let gas_price = U256::from(29075052730u64);
        let gas_limit = 274722u64;
        let to = None;
        let value = U256::from(1000000000u64);
        let data: Vec<u8> = hex::decode("ffff").unwrap();
        let chain_id = Some(U256::one());
        let r = string_to_h256_unsafe(
            "e9637495be4c216a833ef390b1f6798917c8a102ab165c5085cced7ca1f2eb3a",
        );
        let s = string_to_h256_unsafe(
            "57854e7044a8fee7bccb6a2c32c4229dd9cbacad74350789e0ce75bf40b6f713",
        );
        EthereumTransactionCommon {
            type_: TransactionType::Legacy,
            chain_id,
            nonce,
            max_priority_fee_per_gas: gas_price,
            max_fee_per_gas: gas_price,
            gas_limit,
            to,
            value,
            data,
            access_list: vec![],
            authorization_list: None,
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
        let nonce = 0;
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
            type_: TransactionType::Legacy,
            chain_id: Some(U256::one()),
            nonce,
            max_priority_fee_per_gas: gas_price,
            max_fee_per_gas: gas_price,
            gas_limit,
            to,
            value,
            data,
            access_list: vec![],
            authorization_list: None,
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
        let nonce = 46;
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
            type_: TransactionType::Legacy,
            chain_id: Some(U256::one()),
            nonce,
            max_priority_fee_per_gas: gas_price,
            max_fee_per_gas: gas_price,
            gas_limit,
            to,
            value,
            data,
            access_list: vec![],
            authorization_list: None,
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
        let nonce = 46;
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
            type_: TransactionType::Legacy,
            chain_id: Some(U256::one()),
            nonce,
            max_priority_fee_per_gas: gas_price,
            max_fee_per_gas: gas_price,
            gas_limit,
            to,
            value,
            data,
            access_list: vec![],
            authorization_list: None,

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
        let gas_price = U256::from(30000000000u64);
        let expected_transaction = EthereumTransactionCommon {
            type_: TransactionType::Legacy,
            chain_id: Some(U256::one()),
            nonce: 1,
            max_priority_fee_per_gas: gas_price,
            max_fee_per_gas: gas_price,
            gas_limit: 1048576u64,
            to: address_from_str("4e1b2c985d729ae6e05ef7974013eeb48f394449"),
            value: U256::from(1000000000u64),
            data: vec![],
            access_list: vec![],
            authorization_list: None,

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

        let gas_price = U256::from(30000000000u64);
        let transaction = EthereumTransactionCommon {
            type_: TransactionType::Legacy,
            chain_id: Some(U256::one()),
            nonce: 1,
            max_priority_fee_per_gas: gas_price,
            max_fee_per_gas: gas_price,
            gas_limit: 1048576u64,
            to: address_from_str("4e1b2c985d729ae6e05ef7974013eeb48f394449"),
            value: U256::from(1000000000u64),
            data: vec![],
            access_list: vec![],
            authorization_list: None,

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

        let gas_price = U256::from(30000000000u64);
        // setup
        let transaction = EthereumTransactionCommon {
            type_: TransactionType::Legacy,
            chain_id: Some(U256::one()),
            nonce: 1,
            max_priority_fee_per_gas: gas_price,
            max_fee_per_gas: gas_price,
            gas_limit: 1048576u64,
            to: address_from_str("4e1b2c985d729ae6e05ef7974013eeb48f394449"),
            value: U256::from(1000000000u64),
            data: vec![],
            access_list: vec![],
            authorization_list: None,

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

        let gas_price = U256::from(29075052730u64);
        let transaction = EthereumTransactionCommon {
            type_: TransactionType::Legacy,
            chain_id: Some(U256::one()),
            nonce: 46,
            max_priority_fee_per_gas: gas_price,
            max_fee_per_gas: gas_price,
            gas_limit: 274722u64,
            to: address_from_str("ef1c6e67703c7bd7107eed8303fbe6ec2554bf6b"),
            value: U256::from(760460536160301065u64),
            data,
            access_list: vec![],
            authorization_list: None,

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
        let gas_price = U256::from(29075052730u64);
        let transaction = EthereumTransactionCommon {
            type_: TransactionType::Legacy,
            chain_id: Some(U256::one()),
            nonce: 46,
            max_priority_fee_per_gas: gas_price,
            max_fee_per_gas: gas_price,
            gas_limit: 274722u64,
            to: address_from_str("ef1c6e67703c7bd7107eed8303fbe6ec2554bf6b"),
            value: U256::from(760460536160301065u64),
            data,
            access_list: vec![],
            authorization_list: None,

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
        let gas_price = U256::from(29075052730u64);
        let transaction = EthereumTransactionCommon {
            type_: TransactionType::Legacy,
            chain_id: Some(U256::one()),
            nonce: 46,
            max_priority_fee_per_gas: gas_price,
            max_fee_per_gas: gas_price,
            gas_limit: 274722u64,
            to: address_from_str("ef1c6e67703c7bd7107eed8303fbe6ec2554bf6b"),
            value: U256::from(760460536160301065u64),
            data,
            access_list: vec![],
            authorization_list: None,

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
    fn test_rlp_decode_succeeds_without_chain_id() {
        // This transaction is signed but its v doesn't equal to CHAIN_ID * 2 + 35 + {0, 1}
        // but equal to 27/28 as in "old" (before https://eips.ethereum.org/EIPS/eip-155)
        // six fields encoding
        let malformed_tx = "f86c0a8502540be400825208944bbeeb066ed09b7aed07bf39eee0460dfa261520880de0b6b3a7640000801ca0f3ae52c1ef3300f44df0bcfd1341c232ed6134672b16e35699ae3f5fe2493379a023d23d2955a239dd6f61c4e8b2678d174356ff424eac53da53e17706c43ef871".to_string();
        let e = EthereumTransactionCommon::from_hex(malformed_tx);
        assert!(e.is_ok());
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
    fn test_roundtrip_pre_eip_155() {
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
        let decoded = EthereumTransactionCommon::from_bytes(&tx).unwrap();
        let encoded = EthereumTransactionCommon::to_bytes(&decoded);

        // sanity check
        assert_eq!(hex::encode(encoded), signed_tx);
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
            chain_id: Some(U256::one()),
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
            chain_id: Some(U256::max_value()), // chain_id will overflow parity computation
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
            chain_id: Some(U256::max_value()),
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

    #[test]
    fn test_eip2930_signed_enc_dec() {
        let signed_tx = "01f8ad82076c22843b9aca00830186a09409616c3d61b3331fc4109a9e41a8bdb7d9776609865af3107a400086616263646566f838f7940000000000000000000000000000000000000001e1a0010000000000000000000000000000000000000000000000000000000000000080a0ea38506c4afe4bb402e030877fbe1011fa1da47aabcf215db8da8fee5d3af086a051e9af653b8eb98e74e894a766cf88904dbdb10b0bc1fbd12f18f661fa2797a4".to_string();
        let parsed = EthereumTransactionCommon::from_hex(signed_tx.clone()).unwrap();
        let expected = eip2930_tx();
        assert_eq!(expected, parsed);

        assert_eq!(signed_tx, hex::encode(parsed.to_bytes()));
    }

    #[test]
    fn test_eip2930_unsigned_enc_dec() {
        let unsigned_tx = EthereumTransactionCommon {
            signature: None,
            ..eip2930_tx()
        };
        let tx_encoding = "01f86a82076c22843b9aca00830186a09409616c3d61b3331fc4109a9e41a8bdb7d9776609865af3107a400086616263646566f838f7940000000000000000000000000000000000000001e1a00100000000000000000000000000000000000000000000000000000000000000";
        assert_eq!(tx_encoding, hex::encode(unsigned_tx.to_bytes()));
        assert_eq!(
            unsigned_tx,
            EthereumTransactionCommon::from_bytes(&hex::decode(tx_encoding).unwrap())
                .unwrap()
        );
    }

    #[test]
    fn test_different_tx_signature_and_sign() {
        // Test that signature() and sign_transaction() work as expected
        // for both legacy and non-legacy txs.

        fn sign_signature_roundtrip(
            tx: EthereumTransactionCommon,
            string_sk: String,
            expected_r: &str,
            expected_s: &str,
            parity: u8,
        ) {
            let expected_signature = (
                Signature {
                    r: h256_to_scalar(string_to_h256_unsafe(expected_r)),
                    s: h256_to_scalar(string_to_h256_unsafe(expected_s)),
                },
                RecoveryId::parse(parity).unwrap(),
            );

            // Check that parity recovered correctly
            assert_eq!(Ok(expected_signature), tx.signature());

            // Check that signature computed correctly
            assert_eq!(
                Ok(expected_signature),
                tx.sign_transaction(string_sk).unwrap().signature()
            );
        }

        // EIP-155 tx
        let legacy_tx = basic_eip155_transaction();
        sign_signature_roundtrip(
            legacy_tx,
            "4646464646464646464646464646464646464646464646464646464646464646".to_owned(),
            "28EF61340BD939BC2195FE537567866003E1A15D3C71FF63E1590620AA636276",
            "67CBE9D8997F761AECB703304B3800CCF555C9F3DC64214B297FB1966A3B6D83",
            0,
        );

        // EIP-2930 tx
        let eip2930 = eip2930_tx();
        sign_signature_roundtrip(
            eip2930,
            "4c0883a69102937d6231471b5dbb6204fe5129617082792ae468d01a3f362318".to_owned(),
            "ea38506c4afe4bb402e030877fbe1011fa1da47aabcf215db8da8fee5d3af086",
            "51e9af653b8eb98e74e894a766cf88904dbdb10b0bc1fbd12f18f661fa2797a4",
            0,
        );
    }

    #[test]
    fn test_eip1559_enc_dec() {
        let signed_tx = "02f8b20182102e843b9aca008506b1a22f8082918e949f8f72aa9304c8b593d555f12ef6589cc3a579a280b844a9059cbb000000000000000000000000a9d1e08c7793af67e9d92fe308d5697fb81d3e43000000000000000000000000000000000000000000000000f020482e89b73c14c080a0f876cca0898a15f2eaf17ee8a0ac33ec7933b17db612bd5aaf5925774da84fada05c7310fa69b650d583fa5a66fbcfd720e96d9eacadc18632595cb6423e2f613f".to_string();
        let parsed = EthereumTransactionCommon::from_hex(signed_tx.clone()).unwrap();
        let expected = eip1559_tx();
        assert_eq!(expected, parsed);

        assert_eq!(signed_tx, hex::encode(parsed.to_bytes()))
    }

    #[test]
    fn test_eip1559_unsigned_enc_dec() {
        let unsigned_tx = EthereumTransactionCommon {
            signature: None,
            ..eip1559_tx()
        };
        let tx_encoding = "02f86f0182102e843b9aca008506b1a22f8082918e949f8f72aa9304c8b593d555f12ef6589cc3a579a280b844a9059cbb000000000000000000000000a9d1e08c7793af67e9d92fe308d5697fb81d3e43000000000000000000000000000000000000000000000000f020482e89b73c14c0";
        assert_eq!(tx_encoding, hex::encode(unsigned_tx.to_bytes()));
        assert_eq!(
            unsigned_tx,
            EthereumTransactionCommon::from_bytes(&hex::decode(tx_encoding).unwrap())
                .unwrap()
        );
    }

    #[test]
    fn test_hash_exact() {
        let tx_encoded = "f86480843b9aca00825cd6946ce4d79d4e77402e1ef3417fdda433aa744c6e1c0180820a959f3df55056959e51b66a515312fd0b851d629f562cb1e1b30d29fc909acb8450a02edac87401057bbe3a6255a253db01e5c5c6b9a597bfdab07d4ceefe08c03af9";
        let tx_decoded =
            EthereumTransactionCommon::from_bytes(&hex::decode(tx_encoded).unwrap())
                .unwrap();
        let tx_reencoded = hex::encode(tx_decoded.to_bytes());
        assert_eq!(tx_encoded, tx_reencoded);
    }

    #[test]
    fn test_eip7702() {
        let tx_encoded = "04f8c98205390101843b9aca00830186a0946ce4d79d4e77402e1ef3417fdda433aa744c6e1c8080c0f85ef85c82053994d77420f73b4612a7a99dba8c2afd30a1886b03440280a0591b9a1ef4d69710bf7952a2ea7cc88ae32380f85242c523952d8b93d1e51084a001d9c219c8a62ef8d46f19b5a0960e950b3df0ca96ca46703748d41c2170cc7080a095dcd3e32651557e9bd09f4da8d05293b6bbb9946cd35aef5c8e99c0d6fa98a5a0597a2d4d61b846c19f1187b45ef891f499dc9962314e2251fc93c98c92fd4bb3";
        let tx_decoded =
            EthereumTransactionCommon::from_bytes(&hex::decode(tx_encoded).unwrap())
                .unwrap();
        assert_eq!(tx_decoded.type_, TransactionType::Eip7702);
        // "authorizationList": [
        //    {
        //      "chainId": "0x0539",
        //      "address": "0xd77420f73b4612a7a99dba8c2afd30a1886b0344",
        //      "nonce": "0x02",
        //      "yParity": "0x00",
        //      "r": "0x591b9a1ef4d69710bf7952a2ea7cc88ae32380f85242c523952d8b93d1e51084",
        //      "s": "0x01d9c219c8a62ef8d46f19b5a0960e950b3df0ca96ca46703748d41c2170cc70"
        //    }
        // ]
        assert_eq!(
            signed_authorization(tx_decoded.authorization_list.as_deref().unwrap_or_default().to_owned()),
            [
                revm::context::transaction::SignedAuthorization::new_unchecked(
                    revm::context::transaction::Authorization {
                        chain_id: revm::primitives::U256::from(0x0539),
                        address: revm::primitives::Address::from_str(
                            "d77420f73b4612a7a99dba8c2afd30a1886b0344"
                        )
                        .unwrap(),
                        nonce: 0x02
                    },
                    0x00,
                    revm::primitives::U256::from_be_slice(&hex::decode("591b9a1ef4d69710bf7952a2ea7cc88ae32380f85242c523952d8b93d1e51084").unwrap()),
                    revm::primitives::U256::from_be_slice(&hex::decode("01d9c219c8a62ef8d46f19b5a0960e950b3df0ca96ca46703748d41c2170cc70").unwrap())
                )
            ]
        );
        let tx_reencoded = hex::encode(tx_decoded.to_bytes());
        assert_eq!(tx_encoded, tx_reencoded);
    }
}
