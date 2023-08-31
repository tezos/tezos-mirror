// SPDX-FileCopyrightText: 2022-2023 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use hex::FromHexError;
use libsecp256k1::{
    curve::Scalar, sign, Error, Message, RecoveryId, SecretKey, Signature,
};
use primitive_types::{H256, U256};
use rlp::{DecoderError, Encodable, RlpIterator};
use thiserror::Error;

use crate::rlp_helpers::{decode_field, decode_field_h256, next};

/// Represents a **valid** Ethereum signature
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct TxSignature {
    v: U256,
    r: H256,
    s: H256,
}

#[derive(Error, Debug, PartialEq, Clone)]
pub enum ParityError {
    #[error("Couldn't reconstruct V from chain_id: {0}")]
    ChainId(U256),

    #[error("Couldn't reconstruct parity from V: {0}")]
    V(U256),
}

#[derive(Error, Debug, PartialEq, Clone)]
pub enum TxSigError {
    #[error("Error reading a hex string: {0}")]
    HexError(#[from] FromHexError),

    #[error("Error manipulating ECDSA key: {0}")]
    ECDSAError(libsecp256k1::Error),

    #[error("Error recomputing parity of signature: {0}")]
    Parity(ParityError),

    #[error("Invalid R and S: {0} {1}")]
    InvalidRS(H256, H256),
}

impl From<libsecp256k1::Error> for TxSigError {
    fn from(e: libsecp256k1::Error) -> Self {
        Self::ECDSAError(e)
    }
}

pub fn h256_to_scalar(h: H256) -> Scalar {
    let h1: [u8; 32] = h.into();
    let mut h = Scalar([0; 8]);
    let _ = h.set_b32(&h1);
    h
}

impl TxSignature {
    pub fn v(&self) -> U256 {
        self.v
    }

    pub fn r(&self) -> &H256 {
        &self.r
    }

    pub fn s(&self) -> &H256 {
        &self.s
    }

    pub fn new(v: U256, r: H256, s: H256) -> Result<Self, TxSigError> {
        if r != H256::zero() && s != H256::zero() {
            Ok(TxSignature { v, r, s })
        } else {
            Err(TxSigError::InvalidRS(r, s))
        }
    }

    /// Computes the parity for a transaction in the Ethereum "legacy" format,
    /// ie a transaction that encodes the `chain_id` in the value `v`, this
    /// should not be used for EIP-1559 or EIP-2930 transaction for example.
    /// The boolean correspond to parity `0` or `1`.
    fn legacy_compute_parity(&self, chain_id: U256) -> Result<bool, TxSigError> {
        let err = TxSigError::Parity(ParityError::V(self.v));
        let chain_id_encoding = chain_id
            .checked_mul(U256::from(2))
            .ok_or_else(|| err.clone())?
            .checked_add(U256::from(35))
            .ok_or_else(|| err.clone())?;
        let parity = self.v.checked_sub(chain_id_encoding);
        match parity {
            Some(p) if p < U256::from(2) => Ok(p == U256::one()),
            _ => Err(err),
        }
    }

    /// Validate that signatures conforms EIP-2
    /// and that is possible to restore parity from `chain_id` and `v`
    pub fn signature_legacy(
        &self,
        chain_id: U256,
    ) -> Result<(Signature, RecoveryId), TxSigError> {
        let r = h256_to_scalar(self.r.to_owned());
        let s = h256_to_scalar(self.s.to_owned());
        if s.is_high() {
            // if s > secp256k1n / 2 the signature is invalid
            // cf EIP2 (part 2) https://eips.ethereum.org/EIPS/eip-2
            Err(TxSigError::ECDSAError(
                libsecp256k1::Error::InvalidSignature,
            ))
        } else {
            // recompute parity from v and chain_id
            let parity = self.legacy_compute_parity(chain_id)?;
            let ri = RecoveryId::parse(parity as u8)?;
            Ok((Signature { r, s }, ri))
        }
    }

    /// Validate that signatures conforms EIP-2
    pub fn signature(&self) -> Result<(Signature, RecoveryId), TxSigError> {
        let r = h256_to_scalar(self.r.to_owned());
        let s = h256_to_scalar(self.s.to_owned());
        if s.is_high() {
            // if s > secp256k1n / 2 the signature is invalid
            // cf EIP2 (part 2) https://eips.ethereum.org/EIPS/eip-2
            Err(TxSigError::ECDSAError(
                libsecp256k1::Error::InvalidSignature,
            ))
        } else if self.v < U256::from(4) {
            let ri = RecoveryId::parse(self.v().as_u32() as u8)?;
            Ok((Signature { r, s }, ri))
        } else {
            Err(TxSigError::ECDSAError(Error::InvalidRecoveryId))
        }
    }

    /// compute v from parity and chain_id
    fn compute_v(chain_id: U256, parity: u8) -> Option<U256> {
        if chain_id == U256::zero() {
            // we don't support transactions with unpresented chain_id
            None
        } else {
            let chain_id_encoding = chain_id
                .checked_mul(U256::from(2))?
                .checked_add(U256::from(35))?;
            U256::from(parity).checked_add(chain_id_encoding)
        }
    }

    pub fn sign_secp256k1(
        msg: &Message,
        string_sk: String,
    ) -> Result<(Signature, RecoveryId), TxSigError> {
        let hex: &[u8] = &hex::decode(string_sk)?;
        let sk = SecretKey::parse_slice(hex)?;
        Ok(sign(msg, &sk))
    }

    /// This function creates a signature for a legacy tx.
    /// Legacy tx has a tricky approach for signature creation, where
    /// `v` field of a signature carry information about `chain_id`.
    /// For more information see https://eips.ethereum.org/EIPS/eip-155
    pub fn sign_legacy(
        msg: &Message,
        string_sk: String,
        chain_id: U256,
    ) -> Result<Self, TxSigError> {
        let (sig, recovery_id) = Self::sign_secp256k1(msg, string_sk)?;
        let parity: u8 = recovery_id.into();
        let v = Self::compute_v(chain_id, parity)
            .ok_or(TxSigError::Parity(ParityError::ChainId(chain_id)))?;

        let (r, s) = (H256::from(sig.r.b32()), H256::from(sig.s.b32()));
        Ok(TxSignature { v, r, s })
    }

    pub fn sign(msg: &Message, string_sk: String) -> Result<Self, TxSigError> {
        let (sig, recovery_id) = Self::sign_secp256k1(msg, string_sk)?;
        let parity: u8 = recovery_id.into();

        let (r, s) = (H256::from(sig.r.b32()), H256::from(sig.s.b32()));
        Ok(TxSignature {
            v: U256::from(parity),
            r,
            s,
        })
    }

    #[cfg(test)]
    pub fn new_unsafe(v: u64, r: H256, s: H256) -> TxSignature {
        TxSignature::new(U256::from(v), r, s).expect("Signature data should be valid")
    }
}

impl Encodable for TxSignature {
    fn rlp_append(&self, stream: &mut rlp::RlpStream) {
        stream.append(&self.v);
        stream.append(&self.r);
        stream.append(&self.s);
    }
}

// Encode None as (0, 0, 0)
// This encoding is used for transaction object,
// which expects to have a signature.
// However, for deposits there are no signature so we have to mock it.
pub fn rlp_append_opt(sig: &Option<TxSignature>, stream: &mut rlp::RlpStream) {
    match sig {
        Some(sig) => Encodable::rlp_append(sig, stream),
        None => {
            stream.append(&U256::zero());
            stream.append(&H256::zero());
            stream.append(&H256::zero());
        }
    }
}

// Decode (0, 0, 0) as None
// See comment for rlp_append_opt above.
pub fn rlp_decode_opt(
    it: &mut RlpIterator<'_, '_>,
) -> Result<Option<TxSignature>, DecoderError> {
    let v: U256 = decode_field(&next(it)?, "v")?;
    let r: H256 = decode_field_h256(&next(it)?, "r")?;
    let s: H256 = decode_field_h256(&next(it)?, "s")?;
    if r == H256::zero() && s == H256::zero() && v == U256::zero() {
        Ok(None)
    } else {
        let sig = TxSignature::new(v, r, s)
            .map_err(|_| DecoderError::Custom("Invalid signature"))?;
        Ok(Some(sig))
    }
}
