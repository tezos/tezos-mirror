// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

//! This module defines OCaml bindings for the ML-DSA-44 signature scheme from
//! the libcrux-ml-dsa crate and serves as a basis for building
//! the octez-libcrux-ml-dsa OCaml library.

use ocaml::Pointer;
use zeroize::Zeroize;

use libcrux_ml_dsa::ml_dsa_44::{
    MLDSA44KeyPair, MLDSA44Signature, MLDSA44SigningKey, MLDSA44VerificationKey,
};
// This imports the multiplexing versions of these functions
use libcrux_ml_dsa::ml_dsa_44::{generate_key_pair, sign, verify};
use libcrux_ml_dsa::{KEY_GENERATION_RANDOMNESS_SIZE, SIGNING_RANDOMNESS_SIZE};

/// Verification key size for ML-DSA-44
const VERIFICATION_KEY_SIZE: usize = MLDSA44VerificationKey::len();

/// Signing key size for ML-DSA-44
const SIGNING_KEY_SIZE: usize = MLDSA44SigningKey::len();

/// Signature size for ML-DSA-44
const SIGNATURE_SIZE: usize = MLDSA44Signature::len();

/// Maximum signing context size
const MAX_CONTEXT_SIZE: usize = 255;

const EMPTY_CONTEXT: &[u8] = b"";

/// Try to convert a byte slice to a fixed-size array.
///
/// Returns an error if the slice length does not match the expected size.
fn array_from_slice<const SIZE: usize>(bytes: &[u8], name: &str) -> Result<[u8; SIZE], String> {
    bytes
        .try_into()
        .map_err(|_| format!("Invalid {name} size: expected {SIZE}, got {}", bytes.len()))
}

/// Try to convert a byte slice to a fixed-size array.
///
/// Returns [`None`] if the slice length does not match the expected size.
fn array_from_slice_opt<const SIZE: usize>(bytes: &[u8]) -> Option<[u8; SIZE]> {
    bytes.try_into().ok()
}

/// Check that the given length is less than or equal to [`MAX_CONTEXT_SIZE`].
///
/// Returns an error otherwise.
fn check_context_len(len: usize) -> Result<(), String> {
    if len > MAX_CONTEXT_SIZE {
        return Err(format!(
            "Context too long: expected at most {MAX_CONTEXT_SIZE} bytes, got {len}"
        ));
    }
    Ok(())
}

/// An ML-DSA-44 key pair containing both signing and verification keys
#[ocaml::sig]
pub struct KeyPair(MLDSA44KeyPair);

impl Drop for KeyPair {
    fn drop(&mut self) {
        self.0.signing_key.as_ref_mut().zeroize()
    }
}

/// An ML-DSA-44 signing key
#[ocaml::sig]
pub struct SigningKey(MLDSA44SigningKey);

impl Drop for SigningKey {
    fn drop(&mut self) {
        self.0.as_ref_mut().zeroize()
    }
}

/// An ML-DSA-44 verification key
#[ocaml::sig]
pub struct VerificationKey(MLDSA44VerificationKey);

/// An ML-DSA-44 signature
#[ocaml::sig]
pub struct Signature(MLDSA44Signature);

ocaml::custom!(KeyPair);
ocaml::custom!(SigningKey);
ocaml::custom!(VerificationKey);
ocaml::custom!(Signature);

/// Generate a new ML-DSA-44 key pair and return pointers to each key.
///
/// Returns an error if the size of `randomness` is not [`KEY_GENERATION_RANDOMNESS_SIZE`].
///
/// # Security
///
/// `randomness` must be cryptographically secure random bytes.
#[ocaml::func]
#[ocaml::sig("bytes -> (signing_key * verification_key, string) result")]
pub fn octez_libcrux_ml_dsa_44_generate_keys(
    randomness: &[u8],
) -> Result<(Pointer<SigningKey>, Pointer<VerificationKey>), String> {
    let randomness_bytes = array_from_slice::<KEY_GENERATION_RANDOMNESS_SIZE>(
        randomness,
        "key generation randomness",
    )?;

    let key_pair = generate_key_pair(randomness_bytes);
    let signing_key = SigningKey(key_pair.signing_key).into();
    let verification_key = VerificationKey(key_pair.verification_key).into();
    Ok((signing_key, verification_key))
}

/// Generate a new ML-DSA-44 key pair and return pointers to each key.
///
/// Returns [`None`] if the size of `randomness` is not [`KEY_GENERATION_RANDOMNESS_SIZE`].
///
/// # Security
///
/// `randomness` must be cryptographically secure random bytes.
#[ocaml::func]
#[ocaml::sig("bytes -> (signing_key * verification_key) option")]
pub fn octez_libcrux_ml_dsa_44_generate_keys_opt(
    randomness: &[u8],
) -> Option<(Pointer<SigningKey>, Pointer<VerificationKey>)> {
    let randomness_bytes = array_from_slice_opt::<KEY_GENERATION_RANDOMNESS_SIZE>(randomness)?;

    let key_pair = generate_key_pair(randomness_bytes);
    let signing_key = SigningKey(key_pair.signing_key).into();
    let verification_key = VerificationKey(key_pair.verification_key).into();
    Some((signing_key, verification_key))
}

/// Generate a new ML-DSA-44 key pair.
///
/// Should only be used when a [`KeyPair`] needs to be manipulated in OCaml. If the goal is
/// to immediately obtain a signing and a verification key, prefer [`octez_libcrux_ml_dsa_44_generate_keys`]
/// or [`octez_libcrux_ml_dsa_44_generate_keys_opt`] instead as it will result in fewer calls to the FFI
/// and eliminate the need for cloning the key pair.
///
/// Returns an error if the size of `randomness` is not [`KEY_GENERATION_RANDOMNESS_SIZE`].
///
/// # Security
///
/// `randomness` must be cryptographically secure random bytes.
#[ocaml::func]
#[ocaml::sig("bytes -> (key_pair, string) result")]
pub fn octez_libcrux_ml_dsa_44_generate_key_pair(
    randomness: &[u8],
) -> Result<Pointer<KeyPair>, String> {
    let randomness_bytes = array_from_slice::<KEY_GENERATION_RANDOMNESS_SIZE>(
        randomness,
        "key generation randomness",
    )?;

    let key_pair = generate_key_pair(randomness_bytes);
    Ok(KeyPair(key_pair).into())
}

/// Extract the signing key from a key pair by cloning it.
#[ocaml::func]
#[ocaml::sig("key_pair -> signing_key")]
pub fn octez_libcrux_ml_dsa_44_key_pair_get_signing_key(
    key_pair: Pointer<KeyPair>,
) -> Pointer<SigningKey> {
    let signing_key = key_pair.as_ref().0.signing_key.clone();
    SigningKey(signing_key).into()
}

/// Extract the verification key from a key pair by cloning it.
#[ocaml::func]
#[ocaml::sig("key_pair -> verification_key")]
pub fn octez_libcrux_ml_dsa_44_key_pair_get_verification_key(
    key_pair: Pointer<KeyPair>,
) -> Pointer<VerificationKey> {
    let verification_key = key_pair.as_ref().0.verification_key.clone();
    VerificationKey(verification_key).into()
}

/// Sign a message using ML-DSA-44
///
/// Returns an error if:
/// - The size of `randomness` is not [`SIGNING_RANDOMNESS_SIZE`]
/// - The size of `context` exceeds [`MAX_CONTEXT_SIZE`]
/// - Signing fails
///
/// # Security
///
/// `randomness` must be cryptographically secure random bytes.
#[ocaml::func]
#[ocaml::sig("signing_key -> bytes -> bytes -> bytes -> (signature, string) result")]
pub fn octez_libcrux_ml_dsa_44_sign(
    signing_key: Pointer<SigningKey>,
    message: &[u8],
    context: &[u8],
    randomness: &[u8],
) -> Result<Pointer<Signature>, String> {
    let randomness_bytes =
        array_from_slice::<SIGNING_RANDOMNESS_SIZE>(randomness, "signing randomness")?;

    check_context_len(context.len())?;

    match sign(&signing_key.as_ref().0, message, context, randomness_bytes) {
        Ok(signature) => Ok(Signature(signature).into()),
        Err(e) => Err(format!("Signing failed: {e:?}")),
    }
}

/// Sign a message using ML-DSA-44 with an empty signing context
///
/// Returns [`None`] if:
/// - The size of `randomness` is not [`SIGNING_RANDOMNESS_SIZE`]
/// - Signing fails
///
/// # Security
///
/// `randomness` must be cryptographically secure random bytes.
#[ocaml::func]
#[ocaml::sig("signing_key -> bytes -> bytes -> signature option")]
pub fn octez_libcrux_ml_dsa_44_sign_opt(
    signing_key: Pointer<SigningKey>,
    message: &[u8],
    randomness: &[u8],
) -> Option<Pointer<Signature>> {
    let randomness_bytes = array_from_slice_opt::<SIGNING_RANDOMNESS_SIZE>(randomness)?;
    sign(
        &signing_key.as_ref().0,
        message,
        EMPTY_CONTEXT,
        randomness_bytes,
    )
    .ok()
    .map(|signature| Signature(signature).into())
}

/// Verify an ML-DSA-44 signature
///
/// Returns an error if:
/// - The size of `context` exceeds [`MAX_CONTEXT_SIZE`]
/// - Verification fails
#[ocaml::func]
#[ocaml::sig("verification_key -> bytes -> bytes -> signature -> (unit, string) result")]
pub fn octez_libcrux_ml_dsa_44_verify(
    verification_key: Pointer<VerificationKey>,
    message: &[u8],
    context: &[u8],
    signature: Pointer<Signature>,
) -> Result<(), String> {
    check_context_len(context.len())?;

    verify(
        &verification_key.as_ref().0,
        message,
        context,
        &signature.as_ref().0,
    )
    .map_err(|e| format!("Verification failed: {e:?}"))
}

/// Verify an ML-DSA-44 signature with an empty signing context
///
/// Returns false if:
/// - The size of `context` exceeds [`MAX_CONTEXT_SIZE`]
/// - Verification fails
#[ocaml::func]
#[ocaml::sig("verification_key -> bytes -> signature -> bool")]
pub fn octez_libcrux_ml_dsa_44_verify_b(
    verification_key: Pointer<VerificationKey>,
    message: &[u8],
    signature: Pointer<Signature>,
) -> bool {
    verify(
        &verification_key.as_ref().0,
        message,
        EMPTY_CONTEXT,
        &signature.as_ref().0,
    )
    .is_ok()
}

/// Return a new byte buffer of size [`VERIFICATION_KEY_SIZE`] containing
/// the verification key bytes.
#[ocaml::func]
#[ocaml::sig("verification_key -> bytes")]
pub fn octez_libcrux_ml_dsa_44_verification_key_to_bytes(
    verification_key: Pointer<VerificationKey>,
) -> [u8; VERIFICATION_KEY_SIZE] {
    *verification_key.as_ref().0.as_ref()
}

/// Create a new verification key from bytes.
///
/// Returns an error if the size of `bytes` is not [`VERIFICATION_KEY_SIZE`].
#[ocaml::func]
#[ocaml::sig("bytes -> (verification_key, string) result")]
pub fn octez_libcrux_ml_dsa_44_verification_key_from_bytes(
    bytes: &[u8],
) -> Result<Pointer<VerificationKey>, String> {
    let verification_key_bytes =
        array_from_slice::<VERIFICATION_KEY_SIZE>(bytes, "verification key")?;

    Ok(VerificationKey(MLDSA44VerificationKey::new(verification_key_bytes)).into())
}

/// Create a new verification key from bytes.
///
/// Returns [`None`] if the size of `bytes` is not [`VERIFICATION_KEY_SIZE`].
#[ocaml::func]
#[ocaml::sig("bytes -> verification_key option")]
pub fn octez_libcrux_ml_dsa_44_verification_key_from_bytes_opt(
    bytes: &[u8],
) -> Option<Pointer<VerificationKey>> {
    let verification_key_bytes = array_from_slice_opt::<VERIFICATION_KEY_SIZE>(bytes)?;

    Some(VerificationKey(MLDSA44VerificationKey::new(verification_key_bytes)).into())
}

/// Return a new byte buffer of size [`SIGNING_KEY_SIZE`] containing
/// the signing key bytes.
///
/// # Security
///
/// This function exposes sensitive key material. Callers are responsible for
/// securely handling the returned bytes.
#[ocaml::func]
#[ocaml::sig("signing_key -> bytes")]
pub fn octez_libcrux_ml_dsa_44_signing_key_to_bytes(
    signing_key: Pointer<SigningKey>,
) -> [u8; SIGNING_KEY_SIZE] {
    *signing_key.as_ref().0.as_ref()
}

/// Create a new signing key from bytes.
///
/// Returns an error if the size of `bytes` is not [`SIGNING_KEY_SIZE`].
#[ocaml::func]
#[ocaml::sig("bytes -> (signing_key, string) result")]
pub fn octez_libcrux_ml_dsa_44_signing_key_from_bytes(
    bytes: &[u8],
) -> Result<Pointer<SigningKey>, String> {
    let mut signing_key_bytes = array_from_slice::<SIGNING_KEY_SIZE>(bytes, "signing key")?;

    let signing_key = SigningKey(MLDSA44SigningKey::new(signing_key_bytes)).into();

    signing_key_bytes.zeroize();

    Ok(signing_key)
}

/// Create a new signing key from bytes.
///
/// Returns [`None`] if the size of `bytes` is not [`SIGNING_KEY_SIZE`].
#[ocaml::func]
#[ocaml::sig("bytes -> signing_key option")]
pub fn octez_libcrux_ml_dsa_44_signing_key_from_bytes_opt(
    bytes: &[u8],
) -> Option<Pointer<SigningKey>> {
    let mut signing_key_bytes = array_from_slice_opt::<SIGNING_KEY_SIZE>(bytes)?;

    let signing_key = SigningKey(MLDSA44SigningKey::new(signing_key_bytes)).into();

    signing_key_bytes.zeroize();

    Some(signing_key)
}

/// Return a new byte buffer of size [`SIGNATURE_SIZE`] containing
/// the signature bytes.
#[ocaml::func]
#[ocaml::sig("signature -> bytes")]
pub fn octez_libcrux_ml_dsa_44_signature_to_bytes(
    signature: Pointer<Signature>,
) -> [u8; SIGNATURE_SIZE] {
    *signature.as_ref().0.as_ref()
}

/// Create a new signature from bytes.
///
/// Returns an error if the size of `bytes` is not [`SIGNATURE_SIZE`].
#[ocaml::func]
#[ocaml::sig("bytes -> (signature, string) result")]
pub fn octez_libcrux_ml_dsa_44_signature_from_bytes(
    bytes: &[u8],
) -> Result<Pointer<Signature>, String> {
    let signature_bytes = array_from_slice::<SIGNATURE_SIZE>(bytes, "signature")?;

    Ok(Signature(MLDSA44Signature::new(signature_bytes)).into())
}

/// Create a new signature from bytes.
///
/// Returns [`None`] if the size of `bytes` is not [`SIGNATURE_SIZE`].
#[ocaml::func]
#[ocaml::sig("bytes -> signature option")]
pub fn octez_libcrux_ml_dsa_44_signature_from_bytes_opt(
    bytes: &[u8],
) -> Option<Pointer<Signature>> {
    let signature_bytes = array_from_slice_opt::<SIGNATURE_SIZE>(bytes)?;

    Some(Signature(MLDSA44Signature::new(signature_bytes)).into())
}
