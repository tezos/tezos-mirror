// SPDX-FileCopyrightText: 2022-2023 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023 Functori <contact@functori.com>
// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

//! Tezos account state and storage

use crate::{
    context,
    enshrined_contracts::{self, EnshrinedContracts},
};
use num_bigint::BigInt;
use primitive_types::U256;
use rlp::{Decodable, Encodable, Rlp};
use tezos_crypto_rs::hash::ContractKt1Hash;
use tezos_data_encoding::{
    enc::BinWriter,
    nom::NomReader,
    types::{Narith, Zarith},
};
use tezos_ethereum::rlp_helpers::{
    append_u256_le, append_u64_le, decode_field_u256_le, decode_field_u64_le,
};
use tezos_protocol::contract::Contract;
use tezos_smart_rollup::{
    host::{RuntimeError, ValueType},
    types::{PublicKey, PublicKeyHash},
};
use tezos_smart_rollup_host::path::{OwnedPath, RefPath};
use tezos_smart_rollup_host::storage::StorageV1;
use tezos_storage::{
    read_nom_value_bounded, read_optional_nom_value, read_optional_nom_value_bounded,
    read_optional_nom_value_bounded_with_len, store_bin,
};
use tezosx_interfaces::{Origin, TezosXRuntimeError};

/// Upper bound, in bytes, on a `Narith`-encoded balance or counter.
/// Both hold values that fit in a `u64` in practice — balance is bounded
/// by total supply, counters by the operation count — and a `Narith` of
/// a `u64` is at most 10 bytes. 32 leaves generous headroom and matches
/// the gas upper-bound convention (see `COUNTER_SIZE` in `lib.rs`).
///
/// Used to read each field in a single `store_read` host call instead of
/// the `store_value_size` + `store_read` pair that `store_read_all`
/// performs. See [`read_optional_nom_value_bounded`].
const NARITH_FIELD_MAX_BYTES: usize = 32;

/// Upper bound, in bytes, on an encoded [`Manager`]. The largest variant
/// is `Revealed(PublicKey)`: 1 byte for the manager tag plus a public
/// key whose largest variant is BLS, at 1 (algorithm tag) + 48 bytes.
/// 64 leaves headroom over that 50-byte maximum.
const MANAGER_MAX_BYTES: usize = 64;

/// Upper bound, in bytes, on an encoded [`OriginatedContractInfo`]. The
/// record is `tup4(n, n, z, z)`: `code_size` and `storage_size` as
/// `Narith` and `used_bytes`/`paid_bytes` as `Zarith`. Each field holds a
/// byte count that fits in a `u64` in practice — bounded by the contract
/// size and storage limits — so each encodes to at most ~10/11 bytes,
/// roughly 42 bytes for the whole record. 64 leaves generous headroom and
/// lets the record be read with a single bounded `store_read` host call
/// instead of the `store_value_size` + `store_read` pair `store_read_all`
/// performs. See [`read_optional_nom_value_bounded`].
const INFO_MAX_BYTES: usize = 64;

// This enum is inspired of `src/proto_alpha/lib_protocol/manager_repr.ml`
// A manager can be:
//  - a public key, it means that the account is revealed
//  - a public key hash, account is not yet revealed but the
//    reveal public key will match this public key hash
#[derive(Debug, PartialEq, Eq, BinWriter, NomReader)]
#[encoding(tags = "u8")]
pub enum Manager {
    #[encoding(tag = 0)]
    NotRevealed(PublicKeyHash),
    // Tag 1 is for public key before the Seoul
    // protocol and therefore is deprecated
    #[encoding(tag = 2)]
    Revealed(PublicKey),
}

pub trait TezlinkAccount {
    fn path(&self) -> &OwnedPath;
    fn contract(&self) -> Contract;

    /// Get the **balance** of an account in Mutez held by the account.
    fn balance(
        &self,
        host: &impl StorageV1,
    ) -> Result<Narith, tezos_storage::error::Error> {
        let path = context::account::balance_path(self)?;
        Ok(
            read_optional_nom_value_bounded(host, &path, NARITH_FIELD_MAX_BYTES)?
                .unwrap_or(0_u64.into()),
        )
    }

    /// Set the **balance** of an account in Mutez held by the account.
    fn set_balance(
        &self,
        host: &mut impl StorageV1,
        balance: &Narith,
    ) -> Result<(), tezos_storage::error::Error> {
        let path = context::account::balance_path(self)?;
        store_bin(balance, host, &path)
    }

    /// Add amount (in Mutez) to the **balance** held by the account.
    ///
    /// Delegates to `self.balance()` and `self.set_balance()` so that
    /// implementations overriding those methods (e.g. TezosX RLP storage)
    /// get correct behavior without needing to override `add_balance` too.
    ///
    /// TODO: In TezosX, this causes two storage reads + two RLP decodes
    /// (one in balance(), one in set_balance()). If add_balance becomes
    /// a hot path, TezosX should override it with a single
    /// read-modify-write on the RLP blob.
    fn add_balance(
        &self,
        host: &mut impl StorageV1,
        amount: u64,
    ) -> Result<(), tezos_storage::error::Error> {
        let current = self.balance(host)?;
        self.set_balance(host, &Narith(current.0 + amount))
    }
}

#[derive(Debug, PartialEq)]
pub struct TezlinkImplicitAccount {
    pub(crate) path: OwnedPath,
    pub(crate) pkh: PublicKeyHash,
}

impl TezlinkAccount for TezlinkImplicitAccount {
    #[inline]
    fn path(&self) -> &OwnedPath {
        &self.path
    }
    fn contract(&self) -> Contract {
        Contract::Implicit(self.pkh.clone())
    }
}

pub trait TezosImplicitAccountTrait: TezlinkAccount + Sized {
    fn pkh(&self) -> &PublicKeyHash;

    /// Get the **counter** for the Tezlink account.
    fn counter(
        &self,
        host: &impl StorageV1,
    ) -> Result<Narith, tezos_storage::error::Error> {
        let path = context::account::counter_path(self)?;
        Ok(
            read_optional_nom_value_bounded(host, &path, NARITH_FIELD_MAX_BYTES)?
                .unwrap_or(0_u64.into()),
        )
    }

    /// Set the **counter** for the Tezlink account.
    fn set_counter(
        &self,
        host: &mut impl StorageV1,
        counter: &Narith,
    ) -> Result<(), tezos_storage::error::Error> {
        let path = context::account::counter_path(self)?;
        store_bin(counter, host, &path)
    }

    /// Set the **counter** for the Tezlink account to the successor of the current value.
    fn increment_counter(
        &self,
        host: &mut impl StorageV1,
        validated_operations_count: usize,
    ) -> Result<(), tezos_storage::error::Error> {
        self.set_counter(
            host,
            &Narith(self.counter(host)?.0 + validated_operations_count),
        )
    }

    fn manager(
        &self,
        host: &impl StorageV1,
    ) -> Result<Manager, tezos_storage::error::Error> {
        let path = context::account::manager_path(self)?;
        let manager: Manager = read_nom_value_bounded(host, &path, MANAGER_MAX_BYTES)?;
        Ok(manager)
    }

    fn set_manager_public_key_hash(
        &self,
        host: &mut impl StorageV1,
    ) -> Result<(), tezos_storage::error::Error> {
        self.set_manager_pk_hash_internal(host, self.pkh())
    }

    /// This function updates the manager with a public key hash in parameter.
    /// Most of the time, we're dealing with references so this function is here to avoid cloning
    /// the public key hash to build a [Manager] object
    fn set_manager_pk_hash_internal(
        &self,
        host: &mut impl StorageV1,
        public_key_hash: &PublicKeyHash,
    ) -> Result<(), tezos_storage::error::Error> {
        let path = context::account::manager_path(self)?;
        // The tag for public key hash is 0 (see the Manager enum above)
        let mut buffer = vec![0_u8];
        public_key_hash
            .bin_write(&mut buffer)
            .map_err(|_| tezos_smart_rollup::host::RuntimeError::DecodingError)?;
        host.store_write_all(&path, &buffer)?;
        Ok(())
    }

    /// This function is used to test a situation in which we have an
    /// inconsistent manager pkh for an implicit account.
    #[cfg(test)]
    fn force_set_manager_public_key_hash(
        &self,
        host: &mut impl StorageV1,
        pkh: &PublicKeyHash,
    ) -> Result<(), tezos_storage::error::Error> {
        self.set_manager_pk_hash_internal(host, pkh)
    }

    /// This function updates the manager with the public key in parameter.
    /// Most of the time, we're dealing with references so this function is here to avoid cloning
    /// the public key hash to build a [Manager] object
    fn set_manager_public_key(
        &self,
        host: &mut impl StorageV1,
        public_key: &PublicKey,
    ) -> Result<(), tezos_storage::error::Error> {
        let path = context::account::manager_path(self)?;
        // The tag for public key is 2 (see the Manager enum above)
        let mut buffer = vec![2_u8];
        public_key
            .bin_write(&mut buffer)
            .map_err(|_| tezos_smart_rollup::host::RuntimeError::DecodingError)?;
        host.store_write_all(&path, &buffer)?;
        Ok(())
    }

    /// Allocate an account in the durable storage. Does nothing if account was
    /// already allocated.
    fn allocate(
        &self,
        host: &mut impl StorageV1,
    ) -> Result<bool, tezos_storage::error::Error> {
        if self.allocated(host)? {
            return Ok(true);
        }
        self.set_balance(host, &0_u64.into())?;
        // TODO: use a global counter instead of initializing counter at 0
        self.set_counter(host, &0u64.into())?;
        self.set_manager_public_key_hash(host)?;
        Ok(false)
    }

    // Below this comment is multiple functions useful for validate an operation

    /// Verify if an account is allocated by attempting to read its balance
    fn allocated(
        &self,
        host: &impl StorageV1,
    ) -> Result<bool, tezos_storage::error::Error> {
        let path = context::account::balance_path(self)?;
        Ok(Some(ValueType::Value) == host.store_has(&path)?)
    }
}

impl TezosImplicitAccountTrait for TezlinkImplicitAccount {
    fn pkh(&self) -> &PublicKeyHash {
        &self.pkh
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TezlinkOriginatedAccount {
    pub path: OwnedPath,
    pub kt1: ContractKt1Hash,
}

impl TezlinkAccount for TezlinkOriginatedAccount {
    #[inline]
    fn path(&self) -> &OwnedPath {
        &self.path
    }
    fn contract(&self) -> Contract {
        Contract::Originated(self.kt1.clone())
    }
}
#[derive(Debug, PartialEq)]
pub enum Code {
    Code(Vec<u8>),
    Enshrined(EnshrinedContracts),
}

/// Durable-storage path of the single shared Michelson implementation that
/// backs every Tezos X alias. An alias `KT1` carries no script of its own;
/// when its `/data/code` is absent and it is classified [`Origin::Alias`], its
/// code resolves to the bytes stored here (see [`TezosOriginatedAccount::code`]).
///
/// The slot lives in a `__system__` subtree of the `tezosx/` account namespace
/// (`__system__` is not a valid base58 account address, so it cannot collide
/// with a per-account directory) and, being rooted under `/tez/tez_accounts`,
/// is covered by the Michelson state-hash commitment `h(/tez/tez_accounts)`:
/// replicas that disagree on the implementation diverge on the Michelson block
/// `state_root`.
pub(crate) const ALIAS_IMPLEMENTATION_PATH: RefPath =
    RefPath::assert_from(b"/tez/tez_accounts/tezosx/__system__/alias_implementation");

/// Read the shared alias implementation. Returns `None` when the slot has not
/// been seeded yet.
pub fn read_alias_implementation(
    host: &impl StorageV1,
) -> Result<Option<Vec<u8>>, tezos_storage::error::Error> {
    match host.store_read_all(&ALIAS_IMPLEMENTATION_PATH) {
        Ok(bytes) => Ok(Some(bytes)),
        Err(RuntimeError::PathNotFound) => Ok(None),
        Err(err) => Err(err.into()),
    }
}

/// Overwrite the shared alias implementation with `code`. A single write here
/// changes the behaviour of every existing and future alias at once — the O(1)
/// upgrade primitive.
pub fn write_alias_implementation(
    host: &mut impl StorageV1,
    code: &[u8],
) -> Result<(), tezos_storage::error::Error> {
    host.store_write_all(&ALIAS_IMPLEMENTATION_PATH, code)?;
    Ok(())
}

/// Snapshot of a contract's storage-space.
pub struct StorageSpace {
    pub used_bytes: Zarith,
    pub allocated_bytes: Zarith,
}

#[derive(Debug, PartialEq)]
pub struct StorageDelta(Zarith);

impl From<i32> for StorageDelta {
    fn from(value: i32) -> Self {
        Self(value.into())
    }
}

impl From<BigInt> for StorageDelta {
    fn from(value: BigInt) -> Self {
        Self(value.into())
    }
}

/// Aggregated storage-accounting record for an originated contract,
/// persisted at [context::code::info_path]. It bundles the four small
/// counters (`code_size`, `storage_size`, `used_bytes`, `paid_bytes`) into
/// a single value, so the whole accounting state is read and written with a
/// single host call. The code and storage *blobs* (`/data/code`,
/// `/data/storage`) are unbounded and stay at their own keys.
///
/// Binary layout (sequential, self-delimiting): `code_size` and
/// `storage_size` as `Narith`, then `used_bytes` and `paid_bytes` as
/// `Zarith`. The OCaml node decodes the same layout via
/// `Data_encoding.(tup4 n n z z)`.
#[derive(Debug, Clone, PartialEq, Eq, BinWriter, NomReader)]
pub struct OriginatedContractInfo {
    /// Byte length of the contract's code blob (`/data/code`).
    pub code_size: Narith,
    /// Byte length of the contract's storage blob (`/data/storage`).
    pub storage_size: Narith,
    /// Live storage size charged to the contract
    /// (`code_size + storage_size + lazy_storage_size`).
    pub used_bytes: Zarith,
    /// Monotonic high-water-mark of the bytes already paid for.
    pub paid_bytes: Zarith,
}

impl OriginatedContractInfo {
    /// The aggregated record of a freshly originated contract: its
    /// `used_bytes` is the code, storage and initial lazy-storage bytes it
    /// occupies, and `paid_bytes` starts at that same value. Single source of
    /// truth for both [`TezosOriginatedAccount::init`] (which persists it) and
    /// the origination gas charge (which sizes it before the write).
    pub(crate) fn for_new_contract(
        code_size: u64,
        storage_size: u64,
        lazy_storage_size_diff: Zarith,
    ) -> Self {
        let used_bytes = Zarith(
            BigInt::from(code_size)
                + BigInt::from(storage_size)
                + lazy_storage_size_diff.0,
        );
        OriginatedContractInfo {
            code_size: code_size.into(),
            storage_size: storage_size.into(),
            used_bytes: used_bytes.clone(),
            paid_bytes: used_bytes,
        }
    }

    /// Set `storage_size` to `new_storage_size`, fold the resulting
    /// storage-size delta together with `lazy_storage_size_diff` into
    /// `used_bytes`, and raise the `paid_bytes` watermark when the new
    /// size exceeds it. Returns the updated record and the [StorageSpace]
    /// (new `used_bytes` plus the freshly allocated bytes). Pure: no host
    /// access, so callers can size the record for gas before committing
    /// the write. The pure core of a transfer's storage update, shared by
    /// the gas-sizing and the write.
    pub(crate) fn with_storage_size(
        mut self,
        new_storage_size: u64,
        lazy_storage_size_diff: Zarith,
    ) -> (Self, StorageSpace) {
        let storage_size_diff =
            BigInt::from(new_storage_size) - Zarith::from(self.storage_size.clone()).0;
        self.storage_size = new_storage_size.into();

        let used_bytes = Zarith(
            self.used_bytes.0.clone() + storage_size_diff + lazy_storage_size_diff.0,
        );
        let already_paid = self.paid_bytes.0.clone();
        let allocated_bytes = if used_bytes.0 > already_paid {
            let diff = Zarith(&used_bytes.0 - &already_paid);
            self.paid_bytes = used_bytes.clone();
            diff
        } else {
            Zarith::from(0u64)
        };
        self.used_bytes = used_bytes.clone();
        (
            self,
            StorageSpace {
                used_bytes,
                allocated_bytes,
            },
        )
    }

    /// Encoded byte length of this record (`tup4 n n z z`). Used to charge
    /// gas for the exact number of bytes written to the `/info` key, rather
    /// than a fixed bound — the encoding is self-delimiting, so the length
    /// depends on the counter magnitudes. The read side does not call this:
    /// it sizes the read from the length the store returned (see
    /// [TezosOriginatedAccount::read_info_with_len]), so this only runs once
    /// per write, on the post-update record that has not been persisted yet.
    pub(crate) fn encoded_len(&self) -> Result<u64, tezos_storage::error::Error> {
        // `BinWriter` has no length-only path, so we serialise into a
        // throwaway buffer and read its length. The record is four small
        // counters (bounded by `INFO_MAX_BYTES`), so the allocation is
        // negligible; don't "optimise" it away without such a path existing.
        let mut buffer = vec![];
        self.bin_write(&mut buffer)?;
        Ok(buffer.len() as u64)
    }
}

pub trait TezosOriginatedAccount: TezlinkAccount + Clone + Sized {
    fn kt1(&self) -> &ContractKt1Hash;

    fn code(&self, host: &impl StorageV1) -> Result<Code, tezos_storage::error::Error> {
        if let Some(c) = enshrined_contracts::from_kt1(self.kt1()) {
            return Ok(Code::Enshrined(c));
        }
        let code_path = context::code::code_path(self)?;
        match host.store_read_all(&code_path) {
            Ok(code) => Ok(Code::Code(code)),
            // A KT1 with no `/data/code` is either a Tezos X alias — whose
            // script is the single shared implementation — or a genuinely
            // missing contract. Resolving on the code-miss keeps the common
            // path at one read and never reads `/origin` for ordinary
            // contracts; the gate on `is_alias` also means implicit `tz1`
            // accounts (which never reach this trait) are untouched.
            Err(RuntimeError::PathNotFound) => {
                if self.is_alias(host)? {
                    // The slot is guaranteed seeded before any code-less alias
                    // can exist: `ensure_alias` seeds it before materializing
                    // an alias (L2-1529), and migration / runtime activation
                    // seed it on existing networks. So an unseeded slot here is
                    // a should-never-happen invariant violation, surfaced as an
                    // internal error rather than silently substituting code.
                    read_alias_implementation(host)?
                        .map(Code::Code)
                        .ok_or_else(|| {
                            tezos_storage::error::Error::Internal(
                                "alias implementation slot is not seeded".to_string(),
                            )
                        })
                } else {
                    Err(RuntimeError::PathNotFound.into())
                }
            }
            Err(err) => Err(err.into()),
        }
    }

    /// Read the classification record (`Origin`) at the account's `/origin`
    /// path. `None` when the account carries no classification.
    ///
    /// This is the same decode as
    /// [`tezosx_tezos_runtime::account::get_origin_at`] /
    /// `Context::read_origin_for_address`, kept here because `tezos_execution`
    /// sits below those: if the on-disk `Origin` encoding ever changes, all of
    /// them move together. `read_optional_nom_value` already maps a missing
    /// path to `None` and rejects incomplete / trailing bytes.
    fn origin(
        &self,
        host: &impl StorageV1,
    ) -> Result<Option<Origin>, tezos_storage::error::Error> {
        read_optional_nom_value(host, &context::code::origin_path(self)?)
    }

    /// Whether this originated account is a Tezos X alias (classified
    /// [`Origin::Alias`]), whose script resolves to the shared implementation.
    fn is_alias(
        &self,
        host: &impl StorageV1,
    ) -> Result<bool, tezos_storage::error::Error> {
        Ok(matches!(self.origin(host)?, Some(Origin::Alias(_))))
    }

    /// Whether the originated contract exists in durable storage. A regular
    /// contract is marked by the presence of a code blob at `/data/code`
    /// (`Origination` always writes one); enshrined contracts always exist
    /// (their code is synthetic); a Tezos X alias exists despite having no
    /// `/data/code`, since its script is the shared implementation.
    ///
    /// Used as a guard before transferring to an originated destination,
    /// so a Transaction to a never-originated KT1 produces a typed
    /// user-level error instead of falling through to a kernel-internal
    /// `code()` failure.
    fn exists(&self, host: &impl StorageV1) -> Result<bool, tezos_storage::error::Error> {
        if enshrined_contracts::is_enshrined(self.kt1()) {
            return Ok(true);
        }
        let code_path = context::code::code_path(self)?;
        if Some(ValueType::Value) == host.store_has(&code_path)? {
            return Ok(true);
        }
        // A code-less KT1 exists iff it is an alias.
        self.is_alias(host)
    }

    /// Read the aggregated [OriginatedContractInfo] for this contract. The
    /// single `/info` record is read with one bounded `store_read` (see
    /// [INFO_MAX_BYTES]); a contract with no record yet (never originated)
    /// reads as an all-zero record. The individual getters use it so they
    /// keep their `&self, &host` signature and can run on read-only hosts.
    fn read_info(
        &self,
        host: &impl StorageV1,
    ) -> Result<OriginatedContractInfo, tezos_storage::error::Error> {
        Ok(self.read_info_with_len(host)?.0)
    }

    /// Like [Self::read_info], but also returns the number of bytes read
    /// from the `/info` key, so the transfer path can charge the read at
    /// the exact bytes the store returned instead of re-encoding the
    /// record. A contract with no record yet reads as an all-zero record
    /// of length `0`.
    fn read_info_with_len(
        &self,
        host: &impl StorageV1,
    ) -> Result<(OriginatedContractInfo, u64), tezos_storage::error::Error> {
        let info_path = context::code::info_path(self)?;
        Ok(
            read_optional_nom_value_bounded_with_len(host, &info_path, INFO_MAX_BYTES)?
                .map_or_else(
                    || {
                        (
                            OriginatedContractInfo {
                                code_size: 0u64.into(),
                                storage_size: 0u64.into(),
                                used_bytes: Zarith::from(0u64),
                                paid_bytes: Zarith::from(0u64),
                            },
                            0,
                        )
                    },
                    |(info, len)| (info, len as u64),
                ),
        )
    }

    /// Persist the aggregated record in a single host write.
    fn write_info(
        &self,
        host: &mut impl StorageV1,
        info: &OriginatedContractInfo,
    ) -> Result<(), tezos_storage::error::Error> {
        let info_path = context::code::info_path(self)?;
        store_bin(info, host, &info_path)
    }

    /// Only the test suite writes the code blob in isolation: `init`
    /// inlines its own `/data/code` write and refreshes the aggregated
    /// record in one shot, so there is no production caller. Gated to
    /// `test` so it doesn't masquerade as production API.
    #[cfg(test)]
    fn set_code(
        &self,
        host: &mut impl StorageV1,
        data: &[u8],
    ) -> Result<Zarith, tezos_storage::error::Error> {
        let path = context::code::code_path(self)?;
        host.store_write_all(&path, data)?;
        let code_size = data.len() as u64;
        let mut info = self.read_info(host)?;
        info.code_size = code_size.into();
        self.write_info(host, &info)?;
        Ok(code_size.into())
    }

    fn storage(
        &self,
        host: &impl StorageV1,
    ) -> Result<Vec<u8>, tezos_storage::error::Error> {
        if enshrined_contracts::is_enshrined(self.kt1()) {
            return Ok(vec![]);
        }
        let storage_path = context::code::storage_path(self)?;
        let storage = host.store_read_all(&storage_path)?;
        Ok(storage)
    }

    fn code_size(
        &self,
        host: &impl StorageV1,
    ) -> Result<Zarith, tezos_storage::error::Error> {
        Ok(self.read_info(host)?.code_size.into())
    }

    fn storage_size(
        &self,
        host: &mut impl StorageV1,
    ) -> Result<Narith, tezos_storage::error::Error> {
        Ok(self.read_info(host)?.storage_size)
    }

    /// Returns the contract's `used_bytes` watermark, or `0` if nothing
    /// has been written yet.
    fn used_bytes(
        &self,
        host: &impl StorageV1,
    ) -> Result<Zarith, tezos_storage::error::Error> {
        Ok(self.read_info(host)?.used_bytes)
    }

    /// Returns the contract's `paid_bytes` watermark, or `0` if nothing
    /// has been written yet.
    fn paid_bytes(
        &self,
        host: &impl StorageV1,
    ) -> Result<Zarith, tezos_storage::error::Error> {
        Ok(self.read_info(host)?.paid_bytes)
    }

    /// Persists `data` as the contract's main storage and refreshes the
    /// `storage_size` field of the aggregated record. Returns the signed
    /// byte-length delta between the new and the previous content. Returns
    /// zero on enshrined contracts.
    ///
    /// The previous size is read from the record's `storage_size` (kept in
    /// sync with the blob), so this no longer needs a separate
    /// `store_value_size` call on the storage blob.
    ///
    /// Test-only: the transfer path computes the updated record with the
    /// pure [OriginatedContractInfo::with_storage_size] and commits it via
    /// [Self::write_storage_and_info]. Gated to `test` so it doesn't
    /// masquerade as production API.
    #[cfg(test)]
    fn set_storage(
        &self,
        host: &mut impl StorageV1,
        data: &[u8],
    ) -> Result<StorageDelta, tezos_storage::error::Error> {
        if enshrined_contracts::is_enshrined(self.kt1()) {
            return Ok(0.into());
        }
        let path = context::code::storage_path(self)?;
        host.store_write_all(&path, data)?;
        let mut info = self.read_info(host)?;
        let new_storage_size = data.len() as u64;
        let prev_storage_size = Zarith::from(info.storage_size.clone()).0;
        info.storage_size = new_storage_size.into();
        self.write_info(host, &info)?;
        Ok((BigInt::from(new_storage_size) - prev_storage_size).into())
    }

    /// Initialise an originated contract's durable state.
    ///
    /// `code` is `Some` for an ordinary origination. It is `None` for a
    /// Tezos X alias, which carries no script of its own: no `/data/code` is
    /// written, so [`TezosOriginatedAccount::code`] resolves the account to the
    /// single shared alias implementation. A zero code size is still recorded
    /// in the aggregated `/info` record so storage-space accounting reads a
    /// definite code size of `0` — the shared code is not charged per alias.
    fn init(
        &self,
        host: &mut impl StorageV1,
        code: Option<&[u8]>,
        storage: &[u8],
        lazy_storage_size_diff: Zarith,
    ) -> Result<StorageSpace, tezos_storage::error::Error> {
        if enshrined_contracts::is_enshrined(self.kt1()) {
            return Ok(StorageSpace {
                used_bytes: 0.into(),
                allocated_bytes: 0.into(),
            });
        }
        // Origination always targets a fresh KT1, so the record is built
        // entirely from the blob lengths and the initial lazy-storage
        // delta — no read — and `paid_bytes` starts at the freshly
        // accounted `used_bytes`.
        //
        // `None` (a Tezos X alias) writes no `/data/code` and records a code
        // size of `0` in the aggregated record, so storage-space accounting
        // reads a definite code size of `0` — the shared implementation is not
        // charged per alias.
        let code_size = match code {
            Some(code) => {
                let code_path = context::code::code_path(self)?;
                host.store_write_all(&code_path, code)?;
                code.len() as u64
            }
            None => 0,
        };
        let storage_path = context::code::storage_path(self)?;
        host.store_write_all(&storage_path, storage)?;
        let storage_size = storage.len() as u64;
        let info = OriginatedContractInfo::for_new_contract(
            code_size,
            storage_size,
            lazy_storage_size_diff,
        );
        self.write_info(host, &info)?;
        Ok(StorageSpace {
            used_bytes: info.used_bytes.clone(),
            allocated_bytes: info.used_bytes,
        })
    }

    /// Increment the contract's `used_bytes` watermark by the signed sum
    /// of `storage_size_diff` and `lazy_storage_size_diff`, persisting the
    /// updated record in a single write. No-op on enshrined contracts.
    ///
    /// Test-only: production reaches the same accounting through the pure
    /// [OriginatedContractInfo::with_storage_size] (transfer) or builds the
    /// record directly in [Self::init] (origination). Gated to `test` so it
    /// doesn't masquerade as production API.
    ///
    /// This bumps `used_bytes` by the raw signed delta and leaves
    /// `storage_size` alone (the test sequences [Self::set_storage] first,
    /// which already refreshes it), so it intentionally does not route
    /// through `with_storage_size` — that would re-apply the storage delta.
    #[cfg(test)]
    fn update_storage_space(
        &self,
        host: &mut impl StorageV1,
        storage_size_diff: StorageDelta,
        lazy_storage_size_diff: Zarith,
    ) -> Result<StorageSpace, tezos_storage::error::Error> {
        if enshrined_contracts::is_enshrined(self.kt1()) {
            return Ok(StorageSpace {
                used_bytes: 0.into(),
                allocated_bytes: 0.into(),
            });
        }
        let mut info = self.read_info(host)?;
        let used_bytes = Zarith(
            info.used_bytes.0.clone() + storage_size_diff.0 .0 + lazy_storage_size_diff.0,
        );
        let already_paid = info.paid_bytes.0.clone();
        let allocated_bytes = if used_bytes.0 > already_paid {
            let diff = Zarith(&used_bytes.0 - &already_paid);
            info.paid_bytes = used_bytes.clone();
            diff
        } else {
            Zarith::from(0u64)
        };
        info.used_bytes = used_bytes.clone();
        self.write_info(host, &info)?;
        Ok(StorageSpace {
            used_bytes,
            allocated_bytes,
        })
    }

    /// Persist `data` as the contract's storage blob and `info` as its
    /// aggregated accounting record, in that order. No-op on enshrined
    /// contracts (their storage is synthetic and not accounted).
    ///
    /// The caller computes `info` from the pre-update record via
    /// [OriginatedContractInfo::with_storage_size] and charges gas for the
    /// blob and the record *before* calling this, so the durable writes
    /// happen only once their cost is secured.
    fn write_storage_and_info(
        &self,
        host: &mut impl StorageV1,
        data: &[u8],
        info: &OriginatedContractInfo,
    ) -> Result<(), tezos_storage::error::Error> {
        if enshrined_contracts::is_enshrined(self.kt1()) {
            return Ok(());
        }
        let path = context::code::storage_path(self)?;
        host.store_write_all(&path, data)?;
        self.write_info(host, info)
    }
}

impl TezosOriginatedAccount for TezlinkOriginatedAccount {
    fn kt1(&self) -> &ContractKt1Hash {
        &self.kt1
    }
}

pub fn narith_to_u256(
    narith: &Narith,
) -> Result<primitive_types::U256, TezosXRuntimeError> {
    let bytes = narith.0.to_bytes_be();
    if bytes.len() <= 32 {
        Ok(primitive_types::U256::from_big_endian(&bytes))
    } else {
        Err(TezosXRuntimeError::ConversionError(
            "Narith value too large to fit in U256".to_string(),
        ))
    }
}

pub fn u256_to_narith(value: &primitive_types::U256) -> Narith {
    let mut bytes = [0u8; 32];
    value.to_big_endian(&mut bytes);
    Narith(num_bigint::BigUint::from_bytes_be(&bytes))
}

// Used as a value for the durable storage.
#[derive(Debug, Eq, PartialEq, Clone, Default)]
pub struct TezosAccountInfo {
    pub balance: U256,
    pub nonce: u64,
    pub pub_key: Option<PublicKey>,
}

impl Encodable for TezosAccountInfo {
    fn rlp_append(&self, s: &mut rlp::RlpStream) {
        s.begin_list(3);
        append_u256_le(s, &self.balance);
        append_u64_le(s, &self.nonce);
        match &self.pub_key {
            Some(pub_key) => s.append(&pub_key.to_b58check().as_bytes()),
            None => s.append_empty_data(),
        };
    }
}

impl Decodable for TezosAccountInfo {
    fn decode(rlp: &Rlp) -> Result<Self, rlp::DecoderError> {
        if !rlp.is_list() {
            return Err(rlp::DecoderError::RlpExpectedToBeList);
        }
        if rlp.item_count()? != 3 {
            return Err(rlp::DecoderError::RlpIncorrectListLen);
        }
        let mut it = rlp.iter();
        let balance_decoder = it.next().ok_or(rlp::DecoderError::RlpExpectedToBeList)?;
        let balance = decode_field_u256_le(&balance_decoder, "balance")?;
        let nonce_decoder = it.next().ok_or(rlp::DecoderError::RlpExpectedToBeList)?;
        let nonce = decode_field_u64_le(&nonce_decoder, "nonce")?;
        let pub_key_decoder = it.next().ok_or(rlp::DecoderError::RlpExpectedToBeList)?;
        let pub_key: Option<PublicKey> = if pub_key_decoder.is_empty() {
            None
        } else {
            let vec: Vec<u8> = pub_key_decoder.as_val()?;
            let s: String = String::from_utf8(vec).map_err(|_| {
                rlp::DecoderError::Custom("Invalid public key (not a string)")
            })?;
            let pub_key = PublicKey::from_b58check(&s).map_err(|_| {
                rlp::DecoderError::Custom("Invalid public key (b58check)")
            })?;
            Some(pub_key)
        };

        Ok(TezosAccountInfo {
            balance,
            nonce,
            pub_key,
        })
    }
}

#[cfg(test)]
mod tests {
    use std::str::FromStr;

    use super::narith_to_u256;
    use primitive_types::U256;
    use tezos_data_encoding::types::Narith;

    #[test]
    fn convert_narith_u256() {
        let narith = Narith::from(123456789u64);
        let u256_value = narith_to_u256(&narith).unwrap();
        assert_eq!(u256_value.low_u64(), 123456789u64);
    }

    #[test]
    fn convert_u256_narith() {
        let big_value = U256::from_dec_str("123456789012345678901234567890").unwrap();
        let narith_value = super::u256_to_narith(&big_value);
        let expected_narith = Narith(
            num_bigint::BigUint::from_str("123456789012345678901234567890").unwrap(),
        );
        assert_eq!(narith_value, expected_narith);
    }

    #[test]
    fn narith_u256_overflow() {
        let u256_value = U256::MAX;
        let narith_value = super::u256_to_narith(&u256_value);
        let narith_value = Narith(narith_value.0 + num_bigint::BigUint::from(1u64));
        let _ = narith_to_u256(&narith_value).expect_err("Should error on overflow");
    }
}

#[cfg(test)]
mod test {
    use crate::context::Context;

    use super::*;
    use tezos_crypto_rs::PublicKeyWithHash;
    use tezos_evm_runtime::runtime::MockKernelHost;
    use tezos_smart_rollup_host::path::concat;
    use tezos_smart_rollup_host::path::Path;
    use tezos_smart_rollup_host::path::RefPath;
    use tezos_smart_rollup_host::storage::StorageV1;

    /// obtained by `octez-client show address bootstrap1` in mockup mode
    const BOOTSTRAP1_PKH: &str = "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx";
    const BOOTSTRAP1_PK: &str = "edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav";
    const KT1: &str = "KT1QbKzQAyJtzprfvUJZv8VGqwQNch2o89di";

    /// obtained by `octez-codec encode alpha.contract from '"tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx"'`
    const BOOTSTRAP1_CONTRACT: RefPath =
        RefPath::assert_from(b"/000002298c03ed7d454a101eb7022bc95f7e5f41ac78");

    /// obtained by `PublicKey::from_b58check("edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav")`
    const BOOTSTRAP1_PUBLIC_KEY_HEX: &str =
        "02004798d2cc98473d7e250c898885718afd2e4efbcb1a1595ab9730761ed830de0f";

    /// Set a key of bootstrap1 account according to the structure of a Tezos context
    /// We don't use function from [TezlinkImplicitAccount] on purpose to verify that
    /// it reads at the right path
    fn set_bootstrap1_key(
        host: &mut impl StorageV1,
        value_path: &impl Path,
        value: &[u8],
    ) {
        let index = RefPath::assert_from(b"/tez/tez_accounts/contracts/index");
        let contract = concat(&index, &BOOTSTRAP1_CONTRACT)
            .expect("Concatenation should have succeeded");
        let contract_path =
            concat(&contract, value_path).expect("Concatenation should have succeeded");
        host.store_write_all(&contract_path, value).unwrap();
    }

    // Read test use hard coded path on purpose to verify the Tezos compatibility.
    // These paths comes from the context.json generated by the create mockup command
    // of octez-client.
    #[test]
    fn test_read_balance() {
        let mut host = MockKernelHost::default();

        let balance: Narith = 2944_u64.into();
        // octez-codec decode alpha.contract from '000002298c03ed7d454a101eb7022bc95f7e5f41ac78'
        // Result: "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx"
        let path = RefPath::assert_from(b"/tez/tez_accounts/contracts/index/000002298c03ed7d454a101eb7022bc95f7e5f41ac78/balance");
        store_bin(&balance, &mut host, &path)
            .expect("Store balance should have succeeded");

        let mut balance_array = vec![];
        balance.bin_write(&mut balance_array).unwrap();
        set_bootstrap1_key(
            &mut host,
            &RefPath::assert_from(b"/balance"),
            &balance_array,
        );

        // Initialize path for Tezlink context at /tez/tez_accounts
        let context = context::TezlinkContext::init_context();

        let contract = Contract::from_b58check(BOOTSTRAP1_PKH)
            .expect("Contract base58 conversion should succeeded");

        let account = context
            .implicit_from_contract(&contract)
            .expect("Account creation should have succeeded");

        let read_balance = account
            .balance(&host)
            .expect("read_balance should have succeeded");

        assert_eq!(balance, read_balance);
    }

    #[test]
    fn test_read_counter() {
        let mut host = MockKernelHost::default();

        let counter: Narith = 3u64.into();

        let mut bytes = vec![];
        counter.bin_write(&mut bytes).unwrap();
        set_bootstrap1_key(&mut host, &RefPath::assert_from(b"/counter"), &bytes);

        // Initialize path for Tezlink context at /tez/tez_accounts
        let context = context::TezlinkContext::init_context();

        let contract = Contract::from_b58check(BOOTSTRAP1_PKH)
            .expect("Contract base58 conversion should succeeded");

        let account = context
            .implicit_from_contract(&contract)
            .expect("Account creation should have succeeded");

        let read_counter = account
            .counter(&host)
            .expect("read_counter should have succeeded");

        assert_eq!(counter, read_counter);
    }

    #[test]
    fn test_set_and_read_balance() {
        let mut host = MockKernelHost::default();

        let balance = 4579_u64.into();

        // Initialize path for Tezlink context at /tez/tez_accounts
        let context = context::TezlinkContext::init_context();

        let contract = Contract::from_b58check(BOOTSTRAP1_PKH)
            .expect("Contract base58 conversion should succeeded");

        let account = context
            .implicit_from_contract(&contract)
            .expect("Account creation should have succeeded");

        let () = account
            .set_balance(&mut host, &balance)
            .expect("set_balance should succeeded");

        let read_balance = account
            .balance(&host)
            .expect("read_balance should have succeeded");

        assert_eq!(balance, read_balance);
    }

    #[test]
    fn test_set_and_read_counter() {
        let mut host = MockKernelHost::default();

        let counter: Narith = 6u64.into();

        // Initialize path for Tezlink context at /tez/tez_accounts
        let context = context::TezlinkContext::init_context();

        let contract = Contract::from_b58check(BOOTSTRAP1_PKH)
            .expect("Contract base58 conversion should succeeded");

        let account = context
            .implicit_from_contract(&contract)
            .expect("Account creation should have succeeded");

        let () = account
            .set_counter(&mut host, &counter)
            .expect("set_counter should have succeeded");

        let read_counter = account
            .counter(&host)
            .expect("read_counter should have succeeded");

        assert_eq!(counter, read_counter);
    }

    #[test]
    fn test_read_manager() {
        let mut host = MockKernelHost::default();

        let public_key_hexa = hex::decode(BOOTSTRAP1_PUBLIC_KEY_HEX).unwrap();

        set_bootstrap1_key(
            &mut host,
            &RefPath::assert_from(b"/manager"),
            &public_key_hexa,
        );

        // Initialize path for Tezlink context at /tez/tez_accounts
        let context = context::TezlinkContext::init_context();

        let contract = Contract::from_b58check(BOOTSTRAP1_PKH)
            .expect("Contract base58 conversion should succeeded");

        let account = context
            .implicit_from_contract(&contract)
            .expect("Account creation should have succeeded");

        let manager = account.manager(&host).expect("Can't read manager");

        let pkh = match manager {
            Manager::NotRevealed(pkh) => {
                panic!("Manager should be revealed (manager key is a pkh: {pkh:?})")
            }
            Manager::Revealed(pk) => pk.pk_hash(),
        };

        assert_eq!(contract, Contract::Implicit(pkh));
    }

    #[test]
    fn test_set_read_manager_public_key() {
        let mut host = MockKernelHost::default();

        // Initialize path for Tezlink context at /tez/tez_accounts
        let context = context::TezlinkContext::init_context();

        // Create an account for bootstrap1
        let contract = Contract::from_b58check(BOOTSTRAP1_PKH)
            .expect("Contract base58 conversion should succeeded");

        let account = context
            .implicit_from_contract(&contract)
            .expect("Account creation should have succeeded");

        let public_key = PublicKey::from_b58check(BOOTSTRAP1_PK).unwrap();

        let () = account
            .set_manager_public_key(&mut host, &public_key)
            .expect("set_manager_public_key should have succeeded");

        let manager = Manager::Revealed(public_key);

        let read_manager = account
            .manager(&host)
            .expect("read_manager should have succeeded");

        assert_eq!(manager, read_manager);
    }

    #[test]
    fn test_set_read_manager_public_key_hash() {
        let mut host = MockKernelHost::default();

        // Initialize path for Tezlink context at /tez/tez_accounts
        let context = context::TezlinkContext::init_context();

        // Create an account for bootstrap1
        let pkh = PublicKeyHash::from_b58check(BOOTSTRAP1_PKH)
            .expect("PublicKeyHash base58 conversion should succeeded");

        let contract = Contract::Implicit(pkh);
        let account = context
            .implicit_from_contract(&contract)
            .expect("Account creation should have succeeded");

        let pkh = PublicKeyHash::from_b58check(BOOTSTRAP1_PKH)
            .expect("PublicKeyHash base58 conversion should succeeded");

        let () = account
            .set_manager_public_key_hash(&mut host)
            .expect("set_manager_public_key_hash should have succeeded");

        let manager = Manager::NotRevealed(pkh);

        let read_manager = account
            .manager(&host)
            .expect("read_manager should have succeeded");

        assert_eq!(manager, read_manager);
    }

    #[test]
    fn test_account_initialization() {
        let mut host = MockKernelHost::default();

        // Initialize path for Tezlink context at /tez/tez_accounts
        let context = context::TezlinkContext::init_context();

        // Create an account for bootstrap1
        let pkh = PublicKeyHash::from_b58check(BOOTSTRAP1_PKH)
            .expect("PublicKeyHash base58 conversion should succeeded");

        let contract = Contract::Implicit(pkh);
        let account = context
            .implicit_from_contract(&contract)
            .expect("Account creation should have succeeded");

        let exist = account
            .allocated(&host)
            .expect("Exist account should have succeeded");

        assert!(!exist);

        account
            .allocate(&mut host)
            .expect("Account initialization should have succeeded");

        let exist = account
            .allocated(&host)
            .expect("Exist account should have succeeded");

        assert!(exist);

        let test_balance = 1999_u64.into();

        account
            .set_balance(&mut host, &test_balance)
            .expect("Set balance should have succeeded");

        // Calling init on a contract already initialized will do nothing
        // So the balance should not change and still be 1999
        account
            .allocate(&mut host)
            .expect("Account initialization should have succeeded");

        let read_balance = account
            .balance(&host)
            .expect("Read balance should have succeeded");

        assert_eq!(test_balance, read_balance);
    }

    #[test]
    fn test_set_read_large_code() {
        let mut host = MockKernelHost::default();

        // Initialize path for Tezlink context at /tez/tez_accounts
        let context = context::TezlinkContext::init_context();

        // Create an originated account for KT1
        let contract = Contract::from_b58check(KT1).unwrap();
        let account = context
            .originated_from_contract(&contract)
            .expect("Account creation should have succeeded");

        let code = vec![1u8; 10_000];

        // Set the code of the KT1
        account
            .set_code(&mut host, &code)
            .expect("Setting code of the KT1 should succeed");

        let read_code = account
            .code(&host)
            .expect("Read the code of the KT1 should succeed");

        assert_eq!(
            Code::Code(code),
            read_code,
            "Set/Read code have inconsistent behavior"
        );
    }

    /// Writing a bigger value grows the watermark and the delta;
    /// `used_bytes` is the live size.
    #[test]
    fn test_update_storage_space_after_grow() {
        let mut host = MockKernelHost::default();
        let context = context::TezlinkContext::init_context();
        let contract = Contract::from_b58check(KT1).unwrap();
        let account = context.originated_from_contract(&contract).unwrap();

        let code = vec![0xab_u8; 30];
        let storage = vec![0xcd_u8; 20];
        let larger_storage = vec![0x12_u8; 50];

        let StorageSpace {
            allocated_bytes, ..
        } = account
            .init(&mut host, Some(&code), &storage, 0.into())
            .unwrap();
        let initial_size = (code.len() + storage.len()) as u64;
        assert_eq!(allocated_bytes, Zarith::from(initial_size));
        assert_eq!(
            account.used_bytes(&host).unwrap(),
            Zarith::from(initial_size)
        );
        assert_eq!(
            account.paid_bytes(&host).unwrap(),
            Zarith::from(initial_size)
        );

        let diff = account.set_storage(&mut host, &larger_storage).unwrap();
        let StorageSpace {
            allocated_bytes, ..
        } = account
            .update_storage_space(&mut host, diff, 0.into())
            .unwrap();
        let larger_size = (code.len() + larger_storage.len()) as u64;
        assert_eq!(allocated_bytes, Zarith::from(larger_size - initial_size));
        assert_eq!(
            account.used_bytes(&host).unwrap(),
            Zarith::from(larger_size)
        );
        assert_eq!(
            account.paid_bytes(&host).unwrap(),
            Zarith::from(larger_size)
        );
    }

    /// Writing a smaller value leaves the watermark unchanged and
    /// returns no delta; `used_bytes` is the live size.
    #[test]
    fn test_update_storage_space_after_shrink() {
        let mut host = MockKernelHost::default();
        let context = context::TezlinkContext::init_context();
        let contract = Contract::from_b58check(KT1).unwrap();
        let account = context.originated_from_contract(&contract).unwrap();

        let code = vec![0xab_u8; 30];
        let storage = vec![0xcd_u8; 20];
        let smaller_storage = vec![0x34_u8; 5];

        account
            .init(&mut host, Some(&code), &storage, 0.into())
            .unwrap();
        let initial_size = (code.len() + storage.len()) as u64;

        let diff = account.set_storage(&mut host, &smaller_storage).unwrap();
        let StorageSpace {
            allocated_bytes, ..
        } = account
            .update_storage_space(&mut host, diff, 0.into())
            .unwrap();
        assert_eq!(allocated_bytes, Zarith::from(0u64));
        assert_eq!(
            account.used_bytes(&host).unwrap(),
            Zarith::from((code.len() + smaller_storage.len()) as u64),
        );
        assert_eq!(
            account.paid_bytes(&host).unwrap(),
            Zarith::from(initial_size)
        );
    }

    /// `set_storage` returns the signed byte-length delta.
    #[test]
    fn test_set_storage_returns_signed_diff() {
        let mut host = MockKernelHost::default();
        let context = context::TezlinkContext::init_context();
        let contract = Contract::from_b58check(KT1).unwrap();
        let account = context.originated_from_contract(&contract).unwrap();

        let first = account.set_storage(&mut host, &[0u8; 30]).unwrap();
        assert_eq!(first, 30.into(), "first write reports new_len");

        let grow = account.set_storage(&mut host, &[0u8; 50]).unwrap();
        assert_eq!(grow, 20.into(), "grow reports new_len - prev_len");

        let shrink = account.set_storage(&mut host, &[0u8; 5]).unwrap();
        assert_eq!(shrink, (-45).into(), "shrink reports the negative delta");

        let same = account.set_storage(&mut host, &[1u8; 5]).unwrap();
        assert_eq!(same, 0.into(), "same-size overwrite reports zero delta");
    }

    /// `set_storage` early-returns `(0, 0)` on enshrined contracts and
    /// does not write to durable storage.
    #[test]
    fn test_set_storage_returns_zero_for_enshrined() {
        let mut host = MockKernelHost::default();
        let context = context::TezlinkContext::init_context();

        // Gateway is an enshrined contract; set_storage early-returns on it
        // without touching durable storage.
        const GATEWAY_KT1: &str = "KT18oDJJKXMKhfE1bSuAPGp92pYcwVDiqsPw";
        let contract = Contract::from_b58check(GATEWAY_KT1).unwrap();
        let account = context.originated_from_contract(&contract).unwrap();

        let diff = account.set_storage(&mut host, &[0xef_u8; 15]).unwrap();
        assert_eq!(
            diff,
            0.into(),
            "enshrined set_storage must report zero delta"
        );
        let StorageSpace {
            used_bytes,
            allocated_bytes,
        } = account
            .update_storage_space(&mut host, diff, 0.into())
            .unwrap();
        assert_eq!(used_bytes, Zarith::from(0u64));
        assert_eq!(allocated_bytes, Zarith::from(0u64));

        let storage_path = context::code::storage_path(&account).unwrap();
        let info_path = context::code::info_path(&account).unwrap();
        assert_eq!(host.store_has(&storage_path).unwrap(), None);
        assert_eq!(host.store_has(&info_path).unwrap(), None);
    }

    /// `exists` is the guard the transfer pipeline uses to reject calls
    /// to never-originated KT1s before any state write. An enshrined
    /// contract must always report as existing — its code is synthetic
    /// and never written to durable storage.
    #[test]
    fn test_exists_returns_true_for_enshrined() {
        let host = MockKernelHost::default();
        let context = context::TezlinkContext::init_context();

        const GATEWAY_KT1: &str = "KT18oDJJKXMKhfE1bSuAPGp92pYcwVDiqsPw";
        let contract = Contract::from_b58check(GATEWAY_KT1).unwrap();
        let account = context.originated_from_contract(&contract).unwrap();

        assert!(account.exists(&host).unwrap());
    }

    /// A regular originated KT1 with no code blob in storage must report
    /// as not existing — this is the case `transfer` rejects with
    /// `ContractDoesNotExist`.
    #[test]
    fn test_exists_returns_false_when_never_originated() {
        let host = MockKernelHost::default();
        let context = context::TezlinkContext::init_context();

        let contract = Contract::from_b58check(KT1).unwrap();
        let account = context.originated_from_contract(&contract).unwrap();

        assert!(!account.exists(&host).unwrap());
    }

    /// After `set_code` writes a code blob at `/data/code`, `exists`
    /// must report as existing — `Origination` always writes a code blob,
    /// so the presence of one is the marker.
    #[test]
    fn test_exists_returns_true_after_set_code() {
        let mut host = MockKernelHost::default();
        let context = context::TezlinkContext::init_context();

        let contract = Contract::from_b58check(KT1).unwrap();
        let account = context.originated_from_contract(&contract).unwrap();

        assert!(!account.exists(&host).unwrap());
        account.set_code(&mut host, &[0xab_u8; 4]).unwrap();
        assert!(account.exists(&host).unwrap());
    }

    /// `used_bytes` and `paid_bytes` default to zero on a contract that
    /// was never written. The default holds for both freshly-created
    /// accounts (no prior `init`) and for the `paid_bytes` slot of a
    /// contract that has been initialised but whose watermark has not
    /// yet been bumped by the storage-payment pass.
    #[test]
    fn test_getters_default_to_zero_on_absent_path() {
        let host = MockKernelHost::default();
        let context = context::TezlinkContext::init_context();
        let contract = Contract::from_b58check(KT1).unwrap();
        let account = context.originated_from_contract(&contract).unwrap();

        assert_eq!(account.used_bytes(&host).unwrap(), Zarith::from(0u64));
        assert_eq!(account.paid_bytes(&host).unwrap(), Zarith::from(0u64));
    }

    /// Helper: originate a contract with the given main-storage size,
    /// then return the live account.
    fn init_with_storage(
        host: &mut MockKernelHost,
        storage_len: usize,
    ) -> TezlinkOriginatedAccount {
        let context = context::TezlinkContext::init_context();
        let contract = Contract::from_b58check(KT1).unwrap();
        let account = context.originated_from_contract(&contract).unwrap();
        let code = vec![0xab_u8; 30];
        let storage = vec![0xcd_u8; storage_len];
        account.init(host, Some(&code), &storage, 0.into()).unwrap();
        account
    }

    /// A positive `lazy_storage_size_diff` bumps `used_bytes` and the
    /// `paid_bytes` watermark by the same amount.
    #[test]
    fn test_update_storage_space_grows_on_positive_lazy_delta() {
        let mut host = MockKernelHost::default();
        let account = init_with_storage(&mut host, 20);
        let prev_used = account.used_bytes(&host).unwrap();

        let StorageSpace {
            used_bytes,
            allocated_bytes,
        } = account
            .update_storage_space(&mut host, 0.into(), (65 + 7).into())
            .unwrap();

        assert_eq!(used_bytes, Zarith(prev_used.0.clone() + (65 + 7)));
        assert_eq!(allocated_bytes, Zarith::from((65 + 7) as u64));
        assert_eq!(account.paid_bytes(&host).unwrap(), used_bytes);
    }

    /// A negative `lazy_storage_size_diff` lowers `used_bytes` and
    /// leaves `paid_bytes` (the watermark) unchanged.
    #[test]
    fn test_update_storage_space_shrinks_on_negative_lazy_delta() {
        let mut host = MockKernelHost::default();
        let account = init_with_storage(&mut host, 20);
        let prev_used = account.used_bytes(&host).unwrap();
        let prev_paid = account.paid_bytes(&host).unwrap();

        // First grow above the watermark so we have headroom to shrink.
        account
            .update_storage_space(&mut host, 0.into(), 200.into())
            .unwrap();
        let prev_paid_after_grow = account.paid_bytes(&host).unwrap();

        let StorageSpace {
            used_bytes,
            allocated_bytes,
        } = account
            .update_storage_space(&mut host, 0.into(), (-100).into())
            .unwrap();

        assert_eq!(used_bytes, Zarith(prev_used.0 + 100));
        assert_eq!(allocated_bytes, Zarith::from(0u64));
        assert_eq!(account.paid_bytes(&host).unwrap(), prev_paid_after_grow);
        assert!(prev_paid_after_grow.0 > prev_paid.0);
    }

    /// Zero deltas on both sides are a no-op on `used_bytes` /
    /// `paid_bytes` and report `allocated_bytes = 0`.
    #[test]
    fn test_update_storage_space_unchanged_when_both_diffs_zero() {
        let mut host = MockKernelHost::default();
        let account = init_with_storage(&mut host, 20);
        let prev_used = account.used_bytes(&host).unwrap();
        let prev_paid = account.paid_bytes(&host).unwrap();

        let StorageSpace {
            used_bytes,
            allocated_bytes,
        } = account
            .update_storage_space(&mut host, 0.into(), 0.into())
            .unwrap();

        assert_eq!(used_bytes, prev_used);
        assert_eq!(allocated_bytes, Zarith::from(0u64));
        assert_eq!(account.paid_bytes(&host).unwrap(), prev_paid);
    }

    /// `used_bytes` is the sum of both deltas added to the previous
    /// watermark.
    #[test]
    fn test_update_storage_space_sums_storage_and_lazy_diffs() {
        let mut host = MockKernelHost::default();
        let account = init_with_storage(&mut host, 20);
        let prev_used = account.used_bytes(&host).unwrap();

        let StorageSpace { used_bytes, .. } = account
            .update_storage_space(&mut host, 11.into(), 22.into())
            .unwrap();

        assert_eq!(used_bytes, Zarith(prev_used.0 + 33));
    }

    /// Shrinking below the previous `paid_bytes` watermark leaves the
    /// watermark unchanged and reports `allocated_bytes = 0`.
    #[test]
    fn test_update_storage_space_shrink_below_paid_bytes_no_diff() {
        let mut host = MockKernelHost::default();
        let account = init_with_storage(&mut host, 20);

        // Push the watermark up by 500 first.
        account
            .update_storage_space(&mut host, 0.into(), 500.into())
            .unwrap();
        let paid_high = account.paid_bytes(&host).unwrap();

        // Now drop used_bytes by 400 (still below the watermark).
        let StorageSpace {
            used_bytes,
            allocated_bytes,
        } = account
            .update_storage_space(&mut host, 0.into(), (-400).into())
            .unwrap();

        assert!(used_bytes.0 < paid_high.0);
        assert_eq!(allocated_bytes, Zarith::from(0u64));
        assert_eq!(account.paid_bytes(&host).unwrap(), paid_high);
    }

    /// A non-zero `lazy_storage_size_diff` at origination contributes
    /// to the initial `used_bytes` and `paid_bytes` watermarks.
    #[test]
    fn test_init_with_initial_lazy_storage() {
        let mut host = MockKernelHost::default();
        let context = context::TezlinkContext::init_context();
        let contract = Contract::from_b58check(KT1).unwrap();
        let account = context.originated_from_contract(&contract).unwrap();

        let code = vec![0xab_u8; 30];
        let storage = vec![0xcd_u8; 20];
        let initial_lazy: u64 = 33 + 65 + 7;

        let StorageSpace {
            used_bytes,
            allocated_bytes,
        } = account
            .init(&mut host, Some(&code), &storage, initial_lazy.into())
            .unwrap();

        let expected = (code.len() + storage.len()) as u64 + initial_lazy;
        assert_eq!(used_bytes, Zarith::from(expected));
        assert_eq!(allocated_bytes, Zarith::from(expected));
        assert_eq!(account.paid_bytes(&host).unwrap(), used_bytes);
    }

    /// Enshrined contracts short-circuit `update_storage_space` and
    /// return `(0, 0)` regardless of the deltas they receive.
    #[test]
    fn test_update_storage_space_enshrined_ignores_diffs() {
        let mut host = MockKernelHost::default();
        let context = context::TezlinkContext::init_context();
        const GATEWAY_KT1: &str = "KT18oDJJKXMKhfE1bSuAPGp92pYcwVDiqsPw";
        let contract = Contract::from_b58check(GATEWAY_KT1).unwrap();
        let account = context.originated_from_contract(&contract).unwrap();

        let StorageSpace {
            used_bytes,
            allocated_bytes,
        } = account
            .update_storage_space(&mut host, 999_999.into(), 999_999.into())
            .unwrap();

        assert_eq!(used_bytes, Zarith::from(0u64));
        assert_eq!(allocated_bytes, Zarith::from(0u64));
    }

    /// The shared alias-implementation slot must stay under `/tez/tez_accounts`
    /// so it remains covered by the `h(/tez/tez_accounts)` state-hash
    /// commitment. Nothing enforces this at compile time, so guard it here.
    #[test]
    fn alias_implementation_path_is_under_the_accounts_root() {
        use tezos_smart_rollup_host::path::Path;
        assert!(ALIAS_IMPLEMENTATION_PATH
            .as_bytes()
            .starts_with(b"/tez/tez_accounts"));
    }

    fn classify_as_alias(
        host: &mut impl StorageV1,
        account: &impl TezosOriginatedAccount,
    ) {
        use tezosx_interfaces::{AliasInfo, RuntimeId};
        let origin = Origin::Alias(AliasInfo {
            runtime: RuntimeId::Ethereum,
            native_address: b"0xabc".to_vec(),
        });
        let mut buf = vec![];
        origin.bin_write(&mut buf).unwrap();
        host.store_write_all(&context::code::origin_path(account).unwrap(), &buf)
            .unwrap();
    }

    /// A KT1 classified `Origin::Alias` and carrying no `/data/code` resolves to
    /// the shared implementation, and reports as existing.
    #[test]
    fn alias_code_resolves_to_shared_implementation() {
        let mut host = MockKernelHost::default();
        let context = context::TezlinkContext::init_context();
        let contract = Contract::from_b58check(KT1).unwrap();
        let account = context.originated_from_contract(&contract).unwrap();

        classify_as_alias(&mut host, &account);
        let implementation = vec![0x02u8, 0x00, 0x00, 0x00, 0x00];
        write_alias_implementation(&mut host, &implementation).unwrap();

        assert_eq!(account.code(&host).unwrap(), Code::Code(implementation));
        assert!(account.exists(&host).unwrap());
    }

    /// An alias whose shared implementation has not been seeded is an error, not
    /// a silent fallback.
    #[test]
    fn alias_without_seeded_implementation_errors() {
        let mut host = MockKernelHost::default();
        let context = context::TezlinkContext::init_context();
        let contract = Contract::from_b58check(KT1).unwrap();
        let account = context.originated_from_contract(&contract).unwrap();

        classify_as_alias(&mut host, &account);
        // Pin the error type: an unseeded slot is an internal invariant breach,
        // not a generic/typecheck error (so it can't silently change meaning).
        assert!(matches!(
            account.code(&host),
            Err(tezos_storage::error::Error::Internal(_))
        ));
        // Still classified, so it exists.
        assert!(account.exists(&host).unwrap());
    }

    /// A code-less KT1 that is not an alias is a genuinely missing contract:
    /// `code()` errors and `exists()` is false. The `/origin` read only happens
    /// because `/data/code` is absent — a regular contract never pays for it.
    #[test]
    fn code_less_non_alias_is_missing() {
        let host = MockKernelHost::default();
        let context = context::TezlinkContext::init_context();
        let contract = Contract::from_b58check(KT1).unwrap();
        let account = context.originated_from_contract(&contract).unwrap();

        // Pin the variant: a code-less non-alias must surface as the plain
        // missing-path error, not get routed through the alias branch (which
        // would yield `Internal`).
        assert!(matches!(
            account.code(&host),
            Err(tezos_storage::error::Error::Runtime(
                RuntimeError::PathNotFound
            ))
        ));
        assert!(!account.exists(&host).unwrap());
    }

    /// A code-less KT1 classified `Origin::Native` is not an alias either:
    /// still a genuinely missing contract. Guards against relaxing `is_alias`
    /// to `origin.is_some()`, which would flip every Native-classified code-less
    /// KT1 to the shared forwarder.
    #[test]
    fn code_less_native_classified_is_missing() {
        let mut host = MockKernelHost::default();
        let context = context::TezlinkContext::init_context();
        let contract = Contract::from_b58check(KT1).unwrap();
        let account = context.originated_from_contract(&contract).unwrap();

        let mut buf = vec![];
        Origin::Native.bin_write(&mut buf).unwrap();
        host.store_write_all(&context::code::origin_path(&account).unwrap(), &buf)
            .unwrap();

        assert!(matches!(
            account.code(&host),
            Err(tezos_storage::error::Error::Runtime(
                RuntimeError::PathNotFound
            ))
        ));
        assert!(!account.exists(&host).unwrap());
    }

    /// A regular originated contract (with `/data/code`) is unaffected: its own
    /// script is returned and it exists, regardless of the shared slot.
    #[test]
    fn regular_contract_returns_own_code() {
        let mut host = MockKernelHost::default();
        let context = context::TezlinkContext::init_context();
        let contract = Contract::from_b58check(KT1).unwrap();
        let account = context.originated_from_contract(&contract).unwrap();

        let own_code = vec![0x05u8, 0x01, 0x02, 0x03];
        account.set_code(&mut host, &own_code).unwrap();

        assert_eq!(account.code(&host).unwrap(), Code::Code(own_code));
        assert!(account.exists(&host).unwrap());
    }

    /// The `OriginatedContractInfo` binary layout is a fixed cross-language
    /// contract: the OCaml node decodes the same bytes via
    /// `Data_encoding.(tup4 n n z z)`. This test pins the wire format so a
    /// kernel-side encoding change can't silently desync the node decoder.
    /// `code_size`/`storage_size` are `Narith` (`n`), `used`/`paid` are
    /// `Zarith` (`z`); for these small positive values each is a single
    /// byte.
    #[test]
    fn test_info_record_binary_layout() {
        let info = OriginatedContractInfo {
            code_size: 30u64.into(),
            storage_size: 20u64.into(),
            used_bytes: Zarith::from(50u64),
            paid_bytes: Zarith::from(80u64),
        };
        let mut buffer = vec![];
        info.bin_write(&mut buffer).unwrap();
        // n(30)=0x1e, n(20)=0x14, z(50)=0x32 (fits in the 6 low bits of the
        // first z byte), z(80)=0x90 0x01 (80 = 64 + 16: first byte carries
        // the low 6 value bits 0b010000 with the continuation bit set, the
        // second byte the remaining 0b1).
        assert_eq!(buffer, vec![0x1e, 0x14, 0x32, 0x90, 0x01]);

        // Round-trips through the decoder used by the getters.
        let (rest, decoded) = OriginatedContractInfo::nom_read(&buffer).unwrap();
        assert!(rest.is_empty());
        assert_eq!(decoded, info);
    }

    /// A contract just originated reads its record back through the bounded
    /// `store_read` path (`read_info`, capped at [INFO_MAX_BYTES]) and gets
    /// exactly what `init` wrote — the generous bound being larger than the
    /// record is the normal case and must not corrupt the read.
    #[test]
    fn test_bounded_record_read_roundtrips() {
        let mut host = MockKernelHost::default();
        let account = init_with_storage(&mut host, 20);

        let info = account.read_info(&host).unwrap();
        assert_eq!(info.code_size, Narith::from(30u64));
        assert_eq!(info.storage_size, Narith::from(20u64));
        assert_eq!(info.used_bytes, Zarith::from(50u64));
        assert_eq!(info.paid_bytes, Zarith::from(50u64));
    }

    /// Origination writes the aggregated record with the counters computed
    /// from the code and storage blob lengths.
    #[test]
    fn test_init_writes_aggregated_record() {
        let mut host = MockKernelHost::default();
        let context = context::TezlinkContext::init_context();
        let contract = Contract::from_b58check(KT1).unwrap();
        let account = context.originated_from_contract(&contract).unwrap();

        let code = vec![0xab_u8; 30];
        let storage = vec![0xcd_u8; 20];
        account
            .init(&mut host, Some(&code), &storage, 0.into())
            .unwrap();

        let info: OriginatedContractInfo =
            read_optional_nom_value(&host, &context::code::info_path(&account).unwrap())
                .unwrap()
                .expect("the aggregated record must be present after init");
        assert_eq!(info.code_size, Narith::from(30u64));
        assert_eq!(info.storage_size, Narith::from(20u64));
        assert_eq!(info.used_bytes, Zarith::from(50u64));
        assert_eq!(info.paid_bytes, Zarith::from(50u64));
    }
}
