// SPDX-FileCopyrightText: 2022-2023 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023 Functori <contact@functori.com>
// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

//! Tezos account state and storage

use crate::context::{code, contracts};
use crate::enshrined_contracts::{self, EnshrinedContracts};
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
    host::RuntimeError,
    types::{PublicKey, PublicKeyHash},
};
use tezos_smart_rollup_host::path::RefPath;
use tezos_smart_rollup_host::storage::StorageV1;
use tezos_smart_rollup_keyspace::extensions::{KeySpaceExtBin, KeySpaceExtRlp};
use tezos_smart_rollup_keyspace::{Key, KeyError, KeySpace, KeySpaceError, ReadKind};
use tezos_storage::error::StorageReadErrorKind;
use tezos_storage::keyspace::KeySpaceExtBounded;
use tezosx_interfaces::{Origin, TezosXRuntimeError};

// Key of the RLP info record, under `key_to_implicit_account_prefix`. The
// record holds the balance, the counter and, after a reveal, the public key.
const INFO_KEY: Key = Key::from_static(b"/info");

/// Upper bound, in bytes, on a `Narith`-encoded balance or counter.
/// Both hold values that fit in a `u64` in practice — balance is bounded
/// by total supply, counters by the operation count — and a `Narith` of
/// a `u64` is at most 10 bytes. 32 leaves generous headroom and matches
/// the gas upper-bound convention (see `COUNTER_SIZE` in `lib.rs`).
///
/// Used to read each field in a single `store_read` host call instead of
/// the `store_value_size` + `store_read` pair that
/// [`KeySpaceExtBin::read_nom`] performs. See
/// [`KeySpaceExtBounded::read_nom_bounded`].
const NARITH_FIELD_MAX_BYTES: usize = 32;

/// Upper bound, in bytes, on an encoded [`OriginatedContractInfo`]. The
/// record is `tup4(n, n, z, z)`: `code_size` and `storage_size` as
/// `Narith` and `used_bytes`/`paid_bytes` as `Zarith`. Each field holds a
/// byte count that fits in a `u64` in practice — bounded by the contract
/// size and storage limits — so each encodes to at most ~10/11 bytes,
/// roughly 42 bytes for the whole record. 64 leaves generous headroom. One
/// bounded `store_read` host call then reads the whole record, in place of
/// the `store_value_size` + `store_read` pair that
/// [`KeySpaceExtBin::read_nom`] performs. See
/// [`KeySpaceExtBounded::read_nom_bounded_with_len`].
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

pub trait TezosAccount {
    fn contract(&self) -> Contract;

    /// Get the **balance** of an account in Mutez held by the account.
    /// Each account kind reads it from its own storage layout (RLP `/info`
    /// for implicit accounts, the `/balance` key for originated ones), so
    /// there is no shared default.
    fn balance(
        &self,
        tez_accounts: &impl KeySpace,
    ) -> Result<Narith, tezos_storage::error::Error>;

    /// Set the **balance** of an account in Mutez held by the account.
    fn set_balance(
        &self,
        tez_accounts: &mut impl KeySpace,
        balance: &Narith,
    ) -> Result<(), tezos_storage::error::Error>;

    /// Add amount (in Mutez) to the **balance** held by the account.
    ///
    /// Delegates to `self.balance()` and `self.set_balance()`, so each account
    /// kind's own implementation of those (e.g. the implicit account's RLP
    /// `/info` storage) is used without needing to provide `add_balance` too.
    ///
    /// TODO: In TezosX, this causes two storage reads + two RLP decodes
    /// (one in balance(), one in set_balance()). If add_balance becomes
    /// a hot path, TezosX should override it with a single
    /// read-modify-write on the RLP blob.
    fn add_balance(
        &self,
        tez_accounts: &mut impl KeySpace,
        amount: u64,
    ) -> Result<(), tezos_storage::error::Error> {
        let current = self.balance(tez_accounts)?;
        self.set_balance(tez_accounts, &Narith(current.0 + amount))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TezosOriginatedAccount {
    pub(crate) kt1: ContractKt1Hash,
    /// The key of the account in the accounts keyspace. Every key of the
    /// account starts with it. It must equal [`contracts::account_key`] of
    /// the originated contract `kt1`.
    pub(crate) key: Key,
}

impl TezosAccount for TezosOriginatedAccount {
    fn contract(&self) -> Contract {
        Contract::Originated(self.kt1.clone())
    }

    fn balance(
        &self,
        tez_accounts: &impl KeySpace,
    ) -> Result<Narith, tezos_storage::error::Error> {
        let key = contracts::balance_key(self)?;
        Ok(tez_accounts
            .read_nom_bounded(&key, NARITH_FIELD_MAX_BYTES)?
            .unwrap_or(0_u64.into()))
    }

    fn set_balance(
        &self,
        tez_accounts: &mut impl KeySpace,
        balance: &Narith,
    ) -> Result<(), tezos_storage::error::Error> {
        let key = contracts::balance_key(self)?;
        tez_accounts
            .store_bin(&key, balance)
            .map_err(tezos_storage::error::Error::from)
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
/// persisted at [`code::info_key`]. It bundles the four small
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

impl TezosOriginatedAccount {
    pub fn kt1(&self) -> &ContractKt1Hash {
        &self.kt1
    }

    pub fn code(
        &self,
        host: &impl StorageV1,
        tez_accounts: &impl KeySpace,
    ) -> Result<Code, tezos_storage::error::Error> {
        if let Some(c) = enshrined_contracts::from_kt1(self.kt1()) {
            return Ok(Code::Enshrined(c));
        }
        let code_key = code::code_key(self)?;
        match tez_accounts.get(&code_key) {
            Some(code) => Ok(Code::Code(code)),
            // A KT1 with no `/data/code` is either a Tezos X alias — whose
            // script is the single shared implementation — or a genuinely
            // missing contract. Resolving on the code-miss keeps the common
            // path at one read and never reads `/origin` for ordinary
            // contracts; the gate on `is_alias` also means implicit `tz1`
            // accounts (which are never `TezosOriginatedAccount` values) are
            // untouched.
            None => {
                if self.is_alias(tez_accounts)? {
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
                    Err(KeySpaceError::read(
                        tez_accounts.name(),
                        &code_key,
                        ReadKind::Ext(StorageReadErrorKind::NotFound),
                    )
                    .into())
                }
            }
        }
    }

    /// Reads the classification record ([`Origin`]) at the account's `/origin`
    /// key. Returns `None` when the account carries no classification, or when
    /// the host fails to read it. Fails when the bytes are not exactly one
    /// [`Origin`] encoding.
    pub fn origin(
        &self,
        tez_accounts: &impl KeySpace,
    ) -> Result<Option<Origin>, tezos_storage::error::Error> {
        tez_accounts
            .read_nom(&code::origin_key(self)?)
            .map_err(tezos_storage::error::Error::from)
    }

    /// Write the classification record (`Origin`) at the account's `/origin`
    /// key.
    pub fn set_origin(
        &self,
        tez_accounts: &mut impl KeySpace,
        origin: &Origin,
    ) -> Result<(), tezos_storage::error::Error> {
        tez_accounts
            .store_bin(&code::origin_key(self)?, origin)
            .map_err(tezos_storage::error::Error::from)
    }

    /// Whether this originated account is a Tezos X alias (classified
    /// [`Origin::Alias`]), whose script resolves to the shared implementation.
    pub fn is_alias(
        &self,
        tez_accounts: &impl KeySpace,
    ) -> Result<bool, tezos_storage::error::Error> {
        Ok(matches!(self.origin(tez_accounts)?, Some(Origin::Alias(_))))
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
    pub fn exists(
        &self,
        tez_accounts: &impl KeySpace,
    ) -> Result<bool, tezos_storage::error::Error> {
        if enshrined_contracts::is_enshrined(self.kt1()) {
            return Ok(true);
        }
        if tez_accounts.contains(&code::code_key(self)?) {
            return Ok(true);
        }
        // A code-less KT1 exists iff it is an alias.
        self.is_alias(tez_accounts)
    }

    /// Reads the aggregated [`OriginatedContractInfo`] for this contract.
    ///
    /// It reads the single `/info` record with one bounded read of
    /// `tez_accounts` (see [`INFO_MAX_BYTES`]). A contract with no record, or
    /// whose record the host fails to read, reads as an all-zero record. Fails
    /// if the bytes that it reads do not decode. When the record is longer
    /// than [`INFO_MAX_BYTES`], the error says so.
    pub fn read_info(
        &self,
        tez_accounts: &impl KeySpace,
    ) -> Result<OriginatedContractInfo, tezos_storage::error::Error> {
        Ok(self.read_info_with_len(tez_accounts)?.0)
    }

    /// Like [Self::read_info], but also returns the number of bytes read
    /// from the `/info` key, so the transfer path can charge the read at
    /// the exact bytes the store returned instead of re-encoding the
    /// record. A contract with no record yet reads as an all-zero record
    /// of length `0`.
    pub fn read_info_with_len(
        &self,
        tez_accounts: &impl KeySpace,
    ) -> Result<(OriginatedContractInfo, u64), tezos_storage::error::Error> {
        let info_key = code::info_key(self)?;
        Ok(tez_accounts
            .read_nom_bounded_with_len(&info_key, INFO_MAX_BYTES)?
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
            ))
    }

    /// Persist the aggregated record in a single host write.
    pub fn write_info(
        &self,
        tez_accounts: &mut impl KeySpace,
        info: &OriginatedContractInfo,
    ) -> Result<(), tezos_storage::error::Error> {
        let info_key = code::info_key(self)?;
        tez_accounts
            .store_bin(&info_key, info)
            .map_err(tezos_storage::error::Error::from)
    }

    /// Only the test suite writes the code blob in isolation: `init`
    /// inlines its own `/data/code` write and refreshes the aggregated
    /// record in one shot, so there is no production caller. Gated to
    /// `test` so it doesn't masquerade as production API.
    #[cfg(test)]
    pub fn set_code(
        &self,
        tez_accounts: &mut impl KeySpace,
        data: &[u8],
    ) -> Result<Zarith, tezos_storage::error::Error> {
        let path = code::code_key(self)?;
        tez_accounts.set(&path, data)?;
        let code_size = data.len() as u64;
        let mut info = self.read_info(tez_accounts)?;
        info.code_size = code_size.into();
        self.write_info(tez_accounts, &info)?;
        Ok(code_size.into())
    }

    pub fn storage(
        &self,
        tez_accounts: &impl KeySpace,
    ) -> Result<Vec<u8>, tezos_storage::error::Error> {
        if enshrined_contracts::is_enshrined(self.kt1()) {
            return Ok(vec![]);
        }
        let key = code::storage_key(self)?;
        tez_accounts.get(&key).ok_or_else(|| {
            KeySpaceError::read(
                tez_accounts.name(),
                &key,
                ReadKind::Ext(StorageReadErrorKind::NotFound),
            )
            .into()
        })
    }

    pub fn code_size(
        &self,
        tez_accounts: &impl KeySpace,
    ) -> Result<Zarith, tezos_storage::error::Error> {
        Ok(self.read_info(tez_accounts)?.code_size.into())
    }

    pub fn storage_size(
        &self,
        tez_accounts: &mut impl KeySpace,
    ) -> Result<Narith, tezos_storage::error::Error> {
        Ok(self.read_info(tez_accounts)?.storage_size)
    }

    /// Returns the contract's `used_bytes` watermark, or `0` if nothing
    /// has been written yet.
    pub fn used_bytes(
        &self,
        tez_accounts: &impl KeySpace,
    ) -> Result<Zarith, tezos_storage::error::Error> {
        Ok(self.read_info(tez_accounts)?.used_bytes)
    }

    /// Returns the contract's `paid_bytes` watermark, or `0` if nothing
    /// has been written yet.
    pub fn paid_bytes(
        &self,
        tez_accounts: &impl KeySpace,
    ) -> Result<Zarith, tezos_storage::error::Error> {
        Ok(self.read_info(tez_accounts)?.paid_bytes)
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
    pub fn set_storage(
        &self,
        tez_accounts: &mut impl KeySpace,
        data: &[u8],
    ) -> Result<StorageDelta, tezos_storage::error::Error> {
        if enshrined_contracts::is_enshrined(self.kt1()) {
            return Ok(0.into());
        }
        let path = code::storage_key(self)?;
        tez_accounts.set(&path, data)?;
        let mut info = self.read_info(tez_accounts)?;
        let new_storage_size = data.len() as u64;
        let prev_storage_size = Zarith::from(info.storage_size.clone()).0;
        info.storage_size = new_storage_size.into();
        self.write_info(tez_accounts, &info)?;
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
    pub fn init(
        &self,
        tez_accounts: &mut impl KeySpace,
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
                let code_key = code::code_key(self)?;
                tez_accounts.set(&code_key, code)?;
                code.len() as u64
            }
            None => 0,
        };
        let storage_key = code::storage_key(self)?;
        tez_accounts.set(&storage_key, storage)?;
        let storage_size = storage.len() as u64;
        let info = OriginatedContractInfo::for_new_contract(
            code_size,
            storage_size,
            lazy_storage_size_diff,
        );
        self.write_info(tez_accounts, &info)?;
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
    pub fn update_storage_space(
        &self,
        tez_accounts: &mut impl KeySpace,
        storage_size_diff: StorageDelta,
        lazy_storage_size_diff: Zarith,
    ) -> Result<StorageSpace, tezos_storage::error::Error> {
        if enshrined_contracts::is_enshrined(self.kt1()) {
            return Ok(StorageSpace {
                used_bytes: 0.into(),
                allocated_bytes: 0.into(),
            });
        }
        let mut info = self.read_info(tez_accounts)?;
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
        self.write_info(tez_accounts, &info)?;
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
    pub fn write_storage_and_info(
        &self,
        tez_accounts: &mut impl KeySpace,
        data: &[u8],
        info: &OriginatedContractInfo,
    ) -> Result<(), tezos_storage::error::Error> {
        if enshrined_contracts::is_enshrined(self.kt1()) {
            return Ok(());
        }
        let path = code::storage_key(self)?;
        tez_accounts.set(&path, data)?;
        self.write_info(tez_accounts, info)
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

/// Returns the key under which every key of the implicit account
/// `pub_key_hash` sits, relative to the accounts keyspace.
///
/// The RLP info record of the account is at `/info` under this key. Returns a
/// [`KeyError`] if the bytes do not form a valid keyspace key.
pub fn key_to_implicit_account_prefix(
    pub_key_hash: &PublicKeyHash,
) -> Result<Key, KeyError> {
    Key::from_bytes(format!("/tezosx/{pub_key_hash}").as_bytes())
}

/// Returns the key of the RLP info record of the implicit account
/// `pub_key_hash`, under [`key_to_implicit_account_prefix`] in the accounts
/// keyspace.
///
/// Returns a [`KeyError`] if the resulting bytes are not a valid keyspace key.
pub fn key_to_tezos_account(pub_key_hash: &PublicKeyHash) -> Result<Key, KeyError> {
    key_to_implicit_account_prefix(pub_key_hash)?.concat(&INFO_KEY)
}

/// Reads the info record at `key` as [`get_tezos_account_info`] does. `key`
/// must come from [`key_to_tezos_account`].
fn get_tezos_account_info_at(
    tez_accounts: &impl KeySpace,
    key: &Key,
) -> Result<Option<TezosAccountInfo>, TezosXRuntimeError> {
    Ok(tez_accounts
        .read_rlp(key)
        .map_err(|_| RuntimeError::DecodingError)?)
}

/// Returns the info record of the implicit account `pub_key_hash` in
/// `tez_accounts`.
///
/// Returns `None` when the record is absent or when the host fails to read it.
///
/// Fails with [`TezosXRuntimeError::Key`] when the key of `pub_key_hash` is
/// invalid. Fails with [`RuntimeError::DecodingError`] inside
/// [`TezosXRuntimeError::Runtime`] when the stored bytes do not decode into a
/// [`TezosAccountInfo`].
pub fn get_tezos_account_info(
    tez_accounts: &impl KeySpace,
    pub_key_hash: &PublicKeyHash,
) -> Result<Option<TezosAccountInfo>, TezosXRuntimeError> {
    let key = key_to_tezos_account(pub_key_hash)?;
    get_tezos_account_info_at(tez_accounts, &key)
}

/// Reads the implicit-account info record at `key`, as
/// [`get_tezos_account_info_at`] does, and returns the default record when it
/// is absent or when the host fails to read it.
fn get_tezos_account_info_or_init_at(
    tez_accounts: &impl KeySpace,
    key: &Key,
) -> Result<TezosAccountInfo, TezosXRuntimeError> {
    Ok(get_tezos_account_info_at(tez_accounts, key)?.unwrap_or_default())
}

/// Returns the info record of `pub_key_hash` as [`get_tezos_account_info`]
/// does, or the default record when the record is absent or unreadable.
pub fn get_tezos_account_info_or_init(
    tez_accounts: &impl KeySpace,
    pub_key_hash: &PublicKeyHash,
) -> Result<TezosAccountInfo, TezosXRuntimeError> {
    Ok(get_tezos_account_info(tez_accounts, pub_key_hash)?.unwrap_or_default())
}

/// Writes `info` at `key` as [`set_tezos_account_info`] does. `key` must come
/// from [`key_to_tezos_account`].
fn set_tezos_account_info_at(
    tez_accounts: &mut impl KeySpace,
    key: &Key,
    info: TezosAccountInfo,
) -> Result<(), TezosXRuntimeError> {
    tez_accounts
        .store_rlp(key, &info)
        .map_err(|e| TezosXRuntimeError::Custom(e.to_string()))?;
    Ok(())
}

/// Replaces the info record of the implicit account `pub_key_hash` in
/// `tez_accounts` with the RLP encoding of `info`.
///
/// Fails with [`TezosXRuntimeError::Key`] when the key of `pub_key_hash` is
/// invalid, and with [`TezosXRuntimeError::Custom`] when the write fails.
pub fn set_tezos_account_info(
    tez_accounts: &mut impl KeySpace,
    pub_key_hash: &PublicKeyHash,
    info: TezosAccountInfo,
) -> Result<(), TezosXRuntimeError> {
    let key = key_to_tezos_account(pub_key_hash)?;
    set_tezos_account_info_at(tez_accounts, &key, info)
}

pub struct TezosImplicitAccount {
    pub(crate) pkh: PublicKeyHash,
    /// Key of the account's RLP info record in the accounts keyspace, as
    /// [`key_to_tezos_account`] derives it from `pkh`.
    pub(crate) key: Key,
}

impl TezosAccount for TezosImplicitAccount {
    fn contract(&self) -> Contract {
        Contract::Implicit(self.pkh.clone())
    }

    fn balance(
        &self,
        tez_accounts: &impl KeySpace,
    ) -> Result<Narith, tezos_storage::error::Error> {
        match get_tezos_account_info_at(tez_accounts, &self.key) {
            Ok(Some(info)) => Ok(u256_to_narith(&info.balance)),
            Ok(None) => Ok(Narith::from(0u64)),
            Err(e) => Err(tezos_storage::error::Error::TcError(format!("{e}"))),
        }
    }

    fn set_balance(
        &self,
        tez_accounts: &mut impl KeySpace,
        balance: &Narith,
    ) -> Result<(), tezos_storage::error::Error> {
        let mut info = get_tezos_account_info_or_init_at(tez_accounts, &self.key)
            .map_err(|e| tezos_storage::error::Error::TcError(format!("{e}")))?;
        info.balance = narith_to_u256(balance)
            .map_err(|e| tezos_storage::error::Error::TcError(format!("{e}")))?;
        set_tezos_account_info_at(tez_accounts, &self.key, info)
            .map_err(|e| tezos_storage::error::Error::TcError(format!("{e}")))
    }
}

impl TezosImplicitAccount {
    pub fn pkh(&self) -> &PublicKeyHash {
        &self.pkh
    }

    pub fn counter(
        &self,
        tez_accounts: &impl KeySpace,
    ) -> Result<Narith, tezos_storage::error::Error> {
        match get_tezos_account_info_at(tez_accounts, &self.key) {
            Ok(Some(info)) => Ok(Narith::from(info.nonce)),
            Ok(None) => Ok(Narith::from(0u64)),
            Err(e) => Err(tezos_storage::error::Error::NomReadError(format!("{e}"))),
        }
    }

    pub fn set_counter(
        &self,
        tez_accounts: &mut impl KeySpace,
        counter: &Narith,
    ) -> Result<(), tezos_storage::error::Error> {
        let mut info = get_tezos_account_info_or_init_at(tez_accounts, &self.key)
            .map_err(|e| tezos_storage::error::Error::NomReadError(format!("{e}")))?;
        info.nonce = counter
            .0
            .clone()
            .try_into()
            .map_err(|e| tezos_storage::error::Error::NomReadError(format!("{e}")))?;
        set_tezos_account_info_at(tez_accounts, &self.key, info)
            .map_err(|e| tezos_storage::error::Error::NomReadError(format!("{e}")))
    }

    pub fn manager(
        &self,
        tez_accounts: &impl KeySpace,
    ) -> Result<Manager, tezos_storage::error::Error> {
        let info = get_tezos_account_info_at(tez_accounts, &self.key)
            .map_err(|e| tezos_storage::error::Error::NomReadError(format!("{e}")))?;
        match info {
            Some(info) => match info.pub_key {
                Some(pk) => Ok(Manager::Revealed(pk)),
                None => Ok(Manager::NotRevealed(self.pkh.clone())),
            },
            None => Ok(Manager::NotRevealed(self.pkh.clone())),
        }
    }

    pub fn set_manager_pk_hash_internal(
        &self,
        _tez_accounts: &mut impl KeySpace,
        _public_key_hash: &PublicKeyHash,
    ) -> Result<(), tezos_storage::error::Error> {
        // In TezosX, we do not need this function which is used in Tezlink
        // only for backward compatibility.
        Ok(())
    }

    pub fn set_manager_public_key(
        &self,
        tez_accounts: &mut impl KeySpace,
        public_key: &tezos_smart_rollup::types::PublicKey,
    ) -> Result<(), tezos_storage::error::Error> {
        let mut info = get_tezos_account_info_or_init_at(tez_accounts, &self.key)
            .map_err(|e| tezos_storage::error::Error::NomReadError(format!("{e}")))?;
        info.pub_key = Some(public_key.clone());
        set_tezos_account_info_at(tez_accounts, &self.key, info)
            .map_err(|e| tezos_storage::error::Error::NomReadError(format!("{e}")))?;
        Ok(())
    }

    pub fn allocated(
        &self,
        tez_accounts: &impl KeySpace,
    ) -> Result<bool, tezos_storage::error::Error> {
        match get_tezos_account_info_at(tez_accounts, &self.key) {
            Ok(Some(_)) => Ok(true),
            Ok(None) => Ok(false),
            Err(e) => Err(tezos_storage::error::Error::NomReadError(format!("{e}"))),
        }
    }

    /// Set the **counter** for the Tezos account to the successor of the current value.
    pub fn increment_counter(
        &self,
        tez_accounts: &mut impl KeySpace,
        validated_operations_count: usize,
    ) -> Result<(), tezos_storage::error::Error> {
        self.set_counter(
            tez_accounts,
            &Narith(self.counter(tez_accounts)?.0 + validated_operations_count),
        )
    }

    pub fn set_manager_public_key_hash(
        &self,
        tez_accounts: &mut impl KeySpace,
    ) -> Result<(), tezos_storage::error::Error> {
        self.set_manager_pk_hash_internal(tez_accounts, self.pkh())
    }

    /// This function is used to test a situation in which we have an
    /// inconsistent manager pkh for an implicit account.
    #[cfg(test)]
    pub fn force_set_manager_public_key_hash(
        &self,
        tez_accounts: &mut impl KeySpace,
        pkh: &PublicKeyHash,
    ) -> Result<(), tezos_storage::error::Error> {
        self.set_manager_pk_hash_internal(tez_accounts, pkh)
    }

    /// Allocate an account in the durable storage. Does nothing if account was
    /// already allocated.
    pub fn allocate(
        &self,
        tez_accounts: &mut impl KeySpace,
    ) -> Result<bool, tezos_storage::error::Error> {
        if self.allocated(tez_accounts)? {
            return Ok(true);
        }
        set_tezos_account_info_at(tez_accounts, &self.key, TezosAccountInfo::default())
            .map_err(|e| tezos_storage::error::Error::TcError(format!("{e}")))?;
        // TODO: use a global counter instead of initializing counter at 0
        self.set_counter(tez_accounts, &0u64.into())?;
        self.set_manager_public_key_hash(tez_accounts)?;
        Ok(false)
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

    #[test]
    fn set_manager_public_key_reveals_manager() {
        use super::Manager;
        use tezos_crypto_rs::{public_key::PublicKey, public_key_hash::PublicKeyHash};
        use tezos_evm_runtime::runtime::MockKernelHost;
        use tezos_evm_runtime::runtime_keyspaces::RuntimeKeyspaces;

        let mut host = MockKernelHost::default();
        let mut rk = RuntimeKeyspaces::init(&mut host).unwrap();
        let pkh =
            PublicKeyHash::from_b58check("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx").unwrap();
        let public_key = PublicKey::from_b58check(
            "edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav",
        )
        .unwrap();
        let account = crate::context::implicit_from_public_key_hash(&pkh).unwrap();

        // Reveal records the public key as the account's manager. It writes no
        // `/origin` record — an implicit account is Native by construction.
        account
            .set_manager_public_key(rk.tez_accounts_mut(), &public_key)
            .unwrap();
        assert_eq!(
            account.manager(rk.tez_accounts()).unwrap(),
            Manager::Revealed(public_key)
        );
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::account_storage::{
        narith_to_u256, set_tezos_account_info, TezosAccountInfo, TezosImplicitAccount,
    };
    use crate::context::{
        implicit_from_contract, implicit_from_public_key_hash, originated_from_contract,
    };
    use primitive_types::U256;
    use tezos_crypto_rs::PublicKeyWithHash;
    use tezos_evm_runtime::runtime::MockKernelHost;
    use tezos_evm_runtime::runtime_keyspaces::RuntimeKeyspaces;

    /// Build the live implicit account for `pkh` (RLP `/info` layout).
    fn implicit(pkh: &PublicKeyHash) -> TezosImplicitAccount {
        implicit_from_public_key_hash(pkh).expect("implicit account resolution")
    }

    /// Build the live implicit account for an implicit `contract`.
    fn implicit_of(contract: &Contract) -> TezosImplicitAccount {
        implicit_from_contract(contract).expect("implicit account resolution")
    }

    /// Build the originated account for `contract` under the test root.
    fn originated(contract: &Contract) -> TezosOriginatedAccount {
        originated_from_contract(contract).expect("originated account resolution")
    }

    /// obtained by `octez-client show address bootstrap1` in mockup mode
    const BOOTSTRAP1_PKH: &str = "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx";
    const BOOTSTRAP1_PK: &str = "edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav";
    const KT1: &str = "KT1QbKzQAyJtzprfvUJZv8VGqwQNch2o89di";

    /// Seed the live RLP `/info` record for `pkh` directly via the durable
    /// storage helper, bypassing the account API, so the read tests below
    /// verify that the account reads from the right path / layout.
    fn seed_account_info(
        tez_accounts: &mut impl KeySpace,
        pkh: &PublicKeyHash,
        info: TezosAccountInfo,
    ) {
        set_tezos_account_info(tez_accounts, pkh, info)
            .expect("seeding the account info should succeed");
    }

    // Read tests seed the live RLP `/info` record directly (not through the
    // account API) to verify the account reads at the right path / layout.
    #[test]
    fn test_read_balance() {
        let mut host = MockKernelHost::default();
        let mut rk = RuntimeKeyspaces::init(&mut host).unwrap();

        let balance: Narith = 2944_u64.into();

        let pkh = PublicKeyHash::from_b58check(BOOTSTRAP1_PKH)
            .expect("PublicKeyHash base58 conversion should succeeded");

        seed_account_info(
            rk.tez_accounts_mut(),
            &pkh,
            TezosAccountInfo {
                balance: narith_to_u256(&balance).unwrap(),
                nonce: 0,
                pub_key: None,
            },
        );

        let account = implicit(&pkh);

        let read_balance = account
            .balance(rk.tez_accounts())
            .expect("read_balance should have succeeded");

        assert_eq!(balance, read_balance);
    }

    #[test]
    fn test_read_counter() {
        let mut host = MockKernelHost::default();
        let mut rk = RuntimeKeyspaces::init(&mut host).unwrap();

        let counter: Narith = 3u64.into();

        let pkh = PublicKeyHash::from_b58check(BOOTSTRAP1_PKH)
            .expect("PublicKeyHash base58 conversion should succeeded");

        seed_account_info(
            rk.tez_accounts_mut(),
            &pkh,
            TezosAccountInfo {
                balance: U256::zero(),
                nonce: 3,
                pub_key: None,
            },
        );

        let account = implicit(&pkh);

        let read_counter = account
            .counter(rk.tez_accounts())
            .expect("read_counter should have succeeded");

        assert_eq!(counter, read_counter);
    }

    #[test]
    fn test_set_and_read_balance() {
        let mut host = MockKernelHost::default();
        let mut rk = RuntimeKeyspaces::init(&mut host).unwrap();

        let balance = 4579_u64.into();

        let contract = Contract::from_b58check(BOOTSTRAP1_PKH)
            .expect("Contract base58 conversion should succeeded");

        let account = implicit_of(&contract);

        let () = account
            .set_balance(rk.tez_accounts_mut(), &balance)
            .expect("set_balance should succeeded");

        let read_balance = account
            .balance(rk.tez_accounts())
            .expect("read_balance should have succeeded");

        assert_eq!(balance, read_balance);
    }

    #[test]
    fn test_set_and_read_counter() {
        let mut host = MockKernelHost::default();
        let mut rk = RuntimeKeyspaces::init(&mut host).unwrap();

        let counter: Narith = 6u64.into();

        let contract = Contract::from_b58check(BOOTSTRAP1_PKH)
            .expect("Contract base58 conversion should succeeded");

        let account = implicit_of(&contract);

        let () = account
            .set_counter(rk.tez_accounts_mut(), &counter)
            .expect("set_counter should have succeeded");

        let read_counter = account
            .counter(rk.tez_accounts())
            .expect("read_counter should have succeeded");

        assert_eq!(counter, read_counter);
    }

    #[test]
    fn test_read_manager() {
        let mut host = MockKernelHost::default();
        let mut rk = RuntimeKeyspaces::init(&mut host).unwrap();

        let pkh = PublicKeyHash::from_b58check(BOOTSTRAP1_PKH)
            .expect("PublicKeyHash base58 conversion should succeeded");
        let public_key = PublicKey::from_b58check(BOOTSTRAP1_PK).unwrap();

        seed_account_info(
            rk.tez_accounts_mut(),
            &pkh,
            TezosAccountInfo {
                balance: U256::zero(),
                nonce: 0,
                pub_key: Some(public_key),
            },
        );

        let account = implicit(&pkh);

        let manager = account
            .manager(rk.tez_accounts())
            .expect("Can't read manager");

        let read_pkh = match manager {
            Manager::NotRevealed(pkh) => {
                panic!("Manager should be revealed (manager key is a pkh: {pkh:?})")
            }
            Manager::Revealed(pk) => pk.pk_hash(),
        };

        assert_eq!(Contract::Implicit(pkh), Contract::Implicit(read_pkh));
    }

    #[test]
    fn test_set_read_manager_public_key() {
        let mut host = MockKernelHost::default();
        let mut rk = RuntimeKeyspaces::init(&mut host).unwrap();

        // Create an account for bootstrap1
        let contract = Contract::from_b58check(BOOTSTRAP1_PKH)
            .expect("Contract base58 conversion should succeeded");

        let account = implicit_of(&contract);

        let public_key = PublicKey::from_b58check(BOOTSTRAP1_PK).unwrap();

        let () = account
            .set_manager_public_key(rk.tez_accounts_mut(), &public_key)
            .expect("set_manager_public_key should have succeeded");

        let manager = Manager::Revealed(public_key);

        let read_manager = account
            .manager(rk.tez_accounts())
            .expect("read_manager should have succeeded");

        assert_eq!(manager, read_manager);
    }

    #[test]
    fn test_set_read_manager_public_key_hash() {
        let mut host = MockKernelHost::default();
        let mut rk = RuntimeKeyspaces::init(&mut host).unwrap();

        // Create an account for bootstrap1
        let pkh = PublicKeyHash::from_b58check(BOOTSTRAP1_PKH)
            .expect("PublicKeyHash base58 conversion should succeeded");

        let contract = Contract::Implicit(pkh);
        let account = implicit_of(&contract);

        let pkh = PublicKeyHash::from_b58check(BOOTSTRAP1_PKH)
            .expect("PublicKeyHash base58 conversion should succeeded");

        let () = account
            .set_manager_public_key_hash(rk.tez_accounts_mut())
            .expect("set_manager_public_key_hash should have succeeded");

        let manager = Manager::NotRevealed(pkh);

        let read_manager = account
            .manager(rk.tez_accounts())
            .expect("read_manager should have succeeded");

        assert_eq!(manager, read_manager);
    }

    #[test]
    fn test_account_initialization() {
        let mut host = MockKernelHost::default();
        let mut rk = RuntimeKeyspaces::init(&mut host).unwrap();

        // Create an account for bootstrap1
        let pkh = PublicKeyHash::from_b58check(BOOTSTRAP1_PKH)
            .expect("PublicKeyHash base58 conversion should succeeded");

        let contract = Contract::Implicit(pkh);
        let account = implicit_of(&contract);

        let exist = account
            .allocated(rk.tez_accounts())
            .expect("Exist account should have succeeded");

        assert!(!exist);

        account
            .allocate(rk.tez_accounts_mut())
            .expect("Account initialization should have succeeded");

        let exist = account
            .allocated(rk.tez_accounts())
            .expect("Exist account should have succeeded");

        assert!(exist);

        let test_balance = 1999_u64.into();

        account
            .set_balance(rk.tez_accounts_mut(), &test_balance)
            .expect("Set balance should have succeeded");

        // Calling init on a contract already initialized will do nothing
        // So the balance should not change and still be 1999
        account
            .allocate(rk.tez_accounts_mut())
            .expect("Account initialization should have succeeded");

        let read_balance = account
            .balance(rk.tez_accounts())
            .expect("Read balance should have succeeded");

        assert_eq!(test_balance, read_balance);
    }

    #[test]
    fn test_set_read_large_code() {
        let mut host = MockKernelHost::default();
        let mut rk = RuntimeKeyspaces::init(&mut host).unwrap();

        // Create an originated account for KT1
        let contract = Contract::from_b58check(KT1).unwrap();
        let account = originated(&contract);

        let code = vec![1u8; 10_000];

        // Set the code of the KT1
        account
            .set_code(rk.tez_accounts_mut(), &code)
            .expect("Setting code of the KT1 should succeed");

        let read_code = account
            .code(rk.host(), rk.tez_accounts())
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
        let mut rk = RuntimeKeyspaces::init(&mut host).unwrap();
        let contract = Contract::from_b58check(KT1).unwrap();
        let account = originated(&contract);

        let code = vec![0xab_u8; 30];
        let storage = vec![0xcd_u8; 20];
        let larger_storage = vec![0x12_u8; 50];

        let StorageSpace {
            allocated_bytes, ..
        } = account
            .init(rk.tez_accounts_mut(), Some(&code), &storage, 0.into())
            .unwrap();
        let initial_size = (code.len() + storage.len()) as u64;
        assert_eq!(allocated_bytes, Zarith::from(initial_size));
        assert_eq!(
            account.used_bytes(rk.tez_accounts()).unwrap(),
            Zarith::from(initial_size)
        );
        assert_eq!(
            account.paid_bytes(rk.tez_accounts()).unwrap(),
            Zarith::from(initial_size)
        );

        let diff = account
            .set_storage(rk.tez_accounts_mut(), &larger_storage)
            .unwrap();
        let StorageSpace {
            allocated_bytes, ..
        } = account
            .update_storage_space(rk.tez_accounts_mut(), diff, 0.into())
            .unwrap();
        let larger_size = (code.len() + larger_storage.len()) as u64;
        assert_eq!(allocated_bytes, Zarith::from(larger_size - initial_size));
        assert_eq!(
            account.used_bytes(rk.tez_accounts()).unwrap(),
            Zarith::from(larger_size)
        );
        assert_eq!(
            account.paid_bytes(rk.tez_accounts()).unwrap(),
            Zarith::from(larger_size)
        );
    }

    /// Writing a smaller value leaves the watermark unchanged and
    /// returns no delta; `used_bytes` is the live size.
    #[test]
    fn test_update_storage_space_after_shrink() {
        let mut host = MockKernelHost::default();
        let mut rk = RuntimeKeyspaces::init(&mut host).unwrap();
        let contract = Contract::from_b58check(KT1).unwrap();
        let account = originated(&contract);

        let code = vec![0xab_u8; 30];
        let storage = vec![0xcd_u8; 20];
        let smaller_storage = vec![0x34_u8; 5];

        account
            .init(rk.tez_accounts_mut(), Some(&code), &storage, 0.into())
            .unwrap();
        let initial_size = (code.len() + storage.len()) as u64;

        let diff = account
            .set_storage(rk.tez_accounts_mut(), &smaller_storage)
            .unwrap();
        let StorageSpace {
            allocated_bytes, ..
        } = account
            .update_storage_space(rk.tez_accounts_mut(), diff, 0.into())
            .unwrap();
        assert_eq!(allocated_bytes, Zarith::from(0u64));
        assert_eq!(
            account.used_bytes(rk.tez_accounts()).unwrap(),
            Zarith::from((code.len() + smaller_storage.len()) as u64),
        );
        assert_eq!(
            account.paid_bytes(rk.tez_accounts()).unwrap(),
            Zarith::from(initial_size)
        );
    }

    /// `set_storage` returns the signed byte-length delta.
    #[test]
    fn test_set_storage_returns_signed_diff() {
        let mut host = MockKernelHost::default();
        let mut rk = RuntimeKeyspaces::init(&mut host).unwrap();
        let contract = Contract::from_b58check(KT1).unwrap();
        let account = originated(&contract);

        let first = account
            .set_storage(rk.tez_accounts_mut(), &[0u8; 30])
            .unwrap();
        assert_eq!(first, 30.into(), "first write reports new_len");

        let grow = account
            .set_storage(rk.tez_accounts_mut(), &[0u8; 50])
            .unwrap();
        assert_eq!(grow, 20.into(), "grow reports new_len - prev_len");

        let shrink = account
            .set_storage(rk.tez_accounts_mut(), &[0u8; 5])
            .unwrap();
        assert_eq!(shrink, (-45).into(), "shrink reports the negative delta");

        let same = account
            .set_storage(rk.tez_accounts_mut(), &[1u8; 5])
            .unwrap();
        assert_eq!(same, 0.into(), "same-size overwrite reports zero delta");
    }

    /// `set_storage` early-returns `(0, 0)` on enshrined contracts and
    /// does not write to durable storage.
    #[test]
    fn test_set_storage_returns_zero_for_enshrined() {
        let mut host = MockKernelHost::default();
        let mut rk = RuntimeKeyspaces::init(&mut host).unwrap();

        // Gateway is an enshrined contract; set_storage early-returns on it
        // without touching durable storage.
        const GATEWAY_KT1: &str = "KT18oDJJKXMKhfE1bSuAPGp92pYcwVDiqsPw";
        let contract = Contract::from_b58check(GATEWAY_KT1).unwrap();
        let account = originated(&contract);

        let diff = account
            .set_storage(rk.tez_accounts_mut(), &[0xef_u8; 15])
            .unwrap();
        assert_eq!(
            diff,
            0.into(),
            "enshrined set_storage must report zero delta"
        );
        let StorageSpace {
            used_bytes,
            allocated_bytes,
        } = account
            .update_storage_space(rk.tez_accounts_mut(), diff, 0.into())
            .unwrap();
        assert_eq!(used_bytes, Zarith::from(0u64));
        assert_eq!(allocated_bytes, Zarith::from(0u64));

        let storage_key = code::storage_key(&account).unwrap();
        let info_key = code::info_key(&account).unwrap();
        assert!(!rk.tez_accounts().contains(&storage_key));
        assert!(!rk.tez_accounts().contains(&info_key));
    }

    /// `exists` is the guard the transfer pipeline uses to reject calls
    /// to never-originated KT1s before any state write. An enshrined
    /// contract must always report as existing — its code is synthetic
    /// and never written to durable storage.
    #[test]
    fn test_exists_returns_true_for_enshrined() {
        let mut host = MockKernelHost::default();
        let rk = RuntimeKeyspaces::init(&mut host).unwrap();

        const GATEWAY_KT1: &str = "KT18oDJJKXMKhfE1bSuAPGp92pYcwVDiqsPw";
        let contract = Contract::from_b58check(GATEWAY_KT1).unwrap();
        let account = originated(&contract);

        assert!(account.exists(rk.tez_accounts()).unwrap());
    }

    /// A regular originated KT1 with no code blob in storage must report
    /// as not existing — this is the case `transfer` rejects with
    /// `ContractDoesNotExist`.
    #[test]
    fn test_exists_returns_false_when_never_originated() {
        let mut host = MockKernelHost::default();
        let rk = RuntimeKeyspaces::init(&mut host).unwrap();

        let contract = Contract::from_b58check(KT1).unwrap();
        let account = originated(&contract);

        assert!(!account.exists(rk.tez_accounts()).unwrap());
    }

    /// After `set_code` writes a code blob at `/data/code`, `exists`
    /// must report as existing — `Origination` always writes a code blob,
    /// so the presence of one is the marker.
    #[test]
    fn test_exists_returns_true_after_set_code() {
        let mut host = MockKernelHost::default();
        let mut rk = RuntimeKeyspaces::init(&mut host).unwrap();

        let contract = Contract::from_b58check(KT1).unwrap();
        let account = originated(&contract);

        assert!(!account.exists(rk.tez_accounts()).unwrap());
        account
            .set_code(rk.tez_accounts_mut(), &[0xab_u8; 4])
            .unwrap();
        assert!(account.exists(rk.tez_accounts()).unwrap());
    }

    /// `used_bytes` and `paid_bytes` default to zero on a contract that
    /// was never written. The default holds for both freshly-created
    /// accounts (no prior `init`) and for the `paid_bytes` slot of a
    /// contract that has been initialised but whose watermark has not
    /// yet been bumped by the storage-payment pass.
    #[test]
    fn test_getters_default_to_zero_on_absent_path() {
        let mut host = MockKernelHost::default();
        let rk = RuntimeKeyspaces::init(&mut host).unwrap();
        let contract = Contract::from_b58check(KT1).unwrap();
        let account = originated(&contract);

        assert_eq!(
            account.used_bytes(rk.tez_accounts()).unwrap(),
            Zarith::from(0u64)
        );
        assert_eq!(
            account.paid_bytes(rk.tez_accounts()).unwrap(),
            Zarith::from(0u64)
        );
    }

    /// Helper: originate a contract with the given main-storage size,
    /// then return the live account.
    fn init_with_storage(
        tez_accounts: &mut impl KeySpace,
        storage_len: usize,
    ) -> TezosOriginatedAccount {
        let contract = Contract::from_b58check(KT1).unwrap();
        let account = originated(&contract);
        let code = vec![0xab_u8; 30];
        let storage = vec![0xcd_u8; storage_len];
        account
            .init(tez_accounts, Some(&code), &storage, 0.into())
            .unwrap();
        account
    }

    /// A positive `lazy_storage_size_diff` bumps `used_bytes` and the
    /// `paid_bytes` watermark by the same amount.
    #[test]
    fn test_update_storage_space_grows_on_positive_lazy_delta() {
        let mut host = MockKernelHost::default();
        let mut rk = RuntimeKeyspaces::init(&mut host).unwrap();
        let account = init_with_storage(rk.tez_accounts_mut(), 20);
        let prev_used = account.used_bytes(rk.tez_accounts()).unwrap();

        let StorageSpace {
            used_bytes,
            allocated_bytes,
        } = account
            .update_storage_space(rk.tez_accounts_mut(), 0.into(), (65 + 7).into())
            .unwrap();

        assert_eq!(used_bytes, Zarith(prev_used.0.clone() + (65 + 7)));
        assert_eq!(allocated_bytes, Zarith::from((65 + 7) as u64));
        assert_eq!(account.paid_bytes(rk.tez_accounts()).unwrap(), used_bytes);
    }

    /// A negative `lazy_storage_size_diff` lowers `used_bytes` and
    /// leaves `paid_bytes` (the watermark) unchanged.
    #[test]
    fn test_update_storage_space_shrinks_on_negative_lazy_delta() {
        let mut host = MockKernelHost::default();
        let mut rk = RuntimeKeyspaces::init(&mut host).unwrap();
        let account = init_with_storage(rk.tez_accounts_mut(), 20);
        let prev_used = account.used_bytes(rk.tez_accounts()).unwrap();
        let prev_paid = account.paid_bytes(rk.tez_accounts()).unwrap();

        // First grow above the watermark so we have headroom to shrink.
        account
            .update_storage_space(rk.tez_accounts_mut(), 0.into(), 200.into())
            .unwrap();
        let prev_paid_after_grow = account.paid_bytes(rk.tez_accounts()).unwrap();

        let StorageSpace {
            used_bytes,
            allocated_bytes,
        } = account
            .update_storage_space(rk.tez_accounts_mut(), 0.into(), (-100).into())
            .unwrap();

        assert_eq!(used_bytes, Zarith(prev_used.0 + 100));
        assert_eq!(allocated_bytes, Zarith::from(0u64));
        assert_eq!(
            account.paid_bytes(rk.tez_accounts()).unwrap(),
            prev_paid_after_grow
        );
        assert!(prev_paid_after_grow.0 > prev_paid.0);
    }

    /// Zero deltas on both sides are a no-op on `used_bytes` /
    /// `paid_bytes` and report `allocated_bytes = 0`.
    #[test]
    fn test_update_storage_space_unchanged_when_both_diffs_zero() {
        let mut host = MockKernelHost::default();
        let mut rk = RuntimeKeyspaces::init(&mut host).unwrap();
        let account = init_with_storage(rk.tez_accounts_mut(), 20);
        let prev_used = account.used_bytes(rk.tez_accounts()).unwrap();
        let prev_paid = account.paid_bytes(rk.tez_accounts()).unwrap();

        let StorageSpace {
            used_bytes,
            allocated_bytes,
        } = account
            .update_storage_space(rk.tez_accounts_mut(), 0.into(), 0.into())
            .unwrap();

        assert_eq!(used_bytes, prev_used);
        assert_eq!(allocated_bytes, Zarith::from(0u64));
        assert_eq!(account.paid_bytes(rk.tez_accounts()).unwrap(), prev_paid);
    }

    /// `used_bytes` is the sum of both deltas added to the previous
    /// watermark.
    #[test]
    fn test_update_storage_space_sums_storage_and_lazy_diffs() {
        let mut host = MockKernelHost::default();
        let mut rk = RuntimeKeyspaces::init(&mut host).unwrap();
        let account = init_with_storage(rk.tez_accounts_mut(), 20);
        let prev_used = account.used_bytes(rk.tez_accounts()).unwrap();

        let StorageSpace { used_bytes, .. } = account
            .update_storage_space(rk.tez_accounts_mut(), 11.into(), 22.into())
            .unwrap();

        assert_eq!(used_bytes, Zarith(prev_used.0 + 33));
    }

    /// Shrinking below the previous `paid_bytes` watermark leaves the
    /// watermark unchanged and reports `allocated_bytes = 0`.
    #[test]
    fn test_update_storage_space_shrink_below_paid_bytes_no_diff() {
        let mut host = MockKernelHost::default();
        let mut rk = RuntimeKeyspaces::init(&mut host).unwrap();
        let account = init_with_storage(rk.tez_accounts_mut(), 20);

        // Push the watermark up by 500 first.
        account
            .update_storage_space(rk.tez_accounts_mut(), 0.into(), 500.into())
            .unwrap();
        let paid_high = account.paid_bytes(rk.tez_accounts()).unwrap();

        // Now drop used_bytes by 400 (still below the watermark).
        let StorageSpace {
            used_bytes,
            allocated_bytes,
        } = account
            .update_storage_space(rk.tez_accounts_mut(), 0.into(), (-400).into())
            .unwrap();

        assert!(used_bytes.0 < paid_high.0);
        assert_eq!(allocated_bytes, Zarith::from(0u64));
        assert_eq!(account.paid_bytes(rk.tez_accounts()).unwrap(), paid_high);
    }

    /// A non-zero `lazy_storage_size_diff` at origination contributes
    /// to the initial `used_bytes` and `paid_bytes` watermarks.
    #[test]
    fn test_init_with_initial_lazy_storage() {
        let mut host = MockKernelHost::default();
        let mut rk = RuntimeKeyspaces::init(&mut host).unwrap();
        let contract = Contract::from_b58check(KT1).unwrap();
        let account = originated(&contract);

        let code = vec![0xab_u8; 30];
        let storage = vec![0xcd_u8; 20];
        let initial_lazy: u64 = 33 + 65 + 7;

        let StorageSpace {
            used_bytes,
            allocated_bytes,
        } = account
            .init(
                rk.tez_accounts_mut(),
                Some(&code),
                &storage,
                initial_lazy.into(),
            )
            .unwrap();

        let expected = (code.len() + storage.len()) as u64 + initial_lazy;
        assert_eq!(used_bytes, Zarith::from(expected));
        assert_eq!(allocated_bytes, Zarith::from(expected));
        assert_eq!(account.paid_bytes(rk.tez_accounts()).unwrap(), used_bytes);
    }

    /// Enshrined contracts short-circuit `update_storage_space` and
    /// return `(0, 0)` regardless of the deltas they receive.
    #[test]
    fn test_update_storage_space_enshrined_ignores_diffs() {
        let mut host = MockKernelHost::default();
        let mut rk = RuntimeKeyspaces::init(&mut host).unwrap();
        const GATEWAY_KT1: &str = "KT18oDJJKXMKhfE1bSuAPGp92pYcwVDiqsPw";
        let contract = Contract::from_b58check(GATEWAY_KT1).unwrap();
        let account = originated(&contract);

        let StorageSpace {
            used_bytes,
            allocated_bytes,
        } = account
            .update_storage_space(rk.tez_accounts_mut(), 999_999.into(), 999_999.into())
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
        tez_accounts: &mut impl KeySpace,
        account: &TezosOriginatedAccount,
    ) {
        use tezosx_interfaces::{AliasInfo, RuntimeId};
        let origin = Origin::Alias(AliasInfo {
            runtime: RuntimeId::Ethereum,
            native_address: "0xabc".to_string(),
        });
        account.set_origin(tez_accounts, &origin).unwrap();
    }

    /// The `Origin` classification round-trips through `set_origin` / `origin`:
    /// an account with no record reads as `None`, and both variants survive a
    /// write/read cycle.
    #[test]
    fn origin_roundtrip() {
        use tezosx_interfaces::{AliasInfo, RuntimeId};

        let mut host = MockKernelHost::default();
        let mut rk = RuntimeKeyspaces::init(&mut host).unwrap();
        let contract = Contract::from_b58check(KT1).unwrap();
        let account = originated(&contract);

        // An account with no `/origin` record carries no classification.
        assert!(account.origin(rk.tez_accounts()).unwrap().is_none());

        // The native variant round-trips.
        account
            .set_origin(rk.tez_accounts_mut(), &Origin::Native)
            .unwrap();
        assert_eq!(
            account.origin(rk.tez_accounts()).unwrap(),
            Some(Origin::Native)
        );

        // The alias variant round-trips.
        let alias = Origin::Alias(AliasInfo {
            runtime: RuntimeId::Tezos,
            native_address: "tz1...".to_string(),
        });
        account.set_origin(rk.tez_accounts_mut(), &alias).unwrap();
        assert_eq!(account.origin(rk.tez_accounts()).unwrap(), Some(alias));
    }

    /// A KT1 classified `Origin::Alias` and carrying no `/data/code` resolves to
    /// the shared implementation, and reports as existing.
    #[test]
    fn alias_code_resolves_to_shared_implementation() {
        let mut host = MockKernelHost::default();
        let mut rk = RuntimeKeyspaces::init(&mut host).unwrap();
        let contract = Contract::from_b58check(KT1).unwrap();
        let account = originated(&contract);

        classify_as_alias(rk.tez_accounts_mut(), &account);
        let implementation = vec![0x02u8, 0x00, 0x00, 0x00, 0x00];
        write_alias_implementation(rk.host_mut(), &implementation).unwrap();

        assert_eq!(
            account.code(rk.host(), rk.tez_accounts()).unwrap(),
            Code::Code(implementation)
        );
        assert!(account.exists(rk.tez_accounts()).unwrap());
    }

    /// An alias whose shared implementation has not been seeded is an error, not
    /// a silent fallback.
    #[test]
    fn alias_without_seeded_implementation_errors() {
        let mut host = MockKernelHost::default();
        let mut rk = RuntimeKeyspaces::init(&mut host).unwrap();
        let contract = Contract::from_b58check(KT1).unwrap();
        let account = originated(&contract);

        classify_as_alias(rk.tez_accounts_mut(), &account);
        // Pin the error type: an unseeded slot is an internal invariant breach,
        // not a generic/typecheck error (so it can't silently change meaning).
        assert!(matches!(
            account.code(rk.host(), rk.tez_accounts()),
            Err(tezos_storage::error::Error::Internal(_))
        ));
        // Still classified, so it exists.
        assert!(account.exists(rk.tez_accounts()).unwrap());
    }

    /// A code-less KT1 that is not an alias is a genuinely missing contract:
    /// `code()` errors and `exists()` is false. The `/origin` read only happens
    /// because `/data/code` is absent — a regular contract never pays for it.
    #[test]
    fn code_less_non_alias_is_missing() {
        let mut host = MockKernelHost::default();
        let rk = RuntimeKeyspaces::init(&mut host).unwrap();
        let contract = Contract::from_b58check(KT1).unwrap();
        let account = originated(&contract);

        // Pin the variant: a code-less non-alias must surface as the plain
        // missing-value error, and must not go through the alias branch (which
        // yields `Internal`).
        let code_key = code::code_key(&account).unwrap();
        assert_eq!(
            account.code(rk.host(), rk.tez_accounts()),
            Err(KeySpaceError::read(
                rk.tez_accounts().name(),
                &code_key,
                ReadKind::Ext(StorageReadErrorKind::NotFound),
            )
            .into())
        );
        assert!(!account.exists(rk.tez_accounts()).unwrap());
    }

    /// A code-less KT1 classified `Origin::Native` is not an alias either:
    /// still a genuinely missing contract. Guards against relaxing `is_alias`
    /// to `origin.is_some()`, which would flip every Native-classified code-less
    /// KT1 to the shared forwarder.
    #[test]
    fn code_less_native_classified_is_missing() {
        let mut host = MockKernelHost::default();
        let mut rk = RuntimeKeyspaces::init(&mut host).unwrap();
        let contract = Contract::from_b58check(KT1).unwrap();
        let account = originated(&contract);

        account
            .set_origin(rk.tez_accounts_mut(), &Origin::Native)
            .unwrap();

        let code_key = code::code_key(&account).unwrap();
        assert_eq!(
            account.code(rk.host(), rk.tez_accounts()),
            Err(KeySpaceError::read(
                rk.tez_accounts().name(),
                &code_key,
                ReadKind::Ext(StorageReadErrorKind::NotFound),
            )
            .into())
        );
        assert!(!account.exists(rk.tez_accounts()).unwrap());
    }

    /// A regular originated contract (with `/data/code`) is unaffected: its own
    /// script is returned and it exists, regardless of the shared slot.
    #[test]
    fn regular_contract_returns_own_code() {
        let mut host = MockKernelHost::default();
        let mut rk = RuntimeKeyspaces::init(&mut host).unwrap();
        let contract = Contract::from_b58check(KT1).unwrap();
        let account = originated(&contract);

        let own_code = vec![0x05u8, 0x01, 0x02, 0x03];
        account.set_code(rk.tez_accounts_mut(), &own_code).unwrap();

        assert_eq!(
            account.code(rk.host(), rk.tez_accounts()).unwrap(),
            Code::Code(own_code)
        );
        assert!(account.exists(rk.tez_accounts()).unwrap());
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
        let mut rk = RuntimeKeyspaces::init(&mut host).unwrap();
        let account = init_with_storage(rk.tez_accounts_mut(), 20);

        let info = account.read_info(rk.tez_accounts()).unwrap();
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
        let mut rk = RuntimeKeyspaces::init(&mut host).unwrap();
        let contract = Contract::from_b58check(KT1).unwrap();
        let account = originated(&contract);

        let code = vec![0xab_u8; 30];
        let storage = vec![0xcd_u8; 20];
        account
            .init(rk.tez_accounts_mut(), Some(&code), &storage, 0.into())
            .unwrap();

        let info: OriginatedContractInfo = rk
            .tez_accounts()
            .read_nom(&code::info_key(&account).unwrap())
            .unwrap()
            .expect("the aggregated record must be present after init");
        assert_eq!(info.code_size, Narith::from(30u64));
        assert_eq!(info.storage_size, Narith::from(20u64));
        assert_eq!(info.used_bytes, Zarith::from(50u64));
        assert_eq!(info.paid_bytes, Zarith::from(50u64));
    }
}
