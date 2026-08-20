// SPDX-FileCopyrightText: 2022-2023 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023, 2025-2026 Functori <contact@functori.com>
// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use revm::{
    primitives::{Address, Bytes, B256, KECCAK_EMPTY, U256},
    state::{AccountInfo as RevmAccountInfo, Bytecode},
};
use rlp::{Decodable, Encodable, Rlp};
use tezos_data_encoding::{enc::BinWriter, nom::NomReader};
use tezos_ethereum::keyspace::KeySpaceExtU256;
use tezos_smart_rollup_host::{path::RefPath, runtime::RuntimeError};
use tezos_smart_rollup_keyspace::extensions::KeySpaceExtNum;
use tezos_smart_rollup_keyspace::{Key, KeyError, KeySpace};
use tezosx_interfaces::{AliasInfo, Origin};

use evm_types::{FaDepositWithProxy, PrecompileStateError};

use crate::{error::EvmDbError, storage::code::CodeStorage};

/// Path where the L1 address of our withdrawals ticketer is stored.
pub const NATIVE_TOKEN_TICKETER_PATH: RefPath =
    RefPath::assert_from(b"/evm/world_state/ticketer");

/// Size of contract hash encoded in base58.
pub const KT1_B58_SIZE: usize = 36;

/// Path where a possible waiting sequencer upgrade triggered by a precompile
/// is store.
pub const SEQUENCER_KEY_CHANGE_PATH: RefPath =
    RefPath::assert_from(b"/evm/world_state/sequencer_key_change");

/// Path where waiting governance sequencer upgrades are stored.
pub const GOVERNANCE_SEQUENCER_UPGRADE_PATH: RefPath =
    RefPath::assert_from(b"/evm/world_state/sequencer_upgrade");

/// Path where the sequencer key is stored.
pub const SEQUENCER_KEY_PATH: RefPath =
    RefPath::assert_from(b"/evm/world_state/sequencer");

/// Monotonic counter incremented on every sequencer key change (whether it
/// went through the [`change_sequencer_key`] precompile or a governance
/// upgrade). It is folded into the payload signed by the current sequencer when
/// rotating, so a captured `(publicKey, signature)` calldata is bound to one
/// specific counter value and cannot be replayed once the key has rotated again
/// (e.g. after cycling back to a previously-used key).
pub const SEQUENCER_KEY_CHANGE_COUNTER_PATH: RefPath =
    RefPath::assert_from(b"/evm/world_state/sequencer_change_counter");

/// Path where an account nonce is stored. This should be prefixed with the path to
/// where the account is stored for the world state or for the current transaction.
const NONCE: Key = Key::from_static(b"/nonce");

/// Path where an account balance, ether held, is stored. This should be prefixed with the path to
/// where the account is stored for the world state or for the current transaction.
const BALANCE: Key = Key::from_static(b"/balance");

/// "Internal" accounts - accounts with contract code have a contract code hash.
/// This value is computed when the code is stored and kept for future queries. This
/// path should be prefixed with the path to where the account is stored for the world
/// state or for the current transaction.
const CODE_HASH: Key = Key::from_static(b"/code.hash");

/// "Internal" accounts - accounts with contract code, have their code stored here.
/// This path should be prefixed with the path to where the account is stored for the
/// world state or for the current transaction.
const CODE: Key = Key::from_static(b"/code");

/// Path where all the infos of a contract are stored in the same key.
/// This path must contains balance, nonce and code hash. This is the new
/// format for saving the accounts infos that overrides the previous one.
const INFO: Key = Key::from_static(b"/info");

/// The contracts of "internal" accounts have their own storage area. The account
/// location prefixed to this path gives the root path (prefix) to where such storage
/// values are kept. Each index in durable storage gives one complete path to one
/// such 256 bit integer value in storage.
const STORAGE_ROOT: Key = Key::from_static(b"/storage");

/// Path where global counter is stored.
const GLOBAL_COUNTER: Key = Key::from_static(b"/withdrawal_counter");

/// Path where global ticket table is stored.
const TICKET_STORAGE: Key = Key::from_static(b"/ticket_table");

/// Path where global deposit table is stored.
const DEPOSIT_QUEUE_TABLE: Key = Key::from_static(b"/deposits_table");

/// If a contract tries to read a value from storage and it has previously not written
/// anything to this location or if it wrote the default value, then it gets this
/// value back.
const STORAGE_DEFAULT_VALUE: U256 = U256::ZERO;

/// Default balance value for an account.
const BALANCE_DEFAULT_VALUE: U256 = U256::ZERO;

/// Default nonce value for an account.
const NONCE_DEFAULT_VALUE: u64 = 0;

pub fn account_key(address: &Address) -> Result<Key, KeyError> {
    Key::try_from(format!("/{address:x}"))
}

pub fn key_from_u256(index: &U256) -> Result<Key, KeyError> {
    Key::try_from(format!("/{}", hex::encode::<[u8; 32]>(index.to_be_bytes())))
}

pub struct StorageAccount {
    /// This account's subtree, relative to the accounts keyspace.
    key: Key,
}

/// Origin classification of an account, carried as the fourth field of
/// the info record so the hot paths (`basic` read, commit write) learn
/// and persist it for free — including the alias payload, so the
/// record is the single source of truth for the classification.
///
/// Legacy 3-field records (the current Etherlink mainnet format)
/// decode as [`AccountOrigin::Unclassified`] and converge as accounts
/// are touched; this is what makes the migration lazy.
#[derive(Clone, Default, Debug, PartialEq, Eq)]
pub enum AccountOrigin {
    #[default]
    Unclassified,
    Native,
    Alias(AliasInfo),
}

impl AccountOrigin {
    const UNCLASSIFIED_TAG: u8 = 0;
    const NATIVE_TAG: u8 = 1;
    const ALIAS_TAG: u8 = 2;

    /// Byte representation used as the fourth field of the info
    /// record: the tag, followed by the binary [`AliasInfo`] payload
    /// for the `Alias` arm.
    fn to_bytes(&self) -> Result<Vec<u8>, rlp::DecoderError> {
        match self {
            AccountOrigin::Unclassified => Ok(vec![Self::UNCLASSIFIED_TAG]),
            AccountOrigin::Native => Ok(vec![Self::NATIVE_TAG]),
            AccountOrigin::Alias(alias_info) => {
                let mut bytes = vec![Self::ALIAS_TAG];
                alias_info.bin_write(&mut bytes).map_err(|_| {
                    rlp::DecoderError::Custom("Alias payload failed to encode")
                })?;
                Ok(bytes)
            }
        }
    }

    fn from_bytes(bytes: &[u8]) -> Result<Self, rlp::DecoderError> {
        match bytes {
            [Self::UNCLASSIFIED_TAG] => Ok(AccountOrigin::Unclassified),
            [Self::NATIVE_TAG] => Ok(AccountOrigin::Native),
            [Self::ALIAS_TAG, payload @ ..] => {
                let (rest, alias_info) = AliasInfo::nom_read(payload).map_err(|_| {
                    rlp::DecoderError::Custom("Alias payload failed to decode")
                })?;
                if !rest.is_empty() {
                    return Err(rlp::DecoderError::Custom(
                        "Trailing bytes after alias payload",
                    ));
                }
                Ok(AccountOrigin::Alias(alias_info))
            }
            _ => Err(rlp::DecoderError::Custom("Invalid origin field")),
        }
    }
}

impl From<Origin> for AccountOrigin {
    fn from(origin: Origin) -> Self {
        match origin {
            Origin::Native => AccountOrigin::Native,
            Origin::Alias(alias_info) => AccountOrigin::Alias(alias_info),
        }
    }
}

impl From<AccountOrigin> for Option<Origin> {
    fn from(origin: AccountOrigin) -> Self {
        match origin {
            AccountOrigin::Unclassified => None,
            AccountOrigin::Native => Some(Origin::Native),
            AccountOrigin::Alias(alias_info) => Some(Origin::Alias(alias_info)),
        }
    }
}

/// Etherlink's view of an account: revm's fields plus the
/// [`AccountOrigin`] classification, which is part of the account
/// itself and travels with it, in memory as in the durable record.
/// Conversion to [`revm::state::AccountInfo`] happens only at the
/// `Database` boundary, where revm takes over.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct AccountInfo {
    pub balance: U256,
    pub nonce: u64,
    pub code_hash: B256,
    /// Origin classification, persisted as the fourth field of the
    /// info record.
    pub origin: AccountOrigin,
    /// In-memory bytecode, never serialized in the record: the code
    /// itself lives in the code storage, keyed by `code_hash`.
    pub code: Option<Bytecode>,
}

impl Default for AccountInfo {
    // Mirrors `revm::state::AccountInfo::default()`: an account
    // without code carries the empty-code hash, not the zero hash.
    fn default() -> Self {
        AccountInfo {
            balance: U256::ZERO,
            nonce: 0,
            code_hash: KECCAK_EMPTY,
            origin: AccountOrigin::default(),
            code: None,
        }
    }
}

impl AccountInfo {
    // Upper bound used for optimizing reads in the durable storage.
    // The fixed fields take 77 bytes and the origin field is bounded
    // by the longest canonical native address; `set_info` enforces the
    // bound at write time so a bounded read can never truncate.
    //
    // The record can only grow: the fixed fields (balance, nonce, code
    // hash) are constant-size and the origin moves from `Unclassified`
    // to `Native` or `Alias` exactly once, never shrinking. The writer
    // ([`StorageAccount::write_info_record`]) relies on this; see its
    // doc for the escape hatch if the invariant is ever broken.
    pub(crate) const MAX_RLP_SIZE: usize = 256;

    /// Attach an origin classification to a revm account info, e.g.
    /// when the classification is decided at commit time.
    pub fn with_origin(info: RevmAccountInfo, origin: AccountOrigin) -> Self {
        AccountInfo {
            balance: info.balance,
            nonce: info.nonce,
            code_hash: info.code_hash,
            origin,
            code: info.code,
        }
    }

    fn rlp_bytes_checked(&self) -> Result<Vec<u8>, EvmDbError> {
        let origin_bytes = self
            .origin
            .to_bytes()
            .map_err(|_| RuntimeError::DecodingError)?;
        let mut s = rlp::RlpStream::new();
        s.begin_list(4);
        s.append(&self.balance.to_le_bytes::<32>().as_slice());
        s.append(&self.nonce.to_le_bytes().as_slice());
        s.append(&self.code_hash.0.as_slice());
        s.append(&origin_bytes.as_slice());
        let bytes: Vec<u8> = s.out().to_vec();
        // Records larger than the read bound must never reach the
        // storage, or the bounded read in `info` would truncate them.
        if bytes.len() > Self::MAX_RLP_SIZE {
            return Err(EvmDbError::UnexpectedBytesLength {
                expected: Self::MAX_RLP_SIZE,
                actual: bytes.len(),
            });
        }
        Ok(bytes)
    }
}

impl Decodable for AccountInfo {
    fn decode(rlp: &Rlp) -> Result<Self, rlp::DecoderError> {
        if !rlp.is_list() {
            Err(rlp::DecoderError::RlpExpectedToBeList)
        } else {
            let mut it = rlp.iter();
            let balance: Vec<u8> = it
                .next()
                .ok_or(rlp::DecoderError::RlpExpectedToBeList)?
                .as_val()?;
            let nonce: Vec<u8> = it
                .next()
                .ok_or(rlp::DecoderError::RlpExpectedToBeList)?
                .as_val()?;
            let code_hash: Vec<u8> = it
                .next()
                .ok_or(rlp::DecoderError::RlpExpectedToBeList)?
                .as_val()?;
            // Legacy 3-field records (Etherlink mainnet) carry no
            // origin field and decode as `Unclassified`; this is what
            // makes the migration lazy.
            let origin = match it.next() {
                None => AccountOrigin::Unclassified,
                Some(field) => {
                    let field: Vec<u8> = field.as_val()?;
                    AccountOrigin::from_bytes(&field)?
                }
            };
            if it.next().is_some() {
                return Err(rlp::DecoderError::Custom(
                    "Trailing fields after the origin",
                ));
            }

            Ok(Self {
                balance: U256::from_le_bytes::<32>(
                    balance.try_into().map_err(|_| {
                        rlp::DecoderError::Custom("Invalid balance length")
                    })?,
                ),
                nonce: u64::from_le_bytes(
                    nonce
                        .try_into()
                        .map_err(|_| rlp::DecoderError::Custom("Invalid nonce length"))?,
                ),
                code_hash: B256::from_slice(&code_hash),
                origin,
                code: None,
            })
        }
    }
}

impl From<AccountInfo> for RevmAccountInfo {
    fn from(info: AccountInfo) -> Self {
        RevmAccountInfo {
            balance: info.balance,
            nonce: info.nonce,
            code_hash: info.code_hash,
            account_id: None,
            code: info.code,
        }
    }
}

impl StorageAccount {
    pub fn from_address(address: &Address) -> Result<Self, KeyError> {
        Ok(Self {
            key: account_key(address)?,
        })
    }

    /// Key of `segment` under this account.
    fn segment_key(&self, segment: &Key) -> Result<Key, KeyError> {
        self.key.concat(segment)
    }

    pub fn info(
        &self,
        eth_accounts: &mut impl KeySpace,
    ) -> Result<AccountInfo, EvmDbError> {
        if let Some(bytes) = eth_accounts.get(&self.segment_key(&INFO)?) {
            return Ok(AccountInfo::decode(&Rlp::new(&bytes))
                .map_err(|_| RuntimeError::DecodingError)?);
        }

        // Without an `INFO` record it's either :
        // - We don't have the account created yet
        // - The account is stored in the old format (each field in a different key)
        // In the last case we need to migrate the account to the new format
        // We are not verifying if the keys existed before for code readability,
        // this code will run only once for an old address so the overhead is minimal.

        let balance_key = self.segment_key(&BALANCE)?;
        let nonce_key = self.segment_key(&NONCE)?;
        let code_hash_key = self.segment_key(&CODE_HASH)?;
        let code_key = self.segment_key(&CODE)?;

        let info = AccountInfo {
            balance: eth_accounts.get_evm_u256_le_or(&balance_key, BALANCE_DEFAULT_VALUE),
            nonce: eth_accounts.get_le_or(&nonce_key, NONCE_DEFAULT_VALUE),
            code_hash: eth_accounts
                .get_prefix_exact::<32>(&code_hash_key)
                .map_or(KECCAK_EMPTY, B256::from),
            // Split-field accounts predate classification.
            origin: AccountOrigin::Unclassified,
            code: None,
        };

        // Historic check, kept bit-for-bit: it compares the
        // code hash against the zero hash (the old derived
        // default), not `KECCAK_EMPTY`.
        if info.balance == BALANCE_DEFAULT_VALUE
            && info.nonce == NONCE_DEFAULT_VALUE
            && info.code_hash == B256::ZERO
        {
            // Account doesn't exist
            return Ok(AccountInfo::default());
        }

        // Write migration
        if let Some(bytes) = eth_accounts.get(&code_key) {
            CodeStorage::add(
                eth_accounts,
                Bytecode::new_raw_checked(Bytes::from(bytes))
                    .map_err(|_| RuntimeError::DecodingError)?
                    .original_byte_slice(),
                Some(info.code_hash),
            )?;
        }
        self.write_info_record(eth_accounts, &info.rlp_bytes_checked()?)?;

        // Delete legacy account entries
        for key in &[balance_key, nonce_key, code_hash_key, code_key] {
            eth_accounts.delete(key);
        }

        Ok(info)
    }

    pub fn info_without_migration(
        &self,
        eth_accounts: &impl KeySpace,
    ) -> Result<Option<AccountInfo>, EvmDbError> {
        match eth_accounts.get(&self.segment_key(&INFO)?) {
            Some(bytes) => Ok(Some(
                AccountInfo::decode(&Rlp::new(&bytes))
                    .map_err(|_| RuntimeError::DecodingError)?,
            )),
            None => Ok(None),
        }
    }

    /// Persist an already-encoded info record at this account's
    /// `/info`.
    ///
    /// Writes at offset 0 rather than replacing the whole value,
    /// relying on the invariant documented on [`AccountInfo::MAX_RLP_SIZE`]
    /// that the record only ever grows: a write at offset 0 then always
    /// fully covers the previous content, so no stale trailing bytes can
    /// remain.
    ///
    /// If that invariant is ever broken, a field becomes variable-size
    /// and can shrink, or a classification can be downgraded, switch
    /// this back to a whole-value write; otherwise a shorter rewrite would
    /// leave trailing bytes from the previous record and corrupt the
    /// next decode.
    fn write_info_record(
        &self,
        eth_accounts: &mut impl KeySpace,
        value: &[u8],
    ) -> Result<(), EvmDbError> {
        eth_accounts.write(&self.segment_key(&INFO)?, 0, value)?;
        Ok(())
    }

    /// Write the account info record, origin included. Read-modify-write
    /// sites preserve the classification by construction since the
    /// origin travels inside the info they read.
    pub fn set_info(
        &mut self,
        eth_accounts: &mut impl KeySpace,
        mut new_infos: AccountInfo,
    ) -> Result<(), EvmDbError> {
        if let Some(code) = new_infos.code.take() {
            CodeStorage::add(
                eth_accounts,
                code.original_byte_slice(),
                Some(new_infos.code_hash),
            )?;
        }
        let value = new_infos.rlp_bytes_checked()?;
        self.write_info_record(eth_accounts, &value)
    }

    /// Same as [`Self::set_info`] but ignores the in-memory bytecode
    /// instead of persisting it to the code storage.
    pub fn set_info_without_code(
        &mut self,
        eth_accounts: &mut impl KeySpace,
        new_infos: AccountInfo,
    ) -> Result<(), EvmDbError> {
        let value = new_infos.rlp_bytes_checked()?;
        self.write_info_record(eth_accounts, &value)
    }

    // In the future we might want to optimize reading to not use `info`.
    pub fn add_balance(
        &mut self,
        eth_accounts: &mut impl KeySpace,
        amount: U256,
    ) -> Result<(), EvmDbError> {
        let mut info = self.info(eth_accounts)?;
        let current = info.balance;
        info.balance = current
            .checked_add(amount)
            .ok_or(EvmDbError::BalanceOverflow { current, amount })?;
        self.set_info(eth_accounts, info)
    }

    // In the future we might want to optimize reading to not use `info`.
    pub fn sub_balance(
        &mut self,
        eth_accounts: &mut impl KeySpace,
        amount: U256,
    ) -> Result<(), EvmDbError> {
        let mut info = self.info(eth_accounts)?;
        let current = info.balance;
        info.balance = current
            .checked_sub(amount)
            .ok_or(EvmDbError::BalanceUnderflow { current, amount })?;
        self.set_info(eth_accounts, info)
    }

    pub fn storage_key(&self, index: &U256) -> Result<Key, KeyError> {
        self.segment_key(&STORAGE_ROOT)?
            .concat(key_from_u256(index)?)
    }

    pub fn get_storage(
        &self,
        eth_accounts: &impl KeySpace,
        index: &U256,
    ) -> Result<U256, EvmDbError> {
        let key = self.storage_key(index)?;
        Ok(eth_accounts.get_evm_u256_be_or(&key, STORAGE_DEFAULT_VALUE))
    }

    pub fn set_storage(
        &mut self,
        eth_accounts: &mut impl KeySpace,
        index: &U256,
        value: &U256,
    ) -> Result<(), EvmDbError> {
        let key = self.storage_key(index)?;
        Ok(eth_accounts.set(&key, value.to_be_bytes::<{ U256::BYTES }>())?)
    }

    pub(crate) fn read_global_counter(
        &self,
        eth_accounts: &impl KeySpace,
    ) -> Result<U256, PrecompileStateError> {
        let key = self.segment_key(&GLOBAL_COUNTER)?;
        Ok(eth_accounts.get_evm_u256_le_or(&key, U256::ZERO))
    }

    pub(crate) fn write_global_counter(
        &self,
        eth_accounts: &mut impl KeySpace,
        value: U256,
    ) -> Result<(), EvmDbError> {
        let key = self.segment_key(&GLOBAL_COUNTER)?;
        Ok(eth_accounts.store_evm_u256_le(&key, value)?)
    }

    fn ticket_key(&self, ticket_hash: &U256, owner: &Address) -> Result<Key, KeyError> {
        self.segment_key(&TICKET_STORAGE)?
            .concat(key_from_u256(ticket_hash)?)?
            .concat(account_key(owner)?)
    }

    pub fn read_ticket_balance(
        &self,
        eth_accounts: &impl KeySpace,
        ticket_hash: &U256,
        owner: &Address,
    ) -> Result<U256, PrecompileStateError> {
        let key = self.ticket_key(ticket_hash, owner)?;
        Ok(eth_accounts.get_evm_u256_le_or(&key, U256::ZERO))
    }

    pub fn write_ticket_balance(
        &mut self,
        eth_accounts: &mut impl KeySpace,
        ticket_hash: &U256,
        owner: &Address,
        amount: U256,
    ) -> Result<(), EvmDbError> {
        let key = self.ticket_key(ticket_hash, owner)?;
        eth_accounts.store_evm_u256_le(&key, amount)?;
        Ok(())
    }

    /// Delete the durable ticket-balance node for `(ticket_hash, owner)`.
    ///
    /// A zero balance is read-equivalent to an absent node (see
    /// [`Self::read_ticket_balance`], which defaults a missing key to
    /// [`U256::ZERO`]), so a balance that reaches zero must leave no durable
    /// trace. Deleting an already-absent node is a no-op.
    pub(crate) fn delete_ticket_balance(
        &mut self,
        eth_accounts: &mut impl KeySpace,
        ticket_hash: &U256,
        owner: &Address,
    ) -> Result<(), EvmDbError> {
        eth_accounts.delete(&self.ticket_key(ticket_hash, owner)?);
        Ok(())
    }

    /// Whether a durable ticket-balance node exists for `(ticket_hash, owner)`.
    ///
    /// Test-only: unlike [`Self::read_ticket_balance`] (which defaults a
    /// missing node to zero and so cannot tell an absent node from one holding
    /// zero), this distinguishes the two, which is exactly what the orphan-node
    /// regression tests assert.
    #[cfg(test)]
    pub(crate) fn ticket_balance_node_exists(
        &self,
        eth_accounts: &impl KeySpace,
        ticket_hash: &U256,
        owner: &Address,
    ) -> Result<bool, EvmDbError> {
        Ok(eth_accounts.contains(&self.ticket_key(ticket_hash, owner)?))
    }

    fn deposit_key(&self, withdrawal_id: &U256) -> Result<Key, KeyError> {
        self.segment_key(&DEPOSIT_QUEUE_TABLE)?
            .concat(format!("/{withdrawal_id}"))
    }

    pub(crate) fn write_deposit(
        &mut self,
        eth_accounts: &mut impl KeySpace,
        deposit_id: &U256,
        deposit: &FaDepositWithProxy,
    ) -> Result<(), EvmDbError> {
        let deposit_key = self.deposit_key(deposit_id)?;
        Ok(eth_accounts.set(&deposit_key, deposit.rlp_bytes())?)
    }

    pub(crate) fn read_deposit_from_queue(
        &self,
        eth_accounts: &impl KeySpace,
        deposit_id: &U256,
    ) -> Result<FaDepositWithProxy, PrecompileStateError> {
        let deposit_key = self.deposit_key(deposit_id)?;
        let bytes = eth_accounts
            .get(&deposit_key)
            .ok_or(RuntimeError::PathNotFound)?;
        Ok(FaDepositWithProxy::from_raw(bytes)?)
    }

    pub(crate) fn remove_deposit_from_queue(
        &self,
        eth_accounts: &mut impl KeySpace,
        deposit_id: &U256,
    ) -> Result<(), EvmDbError> {
        eth_accounts.delete(&self.deposit_key(deposit_id)?);
        Ok(())
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::error::EvmDbError;
    use crate::{
        precompiles::constants::{
            FA_BRIDGE_SOL_CONTRACT, INTERNAL_FORWARDER_SOL_CONTRACT,
            XTZ_BRIDGE_SOL_CONTRACT,
        },
        storage::code::CodeStorage,
    };

    use revm::{
        primitives::{Bytes, FixedBytes, KECCAK_EMPTY},
        state::Bytecode,
    };
    use tezos_evm_runtime::runtime_keyspaces::{
        MockRuntimeKeyspaces, ETH_ACCOUNTS_KEYSPACE_NAME,
    };
    use tezos_smart_rollup_keyspace::KeySpace;

    fn bytecode_from_static(bytes: &'static [u8]) -> Result<Bytecode, EvmDbError> {
        Ok(Bytecode::new_legacy(Bytes::from_static(bytes)))
    }

    fn check_account_code_info_fetching(
        eth_accounts: &mut impl KeySpace,
        code_voucher: Bytecode,
        code_hash_voucher: FixedBytes<32>,
    ) {
        let bytecode = CodeStorage::new(&code_hash_voucher)
            .unwrap()
            .get_code(eth_accounts)
            .unwrap()
            .unwrap_or_default();

        assert_eq!(code_voucher, bytecode);
    }

    #[test]
    fn check_withdrawal_code_info_fetching() {
        let mut rk = MockRuntimeKeyspaces::default();
        let eth_accounts = rk.eth_accounts_mut();
        let code_voucher = bytecode_from_static(XTZ_BRIDGE_SOL_CONTRACT.code).unwrap();

        check_account_code_info_fetching(
            eth_accounts,
            code_voucher,
            XTZ_BRIDGE_SOL_CONTRACT.code_hash,
        );
    }

    #[test]
    fn check_fa_withdrawal_code_info_fetching() {
        let mut rk = MockRuntimeKeyspaces::default();
        let eth_accounts = rk.eth_accounts_mut();
        let code_voucher = bytecode_from_static(FA_BRIDGE_SOL_CONTRACT.code).unwrap();

        check_account_code_info_fetching(
            eth_accounts,
            code_voucher,
            FA_BRIDGE_SOL_CONTRACT.code_hash,
        );
    }

    #[test]
    fn check_internal_forwarder_code_info_fetching() {
        let mut rk = MockRuntimeKeyspaces::default();
        let eth_accounts = rk.eth_accounts_mut();
        let code_voucher =
            bytecode_from_static(INTERNAL_FORWARDER_SOL_CONTRACT.code).unwrap();

        check_account_code_info_fetching(
            eth_accounts,
            code_voucher,
            INTERNAL_FORWARDER_SOL_CONTRACT.code_hash,
        );
    }

    #[test]
    fn check_empty_account_code_info_fetching() {
        let mut rk = MockRuntimeKeyspaces::default();
        let eth_accounts = rk.eth_accounts_mut();
        let code_voucher = Bytecode::new();

        check_account_code_info_fetching(eth_accounts, code_voucher, KECCAK_EMPTY);
    }

    #[test]
    fn account_info_size_within_read_bound() {
        use crate::storage::world_state_handler::{AccountInfo, AccountOrigin};
        use tezosx_interfaces::{AliasInfo, RuntimeId};

        // Default (Unclassified) record: fixed fields + 1-byte origin.
        let rlp_size = AccountInfo::default().rlp_bytes_checked().unwrap().len();
        assert_eq!(rlp_size, 78);

        // The longest canonical native address today is a 0x-prefixed
        // EVM hex address (42 bytes); leave generous slack below the
        // bound, which `rlp_bytes_checked` enforces at write time.
        let info = AccountInfo {
            origin: AccountOrigin::Alias(AliasInfo {
                runtime: RuntimeId::Ethereum,
                native_address: b"0xffffffffffffffffffffffffffffffffffffffff".to_vec(),
            }),
            ..AccountInfo::default()
        };
        let alias_size = info.rlp_bytes_checked().unwrap().len();
        assert!(alias_size <= AccountInfo::MAX_RLP_SIZE);

        // A pathological native address larger than the bound is
        // refused at write time instead of truncating later reads.
        let info = AccountInfo {
            origin: AccountOrigin::Alias(AliasInfo {
                runtime: RuntimeId::Ethereum,
                native_address: vec![0x61; AccountInfo::MAX_RLP_SIZE],
            }),
            ..AccountInfo::default()
        };
        assert!(info.rlp_bytes_checked().is_err());
    }

    #[test]
    fn account_info_origin_roundtrip() {
        use crate::storage::world_state_handler::{AccountInfo, AccountOrigin};
        use rlp::{Decodable, Rlp};
        use tezosx_interfaces::{AliasInfo, RuntimeId};

        for origin in [
            AccountOrigin::Unclassified,
            AccountOrigin::Native,
            AccountOrigin::Alias(AliasInfo {
                runtime: RuntimeId::Tezos,
                native_address: b"tz1abcdef".to_vec(),
            }),
        ] {
            let info = AccountInfo {
                balance: revm::primitives::U256::from(42),
                nonce: 7,
                code_hash: KECCAK_EMPTY,
                origin,
                code: None,
            };
            let bytes = info.rlp_bytes_checked().unwrap();
            let decoded = AccountInfo::decode(&Rlp::new(&bytes)).unwrap();
            assert_eq!(decoded, info);
        }
    }

    // A record written before the tag existed (3-field RLP) decodes
    // with `Unclassified`: this is the lazy-migration contract.
    #[test]
    fn account_info_legacy_record_decodes_as_unclassified() {
        use crate::storage::world_state_handler::{AccountInfo, AccountOrigin};
        use rlp::{Decodable, Rlp};

        let info = AccountInfo {
            balance: revm::primitives::U256::from(42),
            nonce: 7,
            code_hash: KECCAK_EMPTY,
            origin: AccountOrigin::Unclassified,
            code: None,
        };
        // Re-encode by hand in the legacy 3-field layout.
        let mut stream = rlp::RlpStream::new();
        stream.begin_list(3);
        stream.append(&info.balance.to_le_bytes::<32>().as_slice());
        stream.append(&info.nonce.to_le_bytes().as_slice());
        stream.append(&info.code_hash.0.as_slice());
        let legacy_bytes = stream.out();
        assert_eq!(legacy_bytes.len(), 77);

        let decoded = AccountInfo::decode(&Rlp::new(&legacy_bytes)).unwrap();
        assert_eq!(decoded, info);
    }

    #[test]
    fn account_info_rejects_invalid_tag_and_trailing_fields() {
        use crate::storage::world_state_handler::AccountInfo;
        use rlp::{Decodable, Rlp};

        let append_legacy_fields = |stream: &mut rlp::RlpStream| {
            stream.append(
                &revm::primitives::U256::from(42)
                    .to_le_bytes::<32>()
                    .as_slice(),
            );
            stream.append(&7u64.to_le_bytes().as_slice());
            stream.append(&KECCAK_EMPTY.0.as_slice());
        };

        // Tag out of range.
        let mut stream = rlp::RlpStream::new();
        stream.begin_list(4);
        append_legacy_fields(&mut stream);
        stream.append(&[3u8].as_slice());
        assert!(AccountInfo::decode(&Rlp::new(&stream.out())).is_err());

        // Fifth field.
        let mut stream = rlp::RlpStream::new();
        stream.begin_list(5);
        append_legacy_fields(&mut stream);
        stream.append(&[1u8].as_slice());
        stream.append(&[1u8].as_slice());
        assert!(AccountInfo::decode(&Rlp::new(&stream.out())).is_err());
    }

    // The origin survives a storage round-trip through the account
    // record, including the alias payload. Each origin is written to a
    // fresh account: the writer relies on the record-only-grows
    // invariant (see [`StorageAccount::write_info_record`]), so a single
    // account must never be rewritten with a *shorter* origin field
    // (e.g. `Alias` -> `Unclassified`), which never happens in
    // production either.
    #[test]
    fn origin_roundtrips_through_account_record() {
        use crate::storage::world_state_handler::{
            AccountInfo, AccountOrigin, StorageAccount,
        };
        use revm::primitives::Address;
        use tezosx_interfaces::{AliasInfo, RuntimeId};

        let mut rk = MockRuntimeKeyspaces::default();
        let eth_accounts = rk.eth_accounts_mut();

        for (i, origin) in [
            AccountOrigin::Native,
            AccountOrigin::Alias(AliasInfo {
                runtime: RuntimeId::Tezos,
                native_address: b"tz1abcdef".to_vec(),
            }),
            AccountOrigin::Unclassified,
        ]
        .into_iter()
        .enumerate()
        {
            let addr = Address::from_slice(&[i as u8 + 1; 20]);
            let mut account = StorageAccount::from_address(&addr).unwrap();
            assert!(account
                .info_without_migration(eth_accounts)
                .unwrap()
                .is_none());

            account
                .set_info_without_code(
                    eth_accounts,
                    AccountInfo {
                        origin: origin.clone(),
                        ..AccountInfo::default()
                    },
                )
                .unwrap();
            let read_back = account.info(eth_accounts).unwrap().origin;
            assert_eq!(read_back, origin);
        }
    }

    // In-place upgrades follow the production transitions, which only
    // ever grow the origin field (`Unclassified` -> `Native` ->
    // `Alias`); the record-only-grows invariant `write_info_record`
    // relies on holds across them.
    #[test]
    fn origin_in_place_upgrade_roundtrips() {
        use crate::storage::world_state_handler::{
            AccountInfo, AccountOrigin, StorageAccount,
        };
        use revm::primitives::Address;
        use tezosx_interfaces::{AliasInfo, RuntimeId};

        let mut rk = MockRuntimeKeyspaces::default();
        let eth_accounts = rk.eth_accounts_mut();
        let addr = Address::from_slice(&[0x42; 20]);
        let mut account = StorageAccount::from_address(&addr).unwrap();

        for origin in [
            AccountOrigin::Unclassified,
            AccountOrigin::Native,
            AccountOrigin::Alias(AliasInfo {
                runtime: RuntimeId::Tezos,
                native_address: b"tz1abcdef".to_vec(),
            }),
        ] {
            account
                .set_info_without_code(
                    eth_accounts,
                    AccountInfo {
                        origin: origin.clone(),
                        ..AccountInfo::default()
                    },
                )
                .unwrap();
            let read_back = account.info(eth_accounts).unwrap().origin;
            assert_eq!(read_back, origin);
        }
    }

    // A ticket balance that reaches zero must leave no durable node: a zero
    // node is read-equivalent to an absent one, so persisting it would leave an
    // orphan behind a fully-reverted (or fully-drained) balance change.
    #[test]
    fn delete_ticket_balance_removes_orphan_node() {
        use crate::storage::world_state_handler::StorageAccount;
        use revm::primitives::{Address, U256};

        let mut rk = MockRuntimeKeyspaces::default();
        let eth_accounts = rk.eth_accounts_mut();
        let mut system = StorageAccount::from_address(&Address::ZERO).unwrap();
        let ticket_hash = U256::from(1);
        let owner = Address::from_slice(&[0x11; 20]);

        // A fresh key has no durable node...
        assert!(!system
            .ticket_balance_node_exists(eth_accounts, &ticket_hash, &owner)
            .unwrap());
        // ...and deleting an absent balance is a tolerated no-op.
        system
            .delete_ticket_balance(eth_accounts, &ticket_hash, &owner)
            .unwrap();
        assert!(!system
            .ticket_balance_node_exists(eth_accounts, &ticket_hash, &owner)
            .unwrap());

        // A non-zero balance creates the node.
        system
            .write_ticket_balance(eth_accounts, &ticket_hash, &owner, U256::from(7))
            .unwrap();
        assert!(system
            .ticket_balance_node_exists(eth_accounts, &ticket_hash, &owner)
            .unwrap());
        assert_eq!(
            system
                .read_ticket_balance(eth_accounts, &ticket_hash, &owner)
                .unwrap(),
            U256::from(7)
        );

        // Draining to zero deletes the node instead of persisting a zero.
        system
            .delete_ticket_balance(eth_accounts, &ticket_hash, &owner)
            .unwrap();
        assert!(!system
            .ticket_balance_node_exists(eth_accounts, &ticket_hash, &owner)
            .unwrap());
        // The read is still zero, identical to an absent node.
        assert_eq!(
            system
                .read_ticket_balance(eth_accounts, &ticket_hash, &owner)
                .unwrap(),
            U256::ZERO
        );
    }

    /// The durable path a key of the accounts keyspace resolves to: the
    /// keyspace prepends its own name to every key it is handed.
    fn durable(key: &Key) -> Vec<u8> {
        let name = ETH_ACCOUNTS_KEYSPACE_NAME.to_string();
        [name.as_bytes(), key.as_bytes()].concat()
    }

    #[test]
    fn account_keys_keep_their_durable_paths() {
        let address = Address::from_slice(&[0xab; 20]);
        let account = StorageAccount::from_address(&address).unwrap();
        let prefix = format!("/evm/eth_accounts/{address:x}");

        assert_eq!(
            durable(&account.segment_key(&INFO).unwrap()),
            format!("{prefix}/info").into_bytes()
        );
        assert_eq!(
            durable(&account.segment_key(&BALANCE).unwrap()),
            format!("{prefix}/balance").into_bytes()
        );
        assert_eq!(
            durable(&account.segment_key(&NONCE).unwrap()),
            format!("{prefix}/nonce").into_bytes()
        );
        assert_eq!(
            durable(&account.segment_key(&CODE_HASH).unwrap()),
            format!("{prefix}/code.hash").into_bytes()
        );
        assert_eq!(
            durable(&account.segment_key(&CODE).unwrap()),
            format!("{prefix}/code").into_bytes()
        );
        assert_eq!(
            durable(&account.segment_key(&GLOBAL_COUNTER).unwrap()),
            format!("{prefix}/withdrawal_counter").into_bytes()
        );

        let index = U256::from(1);
        let index_hex = hex::encode::<[u8; 32]>(index.to_be_bytes());
        assert_eq!(
            durable(&account.storage_key(&index).unwrap()),
            format!("{prefix}/storage/{index_hex}").into_bytes()
        );

        let owner = Address::from_slice(&[0xcd; 20]);
        assert_eq!(
            durable(&account.ticket_key(&index, &owner).unwrap()),
            format!("{prefix}/ticket_table/{index_hex}/{owner:x}").into_bytes()
        );
        assert_eq!(
            durable(&account.deposit_key(&index).unwrap()),
            format!("{prefix}/deposits_table/1").into_bytes()
        );
    }
}
