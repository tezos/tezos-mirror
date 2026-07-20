// SPDX-FileCopyrightText: 2023 Functori <contact@functori.com>
// SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use crate::blueprint_storage;
use crate::fallback_upgrade::backup_current_kernel;
use core::fmt;

use crate::error::UpgradeProcessError;
use crate::event::Event;
use crate::storage;
use crate::storage::{store_sequencer, store_sequencer_pool_address};
use anyhow::Context;
use primitive_types::H160;
use rlp::Decodable;
use rlp::DecoderError;
use rlp::Encodable;
use tezos_ethereum::rlp_helpers::append_public_key;
use tezos_ethereum::rlp_helpers::append_timestamp;
use tezos_ethereum::rlp_helpers::decode_field;
use tezos_ethereum::rlp_helpers::decode_public_key;
use tezos_ethereum::rlp_helpers::decode_timestamp;
use tezos_ethereum::rlp_helpers::next;
use tezos_evm_logging::{log, Level::*};
use tezos_evm_runtime::runtime::IsEvmNode;
use tezos_smart_rollup_core::PREIMAGE_HASH_SIZE;
use tezos_smart_rollup_encoding::public_key::PublicKey;
use tezos_smart_rollup_encoding::timestamp::Timestamp;
use tezos_smart_rollup_host::path::OwnedPath;
use tezos_smart_rollup_host::path::Path;
use tezos_smart_rollup_host::path::RefPath;
use tezos_smart_rollup_host::reveal::HostReveal;
use tezos_smart_rollup_host::storage::StorageV1;
use tezos_smart_rollup_host::wasm::WasmHost;
use tezos_smart_rollup_installer_config::binary::promote::upgrade_reveal_flow;
use tezos_smart_rollup_keyspace::KeySpaceLoader;
use tezos_storage::read_optional_rlp;

const KERNEL_UPGRADE: RefPath = RefPath::assert_from(b"/base/kernel_upgrade");
pub const KERNEL_ROOT_HASH: RefPath = RefPath::assert_from(b"/base/kernel_root_hash");
use revm_etherlink::storage::sequencer_key_change::increment_sequencer_change_counter;
use revm_etherlink::storage::world_state_handler::GOVERNANCE_SEQUENCER_UPGRADE_PATH;
use revm_etherlink::storage::world_state_handler::SEQUENCER_KEY_CHANGE_PATH;

#[derive(Debug, PartialEq, Clone)]
pub struct KernelUpgrade {
    pub preimage_hash: [u8; PREIMAGE_HASH_SIZE],
    pub activation_timestamp: Timestamp,
}

impl KernelUpgrade {
    const RLP_LIST_SIZE: usize = 2;
}

impl Decodable for KernelUpgrade {
    fn decode(decoder: &rlp::Rlp) -> Result<Self, DecoderError> {
        if !decoder.is_list() {
            return Err(DecoderError::RlpExpectedToBeList);
        }
        if decoder.item_count()? != KernelUpgrade::RLP_LIST_SIZE {
            return Err(DecoderError::RlpIncorrectListLen);
        }

        let mut it = decoder.iter();
        let preimage_hash: Vec<u8> = decode_field(&next(&mut it)?, "preimage_hash")?;
        let preimage_hash: [u8; PREIMAGE_HASH_SIZE] = preimage_hash
            .try_into()
            .map_err(|_| DecoderError::RlpInvalidLength)?;
        let activation_timestamp = decode_timestamp(&next(&mut it)?)?;

        Ok(Self {
            preimage_hash,
            activation_timestamp,
        })
    }
}

impl Encodable for KernelUpgrade {
    fn rlp_append(&self, stream: &mut rlp::RlpStream) {
        stream.begin_list(KernelUpgrade::RLP_LIST_SIZE);
        stream.append_iter(self.preimage_hash);
        append_timestamp(stream, self.activation_timestamp);
    }
}

pub fn store_kernel_upgrade<Host>(
    host: &mut Host,
    kernel_upgrade: &KernelUpgrade,
) -> anyhow::Result<()>
where
    Host: StorageV1 + IsEvmNode + KeySpaceLoader,
{
    log!(
        Info,
        "An upgrade to {} is planned for {}",
        hex::encode(kernel_upgrade.preimage_hash),
        kernel_upgrade.activation_timestamp
    );
    Event::Upgrade(kernel_upgrade.clone()).store(host)?;
    let path = OwnedPath::from(KERNEL_UPGRADE);
    let bytes = &kernel_upgrade.rlp_bytes();
    host.store_write_all(&path, bytes)
        .context("Failed to store kernel upgrade")
}

fn read_kernel_upgrade_at(
    host: &impl StorageV1,
    path: &impl Path,
) -> anyhow::Result<Option<KernelUpgrade>> {
    read_optional_rlp(host, path).context("Failed to decode kernel upgrade")
}

pub fn read_kernel_upgrade(
    host: &impl StorageV1,
) -> anyhow::Result<Option<KernelUpgrade>> {
    read_kernel_upgrade_at(host, &KERNEL_UPGRADE)
}

pub fn upgrade<Host>(
    host: &mut Host,
    root_hash: [u8; PREIMAGE_HASH_SIZE],
) -> anyhow::Result<()>
where
    Host: StorageV1 + HostReveal + WasmHost,
{
    log!(Info, "Kernel upgrade initialisation.");

    backup_current_kernel(host)?;
    let config = upgrade_reveal_flow(root_hash);
    config
        .evaluate(host)
        .map_err(UpgradeProcessError::InternalUpgrade)?;

    host.store_write_all(&KERNEL_ROOT_HASH, &root_hash)?;
    host.store_delete(&KERNEL_UPGRADE)?;

    log!(Info, "Kernel is ready to be upgraded.");
    Ok(())
}

#[derive(Debug, PartialEq, Clone)]
pub struct SequencerUpgrade {
    pub sequencer: PublicKey,
    pub pool_address: H160,
    pub activation_timestamp: Timestamp,
}

impl SequencerUpgrade {
    const RLP_LIST_SIZE: usize = 3;
}

impl fmt::Display for SequencerUpgrade {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "sequencer: {}, pool_address: {}, activation_timestamp: {}",
            self.sequencer, self.pool_address, self.activation_timestamp
        )
    }
}

impl Decodable for SequencerUpgrade {
    fn decode(decoder: &rlp::Rlp) -> Result<Self, DecoderError> {
        if !decoder.is_list() {
            return Err(DecoderError::RlpExpectedToBeList);
        }
        if decoder.item_count()? != SequencerUpgrade::RLP_LIST_SIZE {
            return Err(DecoderError::RlpIncorrectListLen);
        }

        let mut it = decoder.iter();
        let sequencer = decode_public_key(&next(&mut it)?)?;
        // Reject keys that are not valid points on their curve, so an unusable
        // key cannot become the sequencer key.
        sequencer
            .check_validity()
            .map_err(|_| DecoderError::Custom("invalid sequencer public key"))?;
        let pool_address: H160 = decode_field(&next(&mut it)?, "sequencer_pool_address")?;
        let activation_timestamp = decode_timestamp(&next(&mut it)?)?;

        Ok(Self {
            sequencer,
            pool_address,
            activation_timestamp,
        })
    }
}

impl Encodable for SequencerUpgrade {
    fn rlp_append(&self, stream: &mut rlp::RlpStream) {
        stream.begin_list(SequencerUpgrade::RLP_LIST_SIZE);
        append_public_key(stream, &self.sequencer);
        stream.append(&self.pool_address);
        append_timestamp(stream, self.activation_timestamp);
    }
}

pub fn store_sequencer_upgrade<Host>(
    host: &mut Host,
    sequencer_upgrade: SequencerUpgrade,
) -> anyhow::Result<()>
where
    Host: StorageV1 + IsEvmNode + KeySpaceLoader,
{
    log!(
        Info,
        "A sequencer upgrade to {} is planned for {}",
        sequencer_upgrade.sequencer.to_b58check(),
        sequencer_upgrade.activation_timestamp
    );
    let bytes = &sequencer_upgrade.rlp_bytes();
    Event::SequencerUpgrade(sequencer_upgrade).store(host)?;
    let path = OwnedPath::from(GOVERNANCE_SEQUENCER_UPGRADE_PATH);
    host.store_write_all(&path, bytes)
        .context("Failed to store sequencer upgrade")
}

pub fn read_sequencer_upgrade(
    host: &impl StorageV1,
) -> anyhow::Result<Option<SequencerUpgrade>> {
    let path = OwnedPath::from(GOVERNANCE_SEQUENCER_UPGRADE_PATH);
    read_optional_rlp(host, &path).context("Failed to decode sequencer upgrade")
}

fn delete_sequencer_upgrade(host: &mut impl StorageV1) -> anyhow::Result<()> {
    host.store_delete(&GOVERNANCE_SEQUENCER_UPGRADE_PATH)
        .context("Failed to delete sequencer upgrade")
}

fn sequencer_upgrade<Host>(
    host: &mut Host,
    pool_address: H160,
    sequencer: &PublicKey,
) -> anyhow::Result<()>
where
    Host: StorageV1,
{
    log!(Info, "sequencer upgrade initialisation.");

    store_sequencer(host, sequencer)?;
    // Governance changes bump the counter at apply-time. Unlike the precompile
    // path -- whose signed calldata could otherwise be replayed and so must be
    // invalidated as soon as a change is *stored* -- a governance upgrade carries
    // no replayable signature, and the pending upgrade can be re-scheduled from
    // L1 before activation. Bumping here (rather than at store-time) thus keeps
    // the counter at exactly +1 per applied governance change.
    increment_sequencer_change_counter(host)?;
    store_sequencer_pool_address(host, pool_address)?;
    delete_sequencer_upgrade(host)?;
    delete_sequencer_key_change(host)?;
    log!(Info, "Sequencer has been updated.");
    Ok(())
}

pub fn possible_sequencer_upgrade<Host>(host: &mut Host) -> anyhow::Result<()>
where
    Host: StorageV1 + KeySpaceLoader,
{
    let upgrade = read_sequencer_upgrade(host)?;
    if let Some(upgrade) = upgrade {
        let ipl_timestamp = storage::read_last_info_per_level_timestamp(host)?;
        if ipl_timestamp >= upgrade.activation_timestamp {
            sequencer_upgrade(host, upgrade.pool_address, &upgrade.sequencer)?;
            blueprint_storage::clear_all_blueprints(host)?;
        }
    }
    Ok(())
}

pub use revm_etherlink::storage::sequencer_key_change::SequencerKeyChange as EVMBasedSequencerKeyChange;

pub fn read_sequencer_key_change(
    host: &impl StorageV1,
) -> anyhow::Result<Option<EVMBasedSequencerKeyChange>> {
    let path = OwnedPath::from(SEQUENCER_KEY_CHANGE_PATH);
    read_optional_rlp(host, &path).context("Failed to decode sequencer key change")
}

fn delete_sequencer_key_change(host: &mut impl StorageV1) -> anyhow::Result<()> {
    host.store_delete_value(&SEQUENCER_KEY_CHANGE_PATH)
        .context("Failed to delete sequencer key change")
}

fn sequencer_key_change<Host>(
    host: &mut Host,
    key_change: EVMBasedSequencerKeyChange,
) -> anyhow::Result<()>
where
    Host: StorageV1,
{
    log!(Info, "EVM based sequencer key change initialisation.");

    store_sequencer(host, key_change.sequencer_key())?;
    delete_sequencer_key_change(host)?;

    log!(Info, "Sequencer key has been updated.");
    Ok(())
}

pub fn possible_sequencer_key_change<Host>(
    host: &mut Host,
    evm_timestamp: Timestamp,
) -> anyhow::Result<()>
where
    Host: StorageV1 + KeySpaceLoader,
{
    let upgrade = read_sequencer_key_change(host)?;
    if let Some(upgrade) = upgrade {
        if evm_timestamp >= upgrade.activation_timestamp() {
            sequencer_key_change(host, upgrade)?;
            blueprint_storage::clear_all_blueprints(host)?;
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use revm::primitives::U256;
    use revm_etherlink::storage::sequencer_key_change::{
        read_sequencer_change_counter, store_sequencer_key_change,
    };
    use tezos_evm_runtime::runtime::MockKernelHost;

    fn test_public_key() -> PublicKey {
        PublicKey::from_b58check("edpkuSLWfVU1Vq7Jg9FucPyKmma6otcMHac9zG4oU1KMHSTBpJuGQ2")
            .unwrap()
    }

    // A governance change advances the change counter by exactly one, and
    // does so when the upgrade is *applied*. Governance carries no replayable
    // signed calldata (and its pending upgrade can be re-scheduled from L1
    // before activation), so there is nothing to invalidate at store-time.
    #[test]
    fn governance_change_increments_counter_once_on_apply() {
        let mut host = MockKernelHost::default();

        assert_eq!(read_sequencer_change_counter(&host).unwrap(), U256::ZERO);

        store_sequencer_upgrade(
            &mut host,
            SequencerUpgrade {
                sequencer: test_public_key(),
                pool_address: H160::zero(),
                activation_timestamp: Timestamp::from(100i64),
            },
        )
        .unwrap();

        // Merely scheduling the upgrade must not move the counter.
        assert_eq!(read_sequencer_change_counter(&host).unwrap(), U256::ZERO);

        // Before activation nothing applies, so the counter stays put.
        storage::store_last_info_per_level_timestamp(&mut host, Timestamp::from(50i64))
            .unwrap();
        possible_sequencer_upgrade(&mut host).unwrap();
        assert_eq!(read_sequencer_change_counter(&host).unwrap(), U256::ZERO);

        // At/after activation the upgrade applies exactly once: counter is +1.
        storage::store_last_info_per_level_timestamp(&mut host, Timestamp::from(100i64))
            .unwrap();
        possible_sequencer_upgrade(&mut host).unwrap();
        assert_eq!(read_sequencer_change_counter(&host).unwrap(), U256::ONE);
    }

    // The precompile path bumps the counter at store-time (in
    // `EtherlinkVMDB::commit`). Applying the pending change later must *not* bump
    // it again -- otherwise a single change advances the counter by two and a
    // legitimately pre-signed next change would verify against a stale value.
    #[test]
    fn precompile_change_apply_does_not_increment_counter() {
        let mut host = MockKernelHost::default();

        // Simulate the precompile store-time effects: the pending change is
        // stored and the counter is bumped exactly once.
        store_sequencer_key_change(
            &mut host,
            EVMBasedSequencerKeyChange::new(test_public_key(), Timestamp::from(100i64)),
        )
        .unwrap();
        increment_sequencer_change_counter(&mut host).unwrap();
        assert_eq!(read_sequencer_change_counter(&host).unwrap(), U256::ONE);

        // Before activation nothing applies.
        possible_sequencer_key_change(&mut host, Timestamp::from(50i64)).unwrap();
        assert_eq!(read_sequencer_change_counter(&host).unwrap(), U256::ONE);

        // At/after activation the change applies but the counter is unchanged.
        possible_sequencer_key_change(&mut host, Timestamp::from(100i64)).unwrap();
        assert_eq!(read_sequencer_change_counter(&host).unwrap(), U256::ONE);
    }

    fn upgrade_with(sequencer: PublicKey) -> SequencerUpgrade {
        SequencerUpgrade {
            sequencer,
            pool_address: H160::zero(),
            activation_timestamp: Timestamp::from(0i64),
        }
    }

    #[test]
    fn sequencer_upgrade_decode_rejects_off_curve_key() {
        // A secp256k1 key that decodes (length-only) but is not a point on the
        // curve must be rejected so it cannot become the sequencer key.
        let invalid = PublicKey::from_b58check(
            "sppk7bFP2oW86SDDFzqiDCMtbm8j4obhJ9AVYkG1XFzwz4ik6kGmM5V",
        )
        .unwrap();
        let encoded = rlp::encode(&upgrade_with(invalid));
        assert!(rlp::decode::<SequencerUpgrade>(&encoded).is_err());
    }

    #[test]
    fn sequencer_upgrade_decode_accepts_valid_key() {
        let valid = PublicKey::from_b58check(
            "edpkuSLWfVU1Vq7Jg9FucPyKmma6otcMHac9zG4oU1KMHSTBpJuGQ2",
        )
        .unwrap();
        let encoded = rlp::encode(&upgrade_with(valid));
        assert!(rlp::decode::<SequencerUpgrade>(&encoded).is_ok());
    }

    #[test]
    fn sequencer_upgrade_decode_rejects_invalid_ed25519() {
        // Unlike Michelson `key` literals, the sequencer key is fully
        // validated: an Ed25519 key that decodes (length-only) but is not a
        // valid point is rejected.
        let invalid = PublicKey::from_b58check(
            "edpktf7DydMTKVxjkGzpoCefNuUuSdTa7YVquqoVs2uTjNJ5iCqxLn",
        )
        .unwrap();
        let encoded = rlp::encode(&upgrade_with(invalid));
        assert!(rlp::decode::<SequencerUpgrade>(&encoded).is_err());
    }
}
