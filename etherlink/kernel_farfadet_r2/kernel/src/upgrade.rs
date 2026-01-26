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
use tezos_evm_runtime::runtime::Runtime;
use tezos_smart_rollup_core::PREIMAGE_HASH_SIZE;
use tezos_smart_rollup_encoding::public_key::PublicKey;
use tezos_smart_rollup_encoding::timestamp::Timestamp;
use tezos_smart_rollup_host::path::OwnedPath;
use tezos_smart_rollup_host::path::Path;
use tezos_smart_rollup_host::path::RefPath;
use tezos_smart_rollup_installer_config::binary::promote::upgrade_reveal_flow;
use tezos_storage::read_optional_rlp;

const KERNEL_UPGRADE: RefPath = RefPath::assert_from(b"/evm/kernel_upgrade");
pub const KERNEL_ROOT_HASH: RefPath = RefPath::assert_from(b"/evm/kernel_root_hash");
const SEQUENCER_UPGRADE: RefPath = RefPath::assert_from(b"/evm/sequencer_upgrade");
pub use revm_etherlink::storage::world_state_handler::SEQUENCER_KEY_CHANGE_PATH as SEQUENCER_KEY_CHANGE;

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

pub fn store_kernel_upgrade<Host: Runtime>(
    host: &mut Host,
    kernel_upgrade: &KernelUpgrade,
) -> anyhow::Result<()> {
    log!(
        host,
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
    host: &impl Runtime,
    path: &impl Path,
) -> anyhow::Result<Option<KernelUpgrade>> {
    read_optional_rlp(host, path).context("Failed to decode kernel upgrade")
}

pub fn read_kernel_upgrade<Host: Runtime>(
    host: &Host,
) -> anyhow::Result<Option<KernelUpgrade>> {
    read_kernel_upgrade_at(host, &KERNEL_UPGRADE)
}

pub fn upgrade<Host: Runtime>(
    host: &mut Host,
    root_hash: [u8; PREIMAGE_HASH_SIZE],
) -> anyhow::Result<()> {
    log!(host, Info, "Kernel upgrade initialisation.");

    backup_current_kernel(host)?;
    let config = upgrade_reveal_flow(root_hash);
    config
        .evaluate(host)
        .map_err(UpgradeProcessError::InternalUpgrade)?;

    host.store_write_all(&KERNEL_ROOT_HASH, &root_hash)?;
    host.store_delete(&KERNEL_UPGRADE)?;

    // Mark for reboot, the upgrade/migration will happen at next
    // kernel run, it doesn't matter if it is within the Tezos level
    // or not.
    host.mark_for_reboot()?;

    log!(host, Info, "Kernel is ready to be upgraded.");
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

pub fn store_sequencer_upgrade<Host: Runtime>(
    host: &mut Host,
    sequencer_upgrade: SequencerUpgrade,
) -> anyhow::Result<()> {
    log!(
        host,
        Info,
        "A sequencer upgrade to {} is planned for {}",
        sequencer_upgrade.sequencer.to_b58check(),
        sequencer_upgrade.activation_timestamp
    );
    let bytes = &sequencer_upgrade.rlp_bytes();
    Event::SequencerUpgrade(sequencer_upgrade).store(host)?;
    let path = OwnedPath::from(SEQUENCER_UPGRADE);
    host.store_write_all(&path, bytes)
        .context("Failed to store sequencer upgrade")
}

pub fn read_sequencer_upgrade<Host: Runtime>(
    host: &Host,
) -> anyhow::Result<Option<SequencerUpgrade>> {
    let path = OwnedPath::from(SEQUENCER_UPGRADE);
    read_optional_rlp(host, &path).context("Failed to decode sequencer upgrade")
}

fn delete_sequencer_upgrade<Host: Runtime>(host: &mut Host) -> anyhow::Result<()> {
    host.store_delete(&SEQUENCER_UPGRADE)
        .context("Failed to delete sequencer upgrade")
}

fn sequencer_upgrade<Host: Runtime>(
    host: &mut Host,
    pool_address: H160,
    sequencer: &PublicKey,
) -> anyhow::Result<()> {
    log!(host, Info, "sequencer upgrade initialisation.");

    store_sequencer(host, sequencer)?;
    store_sequencer_pool_address(host, pool_address)?;
    delete_sequencer_upgrade(host)?;
    delete_sequencer_key_change(host)?;
    log!(host, Info, "Sequencer has been updated.");
    Ok(())
}

pub fn possible_sequencer_upgrade<Host: Runtime>(host: &mut Host) -> anyhow::Result<()> {
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

pub fn read_sequencer_key_change<Host: Runtime>(
    host: &Host,
) -> anyhow::Result<Option<EVMBasedSequencerKeyChange>> {
    let path = OwnedPath::from(SEQUENCER_KEY_CHANGE);
    read_optional_rlp(host, &path).context("Failed to decode sequencer key change")
}

fn delete_sequencer_key_change<Host: Runtime>(host: &mut Host) -> anyhow::Result<()> {
    host.store_delete(&SEQUENCER_KEY_CHANGE)
        .context("Failed to delete sequencer key change")
}

fn sequencer_key_change<Host: Runtime>(
    host: &mut Host,
    key_change: EVMBasedSequencerKeyChange,
) -> anyhow::Result<()> {
    log!(host, Info, "EVM based sequencer key change initialisation.");

    store_sequencer(host, key_change.sequencer_key())?;
    delete_sequencer_key_change(host)?;

    log!(host, Info, "Sequencer key has been updated.");
    Ok(())
}

pub fn possible_sequencer_key_change<Host: Runtime>(
    host: &mut Host,
    evm_timestamp: Timestamp,
) -> anyhow::Result<()> {
    let upgrade = read_sequencer_key_change(host)?;
    if let Some(upgrade) = upgrade {
        if evm_timestamp >= upgrade.activation_timestamp() {
            sequencer_key_change(host, upgrade)?;
            blueprint_storage::clear_all_blueprints(host)?;
        }
    }
    Ok(())
}
