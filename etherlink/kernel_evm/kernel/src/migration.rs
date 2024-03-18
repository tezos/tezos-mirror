// SPDX-FileCopyrightText: 2023 Functori <contact@functori.com>
// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2024 Trilitech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use crate::blueprint_storage::{blueprint_path, read_next_blueprint_number};

use crate::delayed_inbox::{DelayedInbox, DelayedInboxItem, Hash};
use crate::error::Error;
use crate::error::UpgradeProcessError::Fallback;
use crate::storage::{
    read_chain_id, read_l1_level, read_last_info_per_level_timestamp, read_rlp,
    read_storage_version, store_storage_version, DELAYED_BRIDGE, STORAGE_VERSION,
};
use tezos_evm_logging::{log, Level::Info};
use tezos_smart_rollup_host::path::OwnedPath;
use tezos_smart_rollup_host::path::{concat, RefPath};
use tezos_smart_rollup_host::runtime::{Runtime, RuntimeError};

pub enum MigrationStatus {
    None,
    InProgress,
    Done,
}

// /!\ the following functions are migratin helpers, do not remove them /!\

#[allow(dead_code)]
fn is_etherlink_ghostnet(host: &impl Runtime) -> anyhow::Result<bool> {
    let chain_id = read_chain_id(host)?;
    Ok(chain_id == 128123.into())
}

#[allow(dead_code)]
fn allow_path_not_found(res: Result<(), RuntimeError>) -> Result<(), RuntimeError> {
    match res {
        Ok(()) => Ok(()),
        Err(RuntimeError::PathNotFound) => Ok(()),
        Err(err) => Err(err),
    }
}

fn wipe_blueprints<Host: Runtime>(host: &mut Host) -> anyhow::Result<()> {
    let next_blueprint = read_next_blueprint_number(host)?;
    let path = blueprint_path(next_blueprint)?;
    let tmp = concat(&RefPath::assert_from(b"/tmp"), &path)?;

    if host.store_move(&path, &tmp).is_err() {
        log!(
            host,
            Info,
            "Moving the next blueprint failed, but it can be expected."
        )
    };

    allow_path_not_found(host.store_delete(&crate::blueprint_storage::EVM_BLUEPRINTS))?;

    if host.store_move(&tmp, &path).is_err() {
        log!(
            host,
            Info,
            "Moving back the next blueprint failed, but it can be expected."
        );
    };

    Ok(())
}

// Delayed inbox recorver helpers; they can be removed once the upgrade has gone through.

const LOST_DELAYED_ITEM: [&str; 28] = [
    "bac5ca9e0682439362b50806d760276b8e684147b7bd811b5daf9b097791b6c6",
    "b024a63ce6eac6762b3ed362009e82b515138b3397204e8564ca72078491b971",
    "fcd88ee15781ad0b265b0190046a991502824aecbe8c831dda31e2a8d9b536ef",
    "b9e0ea03cf66c7aeb001d090000da58408a85aef9911a291e057b5ad521b2bf2",
    "e6ddb533776ff9217f1ecab71dcc7d715dde1126e1c966e04211ff1db3b4db17",
    "fa014954c7f26593d75662b4e1fb3615852106345f83b397814f41e26a6ed032",
    "58d200deb120f01e9b0b13e5fac3ca60de6f309900db39143b41689953622379",
    "85b644356b69cee8d96f48a0c34db4d7c853dcdde6cd579213036d1d3bc6074c",
    "9e1326f29559a4496e6c4226489443e24a975804a42d6c34e410168558517156",
    "cd47a6a21e0898b71d54849d43540fc4fc39661c8ac59554f7affc4e89424c2f",
    "d22431d1c36615bfc2d088a2b891da74366ff3fee1b7dec7baab71a2507b971c",
    "e211407e011b92ef8d15694f21cad4a6705a8260d64ba6d545bd223820362007",
    "09e1babafc181dc07f6b4534228aba5d157b4d27b4c9c240f2ae677eae13f72a",
    "fcacca9bb57a29daad3ef04608da9c1ac9149202f605e7972d904605691d1c83",
    "129bd2fe9596c6c608a535c0cb2d77be37fa952606d41bcd83fc1d484e633824",
    "ba0f0c28a14dd943c4b17210f94fdebb3639ea228accdb413d0c66382c5bb56c",
    "3935b1046897d4b98693385851befe4fa7dbcd8d9474f5a11864fc0b52a27b6f",
    "5b892175d75729fb80dd9015a14490206f1c351753ee8c9b299febbb8f7f47a2",
    "903d4ef4aee633a91732ae8467041b3ef3bf499f1374cd76c96d5c33e176e375",
    "1f9b769d9cead763ec758501b21c95db5677c713398917a2b3d6a3058f36d725",
    "ed86c6b1790fa60d018ee2f02ed8bff91adef56ec540eb44c25536d066a05296",
    "1eb95522ef70cac87935115363528b9b323b8f0a196492813920d397a5744139",
    "d90bafa7c535f04344332cd6f66dc87cf5ac1cb1090851edad52a639cb5451d0",
    "6713f40ddb27b4c59a27e8154017ee6ddbd18bd29d4376cb618d03fc79a2ef12",
    "a514a7682d20cf6f642e35e4c4a9eb792346896bbc4447f498d67747b3b2f006",
    "7e6d84c6d6725b9c69e35a7a3ed058024863baa57dad55aa1be326bb2d82ce86",
    "86a92d90296d62e202a216c09060055312a9ecaf0305f69e3bf2e2f9373999fc",
    "9d81d617293356a67d739a79ca51a2d3efd67ebb374b97432795d1954456c9ae",
];

fn lost_delayed_inbox_data_path(hash: &'static str) -> anyhow::Result<OwnedPath> {
    let path: Vec<u8> = format!("/__internal_debug_delayed-inbox/{}/data", hash).into();
    let path = OwnedPath::try_from(path)?;
    Ok(path)
}

fn recover_lost_delayed_inbox<Host: Runtime>(host: &mut Host) -> anyhow::Result<()> {
    let mut delayed_inbox = DelayedInbox::new(host)?;

    let timestamp = read_last_info_per_level_timestamp(host)?;
    let level = read_l1_level(host)?;

    for hash in LOST_DELAYED_ITEM {
        log!(host, Info, "Recovering {}", hash);

        let path = lost_delayed_inbox_data_path(hash)?;

        if host.store_has(&path)?.is_none() {
            break;
        }

        let contents: DelayedInboxItem = read_rlp(host, &path)?;
        let decoded_hash = hex::decode(hash)?;
        let hash = Hash(decoded_hash.try_into().unwrap());
        let txn = DelayedInbox::transaction_from_delayed(hash, contents.transaction);
        delayed_inbox.save_transaction(host, txn, timestamp, level)?;
    }

    Ok(())
}

// The workflow for migration is the following:
//
// - bump `storage::STORAGE_VERSION` by one
// - fill the scope inside the conditional in `storage_migration` with all the
//   needed migration functions
// - compile the kernel and run all the E2E migration tests to make sure all the
//   data is still available from the EVM proxy-node.
//
// /!\
//     If the migration takes more than 999 reboots, we will lose the inbox
//     of a level. At least one reboot must be allocated to the stage one
//     to consume the inbox. Therefore, if the migration happens to take more
//     than 999 reboots, you have to rethink this. This limitation exists
//     because we consider that the inbox should not be collected during
//     a migration because it impacts the storage. We could in theory end up
//     in an inconsistent storage.
// /!\
//
fn migration<Host: Runtime>(host: &mut Host) -> anyhow::Result<MigrationStatus> {
    let current_version = read_storage_version(host)?;
    if STORAGE_VERSION == current_version + 1 {
        // MIGRATION CODE - START
        if is_etherlink_ghostnet(host)? {
            host.store_write_all(
                &DELAYED_BRIDGE,
                b"KT1J9H3fCg4WxRAR2ELZkWoZZYV9pHJhSWUB",
            )?;
            allow_path_not_found(
                host.store_delete(&RefPath::assert_from(
                    b"/evm/__applied_kernel_upgrade",
                )),
            )?;
            wipe_blueprints(host)?;
            allow_path_not_found(host.store_delete(&RefPath::assert_from(b"/tmp")))?;
            recover_lost_delayed_inbox(host)?;
        }
        // MIGRATION CODE - END
        store_storage_version(host, STORAGE_VERSION)?;
        return Ok(MigrationStatus::Done);
    }
    Ok(MigrationStatus::None)
}

pub fn storage_migration<Host: Runtime>(
    host: &mut Host,
) -> Result<MigrationStatus, Error> {
    let migration_result = migration(host);
    migration_result.map_err(|_| Error::UpgradeError(Fallback))
}
