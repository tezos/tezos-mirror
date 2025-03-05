// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use octez_riscv::pvm::node_pvm::NodePvm;
use octez_riscv::pvm::node_pvm::PvmStorage;
use octez_riscv::storage::Repo;
use octez_riscv::storage::StorageError;
use proptest::prelude::*;
use proptest::strategy::ValueTree;
use proptest::test_runner::TestRunner;

#[test]
fn test_repo() {
    let mut runner = TestRunner::default();

    let tmp_dir = tempfile::tempdir().unwrap();
    let mut test_data = Vec::new();

    // Create a new repo, commit 5 times and check that all 5 commits can
    // be checked out
    let mut repo = Repo::load(tmp_dir.path()).unwrap();
    for _ in 0..5 {
        let data = prop::collection::vec(any::<u8>(), 0..100)
            .new_tree(&mut runner)
            .unwrap()
            .current();
        let commit_id = repo.commit(&data).unwrap();
        test_data.push((commit_id, data));
    }
    for (commit_id, bytes) in test_data.iter() {
        let checked_out_data = repo.checkout(commit_id).unwrap();
        assert_eq!(checked_out_data, *bytes);
    }
    repo.close();

    // Reload the repo and check that all previous commits can be checked out
    let mut repo = Repo::load(tmp_dir.path()).unwrap();
    for (commit_id, bytes) in test_data.iter() {
        let checked_out_data = repo.checkout(commit_id).unwrap();
        assert_eq!(checked_out_data, *bytes);
    }

    // Make 5 additional commits and check that all 10 commits can be checked out
    for _ in 0..5 {
        let data = prop::collection::vec(any::<u8>(), 0..100)
            .new_tree(&mut runner)
            .unwrap()
            .current();
        let commit_id = repo.commit(&data).unwrap();
        test_data.push((commit_id, data));
    }
    for (commit_id, bytes) in test_data.iter() {
        let checked_out_data = repo.checkout(commit_id).unwrap();
        assert_eq!(checked_out_data, *bytes);
    }

    // Check that an unknown commit returns a `NotFound` error
    assert!(matches!(
        repo.checkout(&[0; 32].into()),
        Err(StorageError::NotFound(_))
    ));

    // Check that exporting a snapshot creates a new repo which contains
    // the requested commit
    let snapshot_dir = tempfile::tempdir().unwrap();
    let (export_hash, export_data) = &test_data[0];
    repo.export_snapshot(export_hash, &snapshot_dir).unwrap();
    let snapshot_repo = Repo::load(tmp_dir.path()).unwrap();
    let checked_out_data = snapshot_repo.checkout(export_hash).unwrap();
    assert_eq!(checked_out_data, *export_data);

    tmp_dir.close().unwrap();
    snapshot_dir.close().unwrap()
}

#[test]
fn test_repo_serialised() {
    let mut runner = TestRunner::default();

    let tmp_dir = tempfile::tempdir().unwrap();
    let mut test_data = Vec::new();

    // Create a new repo, commit 5 times and check that all 5 commits can
    // be checked out
    let mut repo = Repo::load(tmp_dir.path()).unwrap();
    for _ in 0..5 {
        let data = prop::collection::vec(any::<u8>(), 0..100)
            .new_tree(&mut runner)
            .unwrap()
            .current();
        let commit_id = repo.commit_serialised(&data).unwrap();
        test_data.push((commit_id, data));
    }
    for (commit_id, bytes) in test_data.iter() {
        let checked_out_data: Vec<u8> = repo.checkout_serialised(commit_id).unwrap();
        assert_eq!(checked_out_data, *bytes);
    }
    repo.close();

    // Reload the repo and check that all previous commits can be checked out
    let mut repo = Repo::load(tmp_dir.path()).unwrap();
    for (commit_id, bytes) in test_data.iter() {
        let checked_out_data: Vec<u8> = repo.checkout_serialised(commit_id).unwrap();
        assert_eq!(checked_out_data, *bytes);
    }

    // Make 5 additional commits and check that all 10 commits can be checked out
    for _ in 0..5 {
        let data = prop::collection::vec(any::<u8>(), 0..100)
            .new_tree(&mut runner)
            .unwrap()
            .current();
        let commit_id = repo.commit_serialised(&data).unwrap();
        test_data.push((commit_id, data));
    }
    for (commit_id, bytes) in test_data.iter() {
        let checked_out_data: Vec<u8> = repo.checkout_serialised(commit_id).unwrap();
        assert_eq!(checked_out_data, *bytes);
    }

    // Check that an unknown commit returns a `NotFound` error
    assert!(matches!(
        repo.checkout_serialised::<Vec<u8>>(&[0; 32].into()),
        Err(StorageError::NotFound(_))
    ));

    // Check that exporting a snapshot creates a new repo which contains
    // the requested commit
    let snapshot_dir = tempfile::tempdir().unwrap();
    let (export_hash, export_data) = &test_data[0];
    repo.export_snapshot(export_hash, &snapshot_dir).unwrap();
    let snapshot_repo = Repo::load(tmp_dir.path()).unwrap();
    let checked_out_data: Vec<u8> = snapshot_repo.checkout_serialised(export_hash).unwrap();
    assert_eq!(checked_out_data, *export_data);

    tmp_dir.close().unwrap();
    snapshot_dir.close().unwrap()
}

// Mirrors `src/lib_riscv/pvm/test/test_storage.ml`
#[test]
fn test_pvm_storage() {
    let tmp_dir = tempfile::tempdir().unwrap();
    let empty = NodePvm::empty();
    let mut repo = PvmStorage::load(tmp_dir.path()).unwrap();
    let id = repo.commit(&empty).unwrap();
    let checked_out_empty = repo.checkout(&id).unwrap();
    assert_eq!(empty, checked_out_empty);
    let id2 = repo.commit(&empty).unwrap();
    assert_eq!(id, id2);
    repo.close()
}
