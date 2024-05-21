// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use octez_riscv::storage::{Repo, StorageError};
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
    let mut repo = Repo::<Vec<u8>>::load(tmp_dir.path()).unwrap();
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
    let mut repo = Repo::<Vec<u8>>::load(tmp_dir.path()).unwrap();
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
        repo.checkout(&[0; 32]),
        Err(StorageError::NotFound(_))
    ));

    tmp_dir.close().unwrap()
}
