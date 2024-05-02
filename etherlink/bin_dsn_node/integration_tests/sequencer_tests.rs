extern crate dsn_integration_tests;

use chrono::DateTime;
use dsn_core::{
    hex_string::HexString,
    types::{BlockHash, DelayedTransactionHash, Preblock, Proposal, Transaction},
};
use dsn_integration_tests::node::RpcNode;
use futures_util::StreamExt;

fn dummy_proposal() -> Proposal {
    Proposal {
        transactions: vec![
            Transaction(vec![1u8; 1]),
            Transaction(vec![2u8; 2]),
            Transaction(vec![3u8; 3]),
        ],
        delayed_transaction_hashes: vec![
            DelayedTransactionHash(HexString(vec![4u8; 32])),
            DelayedTransactionHash(HexString(vec![5u8; 32])),
            DelayedTransactionHash(HexString(vec![6u8; 32])),
        ],
        previous_block_hash: BlockHash(HexString(vec![7u8; 32])),
        current_blueprint_number: "1".into(),
        timestamp: DateTime::from_timestamp_millis(1000000000000).unwrap(),
    }
}

#[tokio::test]
async fn test_sequencer_accepts_proposals_and_returns_preblocks() {
    let mut node = RpcNode::run("dsn-node", vec!["sequencer", "--preblock-time", "1000"]);
    node.wait_till_started().await;

    let client = reqwest::Client::new();

    // Subscribe to preblocks
    let preblock_stream = client
        .get(node.endpoint().join("monitor/preblocks").unwrap())
        .send()
        .await
        .unwrap();
    assert!(preblock_stream.status().is_success());

    // Send proposal
    let proposal = dummy_proposal();
    let res = client
        .post(node.endpoint().join("proposal").unwrap())
        .json(&proposal)
        .send()
        .await
        .unwrap();
    assert!(res.status().is_success());

    let mut stream = preblock_stream.bytes_stream();
    let frame = stream.next().await.unwrap().unwrap();

    let preblock: Preblock = serde_json::from_slice(&frame).unwrap();
    assert_eq!(proposal, preblock.0);
}
