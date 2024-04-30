extern crate dsn_integration_tests;

use dsn_integration_tests::node::RpcNode;
use ethers::{
    abi::AbiDecode,
    providers::{Middleware, ProviderError},
    types::H256,
};
use jsonrpsee_core::{
    client::{BatchResponse, ClientT},
    params::BatchRequestBuilder,
};
use jsonrpsee_http_client::HttpClientBuilder;

#[tokio::test]
async fn test_bundler_proxies_only_raw_transaction_requests() {
    let mut mock = RpcNode::run("jsonrpc-mock", vec![]);
    mock.wait_till_started().await;

    let sequencer_url = mock.endpoint().to_string();

    let mut node = RpcNode::run(
        "dsn-node",
        vec!["bundler", "--sequencer-url", sequencer_url.as_str()],
    );
    node.wait_till_started().await;

    let provider = node.as_provider();

    // Bundler does not support methods other from send raw tx
    if let Err(ProviderError::JsonRpcClientError(_)) = provider.get_block_number().await {
    } else {
        panic!("Expected JSON-RPC error")
    }

    let pending_tx = provider
        .send_raw_transaction(vec![1u8; 100].into())
        .await
        .unwrap();
    // Expected mocked value
    assert_eq!(
        pending_tx.tx_hash(),
        H256::decode_hex("0x73a5b33d17d3d1d78a5133a01870717af2f96d93cf8047de7c52ba184d2a29cf")
            .unwrap()
    );

    // Test notifications and batching
    let client = HttpClientBuilder::default().build(node.endpoint()).unwrap();

    client
        .notification("eth_sendRawTransaction", ["0xdeadbeef"])
        .await
        .unwrap();

    let mut batch = BatchRequestBuilder::new();
    batch
        .insert("eth_sendRawTransaction", ["0xdeadbeef"])
        .unwrap();
    batch.insert("eth_sendRawTransaction", ["0x"]).unwrap();
    batch.insert("eth_getBlock", ["0x"]).unwrap();

    let res: BatchResponse<String> = client.batch_request(batch).await.unwrap();
    assert_eq!(2, res.num_successful_calls());
    assert_eq!(1, res.num_failed_calls());

    assert!(res.into_iter().filter_map(|r| r.ok()).all(
        |r| r.as_str() == "0x73a5b33d17d3d1d78a5133a01870717af2f96d93cf8047de7c52ba184d2a29cf"
    ));
}
