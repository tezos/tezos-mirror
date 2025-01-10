# RPC supported :

- eth_accounts
- eth_blockNumber	
- eth_call
- eth_chainId	
- eth_coinbase	
- eth_estimateGas	
- eth_feeHistory	
- eth_gasPrice	
- eth_getBalance	
- eth_getBlockByHash	
- eth_getBlockByNumber	
- eth_getBlockReceipts	
- eth_getBlockTransactionCountByHash	
- eth_getBlockTransactionCountByNumber	
- eth_getCode	
- eth_getProof	
- eth_getStorageAt	
- eth_getTransactionByBlockHashAndIndex	
- eth_getTransactionByBlockNumberAndIndex	
- eth_getTransactionByHash	
- eth_getTransactionCount	
- eth_getTransactionReceipt	
- eth_getUncleByBlockHashAndIndex	
- eth_getUncleByBlockNumberAndIndex	
- eth_getUncleCountByBlockHash	
- eth_getUncleCountByBlockNumber	
- eth_maxPriorityFeePerGas	
- net_version	
- web3_clientVersion	
- web3_sha3

# RPC not yet supported :
- debug_traceTransaction
- eth_call
- eth_sendRawTransaction (Locust can't handle the nonce when it floods transactions.)
- eth_getLogs	
       
TODO : Add last rpc https://gitlab.com/tezos/tezos/-/issues/7663

https://docs.etherlink.com/building-on-etherlink/endpoint-support/
