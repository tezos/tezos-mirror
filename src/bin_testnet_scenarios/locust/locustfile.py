from locust import HttpUser, task
from json import JSONDecodeError
import json 

with open('src/bin_testnet_scenarios/locust/config.json', 'r') as file:
    data = json.load(file)

userAddr = data["account_address"]
blockNum = data["block_num"]
blockHash = data["block_hash"]
txHash = data["tx_hash"]
contractAddr = data["contract_address"]
rawTx = data["raw_tx"]
rpc_list = data["rpc_list"]


empty_method = ["eth_coinbase"]

uncleIndex = "0x0"

rpc_tuple = {
    "eth_getBalance": [userAddr, "latest"],
    "eth_getBlockByNumber": [blockNum, True],
    "eth_getBlockTransactionCountByHash": [blockHash],
    "eth_getBlockTransactionCountByNumber": [blockNum],
    "eth_getUncleByBlockHashAndIndex": [blockHash, uncleIndex],
    "eth_getUncleByBlockNumberAndIndex": [blockNum, uncleIndex],
    "eth_getUncleCountByBlockNumber": [blockNum],
    "eth_getTransactionByHash": [txHash],
    "eth_getBlockByHash": [blockHash, True],
    "eth_getUncleCountByBlockHash": [blockHash],
    "eth_getTransactionByBlockHashAndIndex": [blockHash, "0x0"],
    "eth_getTransactionByBlockNumberAndIndex": [blockNum, "0x0"],
    "eth_getTransactionCount": [userAddr, "latest"],
    "eth_getTransactionReceipt": [txHash],
    "eth_getCode": [contractAddr, "latest"],
    "eth_getStorageAt": [contractAddr, "0x0", "latest"],
    "eth_getBlockReceipts": ["latest"],
    "web3_sha3": ["0x5610a654"],
    "eth_estimateGas": [{"from":userAddr,"to":userAddr,"value":"0x1"}],
    # TODO : Add last rpc https://gitlab.com/tezos/tezos/-/issues/7663
}


def map_f(x):
    if x in rpc_tuple:
        return (x,rpc_tuple[x])
    else:
        return (x,[])
    
def get_all_rpc():
    l = [] 
    for k,v in rpc_tuple.items():
        l.append((k,v))
    return l


rpc_list = list(map(map_f, rpc_list)) if len(rpc_list)>0 else get_all_rpc()



class RPC(HttpUser):
    def make(self, method, params):
        headers = {"Content-Type": "application/json"}
        json = {"jsonrpc":"2.0","method":method,"id":1} if method in empty_method else {"jsonrpc":"2.0","method":method,"params":params,"id":1}
        return self.client.post("/",name=method,headers=headers, json=json, catch_response=True)


    def rpc_request(self, method, params=None):
        """
        Generic function to call any JSON-RPC method.
        """
        if params is None:
            params = []

        with self.make(method, params) as response:
            try:
                response_data = response.json()
                if "result" not in response_data:
                    print(response_data)
                    response.failure(f"Response did not contain 'result' for method {method} : {response_data}")
                else:
                    return response_data["result"]
            except JSONDecodeError:
                response.failure(f"Response for {method} could not be decoded as JSON")
            except Exception as e:
                response.failure(f"Unexpected error for {method}: {str(e)}")
            return None

    @task
    def rpc(self):
        for (rpc,arg) in rpc_list:
            self.rpc_request(rpc,arg)
      
