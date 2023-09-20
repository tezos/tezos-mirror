// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT

// create contract and call it
// example "storage.sol"

const utils = require('./utils');
const addr = require('../lib/address');
let faucet = require('./players/faucet.json');

let create_data = "0x608060405234801561001057600080fd5b5061017f806100206000396000f3fe608060405234801561001057600080fd5b50600436106100415760003560e01c80634e70b1dc1461004657806360fe47b1146100645780636d4ce63c14610080575b600080fd5b61004e61009e565b60405161005b91906100d0565b60405180910390f35b61007e6004803603810190610079919061011c565b6100a4565b005b6100886100ae565b60405161009591906100d0565b60405180910390f35b60005481565b8060008190555050565b60008054905090565b6000819050919050565b6100ca816100b7565b82525050565b60006020820190506100e560008301846100c1565b92915050565b600080fd5b6100f9816100b7565b811461010457600080fd5b50565b600081359050610116816100f0565b92915050565b600060208284031215610132576101316100eb565b5b600061014084828501610107565b9150509291505056fea2646970667358221220ec57e49a647342208a1f5c9b1f2049bf1a27f02e19940819f38929bf67670a5964736f6c63430008120033"
// call : store(42)
let call_data = "0x60fe47b10000000000000000000000000000000000000000000000000000000000000024"
let nb_players = 10
let nb_calls = 5
let players = []
for (let i = 0; i < nb_players; i++) {
    players.push(addr.create_player())
}

let txs_1 = [];
// every player get the same amount
for (player of players) {
    txs_1.push(utils.transfer(faucet, player, 100000000))
}

// first player originates the contract
let create = utils.create(players[0], 0, create_data)
txs_1.push(create.tx)

let txs_2 = [];
// every player store 42 "nb_calls" times
for (player1 of players) {
    for (let index = 0; index < nb_calls; index++) {
        txs_2.push(utils.send(player1, create.addr, 0, call_data))
    }
}

// first set of messages: initialisation
// second set of messages: calls
utils.print_bench([txs_1, txs_2])
