// SPDX-FileCopyrightText: 2024 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

// Usage: node benchmarks/bench_gas_guzzler.js
// The source code of the gas guzzler contract can be found in
// etherlink/kernel_latest/solidity_examples/gas_guzzler.sol

const utils = require('./utils');
const { contracts_directory, compile_contract_file } = require("../lib/contract");
let faucet = require('./players/faucet.json');
let account = require('./players/player1.json');

let contract = compile_contract_file(contracts_directory, "gas_guzzler.sol")[0];
let create_data = contract.bytecode;

const benchmarks_global = [
    { numCalls: 1, incrementAmount: 5_000, method: "incrementGlobalSum" },
];

const benchmarks_local = [
    { numCalls: 1, incrementAmount: 5_000, method: "incrementLocalSum" },
];
  
const createContract = utils.create(account, 0, create_data, { gasLimit: 3_000_000 });

let txs = [];
txs.push(utils.transfer(faucet, account, 100000000000));
txs.push(createContract.tx);

benchmarks_global.forEach(({ numCalls, incrementAmount, method }) => {
  for (let i = 0; i < numCalls; i++) {
    let data = contract.interface.encodeFunctionData(method, [incrementAmount]);
    txs.push(utils.send(account, createContract.addr, 0, data));
  }
});

benchmarks_local.forEach(({ numCalls, incrementAmount, method }) => {
    for (let i = 0; i < numCalls; i++) {
      let data = contract.interface.encodeFunctionData(method, [incrementAmount]);
      txs.push(utils.send(account, createContract.addr, 0, data));
    }
  });

let mode = utils.bench_args(process.argv);

utils.print_bench([txs], mode);
