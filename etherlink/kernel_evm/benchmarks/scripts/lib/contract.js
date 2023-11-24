// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT

// use this module to compute the address of a contract from sender address and
// nonce

const rlp = require("rlp");
const keccak = require("keccak");
const solc = require("solc");
const fs = require("fs");
const path = require("path");
const ethers = require("ethers");
const { clean_input } = require('./signature');

// nonce must be a number !
const legacy_contract_address = function (player_address, nonce) {
    let hex_addr = clean_input(player_address)
    let rlp_encoded = rlp.encode([hex_addr, nonce]);
    var contract_address_long = keccak("keccak256")
        .update(rlp_encoded)
        .digest("hex");
    return contract_address_long.substring(24);
}

const contracts_directory = path.resolve(__dirname, '..', 'benchmarks', 'contracts');

const compile_contract_file = function (directory, contract_file) {
    let contract_path = path.resolve(directory, contract_file);
    let contract_content = fs.readFileSync(contract_path, "UTF-8");

    let input = {
        language: 'Solidity',
        sources: {
            'contract.sol': {
                content: contract_content
            }
        },
        settings: {
            evmVersion: 'london',
            outputSelection: {
                '*': {
                    '*': ['*']
                }
            }
        }
    };

    let output = JSON.parse(solc.compile(JSON.stringify(input)));
    var contracts = [];
    for (var contractName in output.contracts['contract.sol']) {
        let contract = output.contracts['contract.sol'][contractName];
        let infos = { name: contractName,
                      bytecode : "0x" + contract.evm.bytecode.object,
                      interface : new ethers.Interface(contract.abi) };
        contracts.push(infos);
    }

    return contracts;
}

const find_contract = function (contracts, name) {
    for (const i in contracts) {
        if (contracts[i].name == name) {
            return contracts[i]
        }
    }
}

module.exports = { legacy_contract_address, contracts_directory, compile_contract_file, find_contract }
