const { sign } = require('./scripts/lib/signature');
const ethers = require('ethers');
const crypto = require('crypto');
const fs = require('fs');
const external = require("./scripts/lib/external")
const CHUNKER = external.bin("./octez-evm-node")
const { execSync } = require('child_process');
const yaml = require('js-yaml');

function gen_accounts(n) {
    let accounts = [];
    for (var i = 0; i < n; i++) {
        let wallet = ethers.Wallet.createRandom();
        accounts.push({ address : wallet.address, privateKey : wallet.privateKey } )
    }
    return accounts
}

function readJsonFile(filename) {
  return new Promise((resolve, reject) => {
    fs.readFile(filename, 'utf8', (err, data) => {
      if (err) {
        reject(err);
        return;
      }

      const jsonData = JSON.parse(data);
      resolve(jsonData);
    });
  });
}

var args = process.argv.slice(2);

if (args[0] == 'gen_accounts') {
    let n = args[1];
    let output = args[2];
    let accounts = gen_accounts(n);
    fs.writeFile(output, JSON.stringify(accounts), (err) => {
        if (err) {
            console.error(err);
            return;
        }
        console.log("Accounts written to " + output)
    });
} else if (args[0] == 'gen_transactions') {
    let accounts_file = args[1];
    let sr1 = args[2];
    let nonce = args[3];
    let accounts = readJsonFile(accounts_file).then((accounts) => {
        let transactions = [];
        accounts.forEach((account) => {
            let tx = {
                nonce : parseInt(nonce).toString(16),
                gasPrice : 100,
                gasLimit : 21000,
                to : "0x6ce4d79d4E77402e1ef3417Fdda433aA744C6e1c",
                value : 1,
                data : "",
                chainId : 1337,
                v : 1,
                r : 0,
                s : 0,
            };
            let rawTx = sign(tx, account.privateKey);
            run_chunker_command = `${CHUNKER} chunk data "${rawTx.rawTx}" --rollup-address ${sr1}`;
            chunked_message = new Buffer.from(execSync(run_chunker_command)).toString();
            transactions.push(chunked_message.split("\n").slice(1, -1));
        });
        console.log(transactions.flat())
    });
} else if (args[0] == 'gen_config') {
    let accounts_file = args[1];
    let output = args[2];

    let accounts = readJsonFile(accounts_file).then((accounts) => {
        let instrs = []
        accounts.forEach((account) => {
            let addr = account.address.toLowerCase().slice(2);
            instrs.push({ set : {
                value : "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f",
                to: `/evm/eth_accounts/${addr}/balance`
            }});
        });

        let config = { instructions : instrs };

        fs.writeFile(output, yaml.dump(config), (err) => {
            if (err) {
                console.error(err);
                return;
            }
            console.log("Config written to " + output)
        });
    })
} else {
    console.log(`Another command`);
}
