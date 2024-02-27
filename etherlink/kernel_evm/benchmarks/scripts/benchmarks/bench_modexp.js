const utils = require('./utils');
const {ethers} = require('ethers');
let faucet = require('./players/faucet.json');
let player1 = require('./players/player1.json');

let txs = [];

txs.push(utils.transfer(faucet, player1, 100000000))

let modexp_address = "0000000000000000000000000000000000000005"

let abi = ethers.AbiCoder.defaultAbiCoder();

function build_modexp_call(bsize, esize, msize, b, e, m) {
  let data =
    abi.encode(['uint', 'uint', 'uint'], [bsize, esize, msize]) + b + e + m;

  return utils.send(
    player1,
    modexp_address,
    0,
    data)
}

for(var bs = 1; bs < 5; bs ++) {
  for(var es = 1; es < 5; es ++) {
    for(var ms = 1; ms < 5; ms ++) {
      let bsize = bs * 10
      let esize = es * 10
      let msize = ms * 10

      let b = "88".repeat(bsize);
      let e = "89".repeat(esize);
      let m = "84".repeat(msize);

      txs.push(build_modexp_call(bsize, esize, msize, b, e, m));
    }
  }
}

let mode = utils.bench_args(process.argv);

utils.print_bench([txs], mode);
