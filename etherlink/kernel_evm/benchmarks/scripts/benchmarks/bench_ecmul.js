const utils = require('./utils');
let faucet = require('./players/faucet.json');
let player1 = require('./players/player1.json');

let txs = [];

txs.push(utils.transfer(faucet, player1, 100000000))

let ecadd_precompile_address = "0000000000000000000000000000000000000007"

let x = "030644e72e131a029b85045b68181585d97816a916871ca8d3c208c16d87cfd3";
let y = "15ed738c0e0a7c92e7845f96b2ae9c0a68a6a449e3538fc7ff3ebf7a5a18a2c4";

let s1 = "0000000000000000000000000000000000000000000000000000000000000001";
let s2 = "00000000000000000000000000000000000000000000000f0000000000000000";
let s3 = "000000000000000000000000000000f000000000000000000000000000000000";
let s4 = "0000000000000f00000000000000000000000000000000000000000000000000";
let s5 = "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff";

function build_precompile_call(address, input) {
    return utils.send(player1, address, 0, input)
}

let input_1 = "0x" + x + y + s1;
let input_2 = "0x" + x + y + s2;
let input_3 = "0x" + x + y + s3;
let input_4 = "0x" + x + y + s4;
let input_5 = "0x" + x + y + s5;

txs.push(build_precompile_call(ecadd_precompile_address, input_1));
txs.push(build_precompile_call(ecadd_precompile_address, input_2));
txs.push(build_precompile_call(ecadd_precompile_address, input_3));
txs.push(build_precompile_call(ecadd_precompile_address, input_4));
txs.push(build_precompile_call(ecadd_precompile_address, input_5));

utils.print_bench([txs])
