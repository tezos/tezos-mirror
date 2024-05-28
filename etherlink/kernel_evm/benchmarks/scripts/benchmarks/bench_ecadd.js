const utils = require('./utils');
let faucet = require('./players/faucet.json');
let player1 = require('./players/player1.json');

let txs = [];

txs.push(utils.transfer(faucet, player1, 100000000))

let ecadd_precompile_address = "0000000000000000000000000000000000000006"

let x1 = "0000000000000000000000000000000000000000000000000000000000000001";
let y1 = "0000000000000000000000000000000000000000000000000000000000000002";

let x2 = "030644e72e131a029b85045b68181585d97816a916871ca8d3c208c16d87cfd3";
let y2 = "15ed738c0e0a7c92e7845f96b2ae9c0a68a6a449e3538fc7ff3ebf7a5a18a2c4";

function build_precompile_call(address, input) {
    return utils.send(player1, address, 0, input)
}
let input_small_int = "0x" + x1 + y1 + x1 + y1;
let input_large_int = "0x" + x1 + y1 + x2 + y2;
let input_large_int_2 = "0x" + x2 + y2 + x2 + y2;

txs.push(build_precompile_call(ecadd_precompile_address, input_small_int));
txs.push(build_precompile_call(ecadd_precompile_address, input_large_int));
txs.push(build_precompile_call(ecadd_precompile_address, input_large_int_2));

let mode = utils.bench_args(process.argv);

utils.print_bench([txs], mode)
