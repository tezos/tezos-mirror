const utils = require('./utils');
const { ethers } = require('ethers');
let faucet = require('./players/faucet.json');
let player1 = require('./players/player1.json');

let txs = [];

txs.push(utils.transfer(faucet, player1, 100000000))

let ecadd_precompile_address = "0000000000000000000000000000000000000008"

let x1 = "2cf44499d5d27bb186308b7af7af02ac5bc9eeb6a3d147c186b21fb1b76e18da";
let y1 = "2c0f001f52110ccfe69108924926e45f0b0c868df0e7bde1fe16d3242dc715f6";

let x2 = "1fb19bb476f6b9e44e2a32234da8212f61cd63919354bc06aef31e3cfaff3ebc";
let y2 = "22606845ff186793914e03e21df544c34ffe2f2f3504de8a79d9159eca2d98d9";

let x3 = "2bd368e28381e8eccb5fa81fc26cf3f048eea9abfdd85d7ed3ab3698d63e4f90";
let y3 = "2fe02e47887507adf0ff1743cbac6ba291e66f59be6bd763950bb16041a0a85e";

let x4 = "0000000000000000000000000000000000000000000000000000000000000001";
let y4 = "30644e72e131a029b85045b68181585d97816a916871ca8d3c208c16d87cfd45";

let x5 = "1971ff0471b09fa93caaf13cbf443c1aede09cc4328f5a62aad45f40ec133eb4";
let y5 = "091058a3141822985733cbdddfed0fd8d6c104e9e9eff40bf5abfef9ab163bc7";

let x6 = "2a23af9a5ce2ba2796c1f4e453a370eb0af8c212d9dc9acd8fc02c2e907baea2";
let y6 = "23a8eb0b0996252cb548a4487da97b02422ebc0e834613f954de6c7e0afdc1fc";

function build_precompile_call(address, input) {
    return utils.send(player1, address, 0, input)
}

pair_1 =  x1 + y1 + x2 + y2 + x3 + y3
pair_2 =  x4 + y4 + x5 + y5 + x6 + y6

txs.push(build_precompile_call(ecadd_precompile_address, "0x"));
txs.push(build_precompile_call(ecadd_precompile_address, "0x" + pair_1));
txs.push(build_precompile_call(ecadd_precompile_address, "0x" + pair_1 + pair_2));
txs.push(build_precompile_call(ecadd_precompile_address, "0x" + pair_1 + pair_2 + pair_1));

let mode = utils.bench_args(process.argv);

utils.print_bench([txs], mode)
