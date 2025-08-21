const utils = require('./utils');
const { ethers } = require('ethers');
let faucet = require('./players/faucet.json');
let player1 = require('./players/player1.json');

let txs = [];

txs.push(utils.transfer(faucet, player1, 100000000))

let long_string = Buffer.from('Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. In fermentum et sollicitudin ac orci phasellus. Augue interdum velit euismod in pellentesque massa placerat duis ultricies. Sit amet consectetur adipiscing elit pellentesque habitant morbi tristique. Convallis convallis tellus id interdum velit laoreet. At tempor commodo ullamcorper a lacus. Sit amet purus gravida quis blandit turpis cursus in. Ornare suspendisse sed nisi lacus sed. Vitae tempus quam pellentesque nec nam. Sit amet nisl suscipit adipiscing bibendum est. Eu nisl nunc mi ipsum faucibus vitae aliquet nec. Posuere urna nec tincidunt praesent semper feugiat nibh. Hendrerit dolor magna eget est lorem ipsum dolor sit amet. Tristique et egestas quis ipsum suspendisse ultrices gravida dictum. Commodo quis imperdiet massa tincidunt nunc pulvinar sapien. Fringilla urna porttitor rhoncus dolor purus non enim. Enim lobortis scelerisque fermentum dui faucibus in ornare quam. Dictumst vestibulum rhoncus est pellentesque elit ullamcorper.').toString('hex')
let blake2f_precompile_address = "0000000000000000000000000000000000000009"


function build_precompile_call(address, input) {
    return utils.send(player1, address, 0, input)
}
let input = "48c9bdf267e6096a3ba7ca8485ae67bb2bf894fe72f36e3cf1361d5f3af54fa5d182e6ad7f520e\
511f6c3e2b8c68059b6bbd41fbabd9831f79217e1319cde05b\
616263000000000000000000000000000000000000000000000000000000000000000000000000\
000000000000000000000000000000000000000000000000000000000000000000000000000000\
000000000000000000000000000000000000000000000000000000000000000000000000000000\
0000000000000000000000\
03000000000000000000000000000000\
01"
for (var i = 0; i < 128; i++) {
    // we add after the input an growing arbitrary number of bytes
    // they should be ignored by the precompiled contract and not impact the performances
    txs.push(build_precompile_call(blake2f_precompile_address, '0x' + i.toString(16).padStart(8, '0') + input));
}

let mode = utils.bench_args(process.argv);

utils.print_bench([txs], mode)
