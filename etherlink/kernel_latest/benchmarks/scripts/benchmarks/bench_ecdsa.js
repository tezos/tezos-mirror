const utils = require('./utils');
const { ethers } = require('ethers');
let faucet = require('./players/faucet.json');
let player1 = require('./players/player1.json');

let txs = [];

txs.push(utils.transfer(faucet, player1, 100000000))

let long_string = Buffer.from('Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. In fermentum et sollicitudin ac orci phasellus. Augue interdum velit euismod in pellentesque massa placerat duis ultricies. Sit amet consectetur adipiscing elit pellentesque habitant morbi tristique. Convallis convallis tellus id interdum velit laoreet. At tempor commodo ullamcorper a lacus. Sit amet purus gravida quis blandit turpis cursus in. Ornare suspendisse sed nisi lacus sed. Vitae tempus quam pellentesque nec nam. Sit amet nisl suscipit adipiscing bibendum est. Eu nisl nunc mi ipsum faucibus vitae aliquet nec. Posuere urna nec tincidunt praesent semper feugiat nibh. Hendrerit dolor magna eget est lorem ipsum dolor sit amet. Tristique et egestas quis ipsum suspendisse ultrices gravida dictum. Commodo quis imperdiet massa tincidunt nunc pulvinar sapien. Fringilla urna porttitor rhoncus dolor purus non enim. Enim lobortis scelerisque fermentum dui faucibus in ornare quam. Dictumst vestibulum rhoncus est pellentesque elit ullamcorper.').toString('hex')
let ecdsa_precompile_address = "0000000000000000000000000000000000000001"


function build_precompile_call(address, input) {
    return utils.send(player1, address, 0, input)
}
let input = "0x456e9aea5e197a1f1af7a3e85a3212fa4049a3ba34c2289b4c860fc0b0c64ef3000000000000000000000000000000000000000000000000000000000000001c9242685bf161793cc25603c231bc2f568eb630ea16aa137d2664ac80388256084f8ae3bd7535248d0bd448298cc2e2071e56992d0774dc340c368ae950852ada"
for (var i = 0; i < 10; i++) {
    // we add after the input an growing arbitrary number of bytes
    // they should be ignored by the precompiled contract and not impact the performances
    txs.push(build_precompile_call(ecdsa_precompile_address, input + long_string.slice(0, i * 32)));
}

let mode = utils.bench_args(process.argv);

utils.print_bench([txs], mode)
