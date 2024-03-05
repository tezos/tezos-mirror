const utils = require('./utils');
const {ethers} = require('ethers');
let faucet = require('./players/faucet.json');
let player1 = require('./players/player1.json');

let txs = [];

txs.push(utils.transfer(faucet, player1, 100000000))

let long_string = "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. In fermentum et sollicitudin ac orci phasellus. Augue interdum velit euismod in pellentesque massa placerat duis ultricies. Sit amet consectetur adipiscing elit pellentesque habitant morbi tristique. Convallis convallis tellus id interdum velit laoreet. At tempor commodo ullamcorper a lacus. Sit amet purus gravida quis blandit turpis cursus in. Ornare suspendisse sed nisi lacus sed. Vitae tempus quam pellentesque nec nam. Sit amet nisl suscipit adipiscing bibendum est. Eu nisl nunc mi ipsum faucibus vitae aliquet nec. Posuere urna nec tincidunt praesent semper feugiat nibh. Hendrerit dolor magna eget est lorem ipsum dolor sit amet. Tristique et egestas quis ipsum suspendisse ultrices gravida dictum. Commodo quis imperdiet massa tincidunt nunc pulvinar sapien. Fringilla urna porttitor rhoncus dolor purus non enim. Enim lobortis scelerisque fermentum dui faucibus in ornare quam. Dictumst vestibulum rhoncus est pellentesque elit ullamcorper."
let sha_precompile_address = "0000000000000000000000000000000000000002"
let ripemd_precompile_address = "0000000000000000000000000000000000000003"
let identity_precompile_address = "0000000000000000000000000000000000000004"
let withdrawal_precompile_address = "ff00000000000000000000000000000000000001"

let withdraw_calldata_prefix =  "0xcda4fee2"

let abi = ethers.AbiCoder.defaultAbiCoder();

let valid_withdraw_destination = abi.encode(["string"], ["tz3UQN6nBQHofmgQ3pZannhiYE2CT7TEZFim"]).slice(2)
let invalid_withdraw_destination = abi.encode(["string"], [player1.addr]).slice(2)

function build_precompile_call(address, word_length) {
    return utils.send(player1, address, 0, abi.encode(["string"], [long_string.slice(0, word_length)]))
}

for (var i = 0; i < 10; i++) {
    txs.push(build_precompile_call(sha_precompile_address, i * 32));
    txs.push(build_precompile_call(ripemd_precompile_address, i * 32));
    txs.push(build_precompile_call(identity_precompile_address, i * 32));
}   

txs.push(utils.send(player1, withdrawal_precompile_address, 0, withdraw_calldata_prefix + valid_withdraw_destination))
txs.push(utils.send(player1, withdrawal_precompile_address, 1000, withdraw_calldata_prefix + valid_withdraw_destination))
txs.push(utils.send(player1, withdrawal_precompile_address, 1000, withdraw_calldata_prefix + invalid_withdraw_destination))

utils.print_bench([txs])
