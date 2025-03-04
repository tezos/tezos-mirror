// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

pragma solidity ^0.8.0;

contract CallPrecompile {
    function forwardWithdrawal(string memory receiver, string memory fast_withdrawal_contract_address) public payable {
        address to = 0xff00000000000000000000000000000000000001;
        // Encode the call to fast_withdraw_base58 with the appropriate arguments
        (bool success,) = to.call{value: msg.value}(
            abi.encodeWithSignature(
                "fast_withdraw_base58(string,string,bytes)",
                receiver,
                fast_withdrawal_contract_address,
                hex"abcd1234"
            )
        );
        require(success, "forwardWithdrawal failed");
    }
}
