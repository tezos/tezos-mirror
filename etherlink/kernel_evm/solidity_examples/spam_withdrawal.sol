// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-License-Identifier: MIT

pragma solidity ^0.8.0;

contract SpamWithdrawals {

    function giveFunds() external payable {}

    function doWithdrawals(uint256 n) external payable  {
        for (uint256 i = 0; i < n; i++) {
            address to = 0xff00000000000000000000000000000000000001;
            to.call{value: 1 ether}(abi.encodeWithSignature("withdraw_base58(string)", "tz1WrbkDrzKVqcGXkjw4Qk4fXkjXpAJuNP1j"));
        }
    }
}
