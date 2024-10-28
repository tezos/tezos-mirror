// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

pragma solidity ^0.8.0;

contract CallRevert {
    function failFunction() public pure {
        revert("This function reverts");
    }

    function callerFunction() public pure {
      failFunction();
    }
}
