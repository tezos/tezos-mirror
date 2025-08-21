// SPDX-License-Identifier: MIT
pragma solidity ^0.8.17;

contract calldata_example {

    event Log(address indexed sender, string message);

    function call(string calldata variable) public {
        emit Log(msg.sender, variable);
    }

    function exec(string calldata variable, uint256 runner) public {
        if (runner-- > 0) {
            call(variable);
        }
    }

    function remote(calldata_example rem, string calldata variable, uint256 ext_runner, uint256 runner) public {
        if (runner-- > 0) {
            calldata_example my = rem;
            my.exec(variable, ext_runner);
        }
    }
}
