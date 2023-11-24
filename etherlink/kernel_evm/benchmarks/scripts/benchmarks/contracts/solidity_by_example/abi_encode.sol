// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

interface IERC20 {
    function transfer(address, uint) external;
}

contract Token {
    function transfer(address, uint) external {}
}

contract AbiEncode {
    function test(address _contract, address to, uint amount) external {
        (bool ok, ) = _contract.call(abi.encodeCall(IERC20.transfer, (to, amount)));
        require(ok, "call failed");
    }
}
