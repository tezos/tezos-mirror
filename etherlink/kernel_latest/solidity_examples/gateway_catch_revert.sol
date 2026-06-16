// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

contract GatewayCatchRevert {
    address constant gateway = 0xfF00000000000000000000000000000000000007;

    bool public firstCallSuccess;
    bool public secondCallSuccess;
    bool public executionCompleted;

    function testCatchRevert(
        string memory validAddress,
        string memory failingAddress
    ) external payable {
        uint256 half = msg.value / 2;

        // First call: should succeed (transfer to valid Michelson address)
        (bool ok1, ) = gateway.call{value: half}(
            abi.encodeWithSignature("transfer(string)", validAddress)
        );
        firstCallSuccess = ok1;

        // Second call: should revert (always_fails KT1)
        (bool ok2, ) = gateway.call{value: half}(
            abi.encodeWithSignature("transfer(string)", failingAddress)
        );
        secondCallSuccess = ok2;

        // Proves execution continued past the caught revert
        executionCompleted = true;
    }
}
