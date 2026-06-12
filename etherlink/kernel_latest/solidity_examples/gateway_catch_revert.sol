// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

contract GatewayCatchRevert {
    address constant gateway = 0xfF00000000000000000000000000000000000007;

    struct Header {
        string name;
        string value;
    }

    bool public firstCallSuccess;
    bool public secondCallSuccess;
    bool public executionCompleted;

    /// Build the gateway `call` calldata for a plain transfer to a
    /// Michelson address: a POST to http://tezos/<address> with an
    /// empty body.
    function transferCalldata(string memory destination)
        internal
        pure
        returns (bytes memory)
    {
        return
            abi.encodeWithSignature(
                "call(string,(string,string)[],bytes,uint8)",
                string.concat("http://tezos/", destination),
                new Header[](0),
                bytes(""),
                uint8(1)
            );
    }

    function testCatchRevert(
        string memory validAddress,
        string memory failingAddress
    ) external payable {
        uint256 half = msg.value / 2;

        // First call: should succeed (transfer to valid Michelson address)
        (bool ok1, ) = gateway.call{value: half}(transferCalldata(validAddress));
        firstCallSuccess = ok1;

        // Second call: should revert (always_fails KT1)
        (bool ok2, ) = gateway.call{value: half}(
            transferCalldata(failingAddress)
        );
        secondCallSuccess = ok2;

        // Proves execution continued past the caught revert
        executionCompleted = true;
    }
}
