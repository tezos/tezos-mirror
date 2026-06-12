// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

/// Calls the runtime gateway with a transfer to the given Michelson
/// address (forwarding msg.value), then reverts unconditionally. Used
/// to verify that an alias materialization triggered by the gateway
/// call is rolled back to a Backtracked status when the enclosing
/// EVM frame reverts.
contract GatewayCallThenRevert {
    address constant gateway = 0xfF00000000000000000000000000000000000007;

    struct Header {
        string name;
        string value;
    }

    function callAndRevert(string memory destination) external payable {
        (bool ok, ) = gateway.call{value: msg.value}(
            abi.encodeWithSignature(
                "call(string,(string,string)[],bytes,uint8)",
                string.concat("http://tezos/", destination),
                new Header[](0),
                bytes(""),
                uint8(1)
            )
        );
        require(ok, "inner gateway call failed");
        revert("intentional");
    }
}
