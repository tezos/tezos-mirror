// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

/// Calls the runtime gateway with [callMichelson(destination,
/// "default", michelsonString)] and then reverts unconditionally.
contract CracStoreThenRevert {
    address constant GATEWAY = 0xfF00000000000000000000000000000000000007;

    function callAndRevert(
        string memory destination,
        bytes memory michelsonString
    ) external payable {
        (bool ok, ) = GATEWAY.call{value: msg.value}(
            abi.encodeWithSignature(
                "callMichelson(string,string,bytes)",
                destination,
                "default",
                michelsonString
            )
        );
        require(ok, "inner CRAC failed");
        revert("intentional");
    }
}
