// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

/// Initialized with a Michelson destination and a Michelson
/// string payload; [run()] forwards the call to the destination via
/// the gateway's [callMichelson] precompile.
contract CrossRuntimeStoreEvm {
    address constant GATEWAY = 0xfF00000000000000000000000000000000000007;
    string public destination;
    bytes public michelsonString;

    function initialize(
        string calldata _destination,
        bytes calldata _michelsonString
    ) external {
        destination = _destination;
        michelsonString = _michelsonString;
    }

    function run() external payable {
        (bool ok, ) = GATEWAY.call{value: msg.value}(
            abi.encodeWithSignature(
                "callMichelson(string,string,bytes)",
                destination,
                "default",
                michelsonString
            )
        );
        require(ok, "inner CRAC failed");
    }
}
