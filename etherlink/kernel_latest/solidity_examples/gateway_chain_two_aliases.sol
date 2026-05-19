// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

/// Two single-purpose proxies, each calling the runtime gateway with
/// half of the value they receive. A driver contract holds one
/// instance of each; the entrypoint `runTwoCalls` invokes them in
/// sequence. Because each proxy's address is the [msg.sender]
/// observed by the gateway precompile, the two cross-runtime calls
/// resolve to two DISTINCT senders and therefore materialize two
/// DISTINCT Tezos-side aliases -- exercising the path where one EVM
/// transaction surfaces multiple alias originations in the merged
/// Michelson receipt.
contract GatewayProxyA {
    address constant gateway = 0xfF00000000000000000000000000000000000007;

    function transfer(string memory destination) external payable {
        (bool ok, ) = gateway.call{value: msg.value}(
            abi.encodeWithSignature("transfer(string)", destination)
        );
        require(ok, "GatewayProxyA: inner call failed");
    }
}

contract GatewayProxyB {
    address constant gateway = 0xfF00000000000000000000000000000000000007;

    function transfer(string memory destination) external payable {
        (bool ok, ) = gateway.call{value: msg.value}(
            abi.encodeWithSignature("transfer(string)", destination)
        );
        require(ok, "GatewayProxyB: inner call failed");
    }
}

contract GatewayChainTwoAliases {
    GatewayProxyA public proxyA;
    GatewayProxyB public proxyB;

    constructor() {
        proxyA = new GatewayProxyA();
        proxyB = new GatewayProxyB();
    }

    function runTwoCalls(
        string memory destinationA,
        string memory destinationB
    ) external payable {
        uint256 half = msg.value / 2;
        proxyA.transfer{value: half}(destinationA);
        proxyB.transfer{value: half}(destinationB);
    }
}
