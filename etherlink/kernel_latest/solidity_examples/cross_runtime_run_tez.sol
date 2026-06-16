// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

// EVM-to-Michelson bridge for CRAC test scenarios.
//
// `run()` calls the CRAC gateway to invoke the Michelson entrypoint
// `%run` on `destination` with argument Unit (Micheline-encoded as
// 0x030b). The destination contract must expose a parameterless
// `%run` entrypoint.
//
// Since this contract itself exposes `run()`, it can be composed
// with multi_run_caller.sol, multi_run_caller.tz and
// cross_runtime_run_evm.tz to build arbitrary cross-runtime
// call trees.
//
// Michelson counterpart: cross_runtime_run_evm.tz (TEZ-to-EVM bridge).

contract CrossRuntimeRunTez {
    address constant GATEWAY = 0xfF00000000000000000000000000000000000007;
    uint256 public count;
    string public destination;

    error CrossRuntimeRunTezFailed();

    function initialize(string calldata _destination) external {
        destination = _destination;
    }

    function run() external payable {
        count++;

        // Use low-level call to bypass Solidity's EXTCODESIZE check
        // (the gateway is a precompile with no deployed code).
        (bool success, ) = GATEWAY.call{value: msg.value}(
            abi.encodeWithSignature(
                "callMichelson(string,string,bytes)",
                destination,
                "run",
                hex"030b" // Micheline encoding of Unit
            )
        );

        if (!success) {
            revert CrossRuntimeRunTezFailed();
        }

        count++;
    }
}
