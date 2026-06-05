// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

// Recurses to the given EVM call depth and at the deepest frame
// invokes a Michelson contract via the CRAC gateway. Asserts the
// kernel returns OOG rather than trapping when most of the compute
// budget is already spent on EVM frames.

contract DeepRecurseThenMichelson {
    address constant GATEWAY = 0xfF00000000000000000000000000000000000007;
    string public destination;

    function initialize(string calldata _destination) external {
        destination = _destination;
    }

    function recurse(uint256 depth) external {
        if (depth == 0) {
            // Low level call to bypass Solidity's EXTCODESIZE check.
            (bool ok, ) = GATEWAY.call(
                abi.encodeWithSignature(
                    "callMichelson(string,string,bytes)",
                    destination,
                    "default",
                    hex"030b"
                )
            );
            require(ok, "michelson_failed");
        } else {
            this.recurse(depth - 1);
        }
    }
}
