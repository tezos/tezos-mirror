// SPDX-License-Identifier: MIT
pragma solidity ^0.8.30;

contract StaticCaller {
    function makeStaticCall(address target, bytes calldata payload)
        external
        view
        returns (bool)
    {
        (bool ok, bytes memory ret) = target.staticcall(payload);
        if (!ok) {
            // Propagate
            assembly {
                revert(add(ret, 0x20), mload(ret))
            }
        }
        return ok;
    }
}
