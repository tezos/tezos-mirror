// SPDX-License-Identifier: MIT
pragma solidity ^0.8.30;

contract DelegateCaller {
    function makeDelegateCall(address target, bytes calldata payload)
        external
        returns (bool)
    {
        (bool ok, bytes memory ret) = target.delegatecall(payload);
        if (!ok) {
            // Propagate
            assembly {
                revert(add(ret, 0x20), mload(ret))
            }
        }
        return ok;
    }
}
