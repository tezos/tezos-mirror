// Code taken and adapted from https://ethereum.stackexchange.com/questions/124636/set-data-for-call-delegatecall-etc-in-yul-inline-assembly
// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

contract Delegate {

    function myFunction(bytes4 param1, uint256 param2) public {

    }
}

contract Delegator {

    Delegate delegate;

    function caller(uint256 runs, Delegate _delegate) public {
        delegate = _delegate;
        bytes4 fnSig = hex"7e965aeb";
        bytes4 param1 = hex"c48d6d5e";
        uint256 param2 = 12345678;

        //bytes memory encoded = abi.encodePacked(fnSig, abi.encode(param1, param2));
        //bytes memory encoded = abi.encodeWithSelector(fnSig, param1, param2);
        bytes memory encoded = abi.encodeWithSignature("myFunction(bytes4,uint256)", param1, param2);

        for (uint256 i=0; i < runs; i++) {
            assembly {

                // Get the value at slot `delegate.slot` to get the address of our target contract
                let implementation := sload(delegate.slot)

                // gas : gas()
                // address : implementation -> sload(delegate.slot)
                // argsOffset : add(encoded, 0x20) -> skip the size of the array
                // argsSize : mload(encoded) -> first element of the array is its size
                // retOffset : 0
                // retSize : 0
                let result := delegatecall(gas(), implementation, add(encoded, 0x20), mload(encoded), 0, 0)

                if eq(result, 0) {
                    revert(0, returndatasize())
                }
            }
        }
    }
}
