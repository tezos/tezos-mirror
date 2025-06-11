// SPDX-FileCopyrightText: 2024 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

pragma solidity >=0.8.2 <0.9.0;

contract Stop {
    function call() public {}
}

contract FirstContract {
    constructor(address stop) {
        (bool success, ) = stop.call(abi.encodeWithSignature("call()"));
        require(success, "Test call in constructor failed");
    }

    function callNext(
        address next,
        uint256 n
    ) public returns (bool, bytes memory) {
        return next.call(abi.encodeWithSignature("call(uint256)", n));
    }
}

contract SecondContract {
    uint256 public result;
    address stop;

    constructor(address _stop) {
        stop = _stop;
    }

    function call(uint256 n) public returns (bool, bytes memory) {
        address callee = msg.sender;
        (bool success, ) = callee.call(
            abi.encodeWithSignature("callNext(address,uint256)", stop, n)
        );
        require(success, "Second Contract Call failed");

        result = n + 5;

        if (result > 10) {
            revert("Result exceeds the maximum allowed value of 10");
        }

        return
            callee.call(
                abi.encodeWithSignature("callNext(address,uint256)", stop, n)
            );
    }
}

contract TestDepthCall {
    function startDepth(uint256 n) public {
        // Simple stop contract to avoid looping.
        address stop = address(new Stop());
        // FirstContract takes stop in arguments to verify call in create
        FirstContract first = new FirstContract(stop);
        // The second contract will call stop and re-call first contract with
        // stop in parameter.
        address snd = address(new SecondContract(stop));
        // We call firstContract with second in argument to have a depth 2 call
        (bool success, ) = first.callNext(snd, n);
        require(success, "All call failed");
    }
}
