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

    function callNext(address next) public returns (bool, bytes memory) {
        return next.call(abi.encodeWithSignature("call()"));
    }
}

contract SecondContract {
    address stop;

    constructor(address _stop) {
        stop = _stop;
    }

    function call() public returns (bool, bytes memory) {
        address callee = msg.sender;
        (bool success, ) = callee.call(
            abi.encodeWithSignature("callNext(address)", stop)
        );
        require(success, "Second Contract Call failed");
        return callee.call(abi.encodeWithSignature("callNext(address)", stop));
    }
}

contract TestDepthCall {
    function startDepth() public {
        // Simple stop contract to avoid looping.
        address stop = address(new Stop());
        // FirstContract takes stop in arguments to verify call in create
        FirstContract first = new FirstContract(stop);
        // The second contract will call stop and re-call first contract with
        // stop in parameter.
        address snd = address(new SecondContract(stop));
        // We call firstContract with second in argument to have a depth 2 call
        (bool success, ) = first.callNext(snd);
        require(success, "All call failed");
    }
}
