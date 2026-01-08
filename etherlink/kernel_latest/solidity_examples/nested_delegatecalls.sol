// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

contract D {
    event Context(address msgSender, address thisAddress);

    function logContext() public {
        emit Context(msg.sender, address(this));
    }
}

contract C {
    function callD(address dAddr) public {
        (bool success, ) = dAddr.call(
            abi.encodeWithSignature("logContext()")
        );
        require(success, "call to D failed");
    }
}

contract B {
    function callC(address cAddr, address dAddr) public {
        (bool success, ) = cAddr.call(
            abi.encodeWithSignature("callD(address)", dAddr)
        );
        require(success, "call to C failed");
    }
}

contract A {
    function callB(address bAddr, address cAddr, address dAddr) public {
        (bool success, ) = bAddr.delegatecall(
            abi.encodeWithSignature("callC(address,address)", cAddr, dAddr)
        );
        require(success, "delegatecall to B failed");
    }
}

