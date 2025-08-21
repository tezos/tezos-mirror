// SPDX-FileCopyrightText: 2024 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

pragma solidity >=0.8.2 <0.9.0;

contract SimpleContract {
    uint256 x;
    function setX(uint256 _x) public {
        x = _x;
    }
}

contract ErrorContract {
    function simpleRevert() public pure {
        revert("Expected message for explicit revert");
    }

    function outOfGas(address simple) public {
        (bool success, ) = simple.call{gas: 2000}(
            abi.encodeWithSignature("setX(uint256)", 5)
        );
        require(!success, "Call succeeded despite very small gas limit");
    }

    function invalidOpcode() public pure {
        assembly {
            invalid()
        }
    }

    function requireFailed() public pure {
        require(false, "Expected message for explicit assert false");
    }

    function startTest() public {
        (bool successsr, ) = address(this).call(
            abi.encodeWithSignature("simpleRevert()")
        );
        require(!successsr, "Failure expected after an explicit revert");
        SimpleContract s = new SimpleContract();
        (bool successoog, ) = address(this).call(
            abi.encodeWithSignature("outOfGas(address)", address(s))
        );
        require(successoog, "Success expected if the call ran out of gas");
        (bool successio, ) = address(this).call{gas:2000}(
            abi.encodeWithSignature("invalidOpcode()")
        );
        require(!successio, "Failure expected after the invalid opcode");
        (bool successreq, ) = address(this).call(
            abi.encodeWithSignature("requireFailed()")
        );
        require(!successreq, "Failure expected if the calls fails a require");
    }
}
