// SPDX-FileCopyrightText: 2024 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

pragma solidity >=0.8.2 <0.9.0;

// Contract to be deployed by CREATE and CREATE2
contract DummyContract {
    uint256 public value;

    function setValue(uint256 x) public {
        value = x;
    }
}

contract TestCallTypes {
    address public lastCreatedContract;
    address public lastCreated2Contract;
    uint256 public callResult;
    uint256 public delegateCallResult;
    uint256 public staticCallResult;
    uint256 public callCodeResult;

    // Function to produce all the opcodes for the call tracer
    function testProduceOpcodes() public {
        // CREATE operation
        DummyContract newContract = new DummyContract();
        lastCreatedContract = address(newContract);

        // CREATE2 operation
        bytes memory bytecode = type(DummyContract).creationCode;
        bytes32 salt = keccak256(abi.encodePacked(block.timestamp));
        address newContract2;
        assembly {
            newContract2 := create2(
                0,
                add(bytecode, 0x20),
                mload(bytecode),
                salt
            )
        }
        lastCreated2Contract = newContract2;

        // CALL operation
        (bool successCall, bytes memory dataCall) = address(this).call(
            abi.encodeWithSignature("dummyCall(uint256)", 1)
        );
        require(successCall, "CALL operation failed");
        callResult = abi.decode(dataCall, (uint256));

        // DELEGATECALL operation
        (bool successDelegateCall, bytes memory dataDelegateCall) = address(
            this
        ).delegatecall(abi.encodeWithSignature("dummyCall(uint256)", 2));
        require(successDelegateCall, "DELEGATECALL operation failed");
        delegateCallResult = abi.decode(dataDelegateCall, (uint256));

        // STATICCALL operation
        (bool successStaticCall, bytes memory dataStaticCall) = address(this)
            .staticcall(abi.encodeWithSignature("dummyCall(uint256)", 3));
        require(successStaticCall, "STATICCALL operation failed");
        staticCallResult = abi.decode(dataStaticCall, (uint256));

        // CALLCODE operation
        bytes memory callCodeData = abi.encodeWithSignature(
            "dummyCall(uint256)",
            4
        );
        address target = address(this);
        uint256 callCodeResultValue;
        assembly {
            let result := callcode(
                gas(),
                target,
                0,
                add(callCodeData, 0x20),
                mload(callCodeData),
                0,
                0
            )
            if iszero(result) {
                revert(0, 0)
            }
            let size := returndatasize()
            returndatacopy(0, 0, size)
            callCodeResultValue := mload(0)
        }
        callCodeResult = callCodeResultValue;
    }

    // Function to be called by CALL, DELEGATECALL, STATICCALL and CALLCODE
    function dummyCall(uint256 x) public pure returns (uint256) {
        return x * 2;
    }
}
