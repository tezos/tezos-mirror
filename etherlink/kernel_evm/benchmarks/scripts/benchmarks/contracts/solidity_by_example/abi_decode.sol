// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

contract AbiDecode {
    struct MyStruct {
        string name;
        uint[2] nums;
    }

    function encode(
        uint x,
        address addr,
        uint[] calldata arr,
        MyStruct calldata myStruct
    ) external pure returns (bytes memory) {
        return abi.encode(x, addr, arr, myStruct);
    }

    function decode(
        bytes calldata data
    )
        external
        pure
        returns (
            uint x,
            address addr,
            uint[] memory arr,
            MyStruct memory myStruct
        )
    {
        // (uint x, address addr, uint[] memory arr, MyStruct myStruct) = ...
        (x, addr, arr, myStruct) = abi.decode(
            data,
            (uint, address, uint[], MyStruct)
        );
    }

    function test() public {
        uint[] memory arr = new uint[](2);

        arr[0] = 0;
        arr[1] = 1;

        MyStruct memory myStruct;
        myStruct.name = "name";
        this.decode(this.encode(0, address(0), arr, myStruct));
    }
}
