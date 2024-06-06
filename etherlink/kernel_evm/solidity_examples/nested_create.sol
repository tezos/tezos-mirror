pragma solidity ^0.8.20;

contract Created {
    int value;

    constructor(int x) {
        value = x;
    }
}

contract Creator {
    function create(int x) public returns (address) {
        address created = address(new Created(x));
        return created;
    }

    function create2(int x, bytes32 _salt) public returns (address) {
        address created = address((new Created){salt: _salt}(x));
        return created;
    }
}