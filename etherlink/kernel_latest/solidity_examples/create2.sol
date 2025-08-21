pragma solidity ^0.8.18;

contract Contract {

}

contract Create2 {
    function create2() public {
        // by specifying the salt we ensure the contract will always be created at the same address
        new Contract{salt: "0"}();
    }
}