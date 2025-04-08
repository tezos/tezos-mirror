pragma solidity ^0.8.17;

contract SelfDestructExample {

    receive() external payable {}

    function close() public {
        address payable addr = payable(address(msg.sender));
        selfdestruct(addr);
    }
} 
