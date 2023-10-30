// SPDX-License-Identifier: MIT
pragma solidity ^0.8.18;

contract Callee {
    uint public x = 0;
    uint public value;

    function setX(uint _x) public returns (uint) {
        x = _x;
        return x;
    }

    function setXandSendEther(uint _x) public payable returns (uint, uint) {
        x = _x;
        value = msg.value;

        return (x, value);
    }

    function getX() public view returns (uint) {
        return x;
    }
}


contract Caller {
    function setX(Callee _callee, uint _x) public {
        _callee.setX(_x);
    }

    function setXFromAddress(address _addr, uint _x) public {
        Callee callee = Callee(_addr);
        callee.setX(_x);
    }

    function setXandSendEther(Callee _callee, uint _x) public payable {
        _callee.setXandSendEther{value: msg.value}(_x);
    }

    function getX(Callee _callee) public view returns (uint) {
        return _callee.getX();
    }
}