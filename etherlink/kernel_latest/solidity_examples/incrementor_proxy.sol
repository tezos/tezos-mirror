// SPDX-License-Identifier: MIT
pragma solidity >=0.8.21;

contract Incrementor {
    uint256 total_calls = 0;

    function increment() public  {
        total_calls++;
    }

    function getCalls() public view returns (uint256) {
        return total_calls;
    }
}


contract Proxy {
    Incrementor public incrementor;

    constructor(address contract_address) {
        incrementor = Incrementor(contract_address);
    } 


    function deposit(address receiver, uint256 amount, uint256 ticketHash)
        public
    {
        for(uint i=0; i<amount; i++){
            incrementor.increment();
        }
    }
}
