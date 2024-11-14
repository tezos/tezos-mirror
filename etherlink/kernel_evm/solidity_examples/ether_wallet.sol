pragma solidity ^0.8.20;

contract SharedEtherWallet {
     mapping (address => uint) balance;

    receive() external payable {
        balance[msg.sender] += msg.value;
    }

    function withdraw(uint amount) external {
        require(balance[msg.sender] >= amount, "Not enough balance");
        balance[msg.sender] -= amount;
        payable(msg.sender).transfer(amount);
    }

    function getBalance() external view returns (uint) {
        return balance[msg.sender];
    }
}
