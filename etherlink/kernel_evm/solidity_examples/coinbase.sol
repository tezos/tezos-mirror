pragma solidity >=0.8.2 <0.9.0;

contract Coinbase {
    address private coinbase;

    constructor() {
        setStorageCoinbase();
    }

    function setStorageCoinbase() private {
        coinbase = block.coinbase;
    }

    function getStorageCoinbase() public view returns (address){
        return coinbase;
    }

    function getCurrentCoinbase() public view returns (address){
        return block.coinbase;
    }
}