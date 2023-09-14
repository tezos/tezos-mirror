pragma solidity ^0.8.17;

contract ReadInfo {

    function timestamp() public view returns (uint) {
        return block.timestamp;
    }

    function gas_price() public view returns (uint) {
        return tx.gasprice;
    }

    function coin_base() public view returns (address) {
        return block.coinbase;
    }

    function origin() public view returns (address) {
        return tx.origin;
    }

}
