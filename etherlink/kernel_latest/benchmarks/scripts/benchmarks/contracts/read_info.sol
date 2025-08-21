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

    function gas_limit() public view returns (uint) {
        return block.gaslimit;
    }

    function chain_id() public view returns (uint) {
        return block.chainid;
    }

    function block_number() public view returns (uint) {
        return block.number;
    }

    function balance() public view returns (uint) {
        return address(msg.sender).balance;
    }

    function base_fee() public view returns (uint) {
        return block.basefee;
    }

    function extcodehash() public view returns (bytes32) {
        return keccak256(address(this).code);
    }

    function msize() public pure returns (uint){
        assembly {
            mstore(0x0, msize()) 
            return(0x0, 0x20)
        }
    }

    function get_code() public view returns (bytes memory) {
        return address(this).code;
    } 
}
