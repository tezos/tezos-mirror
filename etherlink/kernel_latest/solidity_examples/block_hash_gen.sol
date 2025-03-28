pragma solidity >=0.8.2 <0.9.0;

contract BlockHashGen {

    uint256 private number;

    constructor() {
        generateFromBlockHash();
    }

    function generateFromBlockHash() private {
        bytes32 blockH = blockhash(block.number - 1);
        number = uint256(keccak256(abi.encodePacked(blockH, block.timestamp))) % 11;
    }
  
}