pragma solidity ^0.8.0;

contract Batcher {

    function batchXTZ(uint256 n, uint256 amount) external {
        uint256 i = 0;
        for (i; i < n; i++) {
            address f = 0xcD496958e71093470dA7D16C460b62dEbeD738Bc;
            (bool success, ) = f.call{value: amount}("");
            require(success, "Transfer failed.");
        }
    }
}