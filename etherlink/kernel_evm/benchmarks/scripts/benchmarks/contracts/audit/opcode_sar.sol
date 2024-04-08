// SPDX-License-Identifier: MIT
pragma solidity ^0.8.17;

contract sar_opcode {

    function run(uint256 shifts) public pure returns (uint256)  {
        uint256 variable = 1;
        uint256 result = 0;
        assembly {
            result := sar(shifts, variable)
        }
        return result;
    }
}
