// SPDX-License-Identifier: MIT
pragma solidity ^0.8.24;

contract TransientStorageMultiplier {
    uint256 public value = 0;

    function multiply(uint inputValue, uint multiplier) public {
        assembly {
            tstore(0, inputValue)
        }
        layer2(multiplier);
    }

    function layer2(uint multiplier) public {
        assembly {
            let v := tload(0)
            sstore(value.slot, mul(v, multiplier))
        }
    }

    function get_output() public view returns (uint256) {
        return value;
    }
}
