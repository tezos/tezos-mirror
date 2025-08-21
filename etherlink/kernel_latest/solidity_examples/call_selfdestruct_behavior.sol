pragma solidity >=0.8.28 <0.9.0;

// Contract used to test selfdestruct behavior before and after EIP-6780

contract Parent {
    address instant_selfdestruct_addr; // address where the SELFDESTRUCT will be in the same transaction as a creation
    Child selfdestruct_in_separate_tx;

    constructor() {
        Child instant_selfdestruct = new Child();
        instant_selfdestruct.destruct();
        instant_selfdestruct_addr = address(instant_selfdestruct);

        selfdestruct_in_separate_tx = new Child();
    }

    function destruct_child_in_separate_tx() public {
        selfdestruct_in_separate_tx.destruct();
    }

    function codesize(address selfdestructed_addr) private view returns (uint) {
        uint256 code_size;
        assembly {
            code_size := extcodesize(selfdestructed_addr)
        }
        return code_size;
    }

    function codesize_after_delete_in_separate_tx() public view returns (uint) {
        return codesize(address(selfdestruct_in_separate_tx));
    }

    function codesize_after_delete_in_same_tx() public view returns (uint) {
        return codesize(instant_selfdestruct_addr);
    }
}

contract Child {
    function destruct() public {
        address payable addr = payable(address(this));
        selfdestruct(addr);
    }
}
