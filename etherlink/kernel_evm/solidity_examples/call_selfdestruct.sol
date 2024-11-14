pragma solidity >=0.8.2 <0.9.0;

// Contract used to test that contract that selfdestruct can still be called in the same transaction

contract C1 {

    uint n;
    address addr;

    constructor() {
        C2 c2 = new C2();
        c2.destruct();
        n = c2.f();
        addr = address(c2);
    }

    function codesize() public view returns (uint){
        address c2 = addr;
        uint256 codeSize;
        assembly {
            codeSize := extcodesize(c2)
        }
        return codeSize;
    }

    function get_n() public view returns (uint) {
        return n;
    }
}

contract C2 {

    function f() public pure returns (uint){
        return 1234567890;
    }

    function destruct() public {
        address payable addr = payable (address(this));
        selfdestruct(addr);
    }
}