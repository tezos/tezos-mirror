pragma solidity ^0.8.0;

contract CallPrecompile {
    function testCall() public payable {
        address to = 0xff00000000000000000000000000000000000001;
        to.call{value: msg.value}(abi.encodeWithSignature("withdraw_base58(string)", "tz1WrbkDrzKVqcGXkjw4Qk4fXkjXpAJuNP1j"));

    }

    function testDelegatecall() public payable {
        address to = 0xff00000000000000000000000000000000000001;
        to.delegatecall(abi.encodeWithSignature("withdraw_base58(string)", "tz1WrbkDrzKVqcGXkjw4Qk4fXkjXpAJuNP1j"));

    }

    function testStaticcall() public payable {
        address to = 0xff00000000000000000000000000000000000001;
        to.staticcall(abi.encodeWithSignature("withdraw_base58(string)", "tz1WrbkDrzKVqcGXkjw4Qk4fXkjXpAJuNP1j"));
    }

}