pragma solidity ^0.8.20;

contract OOGCall {
    function sendViaCall(address payable _to) public payable {
        (bool sent, bytes memory data) = _to.call{gas: 1, value: msg.value}("");
        (bool sent_0, bytes memory data_0) = _to.call{value: msg.value}("");
    }
}
