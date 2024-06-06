// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

contract Timestamp {
  uint public timestamp;

  function getTimestamp() public view returns (uint) {
    return block.timestamp;
  }

  function getSavedTimestamp() public view returns (uint) {
    return timestamp;
  }

  function setTimestamp() public {
      timestamp = block.timestamp;
  }
}
