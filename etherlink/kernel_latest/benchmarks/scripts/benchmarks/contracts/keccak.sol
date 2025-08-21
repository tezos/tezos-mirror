// SPDX-FileCopyrightText: 2018 Tasuku Nakamura <contact@solidity-by-example.org>
//
// SPDX-License-Identifier: MIT

pragma solidity ^0.8.17;

contract GuessTheMagicWord {
    bytes32 public answer =
        0x60298f78cc0b47170ba79c10aa3851d7648bd96f2f8e46a19dbc777c36fb0c00;

    // Magic word is "Solidity"
    function guess(string memory _word) public view returns (bool) {
        return keccak256(abi.encodePacked(_word)) == answer;
    }
}