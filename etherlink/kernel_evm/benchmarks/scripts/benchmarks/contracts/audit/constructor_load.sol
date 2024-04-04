// SPDX-License-Identifier: GPL-3.0

pragma solidity ^0.8.17;

/**
 * @title
 * @dev
 */
contract Constructor {

    constructor () {
        bytes memory message = bytes("do");
        bytes32 messageHash;

        for (uint256 i; i<1_000;i++) {
                messageHash = keccak256(bytes.concat(message, abi.encode(i)));
            }
    }
}
