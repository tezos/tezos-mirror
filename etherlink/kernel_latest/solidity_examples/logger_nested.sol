// SPDX-FileCopyrightText: 2026 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

pragma solidity >=0.8.2 <0.9.0;

// Emits a log from a nested call frame.
contract LoggerB {
    event LogFromB(uint256 indexed value);

    function logValue(uint256 value) public {
        emit LogFromB(value);
    }
}

// Emits a log, calls LoggerB (which emits its own log and returns), then emits
// another log. Used to check that the callTracer attributes each log to the
// call frame that actually emitted it.
contract LoggerA {
    event LogFromA(uint256 indexed value);

    LoggerB public b;

    constructor() {
        b = new LoggerB();
    }

    function run(uint256 first, uint256 second, uint256 third) public {
        emit LogFromA(first);
        b.logValue(second);
        emit LogFromA(third);
    }
}
