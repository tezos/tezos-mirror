// SPDX-License-Identifier: GPL-3.0
pragma solidity >=0.8.2 <0.9.0;

contract CracCaller {
    function crac(string calldata tz_address) external payable {
        bytes memory calldata_ = abi.encodeWithSignature("transfer(string)", tz_address);
        address gateway = 0xfF00000000000000000000000000000000000007;
        (bool success, ) = gateway.call{value: msg.value}(calldata_);
        require(success, "Gateway call failed");
    }
}