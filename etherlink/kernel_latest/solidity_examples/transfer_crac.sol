// SPDX-License-Identifier: GPL-3.0
pragma solidity >=0.8.2 <0.9.0;

contract CracCaller {
    struct Header {
        string name;
        string value;
    }

    function crac(string calldata tz_address) external payable {
        // Forward the value to the Tezos address through the gateway's
        // generic `call` entrypoint: a POST to http://tezos/<address>
        // with an empty body.
        bytes memory calldata_ = abi.encodeWithSignature(
            "call(string,(string,string)[],bytes,uint8)",
            string.concat("http://tezos/", tz_address),
            new Header[](0),
            bytes(""),
            uint8(1)
        );
        address gateway = 0xfF00000000000000000000000000000000000007;
        (bool success, ) = gateway.call{value: msg.value}(calldata_);
        require(success, "Gateway call failed");
    }
}