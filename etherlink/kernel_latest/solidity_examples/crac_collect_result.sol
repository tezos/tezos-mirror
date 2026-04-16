// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

// Thin EVM wrapper around the generic call() precompile used to verify
// that %collect_result bytes deposited during a Michelson CRAC surface
// as the HTTP response body observable by the EVM caller.
//
// run() calls the Michelson destination's %default entrypoint with an
// empty body (Unit parameter) and stores the returned bytes.
contract CracCollectResult {
    address constant GATEWAY = 0xfF00000000000000000000000000000000000007;
    bytes public result;
    string public destination;

    struct Header {
        string name;
        string value;
    }

    function initialize(string calldata _destination) external {
        destination = _destination;
    }

    function run() external {
        bytes memory url = abi.encodePacked(
            "http://tezos/", destination, "/default"
        );
        Header[] memory emptyHeaders = new Header[](0);
        bytes memory cd = abi.encodeWithSignature(
            "call(string,(string,string)[],bytes,uint8)",
            string(url),
            emptyHeaders,
            hex"",
            uint8(1)
        );
        (bool ok, bytes memory ret) = GATEWAY.call(cd);
        require(ok, "CRAC call failed");
        result = abi.decode(ret, (bytes));
    }
}
