// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

// EVM-to-EVM bridge using the generic call(url, headers, body, method)
// precompile. URL targets http://ethereum/<destination>, exercising the
// same-runtime CRAC path through the gateway protocol.
//
// run()      - calls run() on the EVM destination via the precompile.
//              Reverts on failure.
// runCatch() - same call wrapped in try/catch, to verify revert/OOG can
//              be observed by the EVM caller.

contract CracHttpCallEvm {
    address constant GATEWAY = 0xfF00000000000000000000000000000000000007;
    uint256 public count;
    uint256 public catches;
    string public destination;

    function initialize(string calldata _destination) external {
        destination = _destination;
    }

    struct Header {
        string name;
        string value;
    }

    function _buildCalldata() internal view returns (bytes memory) {
        bytes memory url = abi.encodePacked(
            "http://ethereum/", destination
        );
        Header[] memory emptyHeaders = new Header[](0);
        bytes memory body = abi.encodeWithSignature("run()");
        return abi.encodeWithSignature(
            "call(string,(string,string)[],bytes,uint8)",
            string(url),
            emptyHeaders,
            body,
            uint8(1)
        );
    }

    function run() external payable {
        count++;
        (bool ok, ) = GATEWAY.call{value: msg.value}(_buildCalldata());
        if (!ok) {
            revert("CRAC call reverted");
        }
        count++;
    }

    function _doCall() external payable {
        (bool ok, ) = GATEWAY.call{value: msg.value}(_buildCalldata());
        require(ok, "CRAC call failed");
    }

    function runCatch() external payable {
        count++;
        try this._doCall{value: msg.value}() {
            count++;
        } catch {
            catches++;
        }
        count++;
    }
}
