// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

// EVM-to-Michelson bridge using the generic call(url, headers, body, method)
// precompile entrypoint. Used to test the HTTP response classification model:
// 2xx: return body, 4xx: catchable revert, 5xx: block abort.
//
// run()      - calls %run on the Tez destination via call(). Reverts on failure.
// runCatch() - same call wrapped in try/catch to verify 4xx is catchable.

contract CracHttpCall {
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
            "http://tezos/", destination, "/run"
        );
        Header[] memory emptyHeaders = new Header[](0);
        return abi.encodeWithSignature(
            "call(string,(string,string)[],bytes,uint8)",
            string(url),
            emptyHeaders,
            hex"030b",
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

    function _doCallWithGas(uint256 gasLimit) external payable {
        (bool ok, ) = GATEWAY.call{value: msg.value, gas: gasLimit}(_buildCalldata());
        require(ok, "CRAC call failed");
    }

    function runCatchWithGasLimit(uint256 gasLimit) external payable {
        count++;
        try this._doCallWithGas{value: msg.value}(gasLimit) {
            count++;
        } catch {
            catches++;
        }
        count++;
    }
}
