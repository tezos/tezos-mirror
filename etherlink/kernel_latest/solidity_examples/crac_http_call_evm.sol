// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

// EVM-to-EVM bridge using the generic call(url, headers, body, method)
// precompile. URL targets http://ethereum/<destination>, i.e. a
// same-runtime NAC, which the gateway refuses. Used to pin that refusal:
// the gateway CALL returns false and the caller's try/catch observes it.
//
// run()         - calls run() on the EVM destination via the precompile
//                 (POST). Reverts on failure.
// runCatch()    - same call wrapped in try/catch, so the refusal (or a
//                 revert/OOG) can be observed by the EVM caller.
// runCatchGet() - same, with method = GET. The gateway derives the target
//                 runtime from the URL host before splitting on the
//                 method, so GET must be refused just like POST.

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

    function _buildCalldata(uint8 method) internal view returns (bytes memory) {
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
            method
        );
    }

    function run() external payable {
        count++;
        (bool ok, ) = GATEWAY.call{value: msg.value}(_buildCalldata(1));
        if (!ok) {
            revert("cross-runtime call reverted");
        }
        count++;
    }

    function _doCall() external payable {
        (bool ok, ) = GATEWAY.call{value: msg.value}(_buildCalldata(1));
        require(ok, "cross-runtime call failed");
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

    // GET carries no value: the gateway rejects a value-bearing GET, and
    // this path must fail on the same-runtime target instead.
    function _doCallGet() external {
        (bool ok, ) = GATEWAY.call(_buildCalldata(0));
        require(ok, "cross-runtime call failed");
    }

    function runCatchGet() external {
        count++;
        try this._doCallGet() {
            count++;
        } catch {
            catches++;
        }
        count++;
    }
}
