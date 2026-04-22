// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

// Thin EVM wrapper around the generic call() precompile used to verify
// that %collect_result bytes deposited during a Michelson CRAC surface
// as the HTTP response body observable by the EVM caller.
//
// run()      - calls the Michelson destination and stores the returned
//              bytes.  Reverts if the CRAC call itself reverts.
// runCatch() - same call wrapped in try/catch so the outer tx can
//              inspect post-revert state (to check no bytes leaked).
contract CracCollectResult {
    address constant GATEWAY = 0xfF00000000000000000000000000000000000007;
    bytes public result;
    string public destination;
    bool public caught;

    struct Header {
        string name;
        string value;
    }

    function initialize(string calldata _destination) external {
        destination = _destination;
    }

    function _buildCalldata() internal view returns (bytes memory) {
        bytes memory url = abi.encodePacked(
            "http://tezos/", destination, "/default"
        );
        Header[] memory emptyHeaders = new Header[](0);
        return abi.encodeWithSignature(
            "call(string,(string,string)[],bytes,uint8)",
            string(url),
            emptyHeaders,
            hex"",
            uint8(1)
        );
    }

    function _doRun() external {
        require(msg.sender == address(this), "only self");
        (bool ok, bytes memory ret) = GATEWAY.call(_buildCalldata());
        if (!ok) {
            // Propagate the precompile's revert reason verbatim so the
            // outer catch sees the real error rather than a generic
            // message.
            assembly {
                revert(add(ret, 0x20), mload(ret))
            }
        }
        result = abi.decode(ret, (bytes));
    }

    function run() external {
        this._doRun();
    }

    function runCatch() external {
        try this._doRun() {
            // Success: result was set by _doRun.
        } catch {
            caught = true;
        }
    }
}
