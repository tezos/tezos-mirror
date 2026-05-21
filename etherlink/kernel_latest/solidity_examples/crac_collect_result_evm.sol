// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

// EVM-to-EVM bridge that captures the response body of a same-runtime
// CRAC.  Mirrors crac_collect_result.sol but routes through
// http://ethereum/<destination> and invokes store(uint256) on the
// target so the EVM serve produces a non-empty Output::Call body.
//
// run() decodes the precompile's return value (ABI-encoded bytes)
// and stores it in [result].
contract CracCollectResultEvm {
    address constant GATEWAY = 0xfF00000000000000000000000000000000000007;
    bytes public result;
    string public destination;
    uint256 public storedValue;

    struct Header {
        string name;
        string value;
    }

    function initialize(string calldata _destination, uint256 _value) external {
        destination = _destination;
        storedValue = _value;
    }

    function _buildCalldata() internal view returns (bytes memory) {
        bytes memory url = abi.encodePacked(
            "http://ethereum/", destination
        );
        Header[] memory emptyHeaders = new Header[](0);
        bytes memory body = abi.encodeWithSignature(
            "store(uint256)", storedValue
        );
        return abi.encodeWithSignature(
            "call(string,(string,string)[],bytes,uint8)",
            string(url),
            emptyHeaders,
            body,
            uint8(1)
        );
    }

    function _doRun() external {
        require(msg.sender == address(this), "only self");
        (bool ok, bytes memory ret) = GATEWAY.call(_buildCalldata());
        if (!ok) {
            assembly {
                revert(add(ret, 0x20), mload(ret))
            }
        }
        result = abi.decode(ret, (bytes));
    }

    function run() external {
        this._doRun();
    }
}
