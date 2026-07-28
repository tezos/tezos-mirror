// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

// EVM contract that reads a Michelson on-chain VIEW through the CRAC gateway
// using STATICCALL.
//
// `readView()` invokes `callMichelsonView(destination, viewName, input)` on the
// gateway precompile via STATICCALL, a read-only crossing that is
// STATICCALL-safe (no value transfer, no log emission, no durable write). The
// gateway returns the view's Micheline response ABI-encoded as `(bytes)`.
//
// Used to assert the gateway STATICCALL frame in callTracer traces
// (RFC Example B, `callMichelsonView` variant): the gateway child frame is
// recorded with `type = STATICCALL` and `output` = the ABI-encoded `(bytes)`
// response, unlike the state-mutating `callMichelson` entrypoint which discards
// the body and yields an empty output.
contract CracMichelsonViewStaticcall {
    address constant GATEWAY = 0xfF00000000000000000000000000000000000007;
    string public destination;
    string public viewName;
    bytes public lastResponse;

    error CracMichelsonViewFailed();

    function initialize(string calldata _destination, string calldata _viewName)
        external
    {
        destination = _destination;
        viewName = _viewName;
    }

    function readView() external {
        // STATICCALL the gateway (read-only crossing). `input` is the Micheline
        // encoding of Unit (0x030b), matching the view's `unit` parameter.
        (bool success, bytes memory ret) = GATEWAY.staticcall(
            abi.encodeWithSignature(
                "callMichelsonView(string,string,bytes)",
                destination,
                viewName,
                hex"030b"
            )
        );
        if (!success) {
            revert CracMichelsonViewFailed();
        }
        // The gateway returns the view's response ABI-encoded as (bytes).
        lastResponse = abi.decode(ret, (bytes));
    }
}
