import time
from tools import constants, paths, utils
from launchers.sandbox import Sandbox


def scenario():
    """a private tezos network, initialized with network parameters
    and some accounts."""
    with Sandbox(paths.TEZOS_HOME, constants.IDENTITIES) as sandbox:
        # Launch node running protocol alpha
        sandbox.add_node(0, params=constants.NODE_PARAMS)
        utils.activate_alpha(sandbox.client(0))
        # Launch a second node on the same private tezos network
        sandbox.add_node(1, params=constants.NODE_PARAMS)
        # Launch a baker associated to node 0, baking on behalf of delegate
        # bootstrap5
        sandbox.add_baker(
            0,
            ['bootstrap5'],
            proto=constants.ALPHA_DAEMON,
            run_params=['--liquidity-baking-toggle-vote', 'pass'],
        )
        # Wait for second node to update its protocol to alpha, if not
        # it may not know yet the `wait_for_inclusion` operation which is
        # protocol specific
        time.sleep(20)
        # first client tells node 0 to transfer money for an account to another
        # receipt is an object representing the client answer
        receipt = sandbox.client(0).transfer(500, 'bootstrap1', 'bootstrap3')
        transfer_hash = receipt.operation_hash
        # second client waits for inclusion of operation by the second node
        sandbox.client(1).wait_for_inclusion(transfer_hash, check_previous=2)


if __name__ == "__main__":
    scenario()
