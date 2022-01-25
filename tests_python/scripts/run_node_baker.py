#!/usr/bin/env python3
import time
import argparse
import os.path
from tools import constants, paths, utils
from launchers.sandbox import Sandbox


def scenario(contract, storage, round_duration, proto):
    if proto is None:
        proto = 'alpha'
    assert proto in {'alpha'}, 'unknown protocol'
    protos = {
        'alpha': (constants.ALPHA, constants.ALPHA_DAEMON),
    }
    proto_hash, proto_daemon = protos[proto]
    if contract:
        assert os.path.isfile(contract), f'{contract} is not a file'
    if storage is None:
        storage = 'unit'
    with Sandbox(paths.TEZOS_HOME, constants.IDENTITIES) as sandbox:
        parameters = dict(constants.ALPHA_PARAMETERS)
        parameters['minimal_block_delay'] = str(round_duration)
        parameters['delay_increment_per_round'] = str(round_duration)
        sandbox.add_node(1, params=constants.NODE_PARAMS)
        utils.activate_protocol(sandbox.client(1), proto_hash, parameters)
        accounts = [f'bootstrap{i}' for i in range(1, 6)]
        sandbox.add_baker(
            1,
            accounts,
            proto=proto_daemon,
            run_params=['--liquidity-baking-escape-vote', 'pass'],
        )
        client = sandbox.client(1)
        if contract:
            args = ['--init', storage, '--burn-cap', '10.0']
            sender = 'bootstrap2'
            amount = 0
            client.originate(
                'my_contract', sender, amount, sender, contract, args
            )
        while 1:
            client.get_head()
            time.sleep(round_duration)


DESCRIPTION = '''
Utility script to run node/baker in sandbox mode (RPC port 18731).

The script launches client commands to import the bootstrap keys, it optionally
originates a contract on behalf of bootstrap2. Then it displays the chain head
every time_between_blocks seconds.
'''


def main():
    description = DESCRIPTION
    parser = argparse.ArgumentParser(description=description)

    parser.add_argument(
        '--time-between-blocks',
        dest='time_between_blocks',
        metavar='TIME',
        help='time between blocks (seconds), default=2',
        required=False,
        default='2',
    )
    parser.add_argument(
        '--contract',
        dest='contract',
        metavar='CONTRACT',
        help='path to the contract',
        required=False,
    )
    parser.add_argument(
        '--storage',
        dest='storage',
        metavar='STORAGE',
        help='initial storage for contract',
        required=False,
    )
    parser.add_argument(
        '--proto',
        dest='proto',
        metavar='PROTO',
        help='alpha (default alpha)',
        required=False,
    )
    args = parser.parse_args()
    scenario(
        args.contract, args.storage, int(args.time_between_blocks), args.proto
    )


if __name__ == "__main__":
    main()
