import copy
import itertools
import random
import time
import subprocess
from datetime import datetime
from typing import List, Optional, Tuple, Dict, Iterable, Any

import pytest
from tools import utils, constants
from launchers.sandbox import Sandbox
from client.client import Client
from . import protocol

# This test runs NUM_NODES, and 3 bakers. It runs a number of test cycles
# (not to be confused for protocol cycle) where each cycle lasts
# TIME_BETWEEN_CYCLE seconds, for TEST_DURATION seconds.
# At each cycle, a random transaction is injected. Every CHECK_PROGRESS
# cycles, a client checks that the chain is progressing.
# It does so by polling the chain (and checking that the level is increasing)
# at most MAX_RETRY times, with a timeout of TIMEOUT seconds
# At the end of the test, checks that the chain has at least
# EXPECTED_LEVEL blocks

random.seed(42)
KEYS = [f'bootstrap{i}' for i in range(1, 6)]
NEXT_KEY = itertools.cycle(KEYS)
NUM_NODES = 5
TIME_BETWEEN_CYCLE = 2
CHECK_PROGRESS = 10
KILL_BAKER = 4
TIMEOUT = 6
MAX_RETRY = 6
TEST_DURATION = 120  # duration of the main loop
MINIMAL_BLOCK_DELAY = 1
DELAY_INCREMENT_PER_ROUND = 1
MAX_LEVEL_DURATION = 6  # that is, decision expected in at most 3 rounds
EXPECTED_LEVEL = TEST_DURATION // MAX_LEVEL_DURATION


def random_op(client: Client) -> None:
    sender = next(NEXT_KEY)
    dest = random.choice([key for key in KEYS if key != sender])
    amount = random.randrange(10) + 1
    client.run(['transfer', str(amount), 'from', sender, 'to', dest])


def setup_parameters() -> Dict[str, Any]:
    proto_params = dict(protocol.TENDERBAKE_PARAMETERS)
    parameters = copy.deepcopy(proto_params)
    parameters['minimal_block_delay'] = str(MINIMAL_BLOCK_DELAY)
    parameters['delay_increment_per_round'] = str(DELAY_INCREMENT_PER_ROUND)
    parameters['consensus_threshold'] = (
        2 * (parameters['consensus_threshold'] // 3) + 1
    )
    return parameters


def add_nodes(
    sandbox: Sandbox,
    node_peers_assoc: Iterable[Tuple[int, Optional[List[int]]]],
) -> None:
    for node_id, peers in node_peers_assoc:
        sandbox.add_node(
            node_id,
            params=constants.NODE_PARAMS,
            log_levels=constants.TENDERBAKE_NODE_LOG_LEVELS,
            peers=peers,
        )


def add_bakers(sandbox: Sandbox, nodes: Iterable[int]) -> None:
    for node_id in nodes:
        account = f'bootstrap{node_id + 1}'
        sandbox.add_baker(
            node_id,
            [account],
            proto=protocol.DAEMON,
            log_levels=constants.TENDERBAKE_BAKER_LOG_LEVELS,
            run_params=['--liquidity-baking-toggle-vote', 'pass'],
        )


@pytest.mark.baker
@pytest.mark.multinode
@pytest.mark.slow
@pytest.mark.incremental
@pytest.mark.tenderbake
@pytest.mark.manual
class TestAllDaemonsWithOperations:
    """Runs three baker, generates random op, and
    add (or replace) new nodes dynamically. After a little while,
    we kill the bakers and check everyone synchronize to the same head."""

    def test_setup_network(self, sandbox: Sandbox):
        parameters = setup_parameters()
        add_nodes(sandbox, [(i, None) for i in range(NUM_NODES)])
        protocol.activate(sandbox.client(0), parameters=parameters)
        add_bakers(sandbox, range(NUM_NODES - 1))

    def test_wait_for_alpha(self, sandbox: Sandbox):
        clients = sandbox.all_clients()
        for client in clients:
            proto = protocol.HASH
            assert utils.check_protocol(client, proto)

    def test_network_gen_operations(self, sandbox: Sandbox, session):
        dead_baker = NUM_NODES - 1
        cur_time = datetime.now()
        cycle = 1
        while (datetime.now() - cur_time).total_seconds() < TEST_DURATION:
            i = random.randrange(NUM_NODES)
            client = sandbox.client(i)
            try:
                random_op(client)
            except subprocess.CalledProcessError:
                # some operations may be invalid, e.g. the client sends
                # several operation with the same counter
                print('# IGNORED INVALID OPERATION')

            # test chain progresses every X cycles
            if cycle % CHECK_PROGRESS == 0:
                client = sandbox.client(0)

                level_before = client.get_level()
                current_time = datetime.now().strftime("%H:%M:%S")
                print(current_time, "client level: ", level_before)

                retry = MAX_RETRY
                while client.get_level() == level_before and retry >= 1:
                    if retry != MAX_RETRY:
                        current_time = datetime.now().strftime("%H:%M:%S")
                        print(
                            current_time,
                            "level did not increase, retries: ",
                            retry,
                        )
                    time.sleep(TIMEOUT)
                    retry -= 1
                msg = f"chain level didn't increase for {MAX_RETRY*TIMEOUT}s"
                assert retry, msg

            # cyclically kill bakers
            if cycle % KILL_BAKER == 0:
                baker = (dead_baker + 1) % NUM_NODES
                current_time = datetime.now().strftime("%H:%M:%S")
                print(current_time, "killing baker on node ", baker)
                sandbox.rm_baker(baker, proto=protocol.DAEMON)
                time.sleep(1)
                # wake up the dead baker
                current_time = datetime.now().strftime("%H:%M:%S")
                print(current_time, "starting baker on node ", dead_baker)
                sandbox.add_baker(
                    dead_baker,
                    [f'bootstrap{dead_baker+1}'],
                    proto=protocol.DAEMON,
                    log_levels=constants.TENDERBAKE_BAKER_LOG_LEVELS,
                    run_params=['--liquidity-baking-toggle-vote', 'pass'],
                )
                # set the next baker to die
                dead_baker = baker
            time.sleep(TIME_BETWEEN_CYCLE)
            cycle += 1
        session['dead_baker'] = dead_baker

    def test_kill_baker(self, sandbox: Sandbox, session):
        for i in range(NUM_NODES):
            if i != session['dead_baker']:
                sandbox.rm_baker(i, proto=protocol.DAEMON)

    def test_synchronize(self, sandbox: Sandbox):
        utils.synchronize(sandbox.all_clients())

    def test_check_operations(self, sandbox: Sandbox):
        min_level = min(
            [client.get_level() for client in sandbox.all_clients()]
        )
        heads_hash = set()
        assert min_level >= EXPECTED_LEVEL
        # check there is exactly one head
        for client in sandbox.all_clients():
            block_hash = utils.get_block_hash(client, min_level)
            heads_hash.add(block_hash)
        assert len(heads_hash) == 1


@pytest.mark.baker
@pytest.mark.multinode
@pytest.mark.slow
@pytest.mark.incremental
@pytest.mark.tenderbake
@pytest.mark.manual
class TestAllDaemonsWithOperationsRingTopo(TestAllDaemonsWithOperations):
    """Runs three baker, generates random op, and
    add (or replace) new nodes dynamically. After a little while,
    we kill the bakers and check everyone synchronize to the same head.
    Use a ring topology instead of the default clique"""

    def test_setup_network(self, sandbox: Sandbox):
        parameters = setup_parameters()
        add_nodes(
            sandbox,
            [
                (i, [(i + 1) % NUM_NODES, (NUM_NODES + i - 1) % NUM_NODES])
                for i in range(NUM_NODES)
            ],
        )
        protocol.activate(sandbox.client(0), parameters=parameters)
        add_bakers(sandbox, range(NUM_NODES - 1))

    def test_check_topology(self, sandbox: Sandbox):
        for i in range(NUM_NODES):
            client = sandbox.client(i)
            res = client.p2p_stat()
            num_connected = 0
            for point in res.points.values():
                num_connected += point.is_connected
            assert num_connected == 2

    # The calls to super() below allow to implicitly specifiy the test sequence
    # by "pseudo-redefining" some inherited methods. Otherwise, there's no
    # guarantee regarding when test_check_topology will be executed.
    #
    # pylint: disable=W0235
    def test_wait_for_alpha(self, sandbox: Sandbox):
        super().test_wait_for_alpha(sandbox)

    def test_network_gen_operations(self, sandbox: Sandbox, session):
        super().test_network_gen_operations(sandbox, session)

    def test_kill_baker(self, sandbox: Sandbox, session):
        super().test_kill_baker(sandbox, session)

    def test_synchronize(self, sandbox: Sandbox):
        utils.synchronize(sandbox.all_clients())

    def test_check_operations(self, sandbox: Sandbox):
        super().test_check_operations(sandbox)
