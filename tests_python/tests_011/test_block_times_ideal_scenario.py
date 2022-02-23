import random
import time
import pytest
from tools import utils, constants
from launchers.sandbox import Sandbox
from . import protocol

random.seed(42)
NUM_NODES = 5
TEST_DURATION = 10
BD = 3  # base delay = time_between_blocks[0]
PD = 20  # priority delay = time_between_blocks[1]
IE = 2  # initial_endorsers
MD = 2  # base delay = time_between_blocks[0]


@pytest.mark.baker
@pytest.mark.multinode
@pytest.mark.slow
@pytest.mark.incremental
class TestBakers:
    """Run NUM_NODES bakers and check that blocks are produced with the
    expected timestamp.

    """

    def test_setup_network(self, sandbox: Sandbox):
        parameters = dict(protocol.PARAMETERS)
        parameters["time_between_blocks"] = [str(BD), str(PD)]
        parameters["initial_endorsers"] = IE
        parameters["minimal_block_delay"] = "1"
        assert parameters["delay_per_missing_endorsement"] == '1'
        for i in range(NUM_NODES):
            sandbox.add_node(i, params=constants.NODE_PARAMS)

        protocol.activate(sandbox.client(0), parameters)

    def test_wait_for_protocol(self, sandbox: Sandbox):
        clients = sandbox.all_clients()
        for client in clients:
            proto = protocol.HASH
            assert utils.check_protocol(client, proto)
            assert client.get_level() == 1

    def test_add_bakers(self, sandbox: Sandbox):
        for i in range(NUM_NODES):
            sandbox.add_baker(i, [f'bootstrap{i+1}'], proto=protocol.DAEMON)

    def test_check_level_and_timestamp(self, sandbox: Sandbox):
        time.sleep(TEST_DURATION)
        min_level = min(
            [client.get_level() for client in sandbox.all_clients()]
        )
        heads_hash = set()
        # check there is exactly one block at the common level
        for client in sandbox.all_clients():
            header = client.get_header(block=str(min_level))
            heads_hash.add(header['hash'])
        assert len(heads_hash) == 1

        # at least two new blocks should have been produced
        assert min_level >= 3

        # check that the timestamp difference is the expected one,
        # use blocks at levels 2 and 3 (and not 1 and 2) because the
        # one at level 2 may be baked late if the bakers start slowly
        client = sandbox.client(0)
        ts1 = client.get_block_timestamp(block=str(2))
        ts2 = client.get_block_timestamp(block=str(3))
        time_diff = (ts2 - ts1).total_seconds()
        # there will be initial_endorsers missing endorsements
        # so the block delay is BD + IE * 1
        assert time_diff == BD + IE


@pytest.mark.baker
@pytest.mark.endorser
@pytest.mark.multinode
@pytest.mark.slow
@pytest.mark.incremental
class TestBakersAndEndorsers:
    """Run NUM_NODES bakers and endorsers and check that blocks are
    produced with the expected timestamp."""

    def test_setup_network(self, sandbox: Sandbox):
        parameters = dict(protocol.PARAMETERS)
        parameters["time_between_blocks"] = [str(BD), str(PD)]
        parameters["minimal_block_delay"] = "2"
        # we require all endorsements to be present
        parameters["initial_endorsers"] = parameters["endorsers_per_block"]
        for i in range(NUM_NODES):
            sandbox.add_node(i, params=constants.NODE_PARAMS)

        protocol.activate(sandbox.client(0), parameters)

    def test_wait_for_protocol(self, sandbox: Sandbox):
        clients = sandbox.all_clients()
        for client in clients:
            proto = protocol.HASH
            assert utils.check_protocol(client, proto)
            assert client.get_level() == 1

    def test_add_bakers_and_endorsers(self, sandbox: Sandbox):
        for i in range(NUM_NODES):
            sandbox.add_baker(i, [f'bootstrap{i+1}'], proto=protocol.DAEMON)
        for i in range(NUM_NODES):
            sandbox.add_endorser(
                i,
                account=f'bootstrap{i+1}',
                endorsement_delay=0,
                proto=protocol.DAEMON,
            )

    def test_rm_bakers(self, sandbox: Sandbox):
        time.sleep(TEST_DURATION)
        for i in range(NUM_NODES):
            sandbox.rm_baker(i, proto=protocol.DAEMON)

    def test_check_level_and_timestamp(self, sandbox: Sandbox):
        client = sandbox.client(0)
        levels = [client.get_level() for client in sandbox.all_clients()]
        levels.sort()
        min_level = levels[0]
        max_level = levels[NUM_NODES - 1]

        heads_hash = set()
        # check there is exactly one block at the common level
        for client in sandbox.all_clients():
            header = client.get_header(block=str(min_level))
            heads_hash.add(header['hash'])
        assert len(heads_hash) == 1

        # There should be one block every MD seconds, so normally the level
        # should have increased with TEST_DURATION / MD levels.
        # We decrement by 1 "for safety".
        assert max_level >= TEST_DURATION / MD

        # the RPCs should be quick wrt to the time between blocks,
        # so nodes do not have time to diverge
        assert levels[(NUM_NODES + 1) // 2] >= max_level - 2

        # check that the timestamp difference is the expected one
        ts0 = client.get_block_timestamp(block=str(2))
        ts1 = client.get_block_timestamp(block=str(max_level))
        time_diff = (ts1 - ts0).total_seconds()
        assert time_diff == MD * (max_level - 2)
