import json
import os
import datetime

from typing import List

import pytest

from tools import constants, paths

# This test check that the deprecated RPC in protocol alpha works as in delphi.
# This test can be removed after the activation of the protocol 008 that will
# contains the deprecated RPC or after the removal of the deprecated RPC in a
# futur protocol. To do that it runs the same test twice, first with delphi
# activated then with alpha activated.

BAKER = 'bootstrap1'
BAKE_ARGS = ['--minimal-fees', '0', '--minimal-nanotez-per-byte', '0',
             '--minimal-nanotez-per-gas-unit', '0', '--max-priority', '512',
             '--minimal-timestamp']
PROTO_A = constants.DELPHI
PROTO_A_DAEMON = constants.DELPHI_DAEMON
PROTO_A_PATH = f"proto_{PROTO_A_DAEMON.replace('-','_')}"

PROTO_A_PARAMETERS_FILE = (f'{paths.TEZOS_HOME}src/{PROTO_A_PATH}/parameters/'
                           'sandbox-parameters.json')
assert os.path.isfile(PROTO_A_PARAMETERS_FILE), (f'{PROTO_A_PARAMETERS_FILE}'
                                                 ' cannot be found; please '
                                                 'first run `make` in '
                                                 '{paths.TEZOS_HOME}.')
with open(PROTO_A_PARAMETERS_FILE) as f:
    PROTO_A_PARAMETERS = dict(json.load(f))

PROTO_B = constants.ALPHA
PROTO_B_PARAMETERS = constants.PARAMETERS

BLOCKS_PER_CYCLE = 4
CYCLE_PER_VOTING_PERIOD = 5
BLOCKS_PER_VOTING_PERIOD = BLOCKS_PER_CYCLE * CYCLE_PER_VOTING_PERIOD


@pytest.fixture(scope="class")
def client(sandbox, protocol, parameters, node_id):
    # Scenarios run one after the other. To be able to create a new node with a
    # different protocol on the second scenario we need to delete the previous
    # node before.
    for i in range(node_id):
        sandbox.rm_node(i)
    sandbox.add_node(node_id, params=constants.NODE_PARAMS)
    parameters["blocks_per_cycle"] = BLOCKS_PER_CYCLE
    parameters["blocks_per_voting_period"] = BLOCKS_PER_VOTING_PERIOD
    delay = datetime.timedelta(seconds=3600 * 24 * 365)
    sandbox.client(node_id).activate_protocol_json(protocol, parameters,
                                                   delay=delay)
    yield sandbox.client(node_id)


def pytest_generate_tests(metafunc):
    # this function annotates correctly all test with the value from the
    # declared scenario.
    idlist = []
    argvalues = []
    for scenario in metafunc.cls.scenarios:
        idlist.append(scenario[0])
        items = scenario[1].items()
        argnames = [x[0] for x in items]
        argvalues.append([x[1] for x in items])
    metafunc.parametrize(argnames, argvalues, ids=idlist, scope="class")


SCENARIO_PROTO_A = (PROTO_A, {"node_id": 0, "protocol": PROTO_A,
                              "parameters": PROTO_A_PARAMETERS})
SCENARIO_PROTO_B = (PROTO_B, {"node_id": 1, "protocol": PROTO_B,
                              "parameters": PROTO_B_PARAMETERS})


@pytest.mark.incremental
class TestRpc:
    scenarios = [SCENARIO_PROTO_A, SCENARIO_PROTO_B]

    def test_activate(self, client, protocol):
        client.bake(BAKER, BAKE_ARGS)
        assert client.get_protocol() == protocol

    def test_rpc(self, client):
        current_period_kind = client.get_current_period_kind()
        current_level = client.get_current_level()
        metadata = client.get_metadata()
        meta_level = metadata["level"]
        meta_period_kind = metadata["voting_period_kind"]
        assert current_period_kind == meta_period_kind
        assert current_level == meta_level
        assert current_level["level"] == 2
        assert current_level["level_position"] == 1
        assert current_level["cycle"] == 0
        assert current_level["cycle_position"] == 1
        assert current_level["voting_period"] == 0
        assert current_level["voting_period_position"] == 1
        assert current_period_kind == "proposal"

    def test_bake_last_block(self, client):
        for _i in range(BLOCKS_PER_VOTING_PERIOD - 2):
            client.bake(BAKER, BAKE_ARGS)

    def test_rpc_test_last_block(self, client):
        current_period_kind = client.get_current_period_kind()
        current_level = client.get_current_level()
        metadata = client.get_metadata()
        meta_level = metadata["level"]
        meta_period_kind = metadata["voting_period_kind"]
        assert current_period_kind == meta_period_kind
        assert current_level == meta_level
        assert current_level["level"] == BLOCKS_PER_VOTING_PERIOD
        assert current_level["level_position"] == BLOCKS_PER_VOTING_PERIOD - 1
        assert current_level["cycle"] == CYCLE_PER_VOTING_PERIOD - 1
        assert current_level["cycle_position"] == BLOCKS_PER_CYCLE - 1
        assert current_level["voting_period"] == 0
        current_lvl_voting_position = current_level["voting_period_position"]
        assert current_lvl_voting_position == BLOCKS_PER_VOTING_PERIOD - 1
        assert current_period_kind == "proposal"

    def test_rpc_next_period(self, client):
        client.bake(BAKER, BAKE_ARGS)
        current_period_kind = client.get_current_period_kind()
        current_level = client.get_current_level()
        metadata = client.get_metadata()
        meta_level = metadata["level"]
        meta_period_kind = metadata["voting_period_kind"]
        assert current_period_kind == meta_period_kind
        assert current_level == meta_level
        assert current_level["level"] == BLOCKS_PER_VOTING_PERIOD + 1
        assert current_level["level_position"] == BLOCKS_PER_VOTING_PERIOD
        assert current_level["cycle"] == CYCLE_PER_VOTING_PERIOD
        assert current_level["cycle_position"] == 0
        assert current_level["voting_period"] == 1
        assert current_level["voting_period_position"] == 0
        assert current_period_kind == "proposal"
