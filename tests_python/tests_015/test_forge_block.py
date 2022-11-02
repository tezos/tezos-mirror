import datetime
import pytest
from tools import constants
from tools.constants import PROTO_DEMO_NOOPS
from launchers.sandbox import Sandbox


@pytest.mark.slow
@pytest.mark.incremental
class TestForgeBlock:
    """Check that a block more than 5 seconds in the future is rejected"""

    def test_setup_network(self, sandbox: Sandbox):
        sandbox.add_node(1, params=constants.NODE_PARAMS)

    def test_protocol_exists(self, sandbox: Sandbox):
        client = sandbox.client(1)
        protocols = client.list_protocols()
        assert PROTO_DEMO_NOOPS in protocols

    def test_activate_proto_demo_time_shifted_ok(self, sandbox: Sandbox):
        parameters = {}  # type: dict
        delta = datetime.timedelta(seconds=5)
        sandbox.client(1).activate_protocol_json(
            PROTO_DEMO_NOOPS,
            parameters,
            key='activator',
            timestamp=(datetime.datetime.utcnow() + delta).strftime(
                "%Y-%m-%dT%H:%M:%SZ"
            ),
            fitness='1',
        )

    @pytest.mark.xfail
    def test_activate_proto_demo_time_shifted_ko(self, sandbox: Sandbox):
        parameters = {}  # type: dict
        delta = datetime.timedelta(seconds=30)
        sandbox.client(1).activate_protocol_json(
            PROTO_DEMO_NOOPS,
            parameters,
            key='activator',
            timestamp=(datetime.datetime.utcnow() + delta).strftime(
                "%Y-%m-%dT%H:%M:%SZ"
            ),
            fitness='1',
        )
