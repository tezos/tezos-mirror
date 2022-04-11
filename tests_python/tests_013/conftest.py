"""Protocol-specific hooks and fixtures"""

import tempfile
from typing import Optional, Iterator, List
import pytest
from launchers.sandbox import Sandbox
from tools import constants, utils
from tools.client_regression import ClientRegression
from client.client import Client
from client.client_output import CreateMockupResult

from . import protocol


@pytest.fixture(scope="class")
def client(sandbox: Sandbox) -> Iterator[Client]:
    """One node with protocol 013.

    Activate protocol 013 one year in the past. This avoids waiting
    when baking blocks manually from the client using `bake for`
    """
    sandbox.add_node(0, params=constants.NODE_PARAMS)
    client = sandbox.client(0)
    parameters = protocol.get_parameters()
    parameters['consensus_threshold'] = 0
    protocol.activate(client, parameters=parameters, activate_in_the_past=True)
    yield client


@pytest.fixture(scope="class")
def client_regtest_bis(sandbox: Sandbox) -> Iterator[Client]:
    """One node with protocol 013, regression test enabled.

    Activate protocol 013 one year in the past. (see fixture client).
    """

    def reg_client_factory(
        client_path: str,
        admin_client_path: str,
        host: Optional[str] = None,
        base_dir: Optional[str] = None,
        rpc_port: Optional[int] = None,
        use_tls: Optional[bool] = None,
        endpoint: Optional[str] = 'http://127.0.0.1:8732',
        mode: str = None,
        disable_disclaimer: bool = True,
    ) -> ClientRegression:
        client = ClientRegression(
            client_path=client_path,
            admin_client_path=admin_client_path,
            host=host,
            base_dir=base_dir,
            rpc_port=rpc_port,
            use_tls=use_tls,
            endpoint=endpoint,
            mode=mode,
            disable_disclaimer=disable_disclaimer,
        )
        return client

    sandbox.add_node(
        1, client_factory=reg_client_factory, params=constants.NODE_PARAMS
    )
    client = sandbox.client(1)
    parameters = protocol.get_parameters()
    parameters['consensus_threshold'] = 0
    protocol.activate(client, activate_in_the_past=True, parameters=parameters)
    yield client


@pytest.fixture(scope="class")
def clients(sandbox: Sandbox, request) -> Iterator[List[Client]]:
    """N node with protocol 013. Parameterized by the number of nodes.

    Number of nodes is specified as a class annotation.
    @pytest.mark.parametrize('clients', [N], indirect=True)

    Activate protocol 013 one year in the past. (see fixture client).
    """
    assert request.param is not None
    num_nodes = request.param
    for i in range(num_nodes):
        # Large number may increases peers connection time
        sandbox.add_node(i, params=constants.NODE_PARAMS)
    parameters = protocol.get_parameters()
    parameters['consensus_threshold'] = 0
    parameters['minimal_block_delay'] = '1'
    parameters['delay_increment_per_round'] = '1'
    protocol.activate(
        sandbox.client(0), parameters=parameters, activate_in_the_past=True
    )

    clients = sandbox.all_clients()
    for client in clients:
        proto = protocol.HASH
        assert utils.check_protocol(client, proto)
    yield clients


@pytest.fixture
def mockup_client(sandbox: Sandbox) -> Iterator[Client]:
    """
    Returns a mockup client with its persistent directory created

    This is done in two steps, because we want to create the mockup
    with a client that doesn't have "--mode mockup" (as per
    the public documentation) but we want to return a
    client that has "--mode mockup" and uses the base-dir created
    in the first step.

    There is no way around this pattern. If you want to create
    a mockup using custom arguments; you MUST do the same
    as this method.
    """
    with tempfile.TemporaryDirectory(prefix='tezos-client.') as base_dir:
        unmanaged_client = sandbox.create_client(base_dir=base_dir)
        res = unmanaged_client.create_mockup(
            protocol=protocol.HASH
        ).create_mockup_result
        assert res == CreateMockupResult.OK
        yield sandbox.create_client(base_dir=base_dir, mode="mockup")
