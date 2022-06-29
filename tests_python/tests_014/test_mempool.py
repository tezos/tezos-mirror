import pytest
from tools import utils, constants
from launchers.sandbox import Sandbox

from . import protocol


@pytest.mark.mempool
@pytest.mark.multinode
@pytest.mark.slow
@pytest.mark.incremental
class TestMempool:
    "Tests mempool"

    def test_init(self, sandbox: Sandbox):
        sandbox.add_node(1, params=constants.NODE_PARAMS)
        sandbox.add_node(2, params=constants.NODE_PARAMS)
        sandbox.add_node(
            3, params=constants.NODE_PARAMS + ['--disable-mempool']
        )
        protocol.activate(sandbox.client(1), activate_in_the_past=True)

    def test_level1(self, sandbox: Sandbox):
        level = 1
        for client in sandbox.all_clients():
            assert utils.check_level(client, level)

    def test_running_prevalidators(self, sandbox: Sandbox):
        assert sandbox.client(1).get_prevalidator()
        assert sandbox.client(2).get_prevalidator()
        assert not sandbox.client(3).get_prevalidator()

    def test_mempool_empty(self, sandbox: Sandbox):
        for i in range(1, 4):
            assert sandbox.client(i).mempool_is_empty()

    def test_transfer(self, sandbox: Sandbox, session: dict):
        client = sandbox.client(1)
        session['trsfr_hash'] = client.transfer(
            1.000, 'bootstrap1', 'bootstrap2'
        ).operation_hash

    def test_mempool_include_transfer(self, sandbox: Sandbox, session: dict):
        assert utils.check_mempool_contains_operations(
            sandbox.client(1), [session['trsfr_hash']]
        )
        assert utils.check_mempool_contains_operations(
            sandbox.client(2), [session['trsfr_hash']]
        )
        assert sandbox.client(3).mempool_is_empty()

    def test_bake_for1(self, sandbox: Sandbox):
        utils.bake(sandbox.client(1))

    def test_level2(self, sandbox: Sandbox):
        level = 2
        for client in sandbox.all_clients():
            assert utils.check_level(client, level)

    def test_mempools_are_empty(self, sandbox: Sandbox):
        for i in range(1, 4):
            assert sandbox.client(i).mempool_is_empty()

    def test_injection_fails_on_mempool_disabled_node(self, sandbox: Sandbox):
        with pytest.raises(Exception):
            sandbox.client(3).transfer(2.000, 'bootstrap2', 'bootstrap3')
