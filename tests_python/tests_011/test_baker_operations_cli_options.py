"""Simple tests to check support for the following operations-related options
for baking
- --ignore-node-mempool
- --operations-pool [file|uri]
"""

import os
import os.path
import json
import time

from http.server import HTTPServer, SimpleHTTPRequestHandler
from multiprocessing import Process
from typing import List, Any

import pytest

from client.client import Client
from tools import constants, utils
from launchers.sandbox import Sandbox
from . import protocol

PORT = 12121
OPERATIONS_FILES_DIRECTORY = "operations_files"
EMPTY_OPERATIONS = "empty_operations"
ABSENT_OPERATIONS = "this_file_should_not_exist"
SINGLETON_OPERATIONS = "singleton_operations"
TEST_DIR = "tests_011"


class MyHttpServer:
    """Simple HTTP server launching in a separate process"""

    def __init__(self):
        server_address = ('localhost', PORT)
        httpd = HTTPServer(server_address, SimpleHTTPRequestHandler)
        process = Process(target=httpd.serve_forever, args=())
        self.process = process
        self.server = httpd

    def run(self):
        self.process.start()

    def close(self):
        self.server.server_close()
        self.process.terminate()


@pytest.fixture
def http_server():
    server = MyHttpServer()
    server.run()
    yield server
    server.close()


def get_filename(basename: str) -> str:
    return os.path.join(
        TEST_DIR, OPERATIONS_FILES_DIRECTORY, f"{basename}.json"
    )


class TestIgnoreNodeMempool:
    def test_ignore(self, client: Client):
        """Check that a transfer injected into the node is dutifully ignored
        when baking with --ignore-node-mempool
        """
        sender = "bootstrap4"
        balance0 = client.get_balance(sender)
        client.transfer(2, sender, 'bootstrap5')
        utils.bake(
            client, bake_args=['--minimal-timestamp', "--ignore-node-mempool"]
        )
        balance1 = client.get_balance(sender)
        # Make sure the operations has not been included, indirectly through
        # balance checks
        assert balance1 == balance0
        assert client.get_level() == 2

    def test_no_ignore(self, client: Client):
        """Check that a transfer injected, then ignored, can be injected at the
        next block"""
        sender = "bootstrap4"
        balance0 = client.get_balance(sender)
        utils.bake(client, bake_args=['--minimal-timestamp'])
        balance1 = client.get_balance(sender)
        assert balance1 != balance0
        assert client.get_level() == 3


def all_empty(lls: List[List[Any]]) -> bool:
    return all(map(lambda l: len(l) == 0, lls))


def mempool_to_operations(mempool):
    def to_op(applied_op):
        operation = {}
        operation['branch'] = applied_op['branch']

        operation['contents'] = applied_op['contents']
        operation['signature'] = applied_op['signature']

        return operation

    return [to_op(applied_op) for applied_op in mempool['applied']]


def get_operations(client: Client) -> List[dict]:
    return mempool_to_operations(client.get_mempool())


class TestExternalOperations:
    def test_bake_empty_operations_file(self, client: Client):
        level = client.get_level()
        utils.bake(
            client,
            bake_args=[
                '--minimal-timestamp',
                "--operations-pool",
                get_filename(EMPTY_OPERATIONS),
            ],
        )
        assert client.get_level() == level + 1
        head = client.get_head()
        assert all_empty(head['operations'])

    # http_server is a fixture that auto- runs and closes said HTTP server
    # pylint: disable=W0613
    def test_bake_empty_operations_http(self, client: Client, http_server):
        level = client.get_level()
        utils.bake(
            client,
            bake_args=[
                '--minimal-timestamp',
                "--operations-pool",
                f"http://localhost:{PORT}/{get_filename(EMPTY_OPERATIONS)}",
            ],
        )
        assert client.get_level() == level + 1
        head = client.get_head()
        assert all_empty(head['operations'])

    def test_bake_absent_operations_file(self, client: Client):
        """The absent resource should simply be ignored."""
        level = client.get_level()
        utils.bake(
            client,
            bake_args=[
                '--minimal-timestamp',
                "--operations-pool",
                f"{ABSENT_OPERATIONS}",
            ],
        )
        assert client.get_level() == level + 1
        head = client.get_head()
        assert all_empty(head['operations'])

    # pylint: disable=W0613
    def test_bake_absent_operations_http(self, client: Client, http_server):
        """The absent resource should simply be ignored."""
        level = client.get_level()
        utils.bake(
            client,
            bake_args=[
                '--minimal-timestamp',
                "--operations-pool",
                f"http://{ABSENT_OPERATIONS}",
            ],
        )
        assert client.get_level() == level + 1
        head = client.get_head()
        assert all_empty(head['operations'])

    def test_bake_singleton_operations_file_pre(
        self, client: Client, session: dict
    ):
        """Construct a transaction over the current state, and bake it.
        Store it into the context to serves as a dynamic oracle for the next
        steps.
        """
        sender = 'bootstrap2'
        balance0 = client.get_mutez_balance(sender)
        session['amount'] = 2
        client.transfer(session['amount'], sender, 'bootstrap3')

        # Baking
        utils.bake(client, bake_args=['--minimal-timestamp'])
        balance1 = client.get_mutez_balance(sender)
        session['difference'] = balance0 - balance1
        utils.bake(client)

    def test_bake_singleton_operations_file(
        self, client: Client, session: dict
    ):
        """Construct a transaction over the current state, put it into a file,
        and bake it into the chain through --operations-pool option.

        This additionally compares the balance to a normal transfer (through the
        node's mempool) to check that there is no observable difference in
        behaviors between passing through a node's mempool or a hand-rolled
        operations file.
        """
        sender = 'bootstrap4'
        balance0 = client.get_mutez_balance(sender)
        client.transfer(session['amount'], sender, 'bootstrap3')

        pending_ops = get_operations(client)
        assert len(pending_ops) == 1
        assert len(pending_ops[0]['contents']) == 1

        # Write the transaction to a file
        file = get_filename(SINGLETON_OPERATIONS)
        with open(file, 'w') as fdesc:
            fdesc.write(json.dumps(pending_ops))

        # Baking
        utils.bake(
            client,
            bake_args=[
                '--minimal-timestamp',
                "--operations-pool",
                file,
                '--ignore-node-mempool',
            ],
        )
        balance1 = client.get_mutez_balance(sender)
        assert balance0 - balance1 == session['difference']

        # cleanup the operation file
        os.remove(file)

    # pylint: disable=W0613
    def test_bake_singleton_operations_http(
        self, client: Client, sandbox: Sandbox, session: dict, http_server
    ):
        # Restart
        sandbox.node(0).terminate()
        sandbox.node(0).run()
        client.check_node_listening()

        sender = 'bootstrap2'
        balance0 = client.get_mutez_balance(sender)
        client.transfer(session['amount'], sender, 'bootstrap3')

        pending_ops = get_operations(client)
        assert len(pending_ops) == 1
        assert len(pending_ops[0]['contents']) == 1

        # Write the transaction to a file
        file = get_filename(SINGLETON_OPERATIONS)
        with open(file, 'w') as fdesc:
            fdesc.write(json.dumps(pending_ops))

        utils.bake(
            client,
            bake_args=[
                '--minimal-timestamp',
                "--operations-pool",
                f"http://localhost:{PORT}/{file}",
                '--ignore-node-mempool',
            ],
        )

        balance1 = client.get_mutez_balance(sender)
        assert balance0 - balance1 == session['difference']

        # cleanup the operation file
        os.remove(file)


@pytest.mark.incremental
class TestBakerExternalOperations:
    """Test adding an external operations to a baker daemon"""

    def test_init(self, sandbox: Sandbox):
        sandbox.add_node(0, params=constants.NODE_PARAMS)
        parameters = dict(protocol.PARAMETERS)
        utils.activate_protocol(
            sandbox.client(0),
            protocol.HASH,
            parameters=parameters,
            activate_in_the_past=True,
        )

    def test_gen_operations(self, sandbox: Sandbox, session: dict):
        """Generate a transfer operation and save it to a file"""
        sandbox.client(0).transfer(3, 'bootstrap1', 'bootstrap3')
        utils.bake(sandbox.client(0))
        # We are now at level 2, next block at level 3

        session['transfer_value'] = 2
        sandbox.client(0).transfer(
            session['transfer_value'], 'bootstrap1', 'bootstrap3'
        )

        pending_ops = get_operations(sandbox.client(0))

        # Write the transaction to a file
        filename = get_filename(SINGLETON_OPERATIONS)
        session['operations_file'] = filename
        with open(filename, 'w') as fdesc:
            fdesc.write(json.dumps(pending_ops))

    def test_terminate_sandbox(self, sandbox: Sandbox):
        """Cleanup the node's mempool. Forget about the last transfer"""
        sandbox.node(0).terminate()
        time.sleep(1)

    def test_baker(self, sandbox: Sandbox, session: dict):
        """Restart the node and add a baker"""
        sandbox.node(0).run()
        assert sandbox.client(0).check_node_listening()
        sandbox.add_baker(
            0,
            ['bootstrap1'],
            proto=protocol.DAEMON,
            run_params=['--operations-pool', session['operations_file']],
        )

    @pytest.mark.timeout(3)
    def test_wait_until_level5(self, sandbox: Sandbox):
        """Wait until we have seen enough blocks.
        This should not take much time."""
        while sandbox.client(0).get_level() < 5:
            time.sleep(1)

    def test_check_block3(self, sandbox: Sandbox, session: dict):
        """Check that block 3 exactly contains the operations that we put into
        our operations file"""
        block_3 = sandbox.client(0).rpc('get', '/chains/main/blocks/3')
        manager_ops = block_3['operations'][3]
        assert len(manager_ops) == 1
        assert int(
            manager_ops[0]['contents'][0]['amount']
        ) == utils.mutez_of_tez(session['transfer_value'])

    def test_check_block4(self, sandbox: Sandbox, session: dict):
        """Check that block 4 is empty of operations"""
        block_4 = sandbox.client(0).rpc('get', '/chains/main/blocks/4')
        assert all_empty(block_4['operations'])

        # cleanup the operation file
        os.remove(session['operations_file'])
