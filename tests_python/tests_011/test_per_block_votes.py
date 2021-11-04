import time
from typing import Optional, Iterator

import pytest

from launchers.sandbox import Sandbox
from tools import utils, constants, paths

from . import protocol


def run_vote_file_test(sandbox, filename):
    sandbox.rm_baker(0, proto=protocol.DAEMON)
    sandbox.add_baker(
        0,
        ['bootstrap1'],
        proto=protocol.DAEMON,
        run_params=["--votefile", filename],
    )
    if not sandbox.log_dir:
        pytest.skip()
    time.sleep(1)
    assert sandbox.logs
    assert utils.check_logs(sandbox.logs, r'Error')


def run_vote_file_test_error(sandbox, filename, error_pattern):
    sandbox.rm_baker(0, proto=protocol.DAEMON)
    sandbox.add_baker(
        0,
        ['bootstrap1'],
        proto=protocol.DAEMON,
        run_params=["--votefile", filename],
    )
    if not sandbox.log_dir:
        pytest.skip()
    time.sleep(1)
    assert sandbox.logs
    assert not utils.check_logs(sandbox.logs, error_pattern)


def run_nonexistent_file_test(sandbox, filename):
    error_pattern = (
        r'The provided block vote file path '
        f'"{filename}" does not point to an existing file.'
    )
    run_vote_file_test_error(sandbox, filename, error_pattern)


def run_invalid_file_test(sandbox, filename):
    error_pattern = (
        r'The provided block vote file path '
        f'"{filename}" does not point to a valid JSON file.'
    )
    run_vote_file_test_error(sandbox, filename, error_pattern)


def run_wrong_content_file_test(sandbox, filename):
    error_pattern = (
        r'The provided block vote file '
        f'"{filename}" is a valid JSON file but its content is unexpected.'
    )
    run_vote_file_test_error(sandbox, filename, error_pattern)


@pytest.fixture(scope="class")
def sandbox(log_dir: Optional[str], singleprocess: bool) -> Iterator[Sandbox]:
    """Sandboxed network of nodes where daemons are allowed to fail.

    Nodes, bakers and endorsers are added/removed dynamically."""
    # log_dir is None if not provided on command-line
    # singleprocess is false if not provided on command-line
    with Sandbox(
        paths.TEZOS_HOME,
        constants.IDENTITIES,
        log_dir=log_dir,
        singleprocess=singleprocess,
    ) as sandbox:
        yield sandbox


class TestAllPerBlockVotes:
    def test_setup_network(self, sandbox: Sandbox):
        parameters = dict(protocol.PARAMETERS)
        # each priority has a delay of 1 sec
        parameters["time_between_blocks"] = ["1"]
        sandbox.add_node(0, params=constants.NODE_PARAMS)
        protocol.activate(sandbox.client(0), parameters)
        sandbox.add_baker(0, ['bootstrap1'], proto=protocol.DAEMON)

    def test_wait_for_protocol(self, sandbox: Sandbox):
        clients = sandbox.all_clients()
        for client in clients:
            proto = protocol.HASH
            assert utils.check_protocol(client, proto)

    def test_true_vote_file(self, sandbox: Sandbox):
        filename = "tests_011/per_block_vote_files/true.json"
        run_vote_file_test(sandbox, filename)

    def test_false_vote_file(self, sandbox: Sandbox):
        filename = "tests_011/per_block_vote_files/false.json"
        run_vote_file_test(sandbox, filename)

    def test_nonexistent_vote_file(self, sandbox: Sandbox):
        filename = "tests_011/per_block_vote_files/nonexistant.json"
        run_nonexistent_file_test(sandbox, filename)

    def test_invalid_json(self, sandbox: Sandbox):
        filename = "tests_011/per_block_vote_files/invalid.json"
        run_invalid_file_test(sandbox, filename)

    def test_nonboolean(self, sandbox: Sandbox):
        filename = "tests_011/per_block_vote_files/non_boolean.json"
        run_wrong_content_file_test(sandbox, filename)

    def test_wrong_key(self, sandbox: Sandbox):
        filename = "tests_011/per_block_vote_files/wrong_key.json"
        run_wrong_content_file_test(sandbox, filename)
