# This test relies on events read from the logs of the baker. It must
# be run from the tests_python/ directory with the "--log-dir" option
# of pytest set to "tmp"

import time

from typing import Optional, Iterator

import pytest

from launchers.sandbox import Sandbox
from tools import utils, constants, paths

from . import protocol

MINIMAL_BLOCK_DELAY = 2
SLEEP = 2 * MINIMAL_BLOCK_DELAY


def run_vote_file(sandbox: Sandbox, filename: str, default_vote: str) -> None:
    sandbox.rm_baker(0, proto=protocol.DAEMON)
    sandbox.add_baker(
        0,
        [f'bootstrap{i}' for i in range(1, 6)],
        proto=protocol.DAEMON,
        run_params=[
            "--votefile",
            filename,
            '--liquidity-baking-toggle-vote',
            default_vote,
        ],
    )
    if not sandbox.log_dir:
        pytest.skip()
    time.sleep(SLEEP)
    assert sandbox.logs


def check_baker_logs(sandbox: Sandbox, pattern: str) -> bool:
    baker_logs = sandbox.bakers[protocol.DAEMON][0].log_file
    assert baker_logs
    logs = [baker_logs]
    return utils.check_logs(logs, pattern)


def get_what_the_baker_votes_from_its_logs(sandbox: Sandbox) -> str:
    if not check_baker_logs(
        sandbox, 'Voting on for liquidity baking toggle vote'
    ):
        return 'on'
    if not check_baker_logs(
        sandbox, 'Voting off for liquidity baking toggle vote'
    ):
        return 'off'
    if not check_baker_logs(
        sandbox, 'Voting pass for liquidity baking toggle vote'
    ):
        return 'pass'
    return 'error'


def run_vote_file_test(sandbox: Sandbox, filename: str, default_vote) -> str:
    run_vote_file(sandbox, filename, default_vote)
    assert check_baker_logs(
        sandbox,
        (
            r'The provided block vote file path '
            f'"{filename}" does not point to an existing file.'
            '|'
            r'The provided block vote file path '
            f'"{filename}" does not point to a valid JSON file.'
            '|'
            r'The provided block vote file '
            f'"{filename}" is a valid JSON file but its content is unexpected.'
        ),
    )
    return get_what_the_baker_votes_from_its_logs(sandbox)


def run_vote_file_test_error(
    sandbox: Sandbox, filename: str, error_pattern: str, default_vote: str
) -> None:
    run_vote_file(sandbox, filename, default_vote)
    assert not check_baker_logs(sandbox, error_pattern)
    assert get_what_the_baker_votes_from_its_logs(sandbox) == default_vote


def run_nonexistent_file_test(sandbox, filename, default_vote):
    error_pattern = (
        r'The provided block vote file path '
        f'"{filename}" does not point to an existing file.'
    )
    run_vote_file_test_error(sandbox, filename, error_pattern, default_vote)


def run_invalid_file_test(sandbox, filename, default_vote):
    error_pattern = (
        r'The provided block vote file path '
        f'"{filename}" does not point to a valid JSON file.'
    )
    run_vote_file_test_error(sandbox, filename, error_pattern, default_vote)


def run_wrong_content_file_test(sandbox, filename, default_vote):
    error_pattern = (
        r'The provided block vote file '
        f'"{filename}" is a valid JSON file but its content is unexpected.'
    )
    run_vote_file_test_error(sandbox, filename, error_pattern, default_vote)


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
        sandbox.add_node(0, params=constants.NODE_PARAMS)
        parameters = protocol.get_parameters()
        parameters['minimal_block_delay'] = str(MINIMAL_BLOCK_DELAY)
        parameters['delay_increment_per_round'] = "1"
        protocol.activate(sandbox.client(0), parameters=parameters)
        sandbox.add_baker(
            0,
            ['bootstrap1'],
            proto=protocol.DAEMON,
            run_params=['--liquidity-baking-toggle-vote', 'pass'],
        )

    def test_wait_for_protocol(self, sandbox: Sandbox):
        clients = sandbox.all_clients()
        for client in clients:
            proto = protocol.HASH
            assert utils.check_protocol(client, proto)

    def test_on_vote_file(self, sandbox: Sandbox):
        filename = "tests_alpha/per_block_vote_files/on.json"
        assert run_vote_file_test(sandbox, filename, 'off') == 'on'

    def test_off_vote_file(self, sandbox: Sandbox):
        filename = "tests_alpha/per_block_vote_files/off.json"
        assert run_vote_file_test(sandbox, filename, 'on') == 'off'

    def test_pass_vote_file(self, sandbox: Sandbox):
        filename = "tests_alpha/per_block_vote_files/pass.json"
        assert run_vote_file_test(sandbox, filename, 'on') == 'pass'

    def test_nonexistent_vote_file(self, sandbox: Sandbox):
        filename = "tests_alpha/per_block_vote_files/nonexistant.json"
        run_nonexistent_file_test(sandbox, filename, 'pass')

    def test_invalid_json(self, sandbox: Sandbox):
        filename = "tests_alpha/per_block_vote_files/invalid.json"
        run_invalid_file_test(sandbox, filename, 'pass')

    # Tests that using booleans (which was possible until Ithaca) is
    # not possible anymore.
    def test_true_vote_file(self, sandbox: Sandbox):
        filename = "tests_alpha/per_block_vote_files/true.json"
        run_wrong_content_file_test(sandbox, filename, 'pass')

    def test_false_vote_file(self, sandbox: Sandbox):
        filename = "tests_alpha/per_block_vote_files/false.json"
        run_wrong_content_file_test(sandbox, filename, 'pass')

    def test_wrong_key(self, sandbox: Sandbox):
        filename = "tests_alpha/per_block_vote_files/wrong_key.json"
        run_wrong_content_file_test(sandbox, filename, 'pass')
