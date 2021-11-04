import re
import pytest
from client.client import Client
from tools import constants, utils
from . import protocol

ENDORSING_SLOTS_PER_BLOCK = 2048
NUM_ACCOUNTS = 256
ACCOUNTS = [f'bootstrap{i}' for i in range(1, NUM_ACCOUNTS + 1)]
MAX_VALIDATION_TIME_MS = 1000


@pytest.fixture(scope="session")
def required_log_dir(
    log_dir: str,
) -> str:
    """Skip test if CLI user-provided logging directory is not given"""
    if log_dir is None:
        pytest.skip('test must be run with "--log-dir LOG_DIR: option"')
    return log_dir


@pytest.fixture(scope="class")
def client(sandbox):

    sandbox.add_node(0, config_client=False, params=constants.NODE_PARAMS)
    client = sandbox.client(0)
    client.import_secret_key(
        'activator',
        'unencrypted:edsk31vznjHSSpGExDMHYASz45VZqXN4DPxvsa4hAyY8dHM28cZzp6',
    )
    bootstrap_accounts = []
    for account in ACCOUNTS:
        client.gen_key(account)
        address = sandbox.client(0).show_address(account, show_secret=True)
        bootstrap_accounts.append([address.public_key, "4000000000000"])
    parameters = dict(protocol.PARAMETERS)
    parameters["bootstrap_accounts"] = bootstrap_accounts
    parameters["endorsers_per_block"] = ENDORSING_SLOTS_PER_BLOCK
    protocol.activate(client, parameters, activate_in_the_past=True)
    client.logs = sandbox.logs
    yield client


@pytest.mark.slow
@pytest.mark.incremental
class TestManualBaking:
    """This test bakes a block with NUM_ACCOUNTS endorsers and
    check that it takes less than MAX_VALIDATION_TIME_MS to
    bake this block.

    MAX_VALIDATION_TIME_MS is conservative to avoid
    spurious fails due to slow CI."""

    def test_endorse(self, client: Client):
        utils.bake(
            client,
            bake_args=['--max-priority', f'{ENDORSING_SLOTS_PER_BLOCK+1}'],
        )
        for account in ACCOUNTS:
            client.endorse(account)
        utils.bake(
            client,
            bake_args=['--max-priority', f'{ENDORSING_SLOTS_PER_BLOCK+1}'],
        )

    def test_check_baking_time_from_log(self, required_log_dir, client):
        assert required_log_dir
        file = client.logs[0]
        # pattern = r"completed in ?(\w*)ms"
        pattern = r"validator.block.*completed in ?(.*)s"
        time = []
        with open(file, "r") as stream:
            for line in stream:
                match = re.search(pattern, line)
                if match is not None:
                    time.append(match.groups()[0])
        # 3 blocks have been baked in this test
        #  . protocol injection
        #  . empty block
        #  . block with endorsers
        endorser_block_time = time[-1]
        # log format may use s or ms unit
        if endorser_block_time[-1] == 'm':
            endorser_block_time_ms = float(endorser_block_time[:-2])
        else:
            endorser_block_time_ms = float(endorser_block_time[:-1]) * 1000
        assert float(endorser_block_time_ms) < MAX_VALIDATION_TIME_MS
