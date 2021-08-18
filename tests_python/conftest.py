"""Pytest configuration
"""

import pytest
from client.client import Client
from tools.utils import bake


@pytest.fixture(scope="class")
def encrypted_account_with_tez(client: Client):
    """
    Import an encrypted account with some tez
    """
    client.import_secret_key(
        "encrypted_account",
        (
            "encrypted:edesk1n2uGpPtVaeyhWkZzTEcaPRzkQHrqkw5pk8VkZv"
            "p3rM5KSc3mYNH5cJEuNcfB91B3G3JakKzfLQSmrgF4ht"
        ),
        "password",
    )
    client.transfer(
        100, "bootstrap1", "encrypted_account", ["--burn-cap", "1.0"]
    )
    bake(client, bake_for="bootstrap1")


def pytest_addoption(parser) -> None:
    parser.addoption("--log-dir", action="store", help="specify log directory")
    parser.addoption(
        "--singleprocess",
        action='store_true',
        default=False,
        help="the node validates blocks using only one process,\
            useful for debugging",
    )
