"""Pytest configuration
"""

import pytest
from client.client import Client


@pytest.fixture(scope="class")
def encrypted_account(client: Client):
    """
    Import an encrypted account
    """
    client.import_secret_key(
        "encrypted_account",
        (
            "encrypted:edesk1n2uGpPtVaeyhWkZzTEcaPRzkQHrqkw5pk8VkZv"
            "p3rM5KSc3mYNH5cJEuNcfB91B3G3JakKzfLQSmrgF4ht"
        ),
        "password",
    )


def pytest_addoption(parser) -> None:
    parser.addoption("--log-dir", action="store", help="specify log directory")
    parser.addoption(
        "--singleprocess",
        action='store_true',
        default=False,
        help="the node validates blocks using only one process,\
            useful for debugging",
    )
