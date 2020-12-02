"""Pytest configuration
"""


def pytest_addoption(parser) -> None:
    parser.addoption("--log-dir", action="store", help="specify log directory")
    parser.addoption(
        "--singleprocess",
        action='store_true',
        default=False,
        help="the node validates blocks using only one process,\
            useful for debugging",
    )
