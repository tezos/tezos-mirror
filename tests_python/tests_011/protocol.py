import datetime
from tools import constants, utils

HASH = constants.HANGZHOU
DAEMON = constants.HANGZHOU_DAEMON
PARAMETERS = constants.HANGZHOU_PARAMETERS
FOLDER = constants.HANGZHOU_FOLDER


def activate(
    client,
    parameters=PARAMETERS,
    proto=HASH,
    timestamp=None,
    activate_in_the_past=False,
):
    utils.activate_protocol(
        client, proto, parameters, timestamp, activate_in_the_past
    )


def get_now(client) -> str:
    """Returns the timestamp of next-to-last block,
    offset by the minimum time between blocks"""

    timestamp_date = client.get_block_timestamp(block='head~1')

    constants = client.rpc('get', '/chains/main/blocks/head/context/constants')

    delta = datetime.timedelta(seconds=int(constants['minimal_block_delay']))

    now_date = timestamp_date + delta

    rfc3399_format = "%Y-%m-%dT%H:%M:%SZ"
    return now_date.strftime(rfc3399_format)
