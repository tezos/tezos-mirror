import datetime
from enum import Enum, auto
from copy import deepcopy
from tools import constants, utils

HASH = constants.ITHACA
DAEMON = constants.ITHACA_DAEMON
PARAMETERS = constants.ITHACA_PARAMETERS

TENDERBAKE_PARAMETERS = deepcopy(PARAMETERS)
TENDERBAKE_PARAMETERS['consensus_threshold'] = 45
TENDERBAKE_PARAMETERS['consensus_committee_size'] = 67

FOLDER = constants.ITHACA_FOLDER


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


class Protocol(Enum):
    CURRENT = auto()
    PREV = auto()


def get_parameters():
    """
    Returns:
      A fresh copy of the protocol parameters w.r.t to protocol
    """
    # deepcopy call prevents any unforeseen and unwanted side effects
    # on the array parameters
    # e.g., bootstrap_accounts, commitments, endorsement_reward
    return deepcopy(dict((PARAMETERS)))


def get_now(client) -> str:
    """Returns the timestamp of next-to-last block,
    offset by the minimum time between blocks"""

    timestamp_date = client.get_block_timestamp(block='head~1')

    constants = client.rpc('get', '/chains/main/blocks/head/context/constants')

    delta = datetime.timedelta(seconds=int(constants['minimal_block_delay']))

    now_date = timestamp_date + delta

    rfc3399_format = "%Y-%m-%dT%H:%M:%SZ"
    return now_date.strftime(rfc3399_format)
