import datetime
from enum import Enum, auto
from typing import Optional
from copy import deepcopy
from tools import constants, utils

HASH = constants.ALPHA
DAEMON = constants.ALPHA_DAEMON
PARAMETERS = constants.ALPHA_PARAMETERS

TENDERBAKE_PARAMETERS = deepcopy(PARAMETERS)
TENDERBAKE_PARAMETERS['consensus_threshold'] = 45
TENDERBAKE_PARAMETERS['consensus_committee_size'] = 67

FOLDER = constants.ALPHA_FOLDER

PREV_HASH = constants.HANGZHOU
PREV_DAEMON = constants.HANGZHOU_DAEMON
PREV_PARAMETERS = constants.HANGZHOU_PARAMETERS


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


def get_parameters(protocol: Optional[Protocol] = Protocol.CURRENT):
    """
    Args:
      protocol (Protocol): protocol id (either CURRENT or PREV).
                           Defaults to CURRENT

    Returns:
      A fresh copy of the protocol parameters w.r.t to protocol
    """
    # deepcopy call prevents any unforeseen and unwanted side effects
    # on the array parameters
    # e.g., bootstrap_accounts, commitments, endorsement_reward
    return deepcopy(
        dict((PARAMETERS if protocol is Protocol.CURRENT else PREV_PARAMETERS))
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
