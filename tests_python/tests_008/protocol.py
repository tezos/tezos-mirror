from tools import constants, utils

HASH = constants.EDO
DAEMON = constants.EDO_DAEMON
PARAMETERS = constants.EDO_PARAMETERS

PREV_HASH = constants.DELPHI
PREV_DAEMON = constants.DELPHI_DAEMON
PREV_PARAMETERS = constants.DELPHI_PARAMETERS


def activate(
    client, parameters=PARAMETERS, timestamp=None, activate_in_the_past=False
):
    utils.activate_protocol(
        client, HASH, parameters, timestamp, activate_in_the_past
    )
