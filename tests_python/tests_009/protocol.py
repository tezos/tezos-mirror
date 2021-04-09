from tools import constants, utils

HASH = constants.FLORENCE
DAEMON = constants.FLORENCE_DAEMON
PARAMETERS = constants.FLORENCE_PARAMETERS
FOLDER = constants.FLORENCE_FOLDER

PREV_HASH = constants.EDO
PREV_DAEMON = constants.EDO_DAEMON
PREV_PARAMETERS = constants.EDO_PARAMETERS


def activate(
    client, parameters=PARAMETERS, timestamp=None, activate_in_the_past=False
):
    utils.activate_protocol(
        client, HASH, parameters, timestamp, activate_in_the_past
    )
