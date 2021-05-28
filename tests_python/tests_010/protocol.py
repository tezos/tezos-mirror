from tools import constants, utils

HASH = constants.GRANADA
DAEMON = constants.GRANADA_DAEMON
PARAMETERS = constants.GRANADA_PARAMETERS
FOLDER = constants.GRANADA_FOLDER

PREV_HASH = constants.FLORENCE
PREV_DAEMON = constants.FLORENCE_DAEMON
PREV_PARAMETERS = constants.FLORENCE_PARAMETERS


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
