from tools import constants, utils

HASH = constants.ALPHA
DAEMON = constants.ALPHA_DAEMON
PARAMETERS = constants.ALPHA_PARAMETERS
FOLDER = constants.ALPHA_FOLDER

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
