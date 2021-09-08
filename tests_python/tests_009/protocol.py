from tools import constants, utils

HASH = constants.FLORENCE
DAEMON = constants.FLORENCE_DAEMON
PARAMETERS = constants.FLORENCE_PARAMETERS
FOLDER = constants.FLORENCE_FOLDER


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
