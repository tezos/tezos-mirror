from tools import constants, utils

HASH = constants.FLORENCE
DAEMON = constants.FLORENCE_DAEMON
PARAMETERS = constants.FLORENCE_PARAMETERS

PREV_HASH = constants.EDO
PREV_DAEMON = constants.EDO_DAEMON
PREV_PARAMETERS = constants.EDO_PARAMETERS


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
