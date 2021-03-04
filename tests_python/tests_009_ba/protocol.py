from tools import constants, utils

HASH = constants.FLOR_BA
DAEMON = constants.FLOR_BA_DAEMON
PARAMETERS = constants.FLOR_BA_PARAMETERS

PREV_HASH = constants.EDO
PREV_DAEMON = constants.EDO_DAEMON
PREV_PARAMETERS = constants.EDO_PARAMETERS


def activate(
    client, parameters=PARAMETERS, timestamp=None, activate_in_the_past=False
):
    utils.activate_protocol(
        client, HASH, parameters, timestamp, activate_in_the_past
    )
    utils.remember_baker_contracts(client)
