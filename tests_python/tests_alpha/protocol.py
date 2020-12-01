from tools import constants, utils


def activate(
    client, parameters=None, timestamp=None, activate_in_the_past=False
):
    utils.activate_alpha(client, parameters, timestamp, activate_in_the_past)


HASH = constants.ALPHA
DAEMON = constants.ALPHA_DAEMON
PARAMETERS = constants.ALPHA_PARAMETERS

PREV_HASH = constants.EDO
PREV_DAEMON = constants.EDO_DAEMON
PREV_PARAMETERS = constants.EDO_PARAMETERS
