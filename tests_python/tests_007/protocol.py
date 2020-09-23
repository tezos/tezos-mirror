from tools import constants, utils


def activate(
    client, parameters=None, timestamp=None, activate_in_the_past=False
):
    utils.activate_delphi_007(
        client, parameters, timestamp, activate_in_the_past
    )


HASH = constants.DELPHI
DAEMON = constants.DELPHI_DAEMON
PARAMETERS = constants.DELPHI_PARAMETERS

PREV_HASH = constants.CARTHAGE
PREV_DAEMON = constants.CARTHAGE_DAEMON
PREV_PARAMETERS = constants.CARTHAGE_PARAMETERS
