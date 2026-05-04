#!/bin/sh

##############################################################################
#                                                                            #
# SPDX-License-Identifier: MIT                                               #
# SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>       #
#                                                                            #
##############################################################################

SCRIPT_DIR="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
cd "$SCRIPT_DIR" || exit

TZ_SCHEDULE_KIND=SECURITY_SCANS ./scheduled.sh
