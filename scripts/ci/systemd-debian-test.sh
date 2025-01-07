#!/bin/bash

set -eu

apk --no-cache add screen procps bash

scripts/packaging/tests/systemd-docker-test.sh
