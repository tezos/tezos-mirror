#!/bin/sh
set -eu

# Mandatory variables
DISTRIBUTION=${DISTRIBUTION:?You must specify a distribution}
RELEASE=${RELEASE:?You must specify a release}
TESTFILE=${1:?You must specify a test file to run}
DOCKERFILE=${2:?You must specify a dockerfile}

# CI variables with defaults from the local git repository
CI_COMMIT_SHORT_SHA=${CI_COMMIT_SHORT_SHA:-$(git rev-parse --short HEAD)}
CI_COMMIT_REF_SLUG=${CI_COMMIT_REF_SLUG:-$(git rev-parse --abbrev-ref HEAD)}
CI_COMMIT_REF_NAME=${CI_COMMIT_REF_NAME:-$(git symbolic-ref --short HEAD)}
CI_COMMIT_TAG=${CI_COMMIT_TAG:-}
CI_COMMIT_REF_PROTECTED=${CI_COMMIT_REF_PROTECTED:-"false"}

PREFIX=${PREFIX:-""}
CI_PROJECT_NAMESPACE=${CI_PROJECT_NAMESPACE:-tezos}
GCP_LINUX_PACKAGES_BUCKET=${GCP_LINUX_PACKAGES_BUCKET:-tezos-linux-repo}

#shellcheck disable=SC2236
if [ ! -z ${DEP_IMAGE+x} ]; then
  IMAGE="$DEP_IMAGE:${RELEASE}-${CI_COMMIT_REF_SLUG}-${CI_COMMIT_SHORT_SHA}"
  docker pull "$IMAGE"
  echo "Using $IMAGE"
else
  # for local execution, we assume the image name is "systemd"
  IMAGE="systemd"
  docker rm -f $IMAGE || true
  docker rmi $IMAGE || true
  echo "Image $IMAGE does not exist locally."
  echo "Building or updating the local image for you"
  docker build --build-arg IMAGE="$DISTRIBUTION:$RELEASE" -f "$DOCKERFILE" . -t systemd
fi

# Trap signals (EXIT, INT, TERM) and perform cleanup
trap 'echo "Stopping and removing container systemd" && \
  (docker rm -f systemd || true) && (docker rmi systemd || true)' INT TERM EXIT

# Run the container in the background and capture the PID of the background process
screen -d -m /bin/sh -c "docker run -i --rm --privileged --name systemd -v $PWD/$TESTFILE:/$TESTFILE $IMAGE"

timeout=30
elapsed=0
while ! docker inspect -f '{{.State.Running}}' systemd 2> /dev/null | grep -q true; do
  if [ $elapsed -ge $timeout ]; then
    echo "Container did not start within $timeout seconds."
    exit 1
  fi
  sleep 1
  elapsed=$((elapsed + 1))
done

ARGS=""
if [ -n "${DATADIR:-}" ]; then
  ARGS="$ARGS --data-dir $DATADIR"
fi
if [ -n "${AGNOSTIC_BAKER:-}" ]; then
  ARGS="$ARGS --agnostic-baker $AGNOSTIC_BAKER"
fi

# Execute the command inside the container
docker exec \
  -e "PREFIX=$PREFIX" \
  -e "DISTRIBUTION=$DISTRIBUTION" \
  -e "RELEASE=$RELEASE" \
  -e "CI_COMMIT_REF_NAME=$CI_COMMIT_REF_NAME" \
  -e "CI_COMMIT_TAG=$CI_COMMIT_TAG" \
  -e "CI_COMMIT_REF_PROTECTED=$CI_COMMIT_REF_PROTECTED" \
  -e "CI_PROJECT_NAMESPACE=$CI_PROJECT_NAMESPACE" \
  -e "CI_COMMIT_SHORT_SHA=$CI_COMMIT_SHORT_SHA" \
  -e "GCP_LINUX_PACKAGES_BUCKET=$GCP_LINUX_PACKAGES_BUCKET" \
  -i systemd \
  /bin/sh -c "$TESTFILE $DISTRIBUTION $RELEASE $ARGS"

# Capture exit status
EXIT=$?

killall screen
docker rm -f systemd || true

# Exit with the captured status
exit $EXIT
