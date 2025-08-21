#!/bin/bash

# default values
OUTPUT=$(pwd)/output
IMAGE_NAME=evm-benchmark
CONTAINER_NAME=evm-benchmark
FORCE=false
INTERACTIVE=false

# usage and help
function usage() {
  echo "Usage: $(basename "$0") [-h] [-o OUTPUT_DIRECTORY] [-i IMAGE_NAME] \
[-n CONTAINER_NAME] -- [ARGS_PARALLEL] -- [ARGS_BENCHMARK]" 1>&2
}
function print_help {
  # display help
  usage
  echo 1>&2
  echo "Launches the evm-benchmark docker image. Additional arguments \
[ARGS_PARALLEL] are passed on the parallel execution script (eg nb of jobs), \
and [ARGS_BENCHMARK] are passed on to the benchmark script. \
If no additionnal arguments are provided, the default from the Dockerfile apply." 1>&2
  echo 1>&2
  echo "options:" 1>&2
  echo "-o      specify output directory (default ./$(basename "$OUTPUT"))" 1>&2
  echo "-i      specify image name (default $IMAGE_NAME)" 1>&2
  echo "-n      specify container name (default $CONTAINER_NAME)" 1>&2
  echo "-f      force deletion of container even if running" 1>&2
  echo "-h      this help" 1>&2
  echo "-d      launch image in interactive mode (eg for debugging)" 1>&2
}

# parse options and flags
while getopts "dhfi:n:o:" options; do
  case "${options}" in
  d)
    INTERACTIVE=true
    ;;
  i)
    IMAGE_NAME=${OPTARG}
    ;;
  n)
    CONTAINER_NAME=${OPTARG}
    ;;
  o)
    OUTPUT=${OPTARG}
    ;;
  f)
    FORCE=true
    ;;
  :) # If expected argument omitted:
    print_help
    exit 1
    ;;
  *) # If unknown (any other) option:
    print_help
    exit 1
    ;;
  esac
done

# discard parsed options and flag
shift $((OPTIND - 1))

# abort execution if container exist
if [ "$(docker container inspect -f '{{.State.Status}}' "$CONTAINER_NAME")" = "running" ]; then
  echo "Container $CONTAINER_NAME is running"
  if ! $FORCE; then
    echo "abort"
    exit 1
  fi
fi

# create directory, otherwise docker will create as root
if [ ! -d "$OUTPUT" ]; then
  echo "Creating output directory: $OUTPUT"
  mkdir "$OUTPUT"
fi

# delete container if it exists
if [ "$(docker ps -a -q -f name=^"$CONTAINER_NAME"$ | wc -c)" -ne 0 ]; then
  echo "Deleting old container $(docker rm -f "$CONTAINER_NAME")"
fi

echo "Launching new container:"
# TODO: fix log-driver issue on benchmark server
if ! $INTERACTIVE; then
  docker run --log-driver none --name "$CONTAINER_NAME" -d --mount type=bind,src="$OUTPUT",target=/home/tezos/output "$IMAGE_NAME" "$@"
else
  echo "Launching in interactive mode"
  docker run --log-driver none --name "$CONTAINER_NAME" -it --mount type=bind,src="$OUTPUT",target=/home/tezos/output "$IMAGE_NAME" debug
fi
