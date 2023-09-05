#!/bin/sh

# create directory, otherwise docker will create as root
if [ ! -d output ]; then
    echo "Creating output directory"
    mkdir output
fi

# delete container if it exists
if [ "$(docker ps -a -q -f name='^evm-benchmark$' | wc -c)" -ne 0 ]; then
    echo "Deleting old container $(docker rm -f evm-benchmark)"
fi

echo "Launching new container:"
docker run --name evm-benchmark -d --mount type=bind,src="$(pwd)/output",target=/home/tezos/output evm-benchmark