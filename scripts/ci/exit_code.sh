#!/bin/sh

"$@"

CODE=$?
echo "$1 terminated with exit code ${CODE}"
exit ${CODE}
