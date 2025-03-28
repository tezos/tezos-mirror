#!/bin/sh
if [ "$*" = "debug" ]; then
  /bin/sh
else
  ./scripts/parabench.sh -o "$OUTPUT" "$@"
fi
