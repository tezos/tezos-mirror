#!/bin/bash

# This script is a wrapper for opam to use
# a kisscache server, for example located at
# http://localhost:8001/api/v1/fetch/?url=

# kisscache: https://gitlab.com/Linaro/kisscache

# To enable this wrapper
#
# export OPAMFETCH="scripts/kiss-fetch.sh"
#
# before invoking opam
#
# Use the evn var KISSCACHE to specify a remote kisscache server.

KISSCACHE=${KISSCACHE:-http://localhost:8001}

# Original URL passed by opam
original_url=${*: -1}
arguments=${*:1:$#-1}

# Mangle the URL by prepending the base URL
encoded_url=$(echo "$original_url" | sed 's/ /%20/g; s/?/%3F/g; s/&/%26/g; s/=/%3D/g; s/#/%23/g; s/</%3C/g; s/>/%3E/g; s/"/%22/g; s/\//%2F/g; s/:/%3A/g')
mangled_url="${KISSCACHE}/api/v1/fetch?url=${encoded_url}"

# shellcheck disable=SC2086
if curl --output /dev/null --silent --head --fail "$KISSCACHE"; then
  # Use curl to fetch the mangled URL
  echo "curl -L $arguments $mangled_url" >> /tmp/kiss.log
  curl -L $arguments $mangled_url
else
  echo "Kisscache server unreachable, falling back to fetch using mangled URL" >> /tmp/kiss.log
  curl -L $arguments $mangled_url
fi
