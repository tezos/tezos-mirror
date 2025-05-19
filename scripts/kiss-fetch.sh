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

# make sure this file exists and writable and readable by all users
touch /tmp/kiss.log
chmod a+rw /tmp/kiss.log

# shellcheck disable=SC2086
if curl --output /dev/null --silent --head --fail "$KISSCACHE"; then
  # Use curl to fetch the mangled URL
  curl -L $arguments $mangled_url

  # shellcheck disable=SC2181
  if [ $? = 0 ]; then
    echo "Kisscache hit: curl -L $arguments $mangled_url" >> /tmp/kiss.log
  else
    echo "Kisscache error: curl -L $arguments $mangled_url" >> /tmp/kiss.log
    echo "Kisscache error: curl -L $arguments $mangled_url"
    exit 1
  fi
else
  # Use curl to fetch the original URL
  curl -L $arguments $original_url

  # shellcheck disable=SC2181
  if [ $? = 0 ]; then
    echo "Direct download succeded $original_url" >> /tmp/kiss.log
  else
    echo "Direct download failed $original_url" >> /tmp/kiss.log
    echo "Direct download failed $original_url"
    exit 1
  fi
fi
