#!/bin/sh

error=''
die_if_error() {
  if [ -n "${error}" ]; then
    echo 'ERROR: found different version(s) than expected'
    echo 'Checkout Dockerfiles and verify which version is installed'
    echo '⚠️ Alpine packages are often updated and older versions are removed from repositories'
    exit 1
  fi
}

check_version() {
  if [ "${2}" != "${3}" ]; then
    echo "${1}: expected ${3}, got ${2} ❌"
    error='true'
  else
    echo "${1}: ${2} ✅"
  fi
}
