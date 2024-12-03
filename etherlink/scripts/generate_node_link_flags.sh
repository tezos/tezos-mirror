#!/usr/bin/env bash

if [ -f "/etc/os-release" ]; then
  # shellcheck source=/dev/null
  source /etc/os-release
fi

case "$NAME" in
"Alpine Linux")
  # On Alpine Linux, the stack size for threads is rather small (128KBytes on
  # recent versions). Since we are executing kernels natively, we actually need
  # more to deal with recursive calls of solidity contracts.
  #
  # Fortunately, Alpine Linux allows us to set the default thread stack size at
  # link time.
  #
  # See https://wiki.musl-libc.org/functional-differences-from-glibc.html#Thread-stack-size
  echo "(-ccopt -Wl,-z,stack-size=4099072)"
  ;;
*)
  echo "()"
  ;;
esac
