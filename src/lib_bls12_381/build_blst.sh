#!/bin/sh -e

# Modifying `libblst/build.sh` is avoided because we want to keep the fork
# clean.

# If you update the build.sh flags, you must also update
# .github/workflows/build-blst-on-macos accordingly

# Adding -Wno-missing-braces for clang-11 (and also should be for clang-10, but clang 10
# is not officially supported). See .github/workflows/build-blst-on-macos for the reason

# Use BLST_PORTABLE environment variable to overwrite the check in
# libblst/build.sh to use ADX instructions. build.sh uses /proc/cpuinfo to
# decide to use ADX or not. Useful if you build binaries for archs not
# supporting ADX on a arch supporting ADX.

cd libblst
chmod +x build.sh
# If the user does not want to build with assembly optimisations, regardless
# the platform
if [ -n "${BLST_PORTABLE}" ]; then
  echo "(-D__BLST_PORTABLE__)" > ../c_flags_blst.sexp
  ./build.sh -shared -Wno-missing-braces -D__BLST_PORTABLE__
# If the architecture is x86_64 or arm64, we build with the default flags.
elif [ "$(uname -m)" = "x86_64" ] || [ "$(uname -m)" = "arm64" ] || [ "$(uname -m)" = "amd64" ] || [ "$(uname -m)" = "aarch64" ]; then
  echo "()" > ../c_flags_blst.sexp
  ./build.sh -shared -Wno-missing-braces
# By default, we build without any assembly instruction and rely on the C
# compiler
else
  echo "()" > ../c_flags_blst.sexp
  ./build.sh -shared -Wno-missing-braces -D__BLST_NO_ASM__
fi
