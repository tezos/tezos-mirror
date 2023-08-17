#!/bin/sh

# Usage: call this script with the patch file as first argument and the
# directories of the protocols on which it should be applied as following args.

#  Examples:
#  devtools/patchs/apply_proto_patch.sh  scripts/yes-stresstest.patch src/proto_018_Proxford  src/proto_017_PtNairob
#  devtools/patchs/apply_proto_patch.sh  scripts/yes-stresstest.patch src/proto_01[5-9]_*


WORKING_DIR=$(pwd)
# Patch file, with absolute path 
PATCH=$WORKING_DIR/$1
if [ ! -f "$PATCH" ];then
    echo "Patch file $PATCH doesn't exist"
    exit 1
fi

# Removing patch file from arg list, only proto directory should remain.
shift

# For each proto directory, apply the patch.
for proto in "$@"; do
    echo "applying patch $PATCH to proto $proto"
    cd "$proto" || exit 1
    patch -p3 < "${PATCH}"
    cd "$WORKING_DIR"  || exit 1
done
