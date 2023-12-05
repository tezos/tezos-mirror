#!/bin/sh

Usage='Usage: apply_proto_patch.sh [patch] proto_dir...

Call this script with the patch file or commit as first argument and the
directories of the protocols on which it should be applied as following args.


Examples:

  devtools/patchs/apply_proto_patch.sh  scripts/yes-stresstest.patch src/proto_018_Proxford  src/proto_017_PtNairob

  devtools/patchs/apply_proto_patch.sh  scripts/yes-stresstest.patch src/proto_01[5-9]_*

  devtools/patchs/apply_proto_patch.sh  HEAD~1 src/proto_01[5-9]_*

  devtools/patchs/apply_proto_patch.sh  ab6c823252 src/proto_018_*

Note that it also works on documentation:

  devtools/patchs/apply_proto_patch.sh Some_patch_of_alpha_documentation doc/oxford

'

if [ -z "$1" ]; then
    echo "$Usage";
    exit 0
fi

if git cat-file commit "$1" > /dev/null; then
    PATCH_NAME=$1
    PATCH="git format-patch  --stdout  $PATCH_NAME~1..$PATCH_NAME | patch -p3"
else
    WORKING_DIR=$(pwd)
    PATCH_NAME="$WORKING_DIR/$1"
    # Patch file, with absolute path
    PATCH="patch -p3 < $PATCH_NAME"
    if [ ! -f "$PATCH_NAME" ];then
        echo "Patch file $PATCH_NAME doesn't exist"
        exit 1
    fi
fi

# Removing patch file from arg list, only proto directory should remain.
shift

# For each proto directory, apply the patch.
for proto in "$@"; do
    echo "applying patch $PATCH_NAME to proto $proto"
    cd "$proto" || exit 1
    echo "$PATCH"
    eval "$PATCH"
    cd "$WORKING_DIR"  || exit 1
done
