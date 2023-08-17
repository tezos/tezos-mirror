#!/bin/bash

# Usage: call this script with the patch file as first argument, its initial
# environment number and the environments numbers on which it should be applied
# as following args. 

#  Example of using add-hack-module.patch, which was written for env v3,
#  on envs 9 and 10:
#  devtools/patchs/apply_env_patch.sh  devtools/protocol-print/add-hack-module.patch 3 9 10


# Hack to get tezos directory
TEZOS_DIR=$(dirname "$0")/../..

# Patch file, with absolute path 
PATCH=$(pwd)/$1
if [ ! -f "$PATCH" ];then
    echo "Patch file $PATCH doesn't exist"
    exit 1
fi
PATCH_BASE=$(basename "$PATCH")
PATCHS_PATH=$(dirname "$PATCH")

# environement on witch the patch was build
BASE_ENV_NUMBER=$2

# Removing patch file and base env from arg list, only target env numbers should remain.
shift 2

# For each env directory, translate the patch and apply it.
for env in "$@" ; do
    if [[ $env =~ [1-9][0-9]* ]]; then
        echo "applying patch $PATCH replacing environment version $BASE_ENV_NUMBER by $env"
        sed "s/$BASE_ENV_NUMBER/$env/g" "$PATCH" > "$PATCHS_PATH/${env}_${PATCH_BASE}"
        cd "$TEZOS_DIR" || exit 1;
        patch -p1 < "${PATCHS_PATH}/${env}_${PATCH_BASE}"
        cd - || exit 1
    else
        echo "ignoring $env; not an environment number"
    fi
done
