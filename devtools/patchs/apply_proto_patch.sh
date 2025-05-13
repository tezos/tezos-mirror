#!/bin/sh

Usage='Usage: apply_proto_patch.sh [--path src/proto_alpha] [-c|--commit] [patch] proto_dir...

Call this script with the patch file or commit as first argument and the
directories of the protocols on which it should be applied as following args.

If -c option is passed, a commit is created, prepending the name of the protocol
to the title of the commit.
The commit message body is replaced by a reference to the original commit.

If --path is passed, only changes from this path will be considered.

Examples:

  devtools/patchs/apply_proto_patch.sh  scripts/yes-stresstest.patch src/proto_018_Proxford  src/proto_017_PtNairob

  devtools/patchs/apply_proto_patch.sh  scripts/yes-stresstest.patch src/proto_01[5-9]_*

  devtools/patchs/apply_proto_patch.sh  HEAD~1 src/proto_01[5-9]_*

  devtools/patchs/apply_proto_patch.sh  --commit HEAD~1 src/proto_01[5-9]_*

  devtools/patchs/apply_proto_patch.sh  -c ab6c823252 src/proto_018_*

  devtools/patchs/apply_proto_patch.sh --path src/proto_019_* -c ab6c823252 src/proto_018_*

Note that it also works on documentation:

  devtools/patchs/apply_proto_patch.sh Some_patch_of_alpha_documentation doc/oxford

'

if [ -z "$1" ]; then
  echo "$Usage"
  exit 0
fi

COMMITING=false
CHANGES_PATH=""

while [ $# -gt 0 ]; do
  case "$1" in
  --commit | -c)
    COMMITING=true
    shift
    ;;
  --path)
    if [ -n "$2" ]; then
      case "$2" in
      /*) CHANGES_PATH="-- $2" ;;
      *) CHANGES_PATH="-- ../../$2" ;;
      esac
      shift 2
    else
      echo "Error: --path requires a value"
      exit 1
    fi
    ;;
  *)
    break
    ;;
  esac
done

WORKING_DIR=$(pwd)
PATCH_NAME=$1

apply() {
  proto_name=$1
  if git cat-file commit "$PATCH_NAME" > /dev/null; then
    # given name is a valid commit hash or alias according to git

    # getting initial commit hash
    COMMIT_HASH=$(git show --quiet --pretty="%H" "$PATCH_NAME")
    # apply the initial commit locally (assumes we `cd` to the right directory)
    PATCH="git format-patch --stdout $PATCH_NAME~1..$PATCH_NAME $CHANGES_PATH | patch -p3"
    # getting initial commit title
    COMMIT_TITLE=$(git log -1 --pretty=format:%s "$PATCH_NAME")
    echo "applying  $PATCH_NAME ($COMMIT_HASH - $COMMIT_TITLE)"
    echo "$PATCH"
    eval "$PATCH"
    if "$COMMITING"; then
      # adding all files touched by the initial commit
      # shellcheck disable=SC2046
      git add $(git show --name-only --pretty="" "$PATCH_NAME" "$CHANGES_PATH" | cut -d'/' -f3-)
      # committing the changes with a new message
      git commit -m"$proto_name/$COMMIT_TITLE

Porting to proto $proto_name $COMMIT_HASH - $COMMIT_TITLE
"
    fi
  else
    # given name is *not* a valid commit hash or alias according to git
    # assuming it is a patch file

    PATCH_FILE="$WORKING_DIR/$PATCH_NAME"
    # Patch file, with absolute path
    if [ ! -f "$PATCH_FILE" ]; then
      echo "Patch file $PATCH_NAME doesn't exist"
      exit 1
    fi
    PATCH="patch -p3 < $PATCH_FILE"

    if "$COMMITING"; then
      # adding all files touched by the initial commit
      # shellcheck disable=SC2046
      git add $(git apply --numstat "$PATCH_NAME" | awk '{ print $3 }')
      # committing the changes with a new message
      git commit -m"$proto_name/$PATCH_NAME"
    fi
  fi
}
# Removing patch file from arg list, only proto directory should remain.
shift

# For each proto directory, apply the patch.
for proto in "$@"; do
  proto_name=$(basename "$proto" | sed s'/proto_//')
  cd "$proto" || exit 1
  apply "$proto_name"
  cd "$WORKING_DIR" || exit 1
done
