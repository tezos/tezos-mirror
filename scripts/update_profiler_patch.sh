#!/bin/sh

for arg in "$@"; do
  case $arg in
  "--resume")
    RESUME='--resume'
    ;;
  "--check-patch-only")
    CHECKPATCHONLY='--check-patch-only'
    ;;
  "--no-check")
    NOCHECK='--no-check'
    ;;
  esac
done

# Check for uncommitted files, this will ensure `git commit -a` doesn't add
# unwanted files to the patch.
UNCOMMITED=$(git status | grep "Changes")

# We can have uncommitted file if we are resuming, as these files can be results
# from resolving the conflicts
if [ "$UNCOMMITED" != "" ] && [ "$RESUME" = "" ]; then
  echo "Some files are not committed, the script must start from a clean branch"
  exit 2
fi

# Apply the patch with the 3-way merge, so that conflicts are explicitly added
# to the files. Only when not resuming, otherwise it would apply them twice.
if [ "$RESUME" = "" ]; then
  git apply -3 scripts/profile_alpha.patch
fi

CONFLICTS=$(git status | grep "Unmerged paths")

if [ "$CONFLICTS" != "" ]; then

  echo "Please fix conflicts and resume with $0 --resume."
  echo "You should also first try with '--resume --check-patch-only' to ensure the patch is correct and compiling."

  exit 2
fi

# Regenerate protocol environment files and check the patch is still valid.
if [ "$NOCHECK" = "" ]; then
  TEZOS_PPX_PROFILER=t dune build @src/proto_alpha/check
  if [ ! $? ]; then
    echo "The profiler patch is not compiling, please make the change locally and retry with '--resume'."
    exit 2
  fi
fi

if [ "$CHECKPATCHONLY" = "" ]; then

  # Now commit the patch, generate a patch file, revert the commit then finally
  # commit the newly generated patch file.

  git commit -a -m "Profiler: patch proto_alpha"

  OUTPUT_FILE=$(git format-patch -n HEAD^)

  git reset --hard HEAD^

  mv "$OUTPUT_FILE" scripts/profile_alpha.patch

  git add scripts/profile_alpha.patch

  git commit -m "Scripts: regenerate protocol alpha patches"

  # If not resuming after a conflict, this means we are only trying to check the patch
elif [ "$RESUME" = "" ]; then
  git reset --hard HEAD
fi
