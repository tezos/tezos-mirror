#!/bin/sh

## See README.md for the "manual instructions"

# Check for uncommitted files, this will ensure `git commit -a` doesn't add
# unwanted files to the patch.
UNCOMMITED=$(git status | grep "Changes")

if [ "$UNCOMMITED" != "" ] && [ "$1" != "--resume" ]; then
  echo "Some files are not committed, the script must start from a clean branch"
  exit 2
fi

# Apply the patch with the 3-way merge, so that conflicts are explicitly added
# to the files.
if [ "$1" != "--resume" ]; then
  git apply -3 scripts/profile_alpha.patch
fi

CONFLICTS=$(git status | grep "Unmerged paths")

if [ "$CONFLICTS" != "" ]; then
  echo "Please fix conflicts and resume with $0 --resume"
  exit 2
fi

git commit -a -m "Profiler: patch proto_alpha"

OUTPUT_FILE=$(git format-patch -n HEAD^)

git reset --hard HEAD^

mv "$OUTPUT_FILE" scripts/profile_alpha.patch

git add scripts/profile_alpha.patch

git commit -m "Scripts: regenerate protocol alpha patches"
