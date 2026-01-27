#!/usr/bin/env bash
# Pre-commit hook to ensure commits are signed
# This script first checks if git commit signing is properly configured
# and then verifies that all commits since the branch creation are signed.

set -e

# Check if user.signingkey is configured
if ! git config --get user.signingkey > /dev/null 2>&1; then
  echo "❌ Error: git commit signing key not configured."
  echo "Please configure your signing key:"
  echo "  git config user.signingkey <your-key-id>"
  exit 4
fi

# Check if commit.gpgsign is enabled
if ! git config --get commit.gpgsign > /dev/null 2>&1; then
  echo "❌ Error: commit.gpgsign is not set."
  echo "Please enable commit signing for commits:"
  echo "  git config commit.gpgsign true"
  exit 5
fi

# Verify that commit.gpgsign is set to true
gpgsign=$(git config --get commit.gpgsign)
if [ "$gpgsign" != "true" ]; then
  echo "❌ Error: commit.gpgsign is set to '$gpgsign', but must be 'true'."
  echo "Please enable commit signing:"
  echo "  git config commit.gpgsign true"
  exit 6
fi

# ✓ commit signing is properly configured
# Check if all commits since branch creation are signed
base_branch=$(git merge-base HEAD "$(git for-each-ref --format='%(upstream:short)' "$(git symbolic-ref -q HEAD)")")
if [ -z "$base_branch" ]; then
  echo "⚠️  Warning: Could not determine base branch. Skipping commit signature check."
  exit 0
fi

commit_range="$base_branch..HEAD"
total_count=$(git log "$commit_range" --pretty=oneline | wc -l | tr -d ' ')

if [ "$total_count" -eq 0 ]; then
  echo "✓ No new commits to check for signatures."
  exit 0
fi

# Check which commits are not signed
unsigned_commits=()
for commit in $(git rev-list --no-merges "$base_branch..HEAD"); do
  if ! git verify-commit "$commit" > /dev/null 2>&1; then
    unsigned_commits+=("$commit")
  fi
done

if [ ${#unsigned_commits[@]} -gt 0 ]; then
  echo "❌ Error: Found ${#unsigned_commits[@]} unsigned commits:"
  for commit in "${unsigned_commits[@]}"; do
    echo "  - $commit $(git log -1 --pretty=format:'%s' "$commit")"
  done
  echo "Please ensure all commits are signed. "
  echo "  Use \`git commit --amend --no-edit -S\` to sign the latest commit"
  exit 7
fi

echo "✓ All $total_count commits since branch creation are signed."
exit 0
