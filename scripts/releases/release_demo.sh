#!/usr/bin/env bash

set -e

# Release Demo Script
# This script creates a series of demo releases for testing the release page functionality
# It manages both Git tags and GitLab releases

# Parse command line arguments
DRY_RUN=false
for arg in "$@"; do
  case $arg in
  --dry-run)
    DRY_RUN=true
    shift
    ;;
  --help | -h)
    echo "Usage: $0 [--dry-run] [--help]"
    echo ""
    echo "Options:"
    echo "  --dry-run     Show what would be done without executing"
    echo "  --help, -h    Show this help message"
    echo ""
    echo "This script creates demo releases for testing the release page functionality."
    exit 0
    ;;
  *)
    echo "Unknown option: $arg"
    echo "Use --help for usage information"
    exit 1
    ;;
  esac
done

echo "=== Release Demo Script ==="
if [ "$DRY_RUN" = true ]; then
  echo "🔍 DRY RUN: Showing what would be done without executing"
else
  echo "This script will create demo releases for testing purposes"
  echo "WARNING: This will delete existing demo releases matching the pattern 111[0-9].*"
fi
echo ""

# Check if we're in a git repository
if ! git rev-parse --git-dir > /dev/null 2>&1; then
  echo "Error: Not in a git repository"
  exit 1
fi

# Function to delete GitLab releases (requires gitlab CLI or API)
delete_gitlab_release() {
  local tag="$1"
  echo "  Attempting to delete GitLab release: $tag"

  if [ "$DRY_RUN" = true ]; then
    echo "    [DRY RUN] Would delete GitLab release: $tag"
    return
  fi

  # Try using glab CLI if available
  if command -v glab &> /dev/null; then
    glab release delete "$tag" --yes 2> /dev/null || echo "    GitLab release $tag not found or already deleted"
  else
    echo "    glab CLI not found, skipping GitLab release deletion for $tag"
  fi
}

# Function to delete git tags and GitLab releases matching a pattern
cleanup_releases() {
  local pattern="$1"
  local description="$2"

  echo "Cleaning up $description releases matching pattern: $pattern"

  # Find and delete matching tags
  tags_to_delete=$(git tag -l | grep -E "^${pattern}$" || true)

  if [ -n "$tags_to_delete" ]; then
    echo "  Found tags to delete:"
    # shellcheck disable=SC2001
    echo "$tags_to_delete" | sed 's/^/    /'

    for tag in $tags_to_delete; do
      echo "  Deleting tag: $tag"
      if [ "$DRY_RUN" = true ]; then
        echo "    [DRY RUN] Would delete local tag: $tag"
        echo "    [DRY RUN] Would delete remote tag: $tag"
      else
        git tag -d "$tag" 2> /dev/null || echo "    Tag $tag not found locally"

        # Delete remote tag if it exists
        git push nomadic":refs/tags/$tag" 2> /dev/null || echo "    Remote tag $tag not found"
      fi

      # Delete GitLab release
      delete_gitlab_release "$tag"
    done
  else
    echo "  No tags found matching pattern $pattern"
  fi
  echo ""
}

# Function to create a dummy commit
create_dummy_commit() {
  local message="$1"
  echo "  Creating dummy commit: $message"

  if [ "$DRY_RUN" = true ]; then
    echo "    [DRY RUN] Would create dummy commit with message: $message"
    echo "    [DRY RUN] Would add entry to .demo_releases_log"
    return
  fi

  # Create or modify a dummy file
  echo "$(date): $message" >> ".demo_releases_log"
  git add ".demo_releases_log"
  git commit -m "$message" --quiet
}

# Function to create a tag
create_tag() {
  local tag="$1"
  local message="$2"

  echo "Creating tag: $tag"

  if [ "$DRY_RUN" = true ]; then
    echo "  [DRY RUN] Would create tag: $tag with message: $message"
    echo "  [DRY RUN] Would push tag to remote"
    return
  fi

  git tag -a "$tag" -m "$message"

  # Push tag to remote
  git push nomadic "$tag" || echo "  Warning: Could not push tag to remote"

  echo "  Tag $tag created successfully"
}

echo "Step 1: Cleaning up existing demo releases..."

# Clean up existing demo releases
cleanup_releases "octez-v111[0-9]\.[0-9]+(-rc[0-9]+)?" "Octez"
cleanup_releases "octez-smart-rollup-node-v111[0-9]\.[0-9]+(-rc[0-9]+)?" "Octez Smart Rollup Node"
cleanup_releases "teztale-v111[0-9]\.[0-9]+(-rc[0-9]+)?" "Teztale"
cleanup_releases "grafazos-v111[0-9]\.[0-9]+(-rc[0-9]+)?" "Grafazos"

echo "Step 2: Creating demo releases..."
echo "Waiting for 1 minutes"
sleep 60

# Create the demo release sequence
echo "Creating octez-v1111.0~rc1..."
create_dummy_commit "Prepare for Octez v1111.0-rc1 release"
create_tag "octez-v1111.0-rc1" "Octez v1111.0 Release Candidate 1"

echo "Wait for 10 seconds"
sleep 10

create_dummy_commit "Fix issues for rc2"
create_tag "octez-v1111.0-rc2" "Octez v1111.0 Release Candidate 2"

echo "Wait for 10 seconds"
sleep 10

create_dummy_commit "Final preparations for v1111.0"
create_tag "octez-v1111.0" "Octez v1111.0 Stable Release"

echo "Wait for 10 seconds"
sleep 10

create_dummy_commit "Bug fixes for v1111.1"
create_tag "octez-v1111.1" "Octez v1111.1 Patch Release"

echo "Wait for 10 seconds"
sleep 10

create_dummy_commit "Teztale improvements"
create_tag "teztale-v1111.1" "Teztale v1111.1 Release"

echo "Wait for 10 seconds"
sleep 10

create_dummy_commit "More teztale features"
create_tag "teztale-v1111.2" "Teztale v1111.2 Release"

echo "Wait for 10 seconds"
sleep 10

create_dummy_commit "Smart rollup node improvements"
create_tag "octez-smart-rollup-node-v1111.1" "Octez Smart Rollup Node v1111.1"

echo "Wait for 10 seconds"
sleep 10

create_dummy_commit "Grafazos release"
create_tag "grafazos-v1111.1" "Grafazos v1111.1 Release"

echo "Wait for 10 seconds"
sleep 10

create_dummy_commit "Smart rollup node updates"
create_tag "octez-smart-rollup-node-v1111.2" "Octez Smart Rollup Node v1111.2"

echo "Wait for 10 seconds"
sleep 10

create_dummy_commit "Prepare for major release v1112.0"
create_tag "octez-v1112.0" "Octez v1112.0 Major Release"

echo ""
if [ "$DRY_RUN" = true ]; then
  echo "=== Dry Run Complete ==="
  echo ""
  echo "Would have created the following demo releases:"
else
  echo "=== Demo Release Creation Complete ==="
  echo ""
  echo "Created the following demo releases:"
fi
echo "  - octez-v1111.0~rc1"
echo "  - octez-v1111.0~rc2"
echo "  - octez-v1111.0"
echo "  - octez-v1111.1"
echo "  - teztale-v1111.1"
echo "  - teztale-v1111.2"
echo "  - octez-smart-rollup-node-v1111.1"
echo "  - grafazos-v1111.1"
echo "  - octez-smart-rollup-node-v1111.2"
echo "  - octez-v1112.0"
echo ""
if [ "$DRY_RUN" = true ]; then
  echo "To actually execute these operations, run the script without --dry-run."
else
  echo "You can now test the release page functionality with these demo releases."
  echo "To clean up, run this script again - it will delete the demo releases first."
  echo ""
  echo "Note: A .demo_releases_log file has been created to track the dummy commits."
fi
