#!/usr/bin/env bash

#------------------------------------------------------------------------------
# This script automates the process of backporting a GitLab merge request
# to a specified release branch.
#
# It performs the following steps:
#
# 1. Fetches commit information from the specified merge request using
#    the GitLab API
# 2. Fetches merge request details (title, description) to populate the new
#    backport MR
# 3. Checks out the target release branch
# 4. Creates a new branch for the backport
# 5. Cherry-picks the commits from the original MR in the correct order
# 6. Pushes the new branch to the remote repository
# 7. Generates and opens a URL to create a new merge request with pre-filled
#    title and description
#
# Dependencies
# - git: For version control operations
# - curl: For making API requests to GitLab
# - jq: For parsing JSON responses from the GitLab API
#
# The script supports resuming after a cherry-pick conflict has been resolved
# manually.
#------------------------------------------------------------------------------

set -e
set -u

# Self-relocation to a temporary directory to prevent modification by git
# checkout while the script is running.
_IN_TEMP=${_IN_TEMP:-}
if [ -z "$_IN_TEMP" ]; then
  export _IN_TEMP=1
  tmp_dir=$(mktemp -d)
  cp "$0" "$tmp_dir/"
  exec "$tmp_dir/$(basename "$0")" "$@"
fi

#------------------------------------------------------------------------------
# Global variables
#------------------------------------------------------------------------------

remote="${REMOTE:-origin}"
resume=false
VERSION=""
MERGE_REQUEST=""
BASE=""
BRANCH_PREFIX=""
ID=3836952 # Gitlab id for tezos/tezos repository

SCRIPT_NAME=$0
ARGS=$*

#------------------------------------------------------------------------------
# Helper functions for logging and error handling
#------------------------------------------------------------------------------

usage() {
  echo "Usage: $(basename "$0") [options]"
  echo
  echo "Backport commits from a GitLab merge request to a release branch and open an MR."
  echo
  echo "Options:"
  echo "  --version <version>      (Required) The version to backport to (e.g., v24)."
  echo "  --mr <mr_number>         (Required) The merge request number to backport."
  echo "  --base <branch>          The base branch to backport onto. Defaults to '$remote/<V>-release' where <V> is the version provided with --version."
  echo "  --branch-prefix <prefix> The prefix for the backport branch. Defaults to your git gitlab.user."
  echo "  --resume-after-conflict  Resume the script after resolving cherry-pick conflicts."
  echo "  -h, --help               Display this help message."
  echo
}

function step {
  echo -ne "[ ] \033[1m$*\033[0m "
}

function end_step {
  echo -ne "\r[\033[0;32m●\033[0m"
  echo
}

function step_name {
  echo
  echo -e "<⚙> \033[1m$*\033[0m"
}

function end_step_name {
  echo -e "[\033[0;32m●\033[0m] \033[1m$*\033[0m"
  echo
}

function fail {
  echo -e "\n\n\033[0;31mError: $*\033[0m" >&2
  exit 1
}

#------------------------------------------------------------------------------
# Arguments parsing
#------------------------------------------------------------------------------

parse_arguments() {
  if [ $# -eq 0 ]; then
    usage
    exit 0
  fi

  while [[ $# -gt 0 ]]; do
    key="$1"
    case $key in
    -h | --help)
      usage
      exit 0
      ;;
    --resume-after-conflict)
      resume=true
      shift
      ;;
    --version)
      VERSION="$2"
      shift
      shift
      ;;
    --mr)
      MERGE_REQUEST="$2"
      shift
      shift
      ;;
    --base)
      BASE="$2"
      shift
      shift
      ;;
    --branch-prefix)
      BRANCH_PREFIX="$2"
      shift
      shift
      ;;
    *)
      echo "Error: Unknown option: $1" >&2
      usage
      exit 1
      ;;
    esac
  done

  # Check for required arguments
  if [ -z "$VERSION" ] || [ -z "$MERGE_REQUEST" ]; then
    echo "Error: --version and --mr are required." >&2
    usage
    exit 1
  fi

  # Set defaults for optional arguments
  if [ -z "$BASE" ]; then
    BASE="origin/$VERSION-release"
  fi

  if [ -z "$BRANCH_PREFIX" ]; then
    BRANCH_PREFIX=$(git config --get gitlab.user || true)
    if [ -z "$BRANCH_PREFIX" ]; then
      fail "Could not determine branch prefix. Please set it with --branch-prefix or configure git gitlab.user."
    fi
  fi

  target="$BASE"
  branch="$BRANCH_PREFIX@backport_$MERGE_REQUEST"
}

#------------------------------------------------------------------------------
# Core script functions
#------------------------------------------------------------------------------

fetch_mr() {
  step "Fetching MR commits"
  commits=$(
    curl -s --request GET \
      "https://gitlab.com/api/v4/projects/$ID/merge_requests/$MERGE_REQUEST/commits"
  )
  end_step

  mapfile -t hashes < <(echo "$commits" | jq -r 'reverse | .[].id')

  ((${#hashes[@]})) || fail No commits to backport

  commit_list="$(echo "$commits" | jq -r 'reverse | .[] | "- \(.id) \(.title)"')"
  echo "$commit_list"

  step "Fetching MR information"
  mrjson=$(
    curl -s --request GET \
      "https://gitlab.com/api/v4/projects/$ID/merge_requests/$MERGE_REQUEST"
  )
  mr_title="$(echo "$mrjson" | jq -r .title)"
  echo -n " $mr_title"
  end_step

  mr_desc="$(echo "$mrjson" | jq -r .description)"

  title="Backport $VERSION !$MERGE_REQUEST: $mr_title"

  description="Backport !$MERGE_REQUEST to $VERSION: $mr_title

## Commits

$commit_list

# Original MR description

$mr_desc
"
}

# if [ "$resume" = false ]; then
prepare_backport_branch() {
  step "Fetching from remotes"
  git fetch "$remote" &> /dev/null
  end_step

  if [ -n "$BASE" ]; then
    step_name "Checkout $BASE"
    git checkout --detach "$BASE"
    end_step_name "Checked out $BASE"
  fi

  step_name "Creating branch $branch for the backport"
  git switch -c "$branch" || git switch "$branch"
  end_step_name "Branch $branch created"
}

cherry_pick_commits() {
  step_name "Cherry picking commits"
  git cherry-pick "${hashes[@]}" || fail "Cherry-pick failed, resolve, run git cherry-pick --continue and then run the script with $SCRIPT_NAME $ARGS --resume-after-conflict"
  end_step_name "Commits successfully cherry picked"
}

resume_backport() {
  echo "Resuming backport of $MERGE_REQUEST"
  current_branch=$(git branch --show-current)
  if [ "$current_branch" != "$branch" ]; then
    fail "Not on branch $branch. Current branch is $current_branch."
  fi
  if [ -n "$(git ls-files -u)" ]; then
    fail "Unresolved conflicts detected. Please resolve them before resuming."
  fi
  if [ -f "$(git rev-parse --git-dir)/CHERRY_PICK_HEAD" ]; then
    fail "A cherry-pick is still in progress. Please run 'git cherry-pick --continue' or 'git cherry-pick --abort'."
  fi
}

push_backport_branch() {
  step_name "Pushing backport branch to remote $remote"
  git push --force-with-lease --set-upstream "$remote" "$branch" || fail "Could not push branch"
  end_step_name "Pushed backport branch"
}

create_merge_request() {
  url_branch0=$(echo -n "merge_request[source_branch]" | jq -sRr @uri)
  url_branch1=$(echo -n "$branch" | jq -sRr @uri)
  url_target0=$(echo -n "merge_request[target_branch]" | jq -sRr @uri)
  url_target1=$(echo -n "${target#origin/}" | jq -sRr @uri)
  url_title0=$(echo -n "merge_request[title]" | jq -sRr @uri)
  url_title1=$(echo -n "$title" | jq -sRr @uri)
  url_desc0=$(echo -n "merge_request[description]" | jq -sRr @uri)
  url_desc1=$(echo -n "$description" | jq -sRr @uri)
  url="https://gitlab.com/tezos/tezos/-/merge_requests/new?$url_branch0=$url_branch1&$url_target0=$url_target1&$url_title0=$url_title1&$url_desc0=$url_desc1"

  echo
  echo "Merge request can be opened manually using this link:"
  echo "$url"

  step "Opening merge request in browser"
  if command -v xdg-open > /dev/null; then
    xdg-open "$url"
  elif command -v open > /dev/null; then
    open "$url"
  fi
  end_step
}

#------------------------------------------------------------------------------
# Main script execution
#------------------------------------------------------------------------------

parse_arguments "$@"
fetch_mr

if [ "$resume" = false ]; then
  prepare_backport_branch
  cherry_pick_commits
else
  resume_backport
fi

push_backport_branch
create_merge_request
