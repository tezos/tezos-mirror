#!/bin/sh

apt_get_analysis_errors() {
  output="$1"

  case "$output" in
  *"Mirror sync in progress"*)
    return 1 # Retry
    ;;
  *"Unknown error executing apt-key"*)
    return 1 # Retry
    ;;
  *)
    return 0 # No retry
    ;;
  esac
}

dnf_analysis_errors() {
  output="$1"

  case "$output" in
  *"Downloading successful, but checksum doesn't match."*)
    return 1 # Retry
    ;;
  *"All mirrors were tried"*)
    return 1 # Retry
    ;;
  *)
    return 0 # No retry
    ;;
  esac
}

run_package_manager_with_retries() {
  package_manager="$1"
  shift
  output_analysis_func="$1"
  shift

  # Maximum retries
  max_retries=10
  # Initial delay in seconds
  delay=1

  for i in $(seq 1 "$max_retries"); do
    set +e
    # Run the package manager command and capture the output and exit status
    echo "$package_manager" "$@"
    output=$("$package_manager" "$@" 2>&1)
    status=$?
    set -e

    # Check if the command succeeded
    if [ "$status" -eq 0 ]; then
      echo "$output"
      return 0
    fi

    set +e
    # Call the passed error analysis function
    "$output_analysis_func" "$output"
    retry=$?
    set -e

    if [ "$retry" -eq 1 ]; then
      # If the specific error occurs, retry with exponential backoff
      echo "-----------"
      echo "Attempt $i of $max_retries..."
      echo "Error detected. Retrying in $delay seconds..."
      echo "$output"
      echo "-----------"
      sleep "$delay"
      # Exponential backoff (doubling the delay)
      delay=$((delay * 3))
    else
      # If the error is not the one we are looking for, exit with failure
      echo "$package_manager failed with an unexpected error. Exiting."
      echo "$output"
      exit 1
    fi
  done

  echo "$package_manager failed after $max_retries attempts."
  exit 1
}

apt_get_with_retries() {
  run_package_manager_with_retries apt-get apt_get_analysis_errors "$@"
}

# Replace apt-get with the new function
alias apt-get="apt_get_with_retries"

dnf_with_retries() {
  run_package_manager_with_retries dnf dnf_analysis_errors "$@"
}

# Replace dnf with the new function
alias dnf="dnf_with_retries"

get_node_version() {
  url="http://localhost:8732/version"
  max_attempts=100
  wait_seconds=15
  i=1 # Initialize counter
  response=""

  while [ "$i" -le "$max_attempts" ]; do
    # 1. --silent hides progress
    # 2. --fail handles HTTP errors (404/500)
    # 3. 2> /dev/null HIDES the "Failed to connect" connection errors
    if response=$(curl --silent --fail "$url" 2> /dev/null); then
      # SUCCESS: Echo the version to stdout
      echo "$response"
      return 0
    else
      # WAITING: Echo status to stderr
      echo "Service not yet ready, waiting... (Attempt $i/$max_attempts)" >&2
    fi

    # Increment counter
    i=$((i + 1))

    # Wait before next attempt (only if not the last attempt to save time)
    if [ "$i" -le "$max_attempts" ]; then
      sleep "$wait_seconds"
    fi
  done

  echo "Service did not become ready within the specified time." >&2
  return 1
}
