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
