#!/bin/sh

#shellcheck disable=SC2317
# Function to run apt-get with retries and exponential backoff for a specific error
apt_get_with_retries() {
  set +x
  # Maximum retries
  max_retries=5
  # Initial delay in seconds
  delay=1

  # Loop for retries
  for i in $(seq 1 "$max_retries"); do

    set +e
    # Run apt-get and capture the output and exit status
    output=$(apt-get "$@" 2>&1)
    status=$?
    set -e

    # Check if apt-get succeeded
    if [ "$status" -eq 0 ]; then
      echo "$output"
      set -x
      return 0
    fi

    case "$output" in
    *"Mirror sync in progress"*)
      retry=1
      ;;
    *"Unknown error executing apt-key"*)
      retry=1
      ;;
    *)
      retry=0
      ;;
    esac

    #shellcheck disable=SC2181
    if [ "$retry" -eq 1 ]; then
      # If the specific error occurs, retry with exponential backoff
      echo "-----------"
      echo "Attempt $i of $max_retries..."
      echo "Error detected. Retrying in $delay seconds..."
      echo "$output"
      echo "-----------"
      sleep "$delay"
      # Exponential backoff (doubling the delay)
      # 1 + 3 + 9 + 27 + 81 = 31, so we wait 121s maximum
      # in total with max_retries = 5.
      delay=$((delay * 3))
    else
      # If the error is not the one we are looking for, exit with failure
      echo "apt-get failed with an unexpected error. Exiting."
      echo "$output"
      exit 1
    fi
  done

  echo "apt-get failed after $max_retries attempts."
  exit 1
}

# Replace apt-get with the new function
alias apt-get="apt_get_with_retries"
