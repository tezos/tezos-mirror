#!/bin/sh
#
# Retrieve, configure, and import GPG keys for package dual-signing.
#
# On protected branches (on tezos/tezos only, WIF is currently configured to
# filter the access based on GitLab project):
#   1. Fetch the last two private keys (primary or subkey) from GCP Secret Manager.
#   2. Import them into the GPG keyring and extract key IDs.
#   3. Determine if both subkeys share the same master key.
#   4. Fetch passphrase(s): one if same master, two if different masters.
#   5. Fetch and merge the last two public key versions from a GCS bucket.
#      Dual Signing is only enabled when: two private keys are accessible
#      and valid with their own passphrases
#   If any GCP step fails, fall back to pre-configured GitLab CI/CD variables.
#
# On non-protected branches:
#   Embedded test keys from ./scripts/packaging/ are used.
#
# Expected GitLab CI/CD variables:
#
# - These variables must be protected, but can be unmasked:
#   - GCP_SECRET_PATH_GPG_LINUX_PACKAGES_PRIVATE_KEY  (projects/PROJECT/secrets/NAME)
#   - GCP_SECRET_PATH_GPG_LINUX_PACKAGES_PASSPHRASE   (projects/PROJECT/secrets/NAME)
#   Note: legacy format with /versions/latest suffix is also accepted
#         but the version will be ignored.
#   - GCP_BUCKET_GPG_PUBLIC_KEY_URL  (gs://bucket/path/to/key.asc)
#
# - These variables are related to fallback keys and must be protected,
#   masked and hidden; make sure to keep a backup before hiding them:
#   - GPG_LINUX_PACKAGES_FALLBACK_PASSPHRASE
#   - GPG_LINUX_PACKAGES_FALLBACK_PRIVATE_KEY
#   - GPG_LINUX_PACKAGES_FALLBACK_PUBLIC_KEY
#
# Note: The fallback keys are marked as "Package Signing Failback Temporary Key
#       (Temporary key, if this key is used, it indicates that the signing key
#       service is currently down.)"
#
# Exports:
#   GPG_KEY_ID, GPG_KEY_ID_BIS,
#   GPG_PASSPHRASE, GPG_PASSPHRASE_BIS,
#   GPG_PUBLIC_KEY (merged public key content),
#   GPG_DUAL_SIGNING ("true" or "false")
#
# Side effects:
#   - Private keys are imported into the GPG keyring (ready to sign).

set -eu

readonly GCP_SM_BASE_URL="https://secretmanager.googleapis.com/v1"
readonly GCP_STORAGE_BASE_URL="https://storage.googleapis.com/storage/v1"
readonly CURL_MAX_TIME=30
readonly CURL_RETRIES=3

##############################################
# Helpers                                    #
##############################################

# List the N most recent ENABLED version numbers of a secret (newest first).
# Args: $1=access_token, $2=secret_base_path (no /versions), $3=count (default 2)
# Output: version numbers, one per line
list_latest_versions() {
  # shellcheck disable=SC3043
  local token="$1" path="$2" count="${3:-2}"
  # shellcheck disable=SC3043
  local response
  response=$(
    printf 'Authorization: Bearer %s\n' "${token}" |
      curl --silent --fail --show-error \
        --max-time "${CURL_MAX_TIME}" \
        --retry "${CURL_RETRIES}" \
        --header @- \
        "${GCP_SM_BASE_URL}/${path}/versions?filter=state:ENABLED"
  ) || {
    echo "ERROR: Failed to list versions for ${path}." >&2
    return 1
  }

  echo "${response}" | jq -r --argjson n "${count}" \
    '[.versions[]
      | select(.state == "ENABLED")
      | (.name | split("/") | last | tonumber)
     ] | sort | reverse | .[:$n] | .[]'
}

# Fetch a secret at a specific version.
# The access token is passed via stdin to avoid exposing it in the process table.
# Args: $1=access_token, $2=secret_base_path, $3=version
# Output: decoded secret value on stdout
fetch_secret() {
  # shellcheck disable=SC3043
  local token="$1" path="$2" version="$3"
  printf '%s' "${token}" | "./scripts/ci/gcp_sm_get_secret.sh" "${path}/versions/${version}"
}

# Parse a gs:// URI into bucket and object components.
# Args: $1=gs_uri (gs://bucket/object/path)
# Output: two lines — bucket then object
parse_gs_uri() {
  # shellcheck disable=SC3043
  local gs_uri="$1"
  # shellcheck disable=SC3043
  local stripped="${gs_uri#gs://}"
  # shellcheck disable=SC3043
  local bucket="${stripped%%/*}"
  # shellcheck disable=SC3043
  local object="${stripped#*/}"

  if [ -z "${bucket}" ] || [ -z "${object}" ] || [ "${bucket}" = "${stripped}" ]; then
    echo "ERROR: Invalid GCS URI '${gs_uri}'. Expected gs://bucket/object." >&2
    return 1
  fi

  echo "${bucket}"
  echo "${object}"
}

# List the N most recent generations of a GCS object (newest first).
# Requires object versioning to be enabled on the bucket.
# Args: $1=access_token, $2=bucket, $3=object, $4=count (default 2)
# Output: generation numbers, one per line
list_gcs_generations() {
  # shellcheck disable=SC3043
  local token="$1" bucket="$2" object="$3" count="${4:-2}"
  # shellcheck disable=SC3043
  local encoded_object
  encoded_object=$(printf '%s' "${object}" | sed 's|/|%2F|g')

  # Authorization Bearer token not needed here: public key bucket must be public
  # shellcheck disable=SC3043
  local response
  response=$(
    curl --silent --fail --show-error \
      --max-time "${CURL_MAX_TIME}" \
      --retry "${CURL_RETRIES}" \
      "${GCP_STORAGE_BASE_URL}/b/${bucket}/o?versions=true&prefix=${encoded_object}"
  ) || {
    echo "ERROR: Failed to list versions for gs://${bucket}/${object}." >&2
    return 1
  }

  # .generation is a string in the GCS JSON API; lexicographic sort is correct here
  # because generation numbers are monotonically increasing fixed-width timestamps.
  echo "${response}" | jq -r --argjson n "${count}" --arg name "${object}" \
    '[.items // []
      | .[]
      | select(.name == $name)
      | .generation
     ] | sort | reverse | .[:$n] | .[]'
}

# Download a GCS object, optionally at a specific generation.
# Args: $1=access_token, $2=bucket, $3=object, $4=generation (optional)
# Output: object content on stdout
fetch_gcs_object() {
  # shellcheck disable=SC3043
  local token="$1" bucket="$2" object="$3" generation="${4:-}"
  # shellcheck disable=SC3043
  local encoded_object url gcs_output_tmp_file http_status
  encoded_object=$(printf '%s' "${object}" | sed 's|/|%2F|g')
  url="${GCP_STORAGE_BASE_URL}/b/${bucket}/o/${encoded_object}?alt=media"

  if [ -n "${generation}" ]; then
    url="${url}&generation=${generation}"
  fi

  # Authorization Bearer token not needed here: public key bucket must be public
  gcs_output_tmp_file=$(mktemp)
  http_status=$(curl --silent --show-error \
    --max-time "${CURL_MAX_TIME}" \
    --retry "${CURL_RETRIES}" \
    --output "${gcs_output_tmp_file}" \
    --write-out '%{http_code}' \
    "${url}") || true

  if [ "${http_status}" != "200" ]; then
    echo "ERROR: Failed to download gs://${bucket}/${object} (generation=${generation:-latest}): HTTP ${http_status}" >&2
    echo "(DEBUG) GCS response: $(cat "${gcs_output_tmp_file}")" >&2
    rm -f "${gcs_output_tmp_file}"
    return 1
  fi

  cat "${gcs_output_tmp_file}"
  rm -f "${gcs_output_tmp_file}"
}

# Download the public key(s) from a GCS bucket, merging up to 2 generations.
# Args: $1=access_token, $2=gs_uri (gs://bucket/object/path)
# Sets: GPG_PUBLIC_KEY
# Returns: 0 on success, 1 on failure
fetch_public_key_from_bucket() {
  # shellcheck disable=SC3043
  local token="$1" gs_uri="$2"
  # shellcheck disable=SC3043
  local parsed bucket object
  parsed=$(parse_gs_uri "${gs_uri}") || return 1
  bucket=$(echo "${parsed}" | sed -n '1p')
  object=$(echo "${parsed}" | sed -n '2p')

  # Try to list available generations (requires versioning on the bucket)
  # shellcheck disable=SC3043
  local gen_list gen_1 gen_2
  gen_list=$(list_gcs_generations "${token}" "${bucket}" "${object}" 2) || gen_list=""
  gen_1=$(printf '%s\n' "${gen_list}" | sed -n '1p')
  gen_2=$(printf '%s\n' "${gen_list}" | sed -n '2p')

  if [ -n "${gen_2}" ]; then
    echo "Found at least 2 generations in bucket — merging latest two." >&2

    # shellcheck disable=SC3043
    local key_latest key_prev
    key_latest=$(fetch_gcs_object "${token}" "${bucket}" "${object}" "${gen_1}") || return 1
    key_prev=$(fetch_gcs_object "${token}" "${bucket}" "${object}" "${gen_2}") || return 1

    GPG_PUBLIC_KEY="${key_latest}
${key_prev}"
    echo "Merged public keys from generations ${gen_1} and ${gen_2}." >&2

  elif [ -n "${gen_1}" ]; then
    echo "WARNING: Only one generation in bucket." >&2
    GPG_PUBLIC_KEY=$(fetch_gcs_object "${token}" "${bucket}" "${object}" "${gen_1}") || return 1

  else
    # Versioning may be disabled — fetch the live object directly
    echo "WARNING: No generations listed (versioning may be disabled) — fetching live object." >&2
    GPG_PUBLIC_KEY=$(fetch_gcs_object "${token}" "${bucket}" "${object}") || return 1
  fi

  return 0
}

# Import a base64-encoded GPG private key and return the signing key ID.
# The key may be a full primary key export (e.g. for fallback keys or test keys)
# or a subkey-only export (e.g. for keys stored in GCP Secret Manager).
# Args: $1=base64_encoded_key
# Output: signing subkey ID if one exists, else primary key ID
import_gpg_key() {
  # shellcheck disable=SC3043
  local key_b64="$1"

  # Inspect the key material before importing to determine the signing key ID.
  # shellcheck disable=SC3043
  local show_output
  show_output=$(echo "${key_b64}" | base64 -d |
    gpg --show-keys --with-colons 2> /dev/null) || true

  # shellcheck disable=SC3043
  local key_id
  # For subkeys: pick the most recently created signing subkey (ssb, field 12 ~ 's').
  # The creation-date max ensures we get the latest rotation even if multiple
  # signing subkeys are present (e.g. a full primary key export).
  key_id=$(echo "${show_output}" | awk -F: '
    /^ssb:/ && $12 ~ /s/ && $6 >= max_creation_date { max_creation_date=$6; key_id=$5 }
    END { print key_id }
  ')

  if [ -z "${key_id}" ]; then

    # For primary keys: sec (real key, not sec# stub) with no signing subkey.
    echo "WARNING: No subkey found, checking primary keys instead." >&2
    key_id=$(echo "${show_output}" | awk -F: '/^sec:/ { print $5; exit }')

    if [ -z "${key_id}" ]; then
      echo "ERROR: No signing key found in key material." >&2
      return 1
    else
      echo "Importing GPG private primary key (${key_id}) ..." >&2
    fi

  else
    echo "Importing GPG private subkey (${key_id}) ..." >&2
  fi

  # Import the key into the GPG keyring.
  # shellcheck disable=SC3043
  local import_output
  import_output=$(echo "${key_b64}" | base64 -d | gpg --batch --import -- 2>&1)
  echo "${import_output}" >&2

  echo "${key_id}"
}

# Get the primary key fingerprint for a given key ID.
# Works for both subkey IDs (returns the primary's fingerprint) and
# primary key IDs (returns the key's own fingerprint).
# Args: $1=key_id
# Output: primary key fingerprint
get_master_fingerprint() {
  # shellcheck disable=SC3043
  local key_id="$1"
  gpg --list-secret-keys --with-colons "${key_id}" 2> /dev/null |
    awk -F: '/^fpr/ { print $10; exit }'
}

# Fetch all secrets from GCP (Secret Manager + GCS bucket).
# Sets GPG_* variables and imports keys into the GPG keyring.
# Args: $1=access_token
# Returns: 0 on success, 1 on any failure
fetch_gcp_secrets() {
  # shellcheck disable=SC3043
  local token="$1"

  # Strip trailing /versions/* from secret paths if present (legacy format).
  # The functions below append their own /versions/<version>.
  GCP_SECRET_PATH_GPG_LINUX_PACKAGES_PRIVATE_KEY="${GCP_SECRET_PATH_GPG_LINUX_PACKAGES_PRIVATE_KEY%/versions/*}"
  GCP_SECRET_PATH_GPG_LINUX_PACKAGES_PASSPHRASE="${GCP_SECRET_PATH_GPG_LINUX_PACKAGES_PASSPHRASE%/versions/*}"

  ##############################################
  # Step 1: Retrieve and import private keys   #
  ##############################################

  echo "Fetching private key versions..." >&2

  # shellcheck disable=SC3043
  local priv_list priv_version_1 priv_version_2
  priv_list=$(list_latest_versions "${token}" "${GCP_SECRET_PATH_GPG_LINUX_PACKAGES_PRIVATE_KEY}" 2)
  priv_version_1=$(printf '%s\n' "${priv_list}" | sed -n '1p')
  priv_version_2=$(printf '%s\n' "${priv_list}" | sed -n '2p')

  if [ -z "${priv_version_1}" ]; then
    echo "ERROR: No enabled private key versions found." >&2
    return 1
  fi

  # Fetch and import the latest key (may be a primary key or a subkey)
  GPG_PRIVATE_KEY=$(fetch_secret "${token}" \
    "${GCP_SECRET_PATH_GPG_LINUX_PACKAGES_PRIVATE_KEY}" "${priv_version_1}") || return 1
  GPG_KEY_ID=$(import_gpg_key "${GPG_PRIVATE_KEY}") || return 1
  GPG_PRIVATE_KEY=""
  echo "Imported stored private key (version ${priv_version_1}) - key ID: ${GPG_KEY_ID}" >&2

  # Fetch and import second key if available
  # shellcheck disable=SC3043
  local has_second_key=false
  if [ -n "${priv_version_2}" ]; then
    GPG_PRIVATE_KEY_BIS=$(fetch_secret "${token}" \
      "${GCP_SECRET_PATH_GPG_LINUX_PACKAGES_PRIVATE_KEY}" "${priv_version_2}") || return 1
    GPG_KEY_ID_BIS=$(import_gpg_key "${GPG_PRIVATE_KEY_BIS}") || return 1
    GPG_PRIVATE_KEY_BIS=""
    echo "Imported second stored private key (version ${priv_version_2}) - key ID: ${GPG_KEY_ID_BIS}" >&2
    has_second_key=true
  else
    echo "Only one private key version available - dual signing disabled." >&2
  fi

  ##############################################
  # Step 2: Determine primary key relationship #
  ##############################################
  # For subkeys: get_master_fingerprint returns the primary key's fingerprint.
  # For primary keys: get_master_fingerprint returns the key's own fingerprint.
  # Either way, matching fingerprints means both keys share the same passphrase.

  # shellcheck disable=SC3043
  local same_master=false
  if [ "${has_second_key}" = "true" ]; then
    # shellcheck disable=SC3043
    local master_fpr_1 master_fpr_2
    master_fpr_1=$(get_master_fingerprint "${GPG_KEY_ID}")
    master_fpr_2=$(get_master_fingerprint "${GPG_KEY_ID_BIS}")

    if [ "${master_fpr_1}" = "${master_fpr_2}" ]; then
      same_master=true
      echo "Both keys share the same primary key - one passphrase covers both." >&2
    else
      echo "Keys have different primary keys - separate passphrases required." >&2
    fi

    # Unique case when Dual Signing is enabled
    GPG_DUAL_SIGNING="true"
  fi

  ##############################################
  # Step 3: Retrieve passphrases               #
  ##############################################

  echo "Fetching passphrases..." >&2

  GPG_PASSPHRASE=$(fetch_secret "${token}" \
    "${GCP_SECRET_PATH_GPG_LINUX_PACKAGES_PASSPHRASE}" "latest") || return 1

  if [ "${has_second_key}" = "true" ]; then
    if [ "${same_master}" = "true" ]; then
      GPG_PASSPHRASE_BIS="${GPG_PASSPHRASE}"
      echo "Same primary key - reusing latest passphrase for both keys." >&2
    else
      # shellcheck disable=SC3043
      local pass_list pass_version_2
      pass_list=$(list_latest_versions "${token}" "${GCP_SECRET_PATH_GPG_LINUX_PACKAGES_PASSPHRASE}" 2)
      pass_version_2=$(printf '%s\n' "${pass_list}" | sed -n '2p')
      if [ -z "${pass_version_2}" ]; then
        echo "WARNING: Different primary keys but only one passphrase version - falling back to single signing." >&2
        GPG_PRIVATE_KEY_BIS=""
        GPG_KEY_ID_BIS=""
        GPG_PASSPHRASE_BIS=""
        GPG_DUAL_SIGNING="false" # Needed passphrase not accessible for the second key: disable dual signing
      else
        GPG_PASSPHRASE_BIS=$(fetch_secret "${token}" \
          "${GCP_SECRET_PATH_GPG_LINUX_PACKAGES_PASSPHRASE}" "${pass_version_2}") || return 1
        echo "Different primary keys - fetched previous passphrase (version ${pass_version_2})." >&2
      fi
    fi
  fi

  ##############################################
  # Step 4: Retrieve public key from bucket    #
  ##############################################

  echo "Fetching public key from GCS bucket..." >&2
  fetch_public_key_from_bucket "${token}" "${GCP_BUCKET_GPG_PUBLIC_KEY_URL}" || return 1

  return 0
}

##############################################
# Main                                       #
##############################################

# Flush the output variables to avoid any injection during the process
export GPG_KEY_ID=""
export GPG_KEY_ID_BIS=""
export GPG_PASSPHRASE=""
export GPG_PASSPHRASE_BIS=""
export GPG_PUBLIC_KEY=""
export GPG_DUAL_SIGNING="false"

##############################################
# Protected branch: full key retrieval       #
##############################################

if [ "${CI_COMMIT_REF_PROTECTED}" = "true" ]; then

  echo "Protected branch - fetching GPG keys from GCP..." >&2

  # Disable debug to prevent leaking tokens/secrets
  { set +x; } 2> /dev/null

  export GPG_TTY=/dev/console

  # --- Authenticate via WIF and fetch secrets ---
  # If any required variable is unset or any step fails, fall back to GitLab CI/CD variables.
  GCP_AVAILABLE=false

  GCP_VARS_SET=true
  for var in GCP_SECRET_PATH_GPG_LINUX_PACKAGES_PRIVATE_KEY \
    GCP_SECRET_PATH_GPG_LINUX_PACKAGES_PASSPHRASE \
    GCP_BUCKET_GPG_PUBLIC_KEY_URL; do
    eval "val=\${${var}:-}"
    if [ -z "${val}" ]; then
      echo "WARNING: Required GCP variable '${var}' is not set - will use fallback keys." >&2
      GCP_VARS_SET=false
      break
    fi
  done

  if [ "${GCP_VARS_SET}" = "true" ]; then
    if ACCESS_TOKEN=$(./scripts/ci/gcp_get_access_token.sh); then
      if fetch_gcp_secrets "${ACCESS_TOKEN}"; then
        GCP_AVAILABLE=true
      fi
    fi
  fi

  if [ "${GCP_AVAILABLE}" = "true" ]; then
    echo "GPG keys successfully retrieved from GCP." >&2
  else
    #######################################################
    # Fallback: use pre-configured GitLab CI/CD variables #
    #######################################################

    echo "/!\\ WARNING: GCP is NOT available, fallback keys will be used. /!\\" >&2

    for var in GPG_LINUX_PACKAGES_FALLBACK_PASSPHRASE \
      GPG_LINUX_PACKAGES_FALLBACK_PRIVATE_KEY \
      GPG_LINUX_PACKAGES_FALLBACK_PUBLIC_KEY; do
      eval "val=\${${var}:-}"
      if [ -z "${val}" ]; then
        echo "ERROR: Fallback variable '${var}' is not set. Cannot proceed." >&2
        exit 1
      fi
    done

    GPG_KEY_ID=$(import_gpg_key "${GPG_LINUX_PACKAGES_FALLBACK_PRIVATE_KEY}")
    GPG_PASSPHRASE="${GPG_LINUX_PACKAGES_FALLBACK_PASSPHRASE}"
    GPG_PUBLIC_KEY="${GPG_LINUX_PACKAGES_FALLBACK_PUBLIC_KEY}"

    echo "Fallback key imported - key ID: ${GPG_KEY_ID}" >&2
  fi

##############################################
# Non-protected branch: test keys            #
##############################################

else
  echo "Non-protected branch - using embedded test keys." >&2

  export GPG_TTY=/dev/console

  GPG_KEY_ID="24EA481996EB8138"
  GPG_PASSPHRASE="07cde771b39a4ed394864baa46126b"
  GPG_PUBLIC_KEY=$(cat "./scripts/packaging/package-signing-key.asc")

  # Import test key into GPG keyring
  base64 -d < ./scripts/packaging/test_repo_private.key | gpg --batch --import -- 2> /dev/null
  echo "Test key imported into GPG keyring." >&2
fi

##############################################
# Flush sensitive variables                  #
##############################################

unset GPG_LINUX_PACKAGES_FALLBACK_PRIVATE_KEY
unset GPG_LINUX_PACKAGES_FALLBACK_PASSPHRASE

##############################################
# Export all variables                       #
##############################################

export GPG_KEY_ID
export GPG_KEY_ID_BIS
export GPG_PASSPHRASE
export GPG_PASSPHRASE_BIS
export GPG_PUBLIC_KEY
export GPG_DUAL_SIGNING

echo "GPG keys configured (dual_signing=${GPG_DUAL_SIGNING})." >&2
