#!/usr/bin/env bash

##############################################################################
#                                                                            #
# SPDX-License-Identifier: MIT                                               #
# SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>       #
#                                                                            #
##############################################################################

set -e

## The script may create commits. Keep track of the number of commits to clean up in case of an error
commits=0
is_snapshot=false
source_hash=""
source_label=""
long_hash=""
short_hash=""
capitalized_label=""
doc_label=""

skip_copy_source=""
skip_update_protocol_tests=""
skip_update_source=""
skip_update_tezt_tests=""
skip_misc_updates=""
skip_generate_doc=""

## Log colors:
if [[ -t 1 ]]; then
  yellow="\e[33m"
  red="\e[31m"
  blue="\e[34m"
  green="\e[32m"
  cyan="\e[36m"
  magenta="\e[35m"
  blinking="\e[5m"
  reset="\e[0m"
else
  yellow=""
  red=""
  blue=""
  green=""
  cyan=""
  reset=""
  blinking=""
  magenta=""
fi

function error() {
  if [[ $# -eq 2 ]]; then
    echo -e "${red}Error: ${yellow}$1${red} $2${reset}" 1>&2
  else
    echo -e "${red}Error: $*${reset}" 1>&2
  fi
}

function warning() {
  echo -e "${yellow}Warning: $*${reset}" 1>&2
}

function log_blue() {
  echo -e "${blue}$*${reset}"
}

function log_green() {
  echo -e "${green}$*${reset}"
}

function log_cyan() {
  echo -e "${cyan}$*${reset}"
}

function log_magenta() {
  echo -e "${magenta}$*${reset}"
}

function print_and_exit() {
  if [[ $# -ne 2 ]]; then
    error "Exiting because of an error at $0"
  else
    error "Exiting because of an error at $0:$1"
  fi
  if [[ ${commits} -gt 0 ]]; then
    log_green "To clean up git commits run:"
    echo "git reset --hard HEAD~${commits}"
  fi

  log_green "To cleanup other created files after reset --hard"
  echo "rm -rf src/proto_${protocol_target} docs/${label} src/proto_${version}_${short_hash} src/proto_${version}"
  echo "exiting..."
  exit 1
}

function commit() {
  git add "${script_dir}/../"
  # if pre-commit hooks are enabled, run them
  if [[ -f .git/hooks/pre-commit ]]; then
    .git/hooks/pre-commit || true
    git add "${script_dir}/../" || true
  fi
  if [[ -z "$AUTHOR" ]]; then
    if ! git commit -m "${capitalized_label}/$1"; then
      git add "${script_dir}/../"
      if ! git commit -m "${capitalized_label}/$1"; then
        error "Failed to create commit" 1>&2
        print_and_exit 1 "${LINENO}"
      fi
    fi
  else
    if ! git commit -m "${capitalized_label}/$1" --author="${AUTHOR}"; then
      git add "${script_dir}/../"
      if ! git commit -m "${capitalized_label}/$1" --author="${AUTHOR}"; then
        error "Failed to create commit" 1>&2
        print_and_exit 1 "${LINENO}"
      fi
    fi
  fi

  echo -e "${blue}Created commit:${cyan} ${capitalized_label}/$1${reset}"
  commits=$((commits + 1))
}

function commit_if_changes() {
  if [[ $(git status --porcelain | wc -l) -gt 0 ]]; then
    commit "$1"
  fi

}

function commit_no_hooks() {
  git add "${script_dir}/../"
  # if pre-commit hooks are enabled, run them
  if [[ -z "$AUTHOR" ]]; then
    git commit -m "${capitalized_label}/$1" --no-verify
  else
    git commit -m "${capitalized_label}/$1" --no-verify --author="${AUTHOR}"
  fi
  echo -e "${blue}Created commit:${cyan} ${capitalized_label}/$1${reset}"
  commits=$((commits + 1))
}

function commit_no_hooks_if_changes() {
  if [[ $(git status --porcelain | wc -l) -gt 0 ]]; then
    commit_no_hooks "$1"
  fi
}

## Automatically clean and exit on error
trap print_and_exit ERR

function usage() {
  # colored option strings
  f="${green}-f${reset}"
  t="${green}-t${reset}"
  h="${green}-h${reset}"
  s="${green}-s${reset}"
  p="${green}-p${reset}"
  c="${green}-c${reset}"
  a="${green}-a${reset}"
  d="${green}-d${reset}"
  F="${green}-F${reset}"
  from="${green}--from${reset}"
  to="${green}--to${reset}"
  as="${green}--as${reset}"
  help="${green}--help${reset}"
  stabilise="${green}--stabilise${reset}"
  snapshot="${green}--snapshot${reset}"
  hash="${green}--hash${reset}"
  finalize_docs="${green}--finalize-docs${reset}"
  copy="${green}--copy${reset}"
  delete="${green}--delete${reset}"
  force_snapshot="${green}--force-snapshot${reset}"
  ## colored vars
  alpha="${red}alpha${reset}"
  beta="${red}beta${reset}"
  stockholm_023="${red}stockholm_023${reset}"
  stockholm="${magenta}stockholm${reset}"
  pt_stockholm="${red}023_PtStockh${reset}"
  t024="${red}t024${reset}"
  tallinn="${magenta}tallinn${reset}"
  script=${blue}$0${reset}

  echo -e "
${yellow}Usage: ${blue}$0${reset} ${red}[options]${reset}
${red}Options:${reset}
  ${h} , ${help}
    Print this help message.
  ${s}, ${stabilise}
    Stabilise a protocol.
  ${p}, ${snapshot}
    Snapshot a protocol.
  ${hash}
    Updates the hash of a protocol.
  ${finalize_docs}
    Finalize protocol documentation (rename docs folder and update all references).
  ${c}, ${copy}
    Copy a protocol.
  ${d}, ${delete} ${red}<protocol_source_dirname>${reset}
    Delete a protocol from the source code.
  ${f}, ${from} ${red}<protocol_source>${reset}
    The source protocol to stabilise or snapshot.
  ${t}, ${to} ${red}<protocol_target>${reset}
    The target protocol to stabilise or snapshot.
  ${a}, ${as} ${red}<label>${reset}

${yellow}tl;dr:${reset}
- To stabilise protocol alpha into beta and link it in the node, client and codec:
  ${script} ${stabilise} ${from} ${alpha} ${to} ${beta}
  or
  ${script} ${s} ${f} ${alpha} ${t} ${beta}

- To snapshot protocol alpha into stockholm_023 and link it in the node, client and codec:
  ${script} ${snapshot} ${from} ${beta} ${to} ${stockholm_023}
  or
  ${script} ${p} ${f} ${beta} ${t} ${stockholm_023}

- To update the hash of a protocol:
  ${script} ${hash} ${f} ${beta}
  or
  ${script} ${hash} ${from} ${beta}
To update the hash of a previously snapshotted protocol:
  ${script} ${hash} ${f} ${beta} ${F}
  or
  ${script} ${hash} ${from} ${beta} ${force_snapshot}

- To finalize documentation (rename docs folder from code label to human-readable name):
  ${script} ${finalize_docs} ${from} ${t024} ${to} ${tallinn}

- To copy stockholm_023 known as stockholm into beta and link it in the node, client and codec:
  ${script} ${copy} ${from} ${stockholm_023} ${as} ${stockholm} ${to} ${beta}
  or
  ${script} ${c} ${f} ${stockholm_023} ${a} ${stockholm} ${t} ${beta}

- To delete a snapshotted protocol from the source code:
  ${script} ${delete} ${pt_stockholm} ${as} ${stockholm}
  or
  ${script} ${d} ${pt_stockholm} ${a} ${stockholm}

- To delete a protocol in stabilisation from the source code:
  ${script} ${delete} ${beta}
  or
  ${script} ${d} ${beta}
"
}

# Read command line arguments
while true; do
  case "$1" in
  -h | --help)
    usage
    exit 0
    ;;
  -s | --stabilise)
    command="stabilise"
    shift
    ;;
  -p | --snapshot)
    command="snapshot"
    shift
    ;;
  --hash)
    command="hash"
    shift
    ;;
  --finalize-docs)
    command="finalize_docs"
    shift
    ;;
  -c | --copy)
    command="copy"
    shift
    ;;
  -d | --delete)
    command="delete"
    protocol_source="$2"
    shift 2
    ;;
  -f | --from)
    protocol_source="$2"
    # Only read hash if the protocol directory exists (not needed for finalize_docs)
    if [[ -f "src/proto_${protocol_source}/lib_protocol/TEZOS_PROTOCOL" ]]; then
      source_hash=$(grep -oP '(?<="hash": ")[^"]*' "src/proto_${protocol_source}/lib_protocol/TEZOS_PROTOCOL")
    fi
    shift 2
    ;;
  -t | --to)
    protocol_target="$2"
    shift 2
    ;;
  -a | --as)
    source_label="$2"
    shift 2
    ;;
  --)
    shift
    break
    ;;
  --force-snapshot | -F)
    is_snapshot=true
    shift
    ;;
  --skip-copy-source)
    skip_copy_source=true
    shift
    ;;
  --skip-protocol-tests)
    skip_update_protocol_tests=true
    shift
    ;;
  --skip-update-source)
    skip_update_source=true
    shift
    ;;
  --skip-update-tezt-tests)
    skip_update_tezt_tests=true
    shift
    ;;
  --skip-misc-updates)
    skip_misc_updates=true
    shift
    ;;
  --skip-generate-doc)
    skip_generate_doc=true
    shift
    ;;
  *)
    break
    ;;
  esac
done

if [[ -z ${command} ]]; then
  echo "No command specified" 1>&2
  usage 1>&2
  print_and_exit 1 "${LINENO}"
fi

## ensure command is known
case ${command} in
stabilise | snapshot | hash | copy | delete | finalize_docs) ;;
*)
  error "Unknown command: ${command}" 1>&2
  error "Command should be one of stabilise, snapshot, hash, copy, delete or finalize_docs" 1>&2
  usage 1>&2
  print_and_exit 1 "${LINENO}"
  ;;
esac

case ${command} in
delete) ;;
*)
  if [[ -z ${protocol_source} ]]; then
    error "No protocol source specified" 1>&2
    usage 1>&2
    print_and_exit 1 "${LINENO}"
  fi
  ;;
esac

case ${command} in
stabilise | snapshot | copy)
  if [[ -z ${protocol_target} ]]; then
    error "No protocol target specified" 1>&2
    usage 1>&2
    print_and_exit 1 "${LINENO}"
  fi
  if [[ ${protocol_source} == "${protocol_target}" ]]; then
    error "Protocol source and target should be different" 1>&2
    usage 1>&2
    print_and_exit 1 "${LINENO}"
  fi
  msg="Will ${command} protocol from 'src/proto_${protocol_source}' into  'src/proto_${protocol_target}'"
  # check if ./octez-protocol-compiler is present
  if [[ ! -f "./octez-protocol-compiler" ]]; then
    error "octez-protocol-compiler" "not found, compile it before running this script" 1>&2
    print_and_exit 1 "${LINENO}"
  fi

  ;;
*)
  msg="Will ${command} protocol from 'src/proto_${protocol_source}'"
  ;;
esac

## check if git tree is clean
if [[ $(git status --porcelain | wc -l) -gt 0 ]]; then
  error "Git tree is not clean, please commit or stash your changes" 1>&2
  print_and_exit 1 "${LINENO}"
fi

log_blue "${msg}."

# Check if the protocol source exists (not needed for finalize_docs)
if [[ ${command} != "finalize_docs" && ! -d "src/proto_${protocol_source}" ]]; then
  error "'src/proto_${protocol_source}'" "does not exist" 1>&2
  print_and_exit 1 "${LINENO}"
fi

# if stabilise command is used, protocol_target should be of the form [a-z]+[0-9]*
if [[ ${command} == "stabilise" ]]; then
  if ! [[ ${protocol_source} =~ ^[a-z]+[0-9]*$ ]]; then
    error "To ${yellow}stabilise${red}, protocol_source should be of the form [a-z]+[0-9]+" 1>&2
    print_and_exit 1 "${LINENO}"
  fi
  if ! [[ ${protocol_target} =~ ^[a-z]+[0-9]*$ ]]; then
    error "To ${yellow}stabilise${red} protocol_target should be of the form [a-z]+[0-9]+" 1>&2
  fi
  # warn if source_label is given that it will not be used
  if [[ -n ${source_label} ]]; then
    warning "source_label will not be used for stabilisation"
  fi
fi

# if copy command is used, protocol_source should be of the form [0-9][0-9][0-9]_P[A-Za-z]+
if [[ ${command} == "copy" ]]; then
  if ! [[ ${protocol_source} =~ ^[0-9][0-9][0-9]_P[A-Za-z]+$ ]]; then
    error "To ${yellow}copy${red}, protocol_source should be of the form [0-9][0-9][0-9]_P[A-Za-z]+" 1>&2
    print_and_exit 1 "${LINENO}"
  fi
  if ! [[ ${protocol_target} =~ ^[a-z]+[0-9]*$ ]]; then
    error "To ${yellow}copy${red}, protocol_target should be of the form [a-z]+[0-9]+" 1>&2
    print_and_exit 1 "${LINENO}"
  fi
  if [[ -z ${source_label} ]]; then
    error "No source label specified" 1>&2
    usage 1>&2
    print_and_exit 1 "${LINENO}"
  fi
fi

# if snapshot_command is used, protocol_target should be of the form [a-z]+_[0-9][0-9][0-9]
if [[ ${command} == "snapshot" ]]; then
  # if ! [[ ${protocol_source} =~ ^[a-z]+[0-9]*$ ]]; then
  #   error "To ${red}snapshot${reset}, protocol_source should be of the form [a-z]+[0-9]+" 1>&2
  #   print_and_exit 1 "${LINENO}"
  # fi
  if ! [[ ${protocol_target} =~ ^[a-z]+_[0-9][0-9][0-9]$ ]]; then
    error "To ${red}snapshot${reset}, protocol_target should be of the form [a-z]+_[0-9][0-9][0-9]" 1>&2
    clean_and_exit 1 "${LINENO}"
  fi
  # warn if source_label is given that it will not be used
  if [[ -n ${source_label} ]]; then
    warning "source_label will not be used for stabilisation"
  fi
fi

# check if ./octez-protocol-compiler is present
if [[ ! -f "./octez-protocol-compiler" ]]; then
  error "octez-protocol-compiler" "not found, compile it before running this script" 1>&2
  print_and_exit 1 "${LINENO}"
fi

rm -rf /tmp/tezos_proto_snapshot

script_dir="$(cd "$(dirname "$0")" && pwd -P)"
cd "${script_dir}"/..

function alpha_rst() {
  printf "Protocol Alpha
==============

This page documents the changes brought by protocol Alpha with respect
to %s (see :ref:\`naming_convention\`).

For changes brought by Quebec with respect to Paris, see :doc:\`../protocols/021_quebec\`.

The code can be found in directory :src:\`src/proto_alpha\` of the \`\`master\`\`
branch of Octez.

Environment Version
-------------------



Smart Rollups
-------------


Data Availability Layer
-----------------------

Adaptive Issuance
-----------------


Gas improvements
----------------

Breaking Changes
----------------

RPC Changes
-----------

Operation receipts
------------------


Errors
------


Protocol parameters
-------------------



Bug Fixes
---------

Minor Changes
-------------

Internal
--------


" "$1"
}

function sanity_check_before_script() {

  if ! [[ -d "src/proto_${protocol_source}" ]]; then
    error "'src/proto_${protocol_source}'" "does not exist" 1>&2
    print_and_exgxit 1 "${LINENO}"
  fi

  if [[ ${command} == "copy" ]] || [[ ${command} == "delete" ]]; then
    if ! [[ -d "docs/${source_label}" ]]; then
      error "'docs/${source_label}'" "does not exist" 1>&2
      print_and_exit 1 "${LINENO}"
    fi
  else
    if ! [[ -d "docs/${protocol_source}" ]]; then
      error "'docs/${protocol_source}'" "does not exist" 1>&2
      print_and_exit 1 "${LINENO}"
    fi
  fi
  case ${command} in
  stabilise | snapshot | copy)
    if [[ -d "src/proto_${version}" && ! ${skip_copy_source} ]]; then
      error "'src/proto_${version}'" "already exists, you should remove it."
      print_and_exit 1 "${LINENO}"
    fi

    if [[ -d "docs/${label}" && ! ${skip_generate_doc} ]]; then
      error "'docs/${label}'" "already exists, you should remove it."
      print_and_exit 1 "${LINENO}"
    fi

    # check if bc is available
    if ! command -v bc &> /dev/null; then
      error "bc" "is missing, please install it" 1>&2
      print_and_exit 1 "${LINENO}"
    fi
    ;;
  esac
}

# Recompute the different protocol and version names
function recompute_names() {
  if [[ ${is_snapshot} == true ]]; then
    new_protocol_name="${version}_${short_hash}"
    new_tezos_protocol="${version}-${short_hash}"
    new_versioned_name="${version}_${label}"
  else
    new_protocol_name="${version}"
    new_tezos_protocol="${version}"
    new_versioned_name="${label}"
  fi
}

# Compute all protocol names and paths upfront before any operations.
# This function centralizes all naming logic to make workflows predictable.
#
# INPUTS (global variables):
#   - protocol_source: source protocol name (e.g., "alpha", "024")
#   - protocol_target: target protocol label (e.g., "t024", "tallinn")
#   - version: protocol version number (e.g., "024")
#   - is_snapshot: whether to create hashed protocol
#   - short_hash: protocol hash (if known)
#
# OUTPUTS (sets global variables):
#   - capitalized_label: variant name (e.g., "Tallinn" or "T")
#   - capitalized_source: source variant name
#   - tezos_protocol_source: protocol name with dashes
#   - long_hash, short_hash: initialized if not set
#
function compute_protocol_names() {
  # Compute capitalized label
  capitalized_label=$(tr '[:lower:]' '[:upper:]' <<< "${label:0:1}")${label:1}

  # Compute capitalized source
  if [[ ${command} == "copy" ]]; then
    capitalized_source=$(tr '[:lower:]' '[:upper:]' <<< "${source_label:0:1}")${source_label:1}
  else
    capitalized_source=$(tr '[:lower:]' '[:upper:]' <<< "${protocol_source:0:1}")${protocol_source:1}
  fi

  # Protocol name with dashes instead of underscores
  tezos_protocol_source=$(echo "${protocol_source}" | tr '_' '-')

  # Initialize hashes if not set
  long_hash=${long_hash:-}
  short_hash=${short_hash:-}

  log_blue "=== Protocol Naming ==="
  log_blue "Label:              ${label}"
  log_blue "Capitalized label:  ${capitalized_label}"
  log_blue "Capitalized source: ${capitalized_source}"
  log_blue "Tezos protocol:     ${tezos_protocol_source}"
}

# COMMIT 1: Copy protocol source directory
#
# MODE: both stabilise and snapshot
# RENAMES: proto_${protocol_source} → proto_${version}
#   Example: proto_alpha → proto_024
#
# DESCRIPTION:
#   Copies the source protocol directory to a new directory named after
#   the protocol version. Uses git archive to only copy versioned files.
#   Removes auto-generated dune files.
#
# CREATES: 1 commit: "src: copy from ${protocol_source}"
function commit_01_copy_source() {
  if [[ $skip_copy_source ]]; then
    log_cyan "Skipping copy_source step"
    return 0
  fi

  log_blue "Copying src/proto_${protocol_source} to src/proto_${version}"

  # Use git archive to copy only versioned files
  mkdir /tmp/tezos_proto_snapshot
  git archive HEAD "src/proto_${protocol_source}/" | tar -x -C /tmp/tezos_proto_snapshot
  mv "/tmp/tezos_proto_snapshot/src/proto_${protocol_source}" "src/proto_${version}"
  rm -rf /tmp/tezos_proto_snapshot

  # Remove auto-generated dune files
  find "src/proto_${version}" -name dune \
    -exec grep -q "; This file was automatically generated, do not edit." {} \; \
    -exec rm {} \;

  commit_no_hooks "src: copy from ${protocol_source}"
}

# COMMIT 2: Set protocol version strings
#
# MODE: both stabilise and snapshot
# RENAMES: version strings in constants_repr.ml, raw_context.ml, proxy.ml
#   Example: "alpha_current" → "t_024" or "tallinn"
#
# DESCRIPTION:
#   Updates the version_value constant in multiple files to reflect the
#   new protocol name. For snapshot mode, uses exact match. For stabilise,
#   uses pattern match to handle suffixes like "alpha_current".
#
# CREATES: 1 commit: "src: set current version"
function commit_02_set_version() {
  log_blue "Setting current version in raw_context and proxy"

  local pattern

  if [[ ${is_snapshot} == true ]]; then
    # Exact match for snapshot
    pattern="s/let version_value = \"${protocol_source}\"/let version_value = \"${protocol_target}\"/"
  else
    # Pattern match for stabilise (handles alpha_current, etc.)
    pattern="s/let version_value = \"${protocol_source}.*\"/let version_value = \"${protocol_target}\"/"
  fi

  sed -i.old.old -e "${pattern}" \
    "src/proto_${version}/lib_protocol/constants_repr.ml" \
    "src/proto_${version}/lib_protocol/raw_context.ml" \
    "src/proto_${version}/lib_client/proxy.ml"

  commit_no_hooks "src: set current version"
}

# COMMIT 3: Adapt protocol predecessors
#
# MODE: both stabilise and snapshot
# RENAMES: predecessor variants in raw_context.ml, init_storage.ml
#   Example: Alpha → T024 or T
#
# DESCRIPTION:
#   Updates the previous_protocol variant and migration logic to reference
#   the new protocol. Replaces capitalized_source with capitalized_label.
#
# CREATES: 1 commit: "src: adapt ${capitalized_label} predecessors"
function commit_03_adapt_predecessors() {
  log_blue "Adapting ${capitalized_label} predecessors"

  cd "src/proto_${version}/lib_protocol"

  # Update predecessor check in raw_context.ml
  local replace_line="else if Compare.String.(s = \"${label}\") then return (${capitalized_label}, ctxt)"
  sed -i.old -e "s/.*return (${capitalized_source}, ctxt).*/${replace_line}/" raw_context.ml

  # Replace all occurrences of old variant with new
  sed -e "s/${capitalized_source}/${capitalized_label}/g" -i.old raw_context.ml
  sed -e "s/${capitalized_source}/${capitalized_label}/g" -i.old raw_context.mli
  sed -e "s/${capitalized_source}/${capitalized_label}/g" -i.old init_storage.ml

  ocamlformat -i raw_context.ml raw_context.mli init_storage.ml

  cd ../../..

  commit_no_hooks "src: adapt ${capitalized_label} predecessors"
}

# COMMIT 4: Compute hash and rename to hashed protocol
# MODE: vanity nonce (snapshot only), hash update (both), rename (snapshot only)
# RENAMES: proto_024 → proto_024_PtXXXXXX (snapshot only)
#
# DESCRIPTION:
#   Computes the protocol hash and optionally adds vanity nonce.
#   Updates TEZOS_PROTOCOL file with the computed hash.
#   Renames directory to include hash suffix.
#
# CREATES: 2-3 commits:
#   - "src: add vanity nonce" (optional, if vanity nonce provided)
#   - "src: replace ${protocol_source} hash with ${label} hash"
#   - "src: rename proto_${version} to proto_${version}_${short_hash}"
function commit_04_compute_hash_and_rename() {
  # Vanity nonce handling (snapshot only)
  if [[ ${is_snapshot} == true ]]; then

    log_blue "Computing protocol hash"

    # Vanity nonce handling (interactive)
    echo "Current hash is: ${long_hash}"
    echo "If you want to change it use a third party hasher and update nonce"
    sed -i 's/Vanity nonce: .* /Vanity nonce: 0000000000000000 /' "src/proto_${version}/lib_protocol/main.ml"

    echo -e "Dumping protocol source into ${blue}proto_to_hash.txt${reset}"
    ./octez-protocol-compiler -dump-only "src/proto_${version}/lib_protocol/" > proto_to_hash.txt
    sed -i 's/Vanity nonce: 0000000000000000/Vanity nonce: TBD/' "src/proto_${version}/lib_protocol/main.ml"

    echo "For instance using the following command:"
    printf "${yellow}seq 1 8 | parallel --line-buffer ${red}<path_to_hasher> ${blue}proto_to_hash.txt ${magenta}%s${reset}\n" "$(echo "${label}" | cut -c 1-6)"
    echo "Please insert vanity nonce or press enter to continue with the current hash"
    echo -e "${yellow}${blinking}Vanity nonce: ${reset}\c"
    read -r nonce

    if [[ -n ${nonce} ]]; then
      sed -i.old -e "s/Vanity nonce: TBD/Vanity nonce: ${nonce}/" "src/proto_${version}/lib_protocol/main.ml"

      long_hash=$(./octez-protocol-compiler -hash-only "src/proto_${version}/lib_protocol")
      short_hash=$(echo "${long_hash}" | head -c 8)
      log_magenta "New hash computed: ${long_hash}"
      log_magenta "Short hash: ${short_hash}"

      echo "New hash is: ${long_hash}"
      echo "Press y to continue or n to stop"
      read -r continue
      if [[ ${continue} != "y" ]]; then
        print_and_exit 1 "${LINENO}"
      fi
      rm -f proto_to_hash.txt
      commit_no_hooks "src: add vanity nonce"

      # Recompute names now that we have new hash
      recompute_names
    fi
  fi

  # Update TEZOS_PROTOCOL with real hash
  cd "src/proto_${version}/lib_protocol"
  sed -i.old -e 's/"hash": "[^"]*",/"hash": "'"${long_hash}"'",/' TEZOS_PROTOCOL
  cd ../../..
  commit_no_hooks "src: replace ${protocol_source} hash with ${label} hash"

  # Rename directory to include hash (snapshot only)
  if [[ ${is_snapshot} == true ]]; then
    log_blue "Renaming src/proto_${version} to src/proto_${new_protocol_name}"

    if [[ -d "src/proto_${new_protocol_name}" ]]; then
      error "'src/proto_${new_protocol_name}' already exists, you should remove it"
      print_and_exit 1 "${LINENO}"
    fi

    git mv "src/proto_${version}" "src/proto_${new_protocol_name}"
    commit_no_hooks "src: rename proto_${version} to proto_${new_protocol_name}"
  fi
}

# COMMIT 5: Rename binary files
#
# MODE: both
# RENAMES: main_*_${protocol_source}.ml{,i} → main_*_${new_protocol_name}.ml{,i}
#
# DESCRIPTION:
#   Renames all main_*.ml and main_*.mli files in the protocol directory
#   to use the new protocol name instead of the source protocol name.
#   This affects binary entry point files.
#
# CREATES: 1 commit: "src: rename binaries main_*.ml{,i} files"
function commit_05_rename_binaries() {
  cd "src/proto_${new_protocol_name}"
  # rename main_*.ml{,i} files of the binaries
  find . -name main_\*_"${protocol_source}".ml -or -name main_\*_"${protocol_source}".mli | while read -r file; do
    new_file=${file//_"${protocol_source}"/_"${new_protocol_name}"}
    git mv "${file}" "${new_file}"
  done
  commit_no_hooks "src: rename binaries main_*.ml{,i} files"
}

# COMMIT 6: Update protocol references in lib_protocol
#
# MODE: both
# RENAMES: protocol_${protocol_source} → protocol_${new_protocol_name}
#          protocol-${tezos_protocol_source} → protocol-${new_tezos_protocol}
#          protocol-functor-${tezos_protocol_source} → protocol-functor-${new_tezos_protocol}
#
# DESCRIPTION:
#   Updates all protocol references in lib_protocol files using sed.
#   Replaces protocol identifiers with underscore and hyphen variants,
#   then reformats all OCaml files with ocamlformat.
#
# CREATES: 1 commit: "src: replace protocol_${protocol_source} with protocol_${new_protocol_name}"
function commit_06_update_protocol_references() {
  cd lib_protocol
  # We use `--print0` and `xargs -0` instead of just passing the result
  # of find to sed in order to support spaces in filenames.
  find . -type f -print0 | xargs -0 \
    sed -i.old -e "s/protocol_${protocol_source}/protocol_${new_protocol_name}/" \
    -e "s/protocol-${tezos_protocol_source}/protocol-${new_tezos_protocol}/" \
    -e "s/protocol-functor-${tezos-protocol-source}/protocol-functor-${new_tezos_protocol}/"
  find . -type f -name "*.ml" -exec ocamlformat -i {} \;
  find . -type f -name "*.mli" -exec ocamlformat -i {} \;
  commit_no_hooks "src: replace protocol_${protocol_source} with protocol_${new_protocol_name}"
}

# COMMIT 7: Add protocol to final_protocol_versions
#
# MODE: both (but different behavior per mode)
# MODIFIES: lib_protocol_compiler/final_protocol_versions
#
# DESCRIPTION:
#   Updates the immutable protocol versions list.
#   - snapshot mode: replaces source_hash with long_hash
#   - stabilise/copy mode: appends long_hash to the file
#   Then changes directory back to repository root.
#
# CREATES: 1 commit: "src: add protocol to final_protocol_versions"
function commit_07_update_final_protocol_versions() {
  # add this protocol to the immutable list
  if [[ ${is_snapshot} == true ]]; then
    sed -e "s/${source_hash}/${long_hash}/" -i ../../lib_protocol_compiler/final_protocol_versions
  else
    printf "%s\n" "${long_hash}" >> ../../lib_protocol_compiler/final_protocol_versions
  fi
  commit_no_hooks "src: add protocol to final_protocol_versions"
  cd ../../..
}

# COMMIT 8: Update or remove README
#
# MODE: conditional (different behavior per mode)
# MODIFIES: src/proto_${new_protocol_name}/README.md or src/proto_${label}/README.md
#
# DESCRIPTION:
#   Handles README file based on command mode:
#   - copy mode: copies alpha README and renames protocol references (2 commits)
#   - snapshot mode: removes README (1 commit)
#   - stabilise mode: renames protocol references in existing README (1 commit)
#
# CREATES: 1-2 commits depending on mode
function commit_08_update_readme() {
  if [[ ${command} == "copy" ]]; then
    cp "src/proto_alpha/README.md" "src/proto_${label}/README.md"
    commit_no_hooks "src: copy alpha/README"
    sed -i "s/alpha/${label}/g" "src/proto_${label}/README.md"
    commit_no_hooks "src: rename protocol in the README"
  elif [[ ${is_snapshot} == true ]]; then
    rm "src/proto_${new_protocol_name}/README.md"
    commit_no_hooks "src: remove README"
  else
    sed -i "s/${protocol_source}/${label}/g" "src/proto_${label}/README.md"
    commit_no_hooks "src: rename protocol in the README"
  fi
}

# COMMIT 9: Link protocol in manifest
#
# MODE: conditional (different behavior per mode)
# MODIFIES: manifest/product_octez.ml
#
# DESCRIPTION:
#   Links the new protocol in manifest/product_octez.ml:
#   - snapshot mode: replaces source protocol line with versioned+hash entry
#   - copy mode: inserts new protocol entry before alpha
#   - stabilise mode: inserts new protocol entry before source protocol
#   Then formats the manifest file with ocamlformat.
#
# CREATES: 1 commit: "manifest: link protocol in the node, client and codec"
function commit_09_link_in_manifest() {
  echo -e "\e[33mLinking protocol in the node, client and codec\e[0m"
  if [[ ${is_snapshot} == true ]]; then
    sed "s/let _${protocol_source} = active .*/  let _${version}_${short_hash} = active (Name.v \"${short_hash}\" ${version})\n/" -i manifest/product_octez.ml
  elif [[ ${command} == "copy" ]]; then
    sed "/let alpha = active (Name.dev \"alpha\")/i \  let _${label} = active (Name.dev \"${label}\")\n" -i manifest/product_octez.ml
  else
    sed "/let *${protocol_source} = active (Name.dev \"${protocol_source}\")/i \  let _${label} = active (Name.dev \"${label}\")\n" -i manifest/product_octez.ml
  fi
  ocamlformat -i manifest/product_octez.ml
  commit_no_hooks "manifest: link protocol in the node, client and codec"
}

# Remove auto-generated source protocol files (snapshot only)
#
# MODE: snapshot only
# REMOVES: dune files, opam files, devtools files for source protocol
#
# DESCRIPTION:
#   In snapshot mode, removes auto-generated files from the source protocol:
#   - Auto-generated dune files in src/proto_${protocol_source}
#   - All opam files matching *-${protocol_source}*.opam
#   - Devtools files: get_delegates, get_contracts, tool files
#   Does nothing in stabilise or copy modes.
#   These files are not committed here; they will be part of the manifest commit.
#
# CREATES: 0 commits (file removal only)
function remove_autogenerated_protocol_files() {
  if [[ ${is_snapshot} == true ]]; then
    # find all dune files in src/proto_${protocol_source} and remove them
    find "src/proto_${protocol_source}" -name dune -exec grep -q "; This file was automatically generated, do not edit." {} \; -exec rm {} \;
    # remove all proto_${protocol_source} related opam files
    find . -name "*-${protocol_source}*.opam" -exec rm {} \;
    # commit "src: remove proto_${protocol_source} related opam files"
    rm -f "devtools/yes_wallet/get_delegates_${protocol_source}.ml"
    rm -f "devtools/get_contracts/get_contracts_${protocol_source}.ml"
    rm -f "devtools/testnet_experiment_tools/tool_${protocol_source}.ml"
  fi
}

# COMMIT 10: Run make manifest
#
# MODE: both
# MODIFIES: manifest-generated files, devtools
#
# DESCRIPTION:
#   Runs the manifest build process which regenerates all build files
#   based on the manifest configuration. Also reformats devtools OCaml files.
#   This includes any file removals done by remove_autogenerated_protocol_files().
#
# CREATES: 1 commit: "manifest: make manifest"
function commit_10_make_manifest() {
  remove_autogenerated_protocol_files
  log_blue "Make manifest"
  make -C manifest
  find devtools -name '*.ml' -exec ocamlformat -i {} \;
  commit_no_hooks "manifest: make manifest"
}

# Assert that ${version} and ${label} are already defined
function update_hashes() {
  if [[ -n "${long_hash}" && -n "${short_hash}" ]]; then
    log_cyan "Hashes already known"
    log_cyan "Long hash: ${long_hash}"
    log_cyan "Short hash: ${short_hash}"
  elif [ -e "src/proto_${version}/lib_protocol" ]; then
    log_cyan "Computing hash"
    long_hash=$(./octez-protocol-compiler -hash-only "src/proto_${version}/lib_protocol")
    short_hash=$(echo "${long_hash}" | head -c 8)
    log_magenta "Long hash computed: ${long_hash}"
    log_magenta "Short hash: ${short_hash}"
    log_cyan "Updating protocol name and version variables"
    recompute_names
  else
    log_magenta "Can't find src/proto_${version}/lib_protocol"
    exit 1
  fi
}

function copy_source() {

  commit_01_copy_source

  if [[ ${command} == "copy" ]]; then
    protocol_source_original="${protocol_source}"
    #use first part of protocol_source + source_label as new protocol_source (e.g. 023_PtStockholm + stockholm -> stockholm_023)
    protocol_source=$(echo "${protocol_source}" | cut -d'_' -f1)
    protocol_source="${source_label}_${protocol_source}"
    log_blue "protocol_source is now ${protocol_source}"
  fi

  commit_02_set_version

  commit_03_adapt_predecessors

  if [[ ${command} == "copy" ]]; then
    sed -i 's/Vanity nonce: .* /Vanity nonce: TBD /' "src/proto_${version}/lib_protocol/main.ml"
    commit_if_changes "src: restore default vanity nonce"
  fi

  update_hashes

  commit_04_compute_hash_and_rename

  if [[ ${command} == "copy" ]]; then
    protocol_source="${protocol_source_original}"
  fi

  commit_05_rename_binaries

  commit_06_update_protocol_references

  commit_07_update_final_protocol_versions

  commit_08_update_readme

  commit_09_link_in_manifest

  commit_10_make_manifest

  if [[ ${is_snapshot} == true ]]; then
    warning "${protocol_source} has been unlinked in the manifest,  removing it from the source code"
    rm -rf "src/proto_${protocol_source}"
    commit_no_hooks "src: remove proto_${protocol_source}"
  fi

  ## update agnostic_baker
  ## add protocol as active before alpha in parameters.ml
  if ! grep -q "${long_hash}" src/lib_agnostic_baker/parameters.ml; then
    ## look for "ProtoALphaALphaALphaALphaALphaALphaALphaALphaDdp3zK" and add "${longhash};"
    sed -i.old "/ \"ProtoALphaALphaALphaALphaALphaALphaALphaALphaDdp3zK\"/i\"${long_hash}\"; " src/lib_agnostic_baker/parameters.ml
    ocamlformat -i src/lib_agnostic_baker/parameters.ml
    commit "src: add protocol to agnostic_baker"
  fi

}

function update_protocol_tests() {

  if [[ $skip_update_protocol_tests ]]; then
    echo "Skipping protocol tests update"
    return 0
  fi

  # Update protocol tests

  # Replace test invocation headers that mention protocol_source
  find "src/proto_${new_protocol_name}" -type f -path \*/test/\*.ml \
    -exec sed -i "s@Invocation:\(.*\)/proto_${protocol_source}/\(.*\)@Invocation:\1/proto_${new_protocol_name}/\2@" \{\} \;
  commit_no_hooks "tests: fix test invocation headers"

  #Replace all occurences of \[capitalized_protocol_source\] with \[capitalized_label\] in src_proto_${new_protocol_name}
  find "src/proto_${new_protocol_name}" -type f -exec sed -i "s/\\[${capitalized_source}\\]/\\[${capitalized_label}\\]/g" {} \;
  commit_no_hooks "tests: fix tests registrations"

  #update scoru_wasm protocol_migration tests
  # add proto_${label} to proto_name before Proto_alpha -> "Proto_alpha"
  if [[ ${is_snapshot} == true ]]; then
    sed -e "s/ ${capitalized_source} -> \"${capitalized_source}\"/ ${capitalized_label} -> \"${capitalized_label}\"/" -i.old src/lib_scoru_wasm/test/test_protocol_migration.ml
    sed -r "s/\((V.*, V.*,) ${capitalized_source}\);/\(\1 ${capitalized_label});/" -i.old src/lib_scoru_wasm/test/test_protocol_migration.ml
    ocamlformat -i src/lib_scoru_wasm/test/test_protocol_migration.ml

    sed -e "s/let proto_${protocol_source}_name = .*/let proto_${label}_name = \"${label}_${version}\"/" -i.old src/lib_scoru_wasm/constants.ml
    ocamlformat -i src/lib_scoru_wasm/constants.ml

    sed -e "s/${capitalized_source}/ ${capitalized_label}/g" -i.old src/lib_scoru_wasm/pvm_input_kind.ml
    sed -e "s/${capitalized_source}/ ${capitalized_label}/g" -i.old src/lib_scoru_wasm/pvm_input_kind.mli
    sed -e "s/${protocol_source}/${label}/g" -i.old src/lib_scoru_wasm/pvm_input_kind.ml
    ocamlformat -i src/lib_scoru_wasm/pvm_input_kind.ml
    ocamlformat -i src/lib_scoru_wasm/pvm_input_kind.mli

    sed -r "s/${capitalized_source} -> (V.*)/${capitalized_label} -> \1/" -i.old src/lib_scoru_wasm/wasm_vm.ml
    ocamlformat -i src/lib_scoru_wasm/wasm_vm.ml

  else
    sed "/Proto_alpha -> \"Proto_alpha\"/i \  | ${capitalized_label} -> \"${capitalized_label}\"" -i.old src/lib_scoru_wasm/test/test_protocol_migration.ml
    if [[ ${command} == "copy" ]]; then
      sed -r "s/\((V.*, V.*,) ${capitalized_source}\);/ \(\1 ${capitalized_source}\); \(\1 ${capitalized_label});/" -i.old src/lib_scoru_wasm/test/test_protocol_migration.ml
    else
      sed -r "s/\((V.*, V.*,) Proto_${protocol_source}\);/ \(\1 Proto_${protocol_source}\); \(\1 ${capitalized_label});/" -i.old src/lib_scoru_wasm/test/test_protocol_migration.ml
    fi
    ocamlformat -i src/lib_scoru_wasm/test/test_protocol_migration.ml

    sed -r "s/(type protocol =.*)/\1 | ${capitalized_label}/" -i.old src/lib_scoru_wasm/pvm_input_kind.ml
    sed -r "s/(type protocol =.*)/\1 | ${capitalized_label}/" -i.old src/lib_scoru_wasm/pvm_input_kind.mli
    if [[ ${command} == "copy" ]]; then
      sed "/let proto_${source_label}_name = .*/i \let proto_${label}_name = \"${label}\"" -i.old src/lib_scoru_wasm/constants.ml
      sed "/| payload when String.equal payload Constants.proto_${source_label}_name ->/i \  | payload when String.equal payload Constants.proto_${label}_name -> Some (Protocol_migration $capitalized_label)" -i.old src/lib_scoru_wasm/pvm_input_kind.ml
      sed -r "s/(Data_encoding.\(Binary.to_string_exn string Constants.proto_${source_label}_name\))/\1 | ${capitalized_label} ->Data_encoding.(Binary.to_string_exn string Constants.proto_${label}_name)/" -i.old src/lib_scoru_wasm/pvm_input_kind.ml
      sed -r "s/${capitalized_source} -> (V.*)/ ${capitalized_source} -> \1 | ${capitalized_label} -> \1/" -i.old src/lib_scoru_wasm/wasm_vm.ml
    else
      sed "/let proto_${protocol_source}_name = .*/i \let proto_${label}_name = \"${label}\"" -i.old src/lib_scoru_wasm/constants.ml
      sed "/| payload when String.equal payload Constants.proto_${protocol_source}_name ->/i \  | payload when String.equal payload Constants.proto_${label}_name -> Some (Protocol_migration $capitalized_label)" -i.old src/lib_scoru_wasm/pvm_input_kind.ml
      sed -r "s/(Data_encoding.\(Binary.to_string_exn string Constants.proto_${protocol_source}_name\))/\1 | ${capitalized_label} ->Data_encoding.(Binary.to_string_exn string Constants.proto_${label}_name)/" -i.old src/lib_scoru_wasm/pvm_input_kind.ml
      sed -r "s/Proto_${protocol_source} -> (V.*)/ Proto_${protocol_source} -> \1 | ${capitalized_label} -> \1/" -i.old src/lib_scoru_wasm/wasm_vm.ml
    fi
    ocamlformat -i src/lib_scoru_wasm/constants.ml
    ocamlformat -i src/lib_scoru_wasm/pvm_input_kind.ml
    ocamlformat -i src/lib_scoru_wasm/pvm_input_kind.mli
    ocamlformat -i src/lib_scoru_wasm/wasm_vm.ml
  fi

  commit_no_hooks "scoru: update scoru_wasm protocol_migration"

  if [[ ${command} == "copy" ]]; then
    sed -e "s/Proto_${protocol_source}/${capitalized_label}/g" \
      -e "s/\(Protocol_migration ${capitalized_source}\)/(Protocol_migration ${capitalized_label})/g" \
      -e "s/Tezos_scoru_wasm.Constants.proto_${source_label}_name/Tezos_scoru_wasm.Constants.proto_${label}_name/g" \
      -i.old "src/proto_${new_protocol_name}/lib_protocol/test/unit/test_sc_rollup_wasm.ml"
  else
    sed -e "s/Proto_${protocol_source}/${capitalized_label}/g" \
      -e "s/\(Protocol_migration ${capitalized_source}\)/(Protocol_migration ${capitalized_label})/g" \
      -e "s/Tezos_scoru_wasm.Constants.proto_${protocol_source}_name/Tezos_scoru_wasm.Constants.proto_${label}_name/g" \
      -i.old "src/proto_${new_protocol_name}/lib_protocol/test/unit/test_sc_rollup_wasm.ml"
  fi
  ocamlformat -i "src/proto_${new_protocol_name}/lib_protocol/test/unit/test_sc_rollup_wasm.ml"
  commit_no_hooks "sc_rollup: update proto_${new_protocol_name}/test/unit/test_sc_rollup_wasm"

}

function update_source() {

  if [[ $skip_update_source ]]; then
    echo "Skipping source update"
    return 0
  fi

  log_blue "update teztale"
  #  Teztale
  source_short_hash=$(echo "${protocol_source}" | cut -d'_' -f2)
  if [[ ${is_snapshot} == true ]]; then
    git mv "teztale/bin_teztale_archiver/${source_short_hash}_machine.real.ml" "teztale/bin_teztale_archiver/${short_hash}_machine.real.ml"
    sed -e "s/${protocol_source}/${new_protocol_name}/g" -i.old "teztale/bin_teztale_archiver/${short_hash}_machine.real.ml"
    sed -e "s/${protocol_source}/${version}/g" \
      -e "s/${capitalized_source}/${short_hash}/g" -i.old "teztale/bin_teztale_archiver/teztale_archiver_main.ml"
    ocamlformat -i "teztale/bin_teztale_archiver/${short_hash}_machine.real.ml"
  else
    cp "teztale/bin_teztale_archiver/${source_short_hash}_machine.real.ml" "teztale/bin_teztale_archiver/${label}_machine.real.ml"
    git add "teztale/bin_teztale_archiver/${label}_machine.real.ml"
    sed -e "s/${protocol_source}/${label}/g" -i.old "teztale/bin_teztale_archiver/${label}_machine.real.ml"
    # sed -e "s/${protocol_source}/${version}/g" \
    #   -e "s/${capitalized_source}/${capitalized_label}/g" -i.old "teztale/bin_teztale_archiver/teztale_archiver_main.ml"
    sed -e "/module Malpha = Alpha_machine.M/i \ module M${label} = ${capitalized_label}_machine.M" -i.old "teztale/bin_teztale_archiver/teztale_archiver_main.ml"

    ocamlformat -i "teztale/bin_teztale_archiver/${label}_machine.real.ml"
  fi
  ocamlformat -i "teztale/bin_teztale_archiver/teztale_archiver_main.ml"
  commit_if_changes "teztale: update teztale_archiver_main.ml"

  if [[ ${is_snapshot} == false ]]; then
    log_blue "update proto_alpha constants_parametric_previous_repr.ml"
    # Previous parametrics constants are the same in Alpha and Beta, so it is correct to just replace the Alpha previous one by the Alpha current one
    cp src/proto_alpha/lib_protocol/constants_parametric_repr.ml src/proto_alpha/lib_protocol/constants_parametric_previous_repr.ml
    cp src/proto_alpha/lib_protocol/constants_parametric_repr.mli src/proto_alpha/lib_protocol/constants_parametric_previous_repr.mli
    # Remove comment that is meant for constants_parametric_repr and should not be copied to constants_parametric_previous_repr.
    perl -0777 -i -pe 's/\(\*\* Protocol-specific constants\..*?\*\)//s' src/proto_alpha/lib_protocol/constants_parametric_previous_repr.mli
    ocamlformat -i src/proto_alpha/lib_protocol/constants_parametric_previous_repr.mli
  fi

  log_blue "update raw_context.ml"
  # add  "else if Compare.String.(s = "$label") then return ($capitalized_label, ctxt)" before else Lwt.return @@ storage_error (Incompatible_protocol_version s)
  #sed "/else Lwt.return @@ storage_error (Incompatible_protocol_version s)/i \  else if Compare.String.(s = \"${label}\") then return (${capitalized_label}, ctxt)" -i.old "src/proto_${protocol_source}/lib_protocol/raw_context.ml"

  start_source="\(\* Start of ${capitalized_source} stitching. Comment used for automatic snapshot \*\)"
  end_source="\(\* End of ${capitalized_source} stitching. Comment used for automatic snapshot \*\)"
  start_predecessor="\(\* Start of alpha predecessor stitching. Comment used for automatic snapshot \*\)"
  end_predecessor="\(\* End of alpha predecessor stitching. Comment used for automatic snapshot \*\)"
  type_to_remove="\(\* Alpha predecessor \*\)"

  start_remove="\(\* Start of code to remove at next automatic protocol snapshot \*\)"
  remove_comment="\(\* Please add here any code that should be removed at the next automatic protocol snapshot \*\)"
  end_remove="\(\* End of code to remove at next automatic protocol snapshot \*\)"

  start_typechecker="\(\* This line is only here to please the typechecker\,"
  end_typechecker="let\*\! c = get_previous_protocol_constants ctxt in"

  log_blue "fix prepare_first_block"

  if [[ ${is_snapshot} == true ]] || [[ ${command} == "copy" ]]; then
    if [[ ${command} == "copy" ]]; then
      protocol_source_original="${protocol_source}"
      #use first part of protocol_source + source_label as new protocol_source (e.g. 023_PtStockholm + stockholm -> stockholm_023)
      protocol_source=$(echo "${protocol_source}" | cut -d'_' -f1)
      protocol_source="${source_label}_${protocol_source}"
      log_blue "protocol_source is now ${protocol_source}"

      sed -i.old -e "s/s = \"${protocol_source}\"/s = \"${label}\"/g" \
        "src/proto_alpha/lib_protocol/raw_context.ml"
      protocol_source="${protocol_source_original}"
    else
      sed -i.old -e "s/s = \"${protocol_source}\"/s = \"${label}_${version}\"/g" \
        "src/proto_alpha/lib_protocol/raw_context.ml"
    fi
    sed -i.old -e "s/${capitalized_source}/${capitalized_label}/g" \
      -e "s/${protocol_source}/${label}/g" "src/proto_alpha/lib_protocol/raw_context.ml"
    ocamlformat -i "src/proto_alpha/lib_protocol/raw_context.ml"
    sed -i.old -e "s/${capitalized_source}/${capitalized_label}/g" \
      -e "s/${protocol_source}/${label}/g" "src/proto_alpha/lib_protocol/raw_context.mli"
    ocamlformat -i "src/proto_alpha/lib_protocol/raw_context.mli"
    sed -i.old -e "s/${capitalized_source}/${capitalized_label}/g" \
      -e "s/${protocol_source}/${label}/g" "src/proto_alpha/lib_protocol/init_storage.ml"
    ocamlformat -i "src/proto_alpha/lib_protocol/init_storage.ml"
    commit "alpha: add ${capitalized_label} as Alpha previous protocol"
  else
    prepare_first_block=$(sed -n "/${start_source}/,/${end_source}/p" "src/proto_alpha/lib_protocol/raw_context.ml")
    # shellcheck disable=SC2001
    prepare_first_block_patched=$(sed "s/${capitalized_source} stitching/alpha predecessor stitching/g" <<< "${prepare_first_block}")
    # shellcheck disable=SC2001
    prepare_first_block_patched=$(sed "s/${capitalized_source}/${capitalized_label}/g" <<< "${prepare_first_block_patched}")
    # shellcheck disable=SC2001
    prepare_first_block_patched=$(sed "s/let module Previous = Constants_parametric_repr in/let module Previous = Constants_parametric_previous_repr in/" <<< "${prepare_first_block_patched}")
    prepare_first_block_patched=$(perl -0777 -pe "s/${start_typechecker}.*${end_typechecker}//s" <<< "${prepare_first_block_patched}")
    # shellcheck disable=SC2001
    prepare_first_block_patched=$(sed -e "s/let\* c = get_constants ctxt in/let*! c = get_previous_protocol_constants ctxt in/" <<< "${prepare_first_block_patched}")
    escaped_prepare_first_block=$(printf '%s\n' "$prepare_first_block_patched" | sed 's/[`~!@#$%^&*()-_=+{}\|;:",<.>/?]/\\&/g')
    #replace all multiline code between $start_predecessor and $end_predecessor with the content of prepare_first_block_patched in src/proto_alpha/lib_protocol/raw_context.ml using perl
    perl -0777 -pe "s/${start_predecessor}.*${end_predecessor}/${escaped_prepare_first_block}/s" -i "src/proto_alpha/lib_protocol/raw_context.ml"
    # remove all code between $start_remove and $end_remove in src/proto_alpha/lib_protocol/raw_context.ml and init_storage.ml
    perl -0777 -pe "s/${start_remove}.*${end_remove}/${start_remove}\n\n${remove_comment}\n\n${end_remove}\n/s" -i "src/proto_alpha/lib_protocol/raw_context.ml"
    perl -0777 -pe "s/${start_remove}.*${end_remove}/${start_remove}\n\n${remove_comment}\n\n${end_remove}\n/s" -i "src/proto_alpha/lib_protocol/init_storage.ml"
    #replace code between "$type_to_remove' and '$type_to_remove' with capitalized_label in src/proto_alpha/lib_protocol/raw_context.ml
    perl -0777 -pe "s/${type_to_remove}[ \t]+[a-zA-Z0-9_]+[ \t]+${type_to_remove}/${type_to_remove}${capitalized_label}${type_to_remove}/" -i "src/proto_alpha/lib_protocol/raw_context.ml"
    perl -0777 -pe "s/${type_to_remove}[ \t]+[a-zA-Z0-9_]+[ \t]+${type_to_remove}/${type_to_remove}${capitalized_label}${type_to_remove}/" -i "src/proto_alpha/lib_protocol/raw_context.mli"
    replace_string="Compare.String.(s = \"${label}\") then return (${capitalized_label}, ctxt)"
    #replace  code between "$type_to_remove' and '$type_to_remove' with $replace_string in src/proto_alpha/lib_protocol/raw_context.ml
    perl -0777 -pe "s/${type_to_remove}[ \t]+Compare.*${type_to_remove}/${type_to_remove}$replace_string${type_to_remove}/s" -i "src/proto_alpha/lib_protocol/raw_context.ml"

    ocamlformat -i "src/proto_alpha/lib_protocol/raw_context.ml"
    ocamlformat -i "src/proto_alpha/lib_protocol/raw_context.mli"

    prepare_first_block=$(sed -n "/${start_source}/,/${end_source}/p" "src/proto_alpha/lib_protocol/init_storage.ml")
    # shellcheck disable=SC2001
    prepare_first_block_patched=$(echo "${prepare_first_block}" | sed "s/${capitalized_source} stitching/alpha predecessor stitching/g")
    # shellcheck disable=SC2001
    prepare_first_block_patched=$(echo "${prepare_first_block_patched}" | sed "s/${capitalized_source}/${capitalized_label}/g")
    escaped_prepare_first_block=$(printf '%s\n' "$prepare_first_block_patched" | sed 's/[`~!@#$%^&*()-_=+{}\|;:",<.>/?]/\\&/g')
    #replace all code between '(* Start of alpha predecessor stitching. Used for automatic protocol snapshot *)' and '(* End of alpha predecessor stitching. Used for automatic protocol snapshot *)' with the content of prepare_first_block_patched in src/proto_alpha/lib_protocol/raw_context.ml
    perl -0777 -pe "s/${start_predecessor}.*${end_predecessor}/${escaped_prepare_first_block}/s" -i "src/proto_alpha/lib_protocol/init_storage.ml"
    ocamlformat -i "src/proto_alpha/lib_protocol/init_storage.ml"
    commit "Alpha: add ${capitalized_label} as Alpha previous protocol"
  fi

}

function generate_regression_test() {
  log_blue "generate regression test"
  # shellcheck disable=SC2312
  # shellcheck disable=SC1003
  printf '
(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) %s Nomadic Labs <contact@nomadic-labs.com>                  *)
(*                                                                           *)
(*****************************************************************************)

(* Testing
   -------
   Component:    Protocol
   Invocation:   dune exec tezt/tests/main.exe -- --file check_proto_%s_changes.ml
   Subject:      Ensure protocol_%s has not changed
*)

let register () =
  Regression.register
    ~__FILE__
    ~title:"Check that the %s protocol has not changed"
    ~tags:[Tag.layer1; "protocol"; "%s"]
    ~uses_node:false
    ~uses_client:false
    ~uses_admin_client:false
  @@ fun _protocol ->
  (* Check that md5sum of all files in src/proto_%s is consistent *)
    let _, _ =
    Unix.open_process
      "mkdir /tmp/tezos_proto_snapshot && git archive HEAD src/proto_%s | tar -x -C /tmp/tezos_proto_snapshot"
  in
  let ic, _ =
    Unix.open_process
      "find /tmp/tezos_proto_snapshot -type f -exec md5sum {} %s; | sort -k 2 | md5sum"
  in
  let _, _ = Unix.open_process "rm -rf /tmp/tezos_proto_snapshot" in

  let output = input_line ic in
  Regression.capture output ;
  return ()' "$(date +%Y)" "${label}" "${label}" "${label}" "${label}" "${label}" "${label}" '\\' > "tezt/tests/check_proto_${label}_changes.ml"
  ocamlformat -i "tezt/tests/check_proto_${label}_changes.ml"
  git add "tezt/tests/check_proto_${label}_changes.ml"

  sed -i.old -e "s/let register_protocol_independent_tests () =/let register_protocol_independent_tests () =\n  Check_proto_${label}_changes.register ();/" tezt/tests/main.ml
  ocamlformat -i tezt/tests/main.ml
  commit "tezt: generate regression test"
}

function update_tezt_tests() {

  if [[ $skip_update_tezt_tests ]]; then
    echo "Skipping tezt tests update"
    return 0
  fi

  # ensure protocols compile and parameter files are generated
  make

  # Update tezt tests

  # automatically add the new protocol tag to alcotezt

  if [[ ${is_snapshot} == true ]]; then
    sed -i.old -e "s/| Some \"${protocol_source}\" -> \[\"${protocol_source}\"\]/| Some \"${new_protocol_name}\" -> [\"${label}\"]"/ tezt/lib_alcotezt/alcotezt_utils.ml
    commit "tezt: update protocol tag in alcotezt"
  else
    temp_file=$(mktemp)
    tac tezt/lib_alcotezt/alcotezt_utils.ml | tail -n +2 | tac > "${temp_file}"
    echo "  | Some \"${new_protocol_name}\" -> [\"${label}\"]" >> "${temp_file}"
    echo "  | Some _ -> assert false" >> "${temp_file}"
    mv "${temp_file}" tezt/lib_alcotezt/alcotezt_utils.ml
    commit "tezt: add new protocol tag to alcotezt"
  fi

  cd "${script_dir}"/..

  log_blue "Adapt tezt/lib_tezos/protocol.ml"

  if [[ ${is_snapshot} == true ]]; then
    sed -e "s/${capitalized_source} -> \"P[ts].*\"/${capitalized_label} -> \"${long_hash}\"/g" -i.old tezt/lib_tezos/protocol.ml
    sed -e "s/| ${capitalized_source} -> \"${protocol_source}\"//g" -i.old tezt/lib_tezos/protocol.ml
    sed -i.old -e "s/proto_${protocol_source}/proto_${new_protocol_name}/g" tezt/lib_tezos/protocol.ml
    sed -i.old -e "s/${capitalized_source}/${capitalized_label}/g" tezt/lib_tezos/protocol.ml
    sed -i.old -e "s/${protocol_source}/${label}/g" tezt/lib_tezos/protocol.ml
    sed -i.old -e "s/${capitalized_source}/${capitalized_label}/g" tezt/lib_tezos/protocol.mli
  else
    sed -i.old -e "s/type t = / type t =  ${capitalized_label} | /g" \
      -e "s/${capitalized_source} -> \"${capitalized_source}\"/${capitalized_source} -> \"${capitalized_source}\" | ${capitalized_label} -> \"${capitalized_label}\"/g" \
      -e "s/${capitalized_source} -> \"${protocol_source}\"/${capitalized_source} -> \"${protocol_source}\" | ${capitalized_label} -> \"${label}\"/g" \
      -e "s/(\"alpha\", Alpha);/(\"alpha\", Alpha); (\"${label}\", ${capitalized_label});/g" \
      tezt/lib_tezos/protocol.ml
    sed -i.old -e "s/let all = \[/let all = [ ${capitalized_label};/" tezt/lib_tezos/protocol.ml

    #source=Alpha; echo "let number = function ParisC -> 020 | Alpha -> 021" |sed -r 's/(.*) '$source' -> ([0-9][0-9][0-9])/printf "\1 Beta -> \2 | '$source' -> %03i" "$(echo \2+1 | bc)"/ge'
    # shellcheck disable=SC2086
    # shellcheck disable=SC2016
    if [[ ${command} == "copy" ]]; then
      sed -i.old -e "s/Alpha -> \"alpha\"/Alpha -> \"alpha\" | ${capitalized_label} -> \"${label}\"/g" tezt/lib_tezos/protocol.ml
      sed -r 's/(.*) '${capitalized_source}' -> ([0-9][0-9][0-9])/\1 '${capitalized_source}' -> \2 | '${capitalized_label}' -> \2/' -i.old tezt/lib_tezos/protocol.ml
    else
      sed -r 's/(.*) '${capitalized_source}' -> ([0-9][0-9][0-9])/printf "\1 '${capitalized_label}' -> \2 | '${capitalized_source}' -> %03i" "$(echo \2+1 | bc)"/ge' -i.old tezt/lib_tezos/protocol.ml
    fi
    sed "/| ${capitalized_source} -> \"proto_${protocol_source}\"/a | ${capitalized_label} -> \"proto_${label}\"\n" -i.old tezt/lib_tezos/protocol.ml

    # add $(capitalized_label) -> "$long_hash" before "(* DO NOT REMOVE, AUTOMATICALLY ADD STABILISED PROTOCOL HASH HERE *)"
    sed "/\(\* DO NOT REMOVE, AUTOMATICALLY ADD STABILISED PROTOCOL HASH HERE \*\)/i \ | ${capitalized_label} -> \"${long_hash}\"" -i.old tezt/lib_tezos/protocol.ml

    if [[ ${command} == "copy" ]]; then
      sed -r "s/(.*) Alpha -> Some (.*)/\1 Alpha -> Some ${capitalized_label}/g" -i.old tezt/lib_tezos/protocol.ml
      sed -r "s/(.*) ${capitalized_source} -> Some (.*)/\1 ${capitalized_source} -> Some \2 | ${capitalized_label} -> Some \2/g" -i.old tezt/lib_tezos/protocol.ml
    else
      sed -r "s/(.*) ${capitalized_source} -> Some (.*)/\1 ${capitalized_source} -> Some ${capitalized_label} | ${capitalized_label} -> Some \2/g" -i.old tezt/lib_tezos/protocol.ml
    fi
    sed -i.old -e "s/type t = / type t =  ${capitalized_label} | /g" tezt/lib_tezos/protocol.mli
  fi
  ocamlformat -i tezt/lib_tezos/protocol.ml
  ocamlformat -i tezt/lib_tezos/protocol.mli
  commit "tezt: adapt lib_tezos/protocol.ml"

  # TODO: fix and reintroduce this test
  #generate_regression_test

  #fix testnets_scenarios:
  if [[ ${is_snapshot} == true ]]; then
    sed -e "s/Protocol.${capitalized_source}/Protocol.${capitalized_label}/g" -i src/bin_testnet_scenarios/*.ml
  else
    sed -r "s/(.*) Protocol.${capitalized_source} -> (.*)/ \1 Protocol.${capitalized_source} -> \2 | Protocol.${capitalized_label} -> \2/g" -i src/bin_testnet_scenarios/*.ml
  fi
  ocamlformat -i src/bin_testnet_scenarios/*.ml
  commit_if_changes "tezt: fix testnets_scenarios"

  #fix other tests:
  if [[ ${is_snapshot} == true ]]; then
    sed -e "s/Protocol.${capitalized_source}/Protocol.${capitalized_label}/g" -i tezt/tests/*.ml
  else
    sed -r "s/(.*) Protocol.${capitalized_source} -> (.*)/ \1 Protocol.${capitalized_source} -> \2 | Protocol.${capitalized_label} -> \2/g" -i tezt/tests/*.ml
  fi
  ocamlformat -i tezt/tests/*.ml
  commit "tezt: fix other tests"

  mkdir -p "tezt/tests/encoding_samples/${label}"
  if [[ ${is_snapshot} == true ]]; then
    git mv tezt/tests/encoding_samples/"${protocol_source}"/* tezt/tests/encoding_samples/"${label}"
    commit "tezt: move ${protocol_source} encoding samples to ${label}"
  elif [[ ${command} == "copy" ]]; then
    cp -r tezt/tests/encoding_samples/"${source_label}"/* tezt/tests/encoding_samples/"${label}"
    commit "tezt: move ${source_label} encoding samples to ${label}"
  else
    cp -r tezt/tests/encoding_samples/"${protocol_source}"/* tezt/tests/encoding_samples/"${label}"
    commit "tezt: copy ${protocol_source} encoding samples to ${label}"
  fi

  regression_protocol_name="${capitalized_label}-"
  regression_source_name="${capitalized_source}-"
  alpha_regression="Alpha-"

  # for regression files, protocol_name should be at least 5 character long, if not add enough trailing '-' at the end
  #regression_protocol_name=${capitalized_label}
  while [[ ${#regression_protocol_name} -le 5 ]]; do
    regression_protocol_name="${regression_protocol_name}-"
  done

  #regression_source_name=${capitalized_source}
  while [[ ${#regression_source_name} -le 5 ]]; do
    regression_source_name="${regression_source_name}-"
  done

  search_name=""
  if [[ ${is_snapshot} == true ]]; then
    search_name="*${regression_source_name}*.out"
  else
    search_name="*${alpha_regression}*.out"
  fi

  # shellcheck disable=SC2312
  # shellcheck disable=SC2001
  find . -type f -name "${search_name}" | while read -r FILE; do
    if [[ ${is_snapshot} == true ]]; then
      ORIG_FILENAME=${FILE}
      NEW_FILENAME=$(echo "${FILE}" | sed "s/${regression_source_name}/${regression_protocol_name}/g")
    else
      ORIG_FILENAME=$(echo "${FILE}" | sed "s/${alpha_regression}/${regression_source_name}/g")
      NEW_FILENAME=$(echo "${FILE}" | sed "s/${alpha_regression}/${regression_protocol_name}/g")
    fi

    # Create the directory structure for the new file if it doesn't exist
    mkdir -p "$(dirname "${NEW_FILENAME}")"

    # extract filename from NEW_FILENAME wihout extension
    filename=$(basename "${NEW_FILENAME}" .out)
    # NEW_FILENAME should be less than 80 characters
    filename="${filename:0:80}"

    orig_filename=$(basename "${ORIG_FILENAME}" .out)
    orig_filename="${orig_filename:0:80}"
    orig_filename=$(dirname "${ORIG_FILENAME}")/"${orig_filename}".out
    NEW_FILENAME=$(dirname "${NEW_FILENAME}")/"${filename}".out

    if [[ ${is_snapshot} == true ]]; then
      git mv "${FILE}" "${NEW_FILENAME}"
    else
      # Preserve the file permissions
      cp -p "${FILE}" "${NEW_FILENAME}"
    fi
    #replace all occurences of protocol_source with label
    if [[ ${is_snapshot} == true ]]; then
      # Replace `proto_${protocol_source}` with `proto_${version}-${short_hash}`
      sed -i.old -E "s/proto_${protocol_source}/proto_${version}-${short_hash}/g" "${NEW_FILENAME}"

      # Replace `${protocol_source}.` with `${version}-${short_hash}.`
      sed -i.old -E "s/${protocol_source}\./${version}-${short_hash}\./g" "${NEW_FILENAME}"

      # Replace `${protocol_source}` with `${label}` unless it's in a JSON key (avoid any pattern in a string with `"` before and after and followed by `:`)
      perl -i.old -pe 's/\b$ENV{protocol_source}\b(?![^:]*":)/$ENV{label}/g' "$ENV{NEW_FILENAME}"

    else
      sed -i.old -e "s/${protocol_source}/${label}/g" "${NEW_FILENAME}"
    fi
    if [[ ${command} == "copy" ]]; then
      sed -i.old -e "s/${version}-${short_hash}/${label}/g" "${NEW_FILENAME}"
    fi

    #replace all occurences of old hash with new hash
    sed -i.old -e "s/${source_hash}/${long_hash}/g" "${NEW_FILENAME}"
  done
  if [[ ${is_snapshot} == true ]]; then
    commit "tezt: move ${protocol_source} regression files"
  else
    commit "tezt: copy ${protocol_source} regression files"
  fi

  # add new protocol baker
  # this can be removed once https://gitlab.com/tezos/tezos/-/issues/7763 has been tackled
  if [[ ${is_snapshot} == false ]]; then
    awk -v source="$protocol_source" -v target="$protocol_target" '
$0 ~ "let _octez_baker_" source " =" {
  orig1 = $0
  getline
  orig2 = $0
  print gensub(source, target, "g", orig1)
  print gensub(source, target, "g", orig2)
  print ""
  print orig1
  print orig2
  next
}
1
' tezt/lib_tezos/constant.ml > tezt/lib_tezos/constant.ml.tmp
    mv tezt/lib_tezos/constant.ml.tmp tezt/lib_tezos/constant.ml
  else
    lowercase_shorthash=$(echo "$short_hash" | tr '[:upper:]' '[:lower:]')
    awk -v source="$protocol_source" -v short_hash="$short_hash" -v lowercase_shorthash="$lowercase_shorthash" '
$0 ~ "let _octez_baker_" source " =" {
  print                  # line 1: unchanged
  getline                # go to line 2
  gsub("baker_" source, "baker_" lowercase_shorthash)
  gsub("./octez-baker-" source , "./octez-baker-" short_hash)
  print
  next
}
1
' tezt/lib_tezos/constant.ml > tezt/lib_tezos/constant.ml.tmp
    mv tezt/lib_tezos/constant.ml.tmp tezt/lib_tezos/constant.ml
  fi

  ocamlformat -i tezt/lib_tezos/constant.ml
  commit_if_changes "tezt: add unused ${protocol_target} baker"

  # add new protocol accuser
  # this can be removed once https://gitlab.com/tezos/tezos/-/issues/7763 has been tackled
  if [[ ${is_snapshot} == false ]]; then
    awk -v source="$protocol_source" -v target="$protocol_target" '
$0 ~ "let _octez_accuser_" source " =" {
  orig1 = $0
  getline
  orig2 = $0
  print gensub(source, target, "g", orig1)
  print gensub(source, target, "g", orig2)
  print ""
  print orig1
  print orig2
  next
}
1
' tezt/lib_tezos/constant.ml > tezt/lib_tezos/constant.ml.tmp
    mv tezt/lib_tezos/constant.ml.tmp tezt/lib_tezos/constant.ml
  else
    lowercase_shorthash=$(echo "$short_hash" | tr '[:upper:]' '[:lower:]')
    awk -v source="$protocol_source" -v short_hash="$short_hash" -v lowercase_shorthash="$lowercase_shorthash" '
$0 ~ "let _octez_accuser_" source " =" {
  print                  # line 1: unchanged
  getline                # go to line 2
  gsub("baker_" source, "baker_" lowercase_shorthash)
  gsub("./octez-accuser-" source , "./octez-accuser-" short_hash)
  print
  next
}
1
' tezt/lib_tezos/constant.ml > tezt/lib_tezos/constant.ml.tmp
    mv tezt/lib_tezos/constant.ml.tmp tezt/lib_tezos/constant.ml
  fi

  ocamlformat -i tezt/lib_tezos/constant.ml
  commit_if_changes "tezt: add unused ${protocol_target} accuser"

  # mkdir -p "tezt/tests/expected/check_proto_${label}_changes.ml"
  # rm -rf /tmp/tezos_proto_snapshot
  # mkdir -p /tmp/tezos_proto_snapshot
  # git archive HEAD "src/proto_${new_protocol_name}/" | tar -x -C /tmp/tezos_proto_snapshot
  # find /tmp/tezos_proto_snapshot -type f -exec md5sum {} \; | sort -k 2 | md5sum >"tezt/tests/expected/check_proto_${label}_changes.ml/Check that the ${label} protocol has not changed.out"
  # rm -rf /tmp/tezos_proto_snapshot
  # if [[ -n ${git} ]]; then
  #   git add "tezt/tests/expected/check_proto_${label}_changes.ml"
  # fi
  # commit "tezt: add expected output for stabilisation regression test"

  dune exec tezt/tests/main.exe -- --on-unknown-regression-files delete
  commit_if_changes "tezt: delete unknown regression files"

  dune exec tezt/tests/main.exe -- --title 'meta: list runtime dependencies' --reset-regressions
  commit "tezt: reset runtime dependencies regressions"

  if [[ ${is_snapshot} != true ]]; then
    cp tezt/tests/weeklynet_configs/alpha.json tezt/tests/weeklynet_configs/last_snapshotted_protocol.json
    dune exec tezt/tests/main.exe -- --file tezt/tests/weeklynet.ml --reset-regressions
    commit_if_changes "tezt: reset weeklynet regression test"
  fi

}

function misc_updates() {

  if [[ $skip_misc_updates ]]; then
    echo "Skipping miscellaneous updates"
    return 0
  fi

  # Misc. updates

  log_blue "Update kaitai structs"
  make check-kaitai-struct-files || log_blue "updated kaitai files"
  make kaitai-struct-files-update
  if [[ ${is_snapshot} == true ]]; then
    rm -rf "client-libs/kaitai-struct-files/files/${protocol_source}*"
  fi
  commit_if_changes "kaitai: update structs"

  log_blue "add octez-activate-${label} command to client sandbox"

  if [[ ${is_snapshot} == true ]]; then
    sed "/let protocol_${protocol_source}_parameters_template =/,/^$/d" -i devtools/testnet_experiment_tools/testnet_experiment_tools.ml
    sed -i.old -e "s/${capitalized_source}/${capitalized_label}/g" \
      -e "s/protocol_${protocol_source}_parameters_template/(Filename.concat network_parameters_templates_dir \"proto_${version}_${short_hash}_mainnet.json\")/" \
      devtools/testnet_experiment_tools/testnet_experiment_tools.ml
  else
    sed "/let protocol_${protocol_source}_parameters_template =/i \ let protocol_${label}_parameters_template =\n  Filename.current_dir_name \/\/ \"src\" \/\/ \"proto_${label}\" \/\/ \"parameters\"\n  \/\/ \"mainnet_parameters.json\"" -i.old devtools/testnet_experiment_tools/testnet_experiment_tools.ml
    sed "/| Tezt_tezos.Protocol.${capitalized_source} ->/i \  | Tezt_tezos.Protocol.${capitalized_label} ->\n Some protocol_${label}_parameters_template" -i.old devtools/testnet_experiment_tools/testnet_experiment_tools.ml
  fi
  ocamlformat -i devtools/testnet_experiment_tools/testnet_experiment_tools.ml
  commit "devtools: update testnet_experiment_tools"

  if [[ ${is_snapshot} == true ]]; then
    # update linter to remove special rule for stabilised protocol
    sed -i.old -e "s/ -not -name \"proto_${protocol_source}\"//" scripts/lint.sh
    commit "scripts: update linter to remove special rule for ${protocol_source} protocol"
  else
    # update linter to allow reformating of beta protocol
    sed -i.old -e "s/-not -name \"proto_alpha\"/-not -name \"proto_${label}\" -not -name \"proto_alpha\"/" scripts/lint.sh
    commit "scripts: update linter to allow reformating of ${label} protocol"
  fi

  find . -name '*.old' -exec rm {} \;
  scripts/lint.sh --update-ocamlformat || echo "updating ocamlformat files"
  scripts/lint.sh --check-ocamlformat || echo "linting updated ocamlformat files"
  commit_if_changes "scripts: lint"

  log_blue "Update ci"
  make -C ci
  commit "ci: regenerate ci"

}

function generate_doc() {

  if [[ $skip_generate_doc ]]; then
    echo "Skipping doc generation"
    return 0
  fi

  if [[ ${command} == "copy" ]]; then
    doc_path="${source_label}"
  else
    doc_path="${protocol_source}"
  fi

  # Create docs
  if [[ ${is_snapshot} == true ]]; then
    git mv "docs/${doc_path}" "docs/${label}"
    commit "docs: move from docs/${doc_path}"
  else
    echo "Copying docs/${doc_path} to docs/${label}"
    rm -rf /tmp/tezos_proto_doc_snapshot
    mkdir /tmp/tezos_proto_doc_snapshot
    git archive HEAD "docs/${doc_path}/" | tar -x -C /tmp/tezos_proto_doc_snapshot
    mv "/tmp/tezos_proto_doc_snapshot/docs/${doc_path}" "docs/${label}"
    rm -rf /tmp/tezos_proto_doc_snapshot
    commit "docs: copy from docs/${doc_path}"
  fi
  # fix versioned links (in labels, references, and paths) in docs
  echo "Fixing versioned links in docs"
  cd "docs/${label}"
  if [[ ${command} == "copy" ]]; then
    find . -name \*.rst -exec \
      sed -i.old \
      -e "s,src/proto_${protocol_source},src/proto_${new_protocol_name},g" \
      -e "s,tezos-protocol-${tezos_protocol_source}/,tezos-protocol-${new_tezos_protocol}/,g" \
      -e "s,raw_protocol_${protocol_source}/,raw_protocol_${new_protocol_name}/,g" \
      -e "s/_${source_label}:/_${label}:/g" \
      -e "s/_${source_label}>/_${label}>/g" \
      -e "s/_${source_label}\`/_${label}\`/g" \
      -e "s/-${source_label}.html/-${label}.html/g" \
      \{\} \;
  else
    find . -name \*.rst -exec \
      sed -i.old \
      -e "s,src/proto_${protocol_source},src/proto_${new_protocol_name},g" \
      -e "s,tezos-protocol-${tezos_protocol_source}/,tezos-protocol-${new_tezos_protocol}/,g" \
      -e "s,raw_protocol_${protocol_source}/,raw_protocol_${new_protocol_name}/,g" \
      -e "s/_${protocol_source}:/_${label}:/g" \
      -e "s/_${protocol_source}>/_${label}>/g" \
      -e "s/_${protocol_source}\`/_${label}\`/g" \
      -e "s/-${protocol_source}.html/-${label}.html/g" \
      \{\} \;
  fi
  commit "docs: fix versioned links"

  cd ../..

  if [[ ${command} == "copy" ]]; then
    protocol_source_original="${protocol_source}"
    #use first part of protocol_source + source_label as new protocol_source (e.g. 023_PtStockholm + stockholm -> stockholm_023)
    protocol_source=$(echo "${protocol_source}" | cut -d'_' -f1)
    protocol_source="${protocol_source}_${source_label}"
    log_blue "protocol_source is now ${protocol_source}"
  fi

  # generate docs/protocols/${new_versioned_name}.rst from docs/protocols/${protocol_source}.rst
  echo "Copying docs/protocols/${protocol_source}.rst to docs/protocols/${new_versioned_name}.rst"
  if [[ ${is_snapshot} == true ]]; then
    git mv "docs/protocols/${protocol_source}.rst" "docs/protocols/${new_versioned_name}.rst"
    commit "docs: move docs/protocols/${protocol_source}.rst to docs/protocols/${new_versioned_name}.rst"
  else
    cp "docs/protocols/${protocol_source}.rst" "docs/protocols/${new_versioned_name}.rst"
    commit "docs: copy docs/protocols/${protocol_source}.rst to docs/protocols/${new_versioned_name}.rst"
  fi
  if [[ ${command} == "copy" ]]; then
    sed -e "s/^Protocol ${capitalized_source}/Protocol ${capitalized_label}/" \
      -e "s/protocol ${capitalized_source}/protocol ${capitalized_label}/" \
      -e "s,src/proto_${protocol_source},src/proto_${new_protocol_name},g" \
      -e "s/${source_label}/${label}/g" \
      -i "docs/protocols/${new_versioned_name}.rst"
    protocol_source="${protocol_source_original}"
  fi
  uncapitalized_source=$(echo "${protocol_source}" | tr '[:upper:]' '[:lower:]')
  sed -e "s/^Protocol ${capitalized_source}/Protocol ${capitalized_label}/" \
    -e "s/protocol ${capitalized_source}/protocol ${capitalized_label}/" \
    -e "s,src/proto_${protocol_source},src/proto_${new_protocol_name},g" \
    -e "s/${capitalized_source}/${capitalized_label}/g" \
    -e "s/${uncapitalized_source}/${label}/g" \
    -i "docs/protocols/${new_versioned_name}.rst"

  if [[ ${#label} -gt 5 ]]; then
    # add (#label-5) '=' characters to the second line
    missing_equals='='
    for ((i = 0; i < ${#label} - 5; i++)); do
      missing_equals="${missing_equals}="
    done
    sed -i.old -e "2s/$/${missing_equals}/" "docs/protocols/${new_versioned_name}.rst"
  fi

  commit_if_changes "docs: fix docs/protocols/${new_protocol_name}.rst"

  alpha_rst "${capitalized_label}" > "docs/protocols/alpha.rst"
  commit "docs: reset docs/protocols/alpha.rst"

  # add entries in the doc index for the snaptshotted protocol by
  # pattern-matching some existing lines and inserting variations thereof
  echo "Add entries in the doc index"
  doc_index="docs/index.rst"

  if [[ ${is_snapshot} == true ]]; then
    # we are snapshotting, simply replace all occurences of ${protocol_source} with ${label} in the index
    sed -i.old -e "s@protocols/${protocol_source}@protocols/${new_versioned_name}@g" "${doc_index}"
    sed -i.old -e "s/${capitalized_source}/${capitalized_label}/g" "${doc_index}"
    sed -i.old -e "s/${protocol_source}/${label}/g" "${doc_index}"
  else
    awk -v PATTERN1="Alpha Dev Protocol Reference <${protocol_source}/index>" \
      -v REPLACEMENT1="${capitalized_label} Protocol Reference <${label}/index>" \
      -v PATTERN2="protocols/alpha" \
      -v REPLACEMENT2="protocols/${new_versioned_name}" '{
        if ($0 ~ PATTERN1) {
            x=$0
            sub(PATTERN1,REPLACEMENT1)
            print
            print x
        } else if ($0 ~ PATTERN2) {
            x=$0
            sub(PATTERN2,REPLACEMENT2)
            print
            print x
        } else {
          print
        }
     }' < "${doc_index}" > "${doc_index}.tmp"
    mv "${doc_index}.tmp" "${doc_index}"
  fi
  commit "docs: add entries in the doc index"

  # update docs Makefile
  if [[ ${is_snapshot} == true ]]; then
    sed -i.old -r "s/(NAMED_PROTOS .*)/\1 ${label}/" docs/Makefile
    sed -i.old -r "s/(PROTOCOLS .*) ${protocol_source}/\1/" docs/Makefile
    sed -i.old -e "s/-l ${protocol_source}/${label}/g" docs/Makefile
    line=$(printf "%s_short%*s= %s" "${label}" $((8 - ${#label})) '' "$short_hash")
    sed -e "s/${protocol_source}_short .*/${line}/" \
      -e "s/ ${protocol_source}\/rpc\.rst//g" \
      -e "s/${protocol_source}/${label}/g" \
      -e "s/${source_hash}/${long_hash}/g" \
      -i docs/Makefile
  else
    sed -i.old -e "s/xrefscheck:/xrefscheck:\n\t\$\(CHECKXREFS\) -l ${label}/g" docs/Makefile
    sed -i.old -r "s/(PROTOCOLS .*) alpha/\1 ${protocol_source} ${label}/" docs/Makefile
    line=$(printf "%s_long%*s= %s" "${label}" $((10 - ${#label})) '' "$long_hash")
    sed "/alpha_long .*/i${line}" -i docs/Makefile
    line=$(printf "%s_short%*s= %s" "${label}" $((8 - ${#label})) '' "$label")
    sed "/alpha_short .*/i${label}_short = ${label}" -i docs/Makefile
    sed -i.old -e "/[#>]/! s/alpha\/rpc\.rst/alpha\/rpc.rst ${label}\/rpc.rst/g" docs/Makefile
    sed -i.old -r "s/alpha\/octez-\*\.html/alpha\/octez-*.html ${label}\/octez-*.html/g" docs/Makefile
  fi
  commit "docs: update docs Makefile"

  rm -f "docs/${label}/rpc.rst"
  make -C docs "${label}"/rpc.rst
  commit "docs: generate ${label}/rpc.rst"

}

function snapshot_protocol() {
  compute_protocol_names

  if [[ ${is_snapshot} == true ]]; then
    # by default only propose to use one character values for variant and label
    expected_variant="${capitalized_label:0:1}"
    log_blue "Current expected variant is ${magenta}${expected_variant}${reset}"
    read -r -p "Please enter the new variant or press enter to keep this one: " -e -i "${expected_variant}" new_variant
    expected_label="${label:0:1}"
    echo -e "Current expected tag is ${magenta}${expected_label}${reset}"
    read -r -p "Please enter the new tag or press enter to keep this one: " -e -i "${expected_label}" new_tag
    # new_tag must use only lowercase digits and underscores
    while [[ ! "${new_tag}" =~ ^[a-z0-9_]+$ ]]; do
      echo "Tag must use only lowercase digits and underscores"
      read -r -p "Please enter the new tag or press enter to keep the current one: " -e -i "${previous_tag}" new_tag
    done
    capitalized_new_tag=$(tr '[:lower:]' '[:upper:]' <<< "${new_tag:0:1}")${new_tag:1}
    label="${new_tag}"
    capitalized_label="${capitalized_new_tag}"

    tezos_protocol_source=$(echo "${protocol_source}" | tr '_' '-')

    short_hash=$(echo "${protocol_source}" | cut -d'_' -f2)
    if [[ "${is_snapshot}" == false ]]; then
      if [[ ${source_short_hash} != "${short_hash}" ]]; then
        is_snapshot=false
      else
        is_snapshot=true
      fi
    fi
    label="${new_tag}"
    capitalized_label="${capitalized_new_tag}"

    protocol_target="${label}_${version}"

  fi

  sanity_check_before_script

  copy_source

  update_hashes

  update_protocol_tests

  update_source

  update_tezt_tests

  misc_updates

  generate_doc

}

function delete_from_build() {
  warning "SHORT_HASH = ${short_hash}"

  # remove line containing  "let _${protocol_source} = active" from manifest/product_octez.ml and following line if empty
  sed -i.old -e "/let _${protocol_source} = active/,+1d" "manifest/product_octez.ml"
  commit "manifest: remove ${protocol_source} from product_octez.ml"
  find "src/proto_${protocol_source}" -name dune -exec grep -q "; This file was automatically generated, do not edit." {} \; -exec rm {} \;
  find . -name "*-${short_hash}*.opam" -exec rm {} \;
  rm -f "devtools/yes_wallet/get_delegates_${protocol_source}.ml"
  rm -f "devtools/get_contracts/get_contracts_${protocol_source}.ml"
  rm -f "devtools/testnet_experiment_tools/tool_${protocol_source}.ml"
  make -C manifest
  commit "manifest: make manifest"
}

function remove_proto_source() {
  # remove src/proto_${protocol_source} directory
  rm -rf "src/proto_${protocol_source}"
  commit "src: remove src/proto_${protocol_source}"
}

function remove_from_tezt_tests() {
  # remove line containing  "${protocol_source} -> ${source_label}" from tezt/lib_alcotezt/alcotezt_utils.ml
  sed -i.old -e "/${protocol_source} */d" "tezt/lib_alcotezt/alcotezt_utils.ml"
  ocamlformat -i "tezt/lib_alcotezt/alcotezt_utils.ml"
  capitalized_source=$(tr '[:lower:]' '[:upper:]' <<< "${protocol_source:0:1}")${protocol_source:1}

  perl -i.old -pe "
    s/${capitalized_source} -> [0-9][0-9][0-9] \| //g;
    s/\| ${capitalized_source} -> [0-9][0-9][0-9]$//g;
    s/${capitalized_source} -> \".*\" \| //g;
    s/\| ${capitalized_source} -> \".*\"$//g;
    s/\| ${capitalized_label} -> .*\n//g;
    s/\| ${capitalized_label}//g;
    s/${capitalized_label} \|//g;
    s/; ${capitalized_label}//g;
    s/${capitalized_label}\;//g;
    s/; \(\"${source_label}\", ${capitalized_label}\)//g;
    s/\(\"${source_label}\", ${capitalized_label}\);//g;
" tezt/lib_tezos/protocol.ml
  sed -i.old \
    -e "s/| ${capitalized_label}//g" \
    -e "s/${capitalized_label} | //g" \
    tezt/lib_tezos/protocol.mli

  # if it exists a case of ${capitalized_label} -> Some ${capitalized_label} in
  # the file after the previous changes, show the +3-3 lines surrounding this to
  # the user and ask what to use instead
  if grep -q ".* -> Some ${capitalized_label}" tezt/lib_tezos/protocol.ml; then
    echo "Please replace 'Some ${capitalized_label}' in tezt/lib_tezos/protocol.ml"
    grep -C 3 ".* -> Some ${capitalized_label}" tezt/lib_tezos/protocol.ml
    # query the user for the Protocol to use instead and replace it
    read -r -p "What Protocol should be used instead? " -e -i "Alpha" replacing_protocol
    #ensure the user input is capitalized
    capitalized_replacing_protocol=$(tr '[:lower:]' '[:upper:]' <<< "${replacing_protocol:0:1}")${replacing_protocol:1}
    sed -i.old -e "s/Some ${capitalized_label}/Some ${capitalized_replacing_protocol}/g" tezt/lib_tezos/protocol.ml
    #extract VALUE from: line containing 'let version_value = "VALUE"' in src/proto_${replacing_protocol}/lib_protocol/constants_repr.ml
    replacing_version_value=$(grep "let version_value = " src/proto_*"${replacing_protocol}"*/lib_protocol/constants_repr.ml | cut -d'"' -f2)
    sed -i.old \
      -e "s/\"${source_label}\"/\"${replacing_version_value}\"/g" \
      -e "s/${capitalized_label}/${capitalized_replacing_protocol}/g" \
      src/proto_alpha/lib_protocol/raw_context.ml
    sed -i.old \
      -e "s/\"${source_label}\"/\"${replacing_version_value}\"/g" \
      -e "s/${capitalized_label}/${capitalized_replacing_protocol}/g" \
      src/proto_alpha/lib_protocol/raw_context.mli
    ocamlformat -i src/proto_alpha/lib_protocol/raw_context.mli
    sed -i.old \
      -e "s/\"${source_label}\"/\"${replacing_version_value}\"/g" \
      -e "s/${capitalized_label}/${capitalized_replacing_protocol}/g" \
      src/proto_alpha/lib_protocol/init_storage.ml
    ocamlformat -i src/proto_alpha/lib_protocol/init_storage.ml
  fi

  ocamlformat -i tezt/lib_tezos/protocol.ml
  ocamlformat -i tezt/lib_tezos/protocol.mli
  commit "tezt: adapt lib_tezos/protocol.ml"

  # if Protocol.${capitalized_label} is used in tezt/tests/protocol_migration.ml, ask what to use instead (default is Alpha)
  if grep -q "Protocol.${capitalized_label}" tezt/tests/protocol_migration.ml; then
    echo "Please replace 'Protocol.${capitalized_label}' in tezt/tests/protocol_migration.ml"
    grep -C 3 "Protocol.${capitalized_label}" tezt/tests/protocol_migration.ml
    # query the user for the Protocol to use instead and replace it
    read -r -p "What Protocol should be used instead? " -e -i "Alpha" replacing_protocol
    #ensure the user input is capitalized
    capitalized_replacing_protocol=$(tr '[:lower:]' '[:upper:]' <<< "${replacing_protocol:0:1}")${replacing_protocol:1}
    sed -i.old -e "s/Protocol.${capitalized_label}/Protocol.${capitalized_replacing_protocol}/g" tezt/tests/protocol_migration.ml
    ocamlformat -i tezt/tests/protocol_migration.ml
    commit_if_changes "tezt: adapt protocol_migration.ml"
  fi

  sed -i.old -e "/| Protocol.${capitalized_label} -> */d" src/bin_testnet_scenarios/upgrade_etherlink.ml
  sed -i.old -e "/| Protocol.${capitalized_label} -> */d" tezt/tests/sc_rollup_migration.ml
  sed -i.old -e "/| Protocol.${capitalized_label} -> */d" tezt/tests/sc_rollup.ml
  ocamlformat -i src/bin_testnet_scenarios/upgrade_etherlink.ml
  ocamlformat -i tezt/tests/sc_rollup_migration.ml
  ocamlformat -i tezt/tests/sc_rollup.ml
  commit "test: fix other tests"

  # rm -rf tezt/tests/encoding_samples/"${source_label}"
  # commit "tezt: rm ${source_label} encoding samples"

  dune exec tezt/tests/main.exe -- --on-unknown-regression-files delete
  commit_if_changes "tezt: delete unknown regression files"

  dune exec tezt/tests/main.exe -- --title 'meta: list runtime dependencies' --reset-regressions
  commit_if_changes "tezt: reset runtime dependencies regressions"

  if [[ ${is_snapshot} != true ]]; then
    dune exec tezt/tests/main.exe -- --file tezt/tests/protocol_migration.ml --title 'Alpha: weeklynet regression test' --reset-regressions
    commit_if_changes "tezt: reset weeklynet regression test"
  fi
}

function misc_removals() {

  log_blue "Update kaitai structs"
  make check-kaitai-struct-files || log_blue "updated kaitai files"
  make kaitai-struct-files-update
  if [[ ${is_snapshot} == true ]]; then
    rm -rf "client-libs/kaitai-struct-files/files/${protocol_source}*"
  fi
  commit_if_changes "kaitai: update structs"

  #if deleting a protocol in stabilisation
  if [[ ${source_label} == "${protocol_source}" ]]; then
    sed "/| Tezt_tezos.Protocol.${capitalized_label} -> */d" -i devtools/testnet_experiment_tools/testnet_experiment_tools.ml
  else

    sed -i.old -e "/| Tezt_tezos.Protocol.${capitalized_label} ->/,+4d" devtools/testnet_experiment_tools/testnet_experiment_tools.ml
  fi
  ocamlformat -i devtools/testnet_experiment_tools/testnet_experiment_tools.ml
  commit "devtools: update testnet_experiment_tools"

  if [[ ${source_label} == "${protocol_source}" ]]; then
    # update linter to remove special rule for stabilised protocol
    sed -i.old -e "s/-not -name \"proto_${protocol_source}\"//" scripts/lint.sh
    commit_if_changes "scripts: update linter to remove special rule for ${protocol_source} protocol"
  fi

  find . -name '*.old' -exec rm {} \;
  scripts/lint.sh --check-ocamlformat || echo "linting updated ocamlformat files"
  commit_if_changes "scripts: lint"

  sed -i.old \
    -e "/| ${capitalized_label}$/d" \
    -e "/| payload when String.equal payload Constants.proto_${source_label}_name ->/,+1d" \
    -e "/| ${capitalized_label} ->/,+1d" \
    "src/lib_scoru_wasm/pvm_input_kind.ml"
  ocamlformat -i "src/lib_scoru_wasm/pvm_input_kind.ml"

  sed -i.old \
    -e "/| ${capitalized_label}$/d" \
    -e "/| payload when String.equal payload Constants.proto_${source_label}_name ->/,+1d" \
    -e "/| ${capitalized_label} ->/,+1d" \
    "src/lib_scoru_wasm/pvm_input_kind.mli"
  ocamlformat -i "src/lib_scoru_wasm/pvm_input_kind.mli"

  sed -i.old \
    -e "/let proto_${source_label}_name .*/d" \
    "src/lib_scoru_wasm/constants.ml"
  ocamlformat -i "src/lib_scoru_wasm/constants.ml"

  sed -i.old -e "/| ${capitalized_label} -> V.*/d" src/lib_scoru_wasm/wasm_vm.ml
  ocamlformat -i src/lib_scoru_wasm/wasm_vm.ml
  ocamlformat -i "src/lib_scoru_wasm/wasm_vm.ml"

  sed -e "/${capitalized_label} -> \"${capitalized_label}\"/d" -i.old src/lib_scoru_wasm/test/test_protocol_migration.ml
  sed -e "/.*(V.*, V.*, ${capitalized_label});/d" -i.old src/lib_scoru_wasm/test/test_protocol_migration.ml
  ocamlformat -i src/lib_scoru_wasm/test/test_protocol_migration.ml

  commit "scoru: remove proto_${source_label}_name from scoru_wasm"

  sed -i.old -e "/.*${source_hash}.*/d" src/lib_protocol_compiler/final_protocol_versions
  commit "protocol_compiler: remove ${source_hash} from final_protocol_versions"

  log_blue "Update ci"
  make -C ci
  commit_if_changes "ci: regenerate ci"

}

function remove_docs() {
  rm -rf "docs/${source_label}"
  commit "docs: remove docs/${source_label}"

  if [[ ${source_label} == "${protocol_source}" ]]; then
    rm -f "docs/protocols/${source_label}.rst"
  else
    rm -f "docs/protocols/${version}_${source_label}.rst"
  fi
  commit "docs: remove docs/protocols/${source_label}.rst"

  # remove entries in the doc index
  if [[ ${source_label} == "${protocol_source}" ]]; then
    sed -i.old -e "s@protocols/${protocol_source}@@" "docs/index.rst"
  else
    sed -i.old -e "s@protocols/${version}_${source_label}@@" "docs/index.rst"
  fi
  commit "docs: remove entries in the doc index"

  # update docs Makefile
  if [[ ${source_label} == "${protocol_source}" ]]; then
    sed -i.old -e "s/(CHECKXREFS) -l ${source_label}/d" docs/Makefile
    sed -i.old -r "s/(PROTOCOLS .*) ${protocol_source}/\1/" docs/Makefile
  else
    sed -i.old -e "/(CHECKXREFS) ${source_label}/d" docs/Makefile
    sed -i.old -r "s/(PROTOCOLS .*) ${protocol_source}/\1/" docs/Makefile
  fi
  sed -i.old "/${source_label}_short .*/d" docs/Makefile
  sed -i.old "/${source_label}_long .*/d" docs/Makefile
  sed -i.old -e "s/${source_label}\/rpc\.rst//g" docs/Makefile
  sed -i.old -e "s/${source_label}\/octez-\*.html//g" docs/Makefile
  sed -i.old -e "s/${source_label}//g" docs/Makefile
  commit "docs: update docs Makefile"

  sed -i.old -e "/${capitalized_label} .*/d" docs/index.rst
  commit_if_changes "docs: remove ${capitalized_label} from docs/index.rst"
}

function delete_protocol() {
  warning "Deleting protocol ${source_label} from src/proto_${protocol_source}"

  sanity_check_before_script
  delete_from_build

  remove_proto_source

  remove_from_tezt_tests

  misc_removals

  remove_docs

  dune build tezt/tests/main.exe

  echo "Deletion done"
}

function update_files() {
  for file in "$@"; do
    log_blue "Update ${file}"
    sed -i.old \
      -e "s/${source_hash}/${long_hash}/g" \
      -e "s/${source_short_hash}/${short_hash}/g" \
      -e "s/proto_${protocol_source}/proto_${new_protocol_name}/g" \
      -e "s/${capitalized_source}/${capitalized_label}/g" \
      -e "s/${protocol_source}/${label}/g" \
      -e "s/${capitalized_previous_tag}/${capitalized_new_tag}/g" \
      -e "s/${previous_tag}/${new_tag}/g" \
      -e "s/${previous_variant}/${new_variant}/g" \
      "${file}"
    if [[ "${file}" == *.ml || "${file}" == *.mli ]]; then
      ocamlformat -i "${file}"
    fi
  done
}

function finalize_docs() {
  log_cyan "Finalizing documentation: renaming ${protocol_source} to ${doc_label}"

  # Extract version number - handle both "024" and "024_PtHash" formats
  if [[ "${protocol_source}" =~ ^[0-9][0-9][0-9]_ ]]; then
    version=$(echo "${protocol_source}" | cut -d'_' -f1)
  elif [[ "${protocol_source}" =~ ^[a-z]*[0-9][0-9][0-9]$ ]]; then
    # Extract digits from end (e.g., t024 -> 024)
    version=$(echo "${protocol_source}" | grep -o '[0-9][0-9][0-9]$')
  else
    # Assume the whole thing is the version
    version="${protocol_source}"
  fi

  # Capitalize the doc_label for display (e.g., tallinn -> Tallinn)
  capitalized_doc_label=$(tr '[:lower:]' '[:upper:]' <<< "${doc_label:0:1}")${doc_label:1}

  # Check if source docs directory exists
  if [[ ! -d "docs/${protocol_source}" ]]; then
    error "docs/${protocol_source}" "does not exist"
    print_and_exit 1 "${LINENO}"
  fi

  # Check if target docs directory already exists
  if [[ -d "docs/${doc_label}" ]]; then
    error "docs/${doc_label}" "already exists, you should remove it first"
    print_and_exit 1 "${LINENO}"
  fi

  # 1. Rename docs folder
  log_blue "Renaming docs/${protocol_source} to docs/${doc_label}"
  git mv "docs/${protocol_source}" "docs/${doc_label}"
  commit "docs: rename docs/${protocol_source} to docs/${doc_label}"

  # 2. Fix versioned links in docs
  log_blue "Fixing versioned links in docs/${doc_label}"
  cd "docs/${doc_label}"
  find . -name \*.rst -exec \
    sed -i.old \
    -e "s/_${protocol_source}:/_${doc_label}:/g" \
    -e "s/_${protocol_source}>/_${doc_label}>/g" \
    -e "s/_${protocol_source}\`/_${doc_label}\`/g" \
    -e "s/-${protocol_source}.html/-${doc_label}.html/g" \
    -e "s/protocol ${protocol_source}/protocol ${capitalized_doc_label}/g" \
    -e "s/Protocol ${protocol_source}/Protocol ${capitalized_doc_label}/g" \
    -e "s/protocol $(echo "${protocol_source}" | tr '[:lower:]' '[:upper:]')/protocol ${capitalized_doc_label}/g" \
    -e "s/Protocol $(echo "${protocol_source}" | tr '[:lower:]' '[:upper:]')/Protocol ${capitalized_doc_label}/g" \
    \{\} \;
  commit_if_changes "docs: fix versioned links in docs/${doc_label}"
  cd ../..

  # 3. Rename and update protocol changelog
  old_changelog="docs/protocols/${version}_${protocol_source}.rst"
  new_changelog="docs/protocols/${version}_${doc_label}.rst"

  if [[ -f "${old_changelog}" ]]; then
    log_blue "Renaming ${old_changelog} to ${new_changelog}"
    git mv "${old_changelog}" "${new_changelog}"
    commit "docs: rename ${old_changelog} to ${new_changelog}"

    log_blue "Updating ${new_changelog}"
    # Update title and references in changelog
    capitalized_protocol_source=$(tr '[:lower:]' '[:upper:]' <<< "${protocol_source:0:1}")${protocol_source:1}
    sed -i.old \
      -e "s/^Protocol ${capitalized_protocol_source}/Protocol ${capitalized_doc_label}/" \
      -e "s/^Protocol $(echo "${protocol_source}" | tr '[:lower:]' '[:upper:]')/Protocol ${capitalized_doc_label}/" \
      -e "s/protocol ${protocol_source}/protocol ${capitalized_doc_label}/g" \
      -e "s/Protocol ${protocol_source}/Protocol ${capitalized_doc_label}/g" \
      -e "s/protocol $(echo "${protocol_source}" | tr '[:lower:]' '[:upper:]')/protocol ${capitalized_doc_label}/g" \
      -e "s/Protocol $(echo "${protocol_source}" | tr '[:lower:]' '[:upper:]')/Protocol ${capitalized_doc_label}/g" \
      -e "s@\.\./${protocol_source}/index@../${doc_label}/index@g" \
      -e "s/_${protocol_source}_/_${doc_label}_/g" \
      -e "s/<${protocol_source}_/<${doc_label}_/g" \
      "${new_changelog}"
    commit_if_changes "docs: update ${new_changelog}"
  else
    warning "${old_changelog} does not exist, skipping"
  fi

  # 4. Update docs/index.rst
  log_blue "Updating docs/index.rst"
  capitalized_protocol_source=$(tr '[:lower:]' '[:upper:]' <<< "${protocol_source:0:1}")${protocol_source:1}
  sed -i.old \
    -e "s@${capitalized_protocol_source} Protocol Reference <${protocol_source}/index>@${capitalized_doc_label} Protocol Reference <${doc_label}/index>@g" \
    -e "s@$(echo "${protocol_source}" | tr '[:lower:]' '[:upper:]') Protocol Reference <${protocol_source}/index>@${capitalized_doc_label} Protocol Reference <${doc_label}/index>@g" \
    -e "s@protocols/${version}_${protocol_source}@protocols/${version}_${doc_label}@g" \
    docs/index.rst
  commit_if_changes "docs: update docs/index.rst"

  # 5. Update docs/introduction/breaking_changes.rst
  log_blue "Updating docs/introduction/breaking_changes.rst"
  if [[ -f "docs/introduction/breaking_changes.rst" ]]; then
    sed -i.old \
      -e "s/_${protocol_source}_/_${doc_label}_/g" \
      -e "s/<${protocol_source}_/<${doc_label}_/g" \
      -e "s/>${protocol_source}_/>${doc_label}_/g" \
      -e "s/\`${protocol_source}_/\`${doc_label}_/g" \
      -e "s/protocol ${protocol_source}/protocol ${capitalized_doc_label}/g" \
      -e "s/Protocol ${protocol_source}/Protocol ${capitalized_doc_label}/g" \
      -e "s/protocol $(echo "${protocol_source}" | tr '[:lower:]' '[:upper:]')/protocol ${capitalized_doc_label}/g" \
      -e "s/Protocol $(echo "${protocol_source}" | tr '[:lower:]' '[:upper:]')/Protocol ${capitalized_doc_label}/g" \
      -e "s@protocols/${version}_${protocol_source}@protocols/${version}_${doc_label}@g" \
      docs/introduction/breaking_changes.rst

    # Fix title underlines in docs/introduction/breaking_changes.rst
    perl -i.old -0pe 's/^(.+)\n([=\-^]+)$/my $title = $1; my $underline = $2; my $len = length $title; my $u_len = length $underline; if ($len != $u_len) { $underline = substr($underline, 0, 1) x $len; } "$title\n$underline"/gme' docs/introduction/breaking_changes.rst

    commit_if_changes "docs: update docs/introduction/breaking_changes.rst"
  fi

  # 6. Update docs/protocols/alpha.rst
  log_blue "Updating docs/protocols/alpha.rst"
  if [[ -f "docs/protocols/alpha.rst" ]]; then
    sed -i.old \
      -e "s/to ${protocol_source}/to ${capitalized_doc_label}/g" \
      -e "s/to $(echo "${protocol_source}" | tr '[:lower:]' '[:upper:]')/to ${capitalized_doc_label}/g" \
      docs/protocols/alpha.rst
    commit_if_changes "docs: update docs/protocols/alpha.rst"
  fi

  # 7. Update docs/Makefile
  log_blue "Updating docs/Makefile"
  sed -i.old \
    -e "s/NAMED_PROTOS\(.*\) ${protocol_source}/NAMED_PROTOS\1 ${doc_label}/" \
    -e "s/-l ${protocol_source}/-l ${doc_label}/g" \
    -e "s/${protocol_source}_short/${doc_label}_short/g" \
    -e "s/${protocol_source}_long/${doc_label}_long/g" \
    -e "s/ ${protocol_source}\// ${doc_label}\//g" \
    docs/Makefile
  commit_if_changes "docs: update docs/Makefile"

  # 8. Regenerate RPC docs
  log_blue "Regenerating ${doc_label}/rpc.rst"
  rm -f "docs/${doc_label}/rpc.rst"
  make -C docs "${doc_label}"/rpc.rst
  commit_if_changes "docs: regenerate ${doc_label}/rpc.rst"

  log_green "Documentation finalized successfully!"
  log_green "Docs renamed from ${protocol_source} to ${doc_label}"
}

function hash() {

  log_cyan "Computing hash"

  source_short_hash=$(echo "${source_hash}" | head -c 8)
  previous_tag=$(grep "${protocol_source}" tezt/lib_alcotezt/alcotezt_utils.ml | sed -e 's/.*->\s*\["\([^"]*\)"\].*/\1/')
  capitalized_previous_tag=$(tr '[:lower:]' '[:upper:]' <<< "${previous_tag:0:1}")${previous_tag:1}

  previous_variant=$(grep "| .* -> \"proto_${protocol_source}\"" "tezt/lib_tezos/protocol.ml" | cut -d'|' -f2 | cut -d'-' -f1)
  previous_variant=$(echo "${previous_variant}" | tr -d ' ')
  log_blue "Current variant used in types is ${magenta}${previous_variant}${reset}"
  read -r -p "Please enter the new variant or press enter to keep the current one: " -e -i "${previous_variant}" new_variant
  echo -e "Current used tag is ${magenta}${previous_tag}${reset}"
  read -r -p "Please enter the new tag or press enter to keep the current one: " -e -i "${previous_tag}" new_tag
  # new_tag must use only lowercase digits and underscores
  while [[ ! "${new_tag}" =~ ^[a-z0-9_]+$ ]]; do
    echo "Tag must use only lowercase digits and underscores"
    read -r -p "Please enter the new tag or press enter to keep the current one: " -e -i "${previous_tag}" new_tag
  done
  capitalized_new_tag=$(tr '[:lower:]' '[:upper:]' <<< "${new_tag:0:1}")${new_tag:1}
  label="${new_tag}"
  capitalized_label="${capitalized_new_tag}"

  tezos_protocol_source=$(echo "${protocol_source}" | tr '_' '-')

  short_hash=$(echo "${protocol_source}" | cut -d'_' -f2)
  if [[ "${is_snapshot}" == false ]]; then
    if [[ ${source_short_hash} != "${short_hash}" ]]; then
      is_snapshot=false
    else
      is_snapshot=true
    fi
  fi
  echo "Is_snapshot = $is_snapshot"
  # set current version
  # Starting from 018 the version value moved to `constants_repr`. To be
  # able to snapshot older protocol the `raw_context` file is kept even
  # if it is not strictly needed anymore.
  echo "Setting current version in raw_context and proxy"

  if [[ ${is_snapshot} == true ]]; then
    sed -i.old.old -e "s/${previous_variant}/${new_variant}/g" \
      -e "s/${previous_tag}/${new_tag}/g" \
      "src/proto_${protocol_source}/lib_protocol/constants_repr.ml" \
      "src/proto_${protocol_source}/lib_protocol/raw_context.ml" \
      "src/proto_${protocol_source}/lib_protocol/raw_context.mli" \
      "src/proto_${protocol_source}/lib_client/proxy.ml" \
      "src/proto_${protocol_source}/lib_protocol/init_storage.ml"
  fi
  commit_no_hooks_if_changes "src: set current version"

  long_hash=$(./octez-protocol-compiler -hash-only "src/proto_${protocol_source}/lib_protocol")
  short_hash=$(echo "${long_hash}" | head -c 8)
  log_magenta "Known hash is: ${source_hash}"
  log_magenta "Computed hash is: ${long_hash}"
  if [[ ${source_hash} == "${long_hash}" ]]; then
    log_green "Hashes match"
    log_blue "Press y to recompute a vanity hash or n to stop"
    read -r continue
    if [[ ${continue} != "y" ]]; then
      print_and_exit 1 "${LINENO}"
    else
      vanity=true
    fi
  else
    warning "Hashes don't match"
    log_blue "Press y to compute a vanity hash or n to simply update the hash"
    read -r continue
    if [[ ${continue} != "y" ]]; then
      vanity=false
    else
      vanity=true
    fi
  fi

  if [[ ${vanity} == true ]]; then

    sed -i 's/Vanity nonce: .* /Vanity nonce: TBD /' "src/proto_${protocol_source}/lib_protocol/main.ml"
    commit_if_changes "src: restore vanity nonce"

    echo "If you want to change it use a third party hasher and update nonce"
    sed -i 's/Vanity nonce: TBD/Vanity nonce: 0000000000000000/' "src/proto_${protocol_source}/lib_protocol/main.ml"
    # wait for the user to press enter
    echo -e "Dumping protocol source into ${blue}proto_to_hash.txt"
    ./octez-protocol-compiler -dump-only "src/proto_${protocol_source}/lib_protocol/" > proto_to_hash.txt
    sed -i 's/Vanity nonce: 0000000000000000/Vanity nonce: TBD/' "src/proto_${protocol_source}/lib_protocol/main.ml"
    echo "For instance using the following command:"
    printf "${yellow}seq 1 8 | parallel --line-buffer ${red}<path_to_hasher> ${blue}proto_to_hash.txt ${magenta}%s${reset}\n" "$(echo "${label}" | cut -c 1-6)"
    echo "Please insert vanity nonce or press enter to continue with the current hash"
    echo -e "${yellow}${blinking}Vanity nonce: ${reset}\c"
    read -r nonce
    # if nonce is not empty, sed     '(* Vanity nonce: TBD *)'  in proto_${version}/lib_protocol/main.ml
    if [[ -n ${nonce} ]]; then
      sed -i.old -e "s/Vanity nonce: TBD/Vanity nonce: ${nonce}/" "src/proto_${protocol_source}/lib_protocol/main.ml"

      long_hash=$(./octez-protocol-compiler -hash-only "src/proto_${protocol_source}/lib_protocol")
      short_hash=$(echo "${long_hash}" | head -c 8)
      log_magenta "Hash computed: ${long_hash}"
      log_magenta "Short hash: ${short_hash}"
      echo "New hash is: ${long_hash}"
      echo "Press y to continue or n to stop"
      read -r continue
      if [[ ${continue} != "y" ]]; then
        rm -f proto_to_hash.txt
        print_and_exit 1 "${LINENO}"
      else
        echo "Continuing with the current hash"
      fi
      rm -f proto_to_hash.txt
      commit_no_hooks "src: add vanity nonce"
    fi
  fi

  # replace fake hash with real hash, this file doesn't influence the hash
  sed -i.old -e 's/"hash": "[^"]*",/"hash": "'"${long_hash}"'",/' \
    "src/proto_${protocol_source}/lib_protocol/TEZOS_PROTOCOL"
  commit_no_hooks "src: update ${protocol_source} hash"

  if [[ ${is_snapshot} == true ]]; then
    new_protocol_name="${version}_${short_hash}"
    new_tezos_protocol="${version}-${short_hash}"
    new_versioned_name="${version}_${label}"
    if [[ -d "src/proto_${new_protocol_name}" ]]; then
      error "'src/proto_${new_protocol_name}' already exists, you should remove it"
      print_and_exit 1 "${LINENO}"
    fi
    if [[ "${protocol_source}" != "${new_protocol_name}" ]]; then
      echo "Renaming src/proto_${protocol_source} to src/proto_${version}_${short_hash}"
      git mv "src/proto_${protocol_source}" "src/proto_${new_protocol_name}"
      rm -rf "src/proto_${protocol_source}"
      commit_no_hooks "src: rename proto_${protocol_source} to proto_${new_protocol_name}"
    fi
    cd "src/proto_${new_protocol_name}"
    # rename main_*.ml{,i} files of the binaries
    find . -name main_\*_"${protocol_source}".ml -or -name main_\*_"${protocol_source}".mli | while read -r file; do
      new_file=${file//_"${protocol_source}"/_"${new_protocol_name}"}
      git mv "${file}" "${new_file}"
    done
    commit_no_hooks "src: rename binaries main_*.ml{,i} files"
  else
    new_protocol_name="${version}"
    new_tezos_protocol="${version}"
    new_versioned_name="${label}"
    cd "src/proto_${new_protocol_name}"
  fi

  cd lib_protocol
  # We use `--print0` and `xargs -0` instead of just passing the result
  # of find to sed in order to support spaces in filenames.
  find . -type f -print0 | xargs -0 \
    sed -i.old -e "s/protocol_${protocol_source}/protocol_${new_protocol_name}/" \
    -e "s/protocol-${tezos_protocol_source}/protocol-${new_tezos_protocol}/" \
    -e "s/protocol-functor-${tezos-protocol-source}/protocol-functor-${new_tezos_protocol}/"
  find . -type f -name "*.ml" -exec ocamlformat -i {} \;
  find . -type f -name "*.mli" -exec ocamlformat -i {} \;
  commit_no_hooks_if_changes "src: replace protocol_${protocol_source} with protocol_${new_protocol_name}"

  cd ../../..

  sed -e "s/${source_hash}/${long_hash}/" -i src/lib_protocol_compiler/final_protocol_versions
  commit_no_hooks "src: update protocol hash into final_protocol_versions"

  sed "s/${source_short_hash}/${short_hash}/g" -i manifest/product_octez.ml
  ocamlformat -i manifest/product_octez.ml
  commit_no_hooks_if_changes "manifest: link protocol in the node, client and codec"

  # find all dune files in src/proto_${protocol_source} and remove them
  find "src/proto_${new_protocol_name}" -name dune -exec grep -q "; This file was automatically generated, do not edit." {} \; -exec rm {} \;
  # remove all proto_${protocol_source} related opam files
  find . -name "*-${source_short_hash}*.opam" -exec rm {} \;
  # commit "src: remove proto_${protocol_source} related opam files"
  rm -f "devtools/yes_wallet/get_delegates_${protocol_source}.ml"
  rm -f "devtools/get_contracts/get_contracts_${protocol_source}.ml"
  if [[ "${protocol_source}" != "${new_protocol_name}" ]]; then
    mv "devtools/testnet_experiment_tools/tool_${protocol_source}.ml" "devtools/testnet_experiment_tools/tool_${new_protocol_name}.ml"
  fi
  sed -e "s/${protocol_source}/${new_protocol_name}/g" -i.old "devtools/testnet_experiment_tools/tool_${new_protocol_name}.ml"
  log_blue "Make manifest"
  make -C manifest
  find devtools -name '*.ml' -exec ocamlformat -i {} \;
  commit_no_hooks_if_changes "manifest: make manifest"

  ## Update protocol tests
  find "src/proto_${new_protocol_name}" -type f -path \*/test/\*.ml \
    -exec sed -i -e "s@Invocation:\(.*\)/proto_${protocol_source}/\(.*\)@Invocation:\1/proto_${new_protocol_name}/\2@" \
    -e "s/${previous_variant}/${new_variant}/g" \
    -e "s/${previous_tag}/${new_tag}/g" \
    \{\} \;
  sed -i.old -e "s/${protocol_source}/${new_protocol_name}/g" \
    "src/proto_${new_protocol_name}/lib_protocol/test/README.md"
  commit_no_hooks_if_changes "tests: fix test invocation headers"
  #Replace all occurences of \[capitalized_protocol_source\] with \[capitalized_label\] in src_proto_${new_protocol_name}
  find "src/proto_${new_protocol_name}" -type f -exec sed -i "s/\\[${capitalized_source}\\]/\\[${capitalized_label}\\]/g" {} \;
  commit_no_hooks_if_changes "tests: fix tests registrations"

  for file in \
    "src/lib_scoru_wasm/pvm_input_kind.ml" \
    "src/lib_scoru_wasm/pvm_input_kind.mli" \
    "src/lib_scoru_wasm/wasm_vm.ml" \
    "src/lib_scoru_wasm/constants.ml" \
    "src/lib_scoru_wasm/test/test_protocol_migration.ml"; do
    log_blue "Update ${file}"
    sed -e "s/${capitalized_source}/${capitalized_label}/g" -i.old "${file}"
    sed -e "s/${protocol_source}/${label}/g" -i.old "${file}"
    sed -e "s/${previous_tag}/${new_tag}/g" -i.old "${file}"
    sed -e "s/${capitalized_previous_tag}/${capitalized_new_tag}/g" -i.old "${file}"
    sed -e "s/${previous_variant}/${new_variant}/g" -i.old "${file}"
    ocamlformat -i "${file}"
  done
  commit_no_hooks_if_changes "scoru: update scoru_wasm protocol_migration"

  # sed -e "s/Proto_${protocol_source}/${capitalized_label}/g" \
  #   -e "s/_${previous_tag}/${new_tag}/g" \
  #   -i.old "src/proto_${new_protocol_name}/lib_protocol/test/unit/test_sc_rollup_wasm.ml"

  #  Teztale
  if [[ ${is_snapshot} == true ]]; then
    git mv "teztale/bin_teztale_archiver/${source_short_hash}_machine.real.ml" "teztale/bin_teztale_archiver/${short_hash}_machine.real.ml"
    sed -e "s/${protocol_source}/${new_protocol_name}/g" -i.old "teztale/bin_teztale_archiver/${short_hash}_machine.real.ml"
    sed -e "s/${source_short_hash}/${short_hash}/g" -i.old "teztale/bin_teztale_archiver/teztale_archiver_main.ml"
  fi
  ocamlformat -i "teztale/bin_teztale_archiver/teztale_archiver_main.ml"
  ocamlformat -i "teztale/bin_teztale_archiver/${short_hash}_machine.real.ml"
  commit_if_changes "teztale: update teztale_archiver_main.ml"

  for file in \
    "src/proto_alpha/lib_protocol/raw_context.ml" \
    "src/proto_alpha/lib_protocol/raw_context.mli" \
    "src/proto_alpha/lib_protocol/init_storage.ml"; do
    update_files "${file}"
  done
  commit_if_changes "Alpha: add ${capitalized_label} as Alpha previous protocol"

  sed -i.old -e "s/${protocol_source}/${new_protocol_name}/g" \
    -e "s/${previous_tag}/${new_tag}/g" \
    tezt/lib_alcotezt/alcotezt_utils.ml
  commit_if_changes "tezt: update protocol tag in alcotezt"

  sed -e "s/${capitalized_source} -> \"P[ts].*\"/${capitalized_label} -> \"${long_hash}\"/g" -i.old tezt/lib_tezos/protocol.ml
  update_files "tezt/lib_tezos/protocol.ml" "tezt/lib_tezos/protocol.mli"
  ocamlformat -i tezt/lib_tezos/protocol.ml
  ocamlformat -i tezt/lib_tezos/protocol.mli
  commit_if_changes "tezt: adapt lib_tezos/protocol.ml"

  #fix testnets_scenarios:
  sed -e "s/Protocol.${capitalized_source}/Protocol.${capitalized_label}/g" \
    -e "s/${previous_tag}/${new_tag}/g" -i src/bin_testnet_scenarios/*.ml
  ocamlformat -i src/bin_testnet_scenarios/*.ml
  commit_if_changes "tezt: fix testnets_scenarios"

  # fix tezt/lib_tezos/constants.ml
  # replace short hash in Uses.make ~tag:"baker_<uncapitalized short hash>" ~path:"./octez-baker-<short hash>" ()
  echo "Adding new protocol baker"
  # print all used variables for debug
  echo "source_short_hash: ${source_short_hash}"
  echo "short_hash: ${short_hash}"
  echo "tezos_protocol_source: ${tezos_protocol_source}"
  echo "new_tezos_protocol: ${new_tezos_protocol}"
  sed -e "s/baker_${source_short_hash}/baker_${short_hash}/g" \
    -e "s/octez-baker-${source_short_hash}/octez-baker-${short_hash}/g" \
    -e "s/octez-baker-${tezos_protocol_source}/octez-baker-${new_tezos_protocol}/g" \
    -i tezt/lib_tezos/constant.ml
  ocamlformat -i tezt/lib_tezos/constant.ml
  commit_if_changes "tezt: replace baker in constant.ml"

  # fix tezt/lib_tezos/constants.ml
  # replace short hash in Uses.make ~tag:"accuser_<uncapitalized short hash>" ~path:"./octez-accuser-<short hash>" ()
  echo "Adding new protocol accuser"
  sed -e "s/accuser_${source_short_hash}/accuser_${short_hash}/g" \
    -e "s/octez-accuser-${source_short_hash}/octez-accuser-${short_hash}/g" \
    -e "s/octez-accuser-${tezos_protocol_source}/octez-accuser-${new_tezos_protocol}/g" \
    -i tezt/lib_tezos/constant.ml
  ocamlformat -i tezt/lib_tezos/constant.ml
  commit_if_changes "tezt: replace accuser in constant.ml"

  #fix other tests:
  sed -e "s/Protocol.${capitalized_source}/Protocol.${capitalized_label}/g" \
    -e "s/${previous_variant}/${new_variant}/g" -i tezt/tests/*.ml
  ocamlformat -i tezt/tests/*.ml
  commit_if_changes "tezt: fix other tests"

  mkdir -p "tezt/tests/encoding_samples/${new_tag}"
  if [[ "${new_tag}" != "${previous_tag}" ]]; then
    git mv "tezt/tests/encoding_samples/${previous_tag}"/* "tezt/tests/encoding_samples/${new_tag}"
    commit "tezt: move ${previous_tag} encoding samples to ${new_tag}"
  fi

  regression_protocol_name="${capitalized_label}-"
  regression_source_name="${capitalized_source}-"
  alpha_regression="Alpha-"

  while [[ ${#regression_protocol_name} -le 5 ]]; do
    regression_protocol_name="${regression_protocol_name}-"
  done
  while [[ ${#regression_source_name} -le 5 ]]; do
    regression_source_name="${regression_source_name}-"
  done

  search_name="*${regression_source_name}*.out"

  echo "search_name: ${search_name}"

  # shellcheck disable=SC2001
  find . -type f -name "${search_name}" | while read -r FILE; do
    ORIG_FILENAME=${FILE}
    echo "Processing file: ${ORIG_FILENAME}"
    NEW_FILENAME=$(echo "${FILE}" | sed "s/${regression_source_name}/${regression_protocol_name}/g")
    echo "New filename: ${NEW_FILENAME}"

    # Create the directory structure for the new file if it doesn't exist
    mkdir -p "$(dirname "${NEW_FILENAME}")"

    # extract filename from NEW_FILENAME wihout extension
    filename=$(basename "${NEW_FILENAME}" .out)
    # NEW_FILENAME should be less than 80 characters
    filename="${filename:0:80}"

    orig_filename=$(basename "${ORIG_FILENAME}" .out)
    orig_filename="${orig_filename:0:80}"
    orig_filename=$(dirname "${ORIG_FILENAME}")/"${orig_filename}".out
    NEW_FILENAME=$(dirname "${NEW_FILENAME}")/"${filename}".out

    echo "Renaming ${orig_filename} to ${NEW_FILENAME}"

    if [[ "${orig_filename}" != "${NEW_FILENAME}" ]]; then
      ## if $FILE exists
      if [[ -f "${orig_filename}" ]]; then
        git mv "${orig_filename}" "${NEW_FILENAME}"
        update_files "${NEW_FILENAME}"
      else
        echo "File ${orig_filename} does not exist"
      fi
    else
      echo "File ${orig_filename} already has the correct name"
      update_files "${NEW_FILENAME}"
    fi

  done
  commit_if_changes "tezt: move ${protocol_source} regression files"

  make

  if [[ "${is_snapshot}" == true ]]; then
    dune exec tezt/tests/main.exe -- --on-unknown-regression-files delete
    commit_if_changes "tezt: delete unknown regression files"

    dune exec tezt/tests/main.exe -- --title 'meta: list runtime dependencies' --reset-regressions
    commit_if_changes "tezt: reset runtime dependencies regressions"

    # if ${capitalized_label} exist in raw_context.ml, reset weeklynet regression test
    if grep -q "${capitalized_label}" src/proto_alpha/lib_protocol/raw_context.ml; then
      log_blue "${capitalized_label} is an alpha predecessor, reset weeklynet regression test"
      dune exec tezt/tests/main.exe -- --file tezt/tests/weeklynet.ml --reset-regressions
      commit_if_changes "tezt: reset weeklynet regression test"
    fi
  fi

  if [[ "${is_snapshot}" == true ]]; then
    log_blue "Update kaitai structs"
    make check-kaitai-struct-files || log_blue "updated kaitai files"
    make kaitai-struct-files-update
    commit_if_changes "kaitai: update structs"
  fi

  update_files devtools/testnet_experiment_tools/testnet_experiment_tools.ml
  ocamlformat -i devtools/testnet_experiment_tools/testnet_experiment_tools.ml
  commit_if_changes "devtools: update testnet_experiment_tools"

  # update linter to remove special rule for stabilised protocol
  sed -i.old -e "s/${protocol_source}/${label}/g" scripts/lint.sh
  commit_if_changes "scripts: update linter to remove special rule for ${protocol_source} protocol"

  find . -name '*.old' -exec rm {} \;
  scripts/lint.sh --check-ocamlformat || echo "linting updated ocamlformat files"
  commit_if_changes "scripts: lint"

  log_blue "Update ci"
  make -C ci
  commit_if_changes "ci: regenerate ci"

  echo "Fixing versioned links in docs"
  if [[ "${label}" != "${previous_tag}" ]]; then
    git mv "docs/${previous_tag}" "docs/${label}"
    commit "docs: move docs/${previous_tag} to docs/${label}"
  fi

  cd "docs/${label}"
  find . -name \*.rst -exec \
    sed -i.old \
    -e "s,src/proto_${protocol_source},src/proto_${new_protocol_name},g" \
    -e "s,tezos-protocol-${tezos_protocol_source}/,tezos-protocol-${new_tezos_protocol}/,g" \
    -e "s,raw_protocol_${protocol_source}/,raw_protocol_${new_protocol_name}/,g" \
    -e "s/_${previous_tag}:/_${label}:/g" \
    -e "s/_${previous_tag}>/_${label}>/g" \
    -e "s/_${previous_tag}\`/_${label}\`/g" \
    -e "s/-${previous_tag}.html/-${label}.html/g" \
    \{\} \;
  commit_if_changes "docs: fix versioned links"

  cd ../..

  cd "docs/alpha"
  find . -name \*.rst -exec \
    sed -i.old \
    -e "s/_${previous_tag}>/_${label}>/g" \
    -e "s@https://tezos.gitlab.io/${previous_tag}/@https://tezos.gitlab.io/${label}/@g" \
    \{\} \;
  commit_if_changes "alpha docs: fix versioned links"

  cd ../..

  log_blue "docs: update docs/protocols/${new_versioned_name}.rst"
  if [[ ${is_snapshot} == true ]]; then
    versioned_name="${version}_${source_label}"
    if [[ ${versioned_name} != "${new_versioned_name}" ]]; then
      git mv "docs/protocols/${versioned_name}.rst" "docs/protocols/${new_versioned_name}.rst"
      commit "docs: move docs/protocols/${protocol_source}.rst to docs/protocols/${new_versioned_name}.rst"
    fi
  fi

  log_blue "docs: update docs/protocols/${new_versioned_name}.rst"
  sed -e "s,src/proto_${protocol_source},src/proto_${new_protocol_name},g" \
    -e "s/${previous_variant}/${new_variant}/g" \
    -e "s/${capitalized_source}/${capitalized_label}/g" \
    -e "s/${protocol_source}/${label}/g" \
    -i "docs/protocols/${new_versioned_name}.rst"
  commit_if_changes "docs: fix docs/protocols/${new_versioned_name}.rst"

  log_blue "Add entries in the doc index"
  doc_index="docs/index.rst"
  sed -i.old -e "s@protocols/${protocol_source}@protocols/${new_versioned_name}@g" "${doc_index}"
  sed -i.old -e "s/${capitalized_source}/${capitalized_label}/g" "${doc_index}"
  sed -i.old -e "s/${source_label}/${label}/g" "${doc_index}"
  commit_if_changes "docs: add entries in the doc index"

  sed -e "s/${previous_tag}/${new_tag}/g" \
    -e "s/${source_hash}/${long_hash}/g" \
    -e "s/${source_short_hash}/${short_hash}/g" \
    -i docs/Makefile
  commit "docs: update docs Makefile"

  rm -f "docs/${label}/rpc.rst"
  make -C docs "${label}"/rpc.rst
  commit_if_changes "docs: generate ${label}/rpc.rst"

  make -C docs openapi || log_blue "OpenAPI files updated"
  rm -rf openapi-tmp
  commit_if_changes "docs: generate openapi"

  echo "Rehashing done"

  log_cyan "Checking potential leftovers"
  total_occurences=0
  while read -r file; do
    if grep -q -i "${previous_tag}" "${file}"; then
      occurences=$(grep -c -i "${previous_tag}" "${file}")
      total_occurences=$((total_occurences + occurences))
      warning "${file} contains ${occurences} occurences of ${previous_tag}"
    fi
  done <<< "$(git ls-files)"
  warning "Total occurences of ${previous_tag}: ${total_occurences}"
  log_blue "Please review the leftovers and update them manually if needed"

}

#run command
case ${command} in
stabilise)
  label=${protocol_target}
  version=${protocol_target}
  snapshot_protocol "${protocol_source}" "${protocol_target}"
  ;;
snapshot)
  label=$(echo "${protocol_target}" | cut -d'_' -f1)
  version=$(echo "${protocol_target}" | cut -d'_' -f2)
  if ! { [[ ${label} =~ ^[a-z]+$ ]] && [[ ${version} =~ ^[0-9][0-9][0-9]$ ]]; }; then
    error "Wrong protocol version."
    error "Name should be a lowercase alphabetic word."
    error "Number should be a 3-digit number."
    echo
    usage
    print_and_exit 1 "${LINENO}"
  fi
  capitalized_source=$(tr '[:lower:]' '[:upper:]' <<< "${protocol_source:0:1}")${protocol_source:1}

  # extract number in "let number = function ParisC -> 020 | $capitalized_source -> 021 | Alpha -> 022"
  expected_version=$(grep -oP "(?<=${capitalized_source} -> )[0-9]+" "tezt/lib_tezos/protocol.ml")

  if [[ ${version} != "${expected_version}" ]]; then
    error "Wrong protocol version: ${version}"
    error "Expected version: ${expected_version}"
    echo "press y to continue"
    read -r -n 1 -s -p "" key
    if [[ ${key} != "y" ]]; then
      print_and_exit 1 "${LINENO}"
    fi
  fi

  is_snapshot=true
  snapshot_protocol "${protocol_source}" "${protocol_target}"
  ;;
hash)
  short_hash=$(echo "${protocol_source}" | cut -d'_' -f2)
  version=$(echo "${protocol_source}" | cut -d'_' -f1)

  source_label=$(grep "${protocol_source}" tezt/lib_alcotezt/alcotezt_utils.ml | sed -e 's/.*->\s*\["\([^"]*\)"\].*/\1/')
  label=${source_label}
  capitalized_label=$(tr '[:lower:]' '[:upper:]' <<< "${source_label:0:1}")${source_label:1}
  capitalized_source="${capitalized_label}"
  hash
  ;;
finalize_docs)
  # protocol_source should be the current docs folder name (e.g., t024)
  # protocol_target should be the desired docs folder name (e.g., tallinn)
  if [[ -z ${protocol_source} ]]; then
    error "Missing --from argument (current docs folder name)"
    usage
    print_and_exit 1 "${LINENO}"
  fi
  if [[ -z ${protocol_target} ]]; then
    error "Missing --to argument (desired docs folder name)"
    usage
    print_and_exit 1 "${LINENO}"
  fi
  doc_label="${protocol_target}"
  finalize_docs
  ;;
copy)
  label=${protocol_target}
  version=${protocol_target}
  snapshot_protocol "${protocol_source}" "${protocol_target}"
  ;;
delete)
  echo "Source_label: ${source_label}"
  echo "Protocol_source: ${protocol_source}"
  if [[ -z ${source_label} ]]; then
    # deleting a protocol in stabilisation
    source_label=${protocol_source}
    short_hash=$(echo "${protocol_source}" | cut -d'_' -f2)
    capitalized_label=$(tr '[:lower:]' '[:upper:]' <<< "${source_label:0:1}")${source_label:1}
    delete_protocol
  else
    short_hash=$(echo "${protocol_source}" | cut -d'_' -f2)
    version=$(echo "${protocol_source}" | cut -d'_' -f1)
    capitalized_label=$(tr '[:lower:]' '[:upper:]' <<< "${source_label:0:1}")${source_label:1}
    delete_protocol
  fi
  ;;
esac

# remove files generated by sed
find . -name '*.old' -exec rm {} \;

echo "${commits} commits created"
echo "You can review them and squash them if needed"
echo "if you want to remove them you can run"
echo "git reset --hard HEAD~${commits}"
