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
short_hash=""
capitalized_label=""

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

  log_green "To cleanup other created files"
  echo "rm -rf src/proto_${protocol_target}"
  echo "rm -rf docs/${label}"
  echo "rm -rf src/proto_${version}_${short_hash}"
  echo "rm -rf src/proto_${version}"
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
  if ! git commit -m "${capitalized_label}/$1"; then
    git add "${script_dir}/../"
    if ! git commit -m "${capitalized_label}/$1"; then
      error "Failed to create commit" 1>&2
      print_and_exit 1 "${LINENO}"
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
  git commit -m "${capitalized_label}/$1" --no-verify
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
  from="${green}--from${reset}"
  to="${green}--to${reset}"
  as="${green}--as${reset}"
  help="${green}--help${reset}"
  stabilise="${green}--stabilise${reset}"
  snapshot="${green}--snapshot${reset}"
  hash="${green}--hash${reset}"
  copy="${green}--copy${reset}"
  ## colored vars
  alpha="${red}alpha${reset}"
  beta="${red}beta${reset}"
  stockholm_023="${red}stockholm_023${reset}"
  stockholm="${magenta}stockholm${reset}"
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
  ${c}, ${copy}
    Copy a protocol.
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

- To copy stockholm_023 known as stockholm into beta and link it in the node, client and codec:
  ${script} ${copy} ${from} ${stockholm_023} ${as} ${stockholm} ${to} ${beta}
  or
  ${script} ${c} ${f} ${stockholm_023} ${a} ${stockholm} ${t} ${beta}
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
  -c | --copy)
    command="copy"
    shift
    ;;
  -f | --from)
    protocol_source="$2"
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
stabilise | snapshot | hash | copy) ;;
*)
  error "Unknown command: ${command}" 1>&2
  error "Command should be one of stabilise, snapshot, hash or copy" 1>&2
  usage 1>&2
  print_and_exit 1 "${LINENO}"
  ;;
esac

if [[ -z ${protocol_source} ]]; then
  error "No protocol source specified" 1>&2
  usage 1>&2
  print_and_exit 1 "${LINENO}"
fi

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

# Check if the protocol source exists
if [[ ! -d "src/proto_${protocol_source}" ]]; then
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
  if ! [[ ${protocol_source} =~ ^[a-z]+[0-9]*$ ]]; then
    error "To ${red}snapshot${reset}, protocol_source should be of the form [a-z]+[0-9]+" 1>&2
    print_and_exit 1 "${LINENO}"
  fi
  if ! [[ ${protocol_target} =~ ^[a-z]+_[0-9][0-9][0-9]$ ]]; then
    error "To ${red}snapshot${reset}, protocol_target should be of the form [a-z]+_[0-9][0-9][0-9]" 1>&2
    clean_and_exit 1 "${LINENO}"
  fi
  warn if source_label is given that it will not be used
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

For changes brought by Paris with respect to Oxford, see :doc:\`../protocols/019_paris\`.

The code can be found in directory :src:\`src/proto_alpha\` of the \`\`master\`\`
branch of Octez.

Environment Version
-------------------



Smart Rollups
-------------


Zero Knowledge Rollups (ongoing)
--------------------------------

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

  if [[ ${command} == "copy" ]]; then
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
    if [[ -d "src/proto_${version}" ]]; then
      error "'src/proto_${version}'" "already exists, you should remove it."
      print_and_exit 1 "${LINENO}"
    fi

    if [[ -d "docs/${label}" ]]; then
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

function copy_source() {

  # Create proto_beta source-code

  # create a temporary directory until the hash is known
  # this is equivalent to `cp src/proto_${protocol_source}/ src/proto_${version}` but only for versioned files
  echo "Copying src/proto_${protocol_source} to src/proto_${version}"
  mkdir /tmp/tezos_proto_snapshot
  git archive HEAD "src/proto_${protocol_source}/" | tar -x -C /tmp/tezos_proto_snapshot
  mv "/tmp/tezos_proto_snapshot/src/proto_${protocol_source}" "src/proto_${version}"
  rm -rf /tmp/tezos_proto_snapshot
  #delete all dune files in src/proto_${version} containing the line "; This file was automatically generated, do not edit."
  find "src/proto_${version}" -name dune -exec grep -q "; This file was automatically generated, do not edit." {} \; -exec rm {} \;
  commit_no_hooks "src: copy from ${protocol_source}"

  if [[ ${command} == "copy" ]]; then
    protocol_source_original="${protocol_source}"
    #use first part of protocol_source + source_label as new protocol_source (e.g. 023_PtStockholm + stockholm -> stockholm_023)
    protocol_source=$(echo "${protocol_source}" | cut -d'_' -f1)
    protocol_source="${source_label}_${protocol_source}"
    log_blue "protocol_source is now ${protocol_source}"
  fi

  # set current version
  # Starting from 018 the version value moved to `constants_repr`. To be
  # able to snapshot older protocol the `raw_context` file is kept even
  # if it is not strictly needed anymore.
  echo "Setting current version in raw_context and proxy"

  if [[ ${is_snapshot} == true ]]; then
    sed -i.old.old -e "s/let version_value = \"${protocol_source}\"/let version_value = \"${protocol_target}\"/" \
      "src/proto_${version}/lib_protocol/constants_repr.ml" \
      "src/proto_${version}/lib_protocol/raw_context.ml" \
      "src/proto_${version}/lib_client/proxy.ml"
  else
    sed -i.old.old -e "s/let version_value = \"${protocol_source}*\"/let version_value = \"${protocol_target}\"/" \
      "src/proto_${version}/lib_protocol/constants_repr.ml" \
      "src/proto_${version}/lib_protocol/raw_context.ml" \
      "src/proto_${version}/lib_client/proxy.ml"
  fi
  commit_no_hooks "src: set current version"

  cd "src/proto_${version}"/lib_protocol
  echo "${capitalized_source}"
  ### FIX ${capitalized_label} PREDECESSORS
  replace_line="else if Compare.String.(s = \"${label}\") then return (${capitalized_label}, ctxt)"
  # replace line containing "return (${capitalized_source}, ctxt)) with replace_line in raw_context.ml
  sed -i.old -e "s/.*return (${capitalized_source}, ctxt).*/${replace_line}/" raw_context.ml
  sed -e "s/${capitalized_source}/${capitalized_label}/g" -i.old raw_context.ml
  sed -e "s/${capitalized_source}/${capitalized_label}/g" -i.old raw_context.mli
  ocamlformat -i raw_context.ml
  ocamlformat -i raw_context.mli

  sed -e "s/${capitalized_source}/${capitalized_label}/g" -i.old init_storage.ml
  ocamlformat -i init_storage.ml
  commit_no_hooks "src: adapt ${capitalized_label} predecessors"

  cd ../../..

  if [[ ${command} == "copy" ]]; then
    sed -i 's/Vanity nonce: .* /Vanity nonce: TBD /' "src/proto_${version}/lib_protocol/main.ml"
    commit_if_changes "src: restore default vanity nonce"
  fi

  log_cyan "Computing hash"
  long_hash=$(./octez-protocol-compiler -hash-only "src/proto_${version}/lib_protocol")
  short_hash=$(echo "${long_hash}" | head -c 8)
  log_magenta "Hash computed: ${long_hash}"
  log_magenta "Short hash: ${short_hash}"

  if [[ ${is_snapshot} == true ]]; then
    echo "Current hash is: ${long_hash}"
    echo "If you want to change it use a third party hasher and update nonce"
    sed -i 's/Vanity nonce: .* /Vanity nonce: 0000000000000000 /' "src/proto_${version}/lib_protocol/main.ml"
    # wait for the user to press enter
    echo -e "Dumping protocol source into ${blue}proto_to_hash.txt"
    ./octez-protocol-compiler -dump-only "src/proto_${version}/lib_protocol/" > proto_to_hash.txt
    sed -i 's/Vanity nonce: 0000000000000000/Vanity nonce: TBD/' "src/proto_${version}/lib_protocol/main.ml"
    echo "For instance using the following command:"
    printf "${yellow}seq 1 8 | parallel --line-buffer ${red}<path_to_hasher> ${blue}proto_to_hash.txt ${magenta}%s${reset}\n" "$(echo "${label}" | cut -c 1-6)"
    echo "Please insert vanity nonce or press enter to continue with the current hash"
    echo -e "${yellow}${blinking}Vanity nonce: ${reset}\c"
    read -r nonce
    # if nonce is not empty, sed     '(* Vanity nonce: TBD *)'  in proto_${version}/lib_protocol/main.ml
    if [[ -n ${nonce} ]]; then
      sed -i.old -e "s/Vanity nonce: TBD/Vanity nonce: ${nonce}/" "src/proto_${version}/lib_protocol/main.ml"

      long_hash=$(./octez-protocol-compiler -hash-only "src/proto_${version}/lib_protocol")
      short_hash=$(echo "${long_hash}" | head -c 8)
      log_magenta "Hash computed: ${long_hash}"
      log_magenta "Short hash: ${short_hash}"
      echo "New hash is: ${long_hash}"
      echo "Press y to continue or n to stop"
      read -r continue
      if [[ ${continue} != "y" ]]; then
        print_and_exit 1 "${LINENO}"
      else
        echo "Continuing with the current hash"
      fi
      rm -f proto_to_hash.txt
      commit_no_hooks "src: add vanity nonce"
    fi
  fi
  cd "src/proto_${version}/lib_protocol"
  # extract hash from  src/${protocol_source}/TEZOS_PROTOCOL in line "hash": "..."
  source_hash=$(grep -oP '(?<="hash": ")[^"]*' "TEZOS_PROTOCOL")
  # replace fake hash with real hash, this file doesn't influence the hash
  sed -i.old -e 's/"hash": "[^"]*",/"hash": "'"${long_hash}"'",/' \
    TEZOS_PROTOCOL
  commit_no_hooks "src: replace ${protocol_source} hash with ${label} hash"

  cd ../../..

  if [[ ${is_snapshot} == true ]]; then
    echo "Renaming src/proto_${version} to src/proto_${version}_${short_hash}"
    new_protocol_name="${version}_${short_hash}"
    new_tezos_protocol="${version}-${short_hash}"
    new_versioned_name="${version}_${label}"

    if [[ -d "src/proto_${new_protocol_name}" ]]; then
      error "'src/proto_${new_protocol_name}' already exists, you should remove it"
      print_and_exit 1 "${LINENO}"
    fi

    git mv "src/proto_${version}" "src/proto_${new_protocol_name}"
    commit_no_hooks "src: rename proto_${version} to proto_${new_protocol_name}"
  else
    new_protocol_name="${version}"
    new_tezos_protocol="${version}"
    new_versioned_name="${label}"
  fi

  # switch protocol_source with protocol_source_original if it was changed
  if [[ ${command} == "copy" ]]; then
    protocol_source="${protocol_source_original}"
  fi

  cd "src/proto_${new_protocol_name}"
  # rename main_*.ml{,i} files of the binaries
  find . -name main_\*_"${protocol_source}".ml -or -name main_\*_"${protocol_source}".mli | while read -r file; do
    new_file=${file//_"${protocol_source}"/_"${new_protocol_name}"}
    git mv "${file}" "${new_file}"
  done
  commit_no_hooks "src: rename binaries main_*.ml{,i} files"

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

  # add this protocol to the immutable list
  if [[ ${is_snapshot} == true ]]; then
    sed -e "s/${source_hash}/${long_hash}/" -i ../../lib_protocol_compiler/final_protocol_versions
  else
    printf "%s\n" "${long_hash}" >> ../../lib_protocol_compiler/final_protocol_versions
  fi
  commit_no_hooks "src: add protocol to final_protocol_versions"
  cd ../../..

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

  log_blue "Make manifest"
  make -C manifest
  find devtools -name '*.ml' -exec ocamlformat -i {} \;
  commit_no_hooks "manifest: make manifest"

  if [[ ${is_snapshot} == true ]]; then
    warning "${protocol_source} has been unlinked in the manifest,  removing it from the source code"
    rm -rf "src/proto_${protocol_source}"
    commit_no_hooks "src: remove proto_${protocol_source}"
  fi

  # modify the first_argument variable directly
  eval "$1=$long_hash"

}

function update_protocol_tests() {
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
      sed -r "s/(Data_encoding.\(Binary.to_string_exn string Constants.proto_${source_label}_name\))/\1 | ${capitalized_label} ->Data_encoding.(Binary.to_string_exn string Constants.proto_beta_name)/" -i.old src/lib_scoru_wasm/pvm_input_kind.ml
      sed -r "s/${capitalized_source} -> (V.*)/ ${capitalized_source} -> \1 | ${capitalized_label} -> \1/" -i.old src/lib_scoru_wasm/wasm_vm.ml
    else
      sed "/let proto_${protocol_source}_name = .*/i \let proto_${label}_name = \"${label}\"" -i.old src/lib_scoru_wasm/constants.ml
      sed "/| payload when String.equal payload Constants.proto_${protocol_source}_name ->/i \  | payload when String.equal payload Constants.proto_${label}_name -> Some (Protocol_migration $capitalized_label)" -i.old src/lib_scoru_wasm/pvm_input_kind.ml
      sed -r "s/(Data_encoding.\(Binary.to_string_exn string Constants.proto_${protocol_source}_name\))/\1 | ${capitalized_label} ->Data_encoding.(Binary.to_string_exn string Constants.proto_beta_name)/" -i.old src/lib_scoru_wasm/pvm_input_kind.ml
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
    sed -e "s/${protocol_source}/${version}/g" \
      -e "s/${capitalized_source}/${short_hash}/g" -i.old "teztale/bin_teztale_archiver/teztale_archiver_main.ml"
    ocamlformat -i "teztale/bin_teztale_archiver/${label}_machine.real.ml"
  fi
  ocamlformat -i "teztale/bin_teztale_archiver/teztale_archiver_main.ml"
  commit_if_changes "teztale: update teztale_archiver_main.ml"

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
    # remove all code between $start_remove and $end_remove in src/proto_alpha/lib_protocol/raw_context.ml
    perl -0777 -pe "s/${start_remove}.*${end_remove}/${start_remove}\n\n${remove_comment}\n\n${end_remove}\n/s" -i "src/proto_alpha/lib_protocol/raw_context.ml"
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
(* Copyright (c) %s Nomadic Labs <contact@nomadic-labs.com>                *)
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

  # ensure protocols compile and parameter files are generated
  make

  # Update tezt tests

  # automatically add the new protocol tag to alcotezt

  if [[ ${is_snapshot} == true ]]; then
    sed -i.old -e "s/| Some \"${protocol_source}\" -> \[\"${protocol_source}\"\]/| Some \"${new_protocol_name}\" -> [\"${label}\"]"/ tezt/lib_alcotezt/alcotezt_utils.ml
    commit "tezt: update protocol tag in alcotezt"
  else
    temp_file=$(mktemp)
    tac tezt/lib_alcotezt/alcotezt_utils.ml | tail +2 | tac > "${temp_file}"
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
    sed "/| ${capitalized_source} -> \"proto_${protocol_source}\"/i | ${capitalized_label} -> \"proto_${label}\"\n" -i.old tezt/lib_tezos/protocol.ml

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

  # for regression files, protocol_name should be at least 5 character long, if not add enough trailing '-' at the end
  regression_protocol_name=${capitalized_label}
  while [[ ${#regression_protocol_name} -lt 5 ]]; do
    regression_protocol_name="${regression_protocol_name}-"
  done

  regression_source_name=${capitalized_source}
  while [[ ${#regression_source_name} -lt 5 ]]; do
    regression_source_name="${regression_source_name}-"
  done

  # shellcheck disable=SC2312
  # shellcheck disable=SC2001
  find . -type f -name "*${regression_source_name}*.out" | while read -r FILE; do
    NEW_FILENAME=$(echo "${FILE}" | sed "s/${regression_source_name}/${regression_protocol_name}/g")

    # Create the directory structure for the new file if it doesn't exist
    mkdir -p "$(dirname "${NEW_FILENAME}")"

    # extract filename from NEW_FILENAME wihout extension
    filename=$(basename "${NEW_FILENAME}" .out)
    # NEW_FILENAME should be less than 80 characters
    filename="${filename:0:80}"
    NEW_FILENAME=$(dirname "${NEW_FILENAME}")/"${filename}".out

    if [[ ${is_snapshot} == true ]]; then
      git mv "${FILE}" "${NEW_FILENAME}"
    else
      # Preserve the file permissions
      cp -p "${FILE}" "${NEW_FILENAME}"
    fi
    #replace all occurences of protocol_source with label
    if [[ ${is_snapshot} == true ]]; then
      sed -i.old -e "s/proto_${protocol_source}/proto_${version}-${short_hash}/g" "${NEW_FILENAME}"
      sed -i.old -e "s/${protocol_source}/${version}-${short_hash}/g" "${NEW_FILENAME}"
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
    dune exec tezt/tests/main.exe -- --file tezt/tests/protocol_migration.ml --title 'Alpha: weeklynet regression test' --reset-regressions
    commit_if_changes "tezt: reset weeklynet regression test"
  fi

}

function misc_updates() {
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
    sed -e "s@${protocol_source}_parameters_file=.*@${label}_parameters_file=\"\$bin_dir/../../_build/default/src/proto_${version}_${short_hash}/lib_parameters/sandbox-parameters.json\"@" \
      -e "s/alias octez-activate-${protocol_source}=.*/alias octez-activate-${label}=\"\$client -block genesis activate protocol ${long_hash} with fitness 1 and key activator and parameters \$${label}_parameters_file\";/" -i src/bin_client/octez-init-sandboxed-client.sh
  else
    sed "/parameters_file=\"\$bin_dir.*/i \    ${label}_parameters_file=\"\$bin_dir/../../_build/default/src/proto_${label}/lib_parameters/sandbox-parameters.json\"" -i src/bin_client/octez-init-sandboxed-client.sh
    sed "/alias octez-activate-${protocol_source}=.*/i \alias octez-activate-${label}=\"\$client -block genesis activate protocol ${long_hash} with fitness 1 and key activator and parameters \$${label}_parameters_file\";" -i src/bin_client/octez-init-sandboxed-client.sh
  fi
  commit_no_hooks "sandbox: add octez-activate-${label} command to client sandbox"

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
  scripts/lint.sh --check-ocamlformat || echo "linting updated ocamlformat files"
  commit_if_changes "scripts: lint"

  log_blue "Update ci"
  make -C ci
  commit "ci: regenerate ci"

}

function generate_doc() {

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
  sed -e "s/^Protocol ${capitalized_source}/Protocol ${capitalized_label}/" \
    -e "s/protocol ${capitalized_source}/protocol ${capitalized_label}/" \
    -e "s,src/proto_${protocol_source},src/proto_${new_protocol_name},g" \
    -i "docs/protocols/${new_versioned_name}.rst"

  if [[ ${#label} -gt 5 ]]; then
    # add (#label-5) '=' characters to the second line
    missing_equals='='
    for ((i = 0; i < ${#label} - 5; i++)); do
      missing_equals="${missing_equals}="
    done
    sed -i.old -e "2s/$/${missing_equals}/" "docs/protocols/${new_versioned_name}.rst"
  fi

  commit "docs: fix docs/protocols/${new_protocol_name}.rst"

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
    sed -i.old -e "s/alpha\/rpc\.rst/alpha\/rpc.rst ${label}\/rpc.rst/g" docs/Makefile
    sed -i.old -r "s/alpha\/octez-\*\.html/alpha\/octez-*.html ${label}\/octez-*.html/g" docs/Makefile
  fi
  commit "docs: update docs Makefile"

  rm -f "docs/${label}/rpc.rst"
  make -C docs "${label}"/rpc.rst
  commit "docs: generate ${label}/rpc.rst"

}

function snapshot_protocol() {
  capitalized_label=$(tr '[:lower:]' '[:upper:]' <<< "${label:0:1}")${label:1}

  if [[ ${command} == "copy" ]]; then
    capitalized_source=$(tr '[:lower:]' '[:upper:]' <<< "${source_label:0:1}")${source_label:1}
  else
    capitalized_source=$(tr '[:lower:]' '[:upper:]' <<< "${protocol_source:0:1}")${protocol_source:1}
  fi
  #replace _ with - in protocol_source
  tezos_protocol_source=$(echo "${protocol_source}" | tr '_' '-')

  # e.g. Pt8PY9P47nYw7WgPqpr49JZX5iU511ZJ9UPrBKu1CuYtBsLy7q7 (set below)
  long_hash=
  # e.g. Pt8PY9P4 (set below)
  short_hash=

  sanity_check_before_script

  copy_source long_hash
  short_hash=$(echo "${long_hash}" | head -c 8)

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

  sed -i.old -e "/| Protocol.${capitalized_label} -> */d" tezt/tests/sc_rollup_migration.ml
  sed -i.old -e "/| Protocol.${capitalized_label} -> */d" tezt/tests/sc_rollup.ml
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

  sed -i.old -e "/.*${source_label}.*/d" src/bin_client/octez-init-sandboxed-client.sh
  commit_no_hooks "sandbox: remove ${source_label} from client sandbox"

  #if deleting a protocol in stabilisation
  if [[ ${source_label} == "${protocol_source}" ]]; then
    sed "/| Tezt_tezos.Protocol.Beta -> */d" -i devtools/testnet_experiment_tools/testnet_experiment_tools.ml
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
  commit "docs: remove ${capitalized_label} from docs/index.rst"
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

function hash() {

  log_cyan "Computing hash"

  # extract hash from  src/${protocol_source}/TEZOS_PROTOCOL in line "hash": "..."
  source_hash=$(grep -oP '(?<="hash": ")[^"]*' "src/proto_${protocol_source}/lib_protocol/TEZOS_PROTOCOL")
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
  if [[ ${source_short_hash} != "${short_hash}" ]]; then
    is_snapshot=false
  else
    is_snapshot=true
  fi

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

  if [[ ${is_snapshot} == true ]]; then
    # regression_protocol_name = $short hash  minus the first 2 characters
    regression_protocol_name=${short_hash:2}
    # with all characters in lowercase
    regression_protocol_name=$(tr '[:upper:]' '[:lower:]' <<< "${regression_protocol_name}")
    # and capitalized
    regression_protocol_name=$(tr '[:lower:]' '[:upper:]' <<< "${regression_protocol_name:0:1}")${regression_protocol_name:1}

    regression_source_name=${source_short_hash:2}
    regression_source_name=$(tr '[:upper:]' '[:lower:]' <<< "${regression_source_name}")
    regression_source_name=$(tr '[:lower:]' '[:upper:]' <<< "${regression_source_name:0:1}")${regression_source_name:1}
  else
    regression_protocol_name=${capitalized_label}
    regression_source_name=${capitalized_source}
  fi

  while [[ ${#regression_protocol_name} -lt 5 ]]; do
    regression_protocol_name="${regression_protocol_name}-"
  done
  while [[ ${#regression_source_name} -lt 5 ]]; do
    regression_source_name="${regression_source_name}-"
  done

  # shellcheck disable=SC2001
  find . -type f -name "*${regression_source_name}*.out" | while read -r FILE; do
    NEW_FILENAME=$(echo "${FILE}" | sed "s/${capitalized_previous_tag}/${capitalized_new_tag}/g")

    # Create the directory structure for the new file if it doesn't exist
    mkdir -p "$(dirname "${NEW_FILENAME}")"

    # extract filename from NEW_FILENAME wihout extension
    filename=$(basename "${NEW_FILENAME}" .out)
    # NEW_FILENAME should be less than 80 characters
    filename="${filename:0:80}"
    NEW_FILENAME=$(dirname "${NEW_FILENAME}")/"${filename}".out

    if [[ "${FILE}" != "${NEW_FILENAME}" ]]; then
      git mv "${FILE}" "${NEW_FILENAME}"
    fi

    update_files "${NEW_FILENAME}"

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
      log_blue "${capitalized_label} is an alph predecessor, reset weeklynet regression test"
      dune exec tezt/tests/main.exe -- --file tezt/tests/protocol_migration.ml --title 'Alpha: weeklynet regression test' --reset-regressions
      commit_if_changes "tezt: reset weeklynet regression test"
    fi
  fi

  if [[ "${is_snapshot}" == true ]]; then
    log_blue "Update kaitai structs"
    make check-kaitai-struct-files || log_blue "updated kaitai files"
    make kaitai-struct-files-update
    commit_if_changes "kaitai: update structs"
  fi

  update_files src/bin_client/octez-init-sandboxed-client.sh
  commit_no_hooks_if_changes "sandbox: update octez-activate-${label} command to client sandbox"

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
    -e "s/_${protocol_source}:/_${label}:/g" \
    -e "s/_${protocol_source}>/_${label}>/g" \
    -e "s/_${protocol_source}\`/_${label}\`/g" \
    -e "s/-${protocol_source}.html/-${label}.html/g" \
    \{\} \;
  commit_if_changes "docs: fix versioned links"

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
copy)
  label=${protocol_target}
  version=${protocol_target}
  snapshot_protocol "${protocol_source}" "${protocol_target}"
  ;;
esac

# remove files generated by sed
find . -name '*.old' -exec rm {} \;

echo "${commits} commits created"
echo "You can review them and squash them if needed"
echo "if you want to remove them you can run"
echo "git reset --hard HEAD~${commits}"
