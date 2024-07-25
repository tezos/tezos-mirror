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
  git commit -m "${capitalized_label}/$1"
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

## Automatically clean and exit on error
trap print_and_exit ERR

function usage() {
  # colored option strings
  f="${green}-f${reset}"
  t="${green}-t${reset}"
  h="${green}-h${reset}"
  s="${green}-s${reset}"
  p="${green}-p${reset}"
  from="${green}--from${reset}"
  to="${green}--to${reset}"
  help="${green}--help${reset}"
  stabilise="${green}--stabilise${reset}"
  snapshot="${green}--snapshot${reset}"
  ## colored vars
  alpha="${red}alpha${reset}"
  beta="${red}beta${reset}"
  stockholm_023="${red}stockholm_023${reset}"
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
  ${f}, ${from} ${red}<protocol_source>${reset}
    The source protocol to stabilise or snapshot.
  ${t}, ${to} ${red}<protocol_target>${reset}
    The target protocol to stabilise or snapshot.

${yellow}tl;dr:${reset}
- To stabilise protocol alpha into beta and link it in the node, client and codec:
  ${script} ${stabilise} ${from} ${alpha} ${to} ${beta}
  or
  ${script} ${s} ${f} ${alpha} ${t} ${beta}

- To snapshot protocol alpha into stockholm_023 and link it in the node, client and codec:
  ${script} ${snapshot} ${from} ${beta} ${to} ${stockholm_023}
  or
  ${script} ${p} ${f} ${beta} ${t} ${stockholm_023}

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
  -f | --from)
    protocol_source="$2"
    shift 2
    ;;
  -t | --to)
    protocol_target="$2"
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

# Check if commands are valid
# if command =  stabilise then protocol_source and protocol_target are required

if [[ -z ${command} ]]; then
  echo "No command specified" 1>&2
  usage 1>&2
  print_and_exit 1 "${LINENO}"
fi
if [[ ${command} == "stabilise" ]] || [[ ${command} == "snapshot" ]]; then

  if [[ -z ${protocol_target} ]]; then
    error "No protocol target specified" 1>&2
    usage 1>&2
    print_and_exit 1 "${LINENO}"
  fi
  if [[ -z ${protocol_source} ]]; then
    error "No protocol source specified" 1>&2
    usage 1>&2
    print_and_exit 1 "${LINENO}"
  fi
  if [[ ${protocol_source} == "${protocol_target}" ]]; then
    error "Protocol source and target should be different" 1>&2
    usage 1>&2
    print_and_exit 1 "${LINENO}"
  fi
  #
  msg="Will ${command} protocol from 'src/proto_${protocol_source}' into  'src/proto_${protocol_target}'"

fi

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

  if ! [[ -d "docs/${protocol_source}" ]]; then
    error "'docs/${protocol_source}'" "does not exist" 1>&2
    print_and_exit 1 "${LINENO}"
  fi

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
    sed -i.old.old -e "s/let version_value = \"${protocol_source}_current\"/let version_value = \"${protocol_target}\"/" \
      "src/proto_${version}/lib_protocol/constants_repr.ml" \
      "src/proto_${version}/lib_protocol/raw_context.ml" \
      "src/proto_${version}/lib_client/proxy.ml"
  fi
  commit_no_hooks "src: set current version"

  cd "src/proto_${version}"/lib_protocol

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

  log_cyan "Computing hash"
  long_hash=$(./octez-protocol-compiler -hash-only "src/proto_${version}/lib_protocol")
  short_hash=$(echo "${long_hash}" | head -c 8)
  log_magenta "Hash computed: ${long_hash}"
  log_magenta "Short hash: ${short_hash}"

  if [[ ${is_snapshot} == true ]]; then
    echo "Current hash is: ${long_hash}"
    echo "If you want to change it use a third party hasher and update nonce"
    sed -i 's/Vanity nonce: TBD/Vanity nonce: 0000000000000000/' "src/proto_${version}/lib_protocol/main.ml"
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

  if [[ ${is_snapshot} == true ]]; then
    rm "src/proto_${new_protocol_name}/README.md"
    commit_no_hooks "src: remove README"
  else
    sed -i "s/${protocol_source}/${label}/g" "src/proto_${label}/README.md"

    commit_no_hooks "src: rename protocol in the README"
  fi

  echo -e "\e[33mLinking protocol in the node, client and codec\e[0m"
  if [[ ${is_snapshot} == true ]]; then
    sed "s/let _${protocol_source} = active .*/  let _${version}_${short_hash} = active (Name.v \"${short_hash}\" ${version})\n/" -i manifest/product_octez.ml
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

  #update scoru_wasm protocol_migratiton tests
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
    sed "/Proto_${protocol_source} -> \"Proto_${protocol_source}\"/i \  | ${capitalized_label} -> \"${capitalized_label}\"" -i.old src/lib_scoru_wasm/test/test_protocol_migration.ml
    sed -r "s/\((V.*, V.*,) Proto_${protocol_source}\);/ \(\1 Proto_${protocol_source}\); \(\1 ${capitalized_label});/" -i.old src/lib_scoru_wasm/test/test_protocol_migration.ml
    ocamlformat -i src/lib_scoru_wasm/test/test_protocol_migration.ml

    sed "/let proto_${protocol_source}_name = .*/i \let proto_${label}_name = \"${label}\"" -i.old src/lib_scoru_wasm/constants.ml
    ocamlformat -i src/lib_scoru_wasm/constants.ml

    sed "/let proto_${protocol_source}_name = .*/i \let proto_${label}_name = \"${label}\"" -i.old src/lib_scoru_wasm/constants.ml
    ocamlformat -i src/lib_scoru_wasm/constants.ml

    sed -r "s/(type protocol =.*)/\1 | ${capitalized_label}/" -i.old src/lib_scoru_wasm/pvm_input_kind.ml
    sed -r "s/(type protocol =.*)/\1 | ${capitalized_label}/" -i.old src/lib_scoru_wasm/pvm_input_kind.mli
    sed "/| payload when String.equal payload Constants.proto_${protocol_source}_name ->/i \  | payload when String.equal payload Constants.proto_${label}_name -> Some (Protocol_migration $capitalized_label)" -i.old src/lib_scoru_wasm/pvm_input_kind.ml
    sed -r "s/(Data_encoding.\(Binary.to_string_exn string Constants.proto_${protocol_source}_name\))/\1 | ${capitalized_label} ->Data_encoding.(Binary.to_string_exn string Constants.proto_beta_name)/" -i.old src/lib_scoru_wasm/pvm_input_kind.ml
    ocamlformat -i src/lib_scoru_wasm/pvm_input_kind.ml
    ocamlformat -i src/lib_scoru_wasm/pvm_input_kind.mli

    sed -r "s/Proto_${protocol_source} -> (V.*)/ Proto_${protocol_source} -> \1 | ${capitalized_label} -> \1/" -i.old src/lib_scoru_wasm/wasm_vm.ml
    ocamlformat -i src/lib_scoru_wasm/wasm_vm.ml
  fi

  commit_no_hooks "scoru: update scoru_wasm protocol_migration"

  sed -e "s/Proto_${protocol_source}/${capitalized_label}/g" \
    -e "s/${capitalized_source}/${capitalized_label}/g" \
    -e "s/Tezos_scoru_wasm.Constants.proto_${protocol_source}_name/Tezos_scoru_wasm.Constants.proto_${label}_name/g" \
    -i.old "src/proto_${new_protocol_name}/lib_protocol/test/unit/test_sc_rollup_wasm.ml"
  ocamlformat -i "src/proto_${new_protocol_name}/lib_protocol/test/unit/test_sc_rollup_wasm.ml"
  commit_no_hooks "sc_rollup: update proto_${new_protocol_name}/test/unit/test_sc_rollup_wasm"

}

function update_source() {

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
  if [[ ${is_snapshot} == true ]]; then
    sed -i.old -e "s/s = \"${protocol_source}\"/s = \"${label}_${version}\"/g" \
      "src/proto_alpha/lib_protocol/raw_context.ml"
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
    prepare_first_block=$(sed -n "/${start_source}/,/${end_source}/p" "src/proto_${protocol_source}/lib_protocol/raw_context.ml")
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
    #replace all multiline code between $start_predecessor and $end_predecessor with the content of prepare_first_block_patched in src/proto_${protocol_source}/lib_protocol/raw_context.ml using perl
    perl -0777 -pe "s/${start_predecessor}.*${end_predecessor}/${escaped_prepare_first_block}/s" -i "src/proto_${protocol_source}/lib_protocol/raw_context.ml"
    # remove all code between $start_remove and $end_remove in src/proto_${protocol_source}/lib_protocol/raw_context.ml
    perl -0777 -pe "s/${start_remove}.*${end_remove}/${start_remove}\n\n${remove_comment}\n\n${end_remove}\n/s" -i "src/proto_${protocol_source}/lib_protocol/raw_context.ml"
    #replace code between "$type_to_remove' and '$type_to_remove' with capitalized_label in src/proto_${protocol_source}/lib_protocol/raw_context.ml
    perl -0777 -pe "s/${type_to_remove}[ \t]+[a-zA-Z0-9_]+[ \t]+${type_to_remove}/${type_to_remove}${capitalized_label}${type_to_remove}/" -i "src/proto_${protocol_source}/lib_protocol/raw_context.ml"
    perl -0777 -pe "s/${type_to_remove}[ \t]+[a-zA-Z0-9_]+[ \t]+${type_to_remove}/${type_to_remove}${capitalized_label}${type_to_remove}/" -i "src/proto_${protocol_source}/lib_protocol/raw_context.mli"
    replace_string="Compare.String.(s = \"${label}\") then return (${capitalized_label}, ctxt)"
    #replace  code between "$type_to_remove' and '$type_to_remove' with $replace_string in src/proto_${protocol_source}/lib_protocol/raw_context.ml
    perl -0777 -pe "s/${type_to_remove}[ \t]+Compare.*${type_to_remove}/${type_to_remove}$replace_string${type_to_remove}/s" -i "src/proto_${protocol_source}/lib_protocol/raw_context.ml"

    ocamlformat -i "src/proto_${protocol_source}/lib_protocol/raw_context.ml"
    ocamlformat -i "src/proto_${protocol_source}/lib_protocol/raw_context.mli"

    prepare_first_block=$(sed -n "/${start_source}/,/${end_source}/p" "src/proto_${protocol_source}/lib_protocol/init_storage.ml")
    # shellcheck disable=SC2001
    prepare_first_block_patched=$(echo "${prepare_first_block}" | sed "s/${capitalized_source}/${capitalized_label}/g")
    escaped_prepare_first_block=$(printf '%s\n' "$prepare_first_block_patched" | sed 's/[`~!@#$%^&*()-_=+{}\|;:",<.>/?]/\\&/g')
    #replace all code between '(* Start of alpha predecessor stitching. Used for automatic protocol snapshot *)' and '(* End of alpha predecessor stitching. Used for automatic protocol snapshot *)' with the content of prepare_first_block_patched in src/proto_${protocol_source}/lib_protocol/raw_context.ml
    perl -0777 -pe "s/${start_predecessor}.*${end_predecessor}/${escaped_prepare_first_block}/s" -i "src/proto_${protocol_source}/lib_protocol/init_storage.ml"
    ocamlformat -i "src/proto_${protocol_source}/lib_protocol/init_storage.ml"
    commit "alpha: add ${capitalized_label} as Alpha previous protocol"
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

  temp_file=$(mktemp)
  tac tezt/lib_alcotezt/alcotezt_utils.ml | tail +2 | tac > "${temp_file}"
  echo "  | Some \"${new_protocol_name}\" -> [\"${label}\"]" >> "${temp_file}"
  echo "  | Some _ -> assert false" >> "${temp_file}"
  mv "${temp_file}" tezt/lib_alcotezt/alcotezt_utils.ml
  commit "tezt: add new protocol tag to alcotezt"

  cd "${script_dir}"/..

  # Adapt tezt/lib_tezos/protocol.ml

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
      tezt/lib_tezos/protocol.ml
    sed -i.old -e "s/let all = \[/let all = [ ${capitalized_label};/" tezt/lib_tezos/protocol.ml

    #source=Alpha; echo "let number = function ParisC -> 020 | Alpha -> 021" |sed -r 's/(.*) '$source' -> ([0-9][0-9][0-9])/printf "\1 Beta -> \2 | '$source' -> %03i" "$(echo \2+1 | bc)"/ge'
    # shellcheck disable=SC2086
    # shellcheck disable=SC2016
    sed -r 's/(.*) '${capitalized_source}' -> ([0-9][0-9][0-9])/printf "\1 '${capitalized_label}' -> \2 | '${capitalized_source}' -> %03i" "$(echo \2+1 | bc)"/ge' -i.old tezt/lib_tezos/protocol.ml

    sed "/| ${capitalized_source} -> \"proto_${protocol_source}\"/i | ${capitalized_label} -> \"proto_${label}\"\n" -i.old tezt/lib_tezos/protocol.ml

    # add $(capitalized_label) -> "$long_hash" before "(* DO NOT REMOVE, AUTOMATICALLY ADD STABILISED PROTOCOL HASH HERE *)"
    sed "/\(\* DO NOT REMOVE, AUTOMATICALLY ADD STABILISED PROTOCOL HASH HERE \*\)/i \ | ${capitalized_label} -> \"${long_hash}\"" -i.old tezt/lib_tezos/protocol.ml

    sed -r "s/(.*) ${capitalized_source} -> Some (.*)/\1 ${capitalized_source} -> Some ${capitalized_label} | ${capitalized_label} -> Some \2/g" -i.old tezt/lib_tezos/protocol.ml
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
    commit "tezt: reset weeklynet regression test"
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
    sed "/| Tezt_tezos.Protocol.${protocol_source} ->/i \  | Tezt_tezos.Protocol.${capitalized_label} ->\n Some protocol_${label}_parameters_template" -i.old devtools/testnet_experiment_tools/testnet_experiment_tools.ml
  fi
  ocamlformat -i devtools/testnet_experiment_tools/testnet_experiment_tools.ml
  commit "devtools: update testnet_experiment_tools"

  if [[ ${is_snapshot} == true ]]; then
    # update linter to remove special rule for stabilised protocol
    sed -i.old -e "s/-not -name \"proto_${protocol_source}\"//" scripts/lint.sh
    commit "scripts: update linter to remove special rule for ${protocol_source} protocol"
  else
    # update linter to allow reformating of beta protocol
    sed -i.old -e "s/-not -name \"proto_${protocol_source}\"/-not -name \"proto_${label}\" -not -name \"proto_${protocol_source}\"/" scripts/lint.sh
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

  # Create proto_beta docs

  echo "Copying docs/${protocol_source} to docs/${label}"
  mkdir /tmp/tezos_proto_doc_snapshot
  git archive HEAD "docs/${protocol_source}/" | tar -x -C /tmp/tezos_proto_doc_snapshot
  mv "/tmp/tezos_proto_doc_snapshot/docs/${protocol_source}" "docs/${label}"
  rm -rf /tmp/tezos_proto_doc_snapshot commit "copy from docs/${protocol_source}"
  commit "docs: copy from docs/${protocol_source}"

  # fix versioned links (in labels, references, and paths) in docs
  echo "Fixing versioned links in docs"
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
  commit "docs: fix versioned links"

  cd ../..

  # generate docs/protocols/${new_versioned_name}.rst from docs/protocols/${protocol_source}.rst
  echo "Copying docs/protocols/${protocol_source}.rst to docs/protocols/${new_versioned_name}.rst"
  if [[ ${is_snapshot} == true ]]; then
    mv "docs/protocols/${protocol_source}.rst" "docs/protocols/${new_versioned_name}.rst"
  else
    cp "docs/protocols/${protocol_source}.rst" "docs/protocols/${new_versioned_name}.rst"
  fi
  commit "docs: copy docs/protocols/${protocol_source}.rst to docs/protocols/${new_versioned_name}.rst"
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
    awk -v PATTERN1="${capitalized_source} Dev Protocol Reference <${protocol_source}/index>" \
      -v REPLACEMENT1="${capitalized_label} Protocol Reference <${label}/index>" \
      -v PATTERN2="protocols/${protocol_source}" \
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
    sed -i.old -r "s/(PROTOCOLS .*) ${protocol_source}/\1 ${protocol_source} ${label}/" docs/Makefile
    line=$(printf "%s_long%*s= %s" "${label}" $((10 - ${#label})) '' "$long_hash")
    sed "/${protocol_source}_long .*/i${line}" -i docs/Makefile
    line=$(printf "%s_short%*s= %s" "${label}" $((8 - ${#label})) '' "$label")
    sed "/${protocol_source}_short .*/i${label}_short = ${label}" -i docs/Makefile
    sed -i.old -e "s/${protocol_source}\/rpc\.rst/${protocol_source}\/rpc.rst ${label}\/rpc.rst/g" docs/Makefile
    sed -i.old -r "s/${protocol_source}\/octez-\*\.html/${protocol_source}\/octez-*.html ${label}\/octez-*.html/g" docs/Makefile
  fi
  commit "docs: update docs Makefile"

  rm -f "docs/${label}/rpc.rst"
  make -C docs "${label}"/rpc.rst
  commit "docs: generate ${label}/rpc.rst"

  if [[ ${is_snapshot} == true ]]; then
    warning "docs/${protocol_source} will now be removed"
    rm -rf "docs/${protocol_source}"
    commit "docs: remove docs/${protocol_source}"
  fi

}

function snapshot_protocol() {
  capitalized_label=$(tr '[:lower:]' '[:upper:]' <<< "${label:0:1}")${label:1}

  capitalized_source=$(tr '[:lower:]' '[:upper:]' <<< "${protocol_source:0:1}")${protocol_source:1}

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

esac

# remove files generated by sed
find . -name '*.old' -exec rm {} \;

echo "${commits} commits created"
echo "You can review them and squash them if needed"
echo "if you want to remove them you can run"
echo "git reset --hard HEAD~${commits}"
