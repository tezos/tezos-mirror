#!/usr/bin/env bash

usage() {
  cat >&2 << EOF
usage: $0 [<action>] [FILES] [--ignore FILES]

Where <action> can be:

* --update-ocamlformat: update all the \`.ocamlformat\` files and
  git-commit (requires clean repo).
* --check-ocamlformat: check that --update-ocamlformat does nothing.
* --format-scripts: format shell scripts inplace using shfmt
* --check-scripts: shellcheck and check formatting of the .sh files
* --check-redirects: check docs/_build/_redirects.
* --check-coq-attributes: check the presence of coq attributes.
* --check-rust-toolchain: check the contents of rust-toolchain files
* --check-licenses-git-new: check license headers of added OCaml .ml(i) files.
* --check-jsonnet-format: checks that the jsonnet files are formatted.
* --check-jsonnet-lint: checks that the jsonnet files are compliant with linting rules..
* --check-quebec-plugin-ghostnet-fix: checks that the quebec plugin ghostnet fix exists.
* --help: display this and return 0.
EOF
}

shopt -s extglob

## Testing for dependencies
if ! type find > /dev/null 2>&-; then
  echo "find is required but could not be found. Aborting."
  exit 1
fi

set -e

say() {
  echo "$*" >&2
}

declare -a source_directories

# Make sure that the set of source_directories here are also reflected in
# [changeset_lint_files] in [ci/bin/common.ml].
source_directories=(src docs/doc_gen tezt devtools contrib etherlink client-libs)
# Set of newline-separated basic regular expressions to exclude from --check-licenses-git-new.
license_check_exclude=$(
  cat << EOF
.*_generated.ml
src/lib_protocol_environment/sigs/.*
src/riscv/api/octez_riscv_api.ml
src/riscv/api/octez_riscv_api.mli
etherlink/lib_wasm_runtime/ocaml-api/wasm_runtime_gen.ml
etherlink/lib_wasm_runtime/ocaml-api/wasm_runtime_gen.mli
src/rust_deps/rust_igd_next/ocaml-api/igd_next_gen.ml
src/rust_deps/rust_igd_next/ocaml-api/igd_next_gen.mli
src/rust_deps/rust_tezos_context/ocaml-api/rust_tezedge_gen.ml
src/rust_deps/rust_tezos_context/ocaml-api/rust_tezedge_gen.mli
etherlink/lwt_domain/lwt_domain.ml
etherlink/lwt_domain/lwt_domain.mli
src/rust_libcrux/src/octez_libcrux_ml_dsa.ml
src/rust_libcrux/src/octez_libcrux_ml_dsa.mli
EOF
)

update_all_dot_ocamlformats() {
  if ! type ocamlformat > /dev/null 2>&-; then
    echo "ocamlformat is required but could not be found. Aborting."
    exit 1
  fi

  if git diff --name-only HEAD --exit-code; then
    say "Repository clean :thumbsup:"
  else
    say "Repository not clean, which is required by this script."
    exit 2
  fi
  find "${source_directories[@]}" -name ".ocamlformat" -exec git rm {} \;
  # ocamlformat uses [.git], [.hg], and [dune-project] witness files
  # to determine the project root and will not cross that boundary
  # when computing its config. This means that we need a
  # '.ocamlformat' config file next to each dune-project in order to
  # cover all source code in the tree.
  interesting_directories=$(find "${source_directories[@]}" -name "dune-project" -type f -print | sed 's:/[^/]*$::' | LC_COLLATE=C sort -u)
  for d in $interesting_directories; do
    ofmt=$d/.ocamlformat
    cp .ocamlformat "$ofmt"
    git add "$ofmt"
  done
  # we don't want to reformat protocols (but alpha and stabilised protocols) because it would alter its hash
  protocols=$(find src/ -maxdepth 1 -name "proto_*" -not -name "proto_alpha")
  for d in $protocols; do
    (cd "$d/lib_protocol" && (find ./ -maxdepth 1 -name "*.ml*" | sed 's:^./::' | LC_COLLATE=C sort > ".ocamlformat-ignore"))
    git add "$d/lib_protocol/.ocamlformat-ignore"
  done
}

function shellcheck_script() {
  shellcheck --external-sources "$1"
}

function shfmt_script() {
  shfmt -i 2 -sr -d "$1"
}

function shfmt_script_write() {
  shfmt -w -i 2 -sr -d "$1"
}

format_scripts() {
  scripts=$(find "${source_directories[@]}" scripts docs -name "*.sh" -type f -print)

  for script in ${scripts}; do
    shfmt_script_write "$script"
  done
}

check_scripts() {
  # Gather scripts
  scripts=$(find "${source_directories[@]}" scripts docs -name "*.sh" -type f -print)
  exit_code=0

  # Check scripts do not contain the tab character
  tab="$(printf '%b' '\t')"
  for f in $scripts; do
    if grep -q "$tab" "$f"; then
      say "$f has tab character(s) ❌️"
      exit_code=1
    fi
  done

  # Execute shellcheck
  ./scripts/shellcheck_version.sh || return 1 # Check shellcheck's version

  shellcheck_skips=""
  while read -r shellcheck_skip; do
    shellcheck_skips+=" $shellcheck_skip"
  done < "scripts/shellcheck_skips"

  for script in ${scripts}; do
    if [[ "${shellcheck_skips}" == *"${script}"* ]]; then
      # check whether the skipped script, in reality, is warning-free
      if shellcheck_script "${script}" > /dev/null; then
        say "$script shellcheck marked as SKIPPED but actually pass: update shellcheck_skips ❌️"
        exit_code=1
      else
        # script is skipped, we leave a log however, to incite
        # devs to enhance the scripts
        say "$script shellcheck SKIPPED ⚠️"
      fi
    else
      # script is not skipped, let's shellcheck it
      if shellcheck_script "${script}"; then
        say "$script shellcheck PASSED ✅"
      else
        say "$script shellcheck FAILED ❌"
        exit_code=1
      fi
    fi
    if ! shfmt_script "${script}"; then
      say "$script shfmt FAILED ❌"
      exit_code=1
    fi
  done
  # Check that shellcheck_skips doesn't contain a deprecated value
  for shellcheck_skip in ${shellcheck_skips}; do
    if [[ ! -e "${shellcheck_skip}" ]]; then
      say "$shellcheck_skip is mentioned in shellcheck_skips, but doesn't exist anymore"
      say "please delete it from shellcheck_skips"
      exit_code=1
    fi
  done
  # Done executing shellcheck

  exit $exit_code
}

check_redirects() {
  if [[ ! -f docs/_build/_redirects.s3 ]]; then
    say "check-redirects should be run after building the full documentation,"
    say "i.e. by running 'make all && make -C docs all'"
    exit 1
  fi

  exit_code=0
  while read -r old new code; do
    re='^#'
    if [[ $old =~ $re ]]; then continue; fi
    re='^[0-9]+$'
    if ! [[ $code =~ $re && $code -ge 300 ]]; then
      say "in docs/_redirects: redirect $old -> $new has erroneous status code \"$code\""
      exit_code=1
    fi
    re='^https?://'
    re2=':splat'
    dest_local=docs/_build${new}
    if [[ ! $new =~ $re && ! $new =~ $re2 && ! -f $dest_local ]]; then
      say "in docs/_redirects: redirect $old -> $new, $dest_local does not exist"
      exit_code=1
    fi
  done < docs/_build/_redirects.s3
  exit $exit_code
}

check_rust_toolchain_files() {
  authorized_version=("1.88.0")

  declare -a rust_toolchain_files
  mapfile -t rust_toolchain_files <<< "$(find src/ -name rust-toolchain)"

  for file in "${rust_toolchain_files[@]}"; do
    if [[ ! "${authorized_version[*]}" =~ $(cat "${file}") ]]; then
      say "in ${file}: version $(cat "${file}") is not authorized"
      exit 1
    fi
  done
}

check_licenses_git_new() {
  if [ -z "${CHECK_LICENSES_DIFF_BASE:-}" ]; then
    echo 'Action --check-licenses-git-new requires that CHECK_LICENSES_DIFF_BASE is set in the environment.'
    echo 'The value of CHECK_LICENSES_DIFF_BASE should be a commit.'
    echo 'It is used to discover files that have been added and whose license header should be checked.'
    echo
    echo "Typically, it should point to the merge base of the current branch, as given by \`git merge-base\`:"
    echo
    echo "  CHECK_LICENSES_DIFF_BASE=\$(git merge-base HEAD origin/master) $0 --check-licenses-git-new"
    return 1
  elif ! git cat-file -t "${CHECK_LICENSES_DIFF_BASE:-}" > /dev/null 2>&1; then
    echo "The commit specified in CHECK_LICENSES_DIFF_BASE ('$CHECK_LICENSES_DIFF_BASE') could not be found."
    echo 'Consider running:'
    echo
    echo "  git fetch origin $CHECK_LICENSES_DIFF_BASE"
    return 1
  fi

  diff=$(mktemp)
  git diff-tree --no-commit-id --name-only -r --diff-filter=A \
    "${CHECK_LICENSES_DIFF_BASE:-}" HEAD -- "${source_directories[@]}" > "$diff"
  if [ -n "$license_check_exclude" ]; then
    diff2=$(mktemp)
    # 'grep -v' will exit with a non-zero exit code when no lines are selected.
    # consequently, if $diff \ $license_check_exclude is empty, the
    # grep will fail, which we want to allow.
    grep -v "$license_check_exclude" "$diff" > "$diff2" || true
    mv "$diff2" "$diff"
  fi

  # Check that new ml(i) files have a valid license header.
  if ! grep '\.mli\?$' "$diff" | xargs --no-run-if-empty ocaml scripts/check_license/main.ml --verbose; then

    echo "/!\\ Some files .ml(i) does not have a correct license header /!\\"
    echo "/!\\ See https://tezos.gitlab.io/developer/guidelines.html#license /!\\"

    res=1
  else
    echo "OCaml file license headers OK!"
    res=0
  fi

  rm -f "$diff"
  return $res
}

check_jsonnet_format() {

  function jsonnetfmt_script() {
    jsonnetfmt --test grafazos/src/**/*.jsonnet
  }

  if jsonnetfmt_script; then
    echo "Jsonnet files correctly formatted ✅"
  else
    echo "Jsonnet files are not correctly formatted (or formatting failed). ❌"
    echo "Consider running: make -C grafazos/ fmt"
    exit 1
  fi

}

# Check the linting for Jsonnet files
# We check file by file to prevent a known jsonnet-lint bug
# when checking several files at once.
check_jsonnet_lint() {

  all_files_ok=true

  lint_file() {

    set +e

    local file=$1

    RET=0
    function jsonnetlint_script() {
      jsonnet-lint -J grafazos/vendor/ "$file"
      RET="$?"
    }

    jsonnetlint_script &> /dev/null
    lint_output=$(jsonnetlint_script 2>&1)

    if [ "$RET" == "0" ]; then
      echo "$file is correctly linted ✅"
    else
      echo "Error in $file ❌"
      echo "Error details:"
      echo "$lint_output"
      all_files_ok=false
    fi
  }

  files=(grafazos/**/*.jsonnet)

  for file in "${files[@]}"; do
    echo "checking $file..."
    lint_file "$file"
  done

  set -e

  if "$all_files_ok"; then
    echo "All files are correctly linted! ✅"
    exit 0
  else
    echo "Some jsonnet files have linting errors. ❌"
    exit 1
  fi
}

# Mempool in lib_plugin is usually removed when the associated protocol is frozen.
# However to replay Ghostnet's history, it is necessary to keep a fix in the
# mempool's validation.
check_quebec_plugin_ghostnet_fix() {
  count=$(grep -ho "fix_ghostnet_state" src/proto_021_PsQuebec/lib_plugin/ghostnet_fix.ml src/proto_021_PsQuebec/lib_plugin/plugin_registerer.ml | wc -l)
  if [[ "$count" -ne 2 ]]; then
    echo "Missing 'fix_ghostnet_state' in Quebec plugin."
    exit 1
  fi
  exit 0
}

if [ $# -eq 0 ] || [[ "$1" != --* ]]; then
  say "provide one action (see --help)"
  exit 1
else
  action="$1"
  shift
fi

check_clean=false
commit=
on_files=false

case "$action" in
"--update-ocamlformat")
  action=update_all_dot_ocamlformats
  commit="Update .ocamlformat files"
  ;;
"--check-ocamlformat")
  action=update_all_dot_ocamlformats
  check_clean=true
  ;;
"--check-scripts")
  action=check_scripts
  ;;
"--format-scripts")
  action=format_scripts
  ;;
"--check-redirects")
  action=check_redirects
  ;;
"--check-rust-toolchain")
  action=check_rust_toolchain_files
  ;;
"--check-licenses-git-new")
  action=check_licenses_git_new
  ;;
"--check-jsonnet-format")
  action=check_jsonnet_format
  ;;
"--check-jsonnet-lint")
  action=check_jsonnet_lint
  ;;
"--check-quebec-plugin-ghostnet-fix")
  action=check_quebec_plugin_ghostnet_fix
  ;;
"help" | "-help" | "--help" | "-h")
  usage
  exit 0
  ;;
*)
  say "Error no action (arg 1 = '$action') provided"
  usage
  exit 2
  ;;
esac

if $on_files; then
  declare -a input_files files ignored_files
  input_files=()
  while [ $# -gt 0 ]; do
    if [ "$1" = "--ignore" ]; then
      shift
      break
    fi
    input_files+=("$1")
    shift
  done

  if [ ${#input_files[@]} -eq 0 ]; then
    mapfile -t input_files <<< "$(find "${source_directories[@]}" \( -name "*.ml" -o -name "*.mli" -o -name "*.mlt" \) -type f -print)"
  fi

  ignored_files=("$@")

  # $input_files may contain `*.pp.ml{i}` files which can't be linted. They
  # are filtered by the following loop.
  #
  # Note: another option would be to filter them before calling the script
  # but it was more convenient to do it here.
  files=()
  for file in "${input_files[@]}"; do
    if [[ "$file" == *.pp.ml?(i) ]]; then continue; fi
    for ignored_file in "${ignored_files[@]}"; do
      if [[ "$file" =~ ^(.*/)?"$ignored_file"$ ]]; then continue 2; fi
    done
    files+=("$file")
  done
  $action "${files[@]}"
else
  if [ $# -gt 0 ]; then
    usage
    exit 1
  fi
  $action
fi

if [ -n "$commit" ]; then
  git commit -m "$commit"
fi

if $check_clean; then
  echo "Files that differ but that shouldn't:"
  git diff --name-only HEAD --exit-code
  echo "(none, everything looks good)"
fi
