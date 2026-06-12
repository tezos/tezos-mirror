#!/bin/sh

set -eu

# Timing and summary helpers
total_steps=0
failed_steps=0
step_times=""

run_step() {
  step_name="$1"
  shift
  total_steps=$((total_steps + 1))
  start_time=$(date +%s)
  # Capture output; only display it on failure
  if output=$("$@" 2>&1); then
    elapsed=$(($(date +%s) - start_time))
    step_times="${step_times}${step_name}|${elapsed}|OK\n"
  else
    rc=$?
    elapsed=$(($(date +%s) - start_time))
    step_times="${step_times}${step_name}|${elapsed}|FAILED\n"
    failed_steps=$((failed_steps + 1))
    echo "=== FAILED: ${step_name} (${elapsed}s) ==="
    echo "$output"
    echo "=== END: ${step_name} ==="
    return $rc
  fi
}

print_summary() {
  echo ""
  echo "====== lint_misc_check summary ======"
  printf "%-45s %6s  %s\n" "Step" "Time" "Status"
  echo "--------------------------------------------------------------"
  total_time=0
  printf '%b' "$step_times" | while IFS='|' read -r name elapsed status; do
    [ -z "$name" ] && continue
    total_time=$((total_time + elapsed))
    printf "%-45s %5ss  %s\n" "$name" "$elapsed" "$status"
  done
  echo "--------------------------------------------------------------"
  echo "Total steps: ${total_steps}, failed: ${failed_steps}"
  echo "======================================"
}

# Track overall exit code: continue through all steps to report full summary
exit_code=0

# misc linting
run_step "check-opam-linting" make check-opam-linting || exit_code=1

run_step "check-scripts (shellcheck+shfmt)" scripts/lint.sh --check-scripts || exit_code=1
run_step "check-rust-toolchain" scripts/lint.sh --check-rust-toolchain || exit_code=1

# Mempool in lib_plugin is usually removed when the associated protocol is frozen.
# However to replay Ghostnet's history, it is necessary to keep a fix in the
# mempool's validation.
run_step "check-quebec-plugin-ghostnet-fix" scripts/lint.sh --check-quebec-plugin-ghostnet-fix || exit_code=1

# Ensure that all unit tests are restricted to their opam package
run_step "lint-tests-pkg" make lint-tests-pkg || exit_code=1

# FIXME: https://gitlab.com/tezos/tezos/-/issues/2971
# The new version of odoc (2.1.0) is stricter than the old version (1.5.3),
# we temporarily deactivate the odoc checks.
## Ensure there are no mli docstring syntax errors in alpha protocol
#- ODOC_WARN_ERROR=true dune build @src/proto_alpha/lib_protocol/doc
# check that the hack-module patch applies cleanly (--check = dry-run,
# does not modify the working tree)
run_step "apply hack-module patch" git apply --check devtools/protocol-print/add-hack-module.patch || exit_code=1

# Check that the protocol profiling patches can still be applied
run_step "patch-profiler-proto (dry-run)" scripts/patch-profiler-proto.sh --dry-run || exit_code=1

print_summary

exit $exit_code
