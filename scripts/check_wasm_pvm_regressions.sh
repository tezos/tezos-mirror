#!/bin/sh

RES=0
FROZEN_VERSIONS="script-inputs/frozen_wasm_pvm_versions"
GOLDEN="script-inputs/wasm_pvm_golden"
CURRENT="$(mktemp)"

COMMAND="${1}"

while read -r version
do
  find src/lib_scoru_wasm/regressions/expected/tezos_scoru_wasm_regressions.ml/ \
    -name "*(hash- ${version}).out" \
    -exec sha256sum {} \; >> "${CURRENT}"
done < "${FROZEN_VERSIONS}"

sort -o "${CURRENT}" "${CURRENT}"

case "${COMMAND}" in
  "check")
    diff "${GOLDEN}" "${CURRENT}"
    RES=$?
    rm "${CURRENT}"
    ;;
  "promote")
    mv "${CURRENT}" "${GOLDEN}"
    ;;
  *)
    echo "usage:"
    echo "  $0 check"
    echo "  $0 promote"
    ;;
esac

exit ${RES}
