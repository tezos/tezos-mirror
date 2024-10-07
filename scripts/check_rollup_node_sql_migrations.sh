#!/bin/sh

SCRIPT_DIR="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
cd "$SCRIPT_DIR"/.. || exit

RES=0
GOLDEN="script-inputs/rollup_node_sql_migrations"
CURRENT="$(mktemp)"

COMMAND="${1}"

find src/lib_smart_rollup_node/migrations/ -name "*.sql" -print |
  sort |
  xargs sha256sum \
    >> "${CURRENT}"

case "${COMMAND}" in
"check")
  diff -u "${GOLDEN}" "${CURRENT}"
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
