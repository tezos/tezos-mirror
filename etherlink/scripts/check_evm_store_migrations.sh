#!/bin/sh

RES=0
GOLDEN="etherlink/script-inputs/evm_store_migrations"
CURRENT="$(mktemp)"

COMMAND="${1}"

find etherlink/bin_node/migrations/ etherlink/bin_node/tezlink_migrations/ -name "*.sql" -print |
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
