#!/bin/sh

# This file is a copy of the one used for EVM node migrations.
# https://gitlab.com/tezos/tezos/-/blob/7327ffcad13ec75bf2945fb7d30b0dda011911e6/etherlink/scripts/check_evm_store_migrations.sh

RES=0
GOLDEN="src/lib_dal_node/script-inputs/dal_store_migrations"
CURRENT="$(mktemp)"

COMMAND="${1}"

find src/lib_dal_node/migrations/ -name "*.sql" -print |
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
