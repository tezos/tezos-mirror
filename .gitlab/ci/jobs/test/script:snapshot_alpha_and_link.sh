#!/bin/sh

last_proto_name=$(find src -name "proto_[0-9][0-9][0-9]_*" | awk -F'/' '{print $$NF}' | sort -r | head -1)
last_proto_version=$(echo "${last_proto_name}" | cut -d'_' -f2 | sed 's/^0*//')
new_proto_version=$(printf "%03d" $(( last_proto_version + 1 )))

make octez-protocol-compiler
./scripts/snapshot_alpha_and_link.sh "${new_proto_version}" next
./scripts/remove-old-protocols.sh
make

dune build src/proto_"${new_proto_version}"_*/
./scripts/user_activated_upgrade.sh src/proto_"${new_proto_version}"_*/ 1
dune build src/lib_node_config # `user_activated_upgrade.sh` only modifies `lib_node_config`
