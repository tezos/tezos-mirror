TMP=/tmp

CURRENT_PROTO=011-PtHangz2
CURRENT_PROTO_HASH=PtHangz2aRngywmSRGGvrcTyMbbdpWdpFKuS4uMWxg2RaH9i1qx
CURRENT_PROTO_NAME=Hangzhou
NEXT_PROTO=012-Psithaca
NEXT_PROTO_HASH=Psithaca2MLRFYargivpo7YvUr7wUDqyxrdhC5CQq78mRvimz6A
NEXT_PROTO_NAME=Ithaca
ALPHA_PROTO=alpha
ALPHA_PROTO_HASH=ProtoALphaALphaALphaALphaALphaALphaALphaALphaDdp3zK

.PHONY: all
all: accusations_simple_double_endorsing \
	user_activated_upgrade_next \
	user_activated_upgrade_alpha \
	daemons_upgrade_next \
	daemons_upgrade_alpha

# These are the targets that are actually run in ./.gitlab/ci/test/integration.yml
ci: all


# The following rules define how to build Tezos binaries if they are
# missing.
#
# We only run "dune build" if it is strictly necessary.  The CI stores
# the binaries as artefacts, but not _build.  Those rules allow us to
# use these artefacts.  Systematically running dune would cause a full
# rebuild since _build is not an artefact.
#
# The drawback of writing the rules this way is that if we need
# several binaries, we call dune several times, which takes a bit
# longer than calling it only once (dune scans the whole repository
# first).
#
# The real drawback of writing the rules this way is that during
# development, if you change something in the sources, as the binaries
# exist, nothing is recompiled!

tezos-node:
	dune build src/bin_node/main.exe
	cp _build/default/src/bin_node/main.exe $@

tezos-client:
	dune build src/bin_client/main_client.exe
	cp _build/default/src/bin_client/main_client.exe $@

tezos-admin-client:
	dune build src/bin_client/main_admin.exe
	cp _build/default/src/bin_client/main_admin.exe $@

tezos-sandbox:
	dune build src/bin_sandbox/main.exe
	cp _build/default/src/bin_sandbox/main.exe $@

# The following rules define how to run sandbox (i.e. Flextesa) integration tests.
# Note how base ports are well separated to be able to run them in parallel if needed
# (with make -j).
#
# Those rules are directly translated from src/bin_sandbox/dune rules.

.PHONY: accusations_simple_double_endorsing
accusations_simple_double_endorsing: tezos-sandbox tezos-client tezos-node
	./tezos-sandbox accusations simple-double-endorsing \
	  --root-path ${TMP}/flextesa-acc-sde/ \
	  --with-timestamp \
	  --base-port 11_000 \
	  --tezos-client-binary ./tezos-client \
	  --tezos-node-binary ./tezos-node \
	  --protocol-hash ${CURRENT_PROTO_HASH} \
	  --protocol-kind ${CURRENT_PROTO_NAME}

# The use of --second-endorser ./tezos-baker-${NEXT_PROTO} is a hack since
# there is no endorser binary on Tenderbake.
#
# Since user_activated_upgrade_* tests do not really use the endorsers but
# nonetheless check the presence of the file, we can substitute it by another
# file that we know to exist The same hack is applied for the
# daemons_upgrade_* target below
.PHONY: user_activated_upgrade_next
user_activated_upgrade_next: tezos-sandbox tezos-client tezos-node \
	tezos-baker-${CURRENT_PROTO} tezos-endorser-${CURRENT_PROTO} tezos-accuser-${CURRENT_PROTO} \
	tezos-baker-${NEXT_PROTO} tezos-accuser-${NEXT_PROTO}
	./tezos-sandbox mini-net \
	  --root-path ${TMP}/flextesa-hard-fork/ \
	  --base-port 13_000 \
	  --size 2 \
	  --number-of-b 2 \
	  --with-timestamp \
	  --until 20 \
	  --protocol-hash ${CURRENT_PROTO_HASH} \
	  --protocol-kind ${CURRENT_PROTO_NAME} \
	  --tezos-client ./tezos-client \
	  --tezos-node ./tezos-node \
	  --tezos-baker ./tezos-baker-${CURRENT_PROTO} \
	  --tezos-endorser ./tezos-endorser-${CURRENT_PROTO} \
	  --tezos-accuser ./tezos-accuser-${CURRENT_PROTO} \
	  --hard-fork 8:${NEXT_PROTO_HASH} \
	  --hard-fork-baker ./tezos-baker-${NEXT_PROTO} \
	  --hard-fork-endorser ./tezos-baker-${NEXT_PROTO} \
	  --hard-fork-accuser ./tezos-accuser-${NEXT_PROTO}

.PHONY: user_activated_upgrade_alpha
user_activated_upgrade_alpha: tezos-sandbox tezos-client tezos-node \
	tezos-baker-${CURRENT_PROTO} tezos-endorser-${CURRENT_PROTO} tezos-accuser-${CURRENT_PROTO} \
	tezos-baker-${ALPHA_PROTO} tezos-accuser-${ALPHA_PROTO}
	./tezos-sandbox mini-net \
	  --root-path ${TMP}/flextesa-hard-fork-alpha/ \
	  --base-port 14_000 \
	  --size 2 \
	  --number-of-b 2 \
	  --with-timestamp \
	  --until 20 \
	  --protocol-hash ${CURRENT_PROTO_HASH} \
	  --protocol-kind ${CURRENT_PROTO_NAME} \
	  --tezos-client ./tezos-client \
	  --tezos-node ./tezos-node \
	  --tezos-baker ./tezos-baker-${CURRENT_PROTO} \
	  --tezos-endorser ./tezos-endorser-${CURRENT_PROTO} \
	  --tezos-accuser ./tezos-accuser-${CURRENT_PROTO} \
	  --hard-fork 8:${ALPHA_PROTO_HASH} \
	  --hard-fork-baker ./tezos-baker-${ALPHA_PROTO} \
	  --hard-fork-endorser ./tezos-baker-${ALPHA_PROTO} \
	  --hard-fork-accuser ./tezos-accuser-${ALPHA_PROTO}

# See above the reason for --second-endorser ./tezos-baker-${NEXT_PROTO}
.PHONY: daemons_upgrade_next
daemons_upgrade_next: tezos-sandbox tezos-client tezos-admin-client tezos-node \
	tezos-baker-${CURRENT_PROTO} tezos-endorser-${CURRENT_PROTO} tezos-accuser-${CURRENT_PROTO} \
	tezos-baker-${NEXT_PROTO} tezos-accuser-${NEXT_PROTO}
	./tezos-sandbox daemons-upgrade \
	  src/proto_${subst -,_,${NEXT_PROTO}}/lib_protocol/TEZOS_PROTOCOL \
	  --root-path ${TMP}/flextesa-daemons-upgrade/ \
	  --base-port 15_000 \
	  --extra-dummy-proposals-batch-size 2 \
	  --extra-dummy-proposals-batch-levels 3,5 \
	  --size 2 \
	  --number-of-b 2 \
	  --time-betw 3 \
	  --blocks-per-vot 14 \
	  --with-timestamp \
	  --protocol-hash ${CURRENT_PROTO_HASH} \
	  --protocol-kind ${CURRENT_PROTO_NAME} \
	  --tezos-client ./tezos-client \
	  --tezos-admin ./tezos-admin-client \
	  --tezos-node ./tezos-node \
	  --first-baker ./tezos-baker-${CURRENT_PROTO} \
	  --first-endorser ./tezos-endorser-${CURRENT_PROTO} \
	  --first-accuser ./tezos-accuser-${CURRENT_PROTO} \
	  --second-baker ./tezos-baker-${NEXT_PROTO} \
	  --second-endorser ./tezos-baker-${NEXT_PROTO} \
	  --second-accuser ./tezos-accuser-${NEXT_PROTO}

.PHONY: daemons_upgrade_alpha
daemons_upgrade_alpha: tezos-sandbox tezos-client tezos-admin-client tezos-node \
	tezos-baker-${CURRENT_PROTO} tezos-endorser-${CURRENT_PROTO} tezos-accuser-${CURRENT_PROTO} \
	tezos-baker-${ALPHA_PROTO} tezos-accuser-${ALPHA_PROTO}
	./tezos-sandbox daemons-upgrade \
	  src/proto_${subst -,_,${ALPHA_PROTO}}/lib_protocol/TEZOS_PROTOCOL \
	  --root-path ${TMP}/flextesa-daemons-upgrade-alpha/ \
	  --base-port 16_000 \
	  --extra-dummy-proposals-batch-size 2 \
	  --extra-dummy-proposals-batch-levels 3,5 \
	  --size 2 \
	  --number-of-b 2 \
	  --time-betw 3 \
	  --blocks-per-vot 14 \
	  --with-timestamp \
	  --protocol-hash ${CURRENT_PROTO_HASH} \
	  --protocol-kind ${CURRENT_PROTO_NAME} \
	  --tezos-client ./tezos-client \
	  --tezos-admin ./tezos-admin-client \
	  --tezos-node ./tezos-node \
	  --first-baker ./tezos-baker-${CURRENT_PROTO} \
	  --first-endorser ./tezos-endorser-${CURRENT_PROTO} \
	  --first-accuser ./tezos-accuser-${CURRENT_PROTO} \
	  --second-baker ./tezos-baker-${ALPHA_PROTO} \
	  --second-endorser ./tezos-baker-${ALPHA_PROTO} \
	  --second-accuser ./tezos-accuser-${ALPHA_PROTO}
