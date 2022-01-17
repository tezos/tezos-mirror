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
all: accusations_simple_double_endorsing

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
