# To be able to bootstrap this, we don't use any tool to build (not dune in particular).
# This allows to run this before make build-deps as long as OCaml is installed.

SOURCE=JSON_AST.ml JSON_parser.mli JSON_parser.ml JSON_lexer.ml \
       dgraph.mli dgraph.ml \
       tezos_protocol.ml \
       manifest_link_deps.mli manifest_link_deps.ml \
       manifest.mli manifest.ml \
       externals.ml \
       internals.ml \
       product_data_encoding.mli product_data_encoding.ml \
       product_websocket.mli product_websocket.ml \
       product_lwt_domain.ml \
       product_prometheus.mli product_prometheus.ml \
       product_resto.mli product_resto.ml \
       product_efunc_core.mli product_efunc_core.ml \
       product_octez.mli product_octez.ml \
       product_client_libs.mli product_client_libs.ml \
       product_tooling.mli product_tooling.ml \
       product_etherlink.mli product_etherlink.ml \
       product_teztale.ml \
       product_tezindex.ml \
       product_tezt_cloud_tests.ml \
       product_ciao.ml \
       product_tobi.ml \
       main.mli main.ml

.PHONY: all
all: manifest
	(cd .. && manifest/manifest)

_build/JSON_parser.ml _build/JSON_parser.mli: _build/JSON_parser.mly
	ocamlyacc $^

_build/JSON_lexer.ml: _build/JSON_lexer.mll
	ocamllex $^

# We compile files in the _build directory.
# But we prepend them with an OCaml compiler directive that ensures
# error messages are localized in the source directory.
_build/%: %
	@mkdir -p _build
	echo "# 1 \"$*\"" > $@
	cat $* >> $@

manifest: $(foreach file, $(SOURCE), _build/$(file))
	ocamlc -bin-annot -g -w @1..3@5..28@30..39@43@46..47@49..57@61..62 \
		-I _build -I +str str.cma $^ -o manifest

# Running 'make -C manifest l1-deps.svg' produces
# the dependency graph of the main Layer 1 executables
# (node, client, baker, accuser) in 'manifest/octez-deps.svg'.
# It requires the 'dot' executable (apt install graphviz).
l1-deps.dot: manifest
	(cd .. && manifest/manifest --dep-graph manifest/$@ \
		--dep-graph-source src/bin_node \
		--dep-graph-source src/bin_client \
		--dep-graph-source src/proto_alpha/bin_baker \
		--dep-graph-source src/proto_alpha/bin_accuser \
		--dep-graph-without src/proto_0)

rollup-deps.dot: manifest
	(cd .. && manifest/manifest --dep-graph manifest/$@ \
		--dep-graph-source src/bin_smart_rollup_node \
		--dep-graph-without src/proto_0)

evm-deps.dot: manifest
	(cd .. && manifest/manifest --dep-graph manifest/$@ \
		--dep-graph-source etherlink/bin_node)

dal-deps.dot: manifest
	(cd .. && manifest/manifest --dep-graph manifest/$@ \
		--dep-graph-source src/bin_dal_node \
		--dep-graph-without src/proto_0)

opam-deps.dot: manifest
	(cd .. && manifest/manifest --opam-dep-graph manifest/$@ \
		--dep-graph-without octez-protocol-0 \
		--dep-graph-without tezos-protocol-0)

l1-opam-deps.dot: manifest
	(cd .. && manifest/manifest --opam-dep-graph manifest/$@ \
		--dep-graph-source octez-node \
		--dep-graph-source octez-client \
		--dep-graph-source octez-baker- \
		--dep-graph-source octez-accuser- \
		--dep-graph-without octez-protocol-0 \
		--dep-graph-without tezos-protocol-0)

rollup-opam-deps.dot: manifest
	(cd .. && manifest/manifest --opam-dep-graph manifest/$@ \
		--dep-graph-source -rollup- \
		--dep-graph-without octez-protocol-0 \
		--dep-graph-without tezos-protocol-0)

evm-opam-deps.dot: manifest
	(cd .. && manifest/manifest --opam-dep-graph manifest/$@ \
		--dep-graph-source -evm- \
		--dep-graph-without octez-protocol-0 \
		--dep-graph-without tezos-protocol-0)

dal-opam-deps.dot: manifest
	(cd .. && manifest/manifest --opam-dep-graph manifest/$@ \
		--dep-graph-source -dal- \
		--dep-graph-without octez-protocol-0 \
		--dep-graph-without tezos-protocol-0)

%.svg: %.dot
	dot -Tsvg $^ > $@

%.png: %.dot
	dot -Tpng $^ > $@

# Used in the CI.
.PHONY: check
check: all
	@git status | grep "nothing to commit" > /dev/null || ( \
	  echo "Repository not clean after 'make -C manifest'."; \
	  echo "You should not edit generated dune and .opam files directly."; \
	  echo "Edit manifest/main.ml instead."; \
	  echo "Then run 'make -C manifest' and commit the difference."; \
		git status; \
	  exit 1 \
	)

.PHONY: clean
clean:
	rm -rf _build manifest l1-deps.dot l1-deps.svg
