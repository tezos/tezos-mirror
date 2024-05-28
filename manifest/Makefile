# To be able to bootstrap this, we don't use any tool to build (not dune in particular).
# This allows to run this before make build-deps as long as OCaml is installed.

SOURCE=JSON_AST.ml JSON_parser.mli JSON_parser.ml JSON_lexer.ml tezos_protocol.ml \
       manifest.mli manifest.ml \
       externals.ml \
       internals.ml \
       product_octez.mli product_octez.ml \
       product_client_libs.mli product_client_libs.ml \
       product_tooling.mli product_tooling.ml \
       product_etherlink.ml \
       product_ciao.ml \
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
		-I _build str.cma $^ -o manifest

# Used in the CI.
.PHONY: check
check: all
	@git status | grep "nothing to commit" > /dev/null || ( \
	  echo "Repository not clean after 'make -C manifest'."; \
	  echo "You should not edit generated dune and .opam files directly."; \
	  echo "Edit manifest/main.ml instead."; \
	  echo "Then run 'make -C manifest' and commit the difference."; \
	  exit 1 \
	)

.PHONY: clean
clean:
	rm -rf _build manifest
