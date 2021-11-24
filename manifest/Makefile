# To be able to bootstrap this, we don't use any tool to build (not dune in particular).
# This allows to run this before make build-deps as long as OCaml is installed.

SOURCE=manifest.mli manifest.ml main.mli main.ml

.PHONY: all
all: manifest
	./manifest

# We compile files in the _build directory.
# But we prepend them with an OCaml compiler directive that ensures
# error messages are localized in the source directory.
_build/%: %
	@mkdir -p _build
	echo "# 1 \"$*\"" > $@
	cat $* >> $@

manifest: $(foreach file, $(SOURCE), _build/$(file))
	ocamlc -bin-annot -g -w @1..3@5..28@30..39@43@46..47@49..57@61..62 \
		-I _build $^ -o manifest

# Used in the CI.
.PHONY: check
check: all
	@git diff HEAD --exit-code || ( \
	  echo "Repository not clean after 'make -C manifest'."; \
	  echo "You should not edit generated dune and .opam files directly."; \
	  echo "Edit manifest/main.ml instead."; \
	  echo "Then run 'make -C manifest' and commit the difference."; \
	  exit 1 \
	)

.PHONY: clean
clean:
	rm -rf _build manifest
