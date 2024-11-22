all: build

build:
	@dune build src test/test.exe

clean:
	@dune clean

install:
	@opam install .

doc:
	@dune build src @doc
	@cp -rf _build/default/_doc/_html public

test-web:
	@dune build --profile release test/test_js.bc.js
	@cp -f _build/default/test/test_js.bc.js test/www
