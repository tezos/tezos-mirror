all: basic full

BRANCH ?= master

%.jsonnet:
	jsonnet \
		-J vendors/grafonnet-lib/grafonnet \
		--ext-str branch="$(BRANCH)" \
		src/$@ \
			> output/$*.json

clean:
	rm output/*.json

fmt:
	jsonnetfmt -i src/*.jsonnet

basic: octez-basic.jsonnet

full: octez-full.jsonnet
