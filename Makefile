all: octez-basic.jsonnet

BRANCH ?= master

%.jsonnet:
	jsonnet \
		-J vendors/grafonnet-lib/grafonnet \
		--ext-str branch="$(BRANCH)" \
		src/$@ \
			> output/$*.generated

clean:
	rm output/*.generated

fmt:
	jsonnetfmt -i src/*.jsonnet
