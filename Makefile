all: basic full compact

BRANCH ?= master
NODE_INSTANCE_LABEL ?= instance

%.jsonnet:
	jsonnet \
		-J vendors/grafonnet-lib/grafonnet \
		--ext-str branch="$(BRANCH)" \
		--ext-str node_instance_label="$(NODE_INSTANCE_LABEL)" \
		src/$@ \
			> output/$*.json

clean:
	rm output/*.json

fmt:
	jsonnetfmt -i src/*.jsonnet

basic: octez-basic.jsonnet

full: octez-full.jsonnet

compact: octez-compact.jsonnet
