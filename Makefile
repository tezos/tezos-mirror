all: basic logs full compact

BRANCH ?= master
NODE_INSTANCE_LABEL ?= instance
STORAGE_MODE ?= default

%.jsonnet:
	jsonnet \
		-J vendors/grafonnet-lib/grafonnet \
		--ext-str branch="$(BRANCH)" \
		--ext-str node_instance_label="$(NODE_INSTANCE_LABEL)" \
		--ext-str storage_mode="$(STORAGE_MODE)" \
		src/$@ \
			> output/$*.json

clean:
	rm output/*.json

fmt:
	jsonnetfmt -i src/*.jsonnet

basic: octez-basic.jsonnet

logs : octez-with-logs.jsonnet

full: octez-full.jsonnet

compact: octez-compact.jsonnet
