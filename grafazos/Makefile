all: basic logs full compact dal-basic profiling tezt_cloud-layer1

NODE_INSTANCE_LABEL ?= instance
STORAGE_MODE ?= default

%.jsonnet:
	jsonnet \
		-J vendor/ \
		--ext-str node_instance_label="$(NODE_INSTANCE_LABEL)" \
		--ext-str storage_mode="$(STORAGE_MODE)" \
		--ext-str mountpoint="$(MOUNTPOINT)" \
		src/$@ \
			> output/$*.json

clean:
	rm output/*.json

fmt:
	find src -name '*.jsonnet' -exec jsonnetfmt -i {} \;

basic: octez-basic.jsonnet

logs : octez-with-logs.jsonnet

full: octez-full.jsonnet

compact: octez-compact.jsonnet

dal-basic: dal/dal-basic.jsonnet

profiling: octez-profiling.jsonnet

tezt_cloud-layer1: tezt_cloud-layer1.jsonnet
