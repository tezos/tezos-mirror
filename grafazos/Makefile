all: basic logs full compact dal-basic profiling tezt_cloud-layer1

NODE_INSTANCE_LABEL ?= instance
LOGS_LABEL ?= job
ENDPOINT_LABEL ?= endpoint
HARDWARE_SRC ?= netdata
STORAGE_MODE ?= default
UID ?= default
DATASOURCE_SELECTION ?= false
DATASOURCE_DEFAULT ?= Prometheus

%.jsonnet:
	jsonnet \
		-J vendor/ \
		--ext-str node_instance_label="$(NODE_INSTANCE_LABEL)" \
		--ext-str logs_label="$(LOGS_LABEL)" \
		--ext-str endpoint_label="$(ENDPOINT_LABEL)" \
		--ext-str hardware_src="$(HARDWARE_SRC)" \
		--ext-str storage_mode="$(STORAGE_MODE)" \
		--ext-str mountpoint="$(MOUNTPOINT)" \
		--ext-str logsrc="$(LOGSRC)" \
		--ext-str gcp_project="$(GCP_PROJECT_ID)" \
		--ext-str gcp_datasource_uid="$(GCP_DATASOURCE_UID)" \
		--ext-str uid_ext="$(UID)" \
		--ext-str datasource_selection="$(DATASOURCE_SELECTION)" \
		--ext-str datasource_default="$(DATASOURCE_DEFAULT)" \
		src/$@ \
			> output/$*.json

clean:
	rm output/*.json

install-jb:
	jb install github.com/grafana/grafonnet/gen/grafonnet-latest@main

fmt:
	find src -name '*.jsonnet' -exec jsonnetfmt -i {} \;

basic: octez-basic.jsonnet

logs : octez-with-logs.jsonnet

full: octez-full.jsonnet

compact: octez-compact.jsonnet

dal-basic: dal/dal-basic.jsonnet

profiling: octez-profiling.jsonnet

tezt_cloud-layer1: tezt_cloud-layer1.jsonnet
