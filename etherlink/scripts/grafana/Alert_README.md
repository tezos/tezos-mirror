Before importing the alert, you should replace `${instance}`, `${receiver}`, and `${datasourceUid}` in the JSON file.

An example for `${instance}` is `etherlink-mainnet-rollup-node` for the mainnet.

An example for `${receiver}` is `slack`.

As for `${datasourceUid}`, you need to retrieve the UID of your datasource.

Replace ${DB_SOURCE} by the uid of your choice.

To import the alert, place the JSON file in the Grafana provisioning alerting directory.
An example of the path is `/etc/grafana/provisioning/alerting/`.
