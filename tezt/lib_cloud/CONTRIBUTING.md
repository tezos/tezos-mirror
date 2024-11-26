# Coding Guidelines

- The library `tezt-cloud` is designed to remain independent of Octez.
  One of the primary reasons for maintaining this independence is that
  it promotes better abstractions that are more modular and
  extensible. However, since `tezt-cloud` is primarily used within the
  Tezos codebase, breaking this invariant is allowed temporarily or if
  necessary.

- CLI arguments for `tezt-cloud` extend those of Tezt. They are also
  intended to remain independent of Tezos. When adding a new CLI
  command, pay attention to its default value. The default value
  should ensure the following:
  
  - With `--localhost`, it does not require access to GCP.
  - In most use cases, the default value is the desirable one.
  - Existing commands remain functional and unaffected.
  - Deployment time is not significantly increased.

- The public API of `tezt-cloud` should be user-friendly and does not
  need to mirror the internal API. This distinction is handled by the
  `tezt_cloud.mli` interface. Be sure to have docstrings for all the
  functions exported by this interface.

- For each service implemented in `tezt-cloud`, there should be a
  single module dedicated to it (e.g., `prometheus.ml`, `grafana.ml`,
  etc.).

- If you add file required by `tezt-cloud` at runtime (such as a
  configuration file), ensure to update `proxy.ml` accordingly.
