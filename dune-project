(lang dune 3.7)
(formatting (enabled_for ocaml))
(cram enable)
(using ctypes 0.3)
(using menhir 2.1)
(package (name RPC-toy))
(package (name bls12-381))
(package (name dal_node_migrations))
(package (name efunc_core))
(package (name etherlink-benchmark-producer))
(package (name etherlink-governance-observer))
(package (name etherlink-outbox-monitor))
(package (name fa-bridge-watchtower))
(package (name floodgate))
(package (name gitlab_ci))
(package (name internal-devtools))
(package (name internal-devtools_proto-context-du))
(package (name kaitai))
(package (name kaitai-of-data-encoding))
(package (name octez-accuser))
(package (name octez-accuser-PsRiotum))
(package (name octez-accuser-PtSeouLo))
(package (name octez-accuser-alpha))
(package (name octez-alcotezt))
(package (name octez-baker))
(package (name octez-baker-PsRiotum))
(package (name octez-baker-PtSeouLo))
(package (name octez-baker-alpha))
(package (name octez-baker-lib))
(package (name octez-client))
(package (name octez-codec))
(package (name octez-codec-kaitai))
(package (name octez-connpool))
(package (name octez-crawler))
(package (name octez-dal-node))
(package (name octez-distributed-internal))
(package (name octez-distributed-lwt-internal))
(package (name octez-evm-node))
(package (name octez-evm-node-libs))
(package (name octez-evm-node-tests)(allow_empty))
(package (name octez-evm-wasm-runtime-tests)(allow_empty))
(package (name octez-igd-next))
(package (name octez-injector))
(package (name octez-injector-server))
(package (name octez-internal-libs))
(package (name octez-l2-libs))
(package (name octez-lib-upnp))
(package (name octez-lib-upnp-args))
(package (name octez-libs))
(package (name octez-node))
(package (name octez-node-config))
(package (name octez-performance-metrics))
(package (name octez-proto-libs))
(package (name octez-protocol-000-Ps9mPmXa-libs))
(package (name octez-protocol-001-PtCJ7pwo-libs))
(package (name octez-protocol-002-PsYLVpVv-libs))
(package (name octez-protocol-003-PsddFKi3-libs))
(package (name octez-protocol-004-Pt24m4xi-libs))
(package (name octez-protocol-005-PsBabyM1-libs))
(package (name octez-protocol-006-PsCARTHA-libs))
(package (name octez-protocol-007-PsDELPH1-libs))
(package (name octez-protocol-008-PtEdo2Zk-libs))
(package (name octez-protocol-009-PsFLoren-libs))
(package (name octez-protocol-010-PtGRANAD-libs))
(package (name octez-protocol-011-PtHangz2-libs))
(package (name octez-protocol-012-Psithaca-libs))
(package (name octez-protocol-013-PtJakart-libs))
(package (name octez-protocol-014-PtKathma-libs))
(package (name octez-protocol-015-PtLimaPt-libs))
(package (name octez-protocol-016-PtMumbai-libs))
(package (name octez-protocol-017-PtNairob-libs))
(package (name octez-protocol-018-Proxford-libs))
(package (name octez-protocol-019-PtParisB-libs))
(package (name octez-protocol-020-PsParisC-libs))
(package (name octez-protocol-021-PsQuebec-libs))
(package (name octez-protocol-022-PsRiotum-libs))
(package (name octez-protocol-023-PtSeouLo-libs))
(package (name octez-protocol-alpha-libs))
(package (name octez-protocol-compiler))
(package (name octez-protocol-compiler-compat))
(package (name octez-requester-tests)(allow_empty))
(package (name octez-riscv-api))
(package (name octez-riscv-pvm))
(package (name octez-riscv-pvm-test)(allow_empty))
(package (name octez-rpc-process))
(package (name octez-rust-deps))
(package (name octez-rustzcash-deps))
(package (name octez-shell-libs))
(package (name octez-shell-tests)(allow_empty))
(package (name octez-signer))
(package (name octez-smart-rollup-node))
(package (name octez-smart-rollup-node-Proxford)(allow_empty))
(package (name octez-smart-rollup-node-PsParisC)(allow_empty))
(package (name octez-smart-rollup-node-PsQuebec)(allow_empty))
(package (name octez-smart-rollup-node-PsRiotum)(allow_empty))
(package (name octez-smart-rollup-node-PtNairob)(allow_empty))
(package (name octez-smart-rollup-node-PtParisB)(allow_empty))
(package (name octez-smart-rollup-node-PtSeouLo)(allow_empty))
(package (name octez-smart-rollup-node-alpha)(allow_empty))
(package (name octez-smart-rollup-node-lib))
(package (name octez-smart-rollup-wasm-debugger))
(package (name octez-smart-rollup-wasm-debugger-lib))
(package (name octez-smart-rollup-wasm-debugger-plugin))
(package (name octez-snoop))
(package (name octez-store-tests)(allow_empty))
(package (name octez-testnet-scenarios))
(package (name octez-teztale))
(package (name octez-version))
(package (name octogram))
(package (name proto-manager))
(package (name tezos-benchmark))
(package (name tezos-benchmark-022-PsRiotum))
(package (name tezos-benchmark-023-PtSeouLo))
(package (name tezos-benchmark-alpha))
(package (name tezos-benchmark-examples))
(package (name tezos-benchmark-tests)(allow_empty))
(package (name tezos-benchmark-type-inference-022-PsRiotum))
(package (name tezos-benchmark-type-inference-023-PtSeouLo))
(package (name tezos-benchmark-type-inference-alpha))
(package (name tezos-benchmarks-proto-022-PsRiotum))
(package (name tezos-benchmarks-proto-023-PtSeouLo))
(package (name tezos-benchmarks-proto-alpha))
(package (name tezos-client-demo-counter))
(package (name tezos-client-genesis))
(package (name tezos-dal-node-lib))
(package (name tezos-dal-node-services))
(package (name tezos-injector-022-PsRiotum)(allow_empty))
(package (name tezos-injector-023-PtSeouLo)(allow_empty))
(package (name tezos-injector-alpha)(allow_empty))
(package (name tezos-lazy-containers-tests)(allow_empty))
(package (name tezos-micheline-rewriting))
(package (name tezos-openapi))
(package (name tezos-protocol-000-Ps9mPmXa))
(package (name tezos-protocol-001-PtCJ7pwo))
(package (name tezos-protocol-002-PsYLVpVv))
(package (name tezos-protocol-003-PsddFKi3))
(package (name tezos-protocol-004-Pt24m4xi))
(package (name tezos-protocol-005-PsBABY5H))
(package (name tezos-protocol-005-PsBabyM1))
(package (name tezos-protocol-006-PsCARTHA))
(package (name tezos-protocol-007-PsDELPH1))
(package (name tezos-protocol-008-PtEdo2Zk))
(package (name tezos-protocol-008-PtEdoTez))
(package (name tezos-protocol-009-PsFLoren))
(package (name tezos-protocol-010-PtGRANAD))
(package (name tezos-protocol-011-PtHangz2))
(package (name tezos-protocol-012-Psithaca))
(package (name tezos-protocol-013-PtJakart))
(package (name tezos-protocol-014-PtKathma))
(package (name tezos-protocol-015-PtLimaPt))
(package (name tezos-protocol-016-PtMumbai))
(package (name tezos-protocol-017-PtNairob))
(package (name tezos-protocol-018-Proxford))
(package (name tezos-protocol-019-PtParisB))
(package (name tezos-protocol-020-PsParisC))
(package (name tezos-protocol-021-PsQuebec))
(package (name tezos-protocol-022-PsRiotum))
(package (name tezos-protocol-022-PsRiotum-tests)(allow_empty))
(package (name tezos-protocol-023-PtSeouLo))
(package (name tezos-protocol-023-PtSeouLo-tests)(allow_empty))
(package (name tezos-protocol-alpha))
(package (name tezos-protocol-alpha-tests)(allow_empty))
(package (name tezos-protocol-demo-counter))
(package (name tezos-protocol-demo-noops))
(package (name tezos-protocol-genesis))
(package (name tezos-sc-rollup-node-test)(allow_empty))
(package (name tezos-scoru-wasm-regressions)(allow_empty))
(package (name tezos-smart-rollup-node-lib-test)(allow_empty))
(package (name tezos-tooling))
(package (name tezos-tps-evaluation))
(package (name tezos-tree-encoding-test)(allow_empty))
(package (name tezos_internal_brassaia_eio_tests)(allow_empty))
(package (name tezos_internal_brassaia_tests)(allow_empty))
(package (name tezos_internal_irmin_tests)(allow_empty))
(package (name tezt-cloud))
(package (name tezt-etherlink)(allow_empty))
(package (name tezt-tests-cloud)(allow_empty))
(package (name tezt-tezos))
(package (name tezt-tx-kernel)(allow_empty))
(package (name tobi))
; This file was automatically generated, do not edit.
; Edit file manifest/main.ml instead.
