# devtools

This is a place designed to development only tools, it may include, patches, suggestions, ppxes and others. **Tools here should not be used to make release builds, for testing or on the CI**.

## Tools

- protocol-print: This allows printing inside the protocol files for debugging purposes.
- utop-fix (deprecated by [tztop](../src/tooling/tztop/README.md)): allows utop to be used globally in Tezos
- regenerating `src/proto_alpha/lib_protocol/*_costs_generated.ml` in case of any changes to gas values or codegen behaviour.(`protocol_snoop_codegen/run.sh`)
