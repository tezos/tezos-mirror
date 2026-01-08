# lib_protocol tests

This folder contains unit, integration and property-based tests for
the economic protocol definition. The tests are organized in
sub-folders: first by type of test, and for integration, a further
subdivision by theme:

- `unit`: tests that sit below `Alpha_context`.
- `integration`: tests that require passing around a context.
  - `michelson`: tests that involve Micheline expressions.
  - `consensus`: tests for consensus: baking, attestation, etc.
  - `gas`: tests for gas.
  - `operations`: test for operations.
- `pbt`: for property-based tests using `qcheck`.

Finally, `helpers/` contains common definitions for writing tests.

There might not be a clear-cut location for new tests. For new
integration tests, either add them directly to `integration/` or
create a new sub-folder corresponding to the theme of the test.

# Running

To run all the tests, run:

```
dune runtest src/proto_alpha/lib_protocol/
```

To run an individual test file, consult its `Invocation` header.
