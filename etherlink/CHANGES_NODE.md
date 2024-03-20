# Changelog

## Version for NEXT

### Features

### Bug fixes

### Breaking changes

### Internal

## Version for 0a81ce76b3d4f57d8c5194bcb9418f9294fd2be1

### Features

- The private RPC server is no longer launched by default. You need to provide
  the parameter `--private-rpc-port` to launch it. (!12449)
- Delayed EVM transactions no longer pay data-availability fee. (!12401)
- Stop block production if the rollup is lagging behind. (!12482)
- Add a private RPC to access the storage. (!12504)

### Bug fixes

- The transaction pool checks if a transaction can be prepayed before
  inclusion and injection. (!12342)

### Breaking changes

- Delayed Transactions use a dedicated encoding tag in the block in progress. (!12401)
- Record timestamps in executable blueprints. (!12487)

### Internal

- If an error occurs during transaction injection, the trace of errors is
  logged. (!12451)
- Improve resiliency to errors. (!12431)
- Better catchup of possibly missed events. (!12365)
- Improve upgrade detection. (!12459)
- Don't import the delayed inbox when initializing from rollup. (!12506)
- Forbid raw delayed transactions in blueprints sent to the rollup. (!12508)
- Add event for delayed transactions. (!12513)

## Version for 79509a69d01c38eeba38d6cc7a323b4d69c58b94

### Features

- Fetch WASM preimages from a remote endpoint. (!12060)
- When the sequencer evm node diverged from rollup node it fails with
  exit code 100. (!12214)
- Add a new RPC (tez_kernelRootHash) to retrieve the root hash used during the
  last upgrade (!12352)

### Bug fixes

### Breaking changes

### Internal

## Version for 624a144032d6dc6431697c39eb81790bccaacff9

### Features

- Detect when a sequencer upgrade evm event is seen in the rollup node
  kernel. The sequencer upgrade is applied to the sequencer local
  storage. (!12046)
- Version the sequencer store and supports migrations. (!12165)
- Revert message are now propagated in the `data` field of the error. (!11906)

### Bug fixes

### Breaking changes

### Internal

## Version for kernel 20ab639f09a8c7c76f982383c3d9e1f831f38088

### Features

- The sequencer node supports the latest format of the delayed inbox including
  a timeout in number of blocks. (!11811)

### Bug fixes

- Fix `address` parameter to `eth_getLogs` RPC service. (!11990)

### Breaking Changes

- The sequencer and observer nodes now store their locally applied blueprints
  in a sqlite database (as a consquence, the node now depends on `libsqlite3`).
  (!11948)


## Version for kernel c5969505b81b52a779270b69f48b6a66c84da429

### Features

- The sequencer node produces blueprints mentioning the expected context hash
  on which it should be applied. (!11644)
- The sequencer node features a “catch-up mechanism” where it tries sending
  its blueprints again when the difference between its local head and the one
  of the rollup node it is connected to becomes to large. (!11808)
- The sequencer node supports the new features of the delayed inbox, namely the
  timeout mechanism. (!11667)
- The observer node now applies the executable blueprints streamed by other EVM
  node endpoints locally. (!11803)
- The observer node now exposes the JSON RPC API endpoint and the EVM node
  specific services. (!11741)
- Improve the simulation endpoint (in particular to be more meaningful in case
  of errors). (!11816)
- Support parameterizing the limit on the number of active connections. (!11870)
- Support daily log files. (!11752)

### Breaking changes

- The sequencer node now stores its blueprint in `$data_dir/blueprint/execute`
  and `$data_dir/blueprint/publish` (previously, it was only storing the
  executable blueprint in `$data_dir/blueprint`). (!11878)

## Version for kernel 9978f3a5f8bee0be78686c5c568109d2e6148f13

### Features

- Stream the L2 blocks to give a faster and more consistent inclusion of
  transactions on the L1. (!11102)
- Add a keep alive argument that waits until the connection is made with the
  rollup node. (!11236)
- The chunker can also produce blueprints out of the given bytes. (!11497)

### Bug fixes

- Simulation errors are better propagated. (!11381)

### Breaking changes

- The node no longer show the RPC requests by default, you need to specify the
  `--verbose` flag to log the inputs and outputs of RPC. (!11475)
