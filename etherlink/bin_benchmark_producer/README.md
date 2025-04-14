# Etherlink Benchmark Producer

This directory defines a TEZT scenario used to generate EVM blocks containing contract interactions. These blocks are used for benchmarking, but **replay logic is handled separately** (see [`etherlink_benchmark.md`](../docs/src/etherlink_benchmark.md)).

## Purpose

The goal of this scenario is to:

- Deploy a wide variety of EVM-compatible contracts
- Create EVM blocks filled with contract method calls
- Provide a reproducible and controlled environment to measure tick and gas consumption

Each block contains either a contract deployment or a batch of transactions for a single contract method.

## How It Works

The scenario follows this process:

1. **Read Contracts**: Contracts and their associated calls are defined in `primitives.ml`, grouped by subset.
2. **Register Contracts**: `main.ml` registers each subset using `Producer.register`.
3. **Deploy Contracts**: For each contract, one block is produced that contains its deployment transaction.
4. **Fill Blocks with Calls**:
   - For each `(method, args)` pair defined for the contract, a block is produced containing N identical calls.
   - The number of transactions per call (`tx_per_call`) is defined in `main.ml` for each registered subset.

The scenario ensures one block per method call, and all transactions in that block use the same method and arguments.

## Block Usage Summary

- One deployment block per contract
- One call block per method per contract
- Produced blocks are designed for later replay (e.g. via `split_replay.sh`)

## Contract Subsets

Defined in `primitives.ml`, the current subsets include:

- **`general_contracts`**: Typical ERC contracts (ERC20, ERC1155), contract loops, storage operations, and signature verification utilities.
- **`problematic_opcodes`**: Contracts containing EVM opcodes known to be gas-intensive or historically problematic (e.g. SHA3, CREATE2, SSTORE).
- **`gas_sinks`**: Contracts designed to consume large amounts of gas (e.g. deep loops, memory blow-up).
- **`precompiled`**: Contracts that invoke EVM precompiles such as ECRecover, SHA256, RIPEMD160, MODEXP, and BLAKE2f.

## Adding New Contracts or Subsets

To extend the benchmark scenario:

1. **Add a Solidity Contract**:
   - Place the `.sol` file in the appropriate subdirectory under `etherlink/bin_benchmark_producer/<subset>`.

2. **Edit `primitives.ml`**:
   - Use `Solidity_contracts.compile_contract` to load your contract.
   - Define a list of method signatures and argument lists.
   - Add the contract and calls to the appropriate subset list (`registered_general_contracts`, etc.).

3. **Optional**: Create a new subset by defining a new list in `primitives.ml` and registering it in `main.ml`.

## Running the Scenario

The scenario is executed automatically as part of the benchmarking pipeline explained in [`etherlink_benchmark.md`](../docs/src/etherlink_benchmark.md).

You can also run the scenario manually with:
```
dune exec – etherlink/bin_benchmark_producer/main.exe -t
```

## Notes

- The actual replay of blocks is done externally using `split_replay.sh`.
- The graphing of tick and gas usage is done using `create_graph.sh`.
- This module only **produces blocks**, it does not analyze or replay them.
