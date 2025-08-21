# Creating input data for EVM kernel

Some simple Node.js script to create data to test / benchmark evm kernels.
Usage info provided in each script.

- `make_addr.js` creating new addresses, randomly or from private key.
- `sign_tx.js` signing transactions with a private key.
- `lib/` internals of previous scripts.
- `benchmark/` scripts for generating benchmark scenarios.
- `transactions_example/` examples of transactions as JSON, and the different
ways data can be formated. More or less, use "0x..." hex strings,
or small numbers when applicable.