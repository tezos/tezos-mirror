# tezos-store
Summary line: Storage library for storing chain data

## Overview
- `tezos-store` provides an abstraction over the disk storage. It aims
   to handles the on-disk storage of static objects such as blocks,
   operations, block's metadata, protocols and chain data.

## Implementation Details
- `tezos-store` is a virtual library comprising the following packages:
  - `tezos-store` itself only contains a `store.mli` file describing the public interface
    that all implementations of the library must provide.
  - `tezos-store.real` is the real implementation of `tezos-store`, used in production.
    For technical reasons, this library is just a thin layer over `tezos-store-unix`, where the bulk of the implementation resides
  - `tezos-store.mocked` is a mocked, in-memory implementation of `tezos-store`, used in tests and simulations.
  - `tezos-store-shared` contains type definitions and endodings used by all implementations and referred to
    by the public interface `store.mli`
  - `tezos-store-unix` is contains the actual implementation of the store
  - `tezos-store-unix.reconstruction` implements the history reconstruction feature
  - `tezos-store-unix.snapshots` implements facilities for exporting and importing snapshots
- The main module is `Store`. It provides the abstract view of the
storage.
- The main components are:
  - `Cemented_block_store`: persistent block store with linear history
  - `Floating_block_store`: persistent block store with arborescent
    history
  - `Block_store`: persistent and cached generic block store based on
    both cemented and floating blocks stores.
  - `Snapshots`: canonical storage representation for storage
    import/export
- A comprehensive view of the storage implementation is available at
  https://tezos.gitlab.io/shell/storage.html

## API Documentation

- http://tezos.gitlab.io/api/odoc/_html/tezos-storage/index.html
