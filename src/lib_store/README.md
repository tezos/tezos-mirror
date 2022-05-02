# tezos-store
Summary line: Storage library for storing chain data

## Overview
- `tezos-store` provides an abstraction over the disk storage. It aims
   to handles the on-disk storage of static objects such as blocks,
   operations, block's metadata, protocols and chain data.

## Implementation Details
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
