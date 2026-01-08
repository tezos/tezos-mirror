# tezos-context

<!-- Summary line: One sentence about this component. -->
Context library for storing ledger and operations on disk and in memory

## API Documentation

<https://tezos.gitlab.io/api/odoc/_html/octez-libs/Tezos_context/index.html>

## Overview

There are two containers that are used to store different things. The reunion of the two is called the [storage](http://tezos.gitlab.io/shell/storage.html). It can be described rapidly like this:

- storage
  - `store`
    - contains the blocks
  - `tezos-context` (brassaia)
    - ledger
    - contains the state of every account and their updates

`tezos-context` provides an abstraction over the ledger storage (be it on disk and in memory).

## Implementation details

Brassaia stores the context in memory and on disk. Both are created by calling the functor `Lib_context.Disk.Context.Make`. This functor takes an `Brassaia.Schema.Extended` as its parameter. The schema is composed of:
- `Hash`: used to hash anything
- `Branch`: used to represent a branch. *In Tezos branches are simple strings*.
- `Info`: used to represent the information about the commit (author, message, date etc.)
- `Contents`: used to represent the data stored (a file or blob). *In Tezos, these are bytes*.
- `Path`: a string list to represent a path in a `tree` of directory and files. *A string list in Tezos*.
- `Step`: the minimal component of a `Path`. *A string in Tezos*.
- `Metadata`: Not used in Tezos.
- `Commit`: Functor over `Node_key` and `Commit_key` to (*taken from `Brassaia.Libbrassaia.Commitintf`*) generalise the concept of **commit** to one that supports object keys that are not strictly equal to hashes. *Since Tezos applies it to nodes and commits which type is already know, this functor may be useless*.
- `Nodes`: used to represent a node in the `tree` (a directory). This is a functor over `keys` and `contents`. *It's still unclear what this does exactly*.

This will provide us with the following module architecture:
- An in-memory `tree` (the `Tree` module)
- An on-disk `store` (the main level functions)

### Common interface

Both the `store` and the `tree` are called **contexts** and share a common interface `Lib_context.Sigs.Context.VIEW`:
- `t`, `key`, `value`, `tree` are the types used by this view
- `mem`/`mem_tree` returns a boolean corresponding to the fact that a `key` is associated to a `value`/`tree` in a context
- `find`/`find_tree` will return a `value`/`tree` option associated to a `key` in a context
- `add`/`add_tree` will return a context where a `key` has been associated (or its association replaced) to a `value`/`tree`
- `remove` will remove a `key` from a context

### Tree

*It should be noted that `VIEW.t` and `VIEW.tree` are equal in `Tree`.*

The `Tree` defines functions that are useful for an in-memory context:

- `empty`, `is_empty` and `equal` are self explanatory ones and don't need to be explained
- `kind t` returns the fact that `t` is a node or a leaf
- `to_value t` returns `Some v` if `t` is a leaf and `v` is its value and `None` otherwise
- `of_value _ v` is a new leaf `tree v` *It could be called `singleton`*
- `hash t` is the Merkle hash of `t`
- `clear ?depth t` will clear all caches in `t` for subtrees of depth equal to `depth` or higher.

#### Remarks

Some functions are associated to `raw trees` that can be either a `Value of bytes` or a `Tree of raw String.Map.t`. This indicates that it is known in the functor that keys are `string` and values are `bytes` yet this is still hidden from the rest of the module. The functions associated to `raw trees` have not been analysed yet.

### Block-indexed `store`

The main type representing the `store` is `index`. It can be created by:
- the `index` function that takes a `context` (which is both the `tree` and the `index`, this function just extracts the `index` from the `context`)
- an `init` function. This function sets up some parameters like the indexing strategy, the LRU cache size, the root (as in: where it will be stored on disk) etc.

#### Common VIEW functions

Most of the functions implemented for VIEW will work on the `tree` stored in the `context` and will not touch the `store`.

#### Access to the disk

The modifications to the `tree` will be stored in the disk (the `store`) when the `commit` function is called. It will return the hash of the commit.

Other functions use the disk and need to be documented (some of them already are in `Lib_context.Sigs.Context`:
- `sync` synchronises the `context` with the `index`
- `split`: *not sure about what it does currently*
- `export_snapshot` exports a single commit as a standalone store (as I understand it, it is supposed to replay the commit history to have a file that represents the ledger and not the transaction history).
- `set_head`, `set_master`: self-explanatory git-like functions

### Important types

### Tezos configuration`

```ocaml
module Conf = struct
  let entries = 32
  let stable_hash = 256
  let contents_length_header = Some `Varint
  let inode_child_order = `Seeded_hash
  let forbid_empty_dir_persistence = true
end
```

### Context

```ocaml
type index = {
  path : string;
  repo : Store.Repo.t;
  patch_context : (context -> context tzresult Lwt.t) option;
  readonly : bool;
}

and context = {
  index : index;
  parents : Store.Commit.t list;
  tree : Store.tree;
  (* number of `remove`, `add_tree` and `add` calls, not yet flushed *)
  ops : int;
}
type t = context
```

### Summary

- The `Tezos_context` module (dispatched between `Lib_context.Sigs.Context`, `Lib_context.Memory.Context` and `Lib_context.Disk.Context` is the intermediary between Tezos and Brassaia
- Brassaia handles both a `Tree` (in-memory partial representation of the context) and an `index` (on-disk representation of the context that can be limited to the most recent commits)
- Most functions used in `Tezos_context` communicate with the `Tree` and are later reflected to the disk by using the `commit` function

As we saw, the main names are `Context` = `Tree` + `Index` yet Brassaia uses `Store` as an alias for `Context`. Whenever `Store` is seen, it needs to be understood as `Context`.

### Issues: Creating a Tezos context

The way a Tezos context (store) is created is convoluted:
- `Make` expects and `Encoding` of type `Tezos_context_encoding.Context`
- A `Store` is created:
  - by creating `Maker` which is the application of `Brassaia_pack_unix.Maker` to `Encoding.Conf`
  - `Maker.Make` is then applied to `Encoding.Schema`
  - `Schema` is aliased as `Tezos_context_encoding.Context.Schema`
  - `Store.Backend` is almost the same as the `Schema` where all functors have been applied to the relevant modules and some modules have been added.

Creating a `Maker` functor applied to a `Conf` and then applying it to a `Schema` when both are already known beforehand could (should?) be simplified.

**The main thing to understand from this is that a `Context` is created when given:
   - a `Conf` representing the parameters of the in-memory `tree`
   - a `Schema` listing all the internal representations (commits, nodes, contents etc.)**
