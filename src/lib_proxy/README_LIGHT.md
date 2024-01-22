# Light Mode

The light mode is an alternative mode of `octez-client`. It builds
upon the proxy mode. The point of the light mode is
to provide additional security to the proxy mode, by obtaining
data from multiple (hopefully unrelated) endpoints and making sure
all endpoints agree on the same data for a given chain and block.
Readers of this document are recommended to read the corresponding
document for proxy mode (the `README.md` in this directory) if they
need details regarding the proxy mode.

This document is the developers' documentation of the light mode.
It outlines the different modules implementing the light mode and how
they interact with each other. Individual `mli` files contain
additional documentation for things local to a module. This document
aims to provide a higher-level view.

## Overview

At the high-level, the light mode is a variant of the proxy mode
whose retrieval of distant data is more complex. While the proxy mode
requests data from a single endpoint and accepts it without any verification,
the light mode does the following:

* It requires at least 2 endpoints to work.
* It retrieves data from one endpoint, out of the `n` endpoints available.
* Before accepting data from this endpoint, it requests
  `n-1` [merkle_trees](https://en.wikipedia.org/wiki/Merkle_tree) to the other
  endpoints; and check that the hashes in these trees match the
  hashes obtained by hashing data obtained in the previous step.

## `octez-client` CLI

The light mode is requested by passing `--mode light` to the client. For
this, `src/lib_client/client_config.ml` contains a `Mode_light` value.

This flag is inspected in `src/lib_client/client_main_run.ml` to
pass a value of type `Tezos_proxy.Proxy_services.mode` to
`Client_context.unix_proxy`. Ultimately, this value is
used in `lib_proxy/proxy_services.ml` to create different
instances of `Proxy_getter.M`, according to whether being in proxy mode
or in light mode.

## `Proxy_getter.M`

This module type is implemented differently in proxy mode
and in light mode. `lib_proxy/proxy_getter.ml` provides an implementation
which is a functor taking two modules as parameters: `Proxy.CORE`
and `Proxy_proto.PROTO_RPC`.

We focus here on `Proxy.CORE` as it's
the module that is implemented differently by the two modes. Implementations
of this module should provide two functions: `do_rpc` and `get` which
both take a key as parameter. It is guaranteed by callers of this
module that, for any given key, `do_rpc key` will be called before
`get key`. Implementation of `do_rpc key` is expected to retrieve the
data for `key` from distant endpoints and store it with a side effect;
while `get key` is expected to return the data for a key
(data which must be available because `do_rpc`
for this key was performed before).

## `Proxy.CORE`

The light mode's implementation of `Proxy.CORE` is located
in `lib_proxy/light_core.ml`. This implementation performs
the three steps described in the [overview](#overview) above. For that,
it delegates to other files:

* `lib_proxy/light_internal.ml` is a library file, providing
  function to integrate distant data and distant merkle trees
  in the local data tree. The two most important functions are:

  * `union_irmin_tree_opt_merkle_tree` which integrates distant data
    (a merkle tree with data) into the local data tree (implemented
    with an [Irmin](https://github.com/mirage/irmin) in-memory tree).
  * `contains_merkle_tree` which checks that the local data tree
    agrees with a merkle tree consisting only of hashes.
* `lib_proxy/light_consensus.ml` contains the algorithm checking
  that enough endpoints provide merkle trees that agree
  with the local data tree.

## `octez-node` new RPC: `merkle_tree`

For the light mode,
`octez-node` is augmented with a new RPC whose syntax is
`/chains/<chain_id>/blocks/<block_id>/context/merkle_tree/<path_in_context>?holey=[yes|no]`.
For example, to retrieve the merkle tree of `contracts/global_counter`,
one should do:

```
octez-client rpc get /chains/main/blocks/head/context/merkle_tree/contracts/global_counter
```

Here's the RPC's output:

```
[ [ "active_delegates_with_rolls",
    { "H": "CoVe9oDs8t8WgH9JHB3DqbvxCZw1Q5ky7qBsZfMiLeKe6RiSMHn1" } ],
  [ "big_maps",
    { "H": "CoVbip7pyXZDp1umo3cGUbCWJUA8wDkPbWR56wKqS434DiDSwGWC" } ],
  [ "block_priority",
    { "H": "CoVuTbwGSJyu9xD7vYYxxcqCFwCPf55UBqu8iqRcHYrs3Gu31v8y" } ],
  [ "contracts", { "C": [ [ "global_counter", { "D": "00" } ] ] } ],
  [ "cycle",
    { "H": "CoVngnGTJfudgcayQqtz2ZyWTUFB6zHmhvV1itjncRzYS4wndhH8" } ],
  [ "delegates",
    { "H": "CoVe9oDs8t8WgH9JHB3DqbvxCZw1Q5ky7qBsZfMiLeKe6RiSMHn1" } ],
  [ "delegates_with_frozen_balance",
    { "H": "CoVXSYbKxP7jJL4ZSZnCsyynEZ6aeR7HR59UVKCDZdMGa8QCLFfW" } ],
  [ "genesis_key",
    { "H": "CoVSQwz1mSz28kNCBso3F3ZHuLj5GXwXovu4byqweTa96bJAzTX6" } ],
  [ "rolls",
    { "H": "CoVEow8t8iz6gfxB7daEHjFRD5suzdhb3wNZ5rMnoUTdjbYvxbez" } ],
  [ "sandbox_parameter",
    { "H": "CoVZDcjRgjmKnAhetUtb1AVQYwuUi3fBK7js11vjBETPuG5FcU8o" } ],
  [ "sapling",
    { "H": "CoVbip7pyXZDp1umo3cGUbCWJUA8wDkPbWR56wKqS434DiDSwGWC" } ],
  [ "v1", { "H": "CoWRLgT2SwZkCWCwyTBxxkxPxYFvTWtHfKqX8MFQ1hNWL4SS1qdU" } ],
  [ "version",
    { "H": "CoWVK1YzoDnMGrNioKL9Mze6s4XX8Uw9Vp9hPHYXqHfaFpwnmXmA" } ],
  [ "votes",
    { "H": "CoVnWzSVjbYHCQLD53JGJfWRSjUBrkbtCrNMgmsXX6bMhy7CE7E6" } ] ]
```

In this snippet, siblings to nodes on the required path are filled
with hashes; while the required path ends up with a data node. Hashes
are identified by `"H"` keys while data is identified by the `"D"` key.
The `"C"` key is used to recurse within the tree, towards the final data.
In this example, the first segment of the path to the data is `"contracts"`
(hence tagged `"C"`) while the second (and final) segment of the path to the
data is `"global_counter"` (hence tagged `"D"`).

Retrieving data from endpoints, as mentioned above multiple times,
is implemented by doing calls to this RPC. The `holey` flag in the RPC
specifies whether the merkle tree should contain only hashes (`holey=yes`)
or should contain hashes AND the data at the specified key (`holey=no`).
