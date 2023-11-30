# Proxy Mode

The proxy mode is an alternative mode of `octez-client` where some RPCs
are done locally instead of being delegated to the node. The point
is to discharge the node from potentially long computations, such as
estimating baking rights.

This document is the developers' documentation of the proxy mode.
It outlines the different modules implementing the proxy mode and how
they interact with each other. Individual `mli` files contain
additional documentation for things local to a module. This document
aims to provide a higher-level view.

## Overview

At the high-level, the proxy mode consists of:

* A custom RPC client, capable of executing some RPCs locally; and
  of delegating others to the node.
* A proxy context, that is capable of filling itself by requesting
  data from the node on the fly. This is what makes possible to
  execute RPCs locally: missing data is requested over the network
  under the hood.

## `octez-client` CLI

The proxy mode is requested by passing `--mode proxy` to the client. For
this, `src/lib_client/client_config.ml` contains a `Mode_proxy` value.

This flag is inspected in `src/lib_client/client_main_run.ml` to build
a `Client_context.full` instance whose underlying `RPC_context.generic`
behaves differently.

## Custom `RPC_context.generic` instance

The proxy mode's `RPC_context.generic` instance is defined
in `src/lib_proxy/RPC_client.ml`. This instance is capable both
of executing some RPCs locally (by delegating to the mockup's client
`src/lib_mockup_proxy/RPC_client.ml`) and of delegating RPCs to the
node, like the vanilla client does. RPCs that cannot be executed locally
are detected by matching the error `Local_RPC_error (Rpc_not_found _)`.

## Protocol-dependent functions

The proxy mode is implemented both as a library and with protocol-dependent
code. The latter is declared in the module type `Proxy_sig` in
`src/lib_proxy/registration.mli`. Each protocol then registers its
implementation with a side effect, in a manner similar to how the mockup
mode does.

The main protocol-dependent functions are:

* `Proxy_sig`'s `directory` function: it provides the part of the tree
  of RPCs that is implemented in the protocol. This is the tree of RPCs that
  can be executed locally by the proxy's custom `RPC_context.generic`.
* `Proxy_sig`'s `init_env_rpc_context` function: it provides the
  instance of `Tezos_protocol_environment.rpc_context` to use when
  doing an RPC call. This record's main field is an instance of
  `Tezos_protocol_environment.Context.t`.

The value of `Tezos_protocol_environment.Context.t` built by the instances of
`Proxy_sig` is where the proxy's core logic lies. The values returned
are capable of filling themselves in the `get` method, by performing
the `/chains/main/blocks/head/context/raw/bytes` RPC. By _filling themselves_,
we mean that these values grow on-demand whenever a call to `get` is made.
This is implemented in `src/lib_protocol_environment/proxy_context.ml`.

## Wrapper of the `.../raw/bytes` RPC: `Proxy_getter`

The instances of `Proxy_context` do not call the `.../raw/bytes` RPC directly.
Instead they go through the module type `M` of `src/lib_proxy/proxy_getter.mli`.
This interface is implemented in a protocol-independent manner.
It takes care of the following:

* If a request for a key of the context was made already, it avoids redoing
  the corresponding RPC request. This is crucial for performances because
  some algorithms (such as estimating baking rights) request keys
  that may be missing (without this case being a error). Hence
  `Proxy_getter` caches both data obtained and keys for which data is known
  to be missing.
* When a request for some key `k` has to be made, `Proxy_getter` asks the
  protocol whether it is relevant to retrieve a parent key `k'`. This is
  relevant when it's known that sibling keys are often requested one
  after the other. Requesting the parent of both siblings allows to
  perform a single RPC call instead of two. This heuristic, called
  the _get parent_ heuristic, is protocol-dependent because it
  depends on how the protocol's stores data in its context.

Interface of protocol-dependent code used by `Proxy_getter` is
`Proxy_getter.PROTO_RPC`. It has two functions:

* `do_rpc` which is meant to be implemented by calling
  the protocol's `.../raw/bytes` RPC.
* `split_key` which provides the _get parent_ heuristic.

## Should the proxy be the default mode?

Experiments show that clients using the proxy mode do not significantly
reduce the load of nodes. Detailed data is available on the
[proxy's MR](https://gitlab.com/tezos/tezos/-/merge_requests/1943).
In a nutshell, on the node, the cost of executing many `.../raw/bytes` requests
balances the gain of not executing full-fledged RPCs. Hence the proxy mode would
reduce the nodes' load only if HTTP proxies were serving the `.../raw/bytes`
in front of the "real" node. For the moment, as the proxy mode is a milestone
towards the light mode, we prefer not to make the proxy mode the default mode.
