# Tezlink specific imports

Summary: this lib is a dependency of `evm_node_lib_dev`. It's used to import
the types necessary for the tezlink node from the protocol, the plugin,
the shell, etc.

The goal is to avoid adding direct dependencies between the `lib_dev` library
and the version of the protocol used by `Tezlink`.

## Components

- `Tezlink_imports`: exposes modules of the protocol to the rest of the library.
Should be the only file where the protocol name appears.
- `Tezos_types`: exposes types that can be used in the rest of the library and
in `lib_dev`. Not necessarily the types used in the service declaration:
unwieldy types of the protocol might be better hidden, eg. the `level` type
inherited from the protocol contains values about the consensus, which are not
useful for `Tezlink`, and we have no way to directly build values. So a new
`Tezos_types.level` is defined, with only the useful fields, and `Tezos_services`
is in charge of converting it to the protocol version of the `level` type.
- `Tezos_services`: imports services, and exposes a way to produce a rpc
directory in which those services are registered. It is used when starting the
rpc server.
- `Tezlink_backend_sig`: interface of the backend, ie. the module used to read
values from the durable storage or the store. Can be used in the registration
of the services.
- `lib_dev.Tezlink_services_impl`: implementation of the backend, done in
`lib_dev`.

## How to import a service

1. Find the declaration used by the `octez-node`. Can be in the protocol, in the
plugin, in the shell, etc. It might be necessary to make the declaration public.
2. Import the service and the necessary types. Take care to simplify the type
declared in `Tezos_types` and convert it in `Tezos_services` if appropriate.
3. Implement the service, potentially by adding functions in the backend.

### (optional) Add a dependency to modules outside the l2 node

See `manifest/product-etherlink.ml`. Note that we can't add dependencies between
`lib_dev.encoding` and the protocol, it breaks the linking of the binary running
the integration tests (`etherlink/tezts/tests/main.exe`).

### Import the service

Might require adding to `Tezlink_imports` if the service is in a part of the
codebase we haven't imported from yet.

We must reuse the service declarations as much as possible, which means being
able to interpret the input types and to produce the output types.

#### (optional) Import the types

Some types refer to part of the Tezos protocol that we do not care about, such
as the consensus. We should hide as much of the non relevant part of the types
as possible. If necessary, we can declare new types in `Tezos_types` (not
just imported ones) and provide conversion to and from types used by the service.

In particular, we want to avoid having values and types we can't use accessible
through `Tezos_types`, eg. anything requiring a `Alpha_context.context` should
be hidden.

Some types and modules can be imported without change (eg. `Base58`). Declaring
a module in `Tezos_types` allows us to hide where we get it from exactly.

Others contain values we can't reuse (eg `Contract`) because they require a
`Alpha_context.context`. In those case we can declare a new module
`Tezos_types.Contract` and only keep public some values.

Some types don't have builder functions that we can use (eg `level`). In those
cases we can cheat by serializing a value with a compatible encoding, then
deserializing using the encoding of the target type.

### Register the service

Means associating a service declaration and its implementation in a directory.

The implementation uses the backend (typically the backend exposes a function
that can be directly used as the implementation). But if the backend already
provides enough to get all the necessary information from the storage/store, then
we don't need to add to it for each service.

#### (optional) Declare necessary backend

Declare in `Tezlink_backend_sig` and implement in
`lib_dev.Tezlink_services_impl`.

It's implemented as a functor applied to a `Durable_storage.READER` (plus a
few helpers).

### Example: current_level

As an example, here is a summary of the necessary steps to declare
`current_level`. This is an approximation provided as example, not the exact
implementation, and probably not exactly up-to-date.

The RPC `current_level` is declared in the plugin, so we need to add a
dependency. See the manifest `product_etherlink.ml`.
```
let evm_node_lib_dev_tezlink =
  let quebec =
    List.find (fun proto -> Protocol.short_hash proto = "PsQuebec") Protocol.all
  in
  let tezlink_protocol_plugin =
    match Protocol.plugin quebec with
    | Some target -> target
    | None -> (* unreachable *) assert false
  in
  octez_evm_node_lib
    "evm_node_lib_dev_tezlink"
    ~path:"etherlink/bin_node/lib_dev/tezlink"
    ~synopsis:"Tezlink dependencies for the EVM node"
    ~deps:
      [
        evm_node_lib_dev_encoding |> open_;
        tezlink_protocol_plugin;
        octez_base |> open_ ~m:"TzPervasives";
        octez_shell_services;
        octez_version;
      ]
```

Once the plugin is accessible we can import the service in `tezos_services.ml`.
```
  let current_level :
      ( [`GET],
        tezlink_rpc_context,
        tezlink_rpc_context,
        level_query,
        unit,
        Protocol_types.Level.t )
      Tezos_rpc.Service.t =
    import_service Protocol_plugin_services.current_level
```

The `level` type contains information that are irrelevant for Tezlink, so we
redefine the type in `Tezos_types`.
```
type level = {level : int32; cycle : int32; cycle_position : int32}
```

We define a conversion function from the new type to the plugin type. Since the
plugin doesn't provide a simple way to build a value, we go through
serialization. This should be done during registration and doesn't need to be
used outside of `Tezos_services`.
```
    let convert : Tezos_types.level -> Protocol_types.Level.t tzresult =
      Tezos_types.convert_using_serialization
        ~name:"level"
        ~dst:Protocol_types.Level.encoding
        ~src:conversion_encoding
```
Here `conversion_encoding` should encode our type `Tezos_types.level` in the
similar way to the protocol encoding: an object with `5` fields.
```
    let conversion_encoding =
      let open Data_encoding in
      conv
        (fun ({level; cycle; cycle_position} : level) ->
          ( Raw_level.of_int32_exn level,
            Int32.pred level,
            Cycle.of_int32_exn cycle,
            cycle_position,
            false ))
        (fun ( level,
               _level_position,
               cycle,
               cycle_position,
               _expected_commitment ) ->
          let level = Raw_level.to_int32 level in
          let cycle = Cycle.to_int32 cycle in
          {level; cycle; cycle_position})
        (obj5
           (req "level" Raw_level.encoding)
           (req "level_position" int32)
           (req "cycle" Cycle.encoding)
           (req "cycle_position" int32)
           (req "expected_commitment" bool))
```

The implementation is declared in the backend `Tezlink_backend_sig`.
```
  val current_level :
    [> `Main] ->
    [> `Head of 'a] ->
    offset:int32 ->
    Tezos_types.level tzresult Lwt.t
```
and implemented in `lib_dev.Tezlink_services_impl`
```
module Make (Backend : Backend) : Tezlink_backend_sig.S = struct
  let current_level chain block ~offset =
     ...
    return
      Tezos_types.
        {
          level = ...;
          cycle = ...;
          cycle_position = ...;
        }
```

When all is in place we can register the service.
```
(** Builds the directory registering services under `/chains/<main>/blocks/<head>/...`. *)
let build_block_dir (module Backend : Tezlink_backend_sig.S) =
  ...
  |> register_with_conversion
       ~service:Imported_services.current_level
       ~impl:(fun {block; chain} query () ->
         Backend.current_level chain block ~offset:query.offset)
       ~convert_output:Protocol_types.Level.convert
  ...
```
