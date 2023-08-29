# Debugging in protocol code

To allow printing from within protocol code we expose a [patch](devtools/protocol-print/add-hack-module.patch).

## Why not this patch

Proto-env V3 now has `Logging.log` which provides non Lwt logging functions:

```
Logging.(log Debug "hello %d" 42)
```

To make the logging effective, you have to set `Logging.logging_function`.
`Tezos_validation.Protocol_logging.make_log_message_consumer` provides such
a function:

```
let module Proto = ... in
Proto.set_log_message_consumer
  (Protocol_logging.make_log_message_consumer ())
```

## How to use

Apply the patch with:

```sh
# Apply the patch
git apply devtools/protocol-print/add-hack-module.patch

# Rebuild
make
```

You may now use the `Hack` module in protocol code, as in:

```ocaml
let () =
  Hack.printf
    "List is: [%a]@."
    (Format.pp_print_list Format.pp_print_int)
    [1; 2; 3]
```

## how to port to a different environment

You can use
```
devtool/patchs/apply_env_patch.sh devtools/protocol-print/add-hack-module.patch 3 10 11
```
to port this patch to different protocol environment versions (here 10 and 11).

Please recall that the Hack module, as it is defined here, is useless in recent
environments as explained in the first section of this README.
