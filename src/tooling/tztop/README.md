# Tztop

`tztop` is a customised top-level meant to assist Tezos protocol development with a REPL (an interactive Read-Eval-Print-Loop)


## Motivation
Toplevel environment needs `Stdlib`, but Tezos protocols are built with a restricted `Stdlib`.
To work around this, we let the `Persistent_env.Persistent_signature.load` fallback to the default load of `Stdlib` so that top-level can setup its initial environment.


## Usage

### Pre-requisites

If `.lambda-term-inputrc` doesn't exist, an empty one must be created.

```
touch ~/.config/.lambda-term-inputrc
```

### Command syntax

```
dune exec -- tztop DIR [options/flags recognised by uTop]
```

Runs toplevel with libraries defined in DIR and optional flags and args that follow.

### Example Usage

```
dune exec -- tztop src/proto_alpha/
```

If you're in an environment that doesn't have opam env hooks, 


```
opam exec -- dune exec -- tztop src/proto_alpha/
```

`tztop` also accepts flags and options accepted originally by the toplevel.

For instance, for stdin redirection:

```
echo "#show_module Tezos_protocol_alpha.Protocol" | dune exec -- tztop src/proto_alpha -stdin
```

Or, for editor integrations:

```
dune exec -- tztop DIR -emacs
```

## Troubleshooting

If `tztop` segfaults or throws an OCaml backtrace regarding missing rc files, consider creating empty `.utoprc` and `.lambda-term-inputrc` files in the `$XDG_CONFIG_HOME` directory.
