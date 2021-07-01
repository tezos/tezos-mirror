# Debugging in protocol code

To allow printing from within protocol code we expose a [patch](devtools/protocol-print/add-hack-module.patch).

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
