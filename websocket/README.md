# OCaml Websocket library for Cohttp

This library is vendored from https://github.com/vbmithr/ocaml-websocket because
Tezos uses a vendored version of Cohttp.

## Syncing with upstream

Clone the repo https://github.com/vbmithr/ocaml-websocket at the commit (or
branch, tag, etc.):

```sh
git clone git@github.com:vbmithr/ocaml-websocket.git /tmp/ocaml-websocket
```

Copy the meaningful files

```sh
rsync -am  --exclude='_opam' --include='core/**.ml*' --include='lwt/**.ml*' --include='*/' --exclude='*' /tmp/ocaml-websocket/ $(git rev-parse --show-toplevel)/websocket/
```

Apply patches

```bash
git am -3 $(git rev-parse --show-toplevel)/websocket/patches/*
```
