Data-encoding
=============

A library for encoding and decoding data. It offers a great degree of control over the layout of data. It supports json and binary serialisation/deserialisation.


Use
---

For a type `t`, you can use the library's combinators to build a encoding `t
encoding`. You can then use the various reading/writing functions with this
encoding to serialise and deserialise values of the type `t`.

Example:

```
type t = (string * int) list
let encoding = list (tup2 string int31)
let v = [("foo", 32); ("bar", 0)]
let j = Json.construct encoding v
let w = Json.destruct j
let () = assert (v = w)
```

Build and Install
-----------------

`data-encoding` can be built from source using `dune`:

```
dune build
```

or installed directly from `opam`:

```
opam install data-encoding
```
