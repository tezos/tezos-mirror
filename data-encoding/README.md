Data-encoding
=============

A library for encoding and decoding data. It offers a great degree of control over the layout of data. It supports json and binary serialisation/deserialisation.


Usage
-----

For a type `t`, you can use the library's combinators to build an encoding `t
encoding`. You can then use the various reading/writing functions with this
encoding to serialise and deserialise values of the type `t`.

Example:

```
open Data_encoding

type t = (string * int) list
let encoding = list (tup2 string int31)
let v = [("foo", 32); ("bar", 0)]
let j = Json.construct encoding v
let w = Json.destruct encoding j
let () = assert (v = w)
```

Tutorial
--------

A full tutorial is available in markdown form in [`src/tutorial.md`](https://gitlab.com/nomadic-labs/data-encoding/-/blob/master/src/tutorial.md).

It is available online on
[ocaml.org's package documentation](https://ocaml.org/p/data-encoding/0.6/doc/tutorial.html)
and on
[data-encoding's own online documentation](https://nomadic-labs.gitlab.io/data-encoding/data-encoding/tutorial.html).

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
