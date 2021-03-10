# Lwtreslib: an Lwt- and Result-friendly addition/replacement for the Stdlib

The OCaml's Stdlib modules are orthogonal: each define their own datatype and a
set of functions operating on this datatype. `Result` for `result`, `Option`
for `option`, `List` for `list`, etc. This orthogonality provides a high
expressive power for a low lines-of-code count. E.g.,

```
let fold f init xs =
   List.fold_left
      (fun acc x -> Result.bind acc (fun acc -> f acc x))
      (Result.ok init)
      xs
```

However, in code-bases that make heavy uses of some datatypes, a little more
integration is welcome. For example, in code bases that use the `result` type
pervasively, the `fold` function above should be available in a module of
list-traversing functions.

Lwtreslib is a library that supplement some of the OCaml's Stdlib modules with a
tight integration of Lwt and Result. It focuses on data-structures that can be
traversed (iterated, mapped, folded, what have you).


## Design principles

1.  Exception-safety

    The functions exported by Lwtreslib do not raise exceptions. These functions
    may return `option` or `result` to indicate that some error happened during
    traversal, and they may propagate `result`.

    (For convenience, the module `WithExceptions` provides a few
    exception-raising functions because they are convenient in specific
    contexts.)

2.  Consistency

    Exported functions and values have consistent names that reflect their
    consistent semantic.


## Reading guide

The sources of Lwtreslib are organised as follow:

- `bare/` contains the sources for a bare-bones implementation of Lwtreslib that
  provides monadic combinators and collection traversals.

    - `bare/sigs/` contains the sources for the signatures of all the modules
      exported by `bare/`
    - `bare/functor_outputs` contains the sources for the signatures of all the
      modules constructed by functors exported by `bare/`
    - `bare/structs` contains the sources for of all the modules exported by
      `bare/`

- `examples/traces/` contains multiple example implementation of traces. A trace
  is a data-structure that holds multiple errors organised in a way that
  reflects the way the errors happened. Specifically, errors can be stringed
  together to represent that the control flow traversed multiple points, or they
  can be held side-by-side to indicate that they happen in to simultaneously
  evaluating promises.

  The code in this directory is meant more as examples than fully-fledged
  traces, but they can also be used for prototyping or as a basis for a more
  complete trace implementation.

- `traced/` contains the sources for a trace-enabled implementation of Lwtreslib
  that provides monadic combinators and collection traversals. This
  implementation provides all the functionality of `bare/` with added support
  for traces (i.e., structured collections of errors).

    - `traced/sigs/` contains the sources for the signatures of all the modules
      exported by `traced/`
    - `traced/functor_outputs` contains the sources for the signatures of all
      the modules constructed by functors exported by `bare/`
    - `traced/structs` contains the sources for of all the modules exported by
      `bare/`. These modules are functorised over the implementation of a trace.
      The file `traced/structs/structs.ml` contains an all-in-one functor for
      instantiating all of the modules.

- `test/` contains code to test the library.
