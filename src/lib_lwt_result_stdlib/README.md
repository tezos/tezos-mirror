# Lwtreslib: an Lwt- and Result-friendly addition/replacement for the Stdlib

The OCaml's Stdlib modules are orthogonal: each define their a data-type and a
set of functions operating on this data-type. However, in codebases that make
heavy uses of some data-types, a little more integration is welcome

Lwtreslib is a library that supplement some of the OCaml's Stdlib modules with a
tight integration of Lwt and Result. It focuses on data-structures that can be
traversed (iterated, mapped, folded, what have you) and it focuses on functional
(immutable) data-structures because concurrent traversal is more relevant there.

## Design principles

- No exceptions (options for `Not_found`, results for `invalid_argument`)
- meaningful suffixes
- consistent semantic (fail-early, best-effort)

## Construction

- sigs, structs, lib
- Sequential-only parts (independent from trace implementations) vs parallel
  (concurrent) parts
