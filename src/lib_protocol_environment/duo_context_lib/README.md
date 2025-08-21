# Duo_context
<!-- Summary line: One sentence about this component. -->

Duo_context is a context with two internal contexts on which all operations will
be duplicated.

## Overview
<!--
- Describe the purpose of this component.
- Describe the interaction of the code in this directory with the other
  components. This includes dependencies on other components, for instance.
-->

This context is used in order to compare two backends. Typically, if you
introduce a new `Brassaia` backend, you can use a `Duo_context(Irmin, Brassia)`
context in order to check that `Brassaia` acts as `Irmin` and behaves correctly.

This is used for testing purposes.

## Implementation Details

<!--
- Describe the file structure and the location of the main components.
- Other relevant implementation details (e.g., global invariants,
  implementation design rationale, etc.).
- Testing specifics, build-system specifics, etc. as needed.
-->

### Wrapping inner values in variants

`verify_tree_proof` type is given in `TEZOS_CONTEXT` module type:

```ocaml
(** The type for context trees. *)
type tree

type _ verifier := _ -> (tree -> (tree * _) Lwt.t) -> (tree * _, _) result Lwt.t

val verify_tree_proof : (_, _) verifier
```

Type `tree` is abstract, and is dependent on the backend used.

Let's say that we want to have our duo context tree type defined as `{tree_1 :
Backend1.tree; tree_2 : Backend2.tree}` (which was the case with the initial
implementation).

The problem here with `verify_tree_proof proof fn` is that we have to apply `fn`,
that is meant to take a `Duo_context.tree` as argument, and use it with
`Backend1.verify_tree_proof` (which expects a function that takes a `Backend1.tree`)
and `Backend2.verify_tree_proof` (which expects a function that takes a
`Backend2.tree`).

In order to handle this, you want to wrap `fn`, take a `Backend1.tree` as a
parameter, and create a `Duo_context.tree` from it.

This is how it was done in the previous implementation:
```ocaml
  let f_irmin irmin_tree =
      let open Lwt_syntax in
      let+ tree, res = f {tree_1 = tree; tree_2 = Obj.magic tree} in
      (tree.tree_2, res)
    in
    let* irmin_tree, res1 = Irmin_Context.verify_tree_proof proof f_irmin in
```

Yes, you noticed the `Obj.magic`, that will only work if `Backend1.tree ==
Backend2.tree`, which is not what we want.

Now, using a variant in order to wrap the `Duo_context.tree` type, you will have

```ocaml
type wrapped_tree =
  | Backend1_tree of Backend1.tree
  | Backend2_tree of Backend2.tree

type tree = {tree_1 : wrapped_tree; tree_2 : wrapped_tree}
```

The current implementation becomes:

```ocaml
let wrap verifier wrapper extract =
  let f t =
    let tree = wrapper t in
    let open Lwt_syntax in
    let+ tree, res = f {tree_1 = tree; tree_2 = tree} in
    let tree = extract tree in
    (tree, res)
  in
  Lwt.map (Result.map (fun (t, a) -> (wrapper t, a))) (verifier proof f)
in
let verify_tree_proof = function
| Backend1 ->
  verify_tree_proof
    Backend1.verify_tree_proof
    (fun t -> Backend1_tree t)
    (function
      | {tree_1 = Backend1_tree tree; _} -> tree
      | _ -> assert false)
| Backend2 -> ...
```

Which should be allright, at least as long as your tree does not have mutable
fields.
