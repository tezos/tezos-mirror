(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*         Gabriel Scherer, projet Parsifal, INRIA Saclay                 *)
(*                                                                        *)
(*   Copyright 2019 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Either type.

    Either is the simplest and most generic sum/variant type:
    a value of [('a, 'b) Either.t] is either a [Left (v : 'a)]
    or a [Right (v : 'b)].

    It is a natural choice in the API of generic functions where values
    could fall in two different cases, possibly at different types,
    without assigning a specific meaning to what each case should be.

    For example:

{[List.partition_map:
    ('a -> ('b, 'c) Either.t) -> 'a list -> 'b list * 'c list]}

    If you are looking for a parametrized type where
    one alternative means success and the other means failure,
    you should use the more specific type {!Result.t}.

    @since 4.12
*)

(* Unlike [result], no [either] type is made available in Stdlib,
   one needs to access [Either.t] explicitly:

   - This type is less common in typical OCaml codebases,
     which prefer domain-specific variant types whose constructors
     carry more meaning.
   - Adding this to Stdlib would raise warnings in existing codebases
     that already use a constructor named Left or Right:
     + when opening a module that exports such a name,
       warning 45 is raised
     + adding a second constructor of the same name in scope kicks
       in the disambiguation mechanisms, and warning 41 may now
       be raised by existing code.

   If the use becomes more common in the future we can always
   revisit this choice.
*)

type ('a, 'b) t = Left of 'a | Right of 'b (**)
(** A value of [('a, 'b) Either.t] contains
    either a value of ['a]  or a value of ['b] *)

(* some values omitted *)

val equal :
  left:('a -> 'a -> bool) -> right:('b -> 'b -> bool) ->
  ('a, 'b) t -> ('a, 'b) t -> bool
(** [equal ~left ~right e0 e1] tests equality of [e0] and [e1] using [left]
    and [right] to respectively compare values wrapped by [Left _] and
    [Right _]. *)

val compare :
  left:('a -> 'a -> int) -> right:('b -> 'b -> int) ->
  ('a, 'b) t -> ('a, 'b) t -> int
(** [compare ~left ~right e0 e1] totally orders [e0] and [e1] using [left] and
    [right] to respectively compare values wrapped by [Left _ ] and [Right _].
    [Left _] values are smaller than [Right _] values. *)
