(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

module type SNodes = sig
  type t = private int

  val zero : t

  val one : t [@@ocaml.warning "-32"]

  val succ : t -> t

  val add : t -> t -> t

  val to_int : t -> int
end

(** The [Nodes] module is used to count the number of computation steps
    performed when evaluating the size of the in-memory graph corresponding
    to an OCaml value.

    In first approximation, the value of type [Nodes.t] threaded through
    {!expr_size} below and through the module {!Script_typed_ir_size}
    is meant to match the number of recursive calls in the [traverse]
    functions of {!Script_typed_ir} and in that of {!node_size}.

    The assumption is that there's a bounded amount of work performed between
    two such recursive calls, hence that the total work is bounded above
    by something proportional to the [Nodes.t] accumulator.

    Computations on values of type [Nodes.t] do not overflow, as they
    are bounded above by the number of nodes traversed when computing
    an OCaml value.
 *)
module Nodes : SNodes = struct
  type t = int

  let zero = 0

  let one = 1

  let succ x = x + 1

  let add x y = x + y

  let to_int x = x
end

(** {2 Helpers to deal with computing the in-memory size of values} *)

type sint = Saturation_repr.may_saturate Saturation_repr.t

type nodes_and_size = Nodes.t * sint

let ( !! ) = Saturation_repr.safe_int

let ( +! ) = Saturation_repr.add

let ( +? ) s x = Saturation_repr.add s !!x

let ( *? ) s x = Saturation_repr.mul s !!x

let ( ++ ) (n1, s1) (n2, s2) = (Nodes.add n1 n2, s1 +! s2)

let zero = (Nodes.zero, !!0)

let word_size = !!8

let header_size = word_size

let int32_size = header_size +! word_size

let int64_size = header_size +! (word_size *? 2)

let h1w = header_size +! word_size

let h2w = header_size +! (word_size *? 2)

let h3w = header_size +! (word_size *? 3)

let h4w = header_size +! (word_size *? 4)

let h5w = header_size +! (word_size *? 5)

let hh3w = (word_size *? 3) +! (header_size *? 2)

let hh6w = (word_size *? 6) +! (header_size *? 2)

let hh8w = (word_size *? 8) +! (header_size *? 2)

let z_size z =
  let numbits = Z.numbits z in
  (*
      Z does not seem to have a canonical representation of numbers.
      Hence, even though we observed that 24 works in many cases we
      sometimes meet numbers with a larger size, hence we use 32 instead
      of 24 in the following formula.
  *)
  if Compare.Int.(numbits <= 62) then !!0 else (word_size *? Z.size z) +? 32

let string_size_gen len = header_size +? (len + (8 - (len mod 8)))

let bytes_size b = string_size_gen (Bytes.length b)

let string_size s = string_size_gen (String.length s)

let blake2b_hash_size = h1w +! string_size_gen 20

let public_key_hash_in_memory_size = h1w +! blake2b_hash_size

let ret_adding (nodes, size) added = (nodes, size +! added)

let ret_succ_adding (nodes, size) added = (Nodes.succ nodes, size +! added)

let ret_succ (nodes, size) = (Nodes.succ nodes, size)

let option_size some x =
  let some x = h1w +! some x in
  Option.fold ~none:!!0 ~some x

let option_size_vec some x =
  let some x = ret_adding (some x) h1w in
  Option.fold ~none:zero ~some x

let list_cell_size elt_size = header_size +! word_size +! word_size +! elt_size
[@@ocaml.inline always]

let list_fold_size elt_size list =
  List.fold_left
    (fun accu elt -> ret_succ_adding (accu ++ elt_size elt) h2w)
    zero
    list

let boxed_tup2 x y = header_size +! word_size +! word_size +! x +! y
[@@ocaml.inline always]

let node_size =
  let open Micheline in
  (* An OCaml list item occupies 3 words of memory: one for the (::)
     constructor, one for the item itself (head) and one for the
     remainder of the list (tail). *)
  let list_size sns = word_size *? (List.length sns * 3) in
  let annotation_size a =
    List.fold_left
      (fun accu s -> ret_succ_adding accu (h2w +! string_size s))
      zero
      a
  in
  let internal_node_size = function
    | Int (_, z) -> (Nodes.one, h2w +! z_size z)
    | String (_, s) -> (Nodes.one, h2w +! string_size s)
    | Bytes (_, s) -> (Nodes.one, h2w +! bytes_size s)
    | Prim (_, _, args, a) ->
        ret_succ_adding (annotation_size a) (list_size args +! h4w)
    | Seq (_, terms) -> (Nodes.one, list_size terms +! h2w)
  in
  fun node ->
    Script_repr.fold node zero @@ fun accu node ->
    accu ++ internal_node_size node

let expr_size expr = node_size (Micheline.root expr)
