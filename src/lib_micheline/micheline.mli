(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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

type annot = string list

(** The abstract syntax tree of Micheline expressions. The first
    parameter is used to contain locations, but can also embed custom
    data. The second parameter is the type of primitive names. *)
type ('l, 'p) node =
  | Int of 'l * Z.t
  | String of 'l * string
  | Bytes of 'l * Bytes.t
  | Prim of 'l * 'p * ('l, 'p) node list * annot
  | Seq of 'l * ('l, 'p) node list

(** Extract the location of the node. *)
val location : ('l, 'p) node -> 'l

(** Extract the annotations of the node. *)
val annotations : ('l, 'p) node -> string list

(** Expression form using canonical integer numbering as
    locations. The root has number zero, and each node adds one in the
    order of infix traversal. To be used when locations are not
    important, or when one wants to attach properties to nodes in an
    expression without rewriting it (using an indirection table with
    canonical locations as keys). *)
type 'p canonical

(** Canonical integer locations that appear inside {!canonical} expressions. *)
type canonical_location = int

(** A location that won't exist in any well-formed canonical value *)
val dummy_location : canonical_location

(** Compute the canonical form of an expression.
    Drops the concrete locations completely. *)
val strip_locations : (_, 'p) node -> 'p canonical

(** Give the root node of an expression in canonical form. *)
val root : 'p canonical -> (canonical_location, 'p) node

(** Compute the canonical form of an expression.
    Saves the concrete locations in an association list. *)
val extract_locations :
  ('l, 'p) node -> 'p canonical * (canonical_location * 'l) list

(** Transforms an expression in canonical form into a polymorphic one.
    Takes a mapping function to inject the concrete locations. *)
val inject_locations :
  (canonical_location -> 'l) -> 'p canonical -> ('l, 'p) node

(** Copies the tree, updating its primitives. *)
val map : ('a -> 'b) -> 'a canonical -> 'b canonical

(** Copies the tree, updating its primitives and locations. *)
val map_node :
  ('la -> 'lb) -> ('pa -> 'pb) -> ('la, 'pa) node -> ('lb, 'pb) node
