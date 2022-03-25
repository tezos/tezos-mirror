(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.dev>                     *)
(* Copyright (c) 2021-2022 Nomadic Labs <contact@nomadic-labs.com>           *)
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

open Alpha_context
open Script_int
open Dependent_bool

type step_constants = {
  source : Contract.t;
  payer : Contract.t;
  self : Contract.t;
  amount : Tez.t;
  balance : Tez.t;
  chain_id : Chain_id.t;
  now : Script_timestamp.t;
  level : Script_int.n Script_int.num;
}

(* Preliminary definitions. *)

type never = |

type address = {destination : Destination.t; entrypoint : Entrypoint.t}

module Script_signature : sig
  (** [t] is made algebraic in order to distinguish it from the other type
      parameters of [Script_typed_ir.ty]. *)
  type t = Signature_tag of signature [@@ocaml.unboxed]

  val make : signature -> t

  val get : t -> signature

  val encoding : t Data_encoding.t

  val of_b58check_opt : string -> t option

  val check :
    ?watermark:Signature.watermark ->
    Signature.Public_key.t ->
    t ->
    Bytes.t ->
    bool

  val compare : t -> t -> int

  val size : int
end

type signature = Script_signature.t

type tx_rollup_l2_address = Tx_rollup_l2_address.Indexable.value

type ('a, 'b) pair = 'a * 'b

type ('a, 'b) union = L of 'a | R of 'b

module Script_chain_id : sig
  (** [t] is made algebraic in order to distinguish it from the other type
      parameters of [Script_typed_ir.ty]. *)
  type t = Chain_id_tag of Chain_id.t [@@ocaml.unboxed]

  val make : Chain_id.t -> t

  val compare : t -> t -> int

  val size : int

  val encoding : t Data_encoding.t

  val to_b58check : t -> string

  val of_b58check_opt : string -> t option
end

module Script_bls : sig
  module type S = sig
    type t

    type fr

    val add : t -> t -> t

    val mul : t -> fr -> t

    val negate : t -> t

    val of_bytes_opt : Bytes.t -> t option

    val to_bytes : t -> Bytes.t
  end

  module Fr : sig
    (** [t] is made algebraic in order to distinguish it from the other type
        parameters of [Script_typed_ir.ty]. *)
    type t = Fr_tag of Bls12_381.Fr.t [@@ocaml.unboxed]

    include S with type t := t and type fr := t

    val of_z : Z.t -> t

    val to_z : t -> Z.t
  end

  module G1 : sig
    (** [t] is made algebraic in order to distinguish it from the other type
        parameters of [Script_typed_ir.ty]. *)
    type t = G1_tag of Bls12_381.G1.t [@@ocaml.unboxed]

    include S with type t := t and type fr := Fr.t
  end

  module G2 : sig
    (** [t] is made algebraic in order to distinguish it from the other type
        parameters of [Script_typed_ir.ty]. *)
    type t = G2_tag of Bls12_381.G2.t [@@ocaml.unboxed]

    include S with type t := t and type fr := Fr.t
  end

  val pairing_check : (G1.t * G2.t) list -> bool
end

module Script_timelock : sig
  (** [chest_key] is made algebraic in order to distinguish it from the other
      type parameters of [Script_typed_ir.ty]. *)
  type chest_key = Chest_key_tag of Timelock.chest_key [@@ocaml.unboxed]

  val make_chest_key : Timelock.chest_key -> chest_key

  val chest_key_encoding : chest_key Data_encoding.t

  (** [chest] is made algebraic in order to distinguish it from the other type
      parameters of [Script_typed_ir.ty]. *)
  type chest = Chest_tag of Timelock.chest [@@ocaml.unboxed]

  val make_chest : Timelock.chest -> chest

  val chest_encoding : chest Data_encoding.t

  val open_chest : chest -> chest_key -> time:int -> Timelock.opening_result

  val get_plaintext_size : chest -> int
end

type 'a ticket = {ticketer : Contract.t; contents : 'a; amount : n num}

type empty_cell = EmptyCell

type end_of_stack = empty_cell * empty_cell

module Type_size : sig
  type 'a t

  val check_eq :
    error_details:'error_trace Script_tc_errors.error_details ->
    'a t ->
    'b t ->
    (unit, 'error_trace) result

  val to_int : 'a t -> Saturation_repr.mul_safe Saturation_repr.t
end

type 'a ty_metadata = {size : 'a Type_size.t} [@@unboxed]

module type Boxed_set_OPS = sig
  type t

  type elt

  val elt_size : elt -> int (* Gas_input_size.t *)

  val empty : t

  val add : elt -> t -> t

  val mem : elt -> t -> bool

  val remove : elt -> t -> t

  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
end

module type Boxed_set = sig
  type elt

  module OPS : Boxed_set_OPS with type elt = elt

  val boxed : OPS.t

  val size : int
end

(** [set] is made algebraic in order to distinguish it from the other type
    parameters of [ty]. *)
type 'elt set = Set_tag of (module Boxed_set with type elt = 'elt)
[@@ocaml.unboxed]

module type Boxed_map_OPS = sig
  type 'a t

  type key

  val key_size : key -> int (* Gas_input_size.t *)

  val empty : 'value t

  val add : key -> 'value -> 'value t -> 'value t

  val remove : key -> 'value t -> 'value t

  val find : key -> 'value t -> 'value option

  val fold : (key -> 'value -> 'a -> 'a) -> 'value t -> 'a -> 'a

  val fold_es :
    (key -> 'value -> 'a -> 'a tzresult Lwt.t) ->
    'value t ->
    'a ->
    'a tzresult Lwt.t
end

module type Boxed_map = sig
  type key

  type value

  module OPS : Boxed_map_OPS with type key = key

  val boxed : value OPS.t

  val size : int
end

(** [map] is made algebraic in order to distinguish it from the other type
    parameters of [ty]. *)
type ('key, 'value) map =
  | Map_tag of (module Boxed_map with type key = 'key and type value = 'value)
[@@ocaml.unboxed]

module Big_map_overlay : Map.S with type key = Script_expr_hash.t

type ('key, 'value) big_map_overlay = {
  map : ('key * 'value option) Big_map_overlay.t;
  size : int;
}

type 'elt boxed_list = {elements : 'elt list; length : int}

type view = {
  input_ty : Script.node;
  output_ty : Script.node;
  view_code : Script.node;
}

type view_map = (Script_string.t, view) map

(** ['arg entrypoints] represents the tree of entrypoints of a parameter type
    ['arg].
    [name] is the name of the entrypoint at that node if it is not [None].
    [nested] are the entrypoints below the node in the tree.
      It is always [Entrypoints_None] for non-union nodes.
      But it is also ok to have [Entrypoints_None] for a union node, it just
      means that there are no entrypoints below that node in the tree.
*)
type 'arg entrypoints = {
  name : Entrypoint.t option;
  nested : 'arg nested_entrypoints;
}

and 'arg nested_entrypoints =
  | Entrypoints_Union : {
      left : 'l entrypoints;
      right : 'r entrypoints;
    }
      -> ('l, 'r) union nested_entrypoints
  | Entrypoints_None : _ nested_entrypoints

(** [no_entrypoints] is [{name = None; nested = Entrypoints_None}] *)
val no_entrypoints : _ entrypoints

type ('arg, 'storage) script =
  | Script : {
      code :
        (('arg, 'storage) pair, (operation boxed_list, 'storage) pair) lambda;
      arg_type : ('arg, _) ty;
      storage : 'storage;
      storage_type : ('storage, _) ty;
      views : view_map;
      entrypoints : 'arg entrypoints;
      code_size : Cache_memory_helpers.sint;
    }
      -> ('arg, 'storage) script

(* ---- Instructions --------------------------------------------------------*)

(*

   The instructions of Michelson are represented in the following
   Generalized Algebraic Datatypes.

   There are three important aspects in that type declaration.

   First, we follow a tagless approach for values: they are directly
   represented as OCaml values. This reduces the computational cost of
   interpretation because there is no need to check the shape of a
   value before applying an operation to it. To achieve that, the GADT
   encodes the typing rules of the Michelson programming
   language. This static information is sufficient for the typechecker
   to justify the absence of runtime checks.  As a bonus, it also
   ensures that well-typed Michelson programs cannot go wrong: if the
   interpreter typechecks then we have the static guarantee that no
   stack underflow or type error can occur at runtime.

   Second, we maintain the invariant that the stack type always has a
   distinguished topmost element. This invariant is important to
   implement the stack as an accumulator followed by a linked list of
   cells, a so-called A-Stack. This representation is considered in
   the literature[1] as an efficient representation of the stack for a
   stack-based abstract machine, mainly because this opens the
   opportunity for the accumulator to be stored in a hardware
   register. In the GADT, this invariant is encoded by representing
   the stack type using two parameters instead of one: the first one
   is the type of the accumulator while the second is the type of the
   rest of the stack.

   Third, in this representation, each instruction embeds its
   potential successor instructions in the control flow. This design
   choice permits an efficient implementation of the continuation
   stack in the interpreter. Assigning a precise type to this kind of
   instruction which is a cell in a linked list of instructions is
   similar to the typing of delimited continuations: we need to give a
   type to the stack ['before] the execution of the instruction, a
   type to the stack ['after] the execution of the instruction and
   before the execution of the next, and a type for the [`result]ing
   stack type after the execution of the whole chain of instructions.

   Combining these three aspects, the type [kinstr] needs four
   parameters:

   ('before_top, 'before, 'result_top, 'result) kinstr

   Notice that we could have chosen to only give two parameters to
   [kinstr] by manually enforcing each argument to be a pair but this
   is error-prone: with four parameters, this constraint is enforced
   by the arity of the type constructor itself.

   Hence, an instruction which has a successor instruction enjoys a
   type of the form:

   ... * ('after_top, 'after, 'result_top, 'result) kinstr * ... ->
   ('before_top, 'before, 'result_top, 'result) kinstr

   where ['before_top] and ['before] are the types of the stack top
   and rest before the instruction chain, ['after_top] and ['after]
   are the types of the stack top and rest after the instruction
   chain, and ['result_top] and ['result] are the types of the stack
   top and rest after the instruction chain. The [IHalt] instruction
   ends a sequence of instructions and has no successor, as shown by
   its type:

   IHalt : ('a, 's) kinfo -> ('a, 's, 'a, 's) kinstr

   Each instruction is decorated by some metadata (typically to hold
   locations). The type for these metadata is [kinfo]: such a value is
   only used for logging and error reporting and has no impact on the
   operational semantics.

   Notations:
   ----------

   In the following declaration, we use 'a, 'b, 'c, 'd, ...  to assign
   types to stack cell contents while we use 's, 't, 'u, 'v, ... to
   assign types to stacks.

   The types for the final result and stack rest of a whole sequence
   of instructions are written 'r and 'f (standing for "result" and
   "final stack rest", respectively).

   Instructions for internal execution steps
   =========================================

   Some instructions encoded in the following type are not present in the
   source language. They only appear during evaluation to account for
   intermediate execution steps. Indeed, since the interpreter follows
   a small-step style, it is sometimes necessary to decompose a
   source-level instruction (e.g. List_map) into several instructions
   with smaller steps. This technique seems required to get an
   efficient tail-recursive interpreter.

   References
   ==========
   [1]: http://www.complang.tuwien.ac.at/projects/interpreters.html

 *)
and ('before_top, 'before, 'result_top, 'result) kinstr =
  (*
     Stack
     -----
  *)
  | IDrop :
      ('a, 'b * 's) kinfo * ('b, 's, 'r, 'f) kinstr
      -> ('a, 'b * 's, 'r, 'f) kinstr
  | IDup :
      ('a, 's) kinfo * ('a, 'a * 's, 'r, 'f) kinstr
      -> ('a, 's, 'r, 'f) kinstr
  | ISwap :
      ('a, 'b * 's) kinfo * ('b, 'a * 's, 'r, 'f) kinstr
      -> ('a, 'b * 's, 'r, 'f) kinstr
  | IConst :
      ('a, 's) kinfo * 'ty * ('ty, 'a * 's, 'r, 'f) kinstr
      -> ('a, 's, 'r, 'f) kinstr
  (*
     Pairs
     -----
  *)
  | ICons_pair :
      ('a, 'b * 's) kinfo * ('a * 'b, 's, 'r, 'f) kinstr
      -> ('a, 'b * 's, 'r, 'f) kinstr
  | ICar :
      ('a * 'b, 's) kinfo * ('a, 's, 'r, 'f) kinstr
      -> ('a * 'b, 's, 'r, 'f) kinstr
  | ICdr :
      ('a * 'b, 's) kinfo * ('b, 's, 'r, 'f) kinstr
      -> ('a * 'b, 's, 'r, 'f) kinstr
  | IUnpair :
      ('a * 'b, 's) kinfo * ('a, 'b * 's, 'r, 'f) kinstr
      -> ('a * 'b, 's, 'r, 'f) kinstr
  (*
     Options
     -------
   *)
  | ICons_some :
      ('v, 's) kinfo * ('v option, 's, 'r, 'f) kinstr
      -> ('v, 's, 'r, 'f) kinstr
  | ICons_none :
      ('a, 's) kinfo * ('b option, 'a * 's, 'r, 'f) kinstr
      -> ('a, 's, 'r, 'f) kinstr
  | IIf_none : {
      kinfo : ('a option, 'b * 's) kinfo;
      branch_if_none : ('b, 's, 'c, 't) kinstr;
      branch_if_some : ('a, 'b * 's, 'c, 't) kinstr;
      k : ('c, 't, 'r, 'f) kinstr;
    }
      -> ('a option, 'b * 's, 'r, 'f) kinstr
  | IOpt_map : {
      kinfo : ('a option, 's) kinfo;
      body : ('a, 's, 'b, 's) kinstr;
      k : ('b option, 's, 'c, 't) kinstr;
    }
      -> ('a option, 's, 'c, 't) kinstr
  (*
     Unions
     ------
   *)
  | ICons_left :
      ('a, 's) kinfo * (('a, 'b) union, 's, 'r, 'f) kinstr
      -> ('a, 's, 'r, 'f) kinstr
  | ICons_right :
      ('b, 's) kinfo * (('a, 'b) union, 's, 'r, 'f) kinstr
      -> ('b, 's, 'r, 'f) kinstr
  | IIf_left : {
      kinfo : (('a, 'b) union, 's) kinfo;
      branch_if_left : ('a, 's, 'c, 't) kinstr;
      branch_if_right : ('b, 's, 'c, 't) kinstr;
      k : ('c, 't, 'r, 'f) kinstr;
    }
      -> (('a, 'b) union, 's, 'r, 'f) kinstr
  (*
     Lists
     -----
  *)
  | ICons_list :
      ('a, 'a boxed_list * 's) kinfo * ('a boxed_list, 's, 'r, 'f) kinstr
      -> ('a, 'a boxed_list * 's, 'r, 'f) kinstr
  | INil :
      ('a, 's) kinfo * ('b boxed_list, 'a * 's, 'r, 'f) kinstr
      -> ('a, 's, 'r, 'f) kinstr
  | IIf_cons : {
      kinfo : ('a boxed_list, 'b * 's) kinfo;
      branch_if_cons : ('a, 'a boxed_list * ('b * 's), 'c, 't) kinstr;
      branch_if_nil : ('b, 's, 'c, 't) kinstr;
      k : ('c, 't, 'r, 'f) kinstr;
    }
      -> ('a boxed_list, 'b * 's, 'r, 'f) kinstr
  | IList_map :
      ('a boxed_list, 'c * 's) kinfo
      * ('a, 'c * 's, 'b, 'c * 's) kinstr
      * ('b boxed_list, 'c * 's, 'r, 'f) kinstr
      -> ('a boxed_list, 'c * 's, 'r, 'f) kinstr
  | IList_iter :
      ('a boxed_list, 'b * 's) kinfo
      * ('a, 'b * 's, 'b, 's) kinstr
      * ('b, 's, 'r, 'f) kinstr
      -> ('a boxed_list, 'b * 's, 'r, 'f) kinstr
  | IList_size :
      ('a boxed_list, 's) kinfo * (n num, 's, 'r, 'f) kinstr
      -> ('a boxed_list, 's, 'r, 'f) kinstr
  (*
    Sets
    ----
  *)
  | IEmpty_set :
      ('a, 's) kinfo * 'b comparable_ty * ('b set, 'a * 's, 'r, 'f) kinstr
      -> ('a, 's, 'r, 'f) kinstr
  | ISet_iter :
      ('a set, 'b * 's) kinfo
      * ('a, 'b * 's, 'b, 's) kinstr
      * ('b, 's, 'r, 'f) kinstr
      -> ('a set, 'b * 's, 'r, 'f) kinstr
  | ISet_mem :
      ('a, 'a set * 's) kinfo * (bool, 's, 'r, 'f) kinstr
      -> ('a, 'a set * 's, 'r, 'f) kinstr
  | ISet_update :
      ('a, bool * ('a set * 's)) kinfo * ('a set, 's, 'r, 'f) kinstr
      -> ('a, bool * ('a set * 's), 'r, 'f) kinstr
  | ISet_size :
      ('a set, 's) kinfo * (n num, 's, 'r, 'f) kinstr
      -> ('a set, 's, 'r, 'f) kinstr
  (*
     Maps
     ----
   *)
  | IEmpty_map :
      ('a, 's) kinfo * 'b comparable_ty * (('b, 'c) map, 'a * 's, 'r, 'f) kinstr
      -> ('a, 's, 'r, 'f) kinstr
  | IMap_map :
      (('a, 'b) map, 'd * 's) kinfo
      * ('a * 'b, 'd * 's, 'c, 'd * 's) kinstr
      * (('a, 'c) map, 'd * 's, 'r, 'f) kinstr
      -> (('a, 'b) map, 'd * 's, 'r, 'f) kinstr
  | IMap_iter :
      (('a, 'b) map, 'c * 's) kinfo
      * ('a * 'b, 'c * 's, 'c, 's) kinstr
      * ('c, 's, 'r, 'f) kinstr
      -> (('a, 'b) map, 'c * 's, 'r, 'f) kinstr
  | IMap_mem :
      ('a, ('a, 'b) map * 's) kinfo * (bool, 's, 'r, 'f) kinstr
      -> ('a, ('a, 'b) map * 's, 'r, 'f) kinstr
  | IMap_get :
      ('a, ('a, 'b) map * 's) kinfo * ('b option, 's, 'r, 'f) kinstr
      -> ('a, ('a, 'b) map * 's, 'r, 'f) kinstr
  | IMap_update :
      ('a, 'b option * (('a, 'b) map * 's)) kinfo
      * (('a, 'b) map, 's, 'r, 'f) kinstr
      -> ('a, 'b option * (('a, 'b) map * 's), 'r, 'f) kinstr
  | IMap_get_and_update :
      ('a, 'b option * (('a, 'b) map * 's)) kinfo
      * ('b option, ('a, 'b) map * 's, 'r, 'f) kinstr
      -> ('a, 'b option * (('a, 'b) map * 's), 'r, 'f) kinstr
  | IMap_size :
      (('a, 'b) map, 's) kinfo * (n num, 's, 'r, 'f) kinstr
      -> (('a, 'b) map, 's, 'r, 'f) kinstr
  (*
     Big maps
     --------
  *)
  | IEmpty_big_map :
      ('a, 's) kinfo
      * 'b comparable_ty
      * ('c, _) ty
      * (('b, 'c) big_map, 'a * 's, 'r, 'f) kinstr
      -> ('a, 's, 'r, 'f) kinstr
  | IBig_map_mem :
      ('a, ('a, 'b) big_map * 's) kinfo * (bool, 's, 'r, 'f) kinstr
      -> ('a, ('a, 'b) big_map * 's, 'r, 'f) kinstr
  | IBig_map_get :
      ('a, ('a, 'b) big_map * 's) kinfo * ('b option, 's, 'r, 'f) kinstr
      -> ('a, ('a, 'b) big_map * 's, 'r, 'f) kinstr
  | IBig_map_update :
      ('a, 'b option * (('a, 'b) big_map * 's)) kinfo
      * (('a, 'b) big_map, 's, 'r, 'f) kinstr
      -> ('a, 'b option * (('a, 'b) big_map * 's), 'r, 'f) kinstr
  | IBig_map_get_and_update :
      ('a, 'b option * (('a, 'b) big_map * 's)) kinfo
      * ('b option, ('a, 'b) big_map * 's, 'r, 'f) kinstr
      -> ('a, 'b option * (('a, 'b) big_map * 's), 'r, 'f) kinstr
  (*
     Strings
     -------
  *)
  | IConcat_string :
      (Script_string.t boxed_list, 's) kinfo
      * (Script_string.t, 's, 'r, 'f) kinstr
      -> (Script_string.t boxed_list, 's, 'r, 'f) kinstr
  | IConcat_string_pair :
      (Script_string.t, Script_string.t * 's) kinfo
      * (Script_string.t, 's, 'r, 'f) kinstr
      -> (Script_string.t, Script_string.t * 's, 'r, 'f) kinstr
  | ISlice_string :
      (n num, n num * (Script_string.t * 's)) kinfo
      * (Script_string.t option, 's, 'r, 'f) kinstr
      -> (n num, n num * (Script_string.t * 's), 'r, 'f) kinstr
  | IString_size :
      (Script_string.t, 's) kinfo * (n num, 's, 'r, 'f) kinstr
      -> (Script_string.t, 's, 'r, 'f) kinstr
  (*
     Bytes
     -----
  *)
  | IConcat_bytes :
      (bytes boxed_list, 's) kinfo * (bytes, 's, 'r, 'f) kinstr
      -> (bytes boxed_list, 's, 'r, 'f) kinstr
  | IConcat_bytes_pair :
      (bytes, bytes * 's) kinfo * (bytes, 's, 'r, 'f) kinstr
      -> (bytes, bytes * 's, 'r, 'f) kinstr
  | ISlice_bytes :
      (n num, n num * (bytes * 's)) kinfo * (bytes option, 's, 'r, 'f) kinstr
      -> (n num, n num * (bytes * 's), 'r, 'f) kinstr
  | IBytes_size :
      (bytes, 's) kinfo * (n num, 's, 'r, 'f) kinstr
      -> (bytes, 's, 'r, 'f) kinstr
  (*
     Timestamps
     ----------
   *)
  | IAdd_seconds_to_timestamp :
      (z num, Script_timestamp.t * 's) kinfo
      * (Script_timestamp.t, 's, 'r, 'f) kinstr
      -> (z num, Script_timestamp.t * 's, 'r, 'f) kinstr
  | IAdd_timestamp_to_seconds :
      (Script_timestamp.t, z num * 's) kinfo
      * (Script_timestamp.t, 's, 'r, 'f) kinstr
      -> (Script_timestamp.t, z num * 's, 'r, 'f) kinstr
  | ISub_timestamp_seconds :
      (Script_timestamp.t, z num * 's) kinfo
      * (Script_timestamp.t, 's, 'r, 'f) kinstr
      -> (Script_timestamp.t, z num * 's, 'r, 'f) kinstr
  | IDiff_timestamps :
      (Script_timestamp.t, Script_timestamp.t * 's) kinfo
      * (z num, 's, 'r, 'f) kinstr
      -> (Script_timestamp.t, Script_timestamp.t * 's, 'r, 'f) kinstr
  (*
     Tez
     ---
    *)
  | IAdd_tez :
      (Tez.t, Tez.t * 's) kinfo * (Tez.t, 's, 'r, 'f) kinstr
      -> (Tez.t, Tez.t * 's, 'r, 'f) kinstr
  | ISub_tez :
      (Tez.t, Tez.t * 's) kinfo * (Tez.t option, 's, 'r, 'f) kinstr
      -> (Tez.t, Tez.t * 's, 'r, 'f) kinstr
  | ISub_tez_legacy :
      (Tez.t, Tez.t * 's) kinfo * (Tez.t, 's, 'r, 'f) kinstr
      -> (Tez.t, Tez.t * 's, 'r, 'f) kinstr
  | IMul_teznat :
      (Tez.t, n num * 's) kinfo * (Tez.t, 's, 'r, 'f) kinstr
      -> (Tez.t, n num * 's, 'r, 'f) kinstr
  | IMul_nattez :
      (n num, Tez.t * 's) kinfo * (Tez.t, 's, 'r, 'f) kinstr
      -> (n num, Tez.t * 's, 'r, 'f) kinstr
  | IEdiv_teznat :
      (Tez.t, n num * 's) kinfo
      * ((Tez.t, Tez.t) pair option, 's, 'r, 'f) kinstr
      -> (Tez.t, n num * 's, 'r, 'f) kinstr
  | IEdiv_tez :
      (Tez.t, Tez.t * 's) kinfo
      * ((n num, Tez.t) pair option, 's, 'r, 'f) kinstr
      -> (Tez.t, Tez.t * 's, 'r, 'f) kinstr
  (*
     Booleans
     --------
   *)
  | IOr :
      (bool, bool * 's) kinfo * (bool, 's, 'r, 'f) kinstr
      -> (bool, bool * 's, 'r, 'f) kinstr
  | IAnd :
      (bool, bool * 's) kinfo * (bool, 's, 'r, 'f) kinstr
      -> (bool, bool * 's, 'r, 'f) kinstr
  | IXor :
      (bool, bool * 's) kinfo * (bool, 's, 'r, 'f) kinstr
      -> (bool, bool * 's, 'r, 'f) kinstr
  | INot :
      (bool, 's) kinfo * (bool, 's, 'r, 'f) kinstr
      -> (bool, 's, 'r, 'f) kinstr
  (*
     Integers
     --------
  *)
  | IIs_nat :
      (z num, 's) kinfo * (n num option, 's, 'r, 'f) kinstr
      -> (z num, 's, 'r, 'f) kinstr
  | INeg :
      ('a num, 's) kinfo * (z num, 's, 'r, 'f) kinstr
      -> ('a num, 's, 'r, 'f) kinstr
  | IAbs_int :
      (z num, 's) kinfo * (n num, 's, 'r, 'f) kinstr
      -> (z num, 's, 'r, 'f) kinstr
  | IInt_nat :
      (n num, 's) kinfo * (z num, 's, 'r, 'f) kinstr
      -> (n num, 's, 'r, 'f) kinstr
  | IAdd_int :
      ('a num, 'b num * 's) kinfo * (z num, 's, 'r, 'f) kinstr
      -> ('a num, 'b num * 's, 'r, 'f) kinstr
  | IAdd_nat :
      (n num, n num * 's) kinfo * (n num, 's, 'r, 'f) kinstr
      -> (n num, n num * 's, 'r, 'f) kinstr
  | ISub_int :
      ('a num, 'b num * 's) kinfo * (z num, 's, 'r, 'f) kinstr
      -> ('a num, 'b num * 's, 'r, 'f) kinstr
  | IMul_int :
      ('a num, 'b num * 's) kinfo * (z num, 's, 'r, 'f) kinstr
      -> ('a num, 'b num * 's, 'r, 'f) kinstr
  | IMul_nat :
      (n num, 'a num * 's) kinfo * ('a num, 's, 'r, 'f) kinstr
      -> (n num, 'a num * 's, 'r, 'f) kinstr
  | IEdiv_int :
      ('a num, 'b num * 's) kinfo
      * ((z num, n num) pair option, 's, 'r, 'f) kinstr
      -> ('a num, 'b num * 's, 'r, 'f) kinstr
  | IEdiv_nat :
      (n num, 'a num * 's) kinfo
      * (('a num, n num) pair option, 's, 'r, 'f) kinstr
      -> (n num, 'a num * 's, 'r, 'f) kinstr
  | ILsl_nat :
      (n num, n num * 's) kinfo * (n num, 's, 'r, 'f) kinstr
      -> (n num, n num * 's, 'r, 'f) kinstr
  | ILsr_nat :
      (n num, n num * 's) kinfo * (n num, 's, 'r, 'f) kinstr
      -> (n num, n num * 's, 'r, 'f) kinstr
  | IOr_nat :
      (n num, n num * 's) kinfo * (n num, 's, 'r, 'f) kinstr
      -> (n num, n num * 's, 'r, 'f) kinstr
  | IAnd_nat :
      (n num, n num * 's) kinfo * (n num, 's, 'r, 'f) kinstr
      -> (n num, n num * 's, 'r, 'f) kinstr
  | IAnd_int_nat :
      (z num, n num * 's) kinfo * (n num, 's, 'r, 'f) kinstr
      -> (z num, n num * 's, 'r, 'f) kinstr
  | IXor_nat :
      (n num, n num * 's) kinfo * (n num, 's, 'r, 'f) kinstr
      -> (n num, n num * 's, 'r, 'f) kinstr
  | INot_int :
      ('a num, 's) kinfo * (z num, 's, 'r, 'f) kinstr
      -> ('a num, 's, 'r, 'f) kinstr
  (*
     Control
     -------
  *)
  | IIf : {
      kinfo : (bool, 'a * 's) kinfo;
      branch_if_true : ('a, 's, 'b, 'u) kinstr;
      branch_if_false : ('a, 's, 'b, 'u) kinstr;
      k : ('b, 'u, 'r, 'f) kinstr;
    }
      -> (bool, 'a * 's, 'r, 'f) kinstr
  | ILoop :
      (bool, 'a * 's) kinfo
      * ('a, 's, bool, 'a * 's) kinstr
      * ('a, 's, 'r, 'f) kinstr
      -> (bool, 'a * 's, 'r, 'f) kinstr
  | ILoop_left :
      (('a, 'b) union, 's) kinfo
      * ('a, 's, ('a, 'b) union, 's) kinstr
      * ('b, 's, 'r, 'f) kinstr
      -> (('a, 'b) union, 's, 'r, 'f) kinstr
  | IDip :
      ('a, 'b * 's) kinfo
      * ('b, 's, 'c, 't) kinstr
      * ('a, 'c * 't, 'r, 'f) kinstr
      -> ('a, 'b * 's, 'r, 'f) kinstr
  | IExec :
      ('a, ('a, 'b) lambda * 's) kinfo * ('b, 's, 'r, 'f) kinstr
      -> ('a, ('a, 'b) lambda * 's, 'r, 'f) kinstr
  | IApply :
      ('a, ('a * 'b, 'c) lambda * 's) kinfo
      * ('a, _) ty
      * (('b, 'c) lambda, 's, 'r, 'f) kinstr
      -> ('a, ('a * 'b, 'c) lambda * 's, 'r, 'f) kinstr
  | ILambda :
      ('a, 's) kinfo
      * ('b, 'c) lambda
      * (('b, 'c) lambda, 'a * 's, 'r, 'f) kinstr
      -> ('a, 's, 'r, 'f) kinstr
  | IFailwith :
      ('a, 's) kinfo * Script.location * ('a, _) ty
      -> ('a, 's, 'r, 'f) kinstr
  (*
     Comparison
     ----------
  *)
  | ICompare :
      ('a, 'a * 's) kinfo * 'a comparable_ty * (z num, 's, 'r, 'f) kinstr
      -> ('a, 'a * 's, 'r, 'f) kinstr
  (*
     Comparators
     -----------
  *)
  | IEq :
      (z num, 's) kinfo * (bool, 's, 'r, 'f) kinstr
      -> (z num, 's, 'r, 'f) kinstr
  | INeq :
      (z num, 's) kinfo * (bool, 's, 'r, 'f) kinstr
      -> (z num, 's, 'r, 'f) kinstr
  | ILt :
      (z num, 's) kinfo * (bool, 's, 'r, 'f) kinstr
      -> (z num, 's, 'r, 'f) kinstr
  | IGt :
      (z num, 's) kinfo * (bool, 's, 'r, 'f) kinstr
      -> (z num, 's, 'r, 'f) kinstr
  | ILe :
      (z num, 's) kinfo * (bool, 's, 'r, 'f) kinstr
      -> (z num, 's, 'r, 'f) kinstr
  | IGe :
      (z num, 's) kinfo * (bool, 's, 'r, 'f) kinstr
      -> (z num, 's, 'r, 'f) kinstr
  (*
     Protocol
     --------
  *)
  | IAddress :
      ('a typed_contract, 's) kinfo * (address, 's, 'r, 'f) kinstr
      -> ('a typed_contract, 's, 'r, 'f) kinstr
  | IContract :
      (address, 's) kinfo
      * ('a, _) ty
      * Entrypoint.t
      * ('a typed_contract option, 's, 'r, 'f) kinstr
      -> (address, 's, 'r, 'f) kinstr
  | IView :
      ('a, address * 's) kinfo
      * ('a, 'b) view_signature
      * ('b option, 's, 'r, 'f) kinstr
      -> ('a, address * 's, 'r, 'f) kinstr
  | ITransfer_tokens :
      ('a, Tez.t * ('a typed_contract * 's)) kinfo
      * (operation, 's, 'r, 'f) kinstr
      -> ('a, Tez.t * ('a typed_contract * 's), 'r, 'f) kinstr
  | IImplicit_account :
      (public_key_hash, 's) kinfo * (unit typed_contract, 's, 'r, 'f) kinstr
      -> (public_key_hash, 's, 'r, 'f) kinstr
  | ICreate_contract : {
      kinfo : (public_key_hash option, Tez.t * ('a * 's)) kinfo;
      storage_type : ('a, _) ty;
      arg_type : ('b, _) ty;
      lambda : ('b * 'a, operation boxed_list * 'a) lambda;
      views : view_map;
      entrypoints : 'b entrypoints;
      k : (operation, address * 's, 'r, 'f) kinstr;
    }
      -> (public_key_hash option, Tez.t * ('a * 's), 'r, 'f) kinstr
  | ISet_delegate :
      (public_key_hash option, 's) kinfo * (operation, 's, 'r, 'f) kinstr
      -> (public_key_hash option, 's, 'r, 'f) kinstr
  | INow :
      ('a, 's) kinfo * (Script_timestamp.t, 'a * 's, 'r, 'f) kinstr
      -> ('a, 's, 'r, 'f) kinstr
  | IMin_block_time :
      ('a, 's) kinfo * (n num, 'a * 's, 'r, 'f) kinstr
      -> ('a, 's, 'r, 'f) kinstr
  | IBalance :
      ('a, 's) kinfo * (Tez.t, 'a * 's, 'r, 'f) kinstr
      -> ('a, 's, 'r, 'f) kinstr
  | ILevel :
      ('a, 's) kinfo * (n num, 'a * 's, 'r, 'f) kinstr
      -> ('a, 's, 'r, 'f) kinstr
  | ICheck_signature :
      (public_key, signature * (bytes * 's)) kinfo * (bool, 's, 'r, 'f) kinstr
      -> (public_key, signature * (bytes * 's), 'r, 'f) kinstr
  | IHash_key :
      (public_key, 's) kinfo * (public_key_hash, 's, 'r, 'f) kinstr
      -> (public_key, 's, 'r, 'f) kinstr
  | IPack :
      ('a, 's) kinfo * ('a, _) ty * (bytes, 's, 'r, 'f) kinstr
      -> ('a, 's, 'r, 'f) kinstr
  | IUnpack :
      (bytes, 's) kinfo * ('a, _) ty * ('a option, 's, 'r, 'f) kinstr
      -> (bytes, 's, 'r, 'f) kinstr
  | IBlake2b :
      (bytes, 's) kinfo * (bytes, 's, 'r, 'f) kinstr
      -> (bytes, 's, 'r, 'f) kinstr
  | ISha256 :
      (bytes, 's) kinfo * (bytes, 's, 'r, 'f) kinstr
      -> (bytes, 's, 'r, 'f) kinstr
  | ISha512 :
      (bytes, 's) kinfo * (bytes, 's, 'r, 'f) kinstr
      -> (bytes, 's, 'r, 'f) kinstr
  | ISource :
      ('a, 's) kinfo * (address, 'a * 's, 'r, 'f) kinstr
      -> ('a, 's, 'r, 'f) kinstr
  | ISender :
      ('a, 's) kinfo * (address, 'a * 's, 'r, 'f) kinstr
      -> ('a, 's, 'r, 'f) kinstr
  | ISelf :
      ('a, 's) kinfo
      * ('b, _) ty
      * Entrypoint.t
      * ('b typed_contract, 'a * 's, 'r, 'f) kinstr
      -> ('a, 's, 'r, 'f) kinstr
  | ISelf_address :
      ('a, 's) kinfo * (address, 'a * 's, 'r, 'f) kinstr
      -> ('a, 's, 'r, 'f) kinstr
  | IAmount :
      ('a, 's) kinfo * (Tez.t, 'a * 's, 'r, 'f) kinstr
      -> ('a, 's, 'r, 'f) kinstr
  | ISapling_empty_state :
      ('a, 's) kinfo
      * Sapling.Memo_size.t
      * (Sapling.state, 'a * 's, 'b, 'f) kinstr
      -> ('a, 's, 'b, 'f) kinstr
  | ISapling_verify_update :
      (Sapling.transaction, Sapling.state * 's) kinfo
      * ((bytes, (z num, Sapling.state) pair) pair option, 's, 'r, 'f) kinstr
      -> (Sapling.transaction, Sapling.state * 's, 'r, 'f) kinstr
  | ISapling_verify_update_deprecated :
      (* legacy introduced in J *)
      (Sapling.Legacy.transaction, Sapling.state * 's) kinfo
      * ((z num, Sapling.state) pair option, 's, 'r, 'f) kinstr
      -> (Sapling.Legacy.transaction, Sapling.state * 's, 'r, 'f) kinstr
  | IDig :
      ('a, 's) kinfo
      (*
         There is a prefix of length [n] common to the input stack
         of type ['a * 's] and an intermediary stack of type ['d * 'u].
      *)
      * int
        (*
         Under this common prefix, the input stack has type ['b * 'c * 't] and
         the intermediary stack type ['c * 't] because we removed the ['b] from
         the input stack. This value of type ['b] is pushed on top of the
         stack passed to the continuation.
      *)
      * ('b, 'c * 't, 'c, 't, 'a, 's, 'd, 'u) stack_prefix_preservation_witness
      * ('b, 'd * 'u, 'r, 'f) kinstr
      -> ('a, 's, 'r, 'f) kinstr
  | IDug :
      ('a, 'b * 's) kinfo
      (*
         The input stack has type ['a * 'b * 's].

         There is a prefix of length [n] common to its substack
         of type ['b * 's] and the output stack of type ['d * 'u].
      *)
      * int
        (*
         Under this common prefix, the first stack has type ['c * 't]
         and the second has type ['a * 'c * 't] because we have pushed
         the topmost element of this input stack under the common prefix.
      *)
      * ('c, 't, 'a, 'c * 't, 'b, 's, 'd, 'u) stack_prefix_preservation_witness
      * ('d, 'u, 'r, 'f) kinstr
      -> ('a, 'b * 's, 'r, 'f) kinstr
  | IDipn :
      ('a, 's) kinfo
      (*
         The body of Dipn is applied under a prefix of size [n]...
      *)
      * int
        (*
         ... the relation between the types of the input and output stacks
         is characterized by the following witness.
         (See forthcoming comments about [stack_prefix_preservation_witness].)
      *)
      * ('c, 't, 'd, 'v, 'a, 's, 'b, 'u) stack_prefix_preservation_witness
      * ('c, 't, 'd, 'v) kinstr
      * ('b, 'u, 'r, 'f) kinstr
      -> ('a, 's, 'r, 'f) kinstr
  | IDropn :
      ('a, 's) kinfo
      (*
         The input stack enjoys a prefix of length [n]...
      *)
      * int
        (*
         ... and the following value witnesses that under this prefix
         the stack has type ['b * 'u].
      *)
      * ('b, 'u, 'b, 'u, 'a, 's, 'a, 's) stack_prefix_preservation_witness
      (*
         This stack is passed to the continuation since we drop the
         entire prefix.
      *)
      * ('b, 'u, 'r, 'f) kinstr
      -> ('a, 's, 'r, 'f) kinstr
  | IChainId :
      ('a, 's) kinfo * (Script_chain_id.t, 'a * 's, 'r, 'f) kinstr
      -> ('a, 's, 'r, 'f) kinstr
  | INever : (never, 's) kinfo -> (never, 's, 'r, 'f) kinstr
  | IVoting_power :
      (public_key_hash, 's) kinfo * (n num, 's, 'r, 'f) kinstr
      -> (public_key_hash, 's, 'r, 'f) kinstr
  | ITotal_voting_power :
      ('a, 's) kinfo * (n num, 'a * 's, 'r, 'f) kinstr
      -> ('a, 's, 'r, 'f) kinstr
  | IKeccak :
      (bytes, 's) kinfo * (bytes, 's, 'r, 'f) kinstr
      -> (bytes, 's, 'r, 'f) kinstr
  | ISha3 :
      (bytes, 's) kinfo * (bytes, 's, 'r, 'f) kinstr
      -> (bytes, 's, 'r, 'f) kinstr
  | IAdd_bls12_381_g1 :
      (Script_bls.G1.t, Script_bls.G1.t * 's) kinfo
      * (Script_bls.G1.t, 's, 'r, 'f) kinstr
      -> (Script_bls.G1.t, Script_bls.G1.t * 's, 'r, 'f) kinstr
  | IAdd_bls12_381_g2 :
      (Script_bls.G2.t, Script_bls.G2.t * 's) kinfo
      * (Script_bls.G2.t, 's, 'r, 'f) kinstr
      -> (Script_bls.G2.t, Script_bls.G2.t * 's, 'r, 'f) kinstr
  | IAdd_bls12_381_fr :
      (Script_bls.Fr.t, Script_bls.Fr.t * 's) kinfo
      * (Script_bls.Fr.t, 's, 'r, 'f) kinstr
      -> (Script_bls.Fr.t, Script_bls.Fr.t * 's, 'r, 'f) kinstr
  | IMul_bls12_381_g1 :
      (Script_bls.G1.t, Script_bls.Fr.t * 's) kinfo
      * (Script_bls.G1.t, 's, 'r, 'f) kinstr
      -> (Script_bls.G1.t, Script_bls.Fr.t * 's, 'r, 'f) kinstr
  | IMul_bls12_381_g2 :
      (Script_bls.G2.t, Script_bls.Fr.t * 's) kinfo
      * (Script_bls.G2.t, 's, 'r, 'f) kinstr
      -> (Script_bls.G2.t, Script_bls.Fr.t * 's, 'r, 'f) kinstr
  | IMul_bls12_381_fr :
      (Script_bls.Fr.t, Script_bls.Fr.t * 's) kinfo
      * (Script_bls.Fr.t, 's, 'r, 'f) kinstr
      -> (Script_bls.Fr.t, Script_bls.Fr.t * 's, 'r, 'f) kinstr
  | IMul_bls12_381_z_fr :
      (Script_bls.Fr.t, 'a num * 's) kinfo
      * (Script_bls.Fr.t, 's, 'r, 'f) kinstr
      -> (Script_bls.Fr.t, 'a num * 's, 'r, 'f) kinstr
  | IMul_bls12_381_fr_z :
      ('a num, Script_bls.Fr.t * 's) kinfo
      * (Script_bls.Fr.t, 's, 'r, 'f) kinstr
      -> ('a num, Script_bls.Fr.t * 's, 'r, 'f) kinstr
  | IInt_bls12_381_fr :
      (Script_bls.Fr.t, 's) kinfo * (z num, 's, 'r, 'f) kinstr
      -> (Script_bls.Fr.t, 's, 'r, 'f) kinstr
  | INeg_bls12_381_g1 :
      (Script_bls.G1.t, 's) kinfo * (Script_bls.G1.t, 's, 'r, 'f) kinstr
      -> (Script_bls.G1.t, 's, 'r, 'f) kinstr
  | INeg_bls12_381_g2 :
      (Script_bls.G2.t, 's) kinfo * (Script_bls.G2.t, 's, 'r, 'f) kinstr
      -> (Script_bls.G2.t, 's, 'r, 'f) kinstr
  | INeg_bls12_381_fr :
      (Script_bls.Fr.t, 's) kinfo * (Script_bls.Fr.t, 's, 'r, 'f) kinstr
      -> (Script_bls.Fr.t, 's, 'r, 'f) kinstr
  | IPairing_check_bls12_381 :
      ((Script_bls.G1.t, Script_bls.G2.t) pair boxed_list, 's) kinfo
      * (bool, 's, 'r, 'f) kinstr
      -> ((Script_bls.G1.t, Script_bls.G2.t) pair boxed_list, 's, 'r, 'f) kinstr
  | IComb :
      ('a, 's) kinfo
      * int
      * ('a * 's, 'b * 'u) comb_gadt_witness
      * ('b, 'u, 'r, 'f) kinstr
      -> ('a, 's, 'r, 'f) kinstr
  | IUncomb :
      ('a, 's) kinfo
      * int
      * ('a * 's, 'b * 'u) uncomb_gadt_witness
      * ('b, 'u, 'r, 'f) kinstr
      -> ('a, 's, 'r, 'f) kinstr
  | IComb_get :
      ('t, 's) kinfo
      * int
      * ('t, 'v) comb_get_gadt_witness
      * ('v, 's, 'r, 'f) kinstr
      -> ('t, 's, 'r, 'f) kinstr
  | IComb_set :
      ('a, 'b * 's) kinfo
      * int
      * ('a, 'b, 'c) comb_set_gadt_witness
      * ('c, 's, 'r, 'f) kinstr
      -> ('a, 'b * 's, 'r, 'f) kinstr
  | IDup_n :
      ('a, 's) kinfo
      * int
      * ('a * 's, 't) dup_n_gadt_witness
      * ('t, 'a * 's, 'r, 'f) kinstr
      -> ('a, 's, 'r, 'f) kinstr
  | ITicket :
      ('a, n num * 's) kinfo * ('a ticket, 's, 'r, 'f) kinstr
      -> ('a, n num * 's, 'r, 'f) kinstr
  | IRead_ticket :
      ('a ticket, 's) kinfo
      * (address * ('a * n num), 'a ticket * 's, 'r, 'f) kinstr
      -> ('a ticket, 's, 'r, 'f) kinstr
  | ISplit_ticket :
      ('a ticket, (n num * n num) * 's) kinfo
      * (('a ticket * 'a ticket) option, 's, 'r, 'f) kinstr
      -> ('a ticket, (n num * n num) * 's, 'r, 'f) kinstr
  | IJoin_tickets :
      ('a ticket * 'a ticket, 's) kinfo
      * 'a comparable_ty
      * ('a ticket option, 's, 'r, 'f) kinstr
      -> ('a ticket * 'a ticket, 's, 'r, 'f) kinstr
  | IOpen_chest :
      (Script_timelock.chest_key, Script_timelock.chest * (n num * 's)) kinfo
      * ((bytes, bool) union, 's, 'r, 'f) kinstr
      -> ( Script_timelock.chest_key,
           Script_timelock.chest * (n num * 's),
           'r,
           'f )
         kinstr
  (*

     Internal control instructions
     =============================

     The following instructions are not available in the source language.
     They are used by the internals of the interpreter.
  *)
  | IHalt : ('a, 's) kinfo -> ('a, 's, 'a, 's) kinstr
  | ILog :
      ('a, 's) kinfo * logging_event * logger * ('a, 's, 'r, 'f) kinstr
      -> ('a, 's, 'r, 'f) kinstr

and logging_event =
  | LogEntry : logging_event
  | LogExit : ('b, 'u) kinfo -> logging_event

and ('arg, 'ret) lambda =
  | Lam :
      ('arg, end_of_stack, 'ret, end_of_stack) kdescr * Script.node
      -> ('arg, 'ret) lambda
[@@coq_force_gadt]

and 'arg typed_contract =
  | Typed_contract : {
      arg_ty : ('arg, _) ty;
      address : address;
    }
      -> 'arg typed_contract

(*

  Control stack
  =============

  The control stack is a list of [kinstr].

  Since [kinstr] denotes a list  of instructions, the control stack
  can be seen as a list of instruction sequences, each representing a
  form of delimited continuation (i.e. a control stack fragment). The
  [continuation] GADT ensures that the input and output stack types of the
  continuations are consistent.

  Loops have a special treatment because their control stack is reused
  as is for the next iteration. This avoids the reallocation of a
  control stack cell at each iteration.

  To implement [step] as a tail-recursive function, we implement
  higher-order iterators (i.e. MAPs and ITERs) using internal instructions
. Roughly speaking, these instructions help in decomposing the execution
  of [I f c] (where [I] is an higher-order iterator over a container [c])
  into three phases: to start the iteration, to execute [f] if there are
  elements to be processed in [c], and to loop.

  Dip also has a dedicated constructor in the control stack.  This
  allows the stack prefix to be restored after the execution of the
  [Dip]'s body.

  Following the same style as in [kinstr], [continuation] has four
  arguments, two for each stack types. More precisely, with

            [('bef_top, 'bef, 'aft_top, 'aft) continuation]

  we encode the fact that the stack before executing the continuation
  has type [('bef_top * 'bef)] and that the stack after this execution
  has type [('aft_top * 'aft)].

*)
and (_, _, _, _) continuation =
  (* This continuation returns immediately. *)
  | KNil : ('r, 'f, 'r, 'f) continuation
  (* This continuation starts with the next instruction to execute. *)
  | KCons :
      ('a, 's, 'b, 't) kinstr * ('b, 't, 'r, 'f) continuation
      -> ('a, 's, 'r, 'f) continuation
  (* This continuation represents a call frame: it stores the caller's
     stack of type ['s] and the continuation which expects the callee's
     result on top of the stack. *)
  | KReturn :
      's * ('a, 's, 'r, 'f) continuation
      -> ('a, end_of_stack, 'r, 'f) continuation
  (* This continuation is useful when stack head requires some wrapping or
     unwrapping before it can be passed forward. For instance this continuation
     is used after a [MAP] instruction applied to an option in order to wrap the
     result back in a [Some] constructor.

     /!\ When using it, make sure the function runs in constant time or that gas
     has been properly charged beforehand.
     Also make sure it runs with a small, bounded stack.
  *)
  | KMap_head :
      ('a -> 'b) * ('b, 's, 'r, 'f) continuation
      -> ('a, 's, 'r, 'f) continuation
  (* This continuation comes right after a [Dip i] to restore the topmost
     element ['b] of the stack after having executed [i] in the substack
     of type ['a * 's]. *)
  | KUndip :
      'b * ('b, 'a * 's, 'r, 'f) continuation
      -> ('a, 's, 'r, 'f) continuation
  (* This continuation is executed at each iteration of a loop with
     a Boolean condition. *)
  | KLoop_in :
      ('a, 's, bool, 'a * 's) kinstr * ('a, 's, 'r, 'f) continuation
      -> (bool, 'a * 's, 'r, 'f) continuation
  (* This continuation is executed at each iteration of a loop with
     a condition encoded by a sum type. *)
  | KLoop_in_left :
      ('a, 's, ('a, 'b) union, 's) kinstr * ('b, 's, 'r, 'f) continuation
      -> (('a, 'b) union, 's, 'r, 'f) continuation
  (* This continuation is executed at each iteration of a traversal.
     (Used in List, Map and Set.) *)
  | KIter :
      ('a, 'b * 's, 'b, 's) kinstr * 'a list * ('b, 's, 'r, 'f) continuation
      -> ('b, 's, 'r, 'f) continuation
  (* This continuation represents each step of a List.map. *)
  | KList_enter_body :
      ('a, 'c * 's, 'b, 'c * 's) kinstr
      * 'a list
      * 'b list
      * int
      * ('b boxed_list, 'c * 's, 'r, 'f) continuation
      -> ('c, 's, 'r, 'f) continuation
  (* This continuation represents what is done after each step of a List.map. *)
  | KList_exit_body :
      ('a, 'c * 's, 'b, 'c * 's) kinstr
      * 'a list
      * 'b list
      * int
      * ('b boxed_list, 'c * 's, 'r, 'f) continuation
      -> ('b, 'c * 's, 'r, 'f) continuation
  (* This continuation represents each step of a Map.map. *)
  | KMap_enter_body :
      ('a * 'b, 'd * 's, 'c, 'd * 's) kinstr
      * ('a * 'b) list
      * ('a, 'c) map
      * (('a, 'c) map, 'd * 's, 'r, 'f) continuation
      -> ('d, 's, 'r, 'f) continuation
  (* This continuation represents what is done after each step of a Map.map. *)
  | KMap_exit_body :
      ('a * 'b, 'd * 's, 'c, 'd * 's) kinstr
      * ('a * 'b) list
      * ('a, 'c) map
      * 'a
      * (('a, 'c) map, 'd * 's, 'r, 'f) continuation
      -> ('c, 'd * 's, 'r, 'f) continuation
  (* This continuation represents what is done after returning from a view.
     It holds the original step constants value prior to entering the view. *)
  | KView_exit :
      step_constants * ('a, 's, 'r, 'f) continuation
      -> ('a, 's, 'r, 'f) continuation
  (* This continuation instruments the execution with a [logger]. *)
  | KLog :
      ('a, 's, 'r, 'f) continuation * logger
      -> ('a, 's, 'r, 'f) continuation

(*

    Execution instrumentation
    =========================

   One can observe the context and the stack at some specific points
   of an execution step. This feature is implemented by calling back
   some [logging_function]s defined in a record of type [logger]
   passed as argument to the step function.

   A [logger] is typically embedded in an [KLog] continuation by the
   client to trigger an evaluation instrumented with some logging. The
   logger is then automatically propagated to the logging instruction
   [ILog] as well as to any instructions that need to generate a
   backtrace when it fails (e.g., [IFailwith], [IMul_teznat], ...).

*)
and ('a, 's, 'b, 'f, 'c, 'u) logging_function =
  ('a, 's, 'b, 'f) kinstr ->
  context ->
  Script.location ->
  ('c, 'u) stack_ty ->
  'c * 'u ->
  unit

and execution_trace = (Script.location * Gas.t * Script.expr list) list

and logger = {
  log_interp : 'a 's 'b 'f 'c 'u. ('a, 's, 'b, 'f, 'c, 'u) logging_function;
      (** [log_interp] is called at each call of the internal function
          [interp]. [interp] is called when starting the interpretation of
          a script and subsequently at each [Exec] instruction. *)
  log_entry : 'a 's 'b 'f. ('a, 's, 'b, 'f, 'a, 's) logging_function;
      (** [log_entry] is called {i before} executing each instruction but
          {i after} gas for this instruction has been successfully
          consumed. *)
  log_control : 'a 's 'b 'f. ('a, 's, 'b, 'f) continuation -> unit;
      (** [log_control] is called {i before} the interpretation of the
          current continuation. *)
  log_exit : 'a 's 'b 'f 'c 'u. ('a, 's, 'b, 'f, 'c, 'u) logging_function;
      (** [log_exit] is called {i after} executing each instruction. *)
  get_log : unit -> execution_trace option tzresult Lwt.t;
      (** [get_log] allows to obtain an execution trace, if any was
          produced. *)
}

(* ---- Auxiliary types -----------------------------------------------------*)
and ('ty, 'comparable) ty =
  | Unit_t : (unit, yes) ty
  | Int_t : (z num, yes) ty
  | Nat_t : (n num, yes) ty
  | Signature_t : (signature, yes) ty
  | String_t : (Script_string.t, yes) ty
  | Bytes_t : (bytes, yes) ty
  | Mutez_t : (Tez.t, yes) ty
  | Key_hash_t : (public_key_hash, yes) ty
  | Key_t : (public_key, yes) ty
  | Timestamp_t : (Script_timestamp.t, yes) ty
  | Address_t : (address, yes) ty
  | Tx_rollup_l2_address_t : (tx_rollup_l2_address, yes) ty
  | Bool_t : (bool, yes) ty
  | Pair_t :
      ('a, 'ac) ty
      * ('b, 'bc) ty
      * ('a, 'b) pair ty_metadata
      * ('ac, 'bc, 'rc) dand
      -> (('a, 'b) pair, 'rc) ty
  | Union_t :
      ('a, 'ac) ty
      * ('b, 'bc) ty
      * ('a, 'b) union ty_metadata
      * ('ac, 'bc, 'rc) dand
      -> (('a, 'b) union, 'rc) ty
  | Lambda_t :
      ('arg, _) ty * ('ret, _) ty * ('arg, 'ret) lambda ty_metadata
      -> (('arg, 'ret) lambda, no) ty
  | Option_t :
      ('v, 'c) ty * 'v option ty_metadata * 'c dbool
      -> ('v option, 'c) ty
  | List_t : ('v, _) ty * 'v boxed_list ty_metadata -> ('v boxed_list, no) ty
  | Set_t : 'v comparable_ty * 'v set ty_metadata -> ('v set, no) ty
  | Map_t :
      'k comparable_ty * ('v, _) ty * ('k, 'v) map ty_metadata
      -> (('k, 'v) map, no) ty
  | Big_map_t :
      'k comparable_ty * ('v, _) ty * ('k, 'v) big_map ty_metadata
      -> (('k, 'v) big_map, no) ty
  | Contract_t :
      ('arg, _) ty * 'arg typed_contract ty_metadata
      -> ('arg typed_contract, no) ty
  | Sapling_transaction_t : Sapling.Memo_size.t -> (Sapling.transaction, no) ty
  | Sapling_transaction_deprecated_t :
      Sapling.Memo_size.t
      -> (Sapling.Legacy.transaction, no) ty
  | Sapling_state_t : Sapling.Memo_size.t -> (Sapling.state, no) ty
  | Operation_t : (operation, no) ty
  | Chain_id_t : (Script_chain_id.t, yes) ty
  | Never_t : (never, yes) ty
  | Bls12_381_g1_t : (Script_bls.G1.t, no) ty
  | Bls12_381_g2_t : (Script_bls.G2.t, no) ty
  | Bls12_381_fr_t : (Script_bls.Fr.t, no) ty
  | Ticket_t : 'a comparable_ty * 'a ticket ty_metadata -> ('a ticket, no) ty
  | Chest_key_t : (Script_timelock.chest_key, no) ty
  | Chest_t : (Script_timelock.chest, no) ty

and 'ty comparable_ty = ('ty, yes) ty

and ('top_ty, 'resty) stack_ty =
  | Item_t :
      ('ty, _) ty * ('ty2, 'rest) stack_ty
      -> ('ty, 'ty2 * 'rest) stack_ty
  | Bot_t : (empty_cell, empty_cell) stack_ty

and ('key, 'value) big_map =
  | Big_map : {
      id : Big_map.Id.t option;
      diff : ('key, 'value) big_map_overlay;
      key_type : 'key comparable_ty;
      value_type : ('value, _) ty;
    }
      -> ('key, 'value) big_map

and ('a, 's, 'r, 'f) kdescr = {
  kloc : Script.location;
  kbef : ('a, 's) stack_ty;
  kaft : ('r, 'f) stack_ty;
  kinstr : ('a, 's, 'r, 'f) kinstr;
}

and ('a, 's) kinfo = {iloc : Script.location; kstack_ty : ('a, 's) stack_ty}

(*

   Several instructions work under an arbitrary deep stack prefix
   (e.g, IDipn, IDropn, etc). To convince the typechecker that
   these instructions are well-typed, we must provide a witness
   to statically characterize the relationship between the input
   and the output stacks. The inhabitants of the following GADT
   act as such witnesses.

   More precisely, a value [w] of type

   [(c, t, d, v, a, s, b, u) stack_prefix_preservation_witness]

   proves that there is a common prefix between an input stack
   of type [a * s] and an output stack of type [b * u]. This prefix
   is as deep as the number of [KPrefix] application in [w]. When
   used with an operation parameterized by a natural number [n]
   characterizing the depth at which the operation must be applied,
   [w] is the Peano encoding of [n].

   When this prefix is removed from the two stacks, the input stack
   has type [c * t] while the output stack has type [d * v].

*)
and (_, _, _, _, _, _, _, _) stack_prefix_preservation_witness =
  | KPrefix :
      ('y, 'u) kinfo
      * ('c, 'v, 'd, 'w, 'x, 's, 'y, 'u) stack_prefix_preservation_witness
      -> ( 'c,
           'v,
           'd,
           'w,
           'a,
           'x * 's,
           'a,
           'y * 'u )
         stack_prefix_preservation_witness
  | KRest : ('a, 's, 'b, 'u, 'a, 's, 'b, 'u) stack_prefix_preservation_witness

and ('before, 'after) comb_gadt_witness =
  | Comb_one : ('a * ('x * 'before), 'a * ('x * 'before)) comb_gadt_witness
  | Comb_succ :
      ('before, 'b * 'after) comb_gadt_witness
      -> ('a * 'before, ('a * 'b) * 'after) comb_gadt_witness

and ('before, 'after) uncomb_gadt_witness =
  | Uncomb_one : ('rest, 'rest) uncomb_gadt_witness
  | Uncomb_succ :
      ('b * 'before, 'after) uncomb_gadt_witness
      -> (('a * 'b) * 'before, 'a * 'after) uncomb_gadt_witness

and ('before, 'after) comb_get_gadt_witness =
  | Comb_get_zero : ('b, 'b) comb_get_gadt_witness
  | Comb_get_one : ('a * 'b, 'a) comb_get_gadt_witness
  | Comb_get_plus_two :
      ('before, 'after) comb_get_gadt_witness
      -> ('a * 'before, 'after) comb_get_gadt_witness

and ('value, 'before, 'after) comb_set_gadt_witness =
  | Comb_set_zero : ('value, _, 'value) comb_set_gadt_witness
  | Comb_set_one : ('value, 'hd * 'tl, 'value * 'tl) comb_set_gadt_witness
  | Comb_set_plus_two :
      ('value, 'before, 'after) comb_set_gadt_witness
      -> ('value, 'a * 'before, 'a * 'after) comb_set_gadt_witness
[@@coq_force_gadt]

(*

   [dup_n_gadt_witness ('s, 't)] ensures that there exists at least
   [n] elements in ['s] and that the [n]-th element of ['s] is of type
   ['t]. Here [n] follows Peano's encoding (0 and successor).
   Besides, [0] corresponds to the topmost element of ['s].

   This relational predicate is defined by induction on [n].

*)
and (_, _) dup_n_gadt_witness =
  | Dup_n_zero : ('a * 'rest, 'a) dup_n_gadt_witness
  | Dup_n_succ :
      ('stack, 'b) dup_n_gadt_witness
      -> ('a * 'stack, 'b) dup_n_gadt_witness

and ('input, 'output) view_signature =
  | View_signature : {
      name : Script_string.t;
      input_ty : ('input, _) ty;
      output_ty : ('output, _) ty;
    }
      -> ('input, 'output) view_signature

and 'kind manager_operation =
  | Transaction : {
      (* The [transaction.parameters] field may seem useless since we have
         access to a typed version of the field (with [parameters_ty] and
         [parameters]), but we keep it so that we do not have to unparse the
         typed version in order to produce the receipt
         ([Apply_results.internal_manager_operation]). *)
      transaction : Alpha_context.transaction;
      location : Script.location;
      parameters_ty : ('a, _) ty;
      parameters : 'a;
    }
      -> Kind.transaction manager_operation
  | Origination : {
      origination : Alpha_context.origination;
      preorigination : Contract.t;
      storage_type : ('storage, _) ty;
      storage : 'storage;
    }
      -> Kind.origination manager_operation
  | Delegation :
      Signature.Public_key_hash.t option
      -> Kind.delegation manager_operation

and 'kind internal_operation = {
  source : Contract.contract;
  operation : 'kind manager_operation;
  nonce : int;
}

and packed_internal_operation =
  | Internal_operation : 'kind internal_operation -> packed_internal_operation
[@@ocaml.unboxed]

and operation = {
  piop : packed_internal_operation;
  lazy_storage_diff : Lazy_storage.diffs option;
}

type packed_manager_operation =
  | Manager : 'kind manager_operation -> packed_manager_operation
[@@ocaml.unboxed]

val manager_kind : 'kind manager_operation -> 'kind Kind.manager

val kinfo_of_kinstr : ('a, 's, 'b, 'f) kinstr -> ('a, 's) kinfo

type kinstr_rewritek = {
  apply : 'b 'u 'r 'f. ('b, 'u, 'r, 'f) kinstr -> ('b, 'u, 'r, 'f) kinstr;
}

val kinstr_rewritek :
  ('a, 's, 'r, 'f) kinstr -> kinstr_rewritek -> ('a, 's, 'r, 'f) kinstr

val ty_size : ('a, _) ty -> 'a Type_size.t

val comparable_ty_size : 'a comparable_ty -> 'a Type_size.t

val is_comparable : ('v, 'c) ty -> 'c dbool

type 'v ty_ex_c = Ty_ex_c : ('v, _) ty -> 'v ty_ex_c [@@ocaml.unboxed]

val unit_key : unit comparable_ty

val never_key : never comparable_ty

val int_key : z num comparable_ty

val nat_key : n num comparable_ty

val signature_key : signature comparable_ty

val string_key : Script_string.t comparable_ty

val bytes_key : Bytes.t comparable_ty

val mutez_key : Tez.t comparable_ty

val bool_key : bool comparable_ty

val key_hash_key : public_key_hash comparable_ty

val key_key : public_key comparable_ty

val timestamp_key : Script_timestamp.t comparable_ty

val chain_id_key : Script_chain_id.t comparable_ty

val address_key : address comparable_ty

val tx_rollup_l2_address_key : tx_rollup_l2_address comparable_ty

val pair_key :
  Script.location ->
  'a comparable_ty ->
  'b comparable_ty ->
  ('a, 'b) pair comparable_ty tzresult

val pair_3_key :
  Script.location ->
  'a comparable_ty ->
  'b comparable_ty ->
  'c comparable_ty ->
  ('a, ('b, 'c) pair) pair comparable_ty tzresult

val union_key :
  Script.location ->
  'a comparable_ty ->
  'b comparable_ty ->
  ('a, 'b) union comparable_ty tzresult

val option_key :
  Script.location -> 'v comparable_ty -> 'v option comparable_ty tzresult

val unit_t : unit comparable_ty

val int_t : z num comparable_ty

val nat_t : n num comparable_ty

val signature_t : signature comparable_ty

val string_t : Script_string.t comparable_ty

val bytes_t : Bytes.t comparable_ty

val mutez_t : Tez.t comparable_ty

val key_hash_t : public_key_hash comparable_ty

val key_t : public_key comparable_ty

val timestamp_t : Script_timestamp.t comparable_ty

val address_t : address comparable_ty

val tx_rollup_l2_address_t : tx_rollup_l2_address comparable_ty

val bool_t : bool comparable_ty

val pair_t :
  Script.location -> ('a, _) ty -> ('b, _) ty -> ('a, 'b) pair ty_ex_c tzresult

val comparable_pair_t :
  Script.location ->
  ('a, yes) ty ->
  ('b, yes) ty ->
  (('a, 'b) pair, yes) ty tzresult

val union_t :
  Script.location -> ('a, _) ty -> ('b, _) ty -> ('a, 'b) union ty_ex_c tzresult

val comparable_union_t :
  Script.location ->
  ('a, yes) ty ->
  ('b, yes) ty ->
  ('a, 'b) union comparable_ty tzresult

val union_bytes_bool_t : (Bytes.t, bool) union comparable_ty

val lambda_t :
  Script.location ->
  ('arg, _) ty ->
  ('ret, _) ty ->
  (('arg, 'ret) lambda, no) ty tzresult

val option_t : Script.location -> ('v, 'c) ty -> ('v option, 'c) ty tzresult

val option_mutez_t : Tez.t option comparable_ty

val option_string_t : Script_string.t option comparable_ty

val option_bytes_t : Bytes.t option comparable_ty

val option_nat_t : n num option comparable_ty

val option_pair_nat_nat_t : (n num, n num) pair option comparable_ty

val option_pair_nat_mutez_t : (n num, Tez.t) pair option comparable_ty

val option_pair_mutez_mutez_t : (Tez.t, Tez.t) pair option comparable_ty

val option_pair_int_nat_t : (z num, n num) pair option comparable_ty

val list_t : Script.location -> ('v, _) ty -> ('v boxed_list, no) ty tzresult

val list_operation_t : (operation boxed_list, no) ty

val set_t : Script.location -> 'v comparable_ty -> ('v set, no) ty tzresult

val map_t :
  Script.location ->
  'k comparable_ty ->
  ('v, _) ty ->
  (('k, 'v) map, no) ty tzresult

val big_map_t :
  Script.location ->
  'k comparable_ty ->
  ('v, _) ty ->
  (('k, 'v) big_map, no) ty tzresult

val contract_t :
  Script.location -> ('arg, _) ty -> ('arg typed_contract, no) ty tzresult

val contract_unit_t : (unit typed_contract, no) ty

val sapling_transaction_t :
  memo_size:Sapling.Memo_size.t -> (Sapling.transaction, no) ty

val sapling_transaction_deprecated_t :
  memo_size:Sapling.Memo_size.t -> (Sapling.Legacy.transaction, no) ty

val sapling_state_t : memo_size:Sapling.Memo_size.t -> (Sapling.state, no) ty

val operation_t : (operation, no) ty

val chain_id_t : Script_chain_id.t comparable_ty

val never_t : never comparable_ty

val bls12_381_g1_t : (Script_bls.G1.t, no) ty

val bls12_381_g2_t : (Script_bls.G2.t, no) ty

val bls12_381_fr_t : (Script_bls.Fr.t, no) ty

val ticket_t :
  Script.location -> 'a comparable_ty -> ('a ticket, no) ty tzresult

val chest_key_t : (Script_timelock.chest_key, no) ty

val chest_t : (Script_timelock.chest, no) ty

(**

   The following functions named `X_traverse` for X in
   [{ kinstr, ty, comparable_ty, value }] provide tail recursive top down
   traversals over the values of these types.

   The traversal goes through a value and rewrites an accumulator
   along the way starting from some [init]ial value for the
   accumulator.

   All these traversals follow the same recursion scheme: the
   user-provided function is first called on the toplevel value, then
   the traversal recurses on the direct subvalues of the same type.

   Hence, the user-provided function must only compute the
   contribution of the value on the accumulator minus the contribution
   of its subvalues of the same type.

*)
type 'a kinstr_traverse = {
  apply : 'b 'u 'r 'f. 'a -> ('b, 'u, 'r, 'f) kinstr -> 'a;
}

val kinstr_traverse :
  ('a, 'b, 'c, 'd) kinstr -> 'ret -> 'ret kinstr_traverse -> 'ret

type 'a ty_traverse = {
  apply : 't 'tc. 'a -> ('t, 'tc) ty -> 'a;
  apply_comparable : 't. 'a -> 't comparable_ty -> 'a;
}

val comparable_ty_traverse : 'a comparable_ty -> 'r -> 'r ty_traverse -> 'r

val ty_traverse : ('a, _) ty -> 'r -> 'r ty_traverse -> 'r

type 'accu stack_ty_traverse = {
  apply : 'ty 's. 'accu -> ('ty, 's) stack_ty -> 'accu;
}

val stack_ty_traverse : ('a, 's) stack_ty -> 'r -> 'r stack_ty_traverse -> 'r

type 'a value_traverse = {
  apply : 't 'tc. 'a -> ('t, 'tc) ty -> 't -> 'a;
  apply_comparable : 't. 'a -> 't comparable_ty -> 't -> 'a;
}

val value_traverse :
  (('t, _) ty, 't comparable_ty) union -> 't -> 'r -> 'r value_traverse -> 'r

val stack_top_ty : ('a, 'b * 's) stack_ty -> 'a ty_ex_c
