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

(*

    The step function of the interpreter is parametrized by a bunch of values called the step constants.
    These values are indeed constants during the call of a smart contract with the notable exception of
    the IView instruction which modifies `sender`, `self`, and `amount` and the KView_exit continuation
    which restores them.
    ======================

*)
type step_constants = {
  sender : Destination.t;
      (** The address calling this contract, as returned by SENDER. *)
  payer : Signature.public_key_hash;
      (** The address of the implicit account that initiated the chain of contract calls, as returned by SOURCE. *)
  self : Contract_hash.t;
      (** The address of the contract being executed, as returned by SELF and SELF_ADDRESS.
     Also used:
     - as ticketer in TICKET
     - as caller in VIEW, TRANSFER_TOKENS, and CREATE_CONTRACT *)
  amount : Tez.t;
      (** The amount of the current transaction, as returned by AMOUNT. *)
  balance : Tez.t;  (** The balance of the contract as returned by BALANCE. *)
  chain_id : Chain_id.t;
      (** The chain id of the chain, as returned by CHAIN_ID. *)
  now : Script_timestamp.t;
      (** The earliest time at which the current block could have been timestamped, as returned by NOW. *)
  level : Script_int.n Script_int.num;
      (** The level of the current block, as returned by LEVEL. *)
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

  val size : t -> int
end

type signature = Script_signature.t

type ('a, 'b) pair = 'a * 'b

type ('a, 'b) or_ = L of 'a | R of 'b

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
    type t = Fr_tag of Bls.Primitive.Fr.t [@@ocaml.unboxed]

    include S with type t := t and type fr := t

    val of_z : Z.t -> t

    val to_z : t -> Z.t
  end

  module G1 : sig
    (** [t] is made algebraic in order to distinguish it from the other type
        parameters of [Script_typed_ir.ty]. *)
    type t = G1_tag of Bls.Primitive.G1.t [@@ocaml.unboxed]

    include S with type t := t and type fr := Fr.t
  end

  module G2 : sig
    (** [t] is made algebraic in order to distinguish it from the other type
        parameters of [Script_typed_ir.ty]. *)
    type t = G2_tag of Bls.Primitive.G2.t [@@ocaml.unboxed]

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

type ticket_amount = Ticket_amount.t

type 'a ticket = {ticketer : Contract.t; contents : 'a; amount : ticket_amount}

type empty_cell = EmptyCell

type end_of_stack = empty_cell * empty_cell

module Type_size : sig
  type 'a t

  val check_eq :
    error_details:('error_context, 'error_trace) Script_tc_errors.error_details ->
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

type view = {
  input_ty : Script.node;
  output_ty : Script.node;
  view_code : Script.node;
}

type view_map = (Script_string.t, view) map

type entrypoint_info = {name : Entrypoint.t; original_type_expr : Script.node}

(** ['arg entrypoints] represents the tree of entrypoints of a parameter type
    ['arg].
    [at_node] are entrypoint details at that node if it is not [None].
    [nested] are the entrypoints below the node in the tree.
      It is always [Entrypoints_None] for non-or nodes.
      But it is also ok to have [Entrypoints_None] for an or node, it just
      means that there are no entrypoints below that node in the tree.
*)
type 'arg entrypoints_node = {
  at_node : entrypoint_info option;
  nested : 'arg nested_entrypoints;
}

and 'arg nested_entrypoints =
  | Entrypoints_Or : {
      left : 'l entrypoints_node;
      right : 'r entrypoints_node;
    }
      -> ('l, 'r) or_ nested_entrypoints
  | Entrypoints_None : _ nested_entrypoints

(** [no_entrypoints] is [{at_node = None; nested = Entrypoints_None}] *)
val no_entrypoints : _ entrypoints_node

type logging_event = LogEntry | LogExit of Script.location

type 'arg entrypoints = {
  root : 'arg entrypoints_node;
  original_type_expr : Script.node;
}

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

   IHalt : Script.location -> ('a, 'S, 'a, 'S) kinstr

   Each instruction is decorated by its location: its value is only
   used for logging and error reporting and has no impact on the
   operational semantics.

   Notations:
   ----------

   In the following declaration, we use 'a, 'b, 'c, 'd, ...  to assign
   types to stack cell contents while we use 'S, 'T, 'U, ... to
   assign types to stacks.

   The types for the final result and stack rest of a whole sequence
   of instructions are written 'r and 'F (standing for "result" and
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
      Script.location * ('b, 'S, 'r, 'F) kinstr
      -> ('a, 'b * 'S, 'r, 'F) kinstr
  | IDup :
      Script.location * ('a, 'a * ('b * 'S), 'r, 'F) kinstr
      -> ('a, 'b * 'S, 'r, 'F) kinstr
  | ISwap :
      Script.location * ('b, 'a * ('c * 'S), 'r, 'F) kinstr
      -> ('a, 'b * ('c * 'S), 'r, 'F) kinstr
  | IPush :
      Script.location * ('ty, _) ty * 'ty * ('ty, 'a * 'S, 'r, 'F) kinstr
      -> ('a, 'S, 'r, 'F) kinstr
  | IUnit :
      Script.location * (unit, 'a * 'S, 'r, 'F) kinstr
      -> ('a, 'S, 'r, 'F) kinstr
  (*
     Pairs
     -----
  *)
  | ICons_pair :
      Script.location * (('a, 'b) pair, 'c * 'S, 'r, 'F) kinstr
      -> ('a, 'b * ('c * 'S), 'r, 'F) kinstr
  | ICar :
      Script.location * ('a, 'S, 'r, 'F) kinstr
      -> (('a, 'b) pair, 'S, 'r, 'F) kinstr
  | ICdr :
      Script.location * ('b, 'S, 'r, 'F) kinstr
      -> (('a, 'b) pair, 'S, 'r, 'F) kinstr
  | IUnpair :
      Script.location * ('a, 'b * 'S, 'r, 'F) kinstr
      -> (('a, 'b) pair, 'S, 'r, 'F) kinstr
  (*
     Options
     -------
   *)
  | ICons_some :
      Script.location * ('v option, 'a * 'S, 'r, 'F) kinstr
      -> ('v, 'a * 'S, 'r, 'F) kinstr
  | ICons_none :
      Script.location * ('b, _) ty * ('b option, 'a * 'S, 'r, 'F) kinstr
      -> ('a, 'S, 'r, 'F) kinstr
  | IIf_none : {
      loc : Script.location;
      branch_if_none : ('b, 'S, 'c, 'T) kinstr;
      branch_if_some : ('a, 'b * 'S, 'c, 'T) kinstr;
      k : ('c, 'T, 'r, 'F) kinstr;
    }
      -> ('a option, 'b * 'S, 'r, 'F) kinstr
  | IOpt_map : {
      loc : Script.location;
      body : ('a, 'S, 'b, 'S) kinstr;
      k : ('b option, 'S, 'c, 'F) kinstr;
    }
      -> ('a option, 'S, 'c, 'F) kinstr
  (*
     Ors
     ------
   *)
  | ICons_left :
      Script.location * ('b, _) ty * (('a, 'b) or_, 'c * 'S, 'r, 'F) kinstr
      -> ('a, 'c * 'S, 'r, 'F) kinstr
  | ICons_right :
      Script.location * ('a, _) ty * (('a, 'b) or_, 'c * 'S, 'r, 'F) kinstr
      -> ('b, 'c * 'S, 'r, 'F) kinstr
  | IIf_left : {
      loc : Script.location;
      branch_if_left : ('a, 'S, 'c, 'T) kinstr;
      branch_if_right : ('b, 'S, 'c, 'T) kinstr;
      k : ('c, 'T, 'r, 'F) kinstr;
    }
      -> (('a, 'b) or_, 'S, 'r, 'F) kinstr
  (*
     Lists
     -----
  *)
  | ICons_list :
      Script.location * ('a Script_list.t, 'S, 'r, 'F) kinstr
      -> ('a, 'a Script_list.t * 'S, 'r, 'F) kinstr
  | INil :
      Script.location * ('b, _) ty * ('b Script_list.t, 'a * 'S, 'r, 'F) kinstr
      -> ('a, 'S, 'r, 'F) kinstr
  | IIf_cons : {
      loc : Script.location;
      branch_if_cons : ('a, 'a Script_list.t * ('b * 'S), 'c, 'T) kinstr;
      branch_if_nil : ('b, 'S, 'c, 'T) kinstr;
      k : ('c, 'T, 'r, 'F) kinstr;
    }
      -> ('a Script_list.t, 'b * 'S, 'r, 'F) kinstr
  | IList_map :
      Script.location
      * ('a, 'c * 'S, 'b, 'c * 'S) kinstr
      * ('b Script_list.t, _) ty option
      * ('b Script_list.t, 'c * 'S, 'r, 'F) kinstr
      -> ('a Script_list.t, 'c * 'S, 'r, 'F) kinstr
  | IList_iter :
      Script.location
      * ('a, _) ty option
      * ('a, 'b * 'S, 'b, 'S) kinstr
      * ('b, 'S, 'r, 'F) kinstr
      -> ('a Script_list.t, 'b * 'S, 'r, 'F) kinstr
  | IList_size :
      Script.location * (n num, 'S, 'r, 'F) kinstr
      -> ('a Script_list.t, 'S, 'r, 'F) kinstr
  (*
    Sets
    ----
  *)
  | IEmpty_set :
      Script.location * 'b comparable_ty * ('b set, 'a * 'S, 'r, 'F) kinstr
      -> ('a, 'S, 'r, 'F) kinstr
  | ISet_iter :
      Script.location
      * 'a comparable_ty option
      * ('a, 'b * 'S, 'b, 'S) kinstr
      * ('b, 'S, 'r, 'F) kinstr
      -> ('a set, 'b * 'S, 'r, 'F) kinstr
  | ISet_mem :
      Script.location * (bool, 'S, 'r, 'F) kinstr
      -> ('a, 'a set * 'S, 'r, 'F) kinstr
  | ISet_update :
      Script.location * ('a set, 'S, 'r, 'F) kinstr
      -> ('a, bool * ('a set * 'S), 'r, 'F) kinstr
  | ISet_size :
      Script.location * (n num, 'S, 'r, 'F) kinstr
      -> ('a set, 'S, 'r, 'F) kinstr
  (*
     Maps
     ----
   *)
  | IEmpty_map :
      Script.location
      * 'b comparable_ty
      * ('c, _) ty option
      * (('b, 'c) map, 'a * 'S, 'r, 'F) kinstr
      -> ('a, 'S, 'r, 'F) kinstr
  | IMap_map :
      Script.location
      * (('a, 'c) map, _) ty option
      * (('a, 'b) pair, 'd * 'S, 'c, 'd * 'S) kinstr
      * (('a, 'c) map, 'd * 'S, 'r, 'F) kinstr
      -> (('a, 'b) map, 'd * 'S, 'r, 'F) kinstr
  | IMap_iter :
      Script.location
      * (('a, 'b) pair, _) ty option
      * (('a, 'b) pair, 'c * 'S, 'c, 'S) kinstr
      * ('c, 'S, 'r, 'F) kinstr
      -> (('a, 'b) map, 'c * 'S, 'r, 'F) kinstr
  | IMap_mem :
      Script.location * (bool, 'S, 'r, 'F) kinstr
      -> ('a, ('a, 'b) map * 'S, 'r, 'F) kinstr
  | IMap_get :
      Script.location * ('b option, 'S, 'r, 'F) kinstr
      -> ('a, ('a, 'b) map * 'S, 'r, 'F) kinstr
  | IMap_update :
      Script.location * (('a, 'b) map, 'S, 'r, 'F) kinstr
      -> ('a, 'b option * (('a, 'b) map * 'S), 'r, 'F) kinstr
  | IMap_get_and_update :
      Script.location * ('b option, ('a, 'b) map * 'S, 'r, 'F) kinstr
      -> ('a, 'b option * (('a, 'b) map * 'S), 'r, 'F) kinstr
  | IMap_size :
      Script.location * (n num, 'S, 'r, 'F) kinstr
      -> (('a, 'b) map, 'S, 'r, 'F) kinstr
  (*
     Big maps
     --------
  *)
  | IEmpty_big_map :
      Script.location
      * 'b comparable_ty
      * ('c, _) ty
      * (('b, 'c) big_map, 'a * 'S, 'r, 'F) kinstr
      -> ('a, 'S, 'r, 'F) kinstr
  | IBig_map_mem :
      Script.location * (bool, 'S, 'r, 'F) kinstr
      -> ('a, ('a, 'b) big_map * 'S, 'r, 'F) kinstr
  | IBig_map_get :
      Script.location * ('b option, 'S, 'r, 'F) kinstr
      -> ('a, ('a, 'b) big_map * 'S, 'r, 'F) kinstr
  | IBig_map_update :
      Script.location * (('a, 'b) big_map, 'S, 'r, 'F) kinstr
      -> ('a, 'b option * (('a, 'b) big_map * 'S), 'r, 'F) kinstr
  | IBig_map_get_and_update :
      Script.location * ('b option, ('a, 'b) big_map * 'S, 'r, 'F) kinstr
      -> ('a, 'b option * (('a, 'b) big_map * 'S), 'r, 'F) kinstr
  (*
     Strings
     -------
  *)
  | IConcat_string :
      Script.location * (Script_string.t, 'S, 'r, 'F) kinstr
      -> (Script_string.t Script_list.t, 'S, 'r, 'F) kinstr
  | IConcat_string_pair :
      Script.location * (Script_string.t, 'S, 'r, 'F) kinstr
      -> (Script_string.t, Script_string.t * 'S, 'r, 'F) kinstr
  | ISlice_string :
      Script.location * (Script_string.t option, 'S, 'r, 'F) kinstr
      -> (n num, n num * (Script_string.t * 'S), 'r, 'F) kinstr
  | IString_size :
      Script.location * (n num, 'S, 'r, 'F) kinstr
      -> (Script_string.t, 'S, 'r, 'F) kinstr
  (*
     Bytes
     -----
  *)
  | IConcat_bytes :
      Script.location * (bytes, 'S, 'r, 'F) kinstr
      -> (bytes Script_list.t, 'S, 'r, 'F) kinstr
  | IConcat_bytes_pair :
      Script.location * (bytes, 'S, 'r, 'F) kinstr
      -> (bytes, bytes * 'S, 'r, 'F) kinstr
  | ISlice_bytes :
      Script.location * (bytes option, 'S, 'r, 'F) kinstr
      -> (n num, n num * (bytes * 'S), 'r, 'F) kinstr
  | IBytes_size :
      Script.location * (n num, 'S, 'r, 'F) kinstr
      -> (bytes, 'S, 'r, 'F) kinstr
  | ILsl_bytes :
      Script.location * (bytes, 'S, 'r, 'F) kinstr
      -> (bytes, n num * 'S, 'r, 'F) kinstr
  | ILsr_bytes :
      Script.location * (bytes, 'S, 'r, 'F) kinstr
      -> (bytes, n num * 'S, 'r, 'F) kinstr
  | IOr_bytes :
      Script.location * (bytes, 'S, 'r, 'F) kinstr
      -> (bytes, bytes * 'S, 'r, 'F) kinstr
  | IAnd_bytes :
      Script.location * (bytes, 'S, 'r, 'F) kinstr
      -> (bytes, bytes * 'S, 'r, 'F) kinstr
  | IXor_bytes :
      Script.location * (bytes, 'S, 'r, 'F) kinstr
      -> (bytes, bytes * 'S, 'r, 'F) kinstr
  | INot_bytes :
      Script.location * (bytes, 'S, 'r, 'F) kinstr
      -> (bytes, 'S, 'r, 'F) kinstr
  | INat_bytes :
      Script.location * (n num, 'S, 'r, 'F) kinstr
      -> (bytes, 'S, 'r, 'F) kinstr
  | IBytes_nat :
      Script.location * (bytes, 'S, 'r, 'F) kinstr
      -> (n num, 'S, 'r, 'F) kinstr
  | IInt_bytes :
      Script.location * (z num, 'S, 'r, 'F) kinstr
      -> (bytes, 'S, 'r, 'F) kinstr
  | IBytes_int :
      Script.location * (bytes, 'S, 'r, 'F) kinstr
      -> (z num, 'S, 'r, 'F) kinstr
  (*
     Timestamps
     ----------
   *)
  | IAdd_seconds_to_timestamp :
      Script.location * (Script_timestamp.t, 'S, 'r, 'F) kinstr
      -> (z num, Script_timestamp.t * 'S, 'r, 'F) kinstr
  | IAdd_timestamp_to_seconds :
      Script.location * (Script_timestamp.t, 'S, 'r, 'F) kinstr
      -> (Script_timestamp.t, z num * 'S, 'r, 'F) kinstr
  | ISub_timestamp_seconds :
      Script.location * (Script_timestamp.t, 'S, 'r, 'F) kinstr
      -> (Script_timestamp.t, z num * 'S, 'r, 'F) kinstr
  | IDiff_timestamps :
      Script.location * (z num, 'S, 'r, 'F) kinstr
      -> (Script_timestamp.t, Script_timestamp.t * 'S, 'r, 'F) kinstr
  (*
     Tez
     ---
    *)
  | IAdd_tez :
      Script.location * (Tez.t, 'S, 'r, 'F) kinstr
      -> (Tez.t, Tez.t * 'S, 'r, 'F) kinstr
  | ISub_tez :
      Script.location * (Tez.t option, 'S, 'r, 'F) kinstr
      -> (Tez.t, Tez.t * 'S, 'r, 'F) kinstr
  | ISub_tez_legacy :
      Script.location * (Tez.t, 'S, 'r, 'F) kinstr
      -> (Tez.t, Tez.t * 'S, 'r, 'F) kinstr
  | IMul_teznat :
      Script.location * (Tez.t, 'S, 'r, 'F) kinstr
      -> (Tez.t, n num * 'S, 'r, 'F) kinstr
  | IMul_nattez :
      Script.location * (Tez.t, 'S, 'r, 'F) kinstr
      -> (n num, Tez.t * 'S, 'r, 'F) kinstr
  | IEdiv_teznat :
      Script.location * ((Tez.t, Tez.t) pair option, 'S, 'r, 'F) kinstr
      -> (Tez.t, n num * 'S, 'r, 'F) kinstr
  | IEdiv_tez :
      Script.location * ((n num, Tez.t) pair option, 'S, 'r, 'F) kinstr
      -> (Tez.t, Tez.t * 'S, 'r, 'F) kinstr
  (*
     Booleans
     --------
   *)
  | IOr :
      Script.location * (bool, 'S, 'r, 'F) kinstr
      -> (bool, bool * 'S, 'r, 'F) kinstr
  | IAnd :
      Script.location * (bool, 'S, 'r, 'F) kinstr
      -> (bool, bool * 'S, 'r, 'F) kinstr
  | IXor :
      Script.location * (bool, 'S, 'r, 'F) kinstr
      -> (bool, bool * 'S, 'r, 'F) kinstr
  | INot :
      Script.location * (bool, 'S, 'r, 'F) kinstr
      -> (bool, 'S, 'r, 'F) kinstr
  (*
     Integers
     --------
  *)
  | IIs_nat :
      Script.location * (n num option, 'S, 'r, 'F) kinstr
      -> (z num, 'S, 'r, 'F) kinstr
  | INeg :
      Script.location * (z num, 'S, 'r, 'F) kinstr
      -> ('a num, 'S, 'r, 'F) kinstr
  | IAbs_int :
      Script.location * (n num, 'S, 'r, 'F) kinstr
      -> (z num, 'S, 'r, 'F) kinstr
  | IInt_nat :
      Script.location * (z num, 'S, 'r, 'F) kinstr
      -> (n num, 'S, 'r, 'F) kinstr
  | IAdd_int :
      Script.location * (z num, 'S, 'r, 'F) kinstr
      -> ('a num, 'b num * 'S, 'r, 'F) kinstr
  | IAdd_nat :
      Script.location * (n num, 'S, 'r, 'F) kinstr
      -> (n num, n num * 'S, 'r, 'F) kinstr
  | ISub_int :
      Script.location * (z num, 'S, 'r, 'F) kinstr
      -> ('a num, 'b num * 'S, 'r, 'F) kinstr
  | IMul_int :
      Script.location * (z num, 'S, 'r, 'F) kinstr
      -> ('a num, 'b num * 'S, 'r, 'F) kinstr
  | IMul_nat :
      Script.location * ('a num, 'S, 'r, 'F) kinstr
      -> (n num, 'a num * 'S, 'r, 'F) kinstr
  | IEdiv_int :
      Script.location * ((z num, n num) pair option, 'S, 'r, 'F) kinstr
      -> ('a num, 'b num * 'S, 'r, 'F) kinstr
  | IEdiv_nat :
      Script.location * (('a num, n num) pair option, 'S, 'r, 'F) kinstr
      -> (n num, 'a num * 'S, 'r, 'F) kinstr
  | ILsl_nat :
      Script.location * (n num, 'S, 'r, 'F) kinstr
      -> (n num, n num * 'S, 'r, 'F) kinstr
  | ILsr_nat :
      Script.location * (n num, 'S, 'r, 'F) kinstr
      -> (n num, n num * 'S, 'r, 'F) kinstr
  | IOr_nat :
      Script.location * (n num, 'S, 'r, 'F) kinstr
      -> (n num, n num * 'S, 'r, 'F) kinstr
  | IAnd_nat :
      Script.location * (n num, 'S, 'r, 'F) kinstr
      -> (n num, n num * 'S, 'r, 'F) kinstr
  | IAnd_int_nat :
      Script.location * (n num, 'S, 'r, 'F) kinstr
      -> (z num, n num * 'S, 'r, 'F) kinstr
  | IXor_nat :
      Script.location * (n num, 'S, 'r, 'F) kinstr
      -> (n num, n num * 'S, 'r, 'F) kinstr
  | INot_int :
      Script.location * (z num, 'S, 'r, 'F) kinstr
      -> ('a num, 'S, 'r, 'F) kinstr
  (*
     Control
     -------
  *)
  | IIf : {
      loc : Script.location;
      branch_if_true : ('a, 'S, 'b, 'T) kinstr;
      branch_if_false : ('a, 'S, 'b, 'T) kinstr;
      k : ('b, 'T, 'r, 'F) kinstr;
    }
      -> (bool, 'a * 'S, 'r, 'F) kinstr
  | ILoop :
      Script.location * ('a, 'S, bool, 'a * 'S) kinstr * ('a, 'S, 'r, 'F) kinstr
      -> (bool, 'a * 'S, 'r, 'F) kinstr
  | ILoop_left :
      Script.location
      * ('a, 'S, ('a, 'b) or_, 'S) kinstr
      * ('b, 'S, 'r, 'F) kinstr
      -> (('a, 'b) or_, 'S, 'r, 'F) kinstr
  | IDip :
      Script.location
      * ('b, 'S, 'c, 'T) kinstr
      * ('a, _) ty option
      * ('a, 'c * 'T, 'r, 'F) kinstr
      -> ('a, 'b * 'S, 'r, 'F) kinstr
  | IExec :
      Script.location * ('b, 'S) stack_ty option * ('b, 'S, 'r, 'F) kinstr
      -> ('a, ('a, 'b) lambda * 'S, 'r, 'F) kinstr
  | IApply :
      Script.location * ('a, _) ty * (('b, 'c) lambda, 'S, 'r, 'F) kinstr
      -> ('a, (('a, 'b) pair, 'c) lambda * 'S, 'r, 'F) kinstr
  | ILambda :
      Script.location
      * ('b, 'c) lambda
      * (('b, 'c) lambda, 'a * 'S, 'r, 'F) kinstr
      -> ('a, 'S, 'r, 'F) kinstr
  | IFailwith : Script.location * ('a, _) ty -> ('a, 'S, 'r, 'F) kinstr
  (*
     Comparison
     ----------
  *)
  | ICompare :
      Script.location * 'a comparable_ty * (z num, 'b * 'S, 'r, 'F) kinstr
      -> ('a, 'a * ('b * 'S), 'r, 'F) kinstr
  (*
     Comparators
     -----------
  *)
  | IEq :
      Script.location * (bool, 'S, 'r, 'F) kinstr
      -> (z num, 'S, 'r, 'F) kinstr
  | INeq :
      Script.location * (bool, 'S, 'r, 'F) kinstr
      -> (z num, 'S, 'r, 'F) kinstr
  | ILt :
      Script.location * (bool, 'S, 'r, 'F) kinstr
      -> (z num, 'S, 'r, 'F) kinstr
  | IGt :
      Script.location * (bool, 'S, 'r, 'F) kinstr
      -> (z num, 'S, 'r, 'F) kinstr
  | ILe :
      Script.location * (bool, 'S, 'r, 'F) kinstr
      -> (z num, 'S, 'r, 'F) kinstr
  | IGe :
      Script.location * (bool, 'S, 'r, 'F) kinstr
      -> (z num, 'S, 'r, 'F) kinstr
  (*
     Protocol
     --------
  *)
  | IAddress :
      Script.location * (address, 'S, 'r, 'F) kinstr
      -> ('a typed_contract, 'S, 'r, 'F) kinstr
  | IContract :
      Script.location
      * ('a, _) ty
      * Entrypoint.t
      * ('a typed_contract option, 'S, 'r, 'F) kinstr
      -> (address, 'S, 'r, 'F) kinstr
  | IView :
      Script.location
      * ('a, 'b) view_signature
      * ('c, 'S) stack_ty option
      * ('b option, 'c * 'S, 'r, 'F) kinstr
      -> ('a, address * ('c * 'S), 'r, 'F) kinstr
  | ITransfer_tokens :
      Script.location * (operation, 'S, 'r, 'F) kinstr
      -> ('a, Tez.t * ('a typed_contract * 'S), 'r, 'F) kinstr
  | IImplicit_account :
      Script.location * (unit typed_contract, 'S, 'r, 'F) kinstr
      -> (public_key_hash, 'S, 'r, 'F) kinstr
  | IIs_implicit_account :
      Script.location * (public_key_hash option, 'S, 'r, 'F) kinstr
      -> (address, 'S, 'r, 'F) kinstr
  | IIndex_address :
      Script.location * (n num, 'S, 'r, 'F) kinstr
      -> (address, 'S, 'r, 'F) kinstr
  | IGet_address_index :
      Script.location * (n num option, 'S, 'r, 'F) kinstr
      -> (address, 'S, 'r, 'F) kinstr
  | ICreate_contract : {
      loc : Script.location;
      storage_type : ('a, _) ty;
      code : Script.expr;
      k : (operation, address * ('c * 'S), 'r, 'F) kinstr;
    }
      -> (public_key_hash option, Tez.t * ('a * ('c * 'S)), 'r, 'F) kinstr
  | ISet_delegate :
      Script.location * (operation, 'S, 'r, 'F) kinstr
      -> (public_key_hash option, 'S, 'r, 'F) kinstr
  | INow :
      Script.location * (Script_timestamp.t, 'a * 'S, 'r, 'F) kinstr
      -> ('a, 'S, 'r, 'F) kinstr
  | IMin_block_time :
      Script.location * (n num, 'a * 'S, 'r, 'F) kinstr
      -> ('a, 'S, 'r, 'F) kinstr
  | IBalance :
      Script.location * (Tez.t, 'a * 'S, 'r, 'F) kinstr
      -> ('a, 'S, 'r, 'F) kinstr
  | ILevel :
      Script.location * (n num, 'a * 'S, 'r, 'F) kinstr
      -> ('a, 'S, 'r, 'F) kinstr
  | ICheck_signature :
      Script.location * (bool, 'S, 'r, 'F) kinstr
      -> (public_key, signature * (bytes * 'S), 'r, 'F) kinstr
  | IHash_key :
      Script.location * (public_key_hash, 'S, 'r, 'F) kinstr
      -> (public_key, 'S, 'r, 'F) kinstr
  | IPack :
      Script.location * ('a, _) ty * (bytes, 'b * 'S, 'r, 'F) kinstr
      -> ('a, 'b * 'S, 'r, 'F) kinstr
  | IUnpack :
      Script.location * ('a, _) ty * ('a option, 'S, 'r, 'F) kinstr
      -> (bytes, 'S, 'r, 'F) kinstr
  | IBlake2b :
      Script.location * (bytes, 'S, 'r, 'F) kinstr
      -> (bytes, 'S, 'r, 'F) kinstr
  | ISha256 :
      Script.location * (bytes, 'S, 'r, 'F) kinstr
      -> (bytes, 'S, 'r, 'F) kinstr
  | ISha512 :
      Script.location * (bytes, 'S, 'r, 'F) kinstr
      -> (bytes, 'S, 'r, 'F) kinstr
  | ISource :
      Script.location * (address, 'a * 'S, 'r, 'F) kinstr
      -> ('a, 'S, 'r, 'F) kinstr
  | ISender :
      Script.location * (address, 'a * 'S, 'r, 'F) kinstr
      -> ('a, 'S, 'r, 'F) kinstr
  | ISelf :
      Script.location
      * ('b, _) ty
      * Entrypoint.t
      * ('b typed_contract, 'a * 'S, 'r, 'F) kinstr
      -> ('a, 'S, 'r, 'F) kinstr
  | ISelf_address :
      Script.location * (address, 'a * 'S, 'r, 'F) kinstr
      -> ('a, 'S, 'r, 'F) kinstr
  | IAmount :
      Script.location * (Tez.t, 'a * 'S, 'r, 'F) kinstr
      -> ('a, 'S, 'r, 'F) kinstr
  | ISapling_empty_state :
      Script.location
      * Sapling.Memo_size.t
      * (Sapling.state, 'a * 'S, 'b, 'F) kinstr
      -> ('a, 'S, 'b, 'F) kinstr
  | ISapling_verify_update :
      Script.location
      * ((bytes, (z num, Sapling.state) pair) pair option, 'S, 'r, 'F) kinstr
      -> (Sapling.transaction, Sapling.state * 'S, 'r, 'F) kinstr
  | ISapling_verify_update_deprecated :
      (* legacy introduced in J *)
      Script.location
      * ((z num, Sapling.state) pair option, 'S, 'r, 'F) kinstr
      -> (Sapling.Legacy.transaction, Sapling.state * 'S, 'r, 'F) kinstr
  | IDig :
      Script.location
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
      * ('b, 'c * 'T, 'c, 'T, 'a, 'S, 'd, 'U) stack_prefix_preservation_witness
      * ('b, 'd * 'U, 'r, 'F) kinstr
      -> ('a, 'S, 'r, 'F) kinstr
  | IDug :
      Script.location
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
      * ('c, 'T, 'a, 'c * 'T, 'b, 'S, 'd, 'U) stack_prefix_preservation_witness
      * ('d, 'U, 'r, 'F) kinstr
      -> ('a, 'b * 'S, 'r, 'F) kinstr
  | IDipn :
      Script.location
      (* The body of Dipn is applied under a prefix of size [n]... *)
      * int
        (*
        ... the relation between the types of the input and output stacks
        is characterized by the following witness.
        (See forthcoming comments about [stack_prefix_preservation_witness].)
       *)
      * ('c, 'T, 'd, 'V, 'a, 'S, 'b, 'U) stack_prefix_preservation_witness
      * ('c, 'T, 'd, 'V) kinstr
      * ('b, 'U, 'r, 'F) kinstr
      -> ('a, 'S, 'r, 'F) kinstr
  | IDropn :
      Script.location
      (*
         The input stack enjoys a prefix of length [n]...
      *)
      * int
        (*
         ... and the following value witnesses that under this prefix
         the stack has type ['b * 'u].
      *)
      * ('b, 'U, 'b, 'U, 'a, 'S, 'a, 'S) stack_prefix_preservation_witness
      (*
         This stack is passed to the continuation since we drop the
         entire prefix.
      *)
      * ('b, 'U, 'r, 'F) kinstr
      -> ('a, 'S, 'r, 'F) kinstr
  | IChainId :
      Script.location * (Script_chain_id.t, 'a * 'S, 'r, 'F) kinstr
      -> ('a, 'S, 'r, 'F) kinstr
  | INever : Script.location -> (never, 'S, 'r, 'F) kinstr
  | IVoting_power :
      Script.location * (n num, 'S, 'r, 'F) kinstr
      -> (public_key_hash, 'S, 'r, 'F) kinstr
  | ITotal_voting_power :
      Script.location * (n num, 'a * 'S, 'r, 'F) kinstr
      -> ('a, 'S, 'r, 'F) kinstr
  | IKeccak :
      Script.location * (bytes, 'S, 'r, 'F) kinstr
      -> (bytes, 'S, 'r, 'F) kinstr
  | ISha3 :
      Script.location * (bytes, 'S, 'r, 'F) kinstr
      -> (bytes, 'S, 'r, 'F) kinstr
  | IAdd_bls12_381_g1 :
      Script.location * (Script_bls.G1.t, 'S, 'r, 'F) kinstr
      -> (Script_bls.G1.t, Script_bls.G1.t * 'S, 'r, 'F) kinstr
  | IAdd_bls12_381_g2 :
      Script.location * (Script_bls.G2.t, 'S, 'r, 'F) kinstr
      -> (Script_bls.G2.t, Script_bls.G2.t * 'S, 'r, 'F) kinstr
  | IAdd_bls12_381_fr :
      Script.location * (Script_bls.Fr.t, 'S, 'r, 'F) kinstr
      -> (Script_bls.Fr.t, Script_bls.Fr.t * 'S, 'r, 'F) kinstr
  | IMul_bls12_381_g1 :
      Script.location * (Script_bls.G1.t, 'S, 'r, 'F) kinstr
      -> (Script_bls.G1.t, Script_bls.Fr.t * 'S, 'r, 'F) kinstr
  | IMul_bls12_381_g2 :
      Script.location * (Script_bls.G2.t, 'S, 'r, 'F) kinstr
      -> (Script_bls.G2.t, Script_bls.Fr.t * 'S, 'r, 'F) kinstr
  | IMul_bls12_381_fr :
      Script.location * (Script_bls.Fr.t, 'S, 'r, 'F) kinstr
      -> (Script_bls.Fr.t, Script_bls.Fr.t * 'S, 'r, 'F) kinstr
  | IMul_bls12_381_z_fr :
      Script.location * (Script_bls.Fr.t, 'S, 'r, 'F) kinstr
      -> (Script_bls.Fr.t, 'a num * 'S, 'r, 'F) kinstr
  | IMul_bls12_381_fr_z :
      Script.location * (Script_bls.Fr.t, 'S, 'r, 'F) kinstr
      -> ('a num, Script_bls.Fr.t * 'S, 'r, 'F) kinstr
  | IInt_bls12_381_fr :
      Script.location * (z num, 'S, 'r, 'F) kinstr
      -> (Script_bls.Fr.t, 'S, 'r, 'F) kinstr
  | INeg_bls12_381_g1 :
      Script.location * (Script_bls.G1.t, 'S, 'r, 'F) kinstr
      -> (Script_bls.G1.t, 'S, 'r, 'F) kinstr
  | INeg_bls12_381_g2 :
      Script.location * (Script_bls.G2.t, 'S, 'r, 'F) kinstr
      -> (Script_bls.G2.t, 'S, 'r, 'F) kinstr
  | INeg_bls12_381_fr :
      Script.location * (Script_bls.Fr.t, 'S, 'r, 'F) kinstr
      -> (Script_bls.Fr.t, 'S, 'r, 'F) kinstr
  | IPairing_check_bls12_381 :
      Script.location * (bool, 'S, 'r, 'F) kinstr
      -> ( (Script_bls.G1.t, Script_bls.G2.t) pair Script_list.t,
           'S,
           'r,
           'F )
         kinstr
  | IComb :
      Script.location
      * int
      * ('a, 'b, 'S, 'c, 'd, 'T) comb_gadt_witness
      * ('c, 'd * 'T, 'r, 'F) kinstr
      -> ('a, 'b * 'S, 'r, 'F) kinstr
  | IUncomb :
      Script.location
      * int
      * ('a, 'b, 'S, 'c, 'd, 'T) uncomb_gadt_witness
      * ('c, 'd * 'T, 'r, 'F) kinstr
      -> ('a, 'b * 'S, 'r, 'F) kinstr
  | IComb_get :
      Script.location
      * int
      * ('t, 'v) comb_get_gadt_witness
      * ('v, 'a * 'S, 'r, 'F) kinstr
      -> ('t, 'a * 'S, 'r, 'F) kinstr
  | IComb_set :
      Script.location
      * int
      * ('a, 'b, 'c) comb_set_gadt_witness
      * ('c, 'd * 'S, 'r, 'F) kinstr
      -> ('a, 'b * ('d * 'S), 'r, 'F) kinstr
  | IDup_n :
      Script.location
      * int
      * ('a, 'b, 'S, 't) dup_n_gadt_witness
      * ('t, 'a * ('b * 'S), 'r, 'F) kinstr
      -> ('a, 'b * 'S, 'r, 'F) kinstr
  | ITicket :
      Script.location
      * 'a comparable_ty option
      * ('a ticket option, 'S, 'r, 'F) kinstr
      -> ('a, n num * 'S, 'r, 'F) kinstr
  | ITicket_deprecated :
      Script.location * 'a comparable_ty option * ('a ticket, 'S, 'r, 'F) kinstr
      -> ('a, n num * 'S, 'r, 'F) kinstr
  | IRead_ticket :
      Script.location
      * 'a comparable_ty option
      * ((address, ('a, n num) pair) pair, 'a ticket * 'S, 'r, 'F) kinstr
      -> ('a ticket, 'S, 'r, 'F) kinstr
  | ISplit_ticket :
      Script.location * (('a ticket, 'a ticket) pair option, 'S, 'r, 'F) kinstr
      -> ('a ticket, (n num, n num) pair * 'S, 'r, 'F) kinstr
  | IJoin_tickets :
      Script.location * 'a comparable_ty * ('a ticket option, 'S, 'r, 'F) kinstr
      -> (('a ticket, 'a ticket) pair, 'S, 'r, 'F) kinstr
  | IOpen_chest :
      Script.location * (bytes option, 'S, 'r, 'F) kinstr
      -> ( Script_timelock.chest_key,
           Script_timelock.chest * (n num * 'S),
           'r,
           'F )
         kinstr
  | IEmit : {
      loc : Script.location;
      tag : Entrypoint.t;
      ty : ('a, _) ty;
      unparsed_ty : Script.expr;
      k : (operation, 'S, 'r, 'F) kinstr;
    }
      -> ('a, 'S, 'r, 'F) kinstr
  (*

     Internal control instructions
     =============================

     The following instructions are not available in the source language.
     They are used by the internals of the interpreter.
   *)
  | IHalt : Script.location -> ('a, 'S, 'a, 'S) kinstr
  | ILog :
      Script.location
      * ('a, 'S) stack_ty
      * logging_event
      * logger
      * ('a, 'S, 'r, 'F) kinstr
      -> ('a, 'S, 'r, 'F) kinstr

and ('arg, 'ret) lambda =
  | Lam :
      ('arg, end_of_stack, 'ret, end_of_stack) kdescr * Script.node
      -> ('arg, 'ret) lambda
  | LamRec :
      ('arg, ('arg, 'ret) lambda * end_of_stack, 'ret, end_of_stack) kdescr
      * Script.node
      -> ('arg, 'ret) lambda

and 'arg typed_contract =
  | Typed_implicit : public_key_hash -> unit typed_contract
  | Typed_implicit_with_ticket : {
      ticket_ty : ('arg ticket, _) ty;
      destination : public_key_hash;
    }
      -> 'arg ticket typed_contract
  | Typed_originated : {
      arg_ty : ('arg, _) ty;
      contract_hash : Contract_hash.t;
      entrypoint : Entrypoint.t;
    }
      -> 'arg typed_contract
  | Typed_sc_rollup : {
      arg_ty : ('arg, _) ty;
      sc_rollup : Sc_rollup.t;
      entrypoint : Entrypoint.t;
    }
      -> 'arg typed_contract
  | Typed_zk_rollup : {
      arg_ty : (('a ticket, bytes) pair, _) ty;
      zk_rollup : Zk_rollup.t;
    }
      -> ('a ticket, bytes) pair typed_contract

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
  | KNil : ('r, 'F, 'r, 'F) continuation
  (* This continuation starts with the next instruction to execute. *)
  | KCons :
      ('a, 'S, 'b, 'T) kinstr * ('b, 'T, 'r, 'F) continuation
      -> ('a, 'S, 'r, 'F) continuation
  (* This continuation represents a call frame: it stores the caller's
     stack of type ['s] and the continuation which expects the callee's
     result on top of the stack. *)
  | KReturn :
      'S * ('a, 'S) stack_ty option * ('a, 'S, 'r, 'F) continuation
      -> ('a, end_of_stack, 'r, 'F) continuation
  (* This continuation is useful when stack head requires some wrapping or
     unwrapping before it can be passed forward. For instance this continuation
     is used after a [MAP] instruction applied to an option in order to wrap the
     result back in a [Some] constructor.

     /!\ When using it, make sure the function runs in constant time or that gas
     has been properly charged beforehand.
     Also make sure it runs with a small, bounded stack.
  *)
  | KMap_head :
      ('a -> 'b) * ('b, 'S, 'r, 'F) continuation
      -> ('a, 'S, 'r, 'F) continuation
  (* This continuation comes right after a [Dip i] to restore the topmost
     element ['b] of the stack after having executed [i] in the substack
     of type ['a * 's]. *)
  | KUndip :
      'b * ('b, _) ty option * ('b, 'a * 'S, 'r, 'F) continuation
      -> ('a, 'S, 'r, 'F) continuation
  (* This continuation is executed at each iteration of a loop with
     a Boolean condition. *)
  | KLoop_in :
      ('a, 'S, bool, 'a * 'S) kinstr * ('a, 'S, 'r, 'F) continuation
      -> (bool, 'a * 'S, 'r, 'F) continuation
  (* This continuation is executed at each iteration of a loop with
     a condition encoded by a sum type. *)
  | KLoop_in_left :
      ('a, 'S, ('a, 'b) or_, 'S) kinstr * ('b, 'S, 'r, 'F) continuation
      -> (('a, 'b) or_, 'S, 'r, 'F) continuation
  (* This continuation is executed at each iteration of a traversal.
     (Used in List, Map and Set.) *)
  | KIter :
      ('a, 'b * 'S, 'b, 'S) kinstr
      * ('a, _) ty option
      * 'a list
      * ('b, 'S, 'r, 'F) continuation
      -> ('b, 'S, 'r, 'F) continuation
  (* This continuation represents each step of a List.map. *)
  | KList_enter_body :
      ('a, 'c * 'S, 'b, 'c * 'S) kinstr
      * 'a list
      * 'b Script_list.t
      * ('b Script_list.t, _) ty option
      * int
      * ('b Script_list.t, 'c * 'S, 'r, 'F) continuation
      -> ('c, 'S, 'r, 'F) continuation
  (* This continuation represents what is done after each step of a List.map. *)
  | KList_exit_body :
      ('a, 'c * 'S, 'b, 'c * 'S) kinstr
      * 'a list
      * 'b Script_list.t
      * ('b Script_list.t, _) ty option
      * int
      * ('b Script_list.t, 'c * 'S, 'r, 'F) continuation
      -> ('b, 'c * 'S, 'r, 'F) continuation
  (* This continuation represents each step of a Map.map. *)
  | KMap_enter_body :
      (('a, 'b) pair, 'd * 'S, 'c, 'd * 'S) kinstr
      * ('a * 'b) list
      * ('a, 'c) map
      * (('a, 'c) map, _) ty option
      * (('a, 'c) map, 'd * 'S, 'r, 'F) continuation
      -> ('d, 'S, 'r, 'F) continuation
  (* This continuation represents what is done after each step of a Map.map. *)
  | KMap_exit_body :
      (('a, 'b) pair, 'd * 'S, 'c, 'd * 'S) kinstr
      * ('a * 'b) list
      * ('a, 'c) map
      * 'a
      * (('a, 'c) map, _) ty option
      * (('a, 'c) map, 'd * 'S, 'r, 'F) continuation
      -> ('c, 'd * 'S, 'r, 'F) continuation
  (* This continuation represents what is done after returning from a view.
     It holds the original step constants value prior to entering the view. *)
  | KView_exit :
      step_constants * ('a, 'S, 'r, 'F) continuation
      -> ('a, 'S, 'r, 'F) continuation
  (* This continuation instruments the execution with a [logger]. *)
  | KLog :
      ('a, 'S, 'r, 'F) continuation * ('a, 'S) stack_ty * logger
      -> ('a, 'S, 'r, 'F) continuation

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
and ('a, 'S, 'b, 'F, 'c, 'U) logging_function =
  ('a, 'S, 'b, 'F) kinstr ->
  context ->
  Script.location ->
  ('c, 'U) stack_ty ->
  'c * 'U ->
  unit

and execution_trace = (Script.location * Gas.Arith.fp * Script.expr list) list

and logger = {
  log_interp : 'a 'S 'b 'F 'c 'U. ('a, 'S, 'b, 'F, 'c, 'U) logging_function;
      (** [log_interp] is called at each call of the internal function
          [interp]. [interp] is called when starting the interpretation of
          a script and subsequently at each [Exec] instruction. *)
  get_log : unit -> execution_trace option tzresult Lwt.t;
      (** [get_log] allows to obtain an execution trace, if any was
          produced. *)
  klog : 'a 'S 'r 'F. ('a, 'S, 'r, 'F) klog;
      (** [klog] is called on [KLog] inserted when instrumenting
          continuations. *)
  ilog : 'a 'S 'b 'T 'r 'F. ('a, 'S, 'b, 'T, 'r, 'F) ilog;
      (** [ilog] is called on [ILog] inserted when instrumenting
          instructions. *)
  log_kinstr : 'a 'b 'c 'd. ('a, 'b, 'c, 'd) log_kinstr;
      (** [log_kinstr] instruments an instruction with [ILog]. *)
}

and ('a, 'S, 'r, 'F) klog =
  logger ->
  Local_gas_counter.outdated_context * step_constants ->
  Local_gas_counter.local_gas_counter ->
  ('a, 'S) stack_ty ->
  ('a, 'S, 'r, 'F) continuation ->
  ('a, 'S, 'r, 'F) continuation ->
  'a ->
  'S ->
  ('r
  * 'F
  * Local_gas_counter.outdated_context
  * Local_gas_counter.local_gas_counter)
  tzresult
  Lwt.t

and ('a, 'S, 'b, 'T, 'r, 'F) ilog =
  logger ->
  logging_event ->
  ('a, 'S) stack_ty ->
  ('a, 'S, 'b, 'T, 'r, 'F) step_type

and ('a, 'S, 'b, 'T, 'r, 'F) step_type =
  Local_gas_counter.outdated_context * step_constants ->
  Local_gas_counter.local_gas_counter ->
  ('a, 'S, 'b, 'T) kinstr ->
  ('b, 'T, 'r, 'F) continuation ->
  'a ->
  'S ->
  ('r
  * 'F
  * Local_gas_counter.outdated_context
  * Local_gas_counter.local_gas_counter)
  tzresult
  Lwt.t

and ('a, 'b, 'c, 'd) log_kinstr =
  logger ->
  ('a, 'b) stack_ty ->
  ('a, 'b, 'c, 'd) kinstr ->
  ('a, 'b, 'c, 'd) kinstr

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
  | Bool_t : (bool, yes) ty
  | Pair_t :
      ('a, 'ac) ty
      * ('b, 'bc) ty
      * ('a, 'b) pair ty_metadata
      * ('ac, 'bc, 'rc) dand
      -> (('a, 'b) pair, 'rc) ty
  | Or_t :
      ('a, 'ac) ty
      * ('b, 'bc) ty
      * ('a, 'b) or_ ty_metadata
      * ('ac, 'bc, 'rc) dand
      -> (('a, 'b) or_, 'rc) ty
  | Lambda_t :
      ('arg, _) ty * ('ret, _) ty * ('arg, 'ret) lambda ty_metadata
      -> (('arg, 'ret) lambda, no) ty
  | Option_t :
      ('v, 'c) ty * 'v option ty_metadata * 'c dbool
      -> ('v option, 'c) ty
  | List_t :
      ('v, _) ty * 'v Script_list.t ty_metadata
      -> ('v Script_list.t, no) ty
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

and ('a, 'S, 'r, 'F) kdescr = {
  kloc : Script.location;
  kbef : ('a, 'S) stack_ty;
  kaft : ('r, 'F) stack_ty;
  kinstr : ('a, 'S, 'r, 'F) kinstr;
}

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
      Script.location
      * ('a, _) ty
      * ('c, 'V, 'd, 'W, 'x, 'S, 'y, 'U) stack_prefix_preservation_witness
      -> ( 'c,
           'V,
           'd,
           'W,
           'a,
           'x * 'S,
           'a,
           'y * 'U )
         stack_prefix_preservation_witness
  | KRest : ('a, 'S, 'b, 'U, 'a, 'S, 'b, 'U) stack_prefix_preservation_witness

and (_, _, _, _, _, _) comb_gadt_witness =
  | Comb_one : ('a, 'x, 'before, 'a, 'x, 'before) comb_gadt_witness
  | Comb_succ :
      ('b, 'c, 'S, 'd, 'e, 'T) comb_gadt_witness
      -> ('a, 'b, 'c * 'S, 'a * 'd, 'e, 'T) comb_gadt_witness

and (_, _, _, _, _, _) uncomb_gadt_witness =
  | Uncomb_one : ('a, 'x, 'before, 'a, 'x, 'before) uncomb_gadt_witness
  | Uncomb_succ :
      ('b, 'c, 'S, 'd, 'e, 'T) uncomb_gadt_witness
      -> (('a, 'b) pair, 'c, 'S, 'a, 'd, 'e * 'T) uncomb_gadt_witness

and ('before, 'after) comb_get_gadt_witness =
  | Comb_get_zero : ('b, 'b) comb_get_gadt_witness
  | Comb_get_one : (('a, 'b) pair, 'a) comb_get_gadt_witness
  | Comb_get_plus_two :
      ('before, 'after) comb_get_gadt_witness
      -> (('a, 'before) pair, 'after) comb_get_gadt_witness

and ('value, 'before, 'after) comb_set_gadt_witness =
  | Comb_set_zero : ('value, _, 'value) comb_set_gadt_witness
  | Comb_set_one :
      ('value, ('hd, 'tl) pair, ('value, 'tl) pair) comb_set_gadt_witness
  | Comb_set_plus_two :
      ('value, 'before, 'after) comb_set_gadt_witness
      -> ('value, ('a, 'before) pair, ('a, 'after) pair) comb_set_gadt_witness

(*

   [dup_n_gadt_witness ('a, 'b, 'S, 'T)] ensures that there exists at least
   [n] elements in ['a, 'b, 's] and that the [n]-th element is of type
   ['t]. Here [n] follows Peano's encoding (0 and successor).
   Besides, [0] corresponds to the topmost element of ['s].

   This relational predicate is defined by induction on [n].

*)
and (_, _, _, _) dup_n_gadt_witness =
  | Dup_n_zero : ('a, _, _, 'a) dup_n_gadt_witness
  | Dup_n_succ :
      ('b, 'c, 'stack, 'd) dup_n_gadt_witness
      -> ('a, 'b, 'c * 'stack, 'd) dup_n_gadt_witness

and ('input, 'output) view_signature =
  | View_signature : {
      name : Script_string.t;
      input_ty : ('input, _) ty;
      output_ty : ('output, _) ty;
    }
      -> ('input, 'output) view_signature

and 'kind internal_operation_contents =
  | Transaction_to_implicit : {
      destination : Signature.Public_key_hash.t;
      amount : Tez.t;
    }
      -> Kind.transaction internal_operation_contents
  | Transaction_to_implicit_with_ticket : {
      destination : Signature.Public_key_hash.t;
      ticket_ty : ('content ticket, _) ty;
      ticket : 'content ticket;
      unparsed_ticket : Script.lazy_expr;
      amount : Tez.t;
    }
      -> Kind.transaction internal_operation_contents
  | Transaction_to_smart_contract : {
      (* The [unparsed_parameters] field may seem useless since we have
         access to a typed version of the field (with [parameters_ty] and
         [parameters]), but we keep it so that we do not have to unparse the
         typed version in order to produce the receipt
         ([Apply_internal_results.internal_operation_contents]). *)
      destination : Contract_hash.t;
      amount : Tez.t;
      entrypoint : Entrypoint.t;
      location : Script.location;
      parameters_ty : ('a, _) ty;
      parameters : 'a;
      unparsed_parameters : Script.expr;
    }
      -> Kind.transaction internal_operation_contents
  | Transaction_to_sc_rollup : {
      destination : Sc_rollup.t;
      entrypoint : Entrypoint.t;
      parameters_ty : ('a, _) ty;
      parameters : 'a;
      unparsed_parameters : Script.expr;
    }
      -> Kind.transaction internal_operation_contents
  | Event : {
      ty : Script.expr;
      tag : Entrypoint.t;
      unparsed_data : Script.expr;
    }
      -> Kind.event internal_operation_contents
  | Transaction_to_zk_rollup : {
      destination : Zk_rollup.t;
      parameters_ty : (('a ticket, bytes) pair, _) ty;
      parameters : ('a ticket, bytes) pair;
      unparsed_parameters : Script.expr;
    }
      -> Kind.transaction internal_operation_contents
  | Origination : {
      delegate : Signature.Public_key_hash.t option;
      code : Script.expr;
      unparsed_storage : Script.expr;
      credit : Tez.t;
      preorigination : Contract_hash.t;
      storage_type : ('storage, _) ty;
      storage : 'storage;
    }
      -> Kind.origination internal_operation_contents
  | Delegation :
      Signature.Public_key_hash.t option
      -> Kind.delegation internal_operation_contents

and 'kind internal_operation = {
  sender : Destination.t;
  operation : 'kind internal_operation_contents;
  nonce : int;
}

and packed_internal_operation =
  | Internal_operation : 'kind internal_operation -> packed_internal_operation
[@@ocaml.unboxed]

and operation = {
  piop : packed_internal_operation;
  lazy_storage_diff : Lazy_storage.diffs option;
}

type ex_ty = Ex_ty : ('a, _) ty -> ex_ty

type ('arg, 'arg_dep, 'storage, 'storage_dep) types = {
  arg_type : ('arg, 'arg_dep) ty;
  storage_type : ('storage, 'storage_dep) ty;
  entrypoints : 'arg entrypoints;
}

val manager_kind : 'kind internal_operation_contents -> 'kind Kind.manager

val kinstr_location : (_, _, _, _) kinstr -> Script.location

val ty_size : ('a, _) ty -> 'a Type_size.t

val is_comparable : ('v, 'c) ty -> 'c dbool

type 'v ty_ex_c = Ty_ex_c : ('v, _) ty -> 'v ty_ex_c [@@ocaml.unboxed]

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

val bool_t : bool comparable_ty

val pair_t :
  Script.location -> ('a, _) ty -> ('b, _) ty -> ('a, 'b) pair ty_ex_c tzresult

val pair_3_t :
  Script.location ->
  ('a, _) ty ->
  ('b, _) ty ->
  ('c, _) ty ->
  ('a, ('b, 'c) pair) pair ty_ex_c tzresult

val comparable_pair_t :
  Script.location ->
  'a comparable_ty ->
  'b comparable_ty ->
  ('a, 'b) pair comparable_ty tzresult

val comparable_pair_3_t :
  Script.location ->
  'a comparable_ty ->
  'b comparable_ty ->
  'c comparable_ty ->
  ('a, ('b, 'c) pair) pair comparable_ty tzresult

val pair_int_int_unit_t : (z num, (z num, unit) pair) pair comparable_ty

val or_t :
  Script.location -> ('a, _) ty -> ('b, _) ty -> ('a, 'b) or_ ty_ex_c tzresult

val comparable_or_t :
  Script.location ->
  'a comparable_ty ->
  'b comparable_ty ->
  ('a, 'b) or_ comparable_ty tzresult

val or_bytes_bool_t : (Bytes.t, bool) or_ comparable_ty

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

val list_t : Script.location -> ('v, _) ty -> ('v Script_list.t, no) ty tzresult

val list_operation_t : (operation Script_list.t, no) ty

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

val key_hash_option_t : (public_key_hash option, yes) ty

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
   [{ kinstr, ty, stack_ty, value }] provide tail recursive top down
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
  apply : 'b 'S 'r 'F. 'a -> ('b, 'S, 'r, 'F) kinstr -> 'a;
}

val kinstr_traverse :
  ('a, 'S, 'c, 'F) kinstr -> 'ret -> 'ret kinstr_traverse -> 'ret

type 'a ty_traverse = {apply : 't 'tc. 'a -> ('t, 'tc) ty -> 'a}

val ty_traverse : ('a, _) ty -> 'r -> 'r ty_traverse -> 'r

type 'accu stack_ty_traverse = {
  apply : 'ty 'S. 'accu -> ('ty, 'S) stack_ty -> 'accu;
}

val stack_ty_traverse : ('a, 'S) stack_ty -> 'r -> 'r stack_ty_traverse -> 'r

type 'a value_traverse = {apply : 't 'tc. 'a -> ('t, 'tc) ty -> 't -> 'a}

val value_traverse : ('t, _) ty -> 't -> 'r -> 'r value_traverse -> 'r

val stack_top_ty : ('a, 'b * 'S) stack_ty -> 'a ty_ex_c

module Typed_contract : sig
  val destination : _ typed_contract -> Destination.t

  val arg_ty : 'a typed_contract -> 'a ty_ex_c

  val entrypoint : _ typed_contract -> Entrypoint.t

  module Internal_for_tests : sig
    (* This function doesn't guarantee that the contract is well-typed wrt its
       registered type at origination, it only guarantees that the type is
       plausible wrt to the destination kind. *)
    val typed_exn :
      ('a, _) ty -> Destination.t -> Entrypoint.t -> 'a typed_contract
  end
end
