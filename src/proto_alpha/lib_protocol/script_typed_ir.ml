(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.dev>                     *)
(* Copyright (c) 2021-2022 Nomadic Labs <contact@nomadic-labs.com>           *)
(* Copyright (c) 2022 DaiLambda, Inc. <contact@dailambda,jp>                 *)
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

module Script_signature = struct
  type t = Signature_tag of signature [@@ocaml.unboxed]

  let make s = Signature_tag s

  let get (Signature_tag s) = s

  let encoding =
    Data_encoding.conv
      (fun (Signature_tag x) -> x)
      (fun x -> Signature_tag x)
      Signature.encoding

  let of_b58check_opt x = Option.map make (Signature.of_b58check_opt x)

  let check ?watermark pub_key (Signature_tag s) bytes =
    Signature.check ?watermark pub_key s bytes

  let compare (Signature_tag x) (Signature_tag y) = Signature.compare x y

  let size (Signature_tag s) = Signature.size s
end

type signature = Script_signature.t

(* TODO: https://gitlab.com/tezos/tezos/-/issues/2466
   The various attributes of this type should be checked with
   appropriate testing. *)
type tx_rollup_l2_address = Tx_rollup_l2_address.Indexable.value

type ('a, 'b) pair = 'a * 'b

(* We cannot call this type "or" as in Michelson because "or" is an
   OCaml keyword. *)
type ('a, 'b) or_ = L of 'a | R of 'b

module Script_chain_id = struct
  type t = Chain_id_tag of Chain_id.t [@@ocaml.unboxed]

  let make x = Chain_id_tag x

  let compare (Chain_id_tag x) (Chain_id_tag y) = Chain_id.compare x y

  let size = Chain_id.size

  let encoding =
    Data_encoding.conv (fun (Chain_id_tag x) -> x) make Chain_id.encoding

  let to_b58check (Chain_id_tag x) = Chain_id.to_b58check x

  let of_b58check_opt x = Option.map make (Chain_id.of_b58check_opt x)
end

module Script_bls = struct
  module type S = sig
    type t

    type fr

    val add : t -> t -> t

    val mul : t -> fr -> t

    val negate : t -> t

    val of_bytes_opt : Bytes.t -> t option

    val to_bytes : t -> Bytes.t
  end

  module Fr = struct
    type t = Fr_tag of Bls.Primitive.Fr.t [@@ocaml.unboxed]

    open Bls.Primitive.Fr

    let add (Fr_tag x) (Fr_tag y) = Fr_tag (add x y)

    let mul (Fr_tag x) (Fr_tag y) = Fr_tag (mul x y)

    let negate (Fr_tag x) = Fr_tag (negate x)

    let of_bytes_opt bytes = Option.map (fun x -> Fr_tag x) (of_bytes_opt bytes)

    let to_bytes (Fr_tag x) = to_bytes x

    let of_z z = Fr_tag (of_z z)

    let to_z (Fr_tag x) = to_z x
  end

  module G1 = struct
    type t = G1_tag of Bls.Primitive.G1.t [@@ocaml.unboxed]

    open Bls.Primitive.G1

    let add (G1_tag x) (G1_tag y) = G1_tag (add x y)

    let mul (G1_tag x) (Fr.Fr_tag y) = G1_tag (mul x y)

    let negate (G1_tag x) = G1_tag (negate x)

    let of_bytes_opt bytes = Option.map (fun x -> G1_tag x) (of_bytes_opt bytes)

    let to_bytes (G1_tag x) = to_bytes x
  end

  module G2 = struct
    type t = G2_tag of Bls.Primitive.G2.t [@@ocaml.unboxed]

    open Bls.Primitive.G2

    let add (G2_tag x) (G2_tag y) = G2_tag (add x y)

    let mul (G2_tag x) (Fr.Fr_tag y) = G2_tag (mul x y)

    let negate (G2_tag x) = G2_tag (negate x)

    let of_bytes_opt bytes = Option.map (fun x -> G2_tag x) (of_bytes_opt bytes)

    let to_bytes (G2_tag x) = to_bytes x
  end

  let pairing_check l =
    let l = List.map (fun (G1.G1_tag x, G2.G2_tag y) -> (x, y)) l in
    Bls.Primitive.pairing_check l
end

module Script_timelock = struct
  type chest_key = Chest_key_tag of Timelock.chest_key [@@ocaml.unboxed]

  let make_chest_key chest_key = Chest_key_tag chest_key

  let chest_key_encoding =
    Data_encoding.conv
      (fun (Chest_key_tag x) -> x)
      (fun x -> Chest_key_tag x)
      Timelock.chest_key_encoding

  type chest = Chest_tag of Timelock.chest [@@ocaml.unboxed]

  let make_chest chest = Chest_tag chest

  let chest_encoding =
    Data_encoding.conv
      (fun (Chest_tag x) -> x)
      (fun x -> Chest_tag x)
      Timelock.chest_encoding

  let open_chest (Chest_tag chest) (Chest_key_tag chest_key) ~time =
    Timelock.open_chest chest chest_key ~time

  let get_plaintext_size (Chest_tag x) = Timelock.get_plaintext_size x
end

type ticket_amount = Ticket_amount.t

type 'a ticket = {ticketer : Contract.t; contents : 'a; amount : ticket_amount}

module type TYPE_SIZE = sig
  (* A type size represents the size of its type parameter.
     This constraint is enforced inside this module (Script_typed_ir), hence there
     should be no way to construct a type size outside of it.

     It allows keeping type metadata and types non-private.

     The size of a type is the number of nodes in its AST
     representation. In other words, the size of a type is 1 plus the size of
     its arguments. For instance, the size of [Unit] is 1 and the size of
     [Pair ty1 ty2] is [1] plus the size of [ty1] and [ty2].

     This module is here because we want three levels of visibility over this
     code:
     - inside this submodule, we have [type 'a t = int]
     - outside of [Script_typed_ir], the ['a t] type is abstract and we have
        the invariant that whenever [x : 'a t] we have that [x] is exactly
        the size of ['a].
     - in-between (inside [Script_typed_ir] but outside the [Type_size]
        submodule), the type is abstract but we have access to unsafe
        constructors that can break the invariant.
  *)
  type 'a t

  val check_eq :
    error_details:('error_context, 'error_trace) Script_tc_errors.error_details ->
    'a t ->
    'b t ->
    (unit, 'error_trace) result

  val to_int : 'a t -> Saturation_repr.mul_safe Saturation_repr.t

  (* Unsafe constructors, to be used only safely and inside this module *)

  val one : _ t

  val compound1 : Script.location -> _ t -> _ t tzresult

  val compound2 : Script.location -> _ t -> _ t -> _ t tzresult
end

module Type_size : TYPE_SIZE = struct
  type 'a t = int

  let () =
    (* static-like check that all [t] values fit in a [mul_safe] *)
    let (_ : Saturation_repr.mul_safe Saturation_repr.t) =
      Saturation_repr.mul_safe_of_int_exn Constants.michelson_maximum_type_size
    in
    ()

  let to_int = Saturation_repr.mul_safe_of_int_exn

  let one = 1

  let check_eq :
      type a b error_trace.
      error_details:(_, error_trace) Script_tc_errors.error_details ->
      a t ->
      b t ->
      (unit, error_trace) result =
   fun ~error_details x y ->
    if Compare.Int.(x = y) then Result.return_unit
    else
      Error
        (match error_details with
        | Fast -> Inconsistent_types_fast
        | Informative _ ->
            trace_of_error @@ Script_tc_errors.Inconsistent_type_sizes (x, y))

  let of_int loc size =
    let max_size = Constants.michelson_maximum_type_size in
    if Compare.Int.(size <= max_size) then ok size
    else error (Script_tc_errors.Type_too_large (loc, max_size))

  let compound1 loc size = of_int loc (1 + size)

  let compound2 loc size1 size2 = of_int loc (1 + size1 + size2)
end

type empty_cell = EmptyCell

type end_of_stack = empty_cell * empty_cell

type 'a ty_metadata = {size : 'a Type_size.t} [@@unboxed]

(*

   This signature contains the exact set of functions used in the
   protocol. We do not include all [Set.S] because this would
   increase the size of the first class modules used to represent
   [boxed_set].

   Warning: for any change in this signature, there must be a
   change in [Script_typed_ir_size.value_size] which updates
   [boxing_space] in the case for sets.

*)
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

type 'elt set = Set_tag of (module Boxed_set with type elt = 'elt)
[@@ocaml.unboxed]

(*

   Same remark as for [Boxed_set_OPS]. (See below.)

*)
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

type ('key, 'value) map =
  | Map_tag of (module Boxed_map with type key = 'key and type value = 'value)
[@@ocaml.unboxed]

module Big_map_overlay = Map.Make (struct
  type t = Script_expr_hash.t

  let compare = Script_expr_hash.compare
end)

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

let no_entrypoints = {at_node = None; nested = Entrypoints_None}

type logging_event = LogEntry | LogExit of Script.location

type 'arg entrypoints = {
  root : 'arg entrypoints_node;
  original_type_expr : Script.node;
}

(* ---- Instructions --------------------------------------------------------*)
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
  (*
     Pairs
     -----
  *)
  | ICons_pair :
      Script.location * ('a * 'b, 'c * 'S, 'r, 'F) kinstr
      -> ('a, 'b * ('c * 'S), 'r, 'F) kinstr
  | ICar :
      Script.location * ('a, 'S, 'r, 'F) kinstr
      -> ('a * 'b, 'S, 'r, 'F) kinstr
  | ICdr :
      Script.location * ('b, 'S, 'r, 'F) kinstr
      -> ('a * 'b, 'S, 'r, 'F) kinstr
  | IUnpair :
      Script.location * ('a, 'b * 'S, 'r, 'F) kinstr
      -> ('a * 'b, 'S, 'r, 'F) kinstr
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
      * ('a * 'b, 'd * 'S, 'c, 'd * 'S) kinstr
      * (('a, 'c) map, 'd * 'S, 'r, 'F) kinstr
      -> (('a, 'b) map, 'd * 'S, 'r, 'F) kinstr
  | IMap_iter :
      Script.location
      * ('a * 'b, _) ty option
      * ('a * 'b, 'c * 'S, 'c, 'S) kinstr
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
  (* Even though `IAnd_nat` and `IAnd_int_nat` could be merged into a single
     instruction from both the type and behavior point of views, their gas costs
     differ too much (see `cost_N_IAnd_nat` and `cost_N_IAnd_int_nat` in
     `Michelson_v1_gas.Cost_of.Generated_costs`), so we keep them separated. *)
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
      -> ('a, ('a * 'b, 'c) lambda * 'S, 'r, 'F) kinstr
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
      Script.location * ((z num, Sapling.state) pair option, 'S, 'r, 'F) kinstr
      -> (Sapling.Legacy.transaction, Sapling.state * 'S, 'r, 'F) kinstr
  | IDig :
      Script.location
      * int
      * ('b, 'c * 'T, 'c, 'T, 'a, 'S, 'd, 'U) stack_prefix_preservation_witness
      * ('b, 'd * 'U, 'r, 'F) kinstr
      -> ('a, 'S, 'r, 'F) kinstr
  | IDug :
      Script.location
      * int
      * ('c, 'T, 'a, 'c * 'T, 'b, 'S, 'd, 'U) stack_prefix_preservation_witness
      * ('d, 'U, 'r, 'F) kinstr
      -> ('a, 'b * 'S, 'r, 'F) kinstr
  | IDipn :
      Script.location
      * int
      * ('c, 'T, 'd, 'V, 'a, 'S, 'b, 'U) stack_prefix_preservation_witness
      * ('c, 'T, 'd, 'V) kinstr
      * ('b, 'U, 'r, 'F) kinstr
      -> ('a, 'S, 'r, 'F) kinstr
  | IDropn :
      Script.location
      * int
      * ('b, 'U, 'b, 'U, 'a, 'S, 'a, 'S) stack_prefix_preservation_witness
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
      * ('T, 'v) comb_get_gadt_witness
      * ('v, 'a * 'S, 'r, 'F) kinstr
      -> ('T, 'a * 'S, 'r, 'F) kinstr
  | IComb_set :
      Script.location
      * int
      * ('a, 'b, 'c) comb_set_gadt_witness
      * ('c, 'd * 'S, 'r, 'F) kinstr
      -> ('a, 'b * ('d * 'S), 'r, 'F) kinstr
  | IDup_n :
      Script.location
      * int
      * ('a, 'b, 'S, 'T) dup_n_gadt_witness
      * ('T, 'a * ('b * 'S), 'r, 'F) kinstr
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
      * (address * ('a * n num), 'a ticket * 'S, 'r, 'F) kinstr
      -> ('a ticket, 'S, 'r, 'F) kinstr
  | ISplit_ticket :
      Script.location * (('a ticket * 'a ticket) option, 'S, 'r, 'F) kinstr
      -> ('a ticket, (n num * n num) * 'S, 'r, 'F) kinstr
  | IJoin_tickets :
      Script.location * 'a comparable_ty * ('a ticket option, 'S, 'r, 'F) kinstr
      -> ('a ticket * 'a ticket, 'S, 'r, 'F) kinstr
  | IOpen_chest :
      Script.location * ((bytes, bool) or_, 'S, 'r, 'F) kinstr
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
     -----------------------------
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

and (_, _, _, _) continuation =
  | KNil : ('r, 'F, 'r, 'F) continuation
  | KCons :
      ('a, 'S, 'b, 'T) kinstr * ('b, 'T, 'r, 'F) continuation
      -> ('a, 'S, 'r, 'F) continuation
  | KReturn :
      'S * ('a, 'S) stack_ty option * ('a, 'S, 'r, 'F) continuation
      -> ('a, end_of_stack, 'r, 'F) continuation
  | KMap_head :
      ('a -> 'b) * ('b, 'S, 'r, 'F) continuation
      -> ('a, 'S, 'r, 'F) continuation
  | KUndip :
      'b * ('b, _) ty option * ('b, 'a * 'S, 'r, 'F) continuation
      -> ('a, 'S, 'r, 'F) continuation
  | KLoop_in :
      ('a, 'S, bool, 'a * 'S) kinstr * ('a, 'S, 'r, 'F) continuation
      -> (bool, 'a * 'S, 'r, 'F) continuation
  | KLoop_in_left :
      ('a, 'S, ('a, 'b) or_, 'S) kinstr * ('b, 'S, 'r, 'F) continuation
      -> (('a, 'b) or_, 'S, 'r, 'F) continuation
  | KIter :
      ('a, 'b * 'S, 'b, 'S) kinstr
      * ('a, _) ty option
      * 'a list
      * ('b, 'S, 'r, 'F) continuation
      -> ('b, 'S, 'r, 'F) continuation
  | KList_enter_body :
      ('a, 'c * 'S, 'b, 'c * 'S) kinstr
      * 'a list
      * 'b Script_list.t
      * ('b Script_list.t, _) ty option
      * int
      * ('b Script_list.t, 'c * 'S, 'r, 'F) continuation
      -> ('c, 'S, 'r, 'F) continuation
  | KList_exit_body :
      ('a, 'c * 'S, 'b, 'c * 'S) kinstr
      * 'a list
      * 'b Script_list.t
      * ('b Script_list.t, _) ty option
      * int
      * ('b Script_list.t, 'c * 'S, 'r, 'F) continuation
      -> ('b, 'c * 'S, 'r, 'F) continuation
  | KMap_enter_body :
      ('a * 'b, 'd * 'S, 'c, 'd * 'S) kinstr
      * ('a * 'b) list
      * ('a, 'c) map
      * (('a, 'c) map, _) ty option
      * (('a, 'c) map, 'd * 'S, 'r, 'F) continuation
      -> ('d, 'S, 'r, 'F) continuation
  | KMap_exit_body :
      ('a * 'b, 'd * 'S, 'c, 'd * 'S) kinstr
      * ('a * 'b) list
      * ('a, 'c) map
      * 'a
      * (('a, 'c) map, _) ty option
      * (('a, 'c) map, 'd * 'S, 'r, 'F) continuation
      -> ('c, 'd * 'S, 'r, 'F) continuation
  | KView_exit :
      step_constants * ('a, 'S, 'r, 'F) continuation
      -> ('a, 'S, 'r, 'F) continuation
  | KLog :
      ('a, 'S, 'r, 'F) continuation * ('a, 'S) stack_ty * logger
      -> ('a, 'S, 'r, 'F) continuation

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
  get_log : unit -> execution_trace option tzresult Lwt.t;
  klog : 'a 'S 'r 'F. ('a, 'S, 'r, 'F) klog;
  ilog : 'a 'S 'b 'T 'r 'F. ('a, 'S, 'b, 'T, 'r, 'F) ilog;
  log_kinstr : 'a 'b 'c 'd. ('a, 'b, 'c, 'd) log_kinstr;
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
  | Tx_rollup_l2_address_t : (tx_rollup_l2_address, yes) ty
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
      -> ('a * 'b, 'c, 'S, 'a, 'd, 'e * 'T) uncomb_gadt_witness

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
      amount : Tez.tez;
    }
      -> Kind.transaction internal_operation_contents
  | Transaction_to_implicit_with_ticket : {
      destination : Signature.Public_key_hash.t;
      ticket_ty : ('content ticket, _) ty;
      ticket : 'content ticket;
      unparsed_ticket : Script.lazy_expr;
      amount : Tez.tez;
    }
      -> Kind.transaction internal_operation_contents
  | Transaction_to_smart_contract : {
      destination : Contract_hash.t;
      amount : Tez.tez;
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
      credit : Tez.tez;
      preorigination : Contract_hash.t;
      storage_type : ('storage, _) ty;
      storage : 'storage;
    }
      -> Kind.origination internal_operation_contents
  | Delegation :
      Signature.Public_key_hash.t option
      -> Kind.delegation internal_operation_contents

and 'kind internal_operation = {
  source : Destination.t;
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

type ('arg, 'storage) script =
  | Script : {
      code :
        (('arg, 'storage) pair, (operation Script_list.t, 'storage) pair) lambda;
      arg_type : ('arg, _) ty;
      storage : 'storage;
      storage_type : ('storage, _) ty;
      views : view_map;
      entrypoints : 'arg entrypoints;
      code_size : Cache_memory_helpers.sint;
          (* This is an over-approximation of the value size in memory, in
             bytes, of the contract's static part, that is its source
             code. This includes the code of the contract as well as the code
             of the views. The storage size is not taken into account by this
             field as it has a dynamic size. *)
    }
      -> ('arg, 'storage) script

let manager_kind :
    type kind. kind internal_operation_contents -> kind Kind.manager = function
  | Transaction_to_implicit _ -> Kind.Transaction_manager_kind
  | Transaction_to_implicit_with_ticket _ -> Kind.Transaction_manager_kind
  | Transaction_to_smart_contract _ -> Kind.Transaction_manager_kind
  | Transaction_to_sc_rollup _ -> Kind.Transaction_manager_kind
  | Transaction_to_zk_rollup _ -> Kind.Transaction_manager_kind
  | Event _ -> Kind.Event_manager_kind
  | Origination _ -> Kind.Origination_manager_kind
  | Delegation _ -> Kind.Delegation_manager_kind

let kinstr_location : type a s b f. (a, s, b, f) kinstr -> Script.location =
 fun i ->
  match i with
  | IDrop (loc, _) -> loc
  | IDup (loc, _) -> loc
  | ISwap (loc, _) -> loc
  | IPush (loc, _, _, _) -> loc
  | ICons_pair (loc, _) -> loc
  | ICar (loc, _) -> loc
  | ICdr (loc, _) -> loc
  | IUnpair (loc, _) -> loc
  | ICons_some (loc, _) -> loc
  | ICons_none (loc, _, _) -> loc
  | IIf_none {loc; _} -> loc
  | IOpt_map {loc; _} -> loc
  | ICons_left (loc, _, _) -> loc
  | ICons_right (loc, _, _) -> loc
  | IIf_left {loc; _} -> loc
  | ICons_list (loc, _) -> loc
  | INil (loc, _, _) -> loc
  | IIf_cons {loc; _} -> loc
  | IList_map (loc, _, _, _) -> loc
  | IList_iter (loc, _, _, _) -> loc
  | IList_size (loc, _) -> loc
  | IEmpty_set (loc, _, _) -> loc
  | ISet_iter (loc, _, _, _) -> loc
  | ISet_mem (loc, _) -> loc
  | ISet_update (loc, _) -> loc
  | ISet_size (loc, _) -> loc
  | IEmpty_map (loc, _, _, _) -> loc
  | IMap_map (loc, _, _, _) -> loc
  | IMap_iter (loc, _, _, _) -> loc
  | IMap_mem (loc, _) -> loc
  | IMap_get (loc, _) -> loc
  | IMap_update (loc, _) -> loc
  | IMap_get_and_update (loc, _) -> loc
  | IMap_size (loc, _) -> loc
  | IEmpty_big_map (loc, _, _, _) -> loc
  | IBig_map_mem (loc, _) -> loc
  | IBig_map_get (loc, _) -> loc
  | IBig_map_update (loc, _) -> loc
  | IBig_map_get_and_update (loc, _) -> loc
  | IConcat_string (loc, _) -> loc
  | IConcat_string_pair (loc, _) -> loc
  | ISlice_string (loc, _) -> loc
  | IString_size (loc, _) -> loc
  | IConcat_bytes (loc, _) -> loc
  | IConcat_bytes_pair (loc, _) -> loc
  | ISlice_bytes (loc, _) -> loc
  | IBytes_size (loc, _) -> loc
  | ILsl_bytes (loc, _) -> loc
  | ILsr_bytes (loc, _) -> loc
  | IOr_bytes (loc, _) -> loc
  | IAnd_bytes (loc, _) -> loc
  | IXor_bytes (loc, _) -> loc
  | INot_bytes (loc, _) -> loc
  | INat_bytes (loc, _) -> loc
  | IBytes_nat (loc, _) -> loc
  | IInt_bytes (loc, _) -> loc
  | IBytes_int (loc, _) -> loc
  | IAdd_seconds_to_timestamp (loc, _) -> loc
  | IAdd_timestamp_to_seconds (loc, _) -> loc
  | ISub_timestamp_seconds (loc, _) -> loc
  | IDiff_timestamps (loc, _) -> loc
  | IAdd_tez (loc, _) -> loc
  | ISub_tez (loc, _) -> loc
  | ISub_tez_legacy (loc, _) -> loc
  | IMul_teznat (loc, _) -> loc
  | IMul_nattez (loc, _) -> loc
  | IEdiv_teznat (loc, _) -> loc
  | IEdiv_tez (loc, _) -> loc
  | IOr (loc, _) -> loc
  | IAnd (loc, _) -> loc
  | IXor (loc, _) -> loc
  | INot (loc, _) -> loc
  | IIs_nat (loc, _) -> loc
  | INeg (loc, _) -> loc
  | IAbs_int (loc, _) -> loc
  | IInt_nat (loc, _) -> loc
  | IAdd_int (loc, _) -> loc
  | IAdd_nat (loc, _) -> loc
  | ISub_int (loc, _) -> loc
  | IMul_int (loc, _) -> loc
  | IMul_nat (loc, _) -> loc
  | IEdiv_int (loc, _) -> loc
  | IEdiv_nat (loc, _) -> loc
  | ILsl_nat (loc, _) -> loc
  | ILsr_nat (loc, _) -> loc
  | IOr_nat (loc, _) -> loc
  | IAnd_nat (loc, _) -> loc
  | IAnd_int_nat (loc, _) -> loc
  | IXor_nat (loc, _) -> loc
  | INot_int (loc, _) -> loc
  | IIf {loc; _} -> loc
  | ILoop (loc, _, _) -> loc
  | ILoop_left (loc, _, _) -> loc
  | IDip (loc, _, _, _) -> loc
  | IExec (loc, _, _) -> loc
  | IApply (loc, _, _) -> loc
  | ILambda (loc, _, _) -> loc
  | IFailwith (loc, _) -> loc
  | ICompare (loc, _, _) -> loc
  | IEq (loc, _) -> loc
  | INeq (loc, _) -> loc
  | ILt (loc, _) -> loc
  | IGt (loc, _) -> loc
  | ILe (loc, _) -> loc
  | IGe (loc, _) -> loc
  | IAddress (loc, _) -> loc
  | IContract (loc, _, _, _) -> loc
  | ITransfer_tokens (loc, _) -> loc
  | IView (loc, _, _, _) -> loc
  | IImplicit_account (loc, _) -> loc
  | ICreate_contract {loc; _} -> loc
  | ISet_delegate (loc, _) -> loc
  | INow (loc, _) -> loc
  | IMin_block_time (loc, _) -> loc
  | IBalance (loc, _) -> loc
  | ILevel (loc, _) -> loc
  | ICheck_signature (loc, _) -> loc
  | IHash_key (loc, _) -> loc
  | IPack (loc, _, _) -> loc
  | IUnpack (loc, _, _) -> loc
  | IBlake2b (loc, _) -> loc
  | ISha256 (loc, _) -> loc
  | ISha512 (loc, _) -> loc
  | ISource (loc, _) -> loc
  | ISender (loc, _) -> loc
  | ISelf (loc, _, _, _) -> loc
  | ISelf_address (loc, _) -> loc
  | IAmount (loc, _) -> loc
  | ISapling_empty_state (loc, _, _) -> loc
  | ISapling_verify_update (loc, _) -> loc
  | ISapling_verify_update_deprecated (loc, _) -> loc
  | IDig (loc, _, _, _) -> loc
  | IDug (loc, _, _, _) -> loc
  | IDipn (loc, _, _, _, _) -> loc
  | IDropn (loc, _, _, _) -> loc
  | IChainId (loc, _) -> loc
  | INever loc -> loc
  | IVoting_power (loc, _) -> loc
  | ITotal_voting_power (loc, _) -> loc
  | IKeccak (loc, _) -> loc
  | ISha3 (loc, _) -> loc
  | IAdd_bls12_381_g1 (loc, _) -> loc
  | IAdd_bls12_381_g2 (loc, _) -> loc
  | IAdd_bls12_381_fr (loc, _) -> loc
  | IMul_bls12_381_g1 (loc, _) -> loc
  | IMul_bls12_381_g2 (loc, _) -> loc
  | IMul_bls12_381_fr (loc, _) -> loc
  | IMul_bls12_381_z_fr (loc, _) -> loc
  | IMul_bls12_381_fr_z (loc, _) -> loc
  | IInt_bls12_381_fr (loc, _) -> loc
  | INeg_bls12_381_g1 (loc, _) -> loc
  | INeg_bls12_381_g2 (loc, _) -> loc
  | INeg_bls12_381_fr (loc, _) -> loc
  | IPairing_check_bls12_381 (loc, _) -> loc
  | IComb (loc, _, _, _) -> loc
  | IUncomb (loc, _, _, _) -> loc
  | IComb_get (loc, _, _, _) -> loc
  | IComb_set (loc, _, _, _) -> loc
  | IDup_n (loc, _, _, _) -> loc
  | ITicket (loc, _, _) -> loc
  | ITicket_deprecated (loc, _, _) -> loc
  | IRead_ticket (loc, _, _) -> loc
  | ISplit_ticket (loc, _) -> loc
  | IJoin_tickets (loc, _, _) -> loc
  | IOpen_chest (loc, _) -> loc
  | IEmit {loc; _} -> loc
  | IHalt loc -> loc
  | ILog (loc, _, _, _, _) -> loc

let meta_basic = {size = Type_size.one}

let meta_compound1 loc ({size} : _ ty_metadata) : _ ty_metadata tzresult =
  Type_size.compound1 loc size >|? fun size -> {size}

let meta_compound2 loc ({size = size1} : _ ty_metadata)
    ({size = size2} : _ ty_metadata) : _ ty_metadata tzresult =
  Type_size.compound2 loc size1 size2 >|? fun size -> {size}

let unit_metadata : unit ty_metadata = meta_basic

let never_metadata : never ty_metadata = meta_basic

let int_metadata : z num ty_metadata = meta_basic

let nat_metadata : n num ty_metadata = meta_basic

let signature_metadata : signature ty_metadata = meta_basic

let string_metadata : Script_string.t ty_metadata = meta_basic

let bytes_metadata : bytes ty_metadata = meta_basic

let mutez_metadata : Tez.t ty_metadata = meta_basic

let bool_metadata : bool ty_metadata = meta_basic

let key_hash_metadata : public_key_hash ty_metadata = meta_basic

let key_metadata : public_key ty_metadata = meta_basic

let timestamp_metadata : Script_timestamp.t ty_metadata = meta_basic

let chain_id_metadata : Script_chain_id.t ty_metadata = meta_basic

let address_metadata : address ty_metadata = meta_basic

let tx_rollup_l2_address_metadata : tx_rollup_l2_address ty_metadata =
  meta_basic

let sapling_transaction_metadata : Sapling.transaction ty_metadata = meta_basic

let sapling_transaction_deprecated_metadata :
    Sapling.Legacy.transaction ty_metadata =
  meta_basic

let sapling_state_metadata : Sapling.state ty_metadata = meta_basic

let operation_metadata : operation ty_metadata = meta_basic

let bls12_381_g1_metadata : Script_bls.G1.t ty_metadata = meta_basic

let bls12_381_g2_metadata : Script_bls.G2.t ty_metadata = meta_basic

let bls12_381_fr_metadata : Script_bls.Fr.t ty_metadata = meta_basic

let chest_metadata : Script_timelock.chest ty_metadata = meta_basic

let chest_key_metadata : Script_timelock.chest_key ty_metadata = meta_basic

let pair_metadata :
    Script.location ->
    'a ty_metadata ->
    'b ty_metadata ->
    ('a, 'b) pair ty_metadata tzresult =
  meta_compound2

let or_metadata :
    Script.location ->
    'a ty_metadata ->
    'b ty_metadata ->
    ('a, 'b) or_ ty_metadata tzresult =
  meta_compound2

let lambda_metadata :
    Script.location ->
    'a ty_metadata ->
    'b ty_metadata ->
    ('a, 'b) lambda ty_metadata tzresult =
  meta_compound2

let option_metadata :
    Script.location -> 'a ty_metadata -> 'a option ty_metadata tzresult =
  meta_compound1

let list_metadata :
    Script.location -> 'a ty_metadata -> 'a Script_list.t ty_metadata tzresult =
  meta_compound1

let set_metadata :
    Script.location -> 'a ty_metadata -> 'a set ty_metadata tzresult =
  meta_compound1

let map_metadata :
    Script.location ->
    'a ty_metadata ->
    'b ty_metadata ->
    ('a, 'b) map ty_metadata tzresult =
  meta_compound2

let big_map_metadata :
    Script.location ->
    'a ty_metadata ->
    'b ty_metadata ->
    ('a, 'b) big_map ty_metadata tzresult =
  meta_compound2

let contract_metadata :
    Script.location -> 'a ty_metadata -> 'a typed_contract ty_metadata tzresult
    =
  meta_compound1

let ticket_metadata :
    Script.location -> 'a ty_metadata -> 'a ticket ty_metadata tzresult =
  meta_compound1

let ty_metadata : type a ac. (a, ac) ty -> a ty_metadata = function
  | Unit_t -> unit_metadata
  | Never_t -> never_metadata
  | Int_t -> int_metadata
  | Nat_t -> nat_metadata
  | Signature_t -> signature_metadata
  | String_t -> string_metadata
  | Bytes_t -> bytes_metadata
  | Mutez_t -> mutez_metadata
  | Bool_t -> bool_metadata
  | Key_hash_t -> key_hash_metadata
  | Key_t -> key_metadata
  | Timestamp_t -> timestamp_metadata
  | Chain_id_t -> chain_id_metadata
  | Address_t -> address_metadata
  | Tx_rollup_l2_address_t -> tx_rollup_l2_address_metadata
  | Pair_t (_, _, meta, _) -> meta
  | Or_t (_, _, meta, _) -> meta
  | Option_t (_, meta, _) -> meta
  | Lambda_t (_, _, meta) -> meta
  | List_t (_, meta) -> meta
  | Set_t (_, meta) -> meta
  | Map_t (_, _, meta) -> meta
  | Big_map_t (_, _, meta) -> meta
  | Ticket_t (_, meta) -> meta
  | Contract_t (_, meta) -> meta
  | Sapling_transaction_t _ -> sapling_transaction_metadata
  | Sapling_transaction_deprecated_t _ ->
      sapling_transaction_deprecated_metadata
  | Sapling_state_t _ -> sapling_state_metadata
  | Operation_t -> operation_metadata
  | Bls12_381_g1_t -> bls12_381_g1_metadata
  | Bls12_381_g2_t -> bls12_381_g2_metadata
  | Bls12_381_fr_t -> bls12_381_fr_metadata
  | Chest_t -> chest_metadata
  | Chest_key_t -> chest_key_metadata

let ty_size t = (ty_metadata t).size

let is_comparable : type v c. (v, c) ty -> c dbool = function
  | Never_t -> Yes
  | Unit_t -> Yes
  | Int_t -> Yes
  | Nat_t -> Yes
  | Signature_t -> Yes
  | String_t -> Yes
  | Bytes_t -> Yes
  | Mutez_t -> Yes
  | Bool_t -> Yes
  | Key_hash_t -> Yes
  | Key_t -> Yes
  | Timestamp_t -> Yes
  | Chain_id_t -> Yes
  | Address_t -> Yes
  | Tx_rollup_l2_address_t -> Yes
  | Pair_t (_, _, _, dand) -> dbool_of_dand dand
  | Or_t (_, _, _, dand) -> dbool_of_dand dand
  | Option_t (_, _, cmp) -> cmp
  | Lambda_t _ -> No
  | List_t _ -> No
  | Set_t _ -> No
  | Map_t _ -> No
  | Big_map_t _ -> No
  | Ticket_t _ -> No
  | Contract_t _ -> No
  | Sapling_transaction_t _ -> No
  | Sapling_transaction_deprecated_t _ -> No
  | Sapling_state_t _ -> No
  | Operation_t -> No
  | Bls12_381_g1_t -> No
  | Bls12_381_g2_t -> No
  | Bls12_381_fr_t -> No
  | Chest_t -> No
  | Chest_key_t -> No

type 'v ty_ex_c = Ty_ex_c : ('v, _) ty -> 'v ty_ex_c [@@ocaml.unboxed]

let assert_ok1 f x =
  match f Micheline.dummy_location x with
  | Ok res -> res
  | Error _ -> assert false

let assert_ok2 f x y =
  match f Micheline.dummy_location x y with
  | Ok res -> res
  | Error _ -> assert false

let unit_t = Unit_t

let int_t = Int_t

let nat_t = Nat_t

let signature_t = Signature_t

let string_t = String_t

let bytes_t = Bytes_t

let mutez_t = Mutez_t

let key_hash_t = Key_hash_t

let key_t = Key_t

let timestamp_t = Timestamp_t

let address_t = Address_t

let bool_t = Bool_t

let tx_rollup_l2_address_t = Tx_rollup_l2_address_t

let pair_t :
    type a ac b bc.
    Script.location -> (a, ac) ty -> (b, bc) ty -> (a, b) pair ty_ex_c tzresult
    =
 fun loc l r ->
  pair_metadata loc (ty_metadata l) (ty_metadata r) >|? fun metadata ->
  let (Ex_dand cmp) = dand (is_comparable l) (is_comparable r) in
  Ty_ex_c (Pair_t (l, r, metadata, cmp))

let pair_3_t loc l m r = pair_t loc m r >>? fun (Ty_ex_c r) -> pair_t loc l r

let comparable_pair_t loc l r =
  pair_metadata loc (ty_metadata l) (ty_metadata r) >|? fun metadata ->
  Pair_t (l, r, metadata, YesYes)

let comparable_pair_3_t loc l m r =
  comparable_pair_t loc m r >>? fun r -> comparable_pair_t loc l r

let or_t :
    type a ac b bc.
    Script.location -> (a, ac) ty -> (b, bc) ty -> (a, b) or_ ty_ex_c tzresult =
 fun loc l r ->
  or_metadata loc (ty_metadata l) (ty_metadata r) >|? fun metadata ->
  let (Ex_dand cmp) = dand (is_comparable l) (is_comparable r) in
  Ty_ex_c (Or_t (l, r, metadata, cmp))

let or_bytes_bool_t =
  Or_t
    ( bytes_t,
      bool_t,
      assert_ok2 or_metadata bytes_metadata bool_metadata,
      YesYes )

let comparable_or_t loc l r =
  or_metadata loc (ty_metadata l) (ty_metadata r) >|? fun metadata ->
  Or_t (l, r, metadata, YesYes)

let lambda_t loc l r =
  lambda_metadata loc (ty_metadata l) (ty_metadata r) >|? fun metadata ->
  Lambda_t (l, r, metadata)

let option_t loc t =
  option_metadata loc (ty_metadata t) >|? fun metadata ->
  let cmp = is_comparable t in
  Option_t (t, metadata, cmp)

let option_mutez_t =
  Option_t (mutez_t, assert_ok1 option_metadata mutez_metadata, Yes)

let option_string_t =
  Option_t (string_t, assert_ok1 option_metadata string_metadata, Yes)

let option_bytes_t =
  Option_t (bytes_t, assert_ok1 option_metadata bytes_metadata, Yes)

let option_nat_t = Option_t (nat_t, assert_ok1 option_metadata nat_metadata, Yes)

let option_pair_nat_nat_t =
  let pmetadata = assert_ok2 pair_metadata nat_metadata nat_metadata in
  let ometadata = assert_ok1 option_metadata pmetadata in
  Option_t (Pair_t (nat_t, nat_t, pmetadata, YesYes), ometadata, Yes)

let option_pair_nat_mutez_t =
  let pmetadata = assert_ok2 pair_metadata nat_metadata mutez_metadata in
  let ometadata = assert_ok1 option_metadata pmetadata in
  Option_t (Pair_t (nat_t, mutez_t, pmetadata, YesYes), ometadata, Yes)

let option_pair_mutez_mutez_t =
  let pmetadata = assert_ok2 pair_metadata mutez_metadata mutez_metadata in
  let ometadata = assert_ok1 option_metadata pmetadata in
  Option_t (Pair_t (mutez_t, mutez_t, pmetadata, YesYes), ometadata, Yes)

let option_pair_int_nat_t =
  let pmetadata = assert_ok2 pair_metadata int_metadata nat_metadata in
  let ometadata = assert_ok1 option_metadata pmetadata in
  Option_t (Pair_t (int_t, nat_t, pmetadata, YesYes), ometadata, Yes)

let list_t loc t =
  list_metadata loc (ty_metadata t) >|? fun metadata -> List_t (t, metadata)

let operation_t = Operation_t

let list_operation_t =
  List_t (operation_t, assert_ok1 list_metadata operation_metadata)

let set_t loc t =
  set_metadata loc (ty_metadata t) >|? fun metadata -> Set_t (t, metadata)

let map_t loc l r =
  map_metadata loc (ty_metadata l) (ty_metadata r) >|? fun metadata ->
  Map_t (l, r, metadata)

let big_map_t loc l r =
  big_map_metadata loc (ty_metadata l) (ty_metadata r) >|? fun metadata ->
  Big_map_t (l, r, metadata)

let contract_t loc t =
  contract_metadata loc (ty_metadata t) >|? fun metadata ->
  Contract_t (t, metadata)

let contract_unit_t =
  Contract_t (unit_t, assert_ok1 contract_metadata unit_metadata)

let sapling_transaction_t ~memo_size = Sapling_transaction_t memo_size

let sapling_transaction_deprecated_t ~memo_size =
  Sapling_transaction_deprecated_t memo_size

let sapling_state_t ~memo_size = Sapling_state_t memo_size

let chain_id_t = Chain_id_t

let never_t = Never_t

let bls12_381_g1_t = Bls12_381_g1_t

let bls12_381_g2_t = Bls12_381_g2_t

let bls12_381_fr_t = Bls12_381_fr_t

let ticket_t loc t =
  ticket_metadata loc (ty_metadata t) >|? fun metadata -> Ticket_t (t, metadata)

let chest_key_t = Chest_key_t

let chest_t = Chest_t

type 'a kinstr_traverse = {
  apply : 'b 'S 'r 'F. 'a -> ('b, 'S, 'r, 'F) kinstr -> 'a;
}

let kinstr_traverse i init f =
  let rec aux :
      type ret a s r f. 'accu -> (a, s, r, f) kinstr -> ('accu -> ret) -> ret =
   fun accu t continue ->
    let accu = f.apply accu t in
    let next k =
      (aux [@ocaml.tailcall]) accu k (fun accu ->
          (continue [@ocaml.tailcall]) accu)
    in
    let next2 k1 k2 =
      (aux [@ocaml.tailcall]) accu k1 (fun accu ->
          (aux [@ocaml.tailcall]) accu k2 (fun accu ->
              (continue [@ocaml.tailcall]) accu))
    in
    let next3 k1 k2 k3 =
      (aux [@ocaml.tailcall]) accu k1 (fun accu ->
          (aux [@ocaml.tailcall]) accu k2 (fun accu ->
              (aux [@ocaml.tailcall]) accu k3 (fun accu ->
                  (continue [@ocaml.tailcall]) accu)))
    in
    let return () = (continue [@ocaml.tailcall]) accu in
    match t with
    | IDrop (_, k) -> (next [@ocaml.tailcall]) k
    | IDup (_, k) -> (next [@ocaml.tailcall]) k
    | ISwap (_, k) -> (next [@ocaml.tailcall]) k
    | IPush (_, _, _, k) -> (next [@ocaml.tailcall]) k
    | ICons_pair (_, k) -> (next [@ocaml.tailcall]) k
    | ICar (_, k) -> (next [@ocaml.tailcall]) k
    | ICdr (_, k) -> (next [@ocaml.tailcall]) k
    | IUnpair (_, k) -> (next [@ocaml.tailcall]) k
    | ICons_some (_, k) -> (next [@ocaml.tailcall]) k
    | ICons_none (_, _, k) -> (next [@ocaml.tailcall]) k
    | IIf_none {loc = _; branch_if_none = k1; branch_if_some = k2; k} ->
        (next3 [@ocaml.tailcall]) k1 k2 k
    | IOpt_map {loc = _; body; k} -> (next2 [@ocaml.tailcall]) body k
    | ICons_left (_, _, k) -> (next [@ocaml.tailcall]) k
    | ICons_right (_, _, k) -> (next [@ocaml.tailcall]) k
    | IIf_left {loc = _; branch_if_left = k1; branch_if_right = k2; k} ->
        (next3 [@ocaml.tailcall]) k1 k2 k
    | ICons_list (_, k) -> (next [@ocaml.tailcall]) k
    | INil (_, _, k) -> (next [@ocaml.tailcall]) k
    | IIf_cons {loc = _; branch_if_nil = k1; branch_if_cons = k2; k} ->
        (next3 [@ocaml.tailcall]) k1 k2 k
    | IList_map (_, k1, _, k2) -> (next2 [@ocaml.tailcall]) k1 k2
    | IList_iter (_, _, k1, k2) -> (next2 [@ocaml.tailcall]) k1 k2
    | IList_size (_, k) -> (next [@ocaml.tailcall]) k
    | IEmpty_set (_, _, k) -> (next [@ocaml.tailcall]) k
    | ISet_iter (_, _, k1, k2) -> (next2 [@ocaml.tailcall]) k1 k2
    | ISet_mem (_, k) -> (next [@ocaml.tailcall]) k
    | ISet_update (_, k) -> (next [@ocaml.tailcall]) k
    | ISet_size (_, k) -> (next [@ocaml.tailcall]) k
    | IEmpty_map (_, _, _, k) -> (next [@ocaml.tailcall]) k
    | IMap_map (_, _, k1, k2) -> (next2 [@ocaml.tailcall]) k1 k2
    | IMap_iter (_, _, k1, k2) -> (next2 [@ocaml.tailcall]) k1 k2
    | IMap_mem (_, k) -> (next [@ocaml.tailcall]) k
    | IMap_get (_, k) -> (next [@ocaml.tailcall]) k
    | IMap_update (_, k) -> (next [@ocaml.tailcall]) k
    | IMap_get_and_update (_, k) -> (next [@ocaml.tailcall]) k
    | IMap_size (_, k) -> (next [@ocaml.tailcall]) k
    | IEmpty_big_map (_, _, _, k) -> (next [@ocaml.tailcall]) k
    | IBig_map_mem (_, k) -> (next [@ocaml.tailcall]) k
    | IBig_map_get (_, k) -> (next [@ocaml.tailcall]) k
    | IBig_map_update (_, k) -> (next [@ocaml.tailcall]) k
    | IBig_map_get_and_update (_, k) -> (next [@ocaml.tailcall]) k
    | IConcat_string (_, k) -> (next [@ocaml.tailcall]) k
    | IConcat_string_pair (_, k) -> (next [@ocaml.tailcall]) k
    | ISlice_string (_, k) -> (next [@ocaml.tailcall]) k
    | IString_size (_, k) -> (next [@ocaml.tailcall]) k
    | IConcat_bytes (_, k) -> (next [@ocaml.tailcall]) k
    | IConcat_bytes_pair (_, k) -> (next [@ocaml.tailcall]) k
    | ISlice_bytes (_, k) -> (next [@ocaml.tailcall]) k
    | IBytes_size (_, k) -> (next [@ocaml.tailcall]) k
    | ILsl_bytes (_, k) -> (next [@ocaml.tailcall]) k
    | ILsr_bytes (_, k) -> (next [@ocaml.tailcall]) k
    | IOr_bytes (_, k) -> (next [@ocaml.tailcall]) k
    | IAnd_bytes (_, k) -> (next [@ocaml.tailcall]) k
    | IXor_bytes (_, k) -> (next [@ocaml.tailcall]) k
    | INot_bytes (_, k) -> (next [@ocaml.tailcall]) k
    | INat_bytes (_, k) -> (next [@ocaml.tailcall]) k
    | IBytes_nat (_, k) -> (next [@ocaml.tailcall]) k
    | IInt_bytes (_, k) -> (next [@ocaml.tailcall]) k
    | IBytes_int (_, k) -> (next [@ocaml.tailcall]) k
    | IAdd_seconds_to_timestamp (_, k) -> (next [@ocaml.tailcall]) k
    | IAdd_timestamp_to_seconds (_, k) -> (next [@ocaml.tailcall]) k
    | ISub_timestamp_seconds (_, k) -> (next [@ocaml.tailcall]) k
    | IDiff_timestamps (_, k) -> (next [@ocaml.tailcall]) k
    | IAdd_tez (_, k) -> (next [@ocaml.tailcall]) k
    | ISub_tez (_, k) -> (next [@ocaml.tailcall]) k
    | ISub_tez_legacy (_, k) -> (next [@ocaml.tailcall]) k
    | IMul_teznat (_, k) -> (next [@ocaml.tailcall]) k
    | IMul_nattez (_, k) -> (next [@ocaml.tailcall]) k
    | IEdiv_teznat (_, k) -> (next [@ocaml.tailcall]) k
    | IEdiv_tez (_, k) -> (next [@ocaml.tailcall]) k
    | IOr (_, k) -> (next [@ocaml.tailcall]) k
    | IAnd (_, k) -> (next [@ocaml.tailcall]) k
    | IXor (_, k) -> (next [@ocaml.tailcall]) k
    | INot (_, k) -> (next [@ocaml.tailcall]) k
    | IIs_nat (_, k) -> (next [@ocaml.tailcall]) k
    | INeg (_, k) -> (next [@ocaml.tailcall]) k
    | IAbs_int (_, k) -> (next [@ocaml.tailcall]) k
    | IInt_nat (_, k) -> (next [@ocaml.tailcall]) k
    | IAdd_int (_, k) -> (next [@ocaml.tailcall]) k
    | IAdd_nat (_, k) -> (next [@ocaml.tailcall]) k
    | ISub_int (_, k) -> (next [@ocaml.tailcall]) k
    | IMul_int (_, k) -> (next [@ocaml.tailcall]) k
    | IMul_nat (_, k) -> (next [@ocaml.tailcall]) k
    | IEdiv_int (_, k) -> (next [@ocaml.tailcall]) k
    | IEdiv_nat (_, k) -> (next [@ocaml.tailcall]) k
    | ILsl_nat (_, k) -> (next [@ocaml.tailcall]) k
    | ILsr_nat (_, k) -> (next [@ocaml.tailcall]) k
    | IOr_nat (_, k) -> (next [@ocaml.tailcall]) k
    | IAnd_nat (_, k) -> (next [@ocaml.tailcall]) k
    | IAnd_int_nat (_, k) -> (next [@ocaml.tailcall]) k
    | IXor_nat (_, k) -> (next [@ocaml.tailcall]) k
    | INot_int (_, k) -> (next [@ocaml.tailcall]) k
    | IIf {loc = _; branch_if_true = k1; branch_if_false = k2; k} ->
        (next3 [@ocaml.tailcall]) k1 k2 k
    | ILoop (_, k1, k2) -> (next2 [@ocaml.tailcall]) k1 k2
    | ILoop_left (_, k1, k2) -> (next2 [@ocaml.tailcall]) k1 k2
    | IDip (_, k1, _, k2) -> (next2 [@ocaml.tailcall]) k1 k2
    | IExec (_, _, k) -> (next [@ocaml.tailcall]) k
    | IApply (_, _, k) -> (next [@ocaml.tailcall]) k
    | ILambda (_, _, k) -> (next [@ocaml.tailcall]) k
    | IFailwith (_, _) -> (return [@ocaml.tailcall]) ()
    | ICompare (_, _, k) -> (next [@ocaml.tailcall]) k
    | IEq (_, k) -> (next [@ocaml.tailcall]) k
    | INeq (_, k) -> (next [@ocaml.tailcall]) k
    | ILt (_, k) -> (next [@ocaml.tailcall]) k
    | IGt (_, k) -> (next [@ocaml.tailcall]) k
    | ILe (_, k) -> (next [@ocaml.tailcall]) k
    | IGe (_, k) -> (next [@ocaml.tailcall]) k
    | IAddress (_, k) -> (next [@ocaml.tailcall]) k
    | IContract (_, _, _, k) -> (next [@ocaml.tailcall]) k
    | IView (_, _, _, k) -> (next [@ocaml.tailcall]) k
    | ITransfer_tokens (_, k) -> (next [@ocaml.tailcall]) k
    | IImplicit_account (_, k) -> (next [@ocaml.tailcall]) k
    | ICreate_contract {k; _} -> (next [@ocaml.tailcall]) k
    | ISet_delegate (_, k) -> (next [@ocaml.tailcall]) k
    | INow (_, k) -> (next [@ocaml.tailcall]) k
    | IMin_block_time (_, k) -> (next [@ocaml.tailcall]) k
    | IBalance (_, k) -> (next [@ocaml.tailcall]) k
    | ILevel (_, k) -> (next [@ocaml.tailcall]) k
    | ICheck_signature (_, k) -> (next [@ocaml.tailcall]) k
    | IHash_key (_, k) -> (next [@ocaml.tailcall]) k
    | IPack (_, _, k) -> (next [@ocaml.tailcall]) k
    | IUnpack (_, _, k) -> (next [@ocaml.tailcall]) k
    | IBlake2b (_, k) -> (next [@ocaml.tailcall]) k
    | ISha256 (_, k) -> (next [@ocaml.tailcall]) k
    | ISha512 (_, k) -> (next [@ocaml.tailcall]) k
    | ISource (_, k) -> (next [@ocaml.tailcall]) k
    | ISender (_, k) -> (next [@ocaml.tailcall]) k
    | ISelf (_, _, _, k) -> (next [@ocaml.tailcall]) k
    | ISelf_address (_, k) -> (next [@ocaml.tailcall]) k
    | IAmount (_, k) -> (next [@ocaml.tailcall]) k
    | ISapling_empty_state (_, _, k) -> (next [@ocaml.tailcall]) k
    | ISapling_verify_update (_, k) -> (next [@ocaml.tailcall]) k
    | ISapling_verify_update_deprecated (_, k) -> (next [@ocaml.tailcall]) k
    | IDig (_, _, _, k) -> (next [@ocaml.tailcall]) k
    | IDug (_, _, _, k) -> (next [@ocaml.tailcall]) k
    | IDipn (_, _, _, k1, k2) -> (next2 [@ocaml.tailcall]) k1 k2
    | IDropn (_, _, _, k) -> (next [@ocaml.tailcall]) k
    | IChainId (_, k) -> (next [@ocaml.tailcall]) k
    | INever _ -> (return [@ocaml.tailcall]) ()
    | IVoting_power (_, k) -> (next [@ocaml.tailcall]) k
    | ITotal_voting_power (_, k) -> (next [@ocaml.tailcall]) k
    | IKeccak (_, k) -> (next [@ocaml.tailcall]) k
    | ISha3 (_, k) -> (next [@ocaml.tailcall]) k
    | IAdd_bls12_381_g1 (_, k) -> (next [@ocaml.tailcall]) k
    | IAdd_bls12_381_g2 (_, k) -> (next [@ocaml.tailcall]) k
    | IAdd_bls12_381_fr (_, k) -> (next [@ocaml.tailcall]) k
    | IMul_bls12_381_g1 (_, k) -> (next [@ocaml.tailcall]) k
    | IMul_bls12_381_g2 (_, k) -> (next [@ocaml.tailcall]) k
    | IMul_bls12_381_fr (_, k) -> (next [@ocaml.tailcall]) k
    | IMul_bls12_381_z_fr (_, k) -> (next [@ocaml.tailcall]) k
    | IMul_bls12_381_fr_z (_, k) -> (next [@ocaml.tailcall]) k
    | IInt_bls12_381_fr (_, k) -> (next [@ocaml.tailcall]) k
    | INeg_bls12_381_g1 (_, k) -> (next [@ocaml.tailcall]) k
    | INeg_bls12_381_g2 (_, k) -> (next [@ocaml.tailcall]) k
    | INeg_bls12_381_fr (_, k) -> (next [@ocaml.tailcall]) k
    | IPairing_check_bls12_381 (_, k) -> (next [@ocaml.tailcall]) k
    | IComb (_, _, _, k) -> (next [@ocaml.tailcall]) k
    | IUncomb (_, _, _, k) -> (next [@ocaml.tailcall]) k
    | IComb_get (_, _, _, k) -> (next [@ocaml.tailcall]) k
    | IComb_set (_, _, _, k) -> (next [@ocaml.tailcall]) k
    | IDup_n (_, _, _, k) -> (next [@ocaml.tailcall]) k
    | ITicket (_, _, k) -> (next [@ocaml.tailcall]) k
    | ITicket_deprecated (_, _, k) -> (next [@ocaml.tailcall]) k
    | IRead_ticket (_, _, k) -> (next [@ocaml.tailcall]) k
    | ISplit_ticket (_, k) -> (next [@ocaml.tailcall]) k
    | IJoin_tickets (_, _, k) -> (next [@ocaml.tailcall]) k
    | IOpen_chest (_, k) -> (next [@ocaml.tailcall]) k
    | IEmit {k; _} -> (next [@ocaml.tailcall]) k
    | IHalt _ -> (return [@ocaml.tailcall]) ()
    | ILog (_, _, _, _, k) -> (next [@ocaml.tailcall]) k
  in
  aux init i (fun accu -> accu)

type 'a ty_traverse = {apply : 't 'tc. 'a -> ('t, 'tc) ty -> 'a}

let ty_traverse =
  let rec aux :
      type ret t tc accu.
      accu ty_traverse -> accu -> (t, tc) ty -> (accu -> ret) -> ret =
   fun f accu ty continue ->
    let accu = f.apply accu ty in
    match ty with
    | Unit_t | Int_t | Nat_t | Signature_t | String_t | Bytes_t | Mutez_t
    | Key_hash_t | Key_t | Timestamp_t | Address_t | Tx_rollup_l2_address_t
    | Bool_t | Sapling_transaction_t _ | Sapling_transaction_deprecated_t _
    | Sapling_state_t _ | Operation_t | Chain_id_t | Never_t | Bls12_381_g1_t
    | Bls12_381_g2_t | Bls12_381_fr_t ->
        (continue [@ocaml.tailcall]) accu
    | Ticket_t (cty, _) -> (aux [@ocaml.tailcall]) f accu cty continue
    | Chest_key_t | Chest_t -> (continue [@ocaml.tailcall]) accu
    | Pair_t (ty1, ty2, _, _) ->
        (next2 [@ocaml.tailcall]) f accu ty1 ty2 continue
    | Or_t (ty1, ty2, _, _) -> (next2 [@ocaml.tailcall]) f accu ty1 ty2 continue
    | Lambda_t (ty1, ty2, _) ->
        (next2 [@ocaml.tailcall]) f accu ty1 ty2 continue
    | Option_t (ty1, _, _) -> (aux [@ocaml.tailcall]) f accu ty1 continue
    | List_t (ty1, _) -> (aux [@ocaml.tailcall]) f accu ty1 continue
    | Set_t (cty, _) -> (aux [@ocaml.tailcall]) f accu cty continue
    | Map_t (cty, ty1, _) -> (next2 [@ocaml.tailcall]) f accu cty ty1 continue
    | Big_map_t (cty, ty1, _) ->
        (next2 [@ocaml.tailcall]) f accu cty ty1 continue
    | Contract_t (ty1, _) -> (aux [@ocaml.tailcall]) f accu ty1 continue
  and next2 :
      type a ac b bc ret accu.
      accu ty_traverse ->
      accu ->
      (a, ac) ty ->
      (b, bc) ty ->
      (accu -> ret) ->
      ret =
   fun f accu ty1 ty2 continue ->
    (aux [@ocaml.tailcall]) f accu ty1 (fun accu ->
        (aux [@ocaml.tailcall]) f accu ty2 continue)
  in
  fun ty init f -> aux f init ty (fun accu -> accu)

type 'accu stack_ty_traverse = {
  apply : 'ty 'S. 'accu -> ('ty, 'S) stack_ty -> 'accu;
}

let stack_ty_traverse (type a t) (sty : (a, t) stack_ty) init f =
  let rec aux : type b u. 'accu -> (b, u) stack_ty -> 'accu =
   fun accu sty ->
    match sty with
    | Bot_t -> f.apply accu sty
    | Item_t (_, sty') -> aux (f.apply accu sty) sty'
  in
  aux init sty

type 'a value_traverse = {apply : 't 'tc. 'a -> ('t, 'tc) ty -> 't -> 'a}

let value_traverse (type t tc) (ty : (t, tc) ty) (x : t) init f =
  let rec aux : type ret t tc. 'accu -> (t, tc) ty -> t -> ('accu -> ret) -> ret
      =
   fun accu ty x continue ->
    let accu = f.apply accu ty x in
    let next2 ty1 ty2 x1 x2 =
      (aux [@ocaml.tailcall]) accu ty1 x1 (fun accu ->
          (aux [@ocaml.tailcall]) accu ty2 x2 (fun accu ->
              (continue [@ocaml.tailcall]) accu))
    in
    let next ty1 x1 =
      (aux [@ocaml.tailcall]) accu ty1 x1 (fun accu ->
          (continue [@ocaml.tailcall]) accu)
    in
    let return () = (continue [@ocaml.tailcall]) accu in
    let rec on_list ty' accu = function
      | [] -> (continue [@ocaml.tailcall]) accu
      | x :: xs ->
          (aux [@ocaml.tailcall]) accu ty' x (fun accu ->
              (on_list [@ocaml.tailcall]) ty' accu xs)
    in
    match ty with
    | Unit_t | Int_t | Nat_t | Signature_t | String_t | Bytes_t | Mutez_t
    | Key_hash_t | Key_t | Timestamp_t | Address_t | Tx_rollup_l2_address_t
    | Bool_t | Sapling_transaction_t _ | Sapling_transaction_deprecated_t _
    | Sapling_state_t _ | Operation_t | Chain_id_t | Never_t | Bls12_381_g1_t
    | Bls12_381_g2_t | Bls12_381_fr_t | Chest_key_t | Chest_t
    | Lambda_t (_, _, _) ->
        (return [@ocaml.tailcall]) ()
    | Pair_t (ty1, ty2, _, _) ->
        (next2 [@ocaml.tailcall]) ty1 ty2 (fst x) (snd x)
    | Or_t (ty1, ty2, _, _) -> (
        match x with
        | L l -> (next [@ocaml.tailcall]) ty1 l
        | R r -> (next [@ocaml.tailcall]) ty2 r)
    | Option_t (ty, _, _) -> (
        match x with
        | None -> return ()
        | Some v -> (next [@ocaml.tailcall]) ty v)
    | Ticket_t (cty, _) -> (aux [@ocaml.tailcall]) accu cty x.contents continue
    | List_t (ty', _) -> on_list ty' accu x.elements
    | Map_t (kty, ty', _) ->
        let (Map_tag (module M)) = x in
        let bindings = M.OPS.fold (fun k v bs -> (k, v) :: bs) M.boxed [] in
        on_bindings accu kty ty' continue bindings
    | Set_t (ty', _) ->
        let (Set_tag (module M)) = x in
        let elements = M.OPS.fold (fun x s -> x :: s) M.boxed [] in
        on_list ty' accu elements
    | Big_map_t (_, _, _) ->
        (* For big maps, there is no obvious recursion scheme so we
           delegate this case to the client. *)
        (return [@ocaml.tailcall]) ()
    | Contract_t (_, _) -> (return [@ocaml.tailcall]) ()
  and on_bindings :
      type ret k v vc.
      'accu ->
      k comparable_ty ->
      (v, vc) ty ->
      ('accu -> ret) ->
      (k * v) list ->
      ret =
   fun accu kty ty' continue xs ->
    match xs with
    | [] -> (continue [@ocaml.tailcall]) accu
    | (k, v) :: xs ->
        (aux [@ocaml.tailcall]) accu kty k (fun accu ->
            (aux [@ocaml.tailcall]) accu ty' v (fun accu ->
                (on_bindings [@ocaml.tailcall]) accu kty ty' continue xs))
  in
  aux init ty x (fun accu -> accu)

let stack_top_ty : type a b s. (a, b * s) stack_ty -> a ty_ex_c = function
  | Item_t (ty, _) -> Ty_ex_c ty

module Typed_contract = struct
  let destination : type a. a typed_contract -> Destination.t = function
    | Typed_implicit pkh -> Destination.Contract (Implicit pkh)
    | Typed_implicit_with_ticket {destination; _} ->
        Destination.Contract (Implicit destination)
    | Typed_originated {contract_hash; _} ->
        Destination.Contract (Originated contract_hash)
    | Typed_sc_rollup {sc_rollup; _} -> Destination.Sc_rollup sc_rollup
    | Typed_zk_rollup {zk_rollup; _} -> Destination.Zk_rollup zk_rollup

  let arg_ty : type a. a typed_contract -> a ty_ex_c = function
    | Typed_implicit _ -> (Ty_ex_c Unit_t : a ty_ex_c)
    | Typed_implicit_with_ticket {ticket_ty; _} -> Ty_ex_c ticket_ty
    | Typed_originated {arg_ty; _} -> Ty_ex_c arg_ty
    | Typed_sc_rollup {arg_ty; _} -> Ty_ex_c arg_ty
    | Typed_zk_rollup {arg_ty; _} -> Ty_ex_c arg_ty

  let entrypoint : type a. a typed_contract -> Entrypoint.t = function
    | Typed_implicit _ | Typed_implicit_with_ticket _ -> Entrypoint.default
    | Typed_originated {entrypoint; _} | Typed_sc_rollup {entrypoint; _} ->
        entrypoint
    | Typed_zk_rollup _ -> Entrypoint.deposit

  module Internal_for_tests = struct
    let typed_exn :
        type a ac.
        (a, ac) ty -> Destination.t -> Entrypoint.t -> a typed_contract =
     fun arg_ty destination entrypoint ->
      match (destination, arg_ty) with
      | Contract (Implicit pkh), Unit_t -> Typed_implicit pkh
      | Contract (Implicit _), _ ->
          invalid_arg "Implicit contracts expect type unit"
      | Contract (Originated contract_hash), _ ->
          Typed_originated {arg_ty; contract_hash; entrypoint}
      | Sc_rollup sc_rollup, _ ->
          Typed_sc_rollup {arg_ty; sc_rollup; entrypoint}
      | Zk_rollup zk_rollup, Pair_t (Ticket_t _, Bytes_t, _, _) ->
          (Typed_zk_rollup {arg_ty; zk_rollup} : a typed_contract)
      | Zk_rollup _, _ ->
          invalid_arg "ZK rollups expect type (pair (ticket _) bytes)"
  end
end
