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
    the IView instruction which modifies `source`, `self`, and `amount` and the KView_exit continuation
    which restores them.
    ======================

*)
type step_constants = {
  source : Contract.t;
      (** The address calling this contract, as returned by SENDER. *)
  payer : Contract.t;
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

  let size = Signature.size
end

type signature = Script_signature.t

(* TODO: https://gitlab.com/tezos/tezos/-/issues/2466
   The various attributes of this type should be checked with
   appropriate testing. *)
type tx_rollup_l2_address = Tx_rollup_l2_address.Indexable.value

type ('a, 'b) pair = 'a * 'b

type ('a, 'b) union = L of 'a | R of 'b

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
    type t = Fr_tag of Bls12_381.Fr.t [@@ocaml.unboxed]

    open Bls12_381.Fr

    let add (Fr_tag x) (Fr_tag y) = Fr_tag (add x y)

    let mul (Fr_tag x) (Fr_tag y) = Fr_tag (mul x y)

    let negate (Fr_tag x) = Fr_tag (negate x)

    let of_bytes_opt bytes = Option.map (fun x -> Fr_tag x) (of_bytes_opt bytes)

    let to_bytes (Fr_tag x) = to_bytes x

    let of_z z = Fr_tag (of_z z)

    let to_z (Fr_tag x) = to_z x
  end

  module G1 = struct
    type t = G1_tag of Bls12_381.G1.t [@@ocaml.unboxed]

    open Bls12_381.G1

    let add (G1_tag x) (G1_tag y) = G1_tag (add x y)

    let mul (G1_tag x) (Fr.Fr_tag y) = G1_tag (mul x y)

    let negate (G1_tag x) = G1_tag (negate x)

    let of_bytes_opt bytes = Option.map (fun x -> G1_tag x) (of_bytes_opt bytes)

    let to_bytes (G1_tag x) = to_bytes x
  end

  module G2 = struct
    type t = G2_tag of Bls12_381.G2.t [@@ocaml.unboxed]

    open Bls12_381.G2

    let add (G2_tag x) (G2_tag y) = G2_tag (add x y)

    let mul (G2_tag x) (Fr.Fr_tag y) = G2_tag (mul x y)

    let negate (G2_tag x) = G2_tag (negate x)

    let of_bytes_opt bytes = Option.map (fun x -> G2_tag x) (of_bytes_opt bytes)

    let to_bytes (G2_tag x) = to_bytes x
  end

  let pairing_check l =
    let l = List.map (fun (G1.G1_tag x, G2.G2_tag y) -> (x, y)) l in
    Bls12_381.pairing_check l
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

type 'a ticket = {ticketer : Contract.t; contents : 'a; amount : n num}

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

  val two : _ t

  val three : _ t

  val four : (_, _) pair option t

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

  let two = 2

  let three = 3

  let four = 4

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

type 'elt boxed_list = {elements : 'elt list; length : int}

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
  | Entrypoints_Union : {
      left : 'l entrypoints_node;
      right : 'r entrypoints_node;
    }
      -> ('l, 'r) union nested_entrypoints
  | Entrypoints_None : _ nested_entrypoints

let no_entrypoints = {at_node = None; nested = Entrypoints_None}

type 'arg entrypoints = {
  root : 'arg entrypoints_node;
  original_type_expr : Script.node;
}

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
          (* This is an over-approximation of the value size in memory, in
             bytes, of the contract's static part, that is its source
             code. This includes the code of the contract as well as the code
             of the views. The storage size is not taken into account by this
             field as it has a dynamic size. *)
    }
      -> ('arg, 'storage) script

(* ---- Instructions --------------------------------------------------------*)
and ('before_top, 'before, 'result_top, 'result) kinstr =
  (*
     Stack
     -----
  *)
  | IDrop :
      ('a, 'b * 's) kinfo * ('b, 's, 'r, 'f) kinstr
      -> ('a, 'b * 's, 'r, 'f) kinstr
  | IDup :
      ('a, 'b * 's) kinfo * ('a, 'a * ('b * 's), 'r, 'f) kinstr
      -> ('a, 'b * 's, 'r, 'f) kinstr
  | ISwap :
      ('a, 'b * ('c * 's)) kinfo * ('b, 'a * ('c * 's), 'r, 'f) kinstr
      -> ('a, 'b * ('c * 's), 'r, 'f) kinstr
  | IConst :
      ('a, 's) kinfo * ('ty, _) ty * 'ty * ('ty, 'a * 's, 'r, 'f) kinstr
      -> ('a, 's, 'r, 'f) kinstr
  (*
     Pairs
     -----
  *)
  | ICons_pair :
      ('a, 'b * ('c * 's)) kinfo * ('a * 'b, 'c * 's, 'r, 'f) kinstr
      -> ('a, 'b * ('c * 's), 'r, 'f) kinstr
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
      ('v, 'a * 's) kinfo * ('v option, 'a * 's, 'r, 'f) kinstr
      -> ('v, 'a * 's, 'r, 'f) kinstr
  | ICons_none :
      ('a, 's) kinfo * ('b, _) ty * ('b option, 'a * 's, 'r, 'f) kinstr
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
      ('a, 'c * 's) kinfo
      * ('b, _) ty
      * (('a, 'b) union, 'c * 's, 'r, 'f) kinstr
      -> ('a, 'c * 's, 'r, 'f) kinstr
  | ICons_right :
      ('b, 'c * 's) kinfo
      * ('a, _) ty
      * (('a, 'b) union, 'c * 's, 'r, 'f) kinstr
      -> ('b, 'c * 's, 'r, 'f) kinstr
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
      ('a, 's) kinfo * ('b, _) ty * ('b boxed_list, 'a * 's, 'r, 'f) kinstr
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
      * ('a, _) ty
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
      * 'a comparable_ty
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
      ('a, 's) kinfo
      * 'b comparable_ty
      * ('c, _) ty
      * (('b, 'c) map, 'a * 's, 'r, 'f) kinstr
      -> ('a, 's, 'r, 'f) kinstr
  | IMap_map :
      (('a, 'b) map, 'd * 's) kinfo
      * ('a, _) ty
      * ('a * 'b, 'd * 's, 'c, 'd * 's) kinstr
      * (('a, 'c) map, 'd * 's, 'r, 'f) kinstr
      -> (('a, 'b) map, 'd * 's, 'r, 'f) kinstr
  | IMap_iter :
      (('a, 'b) map, 'c * 's) kinfo
      * ('a * 'b, _) ty
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
  (* Even though `IAnd_nat` and `IAnd_int_nat` could be merged into a single
     instruction from both the type and behavior point of views, their gas costs
     differ too much (see `cost_N_IAnd_nat` and `cost_N_IAnd_int_nat` in
     `Michelson_v1_gas.Cost_of.Generated_costs`), so we keep them separated. *)
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
      ('a, 'a * ('b * 's)) kinfo
      * 'a comparable_ty
      * (z num, 'b * 's, 'r, 'f) kinstr
      -> ('a, 'a * ('b * 's), 'r, 'f) kinstr
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
      ('a, address * ('c * 's)) kinfo
      * ('a, 'b) view_signature
      * ('b option, 'c * 's, 'r, 'f) kinstr
      -> ('a, address * ('c * 's), 'r, 'f) kinstr
  | ITransfer_tokens :
      ('a, Tez.t * ('a typed_contract * 's)) kinfo
      * (operation, 's, 'r, 'f) kinstr
      -> ('a, Tez.t * ('a typed_contract * 's), 'r, 'f) kinstr
  | IImplicit_account :
      (public_key_hash, 's) kinfo * (unit typed_contract, 's, 'r, 'f) kinstr
      -> (public_key_hash, 's, 'r, 'f) kinstr
  | ICreate_contract : {
      kinfo : (public_key_hash option, Tez.t * ('a * ('c * 's))) kinfo;
      storage_type : ('a, _) ty;
      code : Script.expr;
      k : (operation, address * ('c * 's), 'r, 'f) kinstr;
    }
      -> (public_key_hash option, Tez.t * ('a * ('c * 's)), 'r, 'f) kinstr
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
      ('a, 'b * 's) kinfo * ('a, _) ty * (bytes, 'b * 's, 'r, 'f) kinstr
      -> ('a, 'b * 's, 'r, 'f) kinstr
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
      (Sapling.Legacy.transaction, Sapling.state * 's) kinfo
      * ((z num, Sapling.state) pair option, 's, 'r, 'f) kinstr
      -> (Sapling.Legacy.transaction, Sapling.state * 's, 'r, 'f) kinstr
  | IDig :
      ('a, 's) kinfo
      * int
      * ('b, 'c * 't, 'c, 't, 'a, 's, 'd, 'u) stack_prefix_preservation_witness
      * ('b, 'd * 'u, 'r, 'f) kinstr
      -> ('a, 's, 'r, 'f) kinstr
  | IDug :
      ('a, 'b * 's) kinfo
      * int
      * ('c, 't, 'a, 'c * 't, 'b, 's, 'd, 'u) stack_prefix_preservation_witness
      * ('d, 'u, 'r, 'f) kinstr
      -> ('a, 'b * 's, 'r, 'f) kinstr
  | IDipn :
      ('a, 's) kinfo
      * int
      * ('c, 't, 'd, 'v, 'a, 's, 'b, 'u) stack_prefix_preservation_witness
      * ('c, 't, 'd, 'v) kinstr
      * ('b, 'u, 'r, 'f) kinstr
      -> ('a, 's, 'r, 'f) kinstr
  | IDropn :
      ('a, 's) kinfo
      * int
      * ('b, 'u, 'b, 'u, 'a, 's, 'a, 's) stack_prefix_preservation_witness
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
      ('a, 'b * 's) kinfo
      * int
      * ('a, 'b, 's, 'c, 'd, 't) comb_gadt_witness
      * ('c, 'd * 't, 'r, 'f) kinstr
      -> ('a, 'b * 's, 'r, 'f) kinstr
  | IUncomb :
      ('a, 'b * 's) kinfo
      * int
      * ('a, 'b, 's, 'c, 'd, 't) uncomb_gadt_witness
      * ('c, 'd * 't, 'r, 'f) kinstr
      -> ('a, 'b * 's, 'r, 'f) kinstr
  | IComb_get :
      ('t, 'a * 's) kinfo
      * int
      * ('t, 'v) comb_get_gadt_witness
      * ('v, 'a * 's, 'r, 'f) kinstr
      -> ('t, 'a * 's, 'r, 'f) kinstr
  | IComb_set :
      ('a, 'b * ('d * 's)) kinfo
      * int
      * ('a, 'b, 'c) comb_set_gadt_witness
      * ('c, 'd * 's, 'r, 'f) kinstr
      -> ('a, 'b * ('d * 's), 'r, 'f) kinstr
  | IDup_n :
      ('a, 'b * 's) kinfo
      * int
      * ('a, 'b, 's, 't) dup_n_gadt_witness
      * ('t, 'a * ('b * 's), 'r, 'f) kinstr
      -> ('a, 'b * 's, 'r, 'f) kinstr
  | ITicket :
      ('a, n num * 's) kinfo * 'a comparable_ty * ('a ticket, 's, 'r, 'f) kinstr
      -> ('a, n num * 's, 'r, 'f) kinstr
  | IRead_ticket :
      ('a ticket, 's) kinfo
      * 'a comparable_ty
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
     -----------------------------
  *)
  | IHalt : ('a, 's) kinfo -> ('a, 's, 'a, 's) kinstr
  | ILog :
      ('a, 's) kinfo
      * ('a, 's) stack_ty
      * logging_event
      * logger
      * ('a, 's, 'r, 'f) kinstr
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

and (_, _, _, _) continuation =
  | KNil : ('r, 'f, 'r, 'f) continuation
  | KCons :
      ('a, 's, 'b, 't) kinstr * ('b, 't, 'r, 'f) continuation
      -> ('a, 's, 'r, 'f) continuation
  | KReturn :
      's * ('a, 's, 'r, 'f) continuation
      -> ('a, end_of_stack, 'r, 'f) continuation
  | KMap_head :
      ('a -> 'b) * ('b, 's, 'r, 'f) continuation
      -> ('a, 's, 'r, 'f) continuation
  | KUndip :
      'b * ('b, 'a * 's, 'r, 'f) continuation
      -> ('a, 's, 'r, 'f) continuation
  | KLoop_in :
      ('a, 's, bool, 'a * 's) kinstr * ('a, 's, 'r, 'f) continuation
      -> (bool, 'a * 's, 'r, 'f) continuation
  | KLoop_in_left :
      ('a, 's, ('a, 'b) union, 's) kinstr * ('b, 's, 'r, 'f) continuation
      -> (('a, 'b) union, 's, 'r, 'f) continuation
  | KIter :
      ('a, 'b * 's, 'b, 's) kinstr
      * ('a, _) ty
      * 'a list
      * ('b, 's, 'r, 'f) continuation
      -> ('b, 's, 'r, 'f) continuation
  | KList_enter_body :
      ('a, 'c * 's, 'b, 'c * 's) kinstr
      * 'a list
      * 'b list
      * int
      * ('b boxed_list, 'c * 's, 'r, 'f) continuation
      -> ('c, 's, 'r, 'f) continuation
  | KList_exit_body :
      ('a, 'c * 's, 'b, 'c * 's) kinstr
      * 'a list
      * 'b list
      * int
      * ('b boxed_list, 'c * 's, 'r, 'f) continuation
      -> ('b, 'c * 's, 'r, 'f) continuation
  | KMap_enter_body :
      ('a * 'b, 'd * 's, 'c, 'd * 's) kinstr
      * ('a * 'b) list
      * ('a, 'c) map
      * (('a, 'c) map, 'd * 's, 'r, 'f) continuation
      -> ('d, 's, 'r, 'f) continuation
  | KMap_exit_body :
      ('a * 'b, 'd * 's, 'c, 'd * 's) kinstr
      * ('a * 'b) list
      * ('a, 'c) map
      * 'a
      * (('a, 'c) map, 'd * 's, 'r, 'f) continuation
      -> ('c, 'd * 's, 'r, 'f) continuation
  | KView_exit :
      step_constants * ('a, 's, 'r, 'f) continuation
      -> ('a, 's, 'r, 'f) continuation
  | KLog :
      ('a, 's, 'r, 'f) continuation * ('a, 's) stack_ty * logger
      -> ('a, 's, 'r, 'f) continuation

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
  log_entry : 'a 's 'b 'f. ('a, 's, 'b, 'f, 'a, 's) logging_function;
  log_control : 'a 's 'b 'f. ('a, 's, 'b, 'f) continuation -> unit;
  log_exit : 'a 's 'b 'f 'c 'u. ('a, 's, 'b, 'f, 'c, 'u) logging_function;
  get_log : unit -> execution_trace option tzresult Lwt.t;
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

and ('a, 's) kinfo = {iloc : Script.location}

and (_, _, _, _, _, _, _, _) stack_prefix_preservation_witness =
  | KPrefix :
      ('y, 'u) kinfo
      * ('a, _) ty
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

and (_, _, _, _, _, _) comb_gadt_witness =
  | Comb_one : ('a, 'x, 'before, 'a, 'x, 'before) comb_gadt_witness
  | Comb_succ :
      ('b, 'c, 's, 'd, 'e, 't) comb_gadt_witness
      -> ('a, 'b, 'c * 's, 'a * 'd, 'e, 't) comb_gadt_witness

and (_, _, _, _, _, _) uncomb_gadt_witness =
  | Uncomb_one : ('a, 'x, 'before, 'a, 'x, 'before) uncomb_gadt_witness
  | Uncomb_succ :
      ('b, 'c, 's, 'd, 'e, 't) uncomb_gadt_witness
      -> ('a * 'b, 'c, 's, 'a, 'd, 'e * 't) uncomb_gadt_witness

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

and 'kind manager_operation =
  | Transaction_to_contract : {
      destination : Contract.t;
      amount : Tez.tez;
      entrypoint : Entrypoint.t;
      location : Script.location;
      parameters_ty : ('a, _) ty;
      parameters : 'a;
      unparsed_parameters : Script.expr;
    }
      -> Kind.transaction manager_operation
  | Transaction_to_tx_rollup : {
      destination : Tx_rollup.t;
      parameters_ty : ('a, _) ty;
      parameters : 'a;
      unparsed_parameters : Script.expr;
    }
      -> Kind.transaction manager_operation
  | Transaction_to_sc_rollup : {
      destination : Sc_rollup.t;
      entrypoint : Entrypoint.t;
      parameters_ty : ('a, _) ty;
      parameters : 'a;
      unparsed_parameters : Script.expr;
    }
      -> Kind.transaction manager_operation
  | Origination : {
      delegate : Signature.Public_key_hash.t option;
      code : Script.expr;
      unparsed_storage : Script.expr;
      credit : Tez.tez;
      preorigination : Contract_hash.t;
      storage_type : ('storage, _) ty;
      storage : 'storage;
    }
      -> Kind.origination manager_operation
  | Delegation :
      Signature.Public_key_hash.t option
      -> Kind.delegation manager_operation

and 'kind internal_operation = {
  source : Contract.t;
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

let manager_kind : type kind. kind manager_operation -> kind Kind.manager =
  function
  | Transaction_to_contract _ -> Kind.Transaction_manager_kind
  | Transaction_to_tx_rollup _ -> Kind.Transaction_manager_kind
  | Transaction_to_sc_rollup _ -> Kind.Transaction_manager_kind
  | Origination _ -> Kind.Origination_manager_kind
  | Delegation _ -> Kind.Delegation_manager_kind

let kinfo_of_kinstr : type a s b f. (a, s, b, f) kinstr -> (a, s) kinfo =
 fun i ->
  match i with
  | IDrop (kinfo, _) -> kinfo
  | IDup (kinfo, _) -> kinfo
  | ISwap (kinfo, _) -> kinfo
  | IConst (kinfo, _, _, _) -> kinfo
  | ICons_pair (kinfo, _) -> kinfo
  | ICar (kinfo, _) -> kinfo
  | ICdr (kinfo, _) -> kinfo
  | IUnpair (kinfo, _) -> kinfo
  | ICons_some (kinfo, _) -> kinfo
  | ICons_none (kinfo, _, _) -> kinfo
  | IIf_none {kinfo; _} -> kinfo
  | IOpt_map {kinfo; _} -> kinfo
  | ICons_left (kinfo, _, _) -> kinfo
  | ICons_right (kinfo, _, _) -> kinfo
  | IIf_left {kinfo; _} -> kinfo
  | ICons_list (kinfo, _) -> kinfo
  | INil (kinfo, _, _) -> kinfo
  | IIf_cons {kinfo; _} -> kinfo
  | IList_map (kinfo, _, _) -> kinfo
  | IList_iter (kinfo, _, _, _) -> kinfo
  | IList_size (kinfo, _) -> kinfo
  | IEmpty_set (kinfo, _, _) -> kinfo
  | ISet_iter (kinfo, _, _, _) -> kinfo
  | ISet_mem (kinfo, _) -> kinfo
  | ISet_update (kinfo, _) -> kinfo
  | ISet_size (kinfo, _) -> kinfo
  | IEmpty_map (kinfo, _, _, _) -> kinfo
  | IMap_map (kinfo, _, _, _) -> kinfo
  | IMap_iter (kinfo, _, _, _) -> kinfo
  | IMap_mem (kinfo, _) -> kinfo
  | IMap_get (kinfo, _) -> kinfo
  | IMap_update (kinfo, _) -> kinfo
  | IMap_get_and_update (kinfo, _) -> kinfo
  | IMap_size (kinfo, _) -> kinfo
  | IEmpty_big_map (kinfo, _, _, _) -> kinfo
  | IBig_map_mem (kinfo, _) -> kinfo
  | IBig_map_get (kinfo, _) -> kinfo
  | IBig_map_update (kinfo, _) -> kinfo
  | IBig_map_get_and_update (kinfo, _) -> kinfo
  | IConcat_string (kinfo, _) -> kinfo
  | IConcat_string_pair (kinfo, _) -> kinfo
  | ISlice_string (kinfo, _) -> kinfo
  | IString_size (kinfo, _) -> kinfo
  | IConcat_bytes (kinfo, _) -> kinfo
  | IConcat_bytes_pair (kinfo, _) -> kinfo
  | ISlice_bytes (kinfo, _) -> kinfo
  | IBytes_size (kinfo, _) -> kinfo
  | IAdd_seconds_to_timestamp (kinfo, _) -> kinfo
  | IAdd_timestamp_to_seconds (kinfo, _) -> kinfo
  | ISub_timestamp_seconds (kinfo, _) -> kinfo
  | IDiff_timestamps (kinfo, _) -> kinfo
  | IAdd_tez (kinfo, _) -> kinfo
  | ISub_tez (kinfo, _) -> kinfo
  | ISub_tez_legacy (kinfo, _) -> kinfo
  | IMul_teznat (kinfo, _) -> kinfo
  | IMul_nattez (kinfo, _) -> kinfo
  | IEdiv_teznat (kinfo, _) -> kinfo
  | IEdiv_tez (kinfo, _) -> kinfo
  | IOr (kinfo, _) -> kinfo
  | IAnd (kinfo, _) -> kinfo
  | IXor (kinfo, _) -> kinfo
  | INot (kinfo, _) -> kinfo
  | IIs_nat (kinfo, _) -> kinfo
  | INeg (kinfo, _) -> kinfo
  | IAbs_int (kinfo, _) -> kinfo
  | IInt_nat (kinfo, _) -> kinfo
  | IAdd_int (kinfo, _) -> kinfo
  | IAdd_nat (kinfo, _) -> kinfo
  | ISub_int (kinfo, _) -> kinfo
  | IMul_int (kinfo, _) -> kinfo
  | IMul_nat (kinfo, _) -> kinfo
  | IEdiv_int (kinfo, _) -> kinfo
  | IEdiv_nat (kinfo, _) -> kinfo
  | ILsl_nat (kinfo, _) -> kinfo
  | ILsr_nat (kinfo, _) -> kinfo
  | IOr_nat (kinfo, _) -> kinfo
  | IAnd_nat (kinfo, _) -> kinfo
  | IAnd_int_nat (kinfo, _) -> kinfo
  | IXor_nat (kinfo, _) -> kinfo
  | INot_int (kinfo, _) -> kinfo
  | IIf {kinfo; _} -> kinfo
  | ILoop (kinfo, _, _) -> kinfo
  | ILoop_left (kinfo, _, _) -> kinfo
  | IDip (kinfo, _, _) -> kinfo
  | IExec (kinfo, _) -> kinfo
  | IApply (kinfo, _, _) -> kinfo
  | ILambda (kinfo, _, _) -> kinfo
  | IFailwith (kinfo, _, _) -> kinfo
  | ICompare (kinfo, _, _) -> kinfo
  | IEq (kinfo, _) -> kinfo
  | INeq (kinfo, _) -> kinfo
  | ILt (kinfo, _) -> kinfo
  | IGt (kinfo, _) -> kinfo
  | ILe (kinfo, _) -> kinfo
  | IGe (kinfo, _) -> kinfo
  | IAddress (kinfo, _) -> kinfo
  | IContract (kinfo, _, _, _) -> kinfo
  | ITransfer_tokens (kinfo, _) -> kinfo
  | IView (kinfo, _, _) -> kinfo
  | IImplicit_account (kinfo, _) -> kinfo
  | ICreate_contract {kinfo; _} -> kinfo
  | ISet_delegate (kinfo, _) -> kinfo
  | INow (kinfo, _) -> kinfo
  | IMin_block_time (kinfo, _) -> kinfo
  | IBalance (kinfo, _) -> kinfo
  | ILevel (kinfo, _) -> kinfo
  | ICheck_signature (kinfo, _) -> kinfo
  | IHash_key (kinfo, _) -> kinfo
  | IPack (kinfo, _, _) -> kinfo
  | IUnpack (kinfo, _, _) -> kinfo
  | IBlake2b (kinfo, _) -> kinfo
  | ISha256 (kinfo, _) -> kinfo
  | ISha512 (kinfo, _) -> kinfo
  | ISource (kinfo, _) -> kinfo
  | ISender (kinfo, _) -> kinfo
  | ISelf (kinfo, _, _, _) -> kinfo
  | ISelf_address (kinfo, _) -> kinfo
  | IAmount (kinfo, _) -> kinfo
  | ISapling_empty_state (kinfo, _, _) -> kinfo
  | ISapling_verify_update (kinfo, _) -> kinfo
  | ISapling_verify_update_deprecated (kinfo, _) -> kinfo
  | IDig (kinfo, _, _, _) -> kinfo
  | IDug (kinfo, _, _, _) -> kinfo
  | IDipn (kinfo, _, _, _, _) -> kinfo
  | IDropn (kinfo, _, _, _) -> kinfo
  | IChainId (kinfo, _) -> kinfo
  | INever kinfo -> kinfo
  | IVoting_power (kinfo, _) -> kinfo
  | ITotal_voting_power (kinfo, _) -> kinfo
  | IKeccak (kinfo, _) -> kinfo
  | ISha3 (kinfo, _) -> kinfo
  | IAdd_bls12_381_g1 (kinfo, _) -> kinfo
  | IAdd_bls12_381_g2 (kinfo, _) -> kinfo
  | IAdd_bls12_381_fr (kinfo, _) -> kinfo
  | IMul_bls12_381_g1 (kinfo, _) -> kinfo
  | IMul_bls12_381_g2 (kinfo, _) -> kinfo
  | IMul_bls12_381_fr (kinfo, _) -> kinfo
  | IMul_bls12_381_z_fr (kinfo, _) -> kinfo
  | IMul_bls12_381_fr_z (kinfo, _) -> kinfo
  | IInt_bls12_381_fr (kinfo, _) -> kinfo
  | INeg_bls12_381_g1 (kinfo, _) -> kinfo
  | INeg_bls12_381_g2 (kinfo, _) -> kinfo
  | INeg_bls12_381_fr (kinfo, _) -> kinfo
  | IPairing_check_bls12_381 (kinfo, _) -> kinfo
  | IComb (kinfo, _, _, _) -> kinfo
  | IUncomb (kinfo, _, _, _) -> kinfo
  | IComb_get (kinfo, _, _, _) -> kinfo
  | IComb_set (kinfo, _, _, _) -> kinfo
  | IDup_n (kinfo, _, _, _) -> kinfo
  | ITicket (kinfo, _, _) -> kinfo
  | IRead_ticket (kinfo, _, _) -> kinfo
  | ISplit_ticket (kinfo, _) -> kinfo
  | IJoin_tickets (kinfo, _, _) -> kinfo
  | IHalt kinfo -> kinfo
  | ILog (kinfo, _, _, _, _) -> kinfo
  | IOpen_chest (kinfo, _) -> kinfo

let meta_basic = {size = Type_size.one}

let ty_metadata : type a ac. (a, ac) ty -> a ty_metadata = function
  | Unit_t | Never_t | Int_t | Nat_t | Signature_t | String_t | Bytes_t
  | Mutez_t | Bool_t | Key_hash_t | Key_t | Timestamp_t | Chain_id_t | Address_t
  | Tx_rollup_l2_address_t ->
      meta_basic
  | Pair_t (_, _, meta, _) -> meta
  | Union_t (_, _, meta, _) -> meta
  | Option_t (_, meta, _) -> meta
  | Lambda_t (_, _, meta) -> meta
  | List_t (_, meta) -> meta
  | Set_t (_, meta) -> meta
  | Map_t (_, _, meta) -> meta
  | Big_map_t (_, _, meta) -> meta
  | Ticket_t (_, meta) -> meta
  | Contract_t (_, meta) -> meta
  | Sapling_transaction_t _ | Sapling_transaction_deprecated_t _
  | Sapling_state_t _ | Operation_t | Bls12_381_g1_t | Bls12_381_g2_t
  | Bls12_381_fr_t | Chest_t | Chest_key_t ->
      meta_basic

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
  | Union_t (_, _, _, dand) -> dbool_of_dand dand
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
  Type_size.compound2 loc (ty_size l) (ty_size r) >|? fun size ->
  let (Ex_dand cmp) = dand (is_comparable l) (is_comparable r) in
  Ty_ex_c (Pair_t (l, r, {size}, cmp))

let pair_3_t loc l m r = pair_t loc m r >>? fun (Ty_ex_c r) -> pair_t loc l r

let comparable_pair_t loc l r =
  Type_size.compound2 loc (ty_size l) (ty_size r) >|? fun size ->
  Pair_t (l, r, {size}, YesYes)

let comparable_pair_3_t loc l m r =
  comparable_pair_t loc m r >>? fun r -> comparable_pair_t loc l r

let union_t :
    type a ac b bc.
    Script.location -> (a, ac) ty -> (b, bc) ty -> (a, b) union ty_ex_c tzresult
    =
 fun loc l r ->
  Type_size.compound2 loc (ty_size l) (ty_size r) >|? fun size ->
  let (Ex_dand cmp) = dand (is_comparable l) (is_comparable r) in
  Ty_ex_c (Union_t (l, r, {size}, cmp))

let union_bytes_bool_t =
  Union_t (bytes_t, bool_t, {size = Type_size.three}, YesYes)

let comparable_union_t loc l r =
  Type_size.compound2 loc (ty_size l) (ty_size r) >|? fun size ->
  Union_t (l, r, {size}, YesYes)

let lambda_t loc l r =
  Type_size.compound2 loc (ty_size l) (ty_size r) >|? fun size ->
  Lambda_t (l, r, {size})

let option_t loc t =
  Type_size.compound1 loc (ty_size t) >|? fun size ->
  let cmp = is_comparable t in
  Option_t (t, {size}, cmp)

let option_mutez_t = Option_t (mutez_t, {size = Type_size.two}, Yes)

let option_string_t = Option_t (string_t, {size = Type_size.two}, Yes)

let option_bytes_t = Option_t (bytes_t, {size = Type_size.two}, Yes)

let option_nat_t = Option_t (nat_t, {size = Type_size.two}, Yes)

let option_pair_nat_nat_t =
  Option_t
    ( Pair_t (nat_t, nat_t, {size = Type_size.three}, YesYes),
      {size = Type_size.four},
      Yes )

let option_pair_nat_mutez_t =
  Option_t
    ( Pair_t (nat_t, mutez_t, {size = Type_size.three}, YesYes),
      {size = Type_size.four},
      Yes )

let option_pair_mutez_mutez_t =
  Option_t
    ( Pair_t (mutez_t, mutez_t, {size = Type_size.three}, YesYes),
      {size = Type_size.four},
      Yes )

let option_pair_int_nat_t =
  Option_t
    ( Pair_t (int_t, nat_t, {size = Type_size.three}, YesYes),
      {size = Type_size.four},
      Yes )

let comparable_option_t loc t =
  Type_size.compound1 loc (ty_size t) >|? fun size -> Option_t (t, {size}, Yes)

let list_t loc t =
  Type_size.compound1 loc (ty_size t) >|? fun size -> List_t (t, {size})

let operation_t = Operation_t

let list_operation_t = List_t (operation_t, {size = Type_size.two})

let set_t loc t =
  Type_size.compound1 loc (ty_size t) >|? fun size -> Set_t (t, {size})

let map_t loc l r =
  Type_size.compound2 loc (ty_size l) (ty_size r) >|? fun size ->
  Map_t (l, r, {size})

let big_map_t loc l r =
  Type_size.compound2 loc (ty_size l) (ty_size r) >|? fun size ->
  Big_map_t (l, r, {size})

let contract_t loc t =
  Type_size.compound1 loc (ty_size t) >|? fun size -> Contract_t (t, {size})

let contract_unit_t = Contract_t (unit_t, {size = Type_size.two})

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
  Type_size.compound1 loc (ty_size t) >|? fun size -> Ticket_t (t, {size})

let chest_key_t = Chest_key_t

let chest_t = Chest_t

type 'a kinstr_traverse = {
  apply : 'b 'u 'r 'f. 'a -> ('b, 'u, 'r, 'f) kinstr -> 'a;
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
    | IConst (_, _, _, k) -> (next [@ocaml.tailcall]) k
    | ICons_pair (_, k) -> (next [@ocaml.tailcall]) k
    | ICar (_, k) -> (next [@ocaml.tailcall]) k
    | ICdr (_, k) -> (next [@ocaml.tailcall]) k
    | IUnpair (_, k) -> (next [@ocaml.tailcall]) k
    | ICons_some (_, k) -> (next [@ocaml.tailcall]) k
    | ICons_none (_, _, k) -> (next [@ocaml.tailcall]) k
    | IIf_none {kinfo = _; branch_if_none = k1; branch_if_some = k2; k} ->
        (next3 [@ocaml.tailcall]) k1 k2 k
    | IOpt_map {kinfo = _; body; k} -> (next2 [@ocaml.tailcall]) body k
    | ICons_left (_, _, k) -> (next [@ocaml.tailcall]) k
    | ICons_right (_, _, k) -> (next [@ocaml.tailcall]) k
    | IIf_left {kinfo = _; branch_if_left = k1; branch_if_right = k2; k} ->
        (next3 [@ocaml.tailcall]) k1 k2 k
    | ICons_list (_, k) -> (next [@ocaml.tailcall]) k
    | INil (_, _, k) -> (next [@ocaml.tailcall]) k
    | IIf_cons {kinfo = _; branch_if_nil = k1; branch_if_cons = k2; k} ->
        (next3 [@ocaml.tailcall]) k1 k2 k
    | IList_map (_, k1, k2) -> (next2 [@ocaml.tailcall]) k1 k2
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
    | IIf {kinfo = _; branch_if_true = k1; branch_if_false = k2; k} ->
        (next3 [@ocaml.tailcall]) k1 k2 k
    | ILoop (_, k1, k2) -> (next2 [@ocaml.tailcall]) k1 k2
    | ILoop_left (_, k1, k2) -> (next2 [@ocaml.tailcall]) k1 k2
    | IDip (_, k1, k2) -> (next2 [@ocaml.tailcall]) k1 k2
    | IExec (_, k) -> (next [@ocaml.tailcall]) k
    | IApply (_, _, k) -> (next [@ocaml.tailcall]) k
    | ILambda (_, _, k) -> (next [@ocaml.tailcall]) k
    | IFailwith (_, _, _) -> (return [@ocaml.tailcall]) ()
    | ICompare (_, _, k) -> (next [@ocaml.tailcall]) k
    | IEq (_, k) -> (next [@ocaml.tailcall]) k
    | INeq (_, k) -> (next [@ocaml.tailcall]) k
    | ILt (_, k) -> (next [@ocaml.tailcall]) k
    | IGt (_, k) -> (next [@ocaml.tailcall]) k
    | ILe (_, k) -> (next [@ocaml.tailcall]) k
    | IGe (_, k) -> (next [@ocaml.tailcall]) k
    | IAddress (_, k) -> (next [@ocaml.tailcall]) k
    | IContract (_, _, _, k) -> (next [@ocaml.tailcall]) k
    | IView (_, _, k) -> (next [@ocaml.tailcall]) k
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
    | IRead_ticket (_, _, k) -> (next [@ocaml.tailcall]) k
    | ISplit_ticket (_, k) -> (next [@ocaml.tailcall]) k
    | IJoin_tickets (_, _, k) -> (next [@ocaml.tailcall]) k
    | IOpen_chest (_, k) -> (next [@ocaml.tailcall]) k
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
    | Ticket_t (cty, _) -> aux f accu cty continue
    | Chest_key_t | Chest_t -> (continue [@ocaml.tailcall]) accu
    | Pair_t (ty1, ty2, _, _) ->
        (next2 [@ocaml.tailcall]) f accu ty1 ty2 continue
    | Union_t (ty1, ty2, _, _) ->
        (next2 [@ocaml.tailcall]) f accu ty1 ty2 continue
    | Lambda_t (ty1, ty2, _) ->
        (next2 [@ocaml.tailcall]) f accu ty1 ty2 continue
    | Option_t (ty1, _, _) -> (next [@ocaml.tailcall]) f accu ty1 continue
    | List_t (ty1, _) -> (next [@ocaml.tailcall]) f accu ty1 continue
    | Set_t (cty, _) -> (aux [@ocaml.tailcall]) f accu cty continue
    | Map_t (cty, ty1, _) ->
        (aux [@ocaml.tailcall]) f accu cty (fun accu ->
            (next [@ocaml.tailcall]) f accu ty1 continue)
    | Big_map_t (cty, ty1, _) ->
        (aux [@ocaml.tailcall]) f accu cty (fun accu ->
            (next [@ocaml.tailcall]) f accu ty1 continue)
    | Contract_t (ty1, _) -> (next [@ocaml.tailcall]) f accu ty1 continue
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
        (aux [@ocaml.tailcall]) f accu ty2 (fun accu ->
            (continue [@ocaml.tailcall]) accu))
  and next :
      type a ac ret accu.
      accu ty_traverse -> accu -> (a, ac) ty -> (accu -> ret) -> ret =
   fun f accu ty1 continue ->
    (aux [@ocaml.tailcall]) f accu ty1 (fun accu ->
        (continue [@ocaml.tailcall]) accu)
  in
  fun ty init f -> aux f init ty (fun accu -> accu)

type 'accu stack_ty_traverse = {
  apply : 'ty 's. 'accu -> ('ty, 's) stack_ty -> 'accu;
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
    | Union_t (ty1, ty2, _, _) -> (
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
  [@@coq_axiom_with_reason "local mutually recursive definition not handled"]

let stack_top_ty : type a b s. (a, b * s) stack_ty -> a ty_ex_c = function
  | Item_t (ty, _) -> Ty_ex_c ty

type kinstr_rewritek = {
  apply :
    'b 'u 'r 'f.
    ('b, 'u) stack_ty -> ('b, 'u, 'r, 'f) kinstr -> ('b, 'u, 'r, 'f) kinstr;
}

type ('a, 's) failed_kinstr_cast = {cast : 'b 'u. ('a, 's, 'b, 'u) kinstr}

type ('a, 's, 'r, 'f) ex_splitted_kinstr =
  | Ex_splitted_kinstr :
      ('b, 'u) stack_ty
      * ('b, 'u, 'r, 'f) kinstr
      * (('b, 'u, 'r, 'f) kinstr -> ('a, 's, 'r, 'f) kinstr)
      -> ('a, 's, 'r, 'f) ex_splitted_kinstr
  | Ex_splitted_log :
      ('a, 's) stack_ty
      * ('a, 's, 'r, 'f) kinstr
      * (('a, 's, 'r, 'f) kinstr -> ('a, 's, 'r, 'f) kinstr)
      -> ('a, 's, 'r, 'f) ex_splitted_kinstr
  | Ex_splitted_loop_may_fail :
      ('b, 'u) stack_ty
      * ('b, 'u, 'r, 'f) kinstr
      * ('c, 'v) stack_ty
      * ('c, 'v, 't, 'g) kinstr
      * (('b, 'u, 'r, 'f) kinstr ->
        ('c, 'v, 't, 'g) kinstr ->
        ('a, 's, 't, 'g) kinstr)
      -> ('a, 's, 't, 'g) ex_splitted_kinstr
  | Ex_splitted_loop_may_not_fail :
      ('b, 'u) stack_ty
      * ('b, 'u, 'r, 'f) kinstr
      * ('c, 'v, 't, 'g) kinstr
      * (('r, 'f) stack_ty -> ('c, 'v) stack_ty tzresult)
      * (('b, 'u, 'r, 'f) kinstr ->
        ('c, 'v, 't, 'g) kinstr ->
        ('a, 's, 't, 'g) kinstr)
      -> ('a, 's, 't, 'g) ex_splitted_kinstr
  | Ex_splitted_if :
      ('b, 'u) stack_ty
      * ('b, 'u, 'r, 'f) kinstr
      * ('c, 'v) stack_ty
      * ('c, 'v, 'r, 'f) kinstr
      * ('r, 'f, 't, 'g) kinstr
      * (('b, 'u, 'r, 'f) kinstr ->
        ('c, 'v, 'r, 'f) kinstr ->
        ('r, 'f, 't, 'g) kinstr ->
        ('a, 's, 't, 'g) kinstr)
      -> ('a, 's, 't, 'g) ex_splitted_kinstr
  | Ex_splitted_halt : ('a, 's) kinfo -> ('a, 's, 'a, 's) ex_splitted_kinstr
  | Ex_splitted_failwith :
      ('a, 's) kinfo
      * Script.location
      * ('a, _) ty
      * ('a, 's) failed_kinstr_cast
      -> ('a, 's, 'r, 'f) ex_splitted_kinstr

let rec stack_prefix_preservation_witness_split_input :
    type a s b t c u d v.
    (b, t, c, u, a, s, d, v) stack_prefix_preservation_witness ->
    (a, s) stack_ty ->
    (b, t) stack_ty =
 fun w s ->
  match (w, s) with
  | KPrefix (_, _, w), Item_t (_, s) ->
      stack_prefix_preservation_witness_split_input w s
  | KRest, s -> s

let rec stack_prefix_preservation_witness_split_output :
    type a s b t c u d v.
    (b, t, c, u, a, s, d, v) stack_prefix_preservation_witness ->
    (c, u) stack_ty ->
    (d, v) stack_ty =
 fun w s ->
  match (w, s) with
  | KPrefix (_, a, w), s ->
      Item_t (a, stack_prefix_preservation_witness_split_output w s)
  | KRest, s -> s

let kinstr_split :
    type a s r f.
    (a, s) stack_ty ->
    (a, s, r, f) kinstr ->
    (a, s, r, f) ex_splitted_kinstr tzresult =
 fun s i ->
  let loc = Micheline.dummy_location in
  match (i, s) with
  | IDrop (kinfo, k), Item_t (_a, s) ->
      ok @@ Ex_splitted_kinstr (s, k, fun k -> IDrop (kinfo, k))
  | IDup (kinfo, k), Item_t (a, s) ->
      let s = Item_t (a, Item_t (a, s)) in
      ok @@ Ex_splitted_kinstr (s, k, fun k -> IDup (kinfo, k))
  | ISwap (kinfo, k), Item_t (a, Item_t (b, s)) ->
      let s = Item_t (b, Item_t (a, s)) in
      ok @@ Ex_splitted_kinstr (s, k, fun k -> ISwap (kinfo, k))
  | IConst (kinfo, a, x, k), s ->
      let s = Item_t (a, s) in
      ok @@ Ex_splitted_kinstr (s, k, fun k -> IConst (kinfo, a, x, k))
  | ICons_pair (kinfo, k), Item_t (a, Item_t (b, s)) ->
      pair_t loc a b >|? fun (Ty_ex_c c) ->
      let s = Item_t (c, s) in
      Ex_splitted_kinstr (s, k, fun k -> ICons_pair (kinfo, k))
  | ICar (kinfo, k), Item_t (Pair_t (a, _b, _meta, _), s) ->
      let s = Item_t (a, s) in
      ok @@ Ex_splitted_kinstr (s, k, fun k -> ICar (kinfo, k))
  | ICdr (kinfo, k), Item_t (Pair_t (_a, b, _meta, _), s) ->
      let s = Item_t (b, s) in
      ok @@ Ex_splitted_kinstr (s, k, fun k -> ICdr (kinfo, k))
  | IUnpair (kinfo, k), Item_t (Pair_t (a, b, _meta, _), s) ->
      let s = Item_t (a, Item_t (b, s)) in
      ok @@ Ex_splitted_kinstr (s, k, fun k -> IUnpair (kinfo, k))
  | ICons_some (kinfo, k), Item_t (a, s) ->
      option_t loc a >|? fun o ->
      let s = Item_t (o, s) in
      Ex_splitted_kinstr (s, k, fun k -> ICons_some (kinfo, k))
  | ICons_none (kinfo, a, k), s ->
      option_t loc a >|? fun o ->
      let s = Item_t (o, s) in
      Ex_splitted_kinstr (s, k, fun k -> ICons_none (kinfo, a, k))
  | ( IIf_none {kinfo; branch_if_none; branch_if_some; k},
      Item_t (Option_t (a, _meta, _), s) ) ->
      ok
      @@ Ex_splitted_if
           ( s,
             branch_if_none,
             Item_t (a, s),
             branch_if_some,
             k,
             fun branch_if_none branch_if_some k ->
               IIf_none {kinfo; branch_if_none; branch_if_some; k} )
  | IOpt_map {kinfo; body; k}, Item_t (Option_t (a, _meta, _), s) ->
      let s = Item_t (a, s) in
      ok
      @@ Ex_splitted_loop_may_not_fail
           ( s,
             body,
             k,
             (function
             | Item_t (b, s) -> option_t loc b >|? fun o -> Item_t (o, s)),
             fun body k -> IOpt_map {kinfo; body; k} )
  | ICons_left (kinfo, b, k), Item_t (a, s) ->
      union_t loc a b >|? fun (Ty_ex_c c) ->
      let s = Item_t (c, s) in
      Ex_splitted_kinstr (s, k, fun k -> ICons_left (kinfo, b, k))
  | ICons_right (kinfo, a, k), Item_t (b, s) ->
      union_t loc a b >|? fun (Ty_ex_c c) ->
      let s = Item_t (c, s) in
      Ex_splitted_kinstr (s, k, fun k -> ICons_right (kinfo, a, k))
  | ( IIf_left {kinfo; branch_if_left; branch_if_right; k},
      Item_t (Union_t (a, b, _meta, _), s) ) ->
      ok
      @@ Ex_splitted_if
           ( Item_t (a, s),
             branch_if_left,
             Item_t (b, s),
             branch_if_right,
             k,
             fun branch_if_left branch_if_right k ->
               IIf_left {kinfo; branch_if_left; branch_if_right; k} )
  | ICons_list (kinfo, k), Item_t (_a, Item_t (l, s)) ->
      let s = Item_t (l, s) in
      ok @@ Ex_splitted_kinstr (s, k, fun k -> ICons_list (kinfo, k))
  | INil (kinfo, a, k), s ->
      list_t loc a >|? fun l ->
      let s = Item_t (l, s) in
      Ex_splitted_kinstr (s, k, fun k -> INil (kinfo, a, k))
  | ( IIf_cons {kinfo; branch_if_cons; branch_if_nil; k},
      Item_t ((List_t (a, _meta) as l), s) ) ->
      ok
      @@ Ex_splitted_if
           ( Item_t (a, Item_t (l, s)),
             branch_if_cons,
             s,
             branch_if_nil,
             k,
             fun branch_if_cons branch_if_nil k ->
               IIf_cons {kinfo; branch_if_cons; branch_if_nil; k} )
  | IList_map (kinfo, body, k), Item_t (List_t (a, _meta), s) ->
      let s = Item_t (a, s) in
      ok
      @@ Ex_splitted_loop_may_not_fail
           ( s,
             body,
             k,
             (function
             | Item_t (b, s) -> list_t loc b >|? fun l -> Item_t (l, s)),
             fun body k -> IList_map (kinfo, body, k) )
  | IList_iter (kinfo, ty, body, k), Item_t (List_t (a, _meta), s) ->
      let s' = Item_t (a, s) in
      ok
      @@ Ex_splitted_loop_may_fail
           (s', body, s, k, fun body k -> IList_iter (kinfo, ty, body, k))
  | IList_size (kinfo, k), Item_t (_l, s) ->
      let s = Item_t (nat_t, s) in
      ok @@ Ex_splitted_kinstr (s, k, fun k -> IList_size (kinfo, k))
  | IEmpty_set (kinfo, a, k), s ->
      set_t loc a >|? fun b ->
      let s = Item_t (b, s) in
      Ex_splitted_kinstr (s, k, fun k -> IEmpty_set (kinfo, a, k))
  | ISet_iter (kinfo, a, body, k), Item_t (_b, s) ->
      let s' = Item_t (a, s) in
      ok
      @@ Ex_splitted_loop_may_fail
           (s', body, s, k, fun body k -> ISet_iter (kinfo, a, body, k))
  | ISet_mem (kinfo, k), Item_t (_, Item_t (_, s)) ->
      let s = Item_t (bool_t, s) in
      ok @@ Ex_splitted_kinstr (s, k, fun k -> ISet_mem (kinfo, k))
  | ISet_update (kinfo, k), Item_t (_, Item_t (_, s)) ->
      ok @@ Ex_splitted_kinstr (s, k, fun k -> ISet_update (kinfo, k))
  | ISet_size (kinfo, k), Item_t (_, s) ->
      let s = Item_t (nat_t, s) in
      ok @@ Ex_splitted_kinstr (s, k, fun k -> ISet_size (kinfo, k))
  | IEmpty_map (kinfo, cty, vty, k), s ->
      map_t loc cty vty >|? fun m ->
      let s = Item_t (m, s) in
      Ex_splitted_kinstr (s, k, fun k -> IEmpty_map (kinfo, cty, vty, k))
  | IMap_map (kinfo, key_ty, body, k), Item_t (Map_t (kty, vty, _meta), s) ->
      pair_t loc key_ty vty >|? fun (Ty_ex_c p) ->
      let s = Item_t (p, s) in
      Ex_splitted_loop_may_not_fail
        ( s,
          body,
          k,
          (fun (Item_t (b, s)) -> map_t loc kty b >|? fun m -> Item_t (m, s)),
          fun body k -> IMap_map (kinfo, key_ty, body, k) )
  | IMap_iter (kinfo, pair_ty, body, k), Item_t (_, s) ->
      let s' = Item_t (pair_ty, s) in
      ok
      @@ Ex_splitted_loop_may_fail
           (s', body, s, k, fun body k -> IMap_iter (kinfo, pair_ty, body, k))
  | IMap_mem (kinfo, k), Item_t (_, Item_t (_, s)) ->
      let s = Item_t (bool_t, s) in
      ok @@ Ex_splitted_kinstr (s, k, fun k -> IMap_mem (kinfo, k))
  | IMap_get (kinfo, k), Item_t (_, Item_t (Map_t (_kty, vty, _meta), s)) ->
      option_t loc vty >|? fun o ->
      let s = Item_t (o, s) in
      Ex_splitted_kinstr (s, k, fun k -> IMap_get (kinfo, k))
  | IMap_update (kinfo, k), Item_t (_, Item_t (_, s)) ->
      ok @@ Ex_splitted_kinstr (s, k, fun k -> IMap_update (kinfo, k))
  | IMap_get_and_update (kinfo, k), Item_t (_, s) ->
      ok @@ Ex_splitted_kinstr (s, k, fun k -> IMap_get_and_update (kinfo, k))
  | IMap_size (kinfo, k), Item_t (_, s) ->
      let s = Item_t (nat_t, s) in
      ok @@ Ex_splitted_kinstr (s, k, fun k -> IMap_size (kinfo, k))
  | IEmpty_big_map (kinfo, cty, ty, k), s ->
      big_map_t loc cty ty >|? fun b ->
      let s = Item_t (b, s) in
      Ex_splitted_kinstr (s, k, fun k -> IEmpty_big_map (kinfo, cty, ty, k))
  | IBig_map_mem (kinfo, k), Item_t (_, Item_t (_, s)) ->
      let s = Item_t (bool_t, s) in
      ok @@ Ex_splitted_kinstr (s, k, fun k -> IBig_map_mem (kinfo, k))
  | IBig_map_get (kinfo, k), Item_t (_, Item_t (Big_map_t (_kty, vty, _meta), s))
    ->
      option_t loc vty >|? fun o ->
      let s = Item_t (o, s) in
      Ex_splitted_kinstr (s, k, fun k -> IBig_map_get (kinfo, k))
  | IBig_map_update (kinfo, k), Item_t (_, Item_t (_, s)) ->
      ok @@ Ex_splitted_kinstr (s, k, fun k -> IBig_map_update (kinfo, k))
  | IBig_map_get_and_update (kinfo, k), Item_t (_, s) ->
      ok
      @@ Ex_splitted_kinstr (s, k, fun k -> IBig_map_get_and_update (kinfo, k))
  | IConcat_string (kinfo, k), Item_t (_, s) ->
      let s = Item_t (string_t, s) in
      ok @@ Ex_splitted_kinstr (s, k, fun k -> IConcat_string (kinfo, k))
  | IConcat_string_pair (kinfo, k), Item_t (_, s) ->
      ok @@ Ex_splitted_kinstr (s, k, fun k -> IConcat_string_pair (kinfo, k))
  | ISlice_string (kinfo, k), Item_t (_, Item_t (_, Item_t (_, s))) ->
      let s = Item_t (option_string_t, s) in
      ok @@ Ex_splitted_kinstr (s, k, fun k -> ISlice_string (kinfo, k))
  | IString_size (kinfo, k), Item_t (_, s) ->
      let s = Item_t (nat_t, s) in
      ok @@ Ex_splitted_kinstr (s, k, fun k -> IString_size (kinfo, k))
  | IConcat_bytes (kinfo, k), Item_t (_, s) ->
      let s = Item_t (bytes_t, s) in
      ok @@ Ex_splitted_kinstr (s, k, fun k -> IConcat_bytes (kinfo, k))
  | IConcat_bytes_pair (kinfo, k), Item_t (_, s) ->
      ok @@ Ex_splitted_kinstr (s, k, fun k -> IConcat_bytes_pair (kinfo, k))
  | ISlice_bytes (kinfo, k), Item_t (_, Item_t (_, Item_t (_, s))) ->
      let s = Item_t (option_bytes_t, s) in
      ok @@ Ex_splitted_kinstr (s, k, fun k -> ISlice_bytes (kinfo, k))
  | IBytes_size (kinfo, k), Item_t (_, s) ->
      let s = Item_t (nat_t, s) in
      ok @@ Ex_splitted_kinstr (s, k, fun k -> IBytes_size (kinfo, k))
  | IAdd_seconds_to_timestamp (kinfo, k), Item_t (_, s) ->
      ok
      @@ Ex_splitted_kinstr (s, k, fun k -> IAdd_seconds_to_timestamp (kinfo, k))
  | IAdd_timestamp_to_seconds (kinfo, k), Item_t (_, Item_t (_, s)) ->
      let s = Item_t (timestamp_t, s) in
      ok
      @@ Ex_splitted_kinstr (s, k, fun k -> IAdd_timestamp_to_seconds (kinfo, k))
  | ISub_timestamp_seconds (kinfo, k), Item_t (_, Item_t (_, s)) ->
      let s = Item_t (timestamp_t, s) in
      ok @@ Ex_splitted_kinstr (s, k, fun k -> ISub_timestamp_seconds (kinfo, k))
  | IDiff_timestamps (kinfo, k), Item_t (_, Item_t (_, s)) ->
      let s = Item_t (int_t, s) in
      ok @@ Ex_splitted_kinstr (s, k, fun k -> IDiff_timestamps (kinfo, k))
  | IAdd_tez (kinfo, k), Item_t (_, s) ->
      ok @@ Ex_splitted_kinstr (s, k, fun k -> IAdd_tez (kinfo, k))
  | ISub_tez (kinfo, k), Item_t (_, Item_t (_, s)) ->
      let s = Item_t (option_mutez_t, s) in
      ok @@ Ex_splitted_kinstr (s, k, fun k -> ISub_tez (kinfo, k))
  | ISub_tez_legacy (kinfo, k), Item_t (_, s) ->
      ok @@ Ex_splitted_kinstr (s, k, fun k -> ISub_tez_legacy (kinfo, k))
  | IMul_teznat (kinfo, k), Item_t (_, Item_t (_, s)) ->
      let s = Item_t (mutez_t, s) in
      ok @@ Ex_splitted_kinstr (s, k, fun k -> IMul_teznat (kinfo, k))
  | IMul_nattez (kinfo, k), Item_t (_, s) ->
      ok @@ Ex_splitted_kinstr (s, k, fun k -> IMul_nattez (kinfo, k))
  | IEdiv_teznat (kinfo, k), Item_t (_, Item_t (_, s)) ->
      let s = Item_t (option_pair_mutez_mutez_t, s) in
      ok @@ Ex_splitted_kinstr (s, k, fun k -> IEdiv_teznat (kinfo, k))
  | IEdiv_tez (kinfo, k), Item_t (_, Item_t (_, s)) ->
      let s = Item_t (option_pair_nat_mutez_t, s) in
      ok @@ Ex_splitted_kinstr (s, k, fun k -> IEdiv_tez (kinfo, k))
  | IOr (kinfo, k), Item_t (_, s) ->
      ok @@ Ex_splitted_kinstr (s, k, fun k -> IOr (kinfo, k))
  | IAnd (kinfo, k), Item_t (_, s) ->
      ok @@ Ex_splitted_kinstr (s, k, fun k -> IAnd (kinfo, k))
  | IXor (kinfo, k), Item_t (_, s) ->
      ok @@ Ex_splitted_kinstr (s, k, fun k -> IXor (kinfo, k))
  | INot (kinfo, k), s ->
      ok @@ Ex_splitted_kinstr (s, k, fun k -> INot (kinfo, k))
  | IIs_nat (kinfo, k), Item_t (_, s) ->
      let s = Item_t (option_nat_t, s) in
      ok @@ Ex_splitted_kinstr (s, k, fun k -> IIs_nat (kinfo, k))
  | INeg (kinfo, k), Item_t (_, s) ->
      let s = Item_t (int_t, s) in
      ok @@ Ex_splitted_kinstr (s, k, fun k -> INeg (kinfo, k))
  | IAbs_int (kinfo, k), Item_t (_, s) ->
      let s = Item_t (nat_t, s) in
      ok @@ Ex_splitted_kinstr (s, k, fun k -> IAbs_int (kinfo, k))
  | IInt_nat (kinfo, k), Item_t (_, s) ->
      let s = Item_t (int_t, s) in
      ok @@ Ex_splitted_kinstr (s, k, fun k -> IInt_nat (kinfo, k))
  | IAdd_int (kinfo, k), Item_t (_, Item_t (_, s)) ->
      let s = Item_t (int_t, s) in
      ok @@ Ex_splitted_kinstr (s, k, fun k -> IAdd_int (kinfo, k))
  | IAdd_nat (kinfo, k), Item_t (_, s) ->
      ok @@ Ex_splitted_kinstr (s, k, fun k -> IAdd_nat (kinfo, k))
  | ISub_int (kinfo, k), Item_t (_, Item_t (_, s)) ->
      let s = Item_t (int_t, s) in
      ok @@ Ex_splitted_kinstr (s, k, fun k -> ISub_int (kinfo, k))
  | IMul_int (kinfo, k), Item_t (_, Item_t (_, s)) ->
      let s = Item_t (int_t, s) in
      ok @@ Ex_splitted_kinstr (s, k, fun k -> IMul_int (kinfo, k))
  | IMul_nat (kinfo, k), Item_t (_, s) ->
      ok @@ Ex_splitted_kinstr (s, k, fun k -> IMul_nat (kinfo, k))
  | IEdiv_int (kinfo, k), Item_t (_, Item_t (_, s)) ->
      let s = Item_t (option_pair_int_nat_t, s) in
      ok @@ Ex_splitted_kinstr (s, k, fun k -> IEdiv_int (kinfo, k))
  | IEdiv_nat (kinfo, k), Item_t (_, Item_t (a, s)) ->
      pair_t loc a nat_t >>? fun (Ty_ex_c p) ->
      option_t loc p >|? fun o ->
      let s = Item_t (o, s) in
      Ex_splitted_kinstr (s, k, fun k -> IEdiv_nat (kinfo, k))
  | ILsl_nat (kinfo, k), Item_t (_, s) ->
      ok @@ Ex_splitted_kinstr (s, k, fun k -> ILsl_nat (kinfo, k))
  | ILsr_nat (kinfo, k), Item_t (_, s) ->
      ok @@ Ex_splitted_kinstr (s, k, fun k -> ILsr_nat (kinfo, k))
  | IOr_nat (kinfo, k), Item_t (_, s) ->
      ok @@ Ex_splitted_kinstr (s, k, fun k -> IOr_nat (kinfo, k))
  | IAnd_nat (kinfo, k), Item_t (_, s) ->
      ok @@ Ex_splitted_kinstr (s, k, fun k -> IAnd_nat (kinfo, k))
  | IAnd_int_nat (kinfo, k), Item_t (_, s) ->
      ok @@ Ex_splitted_kinstr (s, k, fun k -> IAnd_int_nat (kinfo, k))
  | IXor_nat (kinfo, k), Item_t (_, s) ->
      ok @@ Ex_splitted_kinstr (s, k, fun k -> IXor_nat (kinfo, k))
  | INot_int (kinfo, k), Item_t (_, s) ->
      let s = Item_t (int_t, s) in
      ok @@ Ex_splitted_kinstr (s, k, fun k -> INot_int (kinfo, k))
  | IIf {kinfo; branch_if_true; branch_if_false; k}, Item_t (_, s) ->
      ok
      @@ Ex_splitted_if
           ( s,
             branch_if_true,
             s,
             branch_if_false,
             k,
             fun branch_if_true branch_if_false k ->
               IIf {kinfo; branch_if_true; branch_if_false; k} )
  | ILoop (kinfo, body, k), Item_t (_, s) ->
      ok
      @@ Ex_splitted_loop_may_fail
           (s, body, s, k, fun body k -> ILoop (kinfo, body, k))
  | ILoop_left (kinfo, kl, kr), Item_t (Union_t (a, b, _meta, _), s) ->
      ok
      @@ Ex_splitted_loop_may_fail
           ( Item_t (a, s),
             kl,
             Item_t (b, s),
             kr,
             fun kl kr -> ILoop_left (kinfo, kl, kr) )
  | IDip (kinfo, body, k), Item_t (a, s) ->
      ok
      @@ Ex_splitted_loop_may_not_fail
           ( s,
             body,
             k,
             (fun s -> ok @@ Item_t (a, s)),
             fun body k -> IDip (kinfo, body, k) )
  | IExec (kinfo, k), Item_t (_, Item_t (Lambda_t (_, b, _meta), s)) ->
      let s = Item_t (b, s) in
      ok @@ Ex_splitted_kinstr (s, k, fun k -> IExec (kinfo, k))
  | ( IApply (kinfo, ty, k),
      Item_t (_, Item_t (Lambda_t (Pair_t (_, a, _, _), b, _), s)) ) ->
      lambda_t loc a b >|? fun l ->
      let s = Item_t (l, s) in
      Ex_splitted_kinstr (s, k, fun k -> IApply (kinfo, ty, k))
  | ILambda (kinfo, l, k), s ->
      let (Lam (desc, _)) = l in
      let (Item_t (a, Bot_t)) = desc.kbef in
      let (Item_t (b, Bot_t)) = desc.kaft in
      lambda_t loc a b >|? fun lam ->
      let s = Item_t (lam, s) in
      Ex_splitted_kinstr (s, k, fun k -> ILambda (kinfo, l, k))
  | IFailwith (kinfo, i, ty), _ ->
      ok
      @@ Ex_splitted_failwith (kinfo, i, ty, {cast = IFailwith (kinfo, i, ty)})
  | ICompare (kinfo, ty, k), Item_t (_, Item_t (_, s)) ->
      let s = Item_t (int_t, s) in
      ok @@ Ex_splitted_kinstr (s, k, fun k -> ICompare (kinfo, ty, k))
  | IEq (kinfo, k), Item_t (_, s) ->
      let s = Item_t (bool_t, s) in
      ok @@ Ex_splitted_kinstr (s, k, fun k -> IEq (kinfo, k))
  | INeq (kinfo, k), Item_t (_, s) ->
      let s = Item_t (bool_t, s) in
      ok @@ Ex_splitted_kinstr (s, k, fun k -> INeq (kinfo, k))
  | ILt (kinfo, k), Item_t (_, s) ->
      let s = Item_t (bool_t, s) in
      ok @@ Ex_splitted_kinstr (s, k, fun k -> ILt (kinfo, k))
  | IGt (kinfo, k), Item_t (_, s) ->
      let s = Item_t (bool_t, s) in
      ok @@ Ex_splitted_kinstr (s, k, fun k -> IGt (kinfo, k))
  | ILe (kinfo, k), Item_t (_, s) ->
      let s = Item_t (bool_t, s) in
      ok @@ Ex_splitted_kinstr (s, k, fun k -> ILe (kinfo, k))
  | IGe (kinfo, k), Item_t (_, s) ->
      let s = Item_t (bool_t, s) in
      ok @@ Ex_splitted_kinstr (s, k, fun k -> IGe (kinfo, k))
  | IAddress (kinfo, k), Item_t (_, s) ->
      let s = Item_t (address_t, s) in
      ok @@ Ex_splitted_kinstr (s, k, fun k -> IAddress (kinfo, k))
  | IContract (kinfo, ty, code, k), Item_t (_, s) ->
      contract_t loc ty >>? fun c ->
      option_t loc c >|? fun o ->
      let s = Item_t (o, s) in
      Ex_splitted_kinstr (s, k, fun k -> IContract (kinfo, ty, code, k))
  | ITransfer_tokens (kinfo, k), Item_t (_, Item_t (_, Item_t (_, s))) ->
      let s = Item_t (operation_t, s) in
      ok @@ Ex_splitted_kinstr (s, k, fun k -> ITransfer_tokens (kinfo, k))
  | ( IView (kinfo, (View_signature {output_ty; _} as view_signature), k),
      Item_t (_, Item_t (_, s)) ) ->
      option_t loc output_ty >|? fun b ->
      let s = Item_t (b, s) in
      Ex_splitted_kinstr (s, k, fun k -> IView (kinfo, view_signature, k))
  | IImplicit_account (kinfo, k), Item_t (_, s) ->
      let s = Item_t (contract_unit_t, s) in
      ok @@ Ex_splitted_kinstr (s, k, fun k -> IImplicit_account (kinfo, k))
  | ( ICreate_contract {kinfo; storage_type; code; k},
      Item_t (_, Item_t (_, Item_t (_, s))) ) ->
      let s = Item_t (operation_t, Item_t (address_t, s)) in
      ok
      @@ Ex_splitted_kinstr
           (s, k, fun k -> ICreate_contract {kinfo; storage_type; code; k})
  | ISet_delegate (kinfo, k), Item_t (_, s) ->
      let s = Item_t (operation_t, s) in
      ok @@ Ex_splitted_kinstr (s, k, fun k -> ISet_delegate (kinfo, k))
  | INow (kinfo, k), s ->
      let s = Item_t (timestamp_t, s) in
      ok @@ Ex_splitted_kinstr (s, k, fun k -> INow (kinfo, k))
  | IBalance (kinfo, k), s ->
      let s = Item_t (mutez_t, s) in
      ok @@ Ex_splitted_kinstr (s, k, fun k -> IBalance (kinfo, k))
  | ILevel (kinfo, k), s ->
      let s = Item_t (nat_t, s) in
      ok @@ Ex_splitted_kinstr (s, k, fun k -> ILevel (kinfo, k))
  | ICheck_signature (kinfo, k), Item_t (_, Item_t (_, Item_t (_, s))) ->
      let s = Item_t (bool_t, s) in
      ok @@ Ex_splitted_kinstr (s, k, fun k -> ICheck_signature (kinfo, k))
  | IHash_key (kinfo, k), Item_t (_, s) ->
      let s = Item_t (key_hash_t, s) in
      ok @@ Ex_splitted_kinstr (s, k, fun k -> IHash_key (kinfo, k))
  | IPack (kinfo, ty, k), Item_t (_, s) ->
      let s = Item_t (bytes_t, s) in
      ok @@ Ex_splitted_kinstr (s, k, fun k -> IPack (kinfo, ty, k))
  | IUnpack (kinfo, ty, k), Item_t (_, s) ->
      option_t loc ty >|? fun o ->
      let s = Item_t (o, s) in
      Ex_splitted_kinstr (s, k, fun k -> IUnpack (kinfo, ty, k))
  | IBlake2b (kinfo, k), s ->
      ok @@ Ex_splitted_kinstr (s, k, fun k -> IBlake2b (kinfo, k))
  | ISha256 (kinfo, k), s ->
      ok @@ Ex_splitted_kinstr (s, k, fun k -> ISha256 (kinfo, k))
  | ISha512 (kinfo, k), s ->
      ok @@ Ex_splitted_kinstr (s, k, fun k -> ISha512 (kinfo, k))
  | ISource (kinfo, k), s ->
      let s = Item_t (address_t, s) in
      ok @@ Ex_splitted_kinstr (s, k, fun k -> ISource (kinfo, k))
  | ISender (kinfo, k), s ->
      let s = Item_t (address_t, s) in
      ok @@ Ex_splitted_kinstr (s, k, fun k -> ISender (kinfo, k))
  | ISelf (kinfo, ty, ep, k), s ->
      contract_t loc ty >|? fun c ->
      let s = Item_t (c, s) in
      Ex_splitted_kinstr (s, k, fun k -> ISelf (kinfo, ty, ep, k))
  | ISelf_address (kinfo, k), s ->
      let s = Item_t (address_t, s) in
      ok @@ Ex_splitted_kinstr (s, k, fun k -> ISelf_address (kinfo, k))
  | IAmount (kinfo, k), s ->
      let s = Item_t (mutez_t, s) in
      ok @@ Ex_splitted_kinstr (s, k, fun k -> IAmount (kinfo, k))
  | ISapling_empty_state (kinfo, memo_size, k), s ->
      let s = Item_t (sapling_state_t ~memo_size, s) in
      ok
      @@ Ex_splitted_kinstr
           (s, k, fun k -> ISapling_empty_state (kinfo, memo_size, k))
  | ( ISapling_verify_update_deprecated (kinfo, k),
      Item_t (_, Item_t (state_ty, s)) ) ->
      pair_t loc int_t state_ty >>? fun (Ty_ex_c pair_ty) ->
      option_t loc pair_ty >|? fun ty ->
      let s = Item_t (ty, s) in
      Ex_splitted_kinstr
        (s, k, fun k -> ISapling_verify_update_deprecated (kinfo, k))
  | ISapling_verify_update (kinfo, k), Item_t (_, Item_t (state_ty, s)) ->
      pair_t loc int_t state_ty >>? fun (Ty_ex_c int_state_ty) ->
      pair_t loc bytes_t int_state_ty >>? fun (Ty_ex_c pair_ty) ->
      option_t loc pair_ty >|? fun ty ->
      let s = Item_t (ty, s) in
      Ex_splitted_kinstr (s, k, fun k -> ISapling_verify_update (kinfo, k))
  | IDig (kinfo, n, p, k), s ->
      let (Item_t (b, s)) = stack_prefix_preservation_witness_split_input p s in
      let s = stack_prefix_preservation_witness_split_output p s in
      let s = Item_t (b, s) in
      ok @@ Ex_splitted_kinstr (s, k, fun k -> IDig (kinfo, n, p, k))
  | IDug (kinfo, n, p, k), Item_t (a, s) ->
      let s = stack_prefix_preservation_witness_split_input p s in
      let s = Item_t (a, s) in
      let s = stack_prefix_preservation_witness_split_output p s in
      ok @@ Ex_splitted_kinstr (s, k, fun k -> IDug (kinfo, n, p, k))
  | IDipn (kinfo, n, p, k1, k2), s ->
      let s = stack_prefix_preservation_witness_split_input p s in
      ok
      @@ Ex_splitted_loop_may_not_fail
           ( s,
             k1,
             k2,
             (fun s -> ok @@ stack_prefix_preservation_witness_split_output p s),
             fun k1 k2 -> IDipn (kinfo, n, p, k1, k2) )
  | IDropn (kinfo, n, p, k), s ->
      let s = stack_prefix_preservation_witness_split_input p s in
      ok @@ Ex_splitted_kinstr (s, k, fun k -> IDropn (kinfo, n, p, k))
  | IChainId (kinfo, k), s ->
      let s = Item_t (chain_id_t, s) in
      ok @@ Ex_splitted_kinstr (s, k, fun k -> IChainId (kinfo, k))
  | INever kinfo, Item_t (a, _) ->
      ok @@ Ex_splitted_failwith (kinfo, loc, a, {cast = INever kinfo})
  | IVoting_power (kinfo, k), Item_t (_, s) ->
      let s = Item_t (nat_t, s) in
      ok @@ Ex_splitted_kinstr (s, k, fun k -> IVoting_power (kinfo, k))
  | ITotal_voting_power (kinfo, k), s ->
      let s = Item_t (nat_t, s) in
      ok @@ Ex_splitted_kinstr (s, k, fun k -> ITotal_voting_power (kinfo, k))
  | IKeccak (kinfo, k), s ->
      ok @@ Ex_splitted_kinstr (s, k, fun k -> IKeccak (kinfo, k))
  | ISha3 (kinfo, k), s ->
      ok @@ Ex_splitted_kinstr (s, k, fun k -> ISha3 (kinfo, k))
  | IAdd_bls12_381_g1 (kinfo, k), Item_t (_, s) ->
      ok @@ Ex_splitted_kinstr (s, k, fun k -> IAdd_bls12_381_g1 (kinfo, k))
  | IAdd_bls12_381_g2 (kinfo, k), Item_t (_, s) ->
      ok @@ Ex_splitted_kinstr (s, k, fun k -> IAdd_bls12_381_g2 (kinfo, k))
  | IAdd_bls12_381_fr (kinfo, k), Item_t (_, s) ->
      ok @@ Ex_splitted_kinstr (s, k, fun k -> IAdd_bls12_381_fr (kinfo, k))
  | IMul_bls12_381_g1 (kinfo, k), Item_t (g1, Item_t (_, s)) ->
      let s = Item_t (g1, s) in
      ok @@ Ex_splitted_kinstr (s, k, fun k -> IMul_bls12_381_g1 (kinfo, k))
  | IMul_bls12_381_g2 (kinfo, k), Item_t (g2, Item_t (_, s)) ->
      let s = Item_t (g2, s) in
      ok @@ Ex_splitted_kinstr (s, k, fun k -> IMul_bls12_381_g2 (kinfo, k))
  | IMul_bls12_381_fr (kinfo, k), Item_t (_, s) ->
      ok @@ Ex_splitted_kinstr (s, k, fun k -> IMul_bls12_381_fr (kinfo, k))
  | IMul_bls12_381_z_fr (kinfo, k), Item_t (fr, Item_t (_, s)) ->
      let s = Item_t (fr, s) in
      ok @@ Ex_splitted_kinstr (s, k, fun k -> IMul_bls12_381_z_fr (kinfo, k))
  | IMul_bls12_381_fr_z (kinfo, k), Item_t (_, s) ->
      ok @@ Ex_splitted_kinstr (s, k, fun k -> IMul_bls12_381_fr_z (kinfo, k))
  | IInt_bls12_381_fr (kinfo, k), Item_t (_, s) ->
      let s = Item_t (int_t, s) in
      ok @@ Ex_splitted_kinstr (s, k, fun k -> IInt_bls12_381_fr (kinfo, k))
  | INeg_bls12_381_g1 (kinfo, k), s ->
      ok @@ Ex_splitted_kinstr (s, k, fun k -> INeg_bls12_381_g1 (kinfo, k))
  | INeg_bls12_381_g2 (kinfo, k), s ->
      ok @@ Ex_splitted_kinstr (s, k, fun k -> INeg_bls12_381_g2 (kinfo, k))
  | INeg_bls12_381_fr (kinfo, k), s ->
      ok @@ Ex_splitted_kinstr (s, k, fun k -> INeg_bls12_381_fr (kinfo, k))
  | IPairing_check_bls12_381 (kinfo, k), Item_t (_, s) ->
      let s = Item_t (bool_t, s) in
      ok
      @@ Ex_splitted_kinstr (s, k, fun k -> IPairing_check_bls12_381 (kinfo, k))
  | IComb (kinfo, n, p, k), s ->
      let rec aux :
          type a b s c d t.
          (a, b * s) stack_ty ->
          (a, b, s, c, d, t) comb_gadt_witness ->
          (c, d * t) stack_ty tzresult =
       fun s w ->
        match (w, s) with
        | Comb_one, s -> ok s
        | Comb_succ w, Item_t (a, s) ->
            aux s w >>? fun (Item_t (c, t)) ->
            pair_t loc a c >|? fun (Ty_ex_c p) -> Item_t (p, t)
      in
      aux s p >|? fun s ->
      Ex_splitted_kinstr (s, k, fun k -> IComb (kinfo, n, p, k))
  | IUncomb (kinfo, n, p, k), s ->
      let rec aux :
          type a b s c d t.
          (a, b * s) stack_ty ->
          (a, b, s, c, d, t) uncomb_gadt_witness ->
          (c, d * t) stack_ty =
       fun s w ->
        match (w, s) with
        | Uncomb_one, s -> s
        | Uncomb_succ w, Item_t (Pair_t (a, b, _meta, _), s) ->
            let s = aux (Item_t (b, s)) w in
            Item_t (a, s)
      in
      let s = aux s p in
      ok @@ Ex_splitted_kinstr (s, k, fun k -> IUncomb (kinfo, n, p, k))
  | IComb_get (kinfo, n, p, k), Item_t (c, s) ->
      let rec aux :
          type c cc a. (c, cc) ty -> (c, a) comb_get_gadt_witness -> a ty_ex_c =
       fun c w ->
        match (w, c) with
        | Comb_get_zero, c -> Ty_ex_c c
        | Comb_get_one, Pair_t (hd, _tl, _meta, _) -> Ty_ex_c hd
        | Comb_get_plus_two w, Pair_t (_hd, tl, _meta, _) -> aux tl w
      in
      let s =
        let (Ty_ex_c ty) = aux c p in
        Item_t (ty, s)
      in
      ok @@ Ex_splitted_kinstr (s, k, fun k -> IComb_get (kinfo, n, p, k))
  | IComb_set (kinfo, n, p, k), Item_t (a, Item_t (b, s)) ->
      let rec aux :
          type a b c ca cb.
          (a, ca) ty ->
          (b, cb) ty ->
          (a, b, c) comb_set_gadt_witness ->
          c ty_ex_c tzresult =
       fun a b w ->
        match (w, b) with
        | Comb_set_zero, _ -> ok (Ty_ex_c a)
        | Comb_set_one, Pair_t (_hd, tl, _meta, _) -> pair_t loc a tl
        | Comb_set_plus_two w, Pair_t (hd, tl, _meta, _) ->
            aux a tl w >>? fun (Ty_ex_c c) -> pair_t loc hd c
      in
      aux a b p >|? fun (Ty_ex_c c) ->
      let s = Item_t (c, s) in
      Ex_splitted_kinstr (s, k, fun k -> IComb_set (kinfo, n, p, k))
  | IDup_n (kinfo, n, p, k), s ->
      let rec aux :
          type a b s t.
          (a, b * s) stack_ty -> (a, b, s, t) dup_n_gadt_witness -> t ty_ex_c =
       fun s w ->
        match (w, s) with
        | Dup_n_succ w, Item_t (_, s) -> aux s w
        | Dup_n_zero, Item_t (a, _) -> Ty_ex_c a
      in
      let s =
        let (Ty_ex_c ty) = aux s p in
        Item_t (ty, s)
      in
      ok @@ Ex_splitted_kinstr (s, k, fun k -> IDup_n (kinfo, n, p, k))
  | ITicket (kinfo, cty, k), Item_t (_, Item_t (_, s)) ->
      ticket_t loc cty >|? fun t ->
      let s = Item_t (t, s) in
      Ex_splitted_kinstr (s, k, fun k -> ITicket (kinfo, cty, k))
  | IRead_ticket (kinfo, a, k), s ->
      pair_t loc a nat_t >>? fun (Ty_ex_c p) ->
      pair_t loc address_t p >|? fun (Ty_ex_c t) ->
      let s = Item_t (t, s) in
      Ex_splitted_kinstr (s, k, fun k -> IRead_ticket (kinfo, a, k))
  | ISplit_ticket (kinfo, k), Item_t (t, Item_t (_, s)) ->
      pair_t loc t t >>? fun (Ty_ex_c p) ->
      option_t loc p >|? fun o ->
      let s = Item_t (o, s) in
      Ex_splitted_kinstr (s, k, fun k -> ISplit_ticket (kinfo, k))
  | IJoin_tickets (kinfo, ty, k), Item_t (Pair_t (t, _t, _meta, _), s) ->
      option_t loc t >|? fun o ->
      let s = Item_t (o, s) in
      Ex_splitted_kinstr (s, k, fun k -> IJoin_tickets (kinfo, ty, k))
  | IOpen_chest (kinfo, k), Item_t (_, Item_t (_, Item_t (_, s))) ->
      let s = Item_t (union_bytes_bool_t, s) in
      ok @@ Ex_splitted_kinstr (s, k, fun k -> IOpen_chest (kinfo, k))
  | IMin_block_time (kinfo, k), s ->
      let s = Item_t (nat_t, s) in
      ok @@ Ex_splitted_kinstr (s, k, fun k -> IMin_block_time (kinfo, k))
  | IHalt kinfo, _s -> ok @@ Ex_splitted_halt kinfo
  | ILog (kinfo, _stack_ty, event, logger, k), s ->
      ok @@ Ex_splitted_log (s, k, fun k -> ILog (kinfo, s, event, logger, k))

let rec kinstr_final_stack_type :
    type a s r f.
    (a, s) stack_ty -> (a, s, r, f) kinstr -> (r, f) stack_ty option tzresult =
 fun s i ->
  kinstr_split s i >>? function
  | Ex_splitted_kinstr (s, k, _) -> kinstr_final_stack_type s k
  | Ex_splitted_log (s, k, _) -> kinstr_final_stack_type s k
  | Ex_splitted_loop_may_fail (_s, _body, sk, k, _) ->
      kinstr_final_stack_type sk k
  | Ex_splitted_loop_may_not_fail (s, body, k, f, _) -> (
      kinstr_final_stack_type s body >>? function
      | Some after_body ->
          f after_body >>? fun before_k -> kinstr_final_stack_type before_k k
      | None -> ok None)
  | Ex_splitted_if (sa, branch_a, sb, branch_b, k, _ii) -> (
      kinstr_final_stack_type sa branch_a >>? function
      | Some after_branch_a -> kinstr_final_stack_type after_branch_a k
      | None -> (
          kinstr_final_stack_type sb branch_b >>? function
          | Some after_branch_b -> kinstr_final_stack_type after_branch_b k
          | None -> ok None))
  | Ex_splitted_halt _ -> ok @@ Some s
  | Ex_splitted_failwith (_, _, _, {cast = _}) -> ok None

let kinstr_rewritek :
    type a s r f.
    (a, s) stack_ty ->
    (a, s, r, f) kinstr ->
    kinstr_rewritek ->
    (a, s, r, f) kinstr tzresult =
 fun s i f ->
  kinstr_split s i >>? function
  | Ex_splitted_kinstr (s, k, ii) -> ok @@ ii (f.apply s k)
  | Ex_splitted_log (_s, k, ii) -> ok @@ ii k
  | Ex_splitted_loop_may_fail (s, body, sk, k, ii) ->
      ok @@ ii (f.apply s body) (f.apply sk k)
  | Ex_splitted_loop_may_not_fail (s, body, k, sf, ii) ->
      (kinstr_final_stack_type s body >>? function
       | Some after_body -> sf after_body >|? fun before_k -> f.apply before_k k
       | None -> ok k)
      >|? fun k -> ii (f.apply s body) k
  | Ex_splitted_if (sa, branch_a, sb, branch_b, k, ii) ->
      (kinstr_final_stack_type sa branch_a >>? function
       | Some after_branch_a -> ok @@ f.apply after_branch_a k
       | None -> (
           kinstr_final_stack_type sb branch_b >>? function
           | Some after_branch_b -> ok @@ f.apply after_branch_b k
           | None -> ok k))
      >|? fun k -> ii (f.apply sa branch_a) (f.apply sb branch_b) k
  | Ex_splitted_halt kinfo -> ok @@ IHalt kinfo
  | Ex_splitted_failwith (kinfo, loc, ty, _) -> ok @@ IFailwith (kinfo, loc, ty)
