(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2019-2022 Nomadic Labs <contact@nomadic-labs.com>           *)
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

(** This module provides the gas costs for typechecking Michelson scripts,
    parsing and unparsing Michelson values, and interpreting Michelson
    instructions.
*)

open Alpha_context

module Cost_of : sig
  (* The [manager_operation] cost is consumed each time a manager
     operation (internal or external alike) is applied. This cost is
     meant to cover the resources used in {!Apply} either directly
     (dispatching on operation kinds) or indirectly (in particular in
     the production of operation results). *)
  val manager_operation : Gas.cost

  module Interpreter : sig
    val drop : Gas.cost

    val dup : Gas.cost

    val swap : Gas.cost

    val cons_some : Gas.cost

    val cons_none : Gas.cost

    val if_none : Gas.cost

    val opt_map : Gas.cost

    val cons_pair : Gas.cost

    val unpair : Gas.cost

    val car : Gas.cost

    val cdr : Gas.cost

    val cons_left : Gas.cost

    val cons_right : Gas.cost

    val if_left : Gas.cost

    val cons_list : Gas.cost

    val nil : Gas.cost

    val if_cons : Gas.cost

    (* The argument of this function is ignored when calculating gas cost. *)
    val list_map : 'a Script_list.t -> Gas.cost

    val list_size : Gas.cost

    (* The argument of this function is ignored when calculating gas cost. *)
    val list_iter : 'a Script_list.t -> Gas.cost

    val empty_set : Gas.cost

    val set_iter : 'a Script_typed_ir.set -> Gas.cost

    val set_mem : 'a -> 'a Script_typed_ir.set -> Gas.cost

    val set_update : 'a -> 'a Script_typed_ir.set -> Gas.cost

    val set_size : Gas.cost

    val empty_map : Gas.cost

    val map_map : ('k, 'v) Script_typed_ir.map -> Gas.cost

    val map_iter : ('k, 'v) Script_typed_ir.map -> Gas.cost

    val map_mem : 'k -> ('k, 'v) Script_typed_ir.map -> Gas.cost

    val map_get : 'k -> ('k, 'v) Script_typed_ir.map -> Gas.cost

    val map_update : 'k -> ('k, 'v) Script_typed_ir.map -> Gas.cost

    val map_get_and_update : 'k -> ('k, 'v) Script_typed_ir.map -> Gas.cost

    val big_map_mem : (_, _) Script_typed_ir.big_map_overlay -> Gas.cost

    val big_map_get : (_, _) Script_typed_ir.big_map_overlay -> Gas.cost

    val big_map_update : (_, _) Script_typed_ir.big_map_overlay -> Gas.cost

    val big_map_get_and_update :
      (_, _) Script_typed_ir.big_map_overlay -> Gas.cost

    val map_size : Gas.cost

    val add_seconds_timestamp :
      'a Script_int.num -> Script_timestamp.t -> Gas.cost

    val add_timestamp_seconds :
      Script_timestamp.t -> 'a Script_int.num -> Gas.cost

    val sub_timestamp_seconds :
      Script_timestamp.t -> 'a Script_int.num -> Gas.cost

    val diff_timestamps : Script_timestamp.t -> Script_timestamp.t -> Gas.cost

    val concat_string_pair : Script_string.t -> Script_string.t -> Gas.cost

    val slice_string : Script_string.t -> Gas.cost

    val string_size : Gas.cost

    val concat_bytes_pair : bytes -> bytes -> Gas.cost

    val slice_bytes : bytes -> Gas.cost

    val bytes_size : Gas.cost

    val bytes_nat : Script_int.n Script_int.num -> Gas.cost

    val nat_bytes : bytes -> Gas.cost

    val bytes_int : Script_int.z Script_int.num -> Gas.cost

    val int_bytes : bytes -> Gas.cost

    val add_tez : Gas.cost

    val sub_tez : Gas.cost

    val sub_tez_legacy : Gas.cost

    val mul_teznat : Gas.cost

    val mul_nattez : Gas.cost

    val bool_or : Gas.cost

    val bool_and : Gas.cost

    val bool_xor : Gas.cost

    val bool_not : Gas.cost

    val is_nat : Gas.cost

    val abs_int : Script_int.z Script_int.num -> Gas.cost

    val int_nat : Gas.cost

    val neg : 'a Script_int.num -> Gas.cost

    val add_int : 'a Script_int.num -> 'b Script_int.num -> Gas.cost

    val add_nat :
      Script_int.n Script_int.num -> Script_int.n Script_int.num -> Gas.cost

    val sub_int : 'a Script_int.num -> 'b Script_int.num -> Gas.cost

    val mul_int : 'a Script_int.num -> 'b Script_int.num -> Gas.cost

    val mul_nat : Script_int.n Script_int.num -> 'a Script_int.num -> Gas.cost

    val ediv_teznat : 'a -> 'b Script_int.num -> Gas.cost

    val ediv_tez : Gas.cost

    val ediv_int : 'a Script_int.num -> 'b Script_int.num -> Gas.cost

    val ediv_nat : Script_int.n Script_int.num -> 'a Script_int.num -> Gas.cost

    val eq : Gas.cost

    val lsl_nat : 'a Script_int.num -> Gas.cost

    val lsr_nat : 'a Script_int.num -> Gas.cost

    val lsl_bytes : bytes -> Script_int.n Script_int.num -> Gas.cost

    val lsr_bytes : bytes -> Script_int.n Script_int.num -> Gas.cost

    val or_nat : 'a Script_int.num -> 'b Script_int.num -> Gas.cost

    val or_bytes : bytes -> bytes -> Gas.cost

    val and_nat : 'a Script_int.num -> 'b Script_int.num -> Gas.cost

    val and_int_nat :
      Script_int.z Script_int.num -> Script_int.n Script_int.num -> Gas.cost

    val and_bytes : bytes -> bytes -> Gas.cost

    val xor_nat : 'a Script_int.num -> 'b Script_int.num -> Gas.cost

    val xor_bytes : bytes -> bytes -> Gas.cost

    val not_int : 'a Script_int.num -> Gas.cost

    val not_bytes : bytes -> Gas.cost

    val if_ : Gas.cost

    val loop : Gas.cost

    val loop_left : Gas.cost

    val dip : Gas.cost

    type algo = Ed25519 | Secp256k1 | P256 | Bls

    val algo_of_public_key : Signature.public_key -> algo

    val algo_of_public_key_hash : Signature.public_key_hash -> algo

    val check_signature_on_algo : algo -> int -> Gas.cost

    val check_signature : Signature.public_key -> bytes -> Gas.cost

    val blake2b : bytes -> Gas.cost

    val sha256 : bytes -> Gas.cost

    val sha512 : bytes -> Gas.cost

    val dign : int -> Gas.cost

    val dugn : int -> Gas.cost

    val dipn : int -> Gas.cost

    val dropn : int -> Gas.cost

    val voting_power : Gas.cost

    val total_voting_power : Gas.cost

    val keccak : bytes -> Gas.cost

    val sha3 : bytes -> Gas.cost

    val add_bls12_381_g1 : Gas.cost

    val add_bls12_381_g2 : Gas.cost

    val add_bls12_381_fr : Gas.cost

    val mul_bls12_381_g1 : Gas.cost

    val mul_bls12_381_g2 : Gas.cost

    val mul_bls12_381_fr : Gas.cost

    val mul_bls12_381_fr_z : 'a Script_int.num -> Gas.cost

    val mul_bls12_381_z_fr : 'a Script_int.num -> Gas.cost

    val int_bls12_381_fr : Gas.cost

    val neg_bls12_381_g1 : Gas.cost

    val neg_bls12_381_g2 : Gas.cost

    val neg_bls12_381_fr : Gas.cost

    val neq : Gas.cost

    val pairing_check_bls12_381 : 'a Script_list.t -> Gas.cost

    val comb : int -> Gas.cost

    val uncomb : int -> Gas.cost

    val comb_get : int -> Gas.cost

    val comb_set : int -> Gas.cost

    val dupn : int -> Gas.cost

    val compare : 'a Script_typed_ir.comparable_ty -> 'a -> 'a -> Gas.cost

    val concat_string_precheck : 'a Script_list.t -> Gas.cost

    val concat_string :
      Saturation_repr.may_saturate Saturation_repr.t -> Gas.cost

    val concat_bytes :
      Saturation_repr.may_saturate Saturation_repr.t -> Gas.cost

    val halt : Gas.cost

    val push : Gas.cost

    val empty_big_map : Gas.cost

    val lt : Gas.cost

    val le : Gas.cost

    val gt : Gas.cost

    val ge : Gas.cost

    val exec : Gas.cost

    val apply : rec_flag:bool -> Gas.cost

    val lambda : Gas.cost

    val address : Gas.cost

    val contract : Gas.cost

    val view : Gas.cost

    val view_get : Script_string.t -> Script_typed_ir.view_map -> Gas.cost

    val view_update : Script_string.t -> Script_typed_ir.view_map -> Gas.cost

    val transfer_tokens : Gas.cost

    val implicit_account : Gas.cost

    val create_contract : Gas.cost

    val set_delegate : Gas.cost

    val balance : Gas.cost

    val level : Gas.cost

    val now : Gas.cost

    val min_block_time : Gas.cost

    val hash_key : Signature.Public_key.t -> Gas.cost

    val source : Gas.cost

    val sender : Gas.cost

    val self : Gas.cost

    val self_address : Gas.cost

    val amount : Gas.cost

    val chain_id : Gas.cost

    val unpack : bytes -> Gas.cost

    val unpack_failed : string -> Gas.cost

    val sapling_empty_state : Gas.cost

    val sapling_verify_update :
      inputs:int -> outputs:int -> bound_data:int -> Gas.cost

    val sapling_verify_update_deprecated : inputs:int -> outputs:int -> Gas.cost

    val ticket : Gas.cost

    val read_ticket : Gas.cost

    val split_ticket : 'a Script_int.num -> 'a Script_int.num -> Gas.cost

    val join_tickets :
      'a Script_typed_ir.comparable_ty ->
      'a Script_typed_ir.ticket ->
      'a Script_typed_ir.ticket ->
      Gas.cost

    val open_chest :
      chest:Script_typed_ir.Script_timelock.chest -> time:Z.t -> Gas.cost

    (** cost to generate one event emission internal operation *)
    val emit : Gas.cost

    module Control : sig
      val nil : Gas.cost

      val cons : Gas.cost

      val return : Gas.cost

      val view_exit : Gas.cost

      val map_head : Gas.cost

      val undip : Gas.cost

      val loop_in : Gas.cost

      val loop_in_left : Gas.cost

      val iter : Gas.cost

      val list_enter_body : 'a list -> int -> Gas.cost

      val list_exit_body : Gas.cost

      val map_enter_body : Gas.cost

      val map_exit_body : 'k -> ('k, 'v) Script_typed_ir.map -> Gas.cost
    end
  end

  module Typechecking : sig
    val public_key_optimized : Gas.cost

    val public_key_readable : Gas.cost

    val key_hash_optimized : Gas.cost

    val key_hash_readable : Gas.cost

    val signature_optimized : Gas.cost

    val signature_readable : Gas.cost

    val chain_id_optimized : Gas.cost

    val chain_id_readable : Gas.cost

    val address_optimized : Gas.cost

    val contract_optimized : Gas.cost

    val contract_readable : Gas.cost

    val bls12_381_g1 : Gas.cost

    val bls12_381_g2 : Gas.cost

    val bls12_381_fr : Gas.cost

    val check_printable : string -> Gas.cost

    val merge_cycle : Gas.cost

    val parse_type_cycle : Gas.cost

    val parse_instr_cycle : Gas.cost

    val parse_data_cycle : Gas.cost

    val check_dupable_cycle : Gas.cost

    val find_entrypoint_cycle : Gas.cost

    val bool : Gas.cost

    val unit : Gas.cost

    val timestamp_readable : string -> Gas.cost

    val tx_rollup_l2_address : Gas.cost

    val contract_exists : Gas.cost

    val proof_argument : int -> Gas.cost

    val chest_key : Gas.cost

    val chest : bytes:int -> Gas.cost
  end

  module Unparsing : sig
    val public_key_optimized : Gas.cost

    val public_key_readable : Gas.cost

    val key_hash_optimized : Gas.cost

    val key_hash_readable : Gas.cost

    val signature_optimized : Gas.cost

    val signature_readable : Gas.cost

    val chain_id_optimized : Gas.cost

    val chain_id_readable : Gas.cost

    val timestamp_readable : Gas.cost

    val address_optimized : Gas.cost

    val contract_optimized : Gas.cost

    val contract_readable : Gas.cost

    val bls12_381_g1 : Gas.cost

    val bls12_381_g2 : Gas.cost

    val bls12_381_fr : Gas.cost

    val unparse_type : ('a, _) Script_typed_ir.ty -> Gas.cost

    val unparse_instr_cycle : Gas.cost

    val unparse_data_cycle : Gas.cost

    val unit : Gas.cost

    val tx_rollup_l2_address : Gas.cost

    val operation : bytes -> Gas.cost

    val sapling_transaction : Sapling.transaction -> Gas.cost

    val sapling_transaction_deprecated : Sapling.Legacy.transaction -> Gas.cost

    val sapling_diff : Sapling.diff -> Gas.cost

    val chest_key : Gas.cost

    val chest : plaintext_size:int -> Gas.cost
  end
end

module Internal_for_tests : sig
  (** [int] value of {!Cost_of.manager_operation} *)
  val int_cost_of_manager_operation : int
end
