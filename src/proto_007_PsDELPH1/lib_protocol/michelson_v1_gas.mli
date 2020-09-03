(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

module Cost_of : sig
  val manager_operation : Gas.cost

  module Interpreter : sig
    val drop : Gas.cost

    val dup : Gas.cost

    val swap : Gas.cost

    val push : Gas.cost

    val cons_some : Gas.cost

    val cons_none : Gas.cost

    val if_none : Gas.cost

    val cons_pair : Gas.cost

    val car : Gas.cost

    val cdr : Gas.cost

    val cons_left : Gas.cost

    val cons_right : Gas.cost

    val if_left : Gas.cost

    val cons_list : Gas.cost

    val nil : Gas.cost

    val if_cons : Gas.cost

    val list_map : 'a Script_typed_ir.boxed_list -> Gas.cost

    val list_size : Gas.cost

    val list_iter : 'a Script_typed_ir.boxed_list -> Gas.cost

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

    val map_size : Gas.cost

    val add_seconds_timestamp :
      'a Script_int.num -> Script_timestamp.t -> Gas.cost

    val sub_seconds_timestamp :
      'a Script_int.num -> Script_timestamp.t -> Gas.cost

    val diff_timestamps : Script_timestamp.t -> Script_timestamp.t -> Gas.cost

    val concat_string_pair : string -> string -> Gas.cost

    val slice_string : string -> Gas.cost

    val string_size : Gas.cost

    val concat_bytes_pair : MBytes.t -> MBytes.t -> Gas.cost

    val slice_bytes : MBytes.t -> Gas.cost

    val bytes_size : Gas.cost

    val add_tez : Gas.cost

    val sub_tez : Gas.cost

    val mul_teznat : 'a Script_int.num -> Gas.cost

    val bool_or : Gas.cost

    val bool_and : Gas.cost

    val bool_xor : Gas.cost

    val bool_not : Gas.cost

    val is_nat : Gas.cost

    val abs_int : 'a Script_int.num -> Gas.cost

    val int_nat : Gas.cost

    val neg_int : 'a Script_int.num -> Gas.cost

    val neg_nat : 'a Script_int.num -> Gas.cost

    val add_bigint : 'a Script_int.num -> 'b Script_int.num -> Gas.cost

    val sub_bigint : 'a Script_int.num -> 'b Script_int.num -> Gas.cost

    val mul_bigint : 'a Script_int.num -> 'b Script_int.num -> Gas.cost

    val ediv_teznat : 'a -> 'b Script_int.num -> Gas.cost

    val ediv_tez : Gas.cost

    val ediv_bigint : 'a Script_int.num -> 'b Script_int.num -> Gas.cost

    val eq : Gas.cost

    val lsl_nat : 'a Script_int.num -> Gas.cost

    val lsr_nat : 'a Script_int.num -> Gas.cost

    val or_nat : 'a Script_int.num -> 'b Script_int.num -> Gas.cost

    val and_nat : 'a Script_int.num -> 'b Script_int.num -> Gas.cost

    val xor_nat : 'a Script_int.num -> 'b Script_int.num -> Gas.cost

    val not_int : 'a Script_int.num -> Gas.cost

    val not_nat : 'a Script_int.num -> Gas.cost

    val seq : Gas.cost

    val if_ : Gas.cost

    val loop : Gas.cost

    val loop_left : Gas.cost

    val dip : Gas.cost

    val check_signature : Signature.public_key -> MBytes.t -> Gas.cost

    val blake2b : MBytes.t -> Gas.cost

    val sha256 : MBytes.t -> Gas.cost

    val sha512 : MBytes.t -> Gas.cost

    val dign : int -> Gas.cost

    val dugn : int -> Gas.cost

    val dipn : int -> Gas.cost

    val dropn : int -> Gas.cost

    val neq : Gas.cost

    val nop : Gas.cost

    val empty_big_map : Gas.cost

    val compare : 'a Script_typed_ir.comparable_ty -> 'a -> 'a -> Gas.cost

    val concat_string_precheck : 'a Script_typed_ir.boxed_list -> Gas.cost

    val concat_string : Z.t -> Gas.cost

    val concat_bytes : Z.t -> Gas.cost

    val exec : Gas.cost

    val apply : Gas.cost

    val lambda : Gas.cost

    val address : Gas.cost

    val contract : Gas.cost

    val transfer_tokens : Gas.cost

    val implicit_account : Gas.cost

    val create_contract : Gas.cost

    val set_delegate : Gas.cost

    val balance : Gas.cost

    val level : Gas.cost

    val now : Gas.cost

    val hash_key : Signature.Public_key.t -> Gas.cost

    val source : Gas.cost

    val sender : Gas.cost

    val self : Gas.cost

    val self_address : Gas.cost

    val amount : Gas.cost

    val chain_id : Gas.cost

    val unpack_failed : MBytes.t -> Gas.cost
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

    val check_printable : string -> Gas.cost

    val merge_cycle : Gas.cost

    val parse_type_cycle : Gas.cost

    val parse_instr_cycle : Gas.cost

    val parse_data_cycle : Gas.cost

    val bool : Gas.cost

    val unit : Gas.cost

    val timestamp_readable : Gas.cost

    val contract : Gas.cost

    val contract_exists : Gas.cost

    val proof_argument : int -> Gas.cost
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

    val unparse_type_cycle : Gas.cost

    val unparse_instr_cycle : Gas.cost

    val unparse_data_cycle : Gas.cost

    val unit : Gas.cost

    val contract : Gas.cost

    val operation : MBytes.t -> Gas.cost
  end
end
