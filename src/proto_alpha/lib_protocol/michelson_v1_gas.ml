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
open Gas

module Cost_of = struct
  let log2 =
    let rec help acc = function 0 -> acc | n -> help (acc + 1) (n / 2) in
    help 1

  let z_bytes (z : Z.t) =
    let bits = Z.numbits z in
    (7 + bits) / 8

  let int_bytes (z : 'a Script_int.num) = z_bytes (Script_int.to_zint z)

  let timestamp_bytes (t : Script_timestamp.t) =
    let z = Script_timestamp.to_zint t in
    z_bytes z

  (* For now, returns size in bytes, but this could get more complicated... *)
  let rec size_of_comparable :
      type a b. (a, b) Script_typed_ir.comparable_struct -> a -> int =
   fun wit v ->
    match wit with
    | Int_key _ ->
        int_bytes v
    | Nat_key _ ->
        int_bytes v
    | String_key _ ->
        String.length v
    | Bytes_key _ ->
        MBytes.length v
    | Bool_key _ ->
        8
    | Key_hash_key _ ->
        Signature.Public_key_hash.size
    | Timestamp_key _ ->
        timestamp_bytes v
    | Address_key _ ->
        Signature.Public_key_hash.size
    | Mutez_key _ ->
        8
    | Pair_key ((l, _), (r, _), _) ->
        let (lval, rval) = v in
        size_of_comparable l lval + size_of_comparable r rval

  let string length = alloc_bytes_cost length

  let bytes length = alloc_mbytes_cost length

  let manager_operation = step_cost 10_000

  module Legacy = struct
    let zint z = alloc_bits_cost (Z.numbits z)

    let z_to_int64 = step_cost 2 +@ alloc_cost 1

    let hash data len = (10 *@ step_cost (MBytes.length data)) +@ bytes len

    let set_access : type elt. elt -> elt Script_typed_ir.set -> int =
     fun _key (module Box) -> log2 @@ Box.size

    let set_update key _presence set = set_access key set *@ alloc_cost 3
  end

  (* FIXME: hardcoded constant, available in next environment version.
     Set to a reasonable upper bound. *)
  let public_key_size = 64

  module Generated_costs_007 = struct
    (* Automatically generated costs functions. *)

    (* model N_Abs_int *)
    (* Approximating 0.068306 x term *)
    let cost_N_Abs_int size =
      let v0 = size in
      22 + (v0 lsr 4)

    (* model N_Add_intint *)
    (* Approximating 0.082158 x term *)
    let cost_N_Add_intint size1 size2 =
      let v0 = Compare.Int.max size1 size2 in
      0 + ((v0 lsr 4) + (v0 lsr 6))

    (* model N_Add_tez *)
    let cost_N_Add_tez = 95

    (* model N_And *)
    let cost_N_And = 91

    (* model N_And_nat *)
    (* Approximating 0.079325 x term *)
    let cost_N_And_nat size1 size2 =
      let v0 = Compare.Int.min size1 size2 in
      93 + ((v0 lsr 4) + (v0 lsr 6))

    (* model N_Blake2b *)
    (* Approximating 1.366428 x term *)
    let cost_N_Blake2b size =
      let v0 = size in
      519 + (v0 + (v0 lsr 2))

    (* model N_Car *)
    let cost_N_Car = 73

    (* model N_Cdr *)
    let cost_N_Cdr = 82

    (* model N_Check_signature_ed25519 *)
    (* Approximating 1.372685 x term *)
    let cost_N_Check_signature_ed25519 size =
      let v0 = size in
      267512 + (v0 + (v0 lsr 2))

    (* model N_Check_signature_p256 *)
    (* Approximating 1.385771 x term *)
    let cost_N_Check_signature_p256 size =
      let v0 = size in
      590829 + (v0 + (v0 lsr 2) + (v0 lsr 3))

    (* model N_Check_signature_secp256k1 *)
    (* Approximating 1.372411 x term *)
    let cost_N_Check_signature_secp256k1 size =
      let v0 = size in
      55348 + (v0 + (v0 lsr 2))

    (* model N_Compare_address *)
    let cost_N_Compare_address size1 size2 =
      0 + (2 * Compare.Int.min size1 size2)

    (* model N_Compare_bool *)
    let cost_N_Compare_bool size1 size2 =
      0 + (128 * Compare.Int.min size1 size2)

    (* model N_Compare_int *)
    (* Approximating 0.073657 x term *)
    let cost_N_Compare_int size1 size2 =
      let v0 = Compare.Int.min size1 size2 in
      152 + ((v0 lsr 4) + (v0 lsr 7))

    (* model N_Compare_key_hash *)
    let cost_N_Compare_key_hash size1 size2 =
      0 + (2 * Compare.Int.min size1 size2)

    (* model N_Compare_mutez *)
    let cost_N_Compare_mutez size1 size2 =
      0 + (13 * Compare.Int.min size1 size2)

    (* model N_Compare_string *)
    (* Approximating 0.039389 x term *)
    let cost_N_Compare_string size1 size2 =
      let v0 = Compare.Int.min size1 size2 in
      121 + ((v0 lsr 5) + (v0 lsr 7))

    (* model N_Compare_timestamp *)
    (* Approximating 0.072483 x term *)
    let cost_N_Compare_timestamp size1 size2 =
      let v0 = Compare.Int.min size1 size2 in
      141 + ((v0 lsr 4) + (v0 lsr 7))

    (* model N_Concat_string_pair *)
    (* Approximating 0.068808 x term *)
    let cost_N_Concat_string_pair size1 size2 =
      let v0 = size1 + size2 in
      0 + (v0 lsr 4)

    (* model N_Cons_list *)
    let cost_N_Cons_list = 89

    (* model N_Cons_none *)
    let cost_N_Cons_none = 82

    (* model N_Cons_pair *)
    let cost_N_Cons_pair = 82

    (* model N_Cons_some *)
    let cost_N_Cons_some = 74

    (* model N_Const *)
    let cost_N_Const = 85

    (* model N_Dig *)
    let cost_N_Dig size = 106 + (4 * size)

    (* model N_Dip *)
    let cost_N_Dip = 100

    (* model N_DipN *)
    let cost_N_DipN size = 111 + (4 * size)

    (* model N_Drop *)
    let cost_N_Drop = 81

    (* model N_DropN *)
    let cost_N_DropN size = 110 + (4 * size)

    (* model N_Dug *)
    let cost_N_Dug size = 120 + (4 * size)

    (* model N_Dup *)
    let cost_N_Dup = 75

    let indic_lt x y = if Compare.Int.(x < y) then 1 else 0

    (* model N_Ediv_natnat *)
    (* Approximating 0.001599 x term *)
    let cost_N_Ediv_natnat size1 size2 =
      let q = size1 - size2 in
      let v0 = indic_lt size2 size1 * q * size2 in
      (v0 lsr 10) + (v0 lsr 11) + (v0 lsr 13) + 153030

    (* model N_Ediv_tez *)
    let cost_N_Ediv_tez = 208

    (* model N_Ediv_teznat *)
    let cost_N_Ediv_teznat size1 size2 =
      let q = size1 - size2 in
      (30 * (indic_lt size2 size1 * q * size2)) + 0

    (* model N_Empty_map *)
    let cost_N_Empty_map = 212

    (* model N_Empty_set *)
    let cost_N_Empty_set = 238

    (* model N_Eq *)
    let cost_N_Eq = 85

    (* model N_If *)
    let cost_N_If = 57

    (* model N_If_cons *)
    let cost_N_If_cons = 113

    (* model N_If_left *)
    let cost_N_If_left = 91

    (* model N_If_none *)
    let cost_N_If_none = 80

    (* model N_Int_nat *)
    let cost_N_Int_nat = 84

    (* model N_Is_nat *)
    let cost_N_Is_nat = 93

    (* model N_Left *)
    let cost_N_Left = 75

    (* model N_List_iter *)
    let cost_N_List_iter size = 570 + (7 * size)

    (* model N_List_map *)
    let cost_N_List_map size = 0 + (12 * size)

    (* model N_List_size *)
    let cost_N_List_size = 78

    (* model N_Loop *)
    let cost_N_Loop = 72

    (* model N_Loop_left *)
    let cost_N_Loop_left = 76

    (* model N_Lsl_nat *)
    (* Approximating 0.129443 x term *)
    let cost_N_Lsl_nat size =
      let v0 = size in
      159 + (v0 lsr 3)

    (* model N_Lsr_nat *)
    (* Approximating 0.129435 x term *)
    let cost_N_Lsr_nat size =
      let v0 = size in
      103 + (v0 lsr 3)

    (* model N_Map_get *)
    (* Approximating 0.057548 x term *)
    let cost_N_Map_get size1 size2 =
      let v0 = size1 * log2 (1 + size2) in
      (v0 lsr 5) + (v0 lsr 6) + (v0 lsr 7) + 0

    (* model N_Map_iter *)
    let cost_N_Map_iter size = 0 + (40 * size)

    (* model N_Map_map *)
    let cost_N_Map_map size = 0 + (761 * size)

    (* model N_Map_mem *)
    (* Approximating 0.058563 x term *)
    let cost_N_Map_mem size1 size2 =
      let v0 = size1 * log2 (1 + size2) in
      (v0 lsr 5) + (v0 lsr 6) + (v0 lsr 7) + 0

    (* model N_Map_size *)
    let cost_N_Map_size = 91

    (* model N_Map_update *)
    (* Approximating 0.119968 x term *)
    let cost_N_Map_update size1 size2 =
      let v0 = size1 * log2 (1 + size2) in
      (v0 lsr 4) + (v0 lsr 5) + (v0 lsr 6) + (v0 lsr 7) + 0

    (* model N_Mul_intint *)
    let cost_N_Mul_intint size1 size2 =
      let a = size1 + size2 in
      (1 * (a * log2 (1 + a))) + 0

    (* model N_Mul_teznat *)
    let cost_N_Mul_teznat size = 194 + (133 * size)

    (* model N_Neg_int *)
    (* Approximating 0.068419 x term *)
    let cost_N_Neg_int size =
      let v0 = size in
      26 + (v0 lsr 4)

    (* model N_Neq *)
    let cost_N_Neq = 91

    (* model N_Nil *)
    let cost_N_Nil = 80

    (* model N_Nop *)
    let cost_N_Nop = 71

    (* model N_Not *)
    let cost_N_Not = 90

    (* model N_Not_int *)
    (* Approximating 0.076564 x term *)
    let cost_N_Not_int size =
      let v0 = size in
      55 + ((v0 lsr 4) + (v0 lsr 7))

    (* model N_Or *)
    let cost_N_Or = 87

    (* model N_Or_nat *)
    (* Approximating 0.078718 x term *)
    let cost_N_Or_nat size1 size2 =
      let v0 = Compare.Int.max size1 size2 in
      0 + ((v0 lsr 4) + (v0 lsr 6))

    (* model N_Right *)
    let cost_N_Right = 80

    (* model N_Seq *)
    let cost_N_Seq = 58

    (* model N_Set_iter *)
    let cost_N_Set_iter size = 0 + (36 * size)

    (* model N_Set_mem *)
    (* Approximating 0.059410 x term *)
    let cost_N_Set_mem size1 size2 =
      let v0 = size1 * log2 (1 + size2) in
      (v0 lsr 5) + (v0 lsr 6) + (v0 lsr 7) + (v0 lsr 8) + 0

    (* model N_Set_size *)
    let cost_N_Set_size = 79

    (* model N_Set_update *)
    (* Approximating 0.126260 x term *)
    let cost_N_Set_update size1 size2 =
      let v0 = size1 * log2 (1 + size2) in
      (v0 lsr 3) + 0

    (* model N_Sha256 *)
    let cost_N_Sha256 size = 492 + (5 * size)

    (* model N_Sha512 *)
    let cost_N_Sha512 size = 457 + (3 * size)

    (* model N_Slice_string *)
    (* Approximating 0.067048 x term *)
    let cost_N_Slice_string size =
      let v0 = size in
      53 + (v0 lsr 4)

    (* model N_String_size *)
    let cost_N_String_size = 79

    (* model N_Sub_int *)
    (* Approximating 0.082399 x term *)
    let cost_N_Sub_int size1 size2 =
      let v0 = Compare.Int.max size1 size2 in
      0 + ((v0 lsr 4) + (v0 lsr 6))

    (* model N_Sub_tez *)
    let cost_N_Sub_tez = 83

    (* model N_Swap *)
    let cost_N_Swap = 72

    (* model N_Xor *)
    let cost_N_Xor = 100

    (* model N_Xor_nat *)
    (* Approximating 0.078258 x term *)
    let cost_N_Xor_nat size1 size2 =
      let v0 = Compare.Int.max size1 size2 in
      8 + ((v0 lsr 4) + (v0 lsr 6))

    (* model B58CHECK_DECODING_CHAIN_ID *)
    let cost_B58CHECK_DECODING_CHAIN_ID = 1495

    (* model B58CHECK_DECODING_PUBLIC_KEY_HASH_ed25519 *)
    let cost_B58CHECK_DECODING_PUBLIC_KEY_HASH_ed25519 = 3265

    (* model B58CHECK_DECODING_PUBLIC_KEY_HASH_p256 *)
    let cost_B58CHECK_DECODING_PUBLIC_KEY_HASH_p256 = 3153

    (* model B58CHECK_DECODING_PUBLIC_KEY_HASH_secp256k1 *)
    let cost_B58CHECK_DECODING_PUBLIC_KEY_HASH_secp256k1 = 3088

    (* model B58CHECK_DECODING_PUBLIC_KEY_ed25519 *)
    let cost_B58CHECK_DECODING_PUBLIC_KEY_ed25519 = 4220

    (* model B58CHECK_DECODING_PUBLIC_KEY_p256 *)
    let cost_B58CHECK_DECODING_PUBLIC_KEY_p256 = 28631

    (* model B58CHECK_DECODING_PUBLIC_KEY_secp256k1 *)
    let cost_B58CHECK_DECODING_PUBLIC_KEY_secp256k1 = 9419

    (* model B58CHECK_DECODING_SIGNATURE_ed25519 *)
    let cost_B58CHECK_DECODING_SIGNATURE_ed25519 = 6547

    (* model B58CHECK_DECODING_SIGNATURE_p256 *)
    let cost_B58CHECK_DECODING_SIGNATURE_p256 = 6543

    (* model B58CHECK_DECODING_SIGNATURE_secp256k1 *)
    let cost_B58CHECK_DECODING_SIGNATURE_secp256k1 = 6553

    (* model B58CHECK_ENCODING_CHAIN_ID *)
    let cost_B58CHECK_ENCODING_CHAIN_ID = 1610

    (* model B58CHECK_ENCODING_PUBLIC_KEY_HASH_ed25519 *)
    let cost_B58CHECK_ENCODING_PUBLIC_KEY_HASH_ed25519 = 3138

    (* model B58CHECK_ENCODING_PUBLIC_KEY_HASH_p256 *)
    let cost_B58CHECK_ENCODING_PUBLIC_KEY_HASH_p256 = 3728

    (* model B58CHECK_ENCODING_PUBLIC_KEY_HASH_secp256k1 *)
    let cost_B58CHECK_ENCODING_PUBLIC_KEY_HASH_secp256k1 = 3224

    (* model B58CHECK_ENCODING_PUBLIC_KEY_ed25519 *)
    let cost_B58CHECK_ENCODING_PUBLIC_KEY_ed25519 = 4503

    (* model B58CHECK_ENCODING_PUBLIC_KEY_p256 *)
    let cost_B58CHECK_ENCODING_PUBLIC_KEY_p256 = 5246

    (* model B58CHECK_ENCODING_PUBLIC_KEY_secp256k1 *)
    let cost_B58CHECK_ENCODING_PUBLIC_KEY_secp256k1 = 5018

    (* model B58CHECK_ENCODING_SIGNATURE_ed25519 *)
    let cost_B58CHECK_ENCODING_SIGNATURE_ed25519 = 8713

    (* model B58CHECK_ENCODING_SIGNATURE_p256 *)
    let cost_B58CHECK_ENCODING_SIGNATURE_p256 = 8716

    (* model B58CHECK_ENCODING_SIGNATURE_secp256k1 *)
    let cost_B58CHECK_ENCODING_SIGNATURE_secp256k1 = 8628

    (* model DECODING_CHAIN_ID *)
    let cost_DECODING_CHAIN_ID = 48

    (* model DECODING_PUBLIC_KEY_HASH_ed25519 *)
    let cost_DECODING_PUBLIC_KEY_HASH_ed25519 = 54

    (* model DECODING_PUBLIC_KEY_HASH_p256 *)
    let cost_DECODING_PUBLIC_KEY_HASH_p256 = 60

    (* model DECODING_PUBLIC_KEY_HASH_secp256k1 *)
    let cost_DECODING_PUBLIC_KEY_HASH_secp256k1 = 56

    (* model DECODING_PUBLIC_KEY_ed25519 *)
    let cost_DECODING_PUBLIC_KEY_ed25519 = 65

    (* model DECODING_PUBLIC_KEY_p256 *)
    let cost_DECODING_PUBLIC_KEY_p256 = 24394

    (* model DECODING_PUBLIC_KEY_secp256k1 *)
    let cost_DECODING_PUBLIC_KEY_secp256k1 = 5307

    (* model DECODING_SIGNATURE_ed25519 *)
    let cost_DECODING_SIGNATURE_ed25519 = 32

    (* model DECODING_SIGNATURE_p256 *)
    let cost_DECODING_SIGNATURE_p256 = 32

    (* model DECODING_SIGNATURE_secp256k1 *)
    let cost_DECODING_SIGNATURE_secp256k1 = 32

    (* model ENCODING_CHAIN_ID *)
    let cost_ENCODING_CHAIN_ID = 47

    (* model ENCODING_PUBLIC_KEY_HASH_ed25519 *)
    let cost_ENCODING_PUBLIC_KEY_HASH_ed25519 = 65

    (* model ENCODING_PUBLIC_KEY_HASH_p256 *)
    let cost_ENCODING_PUBLIC_KEY_HASH_p256 = 76

    (* model ENCODING_PUBLIC_KEY_HASH_secp256k1 *)
    let cost_ENCODING_PUBLIC_KEY_HASH_secp256k1 = 73

    (* model ENCODING_PUBLIC_KEY_ed25519 *)
    let cost_ENCODING_PUBLIC_KEY_ed25519 = 77

    (* model ENCODING_PUBLIC_KEY_p256 *)
    let cost_ENCODING_PUBLIC_KEY_p256 = 453

    (* model ENCODING_PUBLIC_KEY_secp256k1 *)
    let cost_ENCODING_PUBLIC_KEY_secp256k1 = 486

    (* model ENCODING_SIGNATURE_ed25519 *)
    let cost_ENCODING_SIGNATURE_ed25519 = 39

    (* model ENCODING_SIGNATURE_p256 *)
    let cost_ENCODING_SIGNATURE_p256 = 39

    (* model ENCODING_SIGNATURE_secp256k1 *)
    let cost_ENCODING_SIGNATURE_secp256k1 = 39

    (* model TIMESTAMP_READABLE_DECODING *)
    let cost_TIMESTAMP_READABLE_DECODING = 131

    (* model TIMESTAMP_READABLE_ENCODING *)
    let cost_TIMESTAMP_READABLE_ENCODING = 868

    (* model CHECK_PRINTABLE *)
    let cost_CHECK_PRINTABLE size = 14 + (10 * size)

    (* model MERGE_TYPES
       This is the estimated cost of one iteration of merge_types, extracted
       and copied manually from the parameter fit for the MERGE_TYPES benchmark
       (the model is parametric on the size of the type, which we don't have
       access to in O(1)). *)
    let cost_MERGE_TYPES = 130

    (* model TYPECHECKING_CODE
       This is the cost of one iteration of parse_instr, extracted by hand from the
       parameter fit for the TYPECHECKING_CODE benchmark. *)
    let cost_TYPECHECKING_CODE = 375

    (* model UNPARSING_CODE
       This is the cost of one iteration of unparse_instr, extracted by hand from the
       parameter fit for the UNPARSING_CODE benchmark. *)
    let cost_UNPARSING_CODE = 200

    (* model TYPECHECKING_DATA
       This is the cost of one iteration of parse_data, extracted by hand from the
       parameter fit for the TYPECHECKING_DATA benchmark. *)
    let cost_TYPECHECKING_DATA = 240

    (* model UNPARSING_DATA
       This is the cost of one iteration of unparse_data, extracted by hand from the
       parameter fit for the UNPARSING_DATA benchmark. *)
    let cost_UNPARSING_DATA = 140

    (* model PARSE_TYPE
       This is the cost of one iteration of parse_ty, extracted by hand from the
       parameter fit for the PARSE_TYPE benchmark. *)
    let cost_PARSE_TYPE = 167

    (* model UNPARSE_TYPE
       This is the cost of one iteration of unparse_ty, extracted by hand from the
       parameter fit for the UNPARSE_TYPE benchmark. *)
    let cost_UNPARSE_TYPE = 184
  end

  module Interpreter = struct
    open Generated_costs_007

    let drop = atomic_step_cost cost_N_Drop

    let dup = atomic_step_cost cost_N_Dup

    let swap = atomic_step_cost cost_N_Swap

    let push = atomic_step_cost cost_N_Const

    let cons_some = atomic_step_cost cost_N_Cons_some

    let cons_none = atomic_step_cost cost_N_Cons_none

    let if_none = atomic_step_cost cost_N_If_none

    let cons_pair = atomic_step_cost cost_N_Cons_pair

    let car = atomic_step_cost cost_N_Car

    let cdr = atomic_step_cost cost_N_Cdr

    let cons_left = atomic_step_cost cost_N_Left

    let cons_right = atomic_step_cost cost_N_Right

    let if_left = atomic_step_cost cost_N_If_left

    let cons_list = atomic_step_cost cost_N_Cons_list

    let nil = atomic_step_cost cost_N_Nil

    let if_cons = atomic_step_cost cost_N_If_cons

    let list_map : 'a Script_typed_ir.boxed_list -> Gas.cost =
     fun {length; _} -> atomic_step_cost (cost_N_List_map length)

    let list_size = atomic_step_cost cost_N_List_size

    let list_iter : 'a Script_typed_ir.boxed_list -> Gas.cost =
     fun {length; _} -> atomic_step_cost (cost_N_List_iter length)

    let empty_set = atomic_step_cost cost_N_Empty_set

    let set_iter (type a) ((module Box) : a Script_typed_ir.set) =
      atomic_step_cost (cost_N_Set_iter Box.size)

    let set_mem (type a) (elt : a) ((module Box) : a Script_typed_ir.set) =
      let elt_size = size_of_comparable Box.elt_ty elt in
      atomic_step_cost (cost_N_Set_mem elt_size Box.size)

    let set_update (type a) (elt : a) ((module Box) : a Script_typed_ir.set) =
      let elt_size = size_of_comparable Box.elt_ty elt in
      atomic_step_cost (cost_N_Set_update elt_size Box.size)

    let set_size = atomic_step_cost cost_N_Set_size

    let empty_map = atomic_step_cost cost_N_Empty_map

    let map_map (type k v) ((module Box) : (k, v) Script_typed_ir.map) =
      atomic_step_cost (cost_N_Map_map (snd Box.boxed))

    let map_iter (type k v) ((module Box) : (k, v) Script_typed_ir.map) =
      atomic_step_cost (cost_N_Map_iter (snd Box.boxed))

    let map_mem (type k v) (elt : k)
        ((module Box) : (k, v) Script_typed_ir.map) =
      let elt_size = size_of_comparable Box.key_ty elt in
      atomic_step_cost (cost_N_Map_mem elt_size (snd Box.boxed))

    let map_get (type k v) (elt : k)
        ((module Box) : (k, v) Script_typed_ir.map) =
      let elt_size = size_of_comparable Box.key_ty elt in
      atomic_step_cost (cost_N_Map_get elt_size (snd Box.boxed))

    let map_update (type k v) (elt : k)
        ((module Box) : (k, v) Script_typed_ir.map) =
      let elt_size = size_of_comparable Box.key_ty elt in
      atomic_step_cost (cost_N_Map_update elt_size (snd Box.boxed))

    let map_size = atomic_step_cost cost_N_Map_size

    let big_map_mep = map_mem

    let big_map_get = map_get

    let big_map_update = map_update

    let add_seconds_timestamp :
        'a Script_int.num -> Script_timestamp.t -> Gas.cost =
     fun seconds timestamp ->
      let seconds_bytes = int_bytes seconds in
      let timestamp_bytes = z_bytes (Script_timestamp.to_zint timestamp) in
      atomic_step_cost (cost_N_Add_intint seconds_bytes timestamp_bytes)

    let sub_seconds_timestamp :
        'a Script_int.num -> Script_timestamp.t -> Gas.cost =
     fun seconds timestamp ->
      let seconds_bytes = int_bytes seconds in
      let timestamp_bytes = z_bytes (Script_timestamp.to_zint timestamp) in
      atomic_step_cost (cost_N_Sub_int seconds_bytes timestamp_bytes)

    let diff_timestamps t1 t2 =
      let t1_bytes = z_bytes (Script_timestamp.to_zint t1) in
      let t2_bytes = z_bytes (Script_timestamp.to_zint t2) in
      atomic_step_cost (cost_N_Sub_int t1_bytes t2_bytes)

    let concat_string_pair s1 s2 =
      atomic_step_cost
        (cost_N_Concat_string_pair (String.length s1) (String.length s2))

    let slice_string s =
      atomic_step_cost (cost_N_Slice_string (String.length s))

    let string_size = atomic_step_cost cost_N_String_size

    let concat_bytes_pair b1 b2 =
      atomic_step_cost
        (cost_N_Concat_string_pair (MBytes.length b1) (MBytes.length b2))

    let slice_bytes b =
      atomic_step_cost (cost_N_Slice_string (MBytes.length b))

    let bytes_size = atomic_step_cost cost_N_String_size

    let add_tez = atomic_step_cost cost_N_Add_tez

    let sub_tez = atomic_step_cost cost_N_Sub_tez

    let mul_teznat n = atomic_step_cost (cost_N_Mul_teznat (int_bytes n))

    let bool_or = atomic_step_cost cost_N_Or

    let bool_and = atomic_step_cost cost_N_And

    let bool_xor = atomic_step_cost cost_N_Xor

    let bool_not = atomic_step_cost cost_N_Not

    let is_nat = atomic_step_cost cost_N_Is_nat

    let abs_int i = atomic_step_cost (cost_N_Abs_int (int_bytes i))

    let int_nat = atomic_step_cost cost_N_Int_nat

    let neg_int i = atomic_step_cost (cost_N_Neg_int (int_bytes i))

    let neg_nat n = atomic_step_cost (cost_N_Neg_int (int_bytes n))

    let add_bigint i1 i2 =
      atomic_step_cost (cost_N_Add_intint (int_bytes i1) (int_bytes i2))

    let sub_bigint i1 i2 =
      atomic_step_cost (cost_N_Sub_int (int_bytes i1) (int_bytes i2))

    let mul_bigint i1 i2 =
      atomic_step_cost (cost_N_Mul_intint (int_bytes i1) (int_bytes i2))

    let ediv_teznat _tez n =
      atomic_step_cost (cost_N_Ediv_teznat 8 (int_bytes n))

    let ediv_tez = atomic_step_cost cost_N_Ediv_tez

    let ediv_bigint i1 i2 =
      atomic_step_cost (cost_N_Ediv_natnat (int_bytes i1) (int_bytes i2))

    let eq = atomic_step_cost cost_N_Eq

    let lsl_nat shifted = atomic_step_cost (cost_N_Lsl_nat (int_bytes shifted))

    let lsr_nat shifted = atomic_step_cost (cost_N_Lsr_nat (int_bytes shifted))

    let or_nat n1 n2 =
      atomic_step_cost (cost_N_Or_nat (int_bytes n1) (int_bytes n2))

    let and_nat n1 n2 =
      atomic_step_cost (cost_N_And_nat (int_bytes n1) (int_bytes n2))

    let xor_nat n1 n2 =
      atomic_step_cost (cost_N_Xor_nat (int_bytes n1) (int_bytes n2))

    let not_int i = atomic_step_cost (cost_N_Not_int (int_bytes i))

    let not_nat = not_int

    let seq = atomic_step_cost cost_N_Seq

    let if_ = atomic_step_cost cost_N_If

    let loop = atomic_step_cost cost_N_Loop

    let loop_left = atomic_step_cost cost_N_Loop_left

    let dip = atomic_step_cost cost_N_Dip

    let check_signature (pkey : Signature.public_key) b =
      let cost =
        match pkey with
        | Ed25519 _ ->
            cost_N_Check_signature_ed25519 (MBytes.length b)
        | Secp256k1 _ ->
            cost_N_Check_signature_secp256k1 (MBytes.length b)
        | P256 _ ->
            cost_N_Check_signature_p256 (MBytes.length b)
      in
      atomic_step_cost cost

    let blake2b b = atomic_step_cost (cost_N_Blake2b (MBytes.length b))

    let sha256 b = atomic_step_cost (cost_N_Sha256 (MBytes.length b))

    let sha512 b = atomic_step_cost (cost_N_Sha512 (MBytes.length b))

    let dign n = atomic_step_cost (cost_N_Dig n)

    let dugn n = atomic_step_cost (cost_N_Dug n)

    let dipn n = atomic_step_cost (cost_N_DipN n)

    let dropn n = atomic_step_cost (cost_N_DropN n)

    let neq = atomic_step_cost cost_N_Neq

    let nop = atomic_step_cost cost_N_Nop

    (* --------------------------------------------------------------------- *)
    (* Semi-hand-crafted models *)
    let compare_bool = atomic_step_cost (cost_N_Compare_bool 1 1)

    let compare_string s1 s2 =
      atomic_step_cost
        (cost_N_Compare_string (String.length s1) (String.length s2))

    let compare_bytes b1 b2 =
      atomic_step_cost
        (cost_N_Compare_string (MBytes.length b1) (MBytes.length b2))

    let compare_mutez = atomic_step_cost (cost_N_Compare_mutez 8 8)

    let compare_int i1 i2 =
      atomic_step_cost (cost_N_Compare_int (int_bytes i1) (int_bytes i2))

    let compare_nat n1 n2 =
      atomic_step_cost (cost_N_Compare_int (int_bytes n1) (int_bytes n2))

    let compare_key_hash =
      let sz = Signature.Public_key_hash.size in
      atomic_step_cost (cost_N_Compare_key_hash sz sz)

    let compare_timestamp t1 t2 =
      atomic_step_cost
        (cost_N_Compare_timestamp
           (z_bytes (Script_timestamp.to_zint t1))
           (z_bytes (Script_timestamp.to_zint t2)))

    let compare_address =
      let sz = Signature.Public_key_hash.size + Chain_id.size in
      atomic_step_cost (cost_N_Compare_address sz sz)

    let rec compare :
        type a s. (a, s) Script_typed_ir.comparable_struct -> a -> a -> cost =
     fun ty x y ->
      match ty with
      | Bool_key _ ->
          compare_bool
      | String_key _ ->
          compare_string x y
      | Bytes_key _ ->
          compare_bytes x y
      | Mutez_key _ ->
          compare_mutez
      | Int_key _ ->
          compare_int x y
      | Nat_key _ ->
          compare_nat x y
      | Key_hash_key _ ->
          compare_key_hash
      | Timestamp_key _ ->
          compare_timestamp x y
      | Address_key _ ->
          compare_address
      | Pair_key ((tl, _), (tr, _), _) ->
          (* Reasonable over-approximation of the cost of lexicographic comparison. *)
          let (xl, xr) = x in
          let (yl, yr) = y in
          compare tl xl yl +@ compare tr xr yr

    (* --------------------------------------------------------------------- *)
    (* Hand-crafted models *)

    (* The cost functions below where not benchmarked, a cost model was derived
       from looking at similar instructions. *)

    (* Creating an empty big map involves converting a type to a comparable
       and allocating an empty map. Since the user already paied at typechecking
       time for writing this type, we charge a constant overhead here. *)
    let empty_big_map = atomic_step_cost (100 + cost_N_Empty_map)

    (* Cost for Concat_string is paid in two steps: when entering the interpreter,
       the user pays for the cost of computing the information necessary to compute
       the actual gas (so it's meta-gas): indeed, one needs to run through the
       list of strings to compute the total allocated cost.
       [concat_string_precheck] corresponds to the meta-gas cost of this computation.
     *)
    let concat_string_precheck (l : 'a Script_typed_ir.boxed_list) =
      (* we set the precheck to be slightly more expensive than cost_N_List_iter *)
      atomic_step_cost (l.length * 10)

    (* This is the cost of allocating a string and blitting existing ones into it. *)
    let concat_string total_bytes = atomic_step_cost (100 + (total_bytes / 10))

    (* Same story as Concat_string. *)
    let concat_bytes total_bytes = atomic_step_cost (100 + (total_bytes / 10))

    (* Cost of additional call to logger + overhead of setting up call to [interp]. *)
    let exec = atomic_step_cost 100

    (* Heavy computation happens in the [unparse_data], [unparse_ty]
       functions which are carbonated. We must account for allocating
       the Micheline lambda wrapper. *)
    let apply = atomic_step_cost 1000

    (* Pushing a pointer on the stack. *)
    let lambda = push

    (* Pusing an address on the stack. *)
    let address = push

    (* Most computation happens in [parse_contract_from_script], which is carbonated.
       Account for pushing on the stack. *)
    let contract = push

    (* Most computation happens in [collect_lazy_storage], [extract_lazy_storage_diff]
       and [unparse_data] which are carbonated. The instruction-specific overhead
       is mostly that of updating the internal nonce, which we approximate by the
       cost of a push. *)
    let transfer_tokens = Gas.(push +@ push)

    (* Wrapping a value and pushing it on the stack. *)
    let implicit_account = push

    (* As for [transfer_token], most computation happens elsewhere.
       We still account for the overhead of updating the internal_nonce. *)
    let create_contract = Gas.(push +@ push)

    (* Increments the internal_nonce counter. *)
    let set_delegate = Gas.(push +@ push)

    (* Cost of access taken care of in Contract_storage.get_balance_carbonated *)
    let balance = Gas.free

    (* Accessing the raw_context, Small arithmetic & pushing on the stack. *)
    let level = atomic_step_cost (2 * cost_N_Const)

    (* Same as [cost_level] *)
    let now = level

    (* Public keys are hashed with Blake2b *)
    let hash_key _pk = atomic_step_cost (cost_N_Blake2b public_key_size)

    (* Pushes on the stack an element from the [step_constants] record. *)
    let source = push

    (* Same as cost_source *)
    let sender = source

    (* Same as cost_source *)
    let self = source

    (* Same as cost_source *)
    let self_address = source

    (* Same as cost_source *)
    let amount = source

    (* Same as cost_source *)
    let chain_id = source

    (* Copy cost from 006. *)
    let unpack_failed bytes =
      (* We cannot instrument failed deserialization,
         so we take worst case fees: a set of size 1 bytes values. *)
      let len = MBytes.length bytes in
      (len *@ alloc_mbytes_cost 1)
      +@ (len *@ (log2 len *@ (alloc_cost 3 +@ step_cost 1)))
  end

  module Typechecking = struct
    let cycle = step_cost 1

    let bool = free

    let unit = free

    let string = string

    let bytes = bytes

    let z = Legacy.zint

    let int_of_string str =
      alloc_cost @@ Pervasives.( / ) (String.length str) 5

    let tez = step_cost 1 +@ alloc_cost 1

    let string_timestamp = step_cost 3 +@ alloc_cost 3

    let key = step_cost 3 +@ alloc_cost 3

    let key_hash = step_cost 1 +@ alloc_cost 1

    let signature = step_cost 1 +@ alloc_cost 1

    let chain_id = step_cost 1 +@ alloc_cost 1

    let contract = step_cost 5

    let get_script = step_cost 20 +@ alloc_cost 5

    let contract_exists = step_cost 15 +@ alloc_cost 5

    let pair = alloc_cost 2

    let union = alloc_cost 1

    let lambda = alloc_cost 5 +@ step_cost 3

    let some = alloc_cost 1

    let none = alloc_cost 0

    let list_element = alloc_cost 2 +@ step_cost 1

    let set_element size = log2 size *@ (alloc_cost 3 +@ step_cost 2)

    let map_element size = log2 size *@ (alloc_cost 4 +@ step_cost 2)

    let primitive_type = alloc_cost 1

    let one_arg_type = alloc_cost 2

    let two_arg_type = alloc_cost 3

    let operation b = bytes b

    let type_ nb_args = alloc_cost (nb_args + 1)

    (* Cost of parsing instruction, is cost of allocation of
       constructor + cost of constructor parameters + cost of
       allocation on the stack type *)
    let instr : type b a. (b, a) Script_typed_ir.instr -> cost =
     fun i ->
      let open Script_typed_ir in
      alloc_cost 1
      +@
      (* cost of allocation of constructor *)
      match i with
      | Drop ->
          alloc_cost 0
      | Dup ->
          alloc_cost 1
      | Swap ->
          alloc_cost 0
      | Const _ ->
          alloc_cost 1
      | Cons_pair ->
          alloc_cost 2
      | Car ->
          alloc_cost 1
      | Cdr ->
          alloc_cost 1
      | Cons_some ->
          alloc_cost 2
      | Cons_none _ ->
          alloc_cost 3
      | If_none _ ->
          alloc_cost 2
      | Cons_left ->
          alloc_cost 3
      | Cons_right ->
          alloc_cost 3
      | If_left _ ->
          alloc_cost 2
      | Cons_list ->
          alloc_cost 1
      | Nil ->
          alloc_cost 1
      | If_cons _ ->
          alloc_cost 2
      | List_map _ ->
          alloc_cost 5
      | List_iter _ ->
          alloc_cost 4
      | List_size ->
          alloc_cost 1
      | Empty_set _ ->
          alloc_cost 1
      | Set_iter _ ->
          alloc_cost 4
      | Set_mem ->
          alloc_cost 1
      | Set_update ->
          alloc_cost 1
      | Set_size ->
          alloc_cost 1
      | Empty_map _ ->
          alloc_cost 2
      | Map_map _ ->
          alloc_cost 5
      | Map_iter _ ->
          alloc_cost 4
      | Map_mem ->
          alloc_cost 1
      | Map_get ->
          alloc_cost 1
      | Map_update ->
          alloc_cost 1
      | Map_size ->
          alloc_cost 1
      | Empty_big_map _ ->
          alloc_cost 2
      | Big_map_mem ->
          alloc_cost 1
      | Big_map_get ->
          alloc_cost 1
      | Big_map_update ->
          alloc_cost 1
      | Concat_string ->
          alloc_cost 1
      | Concat_string_pair ->
          alloc_cost 1
      | Concat_bytes ->
          alloc_cost 1
      | Concat_bytes_pair ->
          alloc_cost 1
      | Slice_string ->
          alloc_cost 1
      | Slice_bytes ->
          alloc_cost 1
      | String_size ->
          alloc_cost 1
      | Bytes_size ->
          alloc_cost 1
      | Add_seconds_to_timestamp ->
          alloc_cost 1
      | Add_timestamp_to_seconds ->
          alloc_cost 1
      | Sub_timestamp_seconds ->
          alloc_cost 1
      | Diff_timestamps ->
          alloc_cost 1
      | Add_tez ->
          alloc_cost 1
      | Sub_tez ->
          alloc_cost 1
      | Mul_teznat ->
          alloc_cost 1
      | Mul_nattez ->
          alloc_cost 1
      | Ediv_teznat ->
          alloc_cost 1
      | Ediv_tez ->
          alloc_cost 1
      | Or ->
          alloc_cost 1
      | And ->
          alloc_cost 1
      | Xor ->
          alloc_cost 1
      | Not ->
          alloc_cost 1
      | Is_nat ->
          alloc_cost 1
      | Neg_nat ->
          alloc_cost 1
      | Neg_int ->
          alloc_cost 1
      | Abs_int ->
          alloc_cost 1
      | Int_nat ->
          alloc_cost 1
      | Add_intint ->
          alloc_cost 1
      | Add_intnat ->
          alloc_cost 1
      | Add_natint ->
          alloc_cost 1
      | Add_natnat ->
          alloc_cost 1
      | Sub_int ->
          alloc_cost 1
      | Mul_intint ->
          alloc_cost 1
      | Mul_intnat ->
          alloc_cost 1
      | Mul_natint ->
          alloc_cost 1
      | Mul_natnat ->
          alloc_cost 1
      | Ediv_intint ->
          alloc_cost 1
      | Ediv_intnat ->
          alloc_cost 1
      | Ediv_natint ->
          alloc_cost 1
      | Ediv_natnat ->
          alloc_cost 1
      | Lsl_nat ->
          alloc_cost 1
      | Lsr_nat ->
          alloc_cost 1
      | Or_nat ->
          alloc_cost 1
      | And_nat ->
          alloc_cost 1
      | And_int_nat ->
          alloc_cost 1
      | Xor_nat ->
          alloc_cost 1
      | Not_nat ->
          alloc_cost 1
      | Not_int ->
          alloc_cost 1
      | Seq _ ->
          alloc_cost 8
      | If _ ->
          alloc_cost 8
      | Loop _ ->
          alloc_cost 4
      | Loop_left _ ->
          alloc_cost 5
      | Dip _ ->
          alloc_cost 4
      | Exec ->
          alloc_cost 1
      | Apply _ ->
          alloc_cost 1
      | Lambda _ ->
          alloc_cost 2
      | Failwith _ ->
          alloc_cost 1
      | Nop ->
          alloc_cost 0
      | Compare _ ->
          alloc_cost 1
      | Eq ->
          alloc_cost 1
      | Neq ->
          alloc_cost 1
      | Lt ->
          alloc_cost 1
      | Gt ->
          alloc_cost 1
      | Le ->
          alloc_cost 1
      | Ge ->
          alloc_cost 1
      | Address ->
          alloc_cost 1
      | Contract _ ->
          alloc_cost 2
      | Transfer_tokens ->
          alloc_cost 1
      | Create_account ->
          alloc_cost 2
      | Implicit_account ->
          alloc_cost 1
      | Create_contract _ ->
          alloc_cost 8
      (* Deducted the cost of removed arguments manager, spendable and delegatable:
           - manager: key_hash = 1
           - spendable: bool = 0
           - delegatable: bool = 0
        *)
      | Create_contract_2 _ ->
          alloc_cost 7
      | Set_delegate ->
          alloc_cost 1
      | Now ->
          alloc_cost 1
      | Balance ->
          alloc_cost 1
      | Check_signature ->
          alloc_cost 1
      | Hash_key ->
          alloc_cost 1
      | Pack _ ->
          alloc_cost 2
      | Unpack _ ->
          alloc_cost 2
      | Blake2b ->
          alloc_cost 1
      | Sha256 ->
          alloc_cost 1
      | Sha512 ->
          alloc_cost 1
      | Steps_to_quota ->
          alloc_cost 1
      | Source ->
          alloc_cost 1
      | Sender ->
          alloc_cost 1
      | Self _ ->
          alloc_cost 2
      | Amount ->
          alloc_cost 1
      | Dig (n, _) ->
          n *@ alloc_cost 1 (* _ is a unary development of n *)
      | Dug (n, _) ->
          n *@ alloc_cost 1
      | Dipn (n, _, _) ->
          n *@ alloc_cost 1
      | Dropn (n, _) ->
          n *@ alloc_cost 1
      | ChainId ->
          alloc_cost 1
  end

  module Unparse = struct
    let prim_cost l annot = Script.prim_node_cost_nonrec_of_length l annot

    let seq_cost = Script.seq_node_cost_nonrec_of_length

    let string_cost length = Script.string_node_cost_of_length length

    let cycle = step_cost 1

    let bool = prim_cost 0 []

    let unit = prim_cost 0 []

    (* We count the length of strings and bytes to prevent hidden
       miscalculations due to non detectable expansion of sharing. *)
    let string s = Script.string_node_cost s

    let bytes s = Script.bytes_node_cost s

    let z i = Script.int_node_cost i

    let int i = Script.int_node_cost (Script_int.to_zint i)

    let tez = Script.int_node_cost_of_numbits 60 (* int64 bound *)

    let timestamp x = Script_timestamp.to_zint x |> Script_int.of_zint |> int

    let operation bytes = Script.bytes_node_cost bytes

    let chain_id = string_cost 15

    let key = string_cost 54

    let key_hash = string_cost 36

    let signature = string_cost 128

    let contract = string_cost 36

    let pair = prim_cost 2 []

    let union = prim_cost 1 []

    let some = prim_cost 1 []

    let none = prim_cost 0 []

    let list_element = alloc_cost 2

    let set_element = alloc_cost 2

    let map_element = alloc_cost 2

    let one_arg_type = prim_cost 1

    let two_arg_type = prim_cost 2
  end
end
