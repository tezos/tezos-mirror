(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2019-2020 Nomadic Labs <contact@nomadic-labs.com>           *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.dev>                     *)
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
module S = Saturation_repr

module Cost_of = struct
  module S_syntax = struct
    (* This is a good enough approximation. S.numbits 0 = 0 *)
    let log2 x = S.safe_int (1 + Z.numbits (S.to_z x))

    let ( + ) = S.add

    let ( * ) = S.mul

    let ( lsr ) = S.shift_right
  end

  let z_bytes (z : Z.t) =
    let bits = Z.numbits z in
    (7 + bits) / 8

  let int_bytes (z : 'a Script_int.num) = z_bytes (Script_int.to_zint z)

  let timestamp_bytes (t : Script_timestamp.t) =
    let z = Script_timestamp.to_zint t in
    z_bytes z

  (* Upper-bound on the time to compare the given value.
     For now, returns size in bytes, but this could get more complicated... *)
  let rec size_of_comparable :
      type a. a Script_typed_ir.comparable_ty -> a -> S.may_saturate S.t =
   fun wit v ->
    match (wit, v) with
    | (Unit_key _, _) ->
        S.safe_int 1
    | (Never_key _, _) ->
        .
    | (Int_key _, _) ->
        S.safe_int @@ int_bytes v
    | (Nat_key _, _) ->
        S.safe_int @@ int_bytes v
    | (Signature_key _, _) ->
        S.safe_int Signature.size
    | (String_key _, _) ->
        S.safe_int @@ String.length v
    | (Bytes_key _, _) ->
        S.safe_int @@ Bytes.length v
    | (Bool_key _, _) ->
        S.safe_int 8
    | (Key_hash_key _, _) ->
        S.safe_int Signature.Public_key_hash.size
    | (Key_key _, k) ->
        S.safe_int @@ Signature.Public_key.size k
    | (Timestamp_key _, _) ->
        S.safe_int @@ timestamp_bytes v
    | (Address_key _, _) ->
        S.safe_int Signature.Public_key_hash.size
    | (Mutez_key _, _) ->
        S.safe_int 8
    | (Chain_id_key _, _) ->
        S.safe_int Chain_id.size
    | (Pair_key ((l, _), (r, _), _), (lval, rval)) ->
        S.add (size_of_comparable l lval) (size_of_comparable r rval)
    | (Union_key ((t, _), _, _), L x) ->
        S.add (S.safe_int 1) (size_of_comparable t x)
    | (Union_key (_, (t, _), _), R x) ->
        S.add (S.safe_int 1) (size_of_comparable t x)
    | (Option_key _, None) ->
        S.safe_int 1
    | (Option_key (t, _), Some x) ->
        S.add (S.safe_int 1) (size_of_comparable t x)

  let manager_operation = step_cost @@ S.safe_int 1_000

  (* FIXME: hardcoded constant, available in next environment version.
     Set to a reasonable upper bound. *)
  let public_key_size = 64

  module Generated_costs_007 = struct
    (* Automatically generated costs functions. *)

    (* model N_Abs_int *)
    (* Approximating 0.068306 x term *)
    let cost_N_Abs_int size = S.safe_int @@ (80 + (size lsr 4))

    (* model N_Add_bls12_381_fr *)

    let cost_N_Add_bls12_381_fr = S.safe_int 200

    (* model N_Add_bls12_381_g1 *)

    let cost_N_Add_bls12_381_g1 = S.safe_int 8_300

    (* model N_Add_bls12_381_g2 *)

    let cost_N_Add_bls12_381_g2 = S.safe_int 11_450

    (* model N_Add_intint *)
    (* Approximating 0.082158 x term *)
    let cost_N_Add_intint size1 size2 =
      let v0 = Compare.Int.max size1 size2 in
      S.safe_int (80 + ((v0 lsr 4) + (v0 lsr 6)))

    (* model N_Add_tez *)
    let cost_N_Add_tez = S.safe_int 55

    (* model N_And *)
    let cost_N_And = S.safe_int 50

    (* model N_And_nat *)
    (* Approximating 0.079325 x term *)
    let cost_N_And_nat size1 size2 =
      let v0 = Compare.Int.min size1 size2 in
      S.safe_int (80 + ((v0 lsr 4) + (v0 lsr 6)))

    (* model N_Blake2b *)
    (* Approximating 1.366428 x term *)
    let cost_N_Blake2b size =
      let open S_syntax in
      let size = S.safe_int size in
      S.safe_int 500 + (size + (size lsr 2))

    (* model N_Car *)
    let cost_N_Car = S.safe_int 50

    (* model N_Cdr *)
    let cost_N_Cdr = S.safe_int 50

    (* model N_Check_signature_ed25519 *)
    (* Approximating 1.372685 x term *)
    let cost_N_Check_signature_ed25519 size =
      let open S_syntax in
      let size = S.safe_int size in
      S.safe_int 270_000 + (size + (size lsr 2))

    (* model N_Check_signature_p256 *)
    (* Approximating 1.385771 x term *)
    let cost_N_Check_signature_p256 size =
      let open S_syntax in
      let size = S.safe_int size in
      S.safe_int 600_000 + (size + (size lsr 2) + (size lsr 3))

    (* model N_Check_signature_secp256k1 *)
    (* Approximating 1.372411 x term *)
    let cost_N_Check_signature_secp256k1 size =
      let open S_syntax in
      let size = S.safe_int size in
      S.safe_int 60_000 + (size + (size lsr 2))

    (* model N_Comb *)
    (* Approximating 3.275337 x term *)
    let cost_N_Comb size = S.safe_int (80 + ((3 * size) + (size lsr 2)))

    (* model N_Comb_get *)
    (* Approximating 0.553178 x term *)
    let cost_N_Comb_get size = S.safe_int (80 + ((size lsr 1) + (size lsr 4)))

    (* model N_Comb_set *)
    (* Approximating 1.282976 x term *)
    let cost_N_Comb_set size = S.safe_int (80 + (size + (size lsr 2)))

    (* model N_Compare_address *)
    let cost_N_Compare_address size1 size2 =
      S.safe_int (80 + (2 * Compare.Int.min size1 size2))

    (* model N_Compare_bool *)
    let cost_N_Compare_bool size1 size2 =
      S.safe_int (80 + (128 * Compare.Int.min size1 size2))

    (* model N_Compare_int *)
    (* Approximating 0.073657 x term *)
    let cost_N_Compare_int size1 size2 =
      let v0 = Compare.Int.min size1 size2 in
      S.safe_int (150 + ((v0 lsr 4) + (v0 lsr 7)))

    (* model N_Compare_key_hash *)
    let cost_N_Compare_key_hash size1 size2 =
      S.safe_int (80 + (2 * Compare.Int.min size1 size2))

    (* model N_Compare_mutez *)
    let cost_N_Compare_mutez size1 size2 =
      S.safe_int (13 * Compare.Int.min size1 size2)

    (* model N_Compare_string *)
    (* Approximating 0.039389 x term *)
    let cost_N_Compare_string size1 size2 =
      let v0 = Compare.Int.min size1 size2 in
      S.safe_int (120 + ((v0 lsr 5) + (v0 lsr 7)))

    (* model N_Compare_timestamp *)
    (* Approximating 0.072483 x term *)
    let cost_N_Compare_timestamp size1 size2 =
      let v0 = Compare.Int.min size1 size2 in
      S.safe_int (140 + ((v0 lsr 4) + (v0 lsr 7)))

    (* model N_Concat_string_pair *)
    (* Approximating 0.068808 x term *)
    let cost_N_Concat_string_pair size1 size2 =
      let open S_syntax in
      let v0 = S.safe_int size1 + S.safe_int size2 in
      S.safe_int 80 + (v0 lsr 4)

    (* model N_Cons_list *)
    let cost_N_Cons_list = S.safe_int 50

    (* model N_Cons_none *)
    let cost_N_Cons_none = S.safe_int 45

    (* model N_Cons_pair *)
    let cost_N_Cons_pair = S.safe_int 45

    (* model N_Cons_some *)
    let cost_N_Cons_some = S.safe_int 45

    (* model N_Const *)
    let cost_N_Const = S.safe_int 45

    (* model N_Dig *)
    let cost_N_Dig size = S.safe_int (100 + (4 * size))

    (* model N_Dip *)
    let cost_N_Dip = S.safe_int 45

    (* model N_DipN *)
    let cost_N_DipN size = S.safe_int (100 + (4 * size))

    (* model N_Drop *)
    let cost_N_Drop = S.safe_int 45

    (* model N_DropN *)
    let cost_N_DropN size = S.safe_int (100 + (4 * size))

    (* model N_Dug *)
    let cost_N_Dug size = S.safe_int (100 + (4 * size))

    (* model N_Dup *)
    let cost_N_Dup = S.safe_int 50

    (* model N_DupN *)
    (* Approximating 1.299969 x term *)
    let cost_N_DupN size = S.safe_int (60 + size + (size lsr 2))

    (* model N_Ediv_natnat *)
    (* Approximating 0.001599 x term *)
    let cost_N_Ediv_natnat size1 size2 =
      let q = size1 - size2 in
      if Compare.Int.(q < 0) then S.safe_int 300
      else
        let open S_syntax in
        let v0 = S.safe_int q * S.safe_int size2 in
        S.safe_int 300 + (v0 lsr 10) + (v0 lsr 11) + (v0 lsr 13)

    (* model N_Ediv_tez *)
    let cost_N_Ediv_tez = S.safe_int 200

    (* model N_Ediv_teznat *)
    (* Extracted by hand from the empirical data *)
    let cost_N_Ediv_teznat = S.safe_int 300

    (* model N_Empty_map *)
    let cost_N_Empty_map = S.safe_int 200

    (* model N_Empty_set *)
    let cost_N_Empty_set = S.safe_int 200

    (* model N_Eq *)
    let cost_N_Eq = S.safe_int 50

    (* model N_If *)
    let cost_N_If = S.safe_int 25

    (* model N_If_cons *)
    let cost_N_If_cons = S.safe_int 30

    (* model N_If_left *)
    let cost_N_If_left = S.safe_int 30

    (* model N_If_none *)
    let cost_N_If_none = S.safe_int 30

    (* model N_Int_nat *)
    let cost_N_Int_nat = S.safe_int 45

    (* model N_Is_nat *)
    let cost_N_Is_nat = S.safe_int 50

    (* model N_Keccak *)
    let cost_N_Keccak size =
      let open S_syntax in
      S.safe_int 1_400 + (S.safe_int 30 * S.safe_int size)

    (* model N_Left *)
    let cost_N_Left = S.safe_int 45

    (* model N_List_iter *)
    let cost_N_List_iter size =
      let open S_syntax in
      S.safe_int 500 + (S.safe_int 7 * S.safe_int size)

    (* model N_List_map *)
    let cost_N_List_map size =
      let open S_syntax in
      S.safe_int 500 + (S.safe_int 12 * S.safe_int size)

    (* model N_List_size *)
    let cost_N_List_size = S.safe_int 50

    (* model N_Loop *)
    let cost_N_Loop = S.safe_int 40

    (* model N_Loop_left *)
    let cost_N_Loop_left = S.safe_int 45

    (* model N_Lsl_nat *)
    (* Approximating 0.129443 x term *)
    let cost_N_Lsl_nat size = S.safe_int (150 + (size lsr 3))

    (* model N_Lsr_nat *)
    (* Approximating 0.129435 x term *)
    let cost_N_Lsr_nat size = S.safe_int (150 + (size lsr 3))

    (* model N_Map_get *)
    (* Approximating 0.057548 x term *)
    let cost_N_Map_get size1 size2 =
      let open S_syntax in
      let v0 = size1 * log2 size2 in
      S.safe_int 80 + (v0 lsr 5) + (v0 lsr 6) + (v0 lsr 7)

    (* model N_Map_iter *)
    let cost_N_Map_iter size =
      let open S_syntax in
      S.safe_int 80 + (S.safe_int 40 * S.safe_int size)

    (* model N_Map_map *)
    let cost_N_Map_map size =
      let open S_syntax in
      S.safe_int 80 + (S.safe_int 761 * S.safe_int size)

    (* model N_Map_mem *)
    (* Approximating 0.058563 x term *)
    let cost_N_Map_mem size1 size2 =
      let open S_syntax in
      let v0 = size1 * log2 size2 in
      S.safe_int 80 + (v0 lsr 5) + (v0 lsr 6) + (v0 lsr 7)

    (* model N_Map_size *)
    let cost_N_Map_size = S.safe_int 50

    (* model N_Map_update *)
    (* Approximating 0.119968 x term *)
    let cost_N_Map_update size1 size2 =
      let open S_syntax in
      let v0 = size1 * log2 size2 in
      S.safe_int 80 + (v0 lsr 4) + (v0 lsr 5) + (v0 lsr 6) + (v0 lsr 7)

    (* model N_Mul_bls12_381_fr *)

    let cost_N_Mul_bls12_381_fr = S.safe_int 250

    (* model N_Mul_bls12_381_g1 *)

    let cost_N_Mul_bls12_381_g1 = S.safe_int 242_000

    (* model N_Mul_bls12_381_g2 *)

    let cost_N_Mul_bls12_381_g2 = S.safe_int 785_500

    (* Converting fr from/to S.t *)
    let cost_bls12_381_fr_of_z = S.safe_int 130

    let cost_bls12_381_fr_to_z = S.safe_int 30

    let cost_N_Mul_bls12_381_fr_z =
      S.add cost_bls12_381_fr_of_z cost_N_Mul_bls12_381_fr

    let cost_N_Int_bls12_381_fr = cost_bls12_381_fr_to_z

    (* model N_Mul_intint *)
    let cost_N_Mul_intint size1 size2 =
      let open S_syntax in
      let a = S.add (S.safe_int size1) (S.safe_int size2) in
      S.safe_int 80 + (a * log2 a)

    (* model N_Mul_teznat *)
    let cost_N_Mul_teznat size =
      let open S_syntax in
      S.safe_int 200 + (S.safe_int 133 * S.safe_int size)

    (* model N_Neg_bls12_381_fr *)

    let cost_N_Neg_bls12_381_fr = S.safe_int 150

    (* model N_Neg_bls12_381_g1 *)

    let cost_N_Neg_bls12_381_g1 = S.safe_int 350

    (* model N_Neg_bls12_381_g2 *)

    let cost_N_Neg_bls12_381_g2 = S.safe_int 600

    (* model N_Neg_int *)
    (* Approximating 0.068419 x term *)
    let cost_N_Neg_int size = S.safe_int (80 + (size lsr 4))

    (* model N_Neq *)
    let cost_N_Neq = S.safe_int 45

    (* model N_Nil *)
    let cost_N_Nil = S.safe_int 45

    (* model N_Nop *)
    let cost_N_Nop = S.safe_int 45

    (* model N_Not *)
    let cost_N_Not = S.safe_int 50

    (* model N_Not_int *)
    (* Approximating 0.076564 x term *)
    let cost_N_Not_int size = S.safe_int (55 + ((size lsr 4) + (size lsr 7)))

    (* model N_Or *)
    let cost_N_Or = S.safe_int 50

    (* model N_Or_nat *)
    (* Approximating 0.078718 x term *)
    let cost_N_Or_nat size1 size2 =
      let v0 = Compare.Int.max size1 size2 in
      S.safe_int (80 + ((v0 lsr 4) + (v0 lsr 6)))

    (* model N_Pairing_check_bls12_381 *)

    let cost_N_Pairing_check_bls12_381 size =
      S.add
        (S.safe_int 1_550_000)
        (S.mul (S.safe_int 510_000) (S.safe_int size))

    (* model N_Right *)
    let cost_N_Right = S.safe_int 45

    (* model N_Seq *)
    let cost_N_Seq = S.safe_int 30

    (* model N_Set_iter *)
    let cost_N_Set_iter size =
      let open S_syntax in
      S.safe_int 80 + (S.safe_int 36 * S.safe_int size)

    (* model N_Set_mem *)
    (* Approximating 0.059410 x term *)
    let cost_N_Set_mem size1 size2 =
      let open S_syntax in
      let v0 = size1 * log2 (S.safe_int size2) in
      S.safe_int 80 + (v0 lsr 5) + (v0 lsr 6) + (v0 lsr 7) + (v0 lsr 8)

    (* model N_Set_size *)
    let cost_N_Set_size = S.safe_int 50

    (* model N_Set_update *)
    (* Approximating 0.126260 x term *)
    let cost_N_Set_update size1 size2 =
      let open S_syntax in
      let v0 = size1 * log2 (S.safe_int size2) in
      S.safe_int 80 + (v0 lsr 3)

    (* model N_Sha256 *)
    let cost_N_Sha256 size =
      let open S_syntax in
      S.safe_int 500 + (S.safe_int 5 * S.safe_int size)

    (* model N_Sha3 *)
    let cost_N_Sha3 size =
      let open S_syntax in
      S.safe_int 1_400 + (S.safe_int 32 * S.safe_int size)

    (* model N_Sha512 *)
    let cost_N_Sha512 size =
      let open S_syntax in
      S.safe_int 500 + (S.safe_int 3 * S.safe_int size)

    (* model N_Slice_string *)
    (* Approximating 0.067048 x term *)
    let cost_N_Slice_string size = S.safe_int (80 + (size lsr 4))

    (* model N_String_size *)
    let cost_N_String_size = S.safe_int 50

    (* model N_Sub_int *)
    (* Approximating 0.082399 x term *)
    let cost_N_Sub_int size1 size2 =
      let v0 = Compare.Int.max size1 size2 in
      S.safe_int (80 + ((v0 lsr 4) + (v0 lsr 6)))

    (* model N_Sub_tez *)
    let cost_N_Sub_tez = S.safe_int 55

    (* model N_Swap *)
    let cost_N_Swap = S.safe_int 40

    (* model N_Total_voting_power *)
    let cost_N_Total_voting_power = S.safe_int 400

    (* model N_Uncomb *)
    (* Approximating 3.666332 x term *)
    let cost_N_Uncomb size =
      S.safe_int (80 + ((3 * size) + (size lsr 1) + (size lsr 3)))

    (* model N_Unpair *)
    let cost_N_Unpair = S.safe_int 50

    (* model N_Voting_power *)
    let cost_N_Voting_power = S.safe_int 400

    (* model N_Xor *)
    let cost_N_Xor = S.safe_int 50

    (* model N_Xor_nat *)
    (* Approximating 0.078258 x term *)
    let cost_N_Xor_nat size1 size2 =
      let v0 = Compare.Int.max size1 size2 in
      S.safe_int (80 + ((v0 lsr 4) + (v0 lsr 6)))

    (* model DECODING_BLS_FR *)

    let cost_DECODING_BLS_FR = S.safe_int 50

    (* model DECODING_BLS_G1 *)

    let cost_DECODING_BLS_G1 = S.safe_int 230_000

    (* model DECODING_BLS_G2 *)

    let cost_DECODING_BLS_G2 = S.safe_int 740_000

    (* model B58CHECK_DECODING_CHAIN_ID *)
    let cost_B58CHECK_DECODING_CHAIN_ID = S.safe_int 1_500

    (* model B58CHECK_DECODING_PUBLIC_KEY_HASH_ed25519 *)
    let cost_B58CHECK_DECODING_PUBLIC_KEY_HASH_ed25519 = S.safe_int 3_300

    (* model B58CHECK_DECODING_PUBLIC_KEY_HASH_p256 *)
    let cost_B58CHECK_DECODING_PUBLIC_KEY_HASH_p256 = S.safe_int 3_300

    (* model B58CHECK_DECODING_PUBLIC_KEY_HASH_secp256k1 *)
    let cost_B58CHECK_DECODING_PUBLIC_KEY_HASH_secp256k1 = S.safe_int 3_300

    (* model B58CHECK_DECODING_PUBLIC_KEY_ed25519 *)
    let cost_B58CHECK_DECODING_PUBLIC_KEY_ed25519 = S.safe_int 4_300

    (* model B58CHECK_DECODING_PUBLIC_KEY_p256 *)
    let cost_B58CHECK_DECODING_PUBLIC_KEY_p256 = S.safe_int 29_000

    (* model B58CHECK_DECODING_PUBLIC_KEY_secp256k1 *)
    let cost_B58CHECK_DECODING_PUBLIC_KEY_secp256k1 = S.safe_int 9_400

    (* model B58CHECK_DECODING_SIGNATURE_ed25519 *)
    let cost_B58CHECK_DECODING_SIGNATURE_ed25519 = S.safe_int 6_600

    (* model B58CHECK_DECODING_SIGNATURE_p256 *)
    let cost_B58CHECK_DECODING_SIGNATURE_p256 = S.safe_int 6_600

    (* model B58CHECK_DECODING_SIGNATURE_secp256k1 *)
    let cost_B58CHECK_DECODING_SIGNATURE_secp256k1 = S.safe_int 6_600

    (* model ENCODING_BLS_FR *)

    let cost_ENCODING_BLS_FR = S.safe_int 30

    (* model ENCODING_BLS_G1 *)

    let cost_ENCODING_BLS_G1 = S.safe_int 30

    (* model ENCODING_BLS_G2 *)

    let cost_ENCODING_BLS_G2 = S.safe_int 30

    (* model B58CHECK_ENCODING_CHAIN_ID *)
    let cost_B58CHECK_ENCODING_CHAIN_ID = S.safe_int 1_600

    (* model B58CHECK_ENCODING_PUBLIC_KEY_HASH_ed25519 *)
    let cost_B58CHECK_ENCODING_PUBLIC_KEY_HASH_ed25519 = S.safe_int 3_300

    (* model B58CHECK_ENCODING_PUBLIC_KEY_HASH_p256 *)
    let cost_B58CHECK_ENCODING_PUBLIC_KEY_HASH_p256 = S.safe_int 3_750

    (* model B58CHECK_ENCODING_PUBLIC_KEY_HASH_secp256k1 *)
    let cost_B58CHECK_ENCODING_PUBLIC_KEY_HASH_secp256k1 = S.safe_int 3_300

    (* model B58CHECK_ENCODING_PUBLIC_KEY_ed25519 *)
    let cost_B58CHECK_ENCODING_PUBLIC_KEY_ed25519 = S.safe_int 4_500

    (* model B58CHECK_ENCODING_PUBLIC_KEY_p256 *)
    let cost_B58CHECK_ENCODING_PUBLIC_KEY_p256 = S.safe_int 5_300

    (* model B58CHECK_ENCODING_PUBLIC_KEY_secp256k1 *)
    let cost_B58CHECK_ENCODING_PUBLIC_KEY_secp256k1 = S.safe_int 5_000

    (* model B58CHECK_ENCODING_SIGNATURE_ed25519 *)
    let cost_B58CHECK_ENCODING_SIGNATURE_ed25519 = S.safe_int 8_700

    (* model B58CHECK_ENCODING_SIGNATURE_p256 *)
    let cost_B58CHECK_ENCODING_SIGNATURE_p256 = S.safe_int 8_700

    (* model B58CHECK_ENCODING_SIGNATURE_secp256k1 *)
    let cost_B58CHECK_ENCODING_SIGNATURE_secp256k1 = S.safe_int 8_700

    (* model DECODING_CHAIN_ID *)
    let cost_DECODING_CHAIN_ID = S.safe_int 50

    (* model DECODING_PUBLIC_KEY_HASH_ed25519 *)
    let cost_DECODING_PUBLIC_KEY_HASH_ed25519 = S.safe_int 50

    (* model DECODING_PUBLIC_KEY_HASH_p256 *)
    let cost_DECODING_PUBLIC_KEY_HASH_p256 = S.safe_int 60

    (* model DECODING_PUBLIC_KEY_HASH_secp256k1 *)
    let cost_DECODING_PUBLIC_KEY_HASH_secp256k1 = S.safe_int 60

    (* model DECODING_PUBLIC_KEY_ed25519 *)
    let cost_DECODING_PUBLIC_KEY_ed25519 = S.safe_int 60

    (* model DECODING_PUBLIC_KEY_p256 *)
    let cost_DECODING_PUBLIC_KEY_p256 = S.safe_int 25_000

    (* model DECODING_PUBLIC_KEY_secp256k1 *)
    let cost_DECODING_PUBLIC_KEY_secp256k1 = S.safe_int 5_300

    (* model DECODING_SIGNATURE_ed25519 *)
    let cost_DECODING_SIGNATURE_ed25519 = S.safe_int 30

    (* model DECODING_SIGNATURE_p256 *)
    let cost_DECODING_SIGNATURE_p256 = S.safe_int 30

    (* model DECODING_SIGNATURE_secp256k1 *)
    let cost_DECODING_SIGNATURE_secp256k1 = S.safe_int 30

    (* model ENCODING_CHAIN_ID *)
    let cost_ENCODING_CHAIN_ID = S.safe_int 50

    (* model ENCODING_PUBLIC_KEY_HASH_ed25519 *)
    let cost_ENCODING_PUBLIC_KEY_HASH_ed25519 = S.safe_int 70

    (* model ENCODING_PUBLIC_KEY_HASH_p256 *)
    let cost_ENCODING_PUBLIC_KEY_HASH_p256 = S.safe_int 80

    (* model ENCODING_PUBLIC_KEY_HASH_secp256k1 *)
    let cost_ENCODING_PUBLIC_KEY_HASH_secp256k1 = S.safe_int 70

    (* model ENCODING_PUBLIC_KEY_ed25519 *)
    let cost_ENCODING_PUBLIC_KEY_ed25519 = S.safe_int 80

    (* model ENCODING_PUBLIC_KEY_p256 *)
    let cost_ENCODING_PUBLIC_KEY_p256 = S.safe_int 450

    (* model ENCODING_PUBLIC_KEY_secp256k1 *)
    let cost_ENCODING_PUBLIC_KEY_secp256k1 = S.safe_int 490

    (* model ENCODING_SIGNATURE_ed25519 *)
    let cost_ENCODING_SIGNATURE_ed25519 = S.safe_int 40

    (* model ENCODING_SIGNATURE_p256 *)
    let cost_ENCODING_SIGNATURE_p256 = S.safe_int 40

    (* model ENCODING_SIGNATURE_secp256k1 *)
    let cost_ENCODING_SIGNATURE_secp256k1 = S.safe_int 40

    (* model TIMESTAMP_READABLE_DECODING *)
    let cost_TIMESTAMP_READABLE_DECODING = S.safe_int 130

    (* model TIMESTAMP_READABLE_ENCODING *)
    let cost_TIMESTAMP_READABLE_ENCODING = S.safe_int 900

    (* model CHECK_PRINTABLE *)
    let cost_CHECK_PRINTABLE size =
      let open S_syntax in
      S.safe_int 14 + (S.safe_int 10 * S.safe_int size)

    (* model MERGE_TYPES
       This is the estimated cost of one iteration of merge_types, extracted
       and copied manually from the parameter fit for the MERGE_TYPES benchmark
       (the model is parametric on the size of the type, which we don't have
       access to in O(1)). *)
    let cost_MERGE_TYPES = S.safe_int 130

    (* model TYPECHECKING_CODE
       This is the cost of one iteration of parse_instr, extracted by hand from the
       parameter fit for the TYPECHECKING_CODE benchmark. *)
    let cost_TYPECHECKING_CODE = S.safe_int 375

    (* model UNPARSING_CODE
       This is the cost of one iteration of unparse_instr, extracted by hand from the
       parameter fit for the UNPARSING_CODE benchmark. *)
    let cost_UNPARSING_CODE = S.safe_int 200

    (* model TYPECHECKING_DATA
       This is the cost of one iteration of parse_data, extracted by hand from the
       parameter fit for the TYPECHECKING_DATA benchmark. *)
    let cost_TYPECHECKING_DATA = S.safe_int 240

    (* model UNPARSING_DATA
       This is the cost of one iteration of unparse_data, extracted by hand from the
       parameter fit for the UNPARSING_DATA benchmark. *)
    let cost_UNPARSING_DATA = S.safe_int 140

    (* model PARSE_TYPE
       This is the cost of one iteration of parse_ty, extracted by hand from the
       parameter fit for the PARSE_TYPE benchmark. *)
    let cost_PARSE_TYPE = S.safe_int 170

    (* model UNPARSE_TYPE
       This is the cost of one iteration of unparse_ty, extracted by hand from the
       parameter fit for the UNPARSE_TYPE benchmark. *)
    let cost_UNPARSE_TYPE = S.safe_int 185

    (* TODO: benchmark *)
    let cost_COMPARABLE_TY_OF_TY = S.safe_int 120

    (* model SAPLING_TRANSACTION_ENCODING *)
    let cost_SAPLING_TRANSACTION_ENCODING ~inputs ~outputs =
      S.safe_int (1500 + (inputs * 160) + (outputs * 320))

    (* model SAPLING_DIFF_ENCODING *)
    let cost_SAPLING_DIFF_ENCODING ~nfs ~cms =
      S.safe_int ((nfs * 22) + (cms * 215))
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

    let unpair = atomic_step_cost cost_N_Unpair

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
      atomic_step_cost (cost_N_Map_mem elt_size (S.safe_int (snd Box.boxed)))

    let map_get (type k v) (elt : k)
        ((module Box) : (k, v) Script_typed_ir.map) =
      let elt_size = size_of_comparable Box.key_ty elt in
      atomic_step_cost (cost_N_Map_get elt_size (S.safe_int (snd Box.boxed)))

    let map_update (type k v) (elt : k)
        ((module Box) : (k, v) Script_typed_ir.map) =
      let elt_size = size_of_comparable Box.key_ty elt in
      atomic_step_cost
        (cost_N_Map_update elt_size (S.safe_int (snd Box.boxed)))

    let map_get_and_update (type k v) (elt : k)
        (m : (k, v) Script_typed_ir.map) =
      map_get elt m +@ map_update elt m

    let map_size = atomic_step_cost cost_N_Map_size

    let big_map_elt_size = S.safe_int Script_expr_hash.size

    let big_map_mem ({size; _} : _ Script_typed_ir.big_map_overlay) =
      atomic_step_cost (cost_N_Map_mem big_map_elt_size (S.safe_int size))

    let big_map_get ({size; _} : _ Script_typed_ir.big_map_overlay) =
      atomic_step_cost (cost_N_Map_get big_map_elt_size (S.safe_int size))

    let big_map_update ({size; _} : _ Script_typed_ir.big_map_overlay) =
      atomic_step_cost (cost_N_Map_update big_map_elt_size (S.safe_int size))

    let big_map_get_and_update m = big_map_get m +@ big_map_update m

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
        (cost_N_Concat_string_pair (Bytes.length b1) (Bytes.length b2))

    let slice_bytes b = atomic_step_cost (cost_N_Slice_string (Bytes.length b))

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

    let ediv_teznat _tez _n = atomic_step_cost cost_N_Ediv_teznat

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
            cost_N_Check_signature_ed25519 (Bytes.length b)
        | Secp256k1 _ ->
            cost_N_Check_signature_secp256k1 (Bytes.length b)
        | P256 _ ->
            cost_N_Check_signature_p256 (Bytes.length b)
      in
      atomic_step_cost cost

    let blake2b b = atomic_step_cost (cost_N_Blake2b (Bytes.length b))

    let sha256 b = atomic_step_cost (cost_N_Sha256 (Bytes.length b))

    let sha512 b = atomic_step_cost (cost_N_Sha512 (Bytes.length b))

    let dign n = atomic_step_cost (cost_N_Dig n)

    let dugn n = atomic_step_cost (cost_N_Dug n)

    let dipn n = atomic_step_cost (cost_N_DipN n)

    let dropn n = atomic_step_cost (cost_N_DropN n)

    let voting_power = atomic_step_cost cost_N_Voting_power

    let total_voting_power = atomic_step_cost cost_N_Total_voting_power

    let keccak b = atomic_step_cost (cost_N_Keccak (Bytes.length b))

    let sha3 b = atomic_step_cost (cost_N_Sha3 (Bytes.length b))

    let add_bls12_381_g1 = atomic_step_cost cost_N_Add_bls12_381_g1

    let add_bls12_381_g2 = atomic_step_cost cost_N_Add_bls12_381_g2

    let add_bls12_381_fr = atomic_step_cost cost_N_Add_bls12_381_fr

    let mul_bls12_381_g1 = atomic_step_cost cost_N_Mul_bls12_381_g1

    let mul_bls12_381_g2 = atomic_step_cost cost_N_Mul_bls12_381_g2

    let mul_bls12_381_fr = atomic_step_cost cost_N_Mul_bls12_381_fr

    let mul_bls12_381_fr_z = atomic_step_cost cost_N_Mul_bls12_381_fr_z

    let int_bls12_381_fr = atomic_step_cost cost_N_Int_bls12_381_fr

    let neg_bls12_381_g1 = atomic_step_cost cost_N_Neg_bls12_381_g1

    let neg_bls12_381_g2 = atomic_step_cost cost_N_Neg_bls12_381_g2

    let neg_bls12_381_fr = atomic_step_cost cost_N_Neg_bls12_381_fr

    let neq = atomic_step_cost cost_N_Neq

    let nop = atomic_step_cost cost_N_Nop

    let pairing_check_bls12_381 (l : 'a Script_typed_ir.boxed_list) =
      atomic_step_cost (cost_N_Pairing_check_bls12_381 l.length)

    let comb n = atomic_step_cost (cost_N_Comb n)

    let uncomb n = atomic_step_cost (cost_N_Uncomb n)

    let comb_get n = atomic_step_cost (cost_N_Comb_get n)

    let comb_set n = atomic_step_cost (cost_N_Comb_set n)

    let dupn n = atomic_step_cost (cost_N_DupN n)

    let sapling_verify_update ~inputs ~outputs =
      let open S_syntax in
      atomic_step_cost
        ( S.safe_int 85_000
        + (S.safe_int inputs * S.safe_int 4)
        + (S.safe_int outputs * S.safe_int 30) )

    (* --------------------------------------------------------------------- *)
    (* Semi-hand-crafted models *)
    let compare_unit = atomic_step_cost (S.safe_int 10)

    let compare_union_tag = atomic_step_cost (S.safe_int 10)

    let compare_option_tag = atomic_step_cost (S.safe_int 10)

    let compare_bool = atomic_step_cost (cost_N_Compare_bool 1 1)

    let compare_signature = atomic_step_cost (S.safe_int 92)

    let compare_string s1 s2 =
      atomic_step_cost
        (cost_N_Compare_string (String.length s1) (String.length s2))

    let compare_bytes b1 b2 =
      atomic_step_cost
        (cost_N_Compare_string (Bytes.length b1) (Bytes.length b2))

    let compare_mutez = atomic_step_cost (cost_N_Compare_mutez 8 8)

    let compare_int i1 i2 =
      atomic_step_cost (cost_N_Compare_int (int_bytes i1) (int_bytes i2))

    let compare_nat n1 n2 =
      atomic_step_cost (cost_N_Compare_int (int_bytes n1) (int_bytes n2))

    let compare_key_hash =
      let sz = Signature.Public_key_hash.size in
      atomic_step_cost (cost_N_Compare_key_hash sz sz)

    let compare_key = atomic_step_cost (S.safe_int 92)

    let compare_timestamp t1 t2 =
      atomic_step_cost
        (cost_N_Compare_timestamp
           (z_bytes (Script_timestamp.to_zint t1))
           (z_bytes (Script_timestamp.to_zint t2)))

    let compare_address =
      let sz = Signature.Public_key_hash.size + Chain_id.size in
      atomic_step_cost (cost_N_Compare_address sz sz)

    let compare_chain_id = atomic_step_cost (S.safe_int 30)

    let rec compare : type a. a Script_typed_ir.comparable_ty -> a -> a -> cost
        =
     fun ty x y ->
      match ty with
      | Unit_key _ ->
          compare_unit
      | Never_key _ -> (
        match x with _ -> . )
      | Bool_key _ ->
          compare_bool
      | String_key _ ->
          compare_string x y
      | Signature_key _ ->
          compare_signature
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
      | Key_key _ ->
          compare_key
      | Timestamp_key _ ->
          compare_timestamp x y
      | Address_key _ ->
          compare_address
      | Chain_id_key _ ->
          compare_chain_id
      | Pair_key ((tl, _), (tr, _), _) ->
          (* Reasonable over-approximation of the cost of lexicographic comparison. *)
          let (xl, xr) = x in
          let (yl, yr) = y in
          compare tl xl yl +@ compare tr xr yr
      | Union_key ((tl, _), (tr, _), _) -> (
          compare_union_tag
          +@
          match (x, y) with
          | (L x, L y) ->
              compare tl x y
          | (L _, R _) ->
              free
          | (R _, L _) ->
              free
          | (R x, R y) ->
              compare tr x y )
      | Option_key (t, _) -> (
          compare_option_tag
          +@
          match (x, y) with
          | (None, None) ->
              free
          | (None, Some _) ->
              free
          | (Some _, None) ->
              free
          | (Some x, Some y) ->
              compare t x y )

    (* --------------------------------------------------------------------- *)
    (* Hand-crafted models *)

    (* The cost functions below where not benchmarked, a cost model was derived
       from looking at similar instructions. *)

    let sapling_empty_state = empty_map

    (* Cost for Concat_string is paid in two steps: when entering the interpreter,
       the user pays for the cost of computing the information necessary to compute
       the actual gas (so it's meta-gas): indeed, one needs to run through the
       list of strings to compute the total allocated cost.
       [concat_string_precheck] corresponds to the meta-gas cost of this computation.
     *)
    let concat_string_precheck (l : 'a Script_typed_ir.boxed_list) =
      (* we set the precheck to be slightly more expensive than cost_N_List_iter *)
      atomic_step_cost (S.mul (S.safe_int l.length) (S.safe_int 10))

    (* This is the cost of allocating a string and blitting existing ones into it. *)
    let concat_string total_bytes =
      atomic_step_cost
        S.(add (S.safe_int 100) (S.ediv total_bytes (S.safe_int 10)))

    (* Same story as Concat_string. *)
    let concat_bytes total_bytes =
      atomic_step_cost
        S.(add (S.safe_int 100) (S.ediv total_bytes (S.safe_int 10)))

    (* Cost of additional call to logger + overhead of setting up call to [interp]. *)
    let exec = atomic_step_cost (S.safe_int 100)

    (* Heavy computation happens in the [unparse_data], [unparse_ty]
       functions which are carbonated. We must account for allocating
       the Micheline lambda wrapper. *)
    let apply = atomic_step_cost (S.safe_int 1000)

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
    let level = atomic_step_cost (S.mul (S.safe_int 2) cost_N_Const)

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

    (* TODO benchmark *)
    (* FIXME: imported from 006, needs proper benchmarks *)
    let unpack_failed bytes =
      (* We cannot instrument failed deserialization,
         so we take worst case fees: a set of size 1 bytes values. *)
      let blen = Bytes.length bytes in
      let len = S.safe_int blen in
      let d = Z.numbits (Z.of_int blen) in
      (len *@ alloc_mbytes_cost 1)
      +@ len
         *@ ( S.safe_int d
            *@ (alloc_cost (S.safe_int 3) +@ step_cost (S.safe_int 1)) )

    let ticket = atomic_step_cost (S.safe_int 80)

    let read_ticket = atomic_step_cost (S.safe_int 80)

    let split_ticket ticket_amount amount_a amount_b =
      ticket
      +@ add_bigint amount_a amount_b
      +@ compare_nat ticket_amount ticket_amount

    let join_tickets :
        'a Script_typed_ir.comparable_ty ->
        'a Script_typed_ir.ticket ->
        'a Script_typed_ir.ticket ->
        Gas.cost =
     fun ty ticket_a ticket_b ->
      ticket +@ compare_address
      +@ add_bigint ticket_a.amount ticket_b.amount
      +@ compare ty ticket_a.contents ticket_b.contents
  end

  module Typechecking = struct
    open Generated_costs_007

    let public_key_optimized =
      atomic_step_cost
      @@ S.(
           max
             cost_DECODING_PUBLIC_KEY_ed25519
             (max
                cost_DECODING_PUBLIC_KEY_secp256k1
                cost_DECODING_PUBLIC_KEY_p256))

    let public_key_readable =
      atomic_step_cost
      @@ S.(
           max
             cost_B58CHECK_DECODING_PUBLIC_KEY_ed25519
             (max
                cost_B58CHECK_DECODING_PUBLIC_KEY_secp256k1
                cost_B58CHECK_DECODING_PUBLIC_KEY_p256))

    let key_hash_optimized =
      atomic_step_cost
      @@ S.(
           max
             cost_DECODING_PUBLIC_KEY_HASH_ed25519
             (max
                cost_DECODING_PUBLIC_KEY_HASH_secp256k1
                cost_DECODING_PUBLIC_KEY_HASH_p256))

    let key_hash_readable =
      atomic_step_cost
      @@ S.(
           max
             cost_B58CHECK_DECODING_PUBLIC_KEY_HASH_ed25519
             (max
                cost_B58CHECK_DECODING_PUBLIC_KEY_HASH_secp256k1
                cost_B58CHECK_DECODING_PUBLIC_KEY_HASH_p256))

    let signature_optimized =
      atomic_step_cost
      @@ S.(
           max
             cost_DECODING_SIGNATURE_ed25519
             (max
                cost_DECODING_SIGNATURE_secp256k1
                cost_DECODING_SIGNATURE_p256))

    let signature_readable =
      atomic_step_cost
      @@ S.(
           max
             cost_B58CHECK_DECODING_SIGNATURE_ed25519
             (max
                cost_B58CHECK_DECODING_SIGNATURE_secp256k1
                cost_B58CHECK_DECODING_SIGNATURE_p256))

    let chain_id_optimized = atomic_step_cost cost_DECODING_CHAIN_ID

    let chain_id_readable = atomic_step_cost cost_B58CHECK_DECODING_CHAIN_ID

    (* Reasonable approximation *)
    let address_optimized = key_hash_optimized

    (* Reasonable approximation *)
    let contract_optimized = key_hash_optimized

    (* Reasonable approximation *)
    let contract_readable = key_hash_readable

    let bls12_381_g1 = atomic_step_cost cost_DECODING_BLS_G1

    let bls12_381_g2 = atomic_step_cost cost_DECODING_BLS_G2

    let bls12_381_fr = atomic_step_cost cost_DECODING_BLS_FR

    let check_printable s =
      atomic_step_cost (cost_CHECK_PRINTABLE (String.length s))

    let merge_cycle = atomic_step_cost cost_MERGE_TYPES

    let parse_type_cycle = atomic_step_cost cost_PARSE_TYPE

    let parse_instr_cycle = atomic_step_cost cost_TYPECHECKING_CODE

    let parse_data_cycle = atomic_step_cost cost_TYPECHECKING_DATA

    let comparable_ty_of_ty_cycle = atomic_step_cost cost_COMPARABLE_TY_OF_TY

    (* Cost of a cycle of checking that a type is dupable *)
    (* TODO: bench *)
    let check_dupable_cycle = atomic_step_cost cost_TYPECHECKING_DATA

    let bool = free

    let unit = free

    let timestamp_readable = atomic_step_cost cost_TIMESTAMP_READABLE_DECODING

    (* Reasonable estimate. *)
    let contract = Gas.(S.safe_int 2 *@ public_key_readable)

    (* Assuming unflattened storage: /contracts/hash1/.../hash6/key/balance,
       balance stored on 64 bits *)
    let contract_exists =
      Gas.cost_of_repr
      @@ Storage_costs.read_access ~path_length:9 ~read_bytes:8

    (* Constructing proof arguments consists in a decreasing loop in the result
       monad, allocating at each step. We charge a reasonable overapproximation. *)
    let proof_argument n =
      atomic_step_cost (S.mul (S.safe_int n) (S.safe_int 50))
  end

  module Unparsing = struct
    open Generated_costs_007

    let public_key_optimized =
      atomic_step_cost
      @@ S.(
           max
             cost_ENCODING_PUBLIC_KEY_ed25519
             (max
                cost_ENCODING_PUBLIC_KEY_secp256k1
                cost_ENCODING_PUBLIC_KEY_p256))

    let public_key_readable =
      atomic_step_cost
      @@ S.(
           max
             cost_B58CHECK_ENCODING_PUBLIC_KEY_ed25519
             (max
                cost_B58CHECK_ENCODING_PUBLIC_KEY_secp256k1
                cost_B58CHECK_ENCODING_PUBLIC_KEY_p256))

    let key_hash_optimized =
      atomic_step_cost
      @@ S.(
           max
             cost_ENCODING_PUBLIC_KEY_HASH_ed25519
             (max
                cost_ENCODING_PUBLIC_KEY_HASH_secp256k1
                cost_ENCODING_PUBLIC_KEY_HASH_p256))

    let key_hash_readable =
      atomic_step_cost
      @@ S.(
           max
             cost_B58CHECK_ENCODING_PUBLIC_KEY_HASH_ed25519
             (max
                cost_B58CHECK_ENCODING_PUBLIC_KEY_HASH_secp256k1
                cost_B58CHECK_ENCODING_PUBLIC_KEY_HASH_p256))

    let signature_optimized =
      atomic_step_cost
      @@ S.(
           max
             cost_ENCODING_SIGNATURE_ed25519
             (max
                cost_ENCODING_SIGNATURE_secp256k1
                cost_ENCODING_SIGNATURE_p256))

    let signature_readable =
      atomic_step_cost
      @@ S.(
           max
             cost_B58CHECK_ENCODING_SIGNATURE_ed25519
             (max
                cost_B58CHECK_ENCODING_SIGNATURE_secp256k1
                cost_B58CHECK_ENCODING_SIGNATURE_p256))

    let chain_id_optimized = atomic_step_cost cost_ENCODING_CHAIN_ID

    let chain_id_readable = atomic_step_cost cost_B58CHECK_ENCODING_CHAIN_ID

    let timestamp_readable = atomic_step_cost cost_TIMESTAMP_READABLE_ENCODING

    (* Reasonable approximation *)
    let address_optimized = key_hash_optimized

    (* Reasonable approximation *)
    let contract_optimized = key_hash_optimized

    (* Reasonable approximation *)
    let contract_readable = key_hash_readable

    let bls12_381_g1 = atomic_step_cost cost_ENCODING_BLS_G1

    let bls12_381_g2 = atomic_step_cost cost_ENCODING_BLS_G2

    let bls12_381_fr = atomic_step_cost cost_ENCODING_BLS_FR

    let unparse_type_cycle = atomic_step_cost cost_UNPARSE_TYPE

    let unparse_instr_cycle = atomic_step_cost cost_UNPARSING_CODE

    let unparse_data_cycle = atomic_step_cost cost_UNPARSING_DATA

    let unit = Gas.free

    (* Reasonable estimate. *)
    let contract = Gas.(S.safe_int 2 *@ public_key_readable)

    (* Reuse 006 costs. *)
    let operation bytes = Script.bytes_node_cost bytes

    let sapling_transaction (t : Sapling.transaction) =
      let inputs = List.length t.inputs in
      let outputs = List.length t.outputs in
      atomic_step_cost (cost_SAPLING_TRANSACTION_ENCODING ~inputs ~outputs)

    let sapling_diff (d : Sapling.diff) =
      let nfs = List.length d.nullifiers in
      let cms = List.length d.commitments_and_ciphertexts in
      atomic_step_cost (cost_SAPLING_DIFF_ENCODING ~nfs ~cms)
  end
end
